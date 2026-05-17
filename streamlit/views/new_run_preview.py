"""Preview app-managed DILS run preparation without writing files."""

from __future__ import annotations

import shlex
from pathlib import Path
from typing import Any

import streamlit as st

from config_yaml import load_canonical_defaults, serialize_yaml
from fasta_headers import FastaHeaderSummary, parse_fasta_headers
from run_manager import (
    RunCreationError,
    RunLaunchError,
    RunPaths,
    RunSummary,
    build_slurm_launch_plan,
    create_run_files,
    generate_run_id,
    launch_slurm_run,
    list_existing_runs,
    load_run_metadata,
    mark_submission_failed,
    plan_run_paths,
    refresh_slurm_status,
)
from settings import load_settings

_RUN_ID_KEY = "new_run_preview_run_id"
_FASTA_CACHE_KEY = "new_run_preview_fasta_header_summary_cache"
_CREATED_RUN_KEY = "new_run_preview_created_run"
_DIRECT_DOWNLOAD_LIMIT_BYTES = 100 * 1024 * 1024


def _as_bool(value: Any, default: bool = False) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        return value.strip().lower() in {"true", "yes", "1"}
    return default


def _as_float(value: Any, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _as_int(value: Any, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _uploaded_file_key(uploaded_file) -> tuple[str, int | None, str | None]:
    return (
        getattr(uploaded_file, "name", ""),
        getattr(uploaded_file, "size", None),
        getattr(uploaded_file, "file_id", None),
    )


def _parse_uploaded_fasta(uploaded_file) -> FastaHeaderSummary:
    cache = st.session_state.setdefault(_FASTA_CACHE_KEY, {})
    upload_key = _uploaded_file_key(uploaded_file)
    if upload_key not in cache:
        with st.spinner("Parsing FASTA headers..."):
            cache.clear()
            cache[upload_key] = parse_fasta_headers(uploaded_file)
    return cache[upload_key]


def _ensure_run_id(prefix: str) -> str:
    if _RUN_ID_KEY not in st.session_state:
        st.session_state[_RUN_ID_KEY] = generate_run_id(prefix)
    return st.session_state[_RUN_ID_KEY]


def _path_table(paths: RunPaths) -> None:
    st.table(
        {
            "Path": [
                "run_dir",
                "input_dir",
                "config_dir",
                "work_dir",
                "slurm_logs_dir",
                "snakemake_logs_dir",
                "results_dir",
                "uploaded_fasta_path",
                "config_yaml_path",
                "expected_archive_path",
            ],
            "Value": [
                str(paths.run_dir),
                str(paths.input_dir),
                str(paths.config_dir),
                str(paths.work_dir),
                str(paths.slurm_logs_dir),
                str(paths.snakemake_logs_dir),
                str(paths.results_dir),
                str(paths.uploaded_fasta_path),
                str(paths.config_yaml_path),
                str(paths.expected_archive_path),
            ],
        }
    )


def _created_paths_display(paths: dict[str, str]) -> None:
    labels = [
        ("FASTA", "uploaded_fasta_path"),
        ("YAML", "config_yaml_path"),
        ("Metadata", "metadata_path"),
    ]
    for label, key in labels:
        st.markdown(f"**{label}**")
        st.code(paths[key], language=None)


def _format_file_size(size_bytes: int) -> str:
    size = float(size_bytes)
    for unit in ("B", "KB", "MB", "GB"):
        if size < 1024 or unit == "GB":
            return f"{size:.1f} {unit}" if unit != "B" else f"{size_bytes} B"
        size /= 1024
    return f"{size_bytes} B"


def _render_results_download(metadata: dict[str, Any], show_details: bool) -> None:
    if status := metadata.get("status"):
        if status != "completed":
            return
    else:
        return
    if not metadata.get("final_archive_exists"):
        return

    archive_raw = metadata.get("final_archive_path")
    if not archive_raw:
        st.warning("Metadata says the final archive exists, but no archive path is recorded.")
        return

    archive_path = Path(str(archive_raw))
    if not archive_path.is_file():
        st.warning(
            "Metadata says the final archive exists, but the file is no longer available on disk."
        )
        return

    st.success("Results are ready. The final archive can be opened in the Results viewer.")
    if show_details:
        st.code(str(archive_path), language=None)

    archive_size = archive_path.stat().st_size
    st.write(f"Results archive size: `{_format_file_size(archive_size)}`")
    if archive_size > _DIRECT_DOWNLOAD_LIMIT_BYTES:
        st.warning(
            "The results archive is larger than the direct download limit. "
            "It should be retrieved by another delivery method."
        )
        return

    # Streamlit download_button may load data into server memory, so direct
    # downloads are capped at 100 MB.
    with archive_path.open("rb") as archive_file:
        st.markdown('<div class="dils-accent-action"></div>', unsafe_allow_html=True)
        st.download_button(
            "Download results archive",
            data=archive_file,
            file_name=archive_path.name,
            mime="application/gzip",
            width="stretch",
        )


def _render_job_status(metadata: dict[str, Any], show_details: bool) -> None:
    status = metadata.get("status", "unknown")
    st.markdown(f"**Current status:** `{status}`")

    if metadata.get("slurm_job_id"):
        st.markdown(f"**Slurm job ID:** `{metadata['slurm_job_id']}`")
    if metadata.get("slurm_state") or metadata.get("slurm_exit_code"):
        st.write(
            f"Slurm state: `{metadata.get('slurm_state') or 'unknown'}` · "
            f"Exit code: `{metadata.get('slurm_exit_code') or 'unknown'}`"
        )
    if metadata.get("status_checked_at"):
        st.caption(f"Last checked: `{metadata['status_checked_at']}`")
    if metadata.get("status_warning"):
        st.warning(str(metadata["status_warning"]))
    if status == "submission_failed" and metadata.get("submission_error"):
        st.error("Submission failed before the analysis was queued.")
        if show_details:
            st.code(str(metadata["submission_error"]), language=None)

    stdout_log = metadata.get("stdout_log_resolved") or metadata.get("stdout_log")
    stderr_log = metadata.get("stderr_log_resolved") or metadata.get("stderr_log")
    if show_details and (stdout_log or stderr_log):
        st.markdown("**Slurm logs**")
        if stdout_log:
            st.code(str(stdout_log), language=None)
        if stderr_log:
            st.code(str(stderr_log), language=None)

    if metadata.get("final_archive_exists"):
        _render_results_download(metadata, show_details=show_details)
    elif status == "completed_missing_archive":
        st.warning("Slurm completed successfully, but the expected final archive was not found.")

    if status == "failed" and stderr_log and show_details:
        st.error("Run failed. Check the Slurm stderr log shown above.")


def _render_slurm_status_panel(
    settings,
    metadata_path: Path,
    metadata: dict[str, Any],
    key_suffix: str,
    show_details: bool,
) -> None:
    st.subheader("Slurm job status")

    st.markdown('<div class="dils-accent-action"></div>', unsafe_allow_html=True)
    if st.button("Refresh job status", width="stretch", key=f"refresh_job_status_{key_suffix}"):
        try:
            metadata = refresh_slurm_status(settings, metadata_path)
        except RunLaunchError as exc:
            st.error(str(exc))
        except Exception as exc:
            st.error(f"Could not refresh Slurm job status: {exc}")
        else:
            st.success("Job status refreshed.")

    _render_job_status(metadata, show_details=show_details)


def _render_slurm_launch(settings, created_run: dict[str, str]) -> None:
    st.subheader("Launch with Slurm")

    metadata_path = Path(created_run["metadata_path"])
    try:
        metadata = load_run_metadata(metadata_path)
    except RunLaunchError as exc:
        st.error(str(exc))
        return

    if metadata.get("status") != "created" or metadata.get("snakemake_launched"):
        _render_slurm_status_panel(
            settings,
            metadata_path,
            metadata,
            key_suffix=f"created_{metadata.get('run_id', 'run')}",
            show_details=settings.local_dev_mode,
        )
        return

    try:
        plan = build_slurm_launch_plan(
            settings=settings,
            metadata_path=metadata_path,
        )
    except RunLaunchError as exc:
        st.error(str(exc))
        return

    st.markdown(
        f"**Resources:** {int(settings.default_cpus)} CPU, "
        f"{int(settings.default_memory_gb)} GB RAM"
    )
    st.markdown("**Command preview**")
    st.code(shlex.join(plan.sbatch_command), language="bash")
    st.caption(f"Launch working directory: `{plan.paths.work_dir}`")

    if st.button("Submit Slurm job", width="stretch"):
        try:
            result = launch_slurm_run(
                settings=settings,
                metadata_path=metadata_path,
            )
        except RunLaunchError as exc:
            st.error(str(exc))
        except Exception as exc:
            st.error(f"Could not submit Slurm job: {exc}")
        else:
            if result.slurm_job_id:
                st.success(f"Slurm job submitted: `{result.slurm_job_id}`")
            else:
                st.success("Slurm job submitted.")
            refreshed_metadata = load_run_metadata(metadata_path)
            _render_slurm_status_panel(
                settings,
                metadata_path,
                refreshed_metadata,
                key_suffix=f"created_{refreshed_metadata.get('run_id', 'run')}",
                show_details=settings.local_dev_mode,
            )


def _summary_table(summaries: list[RunSummary]) -> list[dict[str, Any]]:
    return [
        {
            "run_id": item.run_id,
            "status": item.status,
            "nspecies": item.nspecies,
            "nameA": item.nameA,
            "nameB": item.nameB,
            "nameOutgroup": item.nameOutgroup,
            "created_at": item.created_at,
            "submitted_at": item.submitted_at,
            "status_checked_at": item.status_checked_at,
            "slurm_job_id": item.slurm_job_id,
            "final_archive_exists": item.final_archive_exists,
        }
        for item in summaries
    ]


def _render_existing_runs(settings, *, in_expander: bool = True) -> None:
    container = st.expander("Existing runs", expanded=False) if in_expander else st.container()
    with container:
        if not settings.local_dev_mode:
            st.info(
                "Existing-run browsing is disabled in deployment mode until "
                "authenticated run ownership is implemented."
            )
            return

        st.caption(
            "Local dev/admin only. This lists run metadata under the configured runs_root."
        )
        summaries = list_existing_runs(settings)
        if not summaries:
            st.info("No existing app-managed runs found.")
            return

        st.dataframe(_summary_table(summaries), width="stretch", hide_index=True)
        labels = [
            f"{item.run_id} · {item.status} · job {item.slurm_job_id or 'none'}"
            for item in summaries
        ]
        selected_label = st.selectbox("Select existing run", labels)
        selected = summaries[labels.index(selected_label)]

        try:
            metadata = load_run_metadata(selected.metadata_path)
        except RunLaunchError as exc:
            st.error(str(exc))
            return

        st.markdown("**Metadata path**")
        st.code(str(selected.metadata_path), language=None)
        if metadata.get("slurm_job_id"):
            _render_slurm_status_panel(
                settings,
                selected.metadata_path,
                metadata,
                key_suffix=f"existing_{selected.run_id}",
                show_details=True,
            )
        else:
            _render_job_status(metadata, show_details=True)


def render_existing_runs_page() -> None:
    try:
        settings = load_settings()
    except Exception as exc:
        st.error(f"Could not load run settings: {exc}")
        return
    _render_existing_runs(settings, in_expander=False)


def _submit_analysis(
    settings,
    paths: RunPaths,
    uploaded_file,
    yaml_text: str,
    metadata: dict[str, Any],
) -> dict[str, str] | None:
    try:
        created = create_run_files(
            paths=paths,
            uploaded_file=uploaded_file,
            yaml_text=yaml_text,
            metadata=metadata,
        )
    except RunCreationError as exc:
        st.error(str(exc))
        return None
    except Exception as exc:
        st.error(f"Could not create run files: {exc}")
        return None

    created_run = {
        "run_id": paths.run_id,
        "uploaded_fasta_path": str(created.uploaded_fasta_path),
        "config_yaml_path": str(created.config_yaml_path),
        "metadata_path": str(created.metadata_path),
    }
    st.session_state[_CREATED_RUN_KEY] = created_run

    try:
        result = launch_slurm_run(
            settings=settings,
            metadata_path=created.metadata_path,
        )
    except RunLaunchError as exc:
        mark_submission_failed(
            created.metadata_path,
            str(exc),
            int(settings.default_cpus),
            int(settings.default_memory_gb),
        )
        st.error("Run files were created, but Slurm submission failed.")
        if settings.local_dev_mode:
            st.code(str(exc), language=None)
        return created_run
    except Exception as exc:
        mark_submission_failed(
            created.metadata_path,
            str(exc),
            int(settings.default_cpus),
            int(settings.default_memory_gb),
        )
        st.error("Run files were created, but Slurm submission failed.")
        if settings.local_dev_mode:
            st.code(str(exc), language=None)
        return created_run

    if result.slurm_job_id:
        st.success(f"DILS analysis submitted. Slurm job ID: `{result.slurm_job_id}`")
    else:
        st.success("DILS analysis submitted.")
    st.info("Results will be available after the Slurm job completes.")
    return created_run


def _build_values(
    defaults: dict[str, Any],
    n_species: int,
    paths: RunPaths,
    selected: dict[str, Any],
) -> dict[str, Any]:
    values = {
        "mail_address": selected["mail_address"],
        "infile": str(paths.uploaded_fasta_path),
        "region": selected["region"],
        "nspecies": n_species,
        "nameA": selected["nameA"],
        "nameOutgroup": selected["nameOutgroup"],
        "lightMode": True,
        "config_yaml": str(paths.config_yaml_path),
        "timeStamp": paths.run_id,
        "max_N_tolerated": selected["max_N_tolerated"],
        "Lmin": selected["Lmin"],
        "nMin": selected["nMin"],
        "mu": selected["mu"],
        "rho_over_theta": selected["rho_over_theta"],
        "N_min": selected["N_min"],
        "N_max": selected["N_max"],
    }
    if n_species == 1:
        values.update(
            {
                "Tchanges_min": selected["Tchanges_min"],
                "Tchanges_max": selected["Tchanges_max"],
            }
        )
    else:
        values.update(
            {
                "nameB": selected["nameB"],
                "useSFS": selected["useSFS"],
                "population_growth": selected["population_growth"],
                "modeBarrier": selected["modeBarrier"],
                "Tsplit_min": selected["Tsplit_min"],
                "Tsplit_max": selected["Tsplit_max"],
                "M_min": selected["M_min"],
                "M_max": selected["M_max"],
            }
        )
    return values


def render() -> None:
    try:
        settings = load_settings()
        defaults_by_species = load_canonical_defaults()
    except Exception as exc:
        st.error(f"Could not load run settings or YAML defaults: {exc}")
        return

    run_id = _ensure_run_id(settings.run_id_prefix)
    if settings.local_dev_mode:
        c1, c2 = st.columns([3, 1])
        c1.markdown(f"**Planned run ID / timeStamp:** `{run_id}`")
        if c2.button("Regenerate run ID", width="stretch"):
            st.session_state[_RUN_ID_KEY] = generate_run_id(settings.run_id_prefix)
            st.session_state.pop(_CREATED_RUN_KEY, None)
            st.rerun()

    created_run = st.session_state.get(_CREATED_RUN_KEY)
    created_current_run = created_run is not None and created_run.get("run_id") == run_id
    if created_current_run:
        with st.container(border=True):
            st.success("DILS analysis has already been prepared for this run.")
            if settings.local_dev_mode:
                st.warning(
                    "Generate a new run ID before changing options for another run."
                )
                _created_paths_display(created_run)
            metadata_path = Path(created_run["metadata_path"])
            try:
                metadata = load_run_metadata(metadata_path)
            except RunLaunchError as exc:
                st.error(str(exc))
            else:
                _render_slurm_status_panel(
                    settings,
                    metadata_path,
                    metadata,
                    key_suffix=f"active_{metadata.get('run_id', 'run')}",
                    show_details=settings.local_dev_mode,
                )

    with st.container(border=True):
        st.subheader("FASTA and populations")
        uploaded = st.file_uploader(
            "FASTA file",
            type=["fa", "fas", "fasta", "txt"],
            help="The file is saved only when you submit the analysis.",
            disabled=created_current_run,
        )
        if created_current_run:
            if settings.local_dev_mode:
                st.info("Use **Regenerate run ID** to prepare another run.")
            return

        if uploaded is None:
            st.info("Upload a FASTA file to start a DILS analysis.")
            return

        paths = plan_run_paths(settings, run_id=run_id, fasta_filename=uploaded.name)

        detected = _parse_uploaded_fasta(uploaded)
        st.write(
            f"Valid headers: {detected.valid_headers} · "
            f"Malformed headers ignored: {detected.malformed_headers}"
        )
        if not detected.populations:
            st.warning("No population names were detected; the analysis cannot be submitted.")
            if settings.local_dev_mode:
                st.subheader("Planned paths")
                _path_table(paths)
            return

        st.dataframe(
            {"Population": detected.populations},
            width="stretch",
            hide_index=True,
        )

    with st.container(border=True):
        st.subheader("Analysis settings")
        c1, c2 = st.columns(2)
        with c1:
            analysis_label = st.radio(
                "Analysis type",
                ["1 population", "2 populations"],
                horizontal=True,
            )
        n_species = 1 if analysis_label == "1 population" else 2
        defaults = defaults_by_species[n_species]
        with c2:
            region = st.selectbox(
                "region",
                ["coding", "noncoding"],
                index=0 if defaults.get("region") == "coding" else 1,
            )

        if n_species == 2 and len(detected.populations) < 2:
            st.warning("At least two detected populations are required for a 2-population run.")
            if settings.local_dev_mode:
                st.subheader("Planned paths")
                _path_table(paths)
            return

        c1, c2, c3 = st.columns(3)
        with c1:
            name_a_default = defaults.get("nameA")
            name_a_index = detected.populations.index(name_a_default) if name_a_default in detected.populations else 0
            name_a = st.selectbox("nameA", detected.populations, index=name_a_index)
        name_b = ""
        with c2:
            if n_species == 2:
                name_b_default = defaults.get("nameB")
                name_b_index = detected.populations.index(name_b_default) if name_b_default in detected.populations else min(1, len(detected.populations) - 1)
                name_b = st.selectbox("nameB", detected.populations, index=name_b_index)
                if name_a == name_b:
                    st.error("nameA and nameB must be different for a 2-population run.")
        with c3:
            use_outgroup = st.checkbox("Use outgroup", value=False)
            if use_outgroup:
                selected_names = {name_a}
                if n_species == 2:
                    selected_names.add(name_b)
                outgroup_options = [pop for pop in detected.populations if pop not in selected_names]
                if outgroup_options:
                    name_outgroup = st.selectbox("nameOutgroup", outgroup_options)
                else:
                    name_outgroup = "NA"
                    st.warning("No detected population remains available as outgroup.")
            else:
                name_outgroup = "NA"
            st.markdown("**lightMode:** `true`")

        use_sfs = None
        population_growth = None
        mode_barrier = None
        if n_species == 2:
            st.subheader("2-population options")
            c1, c2, c3 = st.columns(3)
            with c1:
                use_sfs = st.checkbox(
                    "useSFS",
                    value=_as_bool(defaults.get("useSFS"), default=True),
                )
            with c2:
                population_growth = st.selectbox(
                    "population_growth",
                    ["constant", "variable"],
                    index=0 if defaults.get("population_growth") == "constant" else 1,
                )
            with c3:
                mode_barrier = st.selectbox(
                    "modeBarrier",
                    ["bimodal", "beta"],
                    index=0 if defaults.get("modeBarrier") == "bimodal" else 1,
                )

        with st.expander("Filtering", expanded=False):
            c1, c2, c3 = st.columns(3)
            with c1:
                max_n_tolerated = st.number_input(
                    "max_N_tolerated",
                    value=_as_float(defaults.get("max_N_tolerated"), 0.2),
                )
            with c2:
                lmin = st.number_input("Lmin", value=_as_int(defaults.get("Lmin"), 100))
            with c3:
                nmin = st.number_input("nMin", value=_as_int(defaults.get("nMin"), 6))

        with st.expander("Priors", expanded=False):
            c1, c2, c3 = st.columns(3)
            with c1:
                mu = st.number_input("mu", value=_as_float(defaults.get("mu"), 1e-8))
                n_min = st.number_input("N_min", value=_as_int(defaults.get("N_min"), 0))
            with c2:
                rho_over_theta = st.number_input(
                    "rho_over_theta",
                    value=_as_float(defaults.get("rho_over_theta"), 0.5),
                )
                n_max = st.number_input("N_max", value=_as_int(defaults.get("N_max"), 100000))
            with c3:
                if n_species == 1:
                    t_min = st.number_input(
                        "Tchanges_min",
                        value=_as_int(defaults.get("Tchanges_min"), 0),
                    )
                    t_max = st.number_input(
                        "Tchanges_max",
                        value=_as_int(defaults.get("Tchanges_max"), 200000),
                    )
                else:
                    t_min = st.number_input(
                        "Tsplit_min",
                        value=_as_int(defaults.get("Tsplit_min"), 10000),
                    )
                    t_max = st.number_input(
                        "Tsplit_max",
                        value=_as_int(defaults.get("Tsplit_max"), 1750000),
                    )
                    m_min = st.number_input("M_min", value=_as_int(defaults.get("M_min"), 1))
                    m_max = st.number_input("M_max", value=_as_int(defaults.get("M_max"), 40))

    with st.container(border=True):
        email_address = st.text_input(
            "Email address",
            value="",
            help=(
                "Required when server-side notifications are enabled. "
                "Used for the DILS mail_address field."
            ),
        ).strip()
        email_required_missing = settings.notifications_enabled and (
            not email_address or "@" not in email_address
        )
        if email_required_missing:
            st.warning(
                "Enter a valid email address containing `@` before submitting. "
                "Notifications are enabled on this server."
            )

    if settings.local_dev_mode:
        st.subheader("Planned paths")
        _path_table(paths)

    if n_species == 2 and name_a == name_b:
        st.warning("Fix nameA/nameB before generating the final YAML preview.")
        return

    selected = {
        "mail_address": email_address,
        "region": region,
        "nameA": name_a,
        "nameB": name_b,
        "nameOutgroup": name_outgroup,
        "useSFS": bool(use_sfs),
        "population_growth": population_growth,
        "modeBarrier": mode_barrier,
        "max_N_tolerated": max_n_tolerated,
        "Lmin": lmin,
        "nMin": nmin,
        "mu": mu,
        "rho_over_theta": rho_over_theta,
        "N_min": n_min,
        "N_max": n_max,
    }
    if n_species == 1:
        selected.update({"Tchanges_min": t_min, "Tchanges_max": t_max})
    else:
        selected.update(
            {
                "Tsplit_min": t_min,
                "Tsplit_max": t_max,
                "M_min": m_min,
                "M_max": m_max,
            }
        )

    yaml_values = _build_values(defaults, n_species, paths, selected)
    yaml_text = serialize_yaml(yaml_values, n_species)
    if settings.local_dev_mode:
        st.subheader("Final YAML preview")
        st.code(yaml_text, language="yaml")

    metadata = {
        "analysis_type": analysis_label,
        "nspecies": n_species,
        "nameA": name_a,
        "nameOutgroup": name_outgroup,
        "mail_address": str(yaml_values.get("mail_address") or ""),
        "uploaded_fasta_original_name": uploaded.name,
    }
    if n_species == 2:
        metadata["nameB"] = name_b

    with st.container(border=True):
        st.subheader("Submit and status")
        st.markdown(
            f"**Resources:** {int(settings.default_cpus)} CPU, "
            f"{int(settings.default_memory_gb)} GB RAM"
        )

        if st.button(
            "Submit DILS analysis",
            type="primary",
            width="stretch",
            disabled=email_required_missing,
        ):
            created_run = _submit_analysis(
                settings=settings,
                paths=paths,
                uploaded_file=uploaded,
                yaml_text=yaml_text,
                metadata=metadata,
            )
            if created_run is not None:
                metadata_path = Path(created_run["metadata_path"])
                try:
                    refreshed_metadata = load_run_metadata(metadata_path)
                except RunLaunchError as exc:
                    st.error(str(exc))
                else:
                    if settings.local_dev_mode:
                        _created_paths_display(created_run)
                        if refreshed_metadata.get("sbatch_command"):
                            st.markdown("**Command preview**")
                            st.code(str(refreshed_metadata["sbatch_command"]), language="bash")
                    _render_slurm_status_panel(
                        settings,
                        metadata_path,
                        refreshed_metadata,
                        key_suffix=f"submitted_{refreshed_metadata.get('run_id', 'run')}",
                        show_details=settings.local_dev_mode,
                    )
