"""YAML configuration builder for DILS runs."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import streamlit as st
import yaml

from fasta_headers import FastaHeaderSummary, parse_fasta_headers
from settings import load_settings

REPO_ROOT = Path(__file__).resolve().parents[2]
CONFIG_1POP = REPO_ROOT / "bin" / "example_config_1pop.yaml"
CONFIG_2POP = REPO_ROOT / "bin" / "example_config_2pop.yaml"

_MANUAL_NAME_MODE = "Enter names manually"
_DETECT_NAME_MODE = "Detect names from FASTA headers"
_FASTA_CACHE_KEY = "fasta_header_summary_cache"

FIELDS_1POP = [
    "mail_address",
    "infile",
    "region",
    "nspecies",
    "nameA",
    "nameOutgroup",
    "lightMode",
    "config_yaml",
    "timeStamp",
    "max_N_tolerated",
    "Lmin",
    "nMin",
    "mu",
    "rho_over_theta",
    "N_min",
    "N_max",
    "Tchanges_min",
    "Tchanges_max",
]

FIELDS_2POP = [
    "mail_address",
    "infile",
    "region",
    "nspecies",
    "nameA",
    "nameB",
    "nameOutgroup",
    "lightMode",
    "useSFS",
    "config_yaml",
    "timeStamp",
    "population_growth",
    "modeBarrier",
    "max_N_tolerated",
    "Lmin",
    "nMin",
    "mu",
    "rho_over_theta",
    "N_min",
    "N_max",
    "Tsplit_min",
    "Tsplit_max",
    "M_min",
    "M_max",
]


@st.cache_data(show_spinner=False)
def _load_defaults() -> dict[int, dict[str, Any]]:
    try:
        with CONFIG_1POP.open(encoding="utf-8") as f:
            one_pop = yaml.safe_load(f) or {}
        with CONFIG_2POP.open(encoding="utf-8") as f:
            two_pop = yaml.safe_load(f) or {}
    except Exception as exc:
        raise RuntimeError(f"Could not load canonical YAML defaults: {exc}") from exc
    return {1: one_pop, 2: two_pop}


def _clean_timestamp(value: str) -> str:
    cleaned = (value or "").strip().replace(" ", "_")
    cleaned = cleaned.replace("/", "_").replace("\\", "_")
    return cleaned or "dils_run"


def _default_run_directory() -> str:
    try:
        return str(Path.cwd().resolve())
    except OSError:
        return str(REPO_ROOT)


def _resolve_run_directory(value: str) -> Path:
    raw = (value or "").strip()
    path = Path(raw) if raw else REPO_ROOT
    return path.resolve()


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


def _ordered_yaml(values: dict[str, Any], fields: list[str]) -> str:
    ordered = {field: values[field] for field in fields}
    return yaml.safe_dump(ordered, sort_keys=False, default_flow_style=False)


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


def _manual_or_detected_name(
    label: str,
    default: str,
    populations: list[str],
    key: str,
) -> str:
    if not populations:
        return st.text_input(label, value=default, key=f"{key}_manual")

    manual_label = "Manual entry"
    options = populations + [manual_label]
    index = populations.index(default) if default in populations else 0
    choice = st.selectbox(label, options, index=index, key=f"{key}_select")
    if choice == manual_label:
        return st.text_input(label, value=default, key=f"{key}_manual")
    return choice


def _outgroup_name_input(
    default: str,
    populations: list[str],
    selected_names: set[str],
) -> str:
    if not populations:
        return st.text_input(
            "nameOutgroup",
            value="" if default == "NA" else default,
        )

    candidates = [pop for pop in populations if pop not in selected_names]
    manual_label = "Manual entry"
    options = candidates + [manual_label]
    if default in candidates:
        index = candidates.index(default)
    else:
        index = 0 if candidates else len(options) - 1
    choice = st.selectbox("nameOutgroup", options, index=index)
    if choice == manual_label:
        return st.text_input(
            "nameOutgroup",
            value="" if default == "NA" else default,
            key="outgroup_manual",
        )
    return choice


def render() -> None:
    st.caption(
        "Build a DILS configuration YAML. This preview does not launch an analysis "
        "and does not write files to disk."
    )

    try:
        defaults_by_species = _load_defaults()
    except RuntimeError as exc:
        st.error(str(exc))
        return

    analysis_label = st.radio(
        "Analysis type",
        ["1 population", "2 populations"],
        horizontal=True,
    )
    n_species = 1 if analysis_label == "1 population" else 2
    defaults = defaults_by_species[n_species]

    st.info(
        "For now, save or use the downloaded YAML manually. FASTA upload and run "
        "management will be handled later."
    )
    with st.expander("Current run settings", expanded=False):
        try:
            app_settings = load_settings()
        except Exception as exc:
            st.error(f"Could not load app settings: {exc}")
        else:
            st.table(
                {
                    "Setting": [
                        "runs_root",
                        "dils_bin_dir",
                        "snakefile_1pop",
                        "snakefile_2pop",
                    ],
                    "Value": [
                        str(app_settings.runs_root),
                        str(app_settings.dils_bin_dir),
                        str(app_settings.snakefile_1pop),
                        str(app_settings.snakefile_2pop),
                    ],
                }
            )

    name_input_mode = st.radio(
        "Population name input mode",
        [_MANUAL_NAME_MODE, _DETECT_NAME_MODE],
        horizontal=True,
    )

    detected: FastaHeaderSummary | None = None
    use_manual_fallback = name_input_mode == _MANUAL_NAME_MODE
    if name_input_mode == _DETECT_NAME_MODE:
        with st.expander("Detect population names from FASTA headers", expanded=True):
            st.caption(
                "This temporary upload is only for the current manual YAML-builder "
                "prototype and is used only to detect population names. The YAML "
                "`infile` path must still point to the FASTA file available where DILS "
                "will run. In the future run launcher, the FASTA will be uploaded once, "
                "saved in the run directory, used for population detection, and passed "
                "to DILS as `infile`."
            )
            # TODO: The future run manager should avoid double upload by storing the
            # FASTA once, parsing names from that stored file, and setting `infile`
            # to it. Expected upload limit should be admin-configurable, likely ~10 GB.
            fasta_upload = st.file_uploader(
                "FASTA file for name detection",
                type=["fa", "fas", "fasta", "txt"],
            )
            if fasta_upload is not None:
                detected = _parse_uploaded_fasta(fasta_upload)
                st.write(
                    f"Valid headers: {detected.valid_headers} · "
                    f"Malformed headers ignored: {detected.malformed_headers}"
                )
                if detected.populations:
                    st.dataframe(
                        {"Population": detected.populations},
                        width="stretch",
                        hide_index=True,
                    )
                    use_manual_fallback = st.checkbox(
                        "Use manual fallback",
                        value=False,
                        help="Use text fields instead of the detected population selectors.",
                    )
                else:
                    use_manual_fallback = True
                    st.warning(
                        "No population names could be detected. Manual name fields remain available."
                    )
            else:
                use_manual_fallback = True
                st.info(
                    "Upload a FASTA file to select detected names, or switch to manual mode."
                )

    detected_populations = (
        detected.populations
        if detected is not None and not use_manual_fallback
        else []
    )

    c1, c2 = st.columns(2)
    with c1:
        mail_address = st.text_input(
            "mail_address",
            value=str(defaults.get("mail_address", "")),
        )
        infile = st.text_input(
            "infile path",
            value=str(defaults.get("infile", "")),
            help=(
                "Path to an existing FASTA file where DILS will run. "
                "The optional upload above is only for detecting names."
            ),
        )
        time_stamp_raw = st.text_input(
            "timeStamp / run name",
            value=str(defaults.get("timeStamp", "dils_run")),
        )
        run_directory = st.text_input(
            "Intended run/config directory",
            value=_default_run_directory(),
            help=(
                "Directory where this YAML file will be saved and where the "
                "DILS run is expected to be launched."
            ),
        )
        region = st.selectbox(
            "region",
            ["coding", "noncoding"],
            index=0 if defaults.get("region") == "coding" else 1,
        )

    with c2:
        name_a = _manual_or_detected_name(
            "nameA",
            str(defaults.get("nameA", "")),
            detected_populations,
            "name_a",
        )
        name_b = ""
        if n_species == 2:
            name_b = _manual_or_detected_name(
                "nameB",
                str(defaults.get("nameB", "")),
                detected_populations,
                "name_b",
            )
            if name_a == name_b:
                st.warning("For 2-pop analyses, nameA and nameB should be different.")

        default_outgroup = str(defaults.get("nameOutgroup", "NA"))
        has_outgroup = st.checkbox(
            "Use outgroup",
            value=default_outgroup not in {"", "NA"},
        )
        if has_outgroup:
            selected_names = {name_a}
            if n_species == 2:
                selected_names.add(name_b)
            name_outgroup = _outgroup_name_input(
                default_outgroup,
                detected_populations,
                selected_names,
            )
        else:
            name_outgroup = "NA"

        st.markdown("**lightMode:** `true`")
        st.caption("Light mode is fixed for this builder version.")

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
        c1, c2 = st.columns(2)
        with c1:
            mu = st.number_input("mu", value=_as_float(defaults.get("mu"), 1e-8))
            n_min = st.number_input("N_min", value=_as_int(defaults.get("N_min"), 0))
        with c2:
            rho_over_theta = st.number_input(
                "rho_over_theta",
                value=_as_float(defaults.get("rho_over_theta"), 0.5),
            )
            n_max = st.number_input(
                "N_max",
                value=_as_int(defaults.get("N_max"), 100000),
            )

        c1, c2 = st.columns(2)
        if n_species == 1:
            with c1:
                t_min = st.number_input(
                    "Tchanges_min",
                    value=_as_int(defaults.get("Tchanges_min"), 0),
                )
            with c2:
                t_max = st.number_input(
                    "Tchanges_max",
                    value=_as_int(defaults.get("Tchanges_max"), 200000),
                )
        else:
            with c1:
                t_min = st.number_input(
                    "Tsplit_min",
                    value=_as_int(defaults.get("Tsplit_min"), 10000),
                )
                m_min = st.number_input(
                    "M_min",
                    value=_as_int(defaults.get("M_min"), 1),
                )
            with c2:
                t_max = st.number_input(
                    "Tsplit_max",
                    value=_as_int(defaults.get("Tsplit_max"), 1750000),
                )
                m_max = st.number_input(
                    "M_max",
                    value=_as_int(defaults.get("M_max"), 40),
                )

    time_stamp = _clean_timestamp(time_stamp_raw)
    config_yaml = str(_resolve_run_directory(run_directory) / f"{time_stamp}.yaml")
    values: dict[str, Any] = {
        "mail_address": mail_address,
        "infile": infile,
        "region": region,
        "nspecies": n_species,
        "nameA": name_a,
        "nameOutgroup": name_outgroup or "NA",
        "lightMode": True,
        "config_yaml": config_yaml,
        "timeStamp": time_stamp,
        "max_N_tolerated": max_n_tolerated,
        "Lmin": lmin,
        "nMin": nmin,
        "mu": mu,
        "rho_over_theta": rho_over_theta,
        "N_min": n_min,
        "N_max": n_max,
    }

    if n_species == 1:
        values.update(
            {
                "Tchanges_min": t_min,
                "Tchanges_max": t_max,
            }
        )
        fields = FIELDS_1POP
    else:
        values.update(
            {
                "nameB": name_b,
                "useSFS": bool(use_sfs),
                "population_growth": population_growth,
                "modeBarrier": mode_barrier,
                "Tsplit_min": t_min,
                "Tsplit_max": t_max,
                "M_min": m_min,
                "M_max": m_max,
            }
        )
        fields = FIELDS_2POP

    yaml_text = _ordered_yaml(values, fields)

    with st.container(border=True):
        st.subheader("YAML preview")
        st.caption(
            "Save the downloaded YAML at the path shown in `config_yaml` before "
            "using it with DILS."
        )
        st.code(yaml_text, language="yaml")
        st.download_button(
            "Download YAML",
            data=yaml_text,
            file_name=f"{time_stamp}.yaml",
            mime="application/x-yaml",
        )
