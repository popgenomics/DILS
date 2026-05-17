"""Pure helpers for future app-managed DILS runs."""

from __future__ import annotations

import json
import re
import secrets
import shlex
import subprocess
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, BinaryIO

from notifications import write_lifecycle_notification
from settings import AppSettings, load_settings

_SAFE_TOKEN_RE = re.compile(r"[^A-Za-z0-9_-]+")


@dataclass(frozen=True)
class RunPaths:
    run_id: str
    run_dir: Path
    input_dir: Path
    config_dir: Path
    logs_dir: Path
    slurm_logs_dir: Path
    snakemake_logs_dir: Path
    work_dir: Path
    results_dir: Path
    metadata_dir: Path
    uploaded_fasta_path: Path
    config_yaml_path: Path
    expected_archive_path: Path


@dataclass(frozen=True)
class CreatedRunFiles:
    metadata_path: Path
    uploaded_fasta_path: Path
    config_yaml_path: Path


@dataclass(frozen=True)
class RunSummary:
    run_id: str
    status: str
    nspecies: int | None
    nameA: str
    nameB: str | None
    nameOutgroup: str
    created_at: str | None
    submitted_at: str | None
    status_checked_at: str | None
    slurm_job_id: str | None
    final_archive_exists: bool
    metadata_path: Path


@dataclass(frozen=True)
class SlurmLaunchPlan:
    paths: RunPaths
    metadata_path: Path
    snakefile_path: Path
    stdout_log: Path
    stderr_log: Path
    snakemake_command: list[str]
    sbatch_command: list[str]


@dataclass(frozen=True)
class SlurmLaunchResult:
    metadata_path: Path
    slurm_job_id: str | None
    stdout: str
    stderr: str
    plan: SlurmLaunchPlan


@dataclass(frozen=True)
class SlurmStatus:
    job_id: str
    state: str | None
    exit_code: str | None
    warning: str | None = None


class RunCreationError(RuntimeError):
    """Raised when run file creation would be unsafe or fails."""


class RunLaunchError(RuntimeError):
    """Raised when Slurm launch would be unsafe or fails."""


def _sanitize_token(value: str, default: str) -> str:
    cleaned = _SAFE_TOKEN_RE.sub("_", value.strip())
    cleaned = cleaned.strip("_")
    return cleaned or default


def _sanitize_fasta_filename(value: str | None, run_id: str) -> str:
    raw = (value or "").strip().replace("\\", "/")
    name = Path(raw).name
    if not name:
        return f"{run_id}.fasta"

    path_name = Path(name)
    stem = _sanitize_token(path_name.stem, run_id)
    suffix_text = path_name.suffix.lstrip(".")
    suffix_token = _sanitize_token(suffix_text, "") if suffix_text else ""
    suffix = f".{suffix_token}" if suffix_token else ""
    return f"{stem}{suffix}" if suffix else stem


def _ensure_within(path: Path, parent: Path, label: str) -> None:
    try:
        path.resolve().relative_to(parent.resolve())
    except ValueError as exc:
        raise RunCreationError(f"Unsafe {label} path: {path}") from exc


def _write_uploaded_file(uploaded_file: BinaryIO, destination: Path) -> None:
    seek = getattr(uploaded_file, "seek", None)
    if callable(seek):
        seek(0)

    with destination.open("xb") as out:
        while True:
            chunk = uploaded_file.read(1024 * 1024)
            if not chunk:
                break
            out.write(chunk)


def _metadata_path(paths: RunPaths) -> Path:
    return paths.metadata_dir / "metadata.json"


def _path_from_metadata(metadata: dict[str, Any], key: str) -> Path | None:
    value = metadata.get(key)
    if not value:
        return None
    return Path(str(value)).expanduser().resolve()


def _parse_slurm_job_id(stdout: str) -> str | None:
    first_line = stdout.strip().splitlines()[0] if stdout.strip() else ""
    if not first_line:
        return None
    return first_line.split(";", 1)[0] or None


def _require_file(path: Path, label: str) -> None:
    if not path.is_file():
        raise RunLaunchError(f"{label} does not exist: {path}")


def _require_dir(path: Path, label: str) -> None:
    if not path.is_dir():
        raise RunLaunchError(f"{label} does not exist: {path}")


def _resolved_slurm_log_paths(paths: RunPaths, job_id: str) -> tuple[Path, Path]:
    return (
        paths.slurm_logs_dir / f"slurm-{paths.run_id}-{job_id}.out",
        paths.slurm_logs_dir / f"slurm-{paths.run_id}-{job_id}.err",
    )


def generate_run_id(prefix: str) -> str:
    """Generate a filesystem-safe DILS run ID / timeStamp value."""
    safe_prefix = _sanitize_token(prefix, "dils")
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    suffix = secrets.token_hex(3)
    return f"{safe_prefix}_{stamp}_{suffix}"


def plan_run_paths(
    settings: AppSettings,
    run_id: str | None = None,
    fasta_filename: str | None = None,
) -> RunPaths:
    """Return the future run path layout without creating directories."""
    safe_run_id = _sanitize_token(run_id or generate_run_id(settings.run_id_prefix), "dils")
    run_dir = settings.runs_root / safe_run_id
    input_dir = run_dir / "input"
    config_dir = run_dir / "config"
    logs_dir = run_dir / "logs"
    slurm_logs_dir = logs_dir / "slurm"
    snakemake_logs_dir = logs_dir / "snakemake"
    work_dir = run_dir / "work"
    results_dir = run_dir / "results"
    metadata_dir = run_dir / "metadata"
    fasta_name = _sanitize_fasta_filename(fasta_filename, safe_run_id)

    return RunPaths(
        run_id=safe_run_id,
        run_dir=run_dir,
        input_dir=input_dir,
        config_dir=config_dir,
        logs_dir=logs_dir,
        slurm_logs_dir=slurm_logs_dir,
        snakemake_logs_dir=snakemake_logs_dir,
        work_dir=work_dir,
        results_dir=results_dir,
        metadata_dir=metadata_dir,
        uploaded_fasta_path=input_dir / fasta_name,
        config_yaml_path=config_dir / f"{safe_run_id}.yaml",
        expected_archive_path=work_dir / f"{safe_run_id}.tar.gz",
    )


def load_run_metadata(metadata_path: Path) -> dict[str, Any]:
    """Load an app-managed run metadata.json file."""
    if not metadata_path.is_file():
        raise RunLaunchError(f"metadata.json does not exist: {metadata_path}")

    try:
        loaded = json.loads(metadata_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise RunLaunchError(f"metadata.json is not valid JSON: {metadata_path}") from exc

    if not isinstance(loaded, dict):
        raise RunLaunchError(f"metadata.json must contain a JSON object: {metadata_path}")
    return loaded


def write_run_metadata(metadata_path: Path, metadata: dict[str, Any]) -> None:
    """Write metadata through the controlled run-manager path."""
    metadata_path.write_text(
        json.dumps(metadata, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def update_run_metadata(metadata_path: Path, updates: dict[str, Any]) -> dict[str, Any]:
    """Load metadata, apply explicit updates, write it back, and return it."""
    metadata = load_run_metadata(metadata_path)
    metadata.update(updates)
    write_run_metadata(metadata_path, metadata)
    return metadata


def _try_write_lifecycle_notification(
    settings: AppSettings,
    metadata_path: Path,
    metadata: dict[str, Any],
    event: str,
) -> dict[str, Any]:
    """Best-effort notification write; never interrupt run lifecycle operations."""
    try:
        notification_updates = write_lifecycle_notification(
            settings=settings,
            metadata_path=metadata_path,
            metadata=metadata,
            event=event,
        )
    except Exception as exc:
        notification_updates = {"notification_last_error": str(exc)}

    if not notification_updates:
        return metadata

    try:
        return update_run_metadata(metadata_path, notification_updates)
    except Exception:
        return metadata


def mark_submission_failed(
    metadata_path: Path,
    error: str,
    cpus: int,
    memory_gb: int,
) -> dict[str, Any]:
    """Mark a created run whose files exist but Slurm submission failed."""
    updated_metadata = update_run_metadata(
        metadata_path,
        {
            "status": "submission_failed",
            "snakemake_launched": False,
            "submission_failed_at": datetime.now(timezone.utc).isoformat(),
            "submission_error": error,
            "cpus": cpus,
            "memory_gb": memory_gb,
            "slurm_memory": f"{memory_gb}G",
        },
    )
    try:
        settings = load_settings()
    except Exception:
        return updated_metadata
    return _try_write_lifecycle_notification(
        settings,
        metadata_path,
        updated_metadata,
        "failed",
    )


def _summary_from_metadata(metadata_path: Path, metadata: dict[str, Any]) -> RunSummary:
    nspecies: int | None
    try:
        nspecies = int(metadata["nspecies"]) if metadata.get("nspecies") is not None else None
    except (TypeError, ValueError):
        nspecies = None

    return RunSummary(
        run_id=str(metadata.get("run_id") or metadata_path.parent.parent.name),
        status=str(metadata.get("status") or "unknown"),
        nspecies=nspecies,
        nameA=str(metadata.get("nameA") or ""),
        nameB=str(metadata["nameB"]) if metadata.get("nameB") else None,
        nameOutgroup=str(metadata.get("nameOutgroup") or ""),
        created_at=(
            str(metadata["created_at"]) if metadata.get("created_at") else None
        ),
        submitted_at=(
            str(metadata["submitted_at"]) if metadata.get("submitted_at") else None
        ),
        status_checked_at=(
            str(metadata["status_checked_at"])
            if metadata.get("status_checked_at")
            else None
        ),
        slurm_job_id=(
            str(metadata["slurm_job_id"]) if metadata.get("slurm_job_id") else None
        ),
        final_archive_exists=bool(metadata.get("final_archive_exists")),
        metadata_path=metadata_path,
    )


def list_existing_runs(settings: AppSettings) -> list[RunSummary]:
    """Return compact run summaries from runs_root without mutating metadata."""
    if not settings.runs_root.is_dir():
        return []

    summaries: list[RunSummary] = []
    for run_dir in settings.runs_root.iterdir():
        if not run_dir.is_dir():
            continue
        metadata_path = run_dir / "metadata" / "metadata.json"
        if not metadata_path.is_file():
            continue
        try:
            metadata = load_run_metadata(metadata_path)
            summaries.append(_summary_from_metadata(metadata_path, metadata))
        except RunLaunchError:
            continue

    return sorted(
        summaries,
        key=lambda item: item.created_at or item.submitted_at or item.run_id,
        reverse=True,
    )


def reconstruct_run_paths(
    settings: AppSettings,
    metadata: dict[str, Any] | None = None,
    run_id: str | None = None,
    fasta_filename: str | None = None,
) -> RunPaths:
    """Rebuild the planned path layout for an existing app-managed run."""
    if metadata is not None:
        run_id = str(metadata.get("run_id") or run_id or "")
        saved_fasta = _path_from_metadata(metadata, "uploaded_fasta_saved_path")
        fasta_filename = (
            saved_fasta.name
            if saved_fasta is not None
            else str(metadata.get("uploaded_fasta_original_name") or fasta_filename or "")
        )

    paths = plan_run_paths(settings, run_id=run_id, fasta_filename=fasta_filename)
    if metadata is None:
        return paths

    uploaded_fasta_path = _path_from_metadata(metadata, "uploaded_fasta_saved_path")
    config_yaml_path = _path_from_metadata(metadata, "config_yaml_path")
    work_dir = _path_from_metadata(metadata, "work_dir")
    expected_archive_path = _path_from_metadata(metadata, "expected_archive_path")

    return RunPaths(
        run_id=paths.run_id,
        run_dir=paths.run_dir,
        input_dir=paths.input_dir,
        config_dir=paths.config_dir,
        logs_dir=paths.logs_dir,
        slurm_logs_dir=paths.slurm_logs_dir,
        snakemake_logs_dir=paths.snakemake_logs_dir,
        work_dir=work_dir or paths.work_dir,
        results_dir=paths.results_dir,
        metadata_dir=paths.metadata_dir,
        uploaded_fasta_path=uploaded_fasta_path or paths.uploaded_fasta_path,
        config_yaml_path=config_yaml_path or paths.config_yaml_path,
        expected_archive_path=expected_archive_path or paths.expected_archive_path,
    )


def select_snakefile(settings: AppSettings, nspecies: int) -> Path:
    """Return the configured Snakefile path for a 1- or 2-population run."""
    if nspecies == 1:
        return settings.snakefile_1pop.resolve()
    if nspecies == 2:
        return settings.snakefile_2pop.resolve()
    raise RunLaunchError(f"Unsupported nspecies for launch: {nspecies!r}")


def build_snakemake_command(
    settings: AppSettings,
    snakefile_path: Path,
    config_yaml_path: Path,
    cpus: int,
) -> list[str]:
    """Build the Snakemake argv launched inside the Slurm allocation."""
    if cpus < 1:
        raise RunLaunchError(f"cpus must be at least 1, got {cpus}")

    return [
        settings.snakemake_executable,
        "--snakefile",
        str(snakefile_path.resolve()),
        "-p",
        "-j",
        str(cpus),
        "--configfile",
        str(config_yaml_path.resolve()),
    ]


def build_sbatch_command(
    settings: AppSettings,
    run_id: str,
    cpus: int,
    memory_gb: int,
    stdout_log: Path,
    stderr_log: Path,
    snakemake_command: list[str],
) -> list[str]:
    """Build the sbatch argv without invoking a shell."""
    if cpus < 1:
        raise RunLaunchError(f"cpus must be at least 1, got {cpus}")
    if memory_gb < 1:
        raise RunLaunchError(f"memory_gb must be at least 1, got {memory_gb}")

    return [
        settings.sbatch_executable,
        "--parsable",
        "--ntasks=1",
        f"--cpus-per-task={cpus}",
        f"--mem={memory_gb}G",
        f"--job-name={run_id}",
        f"--output={stdout_log}",
        f"--error={stderr_log}",
        f"--wrap={shlex.join(snakemake_command)}",
    ]


def build_sacct_command(settings: AppSettings, job_id: str) -> list[str]:
    """Build the sacct argv used for metadata-only status refresh."""
    return [
        settings.sacct_executable,
        "--parsable2",
        "--noheader",
        "--jobs",
        job_id,
        "--format=JobID,State,ExitCode",
    ]


def parse_sacct_output(stdout: str, job_id: str) -> SlurmStatus:
    """Parse pipe-separated sacct output, preferring the top-level job row."""
    for line in stdout.splitlines():
        stripped = line.strip()
        if not stripped:
            continue
        parts = stripped.split("|")
        if len(parts) < 3:
            continue

        row_job_id = parts[0].strip()
        if row_job_id != job_id:
            continue

        state = parts[1].strip() or None
        exit_code = parts[2].strip() or None
        return SlurmStatus(job_id=job_id, state=state, exit_code=exit_code)

    return SlurmStatus(
        job_id=job_id,
        state=None,
        exit_code=None,
        warning=f"No top-level sacct row found for Slurm job {job_id}.",
    )


def build_slurm_launch_plan(
    settings: AppSettings,
    metadata_path: Path,
) -> SlurmLaunchPlan:
    """Validate launch preconditions and return the exact commands to run."""
    metadata = load_run_metadata(metadata_path)
    paths = reconstruct_run_paths(settings, metadata=metadata)
    expected_metadata_path = _metadata_path(paths)
    cpus = int(settings.default_cpus)
    memory_gb = int(settings.default_memory_gb)

    _ensure_within(metadata_path, paths.metadata_dir, "metadata")
    _ensure_within(metadata_path, paths.run_dir, "metadata")
    if metadata_path.resolve() != expected_metadata_path.resolve():
        raise RunLaunchError(
            f"metadata.json must be at the planned metadata path: {expected_metadata_path}"
        )

    if metadata.get("status") != "created":
        raise RunLaunchError(
            f"Run status must be 'created' before launch, got {metadata.get('status')!r}"
        )
    if bool(metadata.get("snakemake_launched")):
        raise RunLaunchError("Snakemake has already been launched for this run.")

    nspecies = metadata.get("nspecies")
    try:
        nspecies_int = int(nspecies)
    except (TypeError, ValueError) as exc:
        raise RunLaunchError(f"Invalid nspecies in metadata: {nspecies!r}") from exc

    snakefile_path = select_snakefile(settings, nspecies_int)
    stdout_log = paths.slurm_logs_dir / f"slurm-{paths.run_id}-%j.out"
    stderr_log = paths.slurm_logs_dir / f"slurm-{paths.run_id}-%j.err"

    for path, parent, label in (
        (paths.uploaded_fasta_path, paths.input_dir, "uploaded FASTA"),
        (paths.config_yaml_path, paths.config_dir, "config YAML"),
        (paths.work_dir, paths.run_dir, "work directory"),
        (stdout_log, paths.slurm_logs_dir, "Slurm stdout log"),
        (stderr_log, paths.slurm_logs_dir, "Slurm stderr log"),
    ):
        _ensure_within(path, parent, label)
        _ensure_within(path, paths.run_dir, label)

    _require_file(metadata_path, "metadata.json")
    _require_file(paths.config_yaml_path, "YAML config")
    _require_file(paths.uploaded_fasta_path, "FASTA input")
    _require_dir(paths.work_dir, "work directory")
    _require_file(snakefile_path, "Snakefile")
    _require_dir(paths.slurm_logs_dir, "Slurm log directory")

    snakemake_command = build_snakemake_command(
        settings=settings,
        snakefile_path=snakefile_path,
        config_yaml_path=paths.config_yaml_path,
        cpus=cpus,
    )
    sbatch_command = build_sbatch_command(
        settings=settings,
        run_id=paths.run_id,
        cpus=cpus,
        memory_gb=memory_gb,
        stdout_log=stdout_log,
        stderr_log=stderr_log,
        snakemake_command=snakemake_command,
    )

    return SlurmLaunchPlan(
        paths=paths,
        metadata_path=metadata_path,
        snakefile_path=snakefile_path,
        stdout_log=stdout_log,
        stderr_log=stderr_log,
        snakemake_command=snakemake_command,
        sbatch_command=sbatch_command,
    )


def launch_slurm_run(
    settings: AppSettings,
    metadata_path: Path,
) -> SlurmLaunchResult:
    """Submit an already-created run to Slurm and mark metadata submitted."""
    plan = build_slurm_launch_plan(settings, metadata_path=metadata_path)
    cpus = int(settings.default_cpus)
    memory_gb = int(settings.default_memory_gb)

    try:
        completed = subprocess.run(
            plan.sbatch_command,
            cwd=plan.paths.work_dir,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError as exc:
        raise RunLaunchError(f"Could not execute sbatch: {exc}") from exc

    if completed.returncode != 0:
        detail = (
            f"sbatch failed with exit code {completed.returncode}.\n"
            f"stdout:\n{completed.stdout or '(empty)'}\n"
            f"stderr:\n{completed.stderr or '(empty)'}"
        )
        raise RunLaunchError(detail)

    slurm_job_id = _parse_slurm_job_id(completed.stdout)
    resolved_stdout_log: Path | None = None
    resolved_stderr_log: Path | None = None
    if slurm_job_id:
        resolved_stdout_log, resolved_stderr_log = _resolved_slurm_log_paths(
            plan.paths,
            slurm_job_id,
        )

    updated_metadata = update_run_metadata(
        plan.metadata_path,
        {
            "status": "submitted",
            "snakemake_launched": True,
            "slurm_job_id": slurm_job_id,
            "submitted_at": datetime.now(timezone.utc).isoformat(),
            "command": shlex.join(plan.sbatch_command),
            "snakemake_command": shlex.join(plan.snakemake_command),
            "sbatch_command": shlex.join(plan.sbatch_command),
            "stdout_log": str(plan.stdout_log),
            "stderr_log": str(plan.stderr_log),
            "stdout_log_resolved": (
                str(resolved_stdout_log) if resolved_stdout_log is not None else None
            ),
            "stderr_log_resolved": (
                str(resolved_stderr_log) if resolved_stderr_log is not None else None
            ),
            "cpus": cpus,
            "memory_gb": memory_gb,
            "slurm_memory": f"{memory_gb}G",
            "work_dir": str(plan.paths.work_dir),
        },
    )
    _try_write_lifecycle_notification(
        settings,
        plan.metadata_path,
        updated_metadata,
        "submitted",
    )

    return SlurmLaunchResult(
        metadata_path=plan.metadata_path,
        slurm_job_id=slurm_job_id,
        stdout=completed.stdout,
        stderr=completed.stderr,
        plan=plan,
    )


def run_sacct_status(settings: AppSettings, job_id: str) -> SlurmStatus:
    """Query sacct for a Slurm job without changing scheduler state."""
    command = build_sacct_command(settings, job_id)
    try:
        completed = subprocess.run(
            command,
            shell=False,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError as exc:
        raise RunLaunchError(f"Could not execute sacct: {exc}") from exc

    if completed.returncode != 0:
        detail = (
            f"sacct failed with exit code {completed.returncode}.\n"
            f"stdout:\n{completed.stdout or '(empty)'}\n"
            f"stderr:\n{completed.stderr or '(empty)'}"
        )
        raise RunLaunchError(detail)

    return parse_sacct_output(completed.stdout, job_id)


def _status_from_slurm(
    slurm_state: str | None,
    slurm_exit_code: str | None,
    final_archive_exists: bool,
    current_status: str,
) -> tuple[str, str | None]:
    if not slurm_state:
        return current_status, "Could not parse Slurm state from sacct output."

    state = slurm_state.upper()
    if state == "COMPLETED":
        if slurm_exit_code == "0:0":
            if final_archive_exists:
                return "completed", None
            return "completed_missing_archive", None
        return "failed", None

    if (
        state.startswith("CANCELLED")
        or state
        in {
            "FAILED",
            "TIMEOUT",
            "OUT_OF_MEMORY",
            "NODE_FAIL",
            "PREEMPTED",
            "BOOT_FAIL",
        }
    ):
        return "failed", None

    if state in {"RUNNING", "PENDING", "CONFIGURING", "COMPLETING", "SUSPENDED"}:
        return "running", None

    return current_status, f"Unrecognized Slurm state from sacct: {slurm_state!r}."


def _cleanup_uploaded_fasta_after_completion(
    settings: AppSettings,
    metadata: dict[str, Any],
    paths: RunPaths,
) -> dict[str, Any]:
    """Delete the Streamlit-managed FASTA copy after a completed archived run."""
    if not settings.delete_uploaded_fasta_after_completion:
        return {}
    if metadata.get("status") != "completed":
        return {}
    if not bool(metadata.get("final_archive_exists")):
        return {}
    if bool(metadata.get("uploaded_fasta_deleted")):
        return {}

    final_archive_path = _path_from_metadata(metadata, "final_archive_path")
    if final_archive_path is None or not final_archive_path.is_file():
        return {}

    uploaded_fasta_path = _path_from_metadata(metadata, "uploaded_fasta_saved_path")
    if uploaded_fasta_path is None or not uploaded_fasta_path.is_file():
        return {}

    attempted_at = datetime.now(timezone.utc).isoformat()
    try:
        _ensure_within(uploaded_fasta_path, paths.input_dir, "uploaded FASTA")
        _ensure_within(uploaded_fasta_path, paths.run_dir, "uploaded FASTA")
        uploaded_fasta_path.unlink()
    except Exception as exc:
        return {
            "uploaded_fasta_deleted": False,
            "uploaded_fasta_cleanup_attempted_at": attempted_at,
            "uploaded_fasta_cleanup_error": str(exc),
        }

    return {
        "uploaded_fasta_deleted": True,
        "uploaded_fasta_deleted_at": attempted_at,
        "uploaded_fasta_deleted_path": str(uploaded_fasta_path),
        "uploaded_fasta_cleanup_error": None,
    }


def refresh_slurm_status(
    settings: AppSettings,
    metadata_path: Path,
) -> dict[str, Any]:
    """Refresh Slurm status and final archive fields in metadata.json."""
    metadata = load_run_metadata(metadata_path)
    job_id = str(metadata.get("slurm_job_id") or "").strip()
    if not job_id:
        raise RunLaunchError("Cannot refresh status: metadata has no slurm_job_id.")

    paths = reconstruct_run_paths(settings, metadata=metadata)
    stdout_log_resolved, stderr_log_resolved = _resolved_slurm_log_paths(paths, job_id)
    _ensure_within(stdout_log_resolved, paths.slurm_logs_dir, "resolved Slurm stdout log")
    _ensure_within(stderr_log_resolved, paths.slurm_logs_dir, "resolved Slurm stderr log")
    _ensure_within(paths.expected_archive_path, paths.work_dir, "final archive")
    _ensure_within(paths.expected_archive_path, paths.run_dir, "final archive")

    slurm_status = run_sacct_status(settings, job_id)
    final_archive_exists = paths.expected_archive_path.is_file()
    next_status, status_warning = _status_from_slurm(
        slurm_status.state,
        slurm_status.exit_code,
        final_archive_exists,
        str(metadata.get("status", "submitted")),
    )
    status_warning = slurm_status.warning or status_warning

    updates: dict[str, Any] = {
        "status": next_status,
        "slurm_state": slurm_status.state,
        "slurm_exit_code": slurm_status.exit_code,
        "status_checked_at": datetime.now(timezone.utc).isoformat(),
        "stdout_log_resolved": str(stdout_log_resolved),
        "stderr_log_resolved": str(stderr_log_resolved),
        "final_archive_exists": final_archive_exists,
        "final_archive_path": (
            str(paths.expected_archive_path) if final_archive_exists else None
        ),
    }
    if status_warning:
        updates["status_warning"] = status_warning
    else:
        updates["status_warning"] = None

    updated_metadata = update_run_metadata(metadata_path, updates)
    cleanup_updates = _cleanup_uploaded_fasta_after_completion(
        settings,
        updated_metadata,
        paths,
    )
    if cleanup_updates:
        updated_metadata = update_run_metadata(metadata_path, cleanup_updates)

    if updated_metadata.get("status") == "completed":
        updated_metadata = _try_write_lifecycle_notification(
            settings,
            metadata_path,
            updated_metadata,
            "completed",
        )
    elif updated_metadata.get("status") in {"failed", "completed_missing_archive"}:
        updated_metadata = _try_write_lifecycle_notification(
            settings,
            metadata_path,
            updated_metadata,
            "failed",
        )

    return updated_metadata


def create_run_files(
    paths: RunPaths,
    uploaded_file: BinaryIO,
    yaml_text: str,
    metadata: dict[str, Any],
) -> CreatedRunFiles:
    """
    Create the planned run directory structure and write FASTA/YAML/metadata.

    This function does not launch Snakemake or call sbatch.
    """
    metadata_path = paths.metadata_dir / "metadata.json"

    for path, parent, label in (
        (paths.uploaded_fasta_path, paths.input_dir, "uploaded FASTA"),
        (paths.config_yaml_path, paths.config_dir, "config YAML"),
        (metadata_path, paths.metadata_dir, "metadata"),
    ):
        _ensure_within(path, parent, label)
        _ensure_within(path, paths.run_dir, label)

    if paths.run_dir.exists():
        raise RunCreationError(f"Run directory already exists: {paths.run_dir}")

    for directory in (
        paths.run_dir,
        paths.input_dir,
        paths.config_dir,
        paths.logs_dir,
        paths.slurm_logs_dir,
        paths.snakemake_logs_dir,
        paths.work_dir,
        paths.results_dir,
        paths.metadata_dir,
    ):
        directory.mkdir(parents=True, exist_ok=False)

    if paths.uploaded_fasta_path.exists():
        raise RunCreationError(f"FASTA already exists: {paths.uploaded_fasta_path}")
    if paths.config_yaml_path.exists():
        raise RunCreationError(f"YAML already exists: {paths.config_yaml_path}")
    if metadata_path.exists():
        raise RunCreationError(f"Metadata already exists: {metadata_path}")

    _write_uploaded_file(uploaded_file, paths.uploaded_fasta_path)
    paths.config_yaml_path.write_text(yaml_text, encoding="utf-8")

    final_metadata = {
        **metadata,
        "run_id": paths.run_id,
        "created_at": datetime.now(timezone.utc).isoformat(),
        "uploaded_fasta_saved_path": str(paths.uploaded_fasta_path),
        "config_yaml_path": str(paths.config_yaml_path),
        "work_dir": str(paths.work_dir),
        "expected_archive_path": str(paths.expected_archive_path),
        "status": "created",
        "snakemake_launched": False,
    }
    write_run_metadata(metadata_path, final_metadata)

    return CreatedRunFiles(
        metadata_path=metadata_path,
        uploaded_fasta_path=paths.uploaded_fasta_path,
        config_yaml_path=paths.config_yaml_path,
    )
