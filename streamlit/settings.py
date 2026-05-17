"""Central settings for the Streamlit DILS app."""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

REPO_ROOT = Path(__file__).resolve().parents[1]
SETTINGS_PATH = Path(__file__).with_name("settings.yaml")


@dataclass(frozen=True)
class AppSettings:
    runs_root: Path
    dils_bin_dir: Path
    snakefile_1pop: Path
    snakefile_2pop: Path
    sbatch_executable: str
    sacct_executable: str
    snakemake_executable: str
    default_cpus: int
    default_memory_gb: int
    run_id_prefix: str
    local_dev_mode: bool
    delete_uploaded_fasta_after_completion: bool
    notifications_enabled: bool
    notification_backend: str
    notification_from_email: str
    notification_signature: str
    app_public_url: str
    manual_url: str
    support_contact_text: str


def _default_settings_dict() -> dict[str, Any]:
    dils_bin_dir = REPO_ROOT / "bin"
    return {
        "runs_root": REPO_ROOT / "streamlit_runs",
        "dils_bin_dir": dils_bin_dir,
        "snakefile_1pop": dils_bin_dir / "Snakefile_1pop",
        "snakefile_2pop": dils_bin_dir / "Snakefile_2pop",
        "sbatch_executable": "sbatch",
        "sacct_executable": "sacct",
        "snakemake_executable": "snakemake",
        "default_cpus": 10,
        "default_memory_gb": 10,
        "run_id_prefix": "dils",
        "local_dev_mode": True,
        "delete_uploaded_fasta_after_completion": False,
        "notifications_enabled": False,
        "notification_backend": "file",
        "notification_from_email": "dils <dils@univ-lyon1.fr>",
        "notification_signature": "The DILS team",
        "app_public_url": "",
        "manual_url": "",
        "support_contact_text": (
            "If you encounter any problem or have suggestions for improvement, "
            "please contact the DILS team."
        ),
    }


def _resolve_path(value: Any) -> Path:
    path = Path(str(value)).expanduser()
    if not path.is_absolute():
        path = REPO_ROOT / path
    return path.resolve()


def _load_yaml_overrides() -> dict[str, Any]:
    if not SETTINGS_PATH.is_file():
        return {}
    with SETTINGS_PATH.open(encoding="utf-8") as f:
        loaded = yaml.safe_load(f) or {}
    if not isinstance(loaded, dict):
        raise ValueError(f"{SETTINGS_PATH} must contain a YAML mapping.")
    return loaded


def load_settings() -> AppSettings:
    """Load app settings from defaults, optional YAML, and env overrides."""
    values = _default_settings_dict()
    values.update(_load_yaml_overrides())

    runs_root_env = os.environ.get("DILS_RUNS_ROOT")
    if runs_root_env:
        values["runs_root"] = runs_root_env

    dils_bin_dir = _resolve_path(values["dils_bin_dir"])
    return AppSettings(
        runs_root=_resolve_path(values["runs_root"]),
        dils_bin_dir=dils_bin_dir,
        snakefile_1pop=_resolve_path(
            values.get("snakefile_1pop", dils_bin_dir / "Snakefile_1pop")
        ),
        snakefile_2pop=_resolve_path(
            values.get("snakefile_2pop", dils_bin_dir / "Snakefile_2pop")
        ),
        sbatch_executable=str(values.get("sbatch_executable", "sbatch")),
        sacct_executable=str(values.get("sacct_executable", "sacct")),
        snakemake_executable=str(values.get("snakemake_executable", "snakemake")),
        default_cpus=int(values.get("default_cpus", 10)),
        default_memory_gb=int(values.get("default_memory_gb", 10)),
        run_id_prefix=str(values.get("run_id_prefix", "dils")),
        local_dev_mode=bool(values.get("local_dev_mode", True)),
        delete_uploaded_fasta_after_completion=bool(
            values.get("delete_uploaded_fasta_after_completion", False)
        ),
        notifications_enabled=bool(values.get("notifications_enabled", False)),
        notification_backend=str(values.get("notification_backend", "file")),
        notification_from_email=str(
            values.get("notification_from_email", "dils <dils@univ-lyon1.fr>")
        ),
        notification_signature=str(
            values.get("notification_signature", "The DILS team")
        ),
        app_public_url=str(values.get("app_public_url", "")),
        manual_url=str(values.get("manual_url", "")),
        support_contact_text=str(
            values.get(
                "support_contact_text",
                "If you encounter any problem or have suggestions for improvement, "
                "please contact the DILS team.",
            )
        ),
    )
