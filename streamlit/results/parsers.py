"""Parse DILS result files from an extracted archive root directory."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import pandas as pd
import yaml


@dataclass
class ParsedResults:
    root: Path
    display_name: str
    general_infos: dict[str, str]
    config: dict[str, Any]
    nref: float | None
    hierarchical: pd.DataFrame
    abcstat_loci: pd.DataFrame
    locus_specific: pd.DataFrame
    abcstat_global: pd.DataFrame
    posterior: pd.DataFrame
    prior: pd.DataFrame
    posterior_rf: pd.DataFrame
    posterior_optimized: pd.DataFrame
    parameter_reports: pd.DataFrame
    gof_stats: pd.DataFrame
    gof_stats_optimized: pd.DataFrame
    sfs_gof: pd.DataFrame
    abcjsfs: pd.DataFrame | None
    pca_coords: pd.DataFrame | None
    locus_infos: pd.DataFrame | None

    @property
    def n_species(self) -> int:
        raw = self.general_infos.get("nSpecies", self.general_infos.get("nspecies", 0))
        try:
            return int(str(raw).strip())
        except (TypeError, ValueError):
            return 0

    @property
    def is_one_pop(self) -> bool:
        return self.n_species == 1

    @property
    def is_two_pop(self) -> bool:
        return self.n_species == 2

    @property
    def name_a(self) -> str:
        return self.general_infos.get("nameA", "A")

    @property
    def name_b(self) -> str:
        return self.general_infos.get("nameB", "B")

    @property
    def n_loci(self) -> int:
        try:
            return int(self.general_infos.get("nLoci", 0))
        except (TypeError, ValueError):
            return len(self.abcstat_loci)


def _read_general_infos(path: Path) -> dict[str, str]:
    info = {}
    with path.open(encoding="utf-8", errors="replace") as f:
        for line in f:
            line = line.strip()
            if not line or "," not in line:
                continue
            key, val = line.split(",", 1)
            info[key.strip()] = val.strip()
    return info


def _read_hierarchical(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path, sep="\t", header=None)
    if df.shape[0] < 3:
        raise ValueError(f"Unexpected hierarchical_models format: {path}")
    return df


def _parse_n_species(general_infos: dict[str, str]) -> int:
    raw = general_infos.get("nSpecies", general_infos.get("nspecies"))
    try:
        n_species = int(str(raw).strip())
    except (TypeError, ValueError) as exc:
        raise ValueError(f"Unsupported archive: invalid nSpecies={raw}.") from exc
    if n_species not in {1, 2}:
        raise ValueError(
            "v0 supports 1- or 2-population results only "
            f"(nSpecies={raw})."
        )
    return n_species


def _read_optional_table(path: Path, sep: str = "\t") -> pd.DataFrame:
    return _read_table(path, sep=sep) if path.is_file() else pd.DataFrame()


def _read_table(path: Path, sep: str = "\t") -> pd.DataFrame:
    return pd.read_csv(path, sep=sep, index_col=False)


def _read_parameter_report(path: Path, stage: str) -> pd.DataFrame:
    """Parse 1-pop report_<nameA>.txt parameter summaries if present."""
    columns = ["stage", "method", "param", "HPD2.5%", "median", "HPD97.5%"]
    if not path.is_file():
        return pd.DataFrame(columns=columns)

    try:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return pd.DataFrame(columns=columns)

    header_index = next(
        (i for i, line in enumerate(lines) if line.strip().startswith("param")),
        None,
    )
    if header_index is None:
        return pd.DataFrame(columns=columns)

    rows = []
    counts_by_param: dict[str, int] = {}
    for line in lines[header_index + 1 :]:
        stripped = line.strip()
        if not stripped:
            continue
        parts = stripped.split()
        if len(parts) < 4:
            continue

        param = parts[0]
        occurrence = counts_by_param.get(param, 0)
        method = "neural_network" if occurrence % 2 == 0 else "random_forest"
        counts_by_param[param] = occurrence + 1

        try:
            hpd_low = float(parts[1])
            median = float(parts[2])
            hpd_high = float(parts[3])
        except ValueError:
            continue

        rows.append(
            {
                "stage": stage,
                "method": method,
                "param": param,
                "HPD2.5%": hpd_low,
                "median": median,
                "HPD97.5%": hpd_high,
            }
        )

    return pd.DataFrame(rows, columns=columns)


def _parse_sfs_bin(label: str) -> tuple[int | None, int | None]:
    parts = str(label).split("_")
    if len(parts) == 1 and parts[0].startswith("fA"):
        try:
            return int(parts[0][2:]), None
        except ValueError:
            return None, None
    if len(parts) == 2 and parts[0].startswith("fA") and parts[1].startswith("fB"):
        try:
            return int(parts[0][2:]), int(parts[1][2:])
        except ValueError:
            return None, None
    return None, None


def _read_sfs_gof(path: Path, stage: str) -> pd.DataFrame:
    """Parse DILS gof_sfs.txt as a tidy table, if present and well-formed."""
    columns = [
        "stage",
        "bin",
        "observed",
        "expected",
        "expected_minus_observed",
        "observed_minus_expected",
        "p_value",
        "freq_a",
        "freq_b",
    ]
    if not path.is_file():
        return pd.DataFrame(columns=columns)

    try:
        raw = pd.read_csv(path, sep="\t", header=None, dtype=str)
    except Exception:
        return pd.DataFrame(columns=columns)

    if raw.shape[0] < 5:
        return pd.DataFrame(columns=columns)

    bins = raw.iloc[0].tolist()
    values = raw.iloc[1:5].apply(pd.to_numeric, errors="coerce")
    if values.shape[1] != len(bins):
        return pd.DataFrame(columns=columns)

    rows = []
    for idx, bin_label in enumerate(bins):
        observed = values.iat[0, idx]
        expected = values.iat[1, idx]
        expected_minus_observed = values.iat[2, idx]
        p_value = values.iat[3, idx]
        if pd.isna(observed) or pd.isna(expected):
            continue
        freq_a, freq_b = _parse_sfs_bin(str(bin_label))
        rows.append(
            {
                "stage": stage,
                "bin": str(bin_label),
                "observed": observed,
                "expected": expected,
                "expected_minus_observed": expected_minus_observed,
                "observed_minus_expected": (
                    -expected_minus_observed
                    if pd.notna(expected_minus_observed)
                    else pd.NA
                ),
                "p_value": p_value,
                "freq_a": freq_a,
                "freq_b": freq_b,
            }
        )

    return pd.DataFrame(rows, columns=columns)


def parse_archive(root: Path, display_name: str = "") -> ParsedResults:
    """Parse Tier A (+ optional Tier B) files from extracted root."""
    gi_path = root / "general_infos.txt"
    if not gi_path.is_file():
        raise FileNotFoundError(f"Missing general_infos.txt in {root}")

    general_infos = _read_general_infos(gi_path)
    n_species = _parse_n_species(general_infos)

    config_path = root / "config.yaml"
    with config_path.open(encoding="utf-8") as f:
        config = yaml.safe_load(f) or {}

    nref_path = root / "Nref.txt"
    nref = (
        float(pd.read_csv(nref_path, header=None).iloc[0, 0])
        if nref_path.is_file()
        else None
    )

    hierarchical = _read_hierarchical(root / "modelComp" / "hierarchical_models.txt")
    abcstat_loci = _read_table(root / "ABCstat_loci.txt")
    locus_specific = _read_optional_table(
        root / "locus_modelComp" / "locus_specific_modelComp.txt"
    )
    abcstat_global = _read_table(root / "ABCstat_global.txt")

    posterior = _read_table(root / "best_model" / "posterior_bestModel.txt")
    prior = _read_table(root / "best_model" / "priorfile.txt")
    posterior_rf = _read_optional_table(
        root / "best_model" / "posterior_summary_RandomForest_bestModel.txt"
    )

    opt_dir = "best_model_7" if n_species == 1 else "best_model_5"
    opt_path = root / opt_dir / "posterior_bestModel.txt"
    posterior_optimized = (
        _read_table(opt_path) if opt_path.is_file() else pd.DataFrame()
    )

    parameter_reports = pd.DataFrame(
        columns=["stage", "method", "param", "HPD2.5%", "median", "HPD97.5%"]
    )
    if n_species == 1:
        name_a = general_infos.get("nameA", "A")
        report_tables = [
            _read_parameter_report(
                root / "best_model" / f"report_{name_a}.txt",
                "posterior",
            ),
            _read_parameter_report(
                root / "best_model_7" / f"report_{name_a}.txt",
                "optimized posterior",
            ),
        ]
        report_tables = [df for df in report_tables if not df.empty]
        if report_tables:
            parameter_reports = pd.concat(report_tables, ignore_index=True)

    gof_stats = _read_optional_table(root / "gof" / "goodness_of_fit_test.txt")
    gof_stats_optimized = _read_optional_table(
        root / "gof_2" / "goodness_of_fit_test.txt"
    )
    sfs_tables = [
        _read_sfs_gof(root / "gof" / "gof_sfs.txt", "Posterior"),
        _read_sfs_gof(root / "gof_2" / "gof_sfs.txt", "Optimized posterior"),
    ]
    sfs_tables = [df for df in sfs_tables if not df.empty]
    sfs_gof = (
        pd.concat(sfs_tables, ignore_index=True)
        if sfs_tables
        else pd.DataFrame(
            columns=[
                "stage",
                "bin",
                "observed",
                "expected",
                "expected_minus_observed",
                "observed_minus_expected",
                "p_value",
                "freq_a",
                "freq_b",
            ]
        )
    )

    abcjsfs = None
    jsfs_path = root / "ABCjsfs.txt"
    if jsfs_path.is_file():
        abcjsfs = _read_table(jsfs_path)

    pca_coords = None
    pca_path = root / "table_coord_PCA_SS.txt"
    if pca_path.is_file():
        pca_coords = _read_table(pca_path)

    locus_infos = None
    name_a = general_infos.get("nameA", "A")
    if n_species == 1:
        infos_path = root / f"{name_a}_infos.txt"
    else:
        name_b = general_infos.get("nameB", "B")
        infos_path = root / f"{name_a}_{name_b}_infos.txt"
    if infos_path.is_file():
        locus_infos = _read_table(infos_path)

    return ParsedResults(
        root=root,
        display_name=display_name or root.name,
        general_infos=general_infos,
        config=config,
        nref=nref,
        hierarchical=hierarchical,
        abcstat_loci=abcstat_loci,
        locus_specific=locus_specific,
        abcstat_global=abcstat_global,
        posterior=posterior,
        prior=prior,
        posterior_rf=posterior_rf,
        posterior_optimized=posterior_optimized,
        parameter_reports=parameter_reports,
        gof_stats=gof_stats,
        gof_stats_optimized=gof_stats_optimized,
        sfs_gof=sfs_gof,
        abcjsfs=abcjsfs,
        pca_coords=pca_coords,
        locus_infos=locus_infos,
    )
