"""Locus name resolution for tables and plot hovers."""

from __future__ import annotations

import pandas as pd


def _as_str(series: pd.Series) -> pd.Series:
    return series.astype(str).str.strip()


def _looks_all_numeric(names: pd.Series) -> bool:
    if names.empty:
        return False
    return bool(names.str.match(r"^-?\d+(\.\d+)?$").all())


def locus_name_series(
    df: pd.DataFrame,
    locus_infos: pd.DataFrame | None = None,
    dataset_col: str = "dataset",
) -> pd.Series:
    """
    Resolve a display name per row.

    Prefer `dataset` when it looks like a locus identifier; otherwise join
    `locus_infos.locusName` by key or row order.
    """
    if dataset_col not in df.columns:
        if locus_infos is not None and "locusName" in locus_infos.columns:
            names = _as_str(locus_infos["locusName"])
            if len(names) == len(df):
                return names.reset_index(drop=True)
        return pd.Series([""] * len(df), index=df.index, dtype=str)

    base = _as_str(df[dataset_col])
    if not _looks_all_numeric(base):
        return base

    if locus_infos is None or "locusName" not in locus_infos.columns:
        return base

    ln = _as_str(locus_infos["locusName"])

    if dataset_col in locus_infos.columns:
        lookup = dict(zip(_as_str(locus_infos[dataset_col]), ln))
        mapped = base.map(lookup)
        if mapped.notna().any():
            return mapped.fillna(base)

    by_name = pd.DataFrame({dataset_col: base}).merge(
        locus_infos.assign(**{dataset_col: ln})[[dataset_col, "locusName"]],
        on=dataset_col,
        how="left",
    )
    if "locusName" in by_name.columns and by_name["locusName"].notna().any():
        return _as_str(by_name["locusName"].fillna(base))

    if len(ln) == len(df):
        return ln.reset_index(drop=True)

    return base


def with_locus_name(
    df: pd.DataFrame,
    locus_infos: pd.DataFrame | None = None,
    dataset_col: str = "dataset",
) -> pd.DataFrame:
    """Return a copy with `locus_name` as the first column."""
    out = df.copy()
    names = locus_name_series(out, locus_infos, dataset_col=dataset_col)
    if "locus_name" in out.columns:
        out = out.drop(columns=["locus_name"])
    out.insert(0, "locus_name", names.values)
    return out
