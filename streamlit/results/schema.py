"""Constants and scaling rules for DILS results (v0)."""

from __future__ import annotations

# Bundled example archives (repo example/, read-only)
BUNDLED_ARCHIVES = {
    "1pop legacy example": "project1pop.tar.gz",
    "2pop light, no outgroup, no SFS, beta constant coding": (
        "2pop_light_noOut_noSFS_beta_constant_coding.tar.gz"
    ),
    "2pop light, no outgroup, SFS beta variable coding": (
        "2pop_light_noOut_SFS_beta_variable_coding.tar.gz"
    ),
    "project2popv2 (legacy example)": "project2popv2.tar.gz",
}

# Parameters scaled by Nref (diploid effective size)
N_SCALED_PARAMS = frozenset({"N1", "N2", "Na"})

# Parameters scaled by 4 * Nref (time in generations)
TIME_SCALED_PARAMS = frozenset(
    {"Tsplit", "Tam", "Tsc", "Tdem1", "Tdem2"}
)

# Shape / migration parameters: no scaling
UNIT_PARAMS = frozenset(
    {
        "M12",
        "M21",
        "shape_N_a",
        "shape_N_b",
        "shape_M12_a",
        "shape_M12_b",
        "shape_M21_a",
        "shape_M21_b",
        "founders1",
        "founders2",
        "nBarriersM12",
        "nBarriersM21",
    }
)


def scale_parameter(name: str, values, nref: float | None):
    """Return values on display scale (matches Shiny app.R logic)."""
    import pandas as pd

    s = pd.Series(values, copy=True)
    if nref is None:
        return s
    if name in N_SCALED_PARAMS:
        return s * nref
    if name in TIME_SCALED_PARAMS:
        return s * (4.0 * nref)
    return s


def hierarchical_labels(first_winner: str, n_species: int = 2) -> list[str]:
    """Column labels for hierarchical_models row 0, depending on migration vs isolation."""
    if n_species == 1:
        return [
            "Expansion versus Constant versus Contraction",
            "N-homo versus N-hetero",
        ]
    if str(first_winner).strip().lower() == "migration":
        return [
            "Migration versus isolation",
            "IM versus SC",
            "M-homo versus M-hetero",
            "N-homo versus N-hetero",
        ]
    return [
        "Migration versus isolation",
        "AM versus SI",
        "N-homo versus N-hetero",
    ]
