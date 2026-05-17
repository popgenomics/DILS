"""Canonical DILS YAML defaults, field order, and serialization."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml

REPO_ROOT = Path(__file__).resolve().parents[1]
CONFIG_1POP = REPO_ROOT / "bin" / "example_config_1pop.yaml"
CONFIG_2POP = REPO_ROOT / "bin" / "example_config_2pop.yaml"

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


def load_canonical_defaults() -> dict[int, dict[str, Any]]:
    with CONFIG_1POP.open(encoding="utf-8") as f:
        one_pop = yaml.safe_load(f) or {}
    with CONFIG_2POP.open(encoding="utf-8") as f:
        two_pop = yaml.safe_load(f) or {}
    return {1: one_pop, 2: two_pop}


def fields_for_species(n_species: int) -> list[str]:
    return FIELDS_1POP if n_species == 1 else FIELDS_2POP


def build_yaml_dict(values: dict[str, Any], n_species: int) -> dict[str, Any]:
    return {field: values[field] for field in fields_for_species(n_species)}


def serialize_yaml(values: dict[str, Any], n_species: int) -> str:
    return yaml.safe_dump(
        build_yaml_dict(values, n_species),
        sort_keys=False,
        default_flow_style=False,
    )
