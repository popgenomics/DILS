"""Shared Plotly layout defaults for readable figures (~50% larger text)."""

from __future__ import annotations

from typing import Any

import plotly.graph_objects as go

# Base sizes (px) before scale factor 1.5
_TICK = 12
_AXIS_TITLE = 14
_LEGEND = 12
_HOVER = 13
_GENERAL = 13

SCALE = 1.5

TICK_FONT_SIZE = int(_TICK * SCALE)
AXIS_TITLE_FONT_SIZE = int(_AXIS_TITLE * SCALE)
LEGEND_FONT_SIZE = int(_LEGEND * SCALE)
HOVER_FONT_SIZE = int(_HOVER * SCALE)
GENERAL_FONT_SIZE = int(_GENERAL * SCALE)


def _merge_nested_dict(base: dict, override: dict) -> dict:
    """Shallow merge with one level of nesting (e.g. legend.font)."""
    merged = {**base}
    for key, val in override.items():
        if isinstance(val, dict) and isinstance(merged.get(key), dict):
            merged[key] = {**merged[key], **val}
        else:
            merged[key] = val
    return merged


def apply_readable_style(fig: go.Figure, **layout: Any) -> go.Figure:
    """
    Apply consistent font sizes to a Plotly figure (graph_objects or express).
    Pass extra layout keys (height, margin, …) as keyword arguments.
    Caller `legend` settings are merged with default legend font size.
    """
    overrides = dict(layout)
    legend = {"font": {"size": LEGEND_FONT_SIZE}}
    if "legend" in overrides:
        user_legend = overrides.pop("legend")
        if isinstance(user_legend, dict):
            legend = _merge_nested_dict(legend, user_legend)

    fig.update_layout(
        font=dict(size=GENERAL_FONT_SIZE),
        hoverlabel=dict(font_size=HOVER_FONT_SIZE),
        legend=legend,
        **overrides,
    )
    fig.update_xaxes(
        title_font=dict(size=AXIS_TITLE_FONT_SIZE),
        tickfont=dict(size=TICK_FONT_SIZE),
    )
    fig.update_yaxes(
        title_font=dict(size=AXIS_TITLE_FONT_SIZE),
        tickfont=dict(size=TICK_FONT_SIZE),
    )
    return fig
