"""Estimated parameters tab."""

from __future__ import annotations

import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from plot_style import apply_readable_style
from results.parsers import ParsedResults
from results.schema import scale_parameter

_RANGE_FULL = "Full prior range"
_RANGE_POSTERIOR = "Posterior-focused range"


def _focused_x_range(
    post_vals: pd.Series,
    opt_vals: pd.Series | None,
    q_low: float = 0.01,
    q_high: float = 0.99,
    margin_frac: float = 0.05,
) -> tuple[float, float] | None:
    """1–99% range from posterior (+ optimized), with margin; None if unsafe."""
    parts = [post_vals.dropna()]
    if opt_vals is not None and not opt_vals.empty:
        parts.append(opt_vals.dropna())
    combined = pd.concat(parts, ignore_index=True)
    if len(combined) < 2:
        return None
    lo = float(combined.quantile(q_low))
    hi = float(combined.quantile(q_high))
    if not pd.notna(lo) or not pd.notna(hi) or lo >= hi:
        return None
    span = hi - lo
    margin = span * margin_frac if span > 0 else abs(hi) * margin_frac + 1e-9
    return lo - margin, hi + margin


def render(data: ParsedResults) -> None:
    st.subheader("Estimated parameters")

    param_cols = [c for c in data.posterior.columns if c in data.prior.columns]
    if not param_cols:
        st.warning("No overlapping parameters between prior and posterior files.")
        return

    param = st.selectbox("Parameter", param_cols, key="param_select")
    x_range_mode = st.radio(
        "X-axis range",
        [_RANGE_FULL, _RANGE_POSTERIOR],
        index=0,
        horizontal=True,
        key="param_x_range_mode",
    )

    prior_vals = scale_parameter(param, data.prior[param], data.nref)
    post_vals = scale_parameter(param, data.posterior[param], data.nref)
    opt_vals: pd.Series | None = None
    if (
        not data.posterior_optimized.empty
        and param in data.posterior_optimized.columns
    ):
        opt_vals = scale_parameter(
            param, data.posterior_optimized[param], data.nref
        )

    hist_kw = dict(histnorm="probability density", nbinsx=40)

    fig = go.Figure()
    fig.add_trace(
        go.Histogram(
            x=prior_vals,
            name="Prior",
            opacity=0.45,
            marker_color="#b0b0b0",
            **hist_kw,
        )
    )
    fig.add_trace(
        go.Histogram(
            x=post_vals,
            name="Posterior",
            opacity=0.6,
            marker_color="#2166ac",
            **hist_kw,
        )
    )
    if opt_vals is not None:
        fig.add_trace(
            go.Histogram(
                x=opt_vals,
                name="Optimized posterior",
                opacity=0.6,
                marker_color="#e67e22",
                **hist_kw,
            )
        )

    layout_kw: dict = dict(
        barmode="overlay",
        xaxis_title=param,
        yaxis_title="Density",
        height=450,
        legend=dict(orientation="h", yanchor="bottom", y=1.02),
    )

    if x_range_mode == _RANGE_POSTERIOR:
        xlim = _focused_x_range(
            pd.Series(post_vals),
            pd.Series(opt_vals) if opt_vals is not None else None,
        )
        if xlim is not None:
            layout_kw["xaxis_range"] = list(xlim)
            st.caption(
                "Posterior-focused range: x-axis zoomed to the 1st–99th percentile "
                "of the posterior (and optimized posterior, if present). "
                "The prior is still shown but clipped to this window."
            )
        else:
            st.caption(
                "Could not compute a posterior-focused range; showing full prior range."
            )

    apply_readable_style(fig, **layout_kw)
    st.plotly_chart(fig, width="stretch")

    if data.is_one_pop:
        with st.container(border=True):
            st.subheader("Parameter summary reports")
            if data.parameter_reports.empty:
                st.info("No 1-pop parameter report summaries found in this archive.")
            else:
                st.caption(
                    "The report files list two estimation methods for each parameter: "
                    "neural_network and random_forest."
                )
                st.dataframe(data.parameter_reports, width="stretch", hide_index=True)
        return

    with st.container(border=True):
        st.subheader("Random forest — highest posterior density")
        if data.posterior_rf.empty:
            st.info("No random-forest posterior summary found in this archive.")
        else:
            st.dataframe(data.posterior_rf, width="stretch", hide_index=True)
