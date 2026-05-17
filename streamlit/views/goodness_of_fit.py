"""Goodness-of-fit tab."""

from __future__ import annotations

import math

import pandas as pd
import plotly.graph_objects as go
import streamlit as st

from plot_style import apply_readable_style
from results.parsers import ParsedResults

# Minimum |mean_exp| for relative deviation (avoids blow-up near zero)
_MEAN_EXP_EPS = 1e-12

_OBSERVED_ORIGIN_MARKERS = ("observed",)


def _is_observed_origin(origin: str) -> bool:
    o = str(origin).lower()
    return any(marker in o for marker in _OBSERVED_ORIGIN_MARKERS)


def _relative_deviation(mean_obs: pd.Series, mean_exp: pd.Series) -> pd.Series:
    denom = mean_exp.abs()
    safe = denom >= _MEAN_EXP_EPS
    rel = (mean_obs - mean_exp) / denom
    return rel.where(safe, other=pd.NA)


def _origin_key(series: pd.Series) -> pd.Series:
    return series.astype(str).str.strip().str.lower()


def _mask_origin(pca: pd.DataFrame, label: str) -> pd.Series:
    return _origin_key(pca["origin"]) == label.strip().lower()


def _is_significant_fdr(value: object, alpha: float) -> bool:
    p = pd.to_numeric(value, errors="coerce")
    return bool(pd.notna(p) and p < alpha)


def _pca_scatter(pca: pd.DataFrame) -> go.Figure:
    """
    Draw PCA points bottom → top: prior, posterior, optimized posterior, observed.
    """
    fig = go.Figure()
    base_size = 6
    obs_size = base_size * 3

    # (origin label, marker style) — traces are added in z-order (bottom first)
    layers: list[tuple[str, dict]] = [
        (
            "prior",
            dict(
                size=base_size,
                symbol="circle",
                color="#b0b0b0",
                opacity=0.28,
                line=dict(width=0),
            ),
        ),
        (
            "posterior",
            dict(
                size=base_size,
                symbol="circle",
                color="#2166ac",
                opacity=0.55,
                line=dict(width=0.4, color="#1a4480"),
            ),
        ),
        (
            "optimized posterior",
            dict(
                size=base_size + 1,
                symbol="diamond",
                color="#e67e22",
                opacity=0.72,
                line=dict(width=0.5, color="#a04000"),
            ),
        ),
    ]

    known = {label for label, _ in layers} | {"observed dataset"}
    okey = _origin_key(pca["origin"])

    for label, marker in layers:
        sub = pca[_mask_origin(pca, label)]
        if sub.empty:
            continue
        fig.add_trace(
            go.Scatter(
                x=sub["Dim.1"],
                y=sub["Dim.2"],
                mode="markers",
                name=label,
                marker=marker,
            )
        )

    # Any unexpected origin (except observed) between optimized and observed
    other = pca[~okey.isin(known)]
    if not other.empty:
        for origin in sorted(other["origin"].unique(), key=str):
            sub = other[other["origin"] == origin]
            fig.add_trace(
                go.Scatter(
                    x=sub["Dim.1"],
                    y=sub["Dim.2"],
                    mode="markers",
                    name=str(origin),
                    marker=dict(
                        size=base_size,
                        symbol="circle",
                        color="#7f8c8d",
                        opacity=0.5,
                    ),
                )
            )

    observed = pca[pca["origin"].map(_is_observed_origin)]
    if not observed.empty:
        label = str(observed["origin"].iloc[0])
        fig.add_trace(
            go.Scatter(
                x=observed["Dim.1"],
                y=observed["Dim.2"],
                mode="markers",
                name=label,
                marker=dict(
                    size=obs_size,
                    symbol="star",
                    color="#00cc44",
                    line=dict(width=2, color="#000000"),
                    opacity=1.0,
                ),
            )
        )

    apply_readable_style(
        fig,
        height=450,
        xaxis_title="Dim. 1",
        yaxis_title="Dim. 2",
        legend=dict(orientation="h", yanchor="bottom", y=1.02),
    )
    return fig


def _available_gof_stages(data: ParsedResults) -> list[str]:
    stages = []
    sfs_stages = set(data.sfs_gof["stage"]) if not data.sfs_gof.empty else set()
    if not data.gof_stats.empty or "Posterior" in sfs_stages:
        stages.append("Posterior")
    if not data.gof_stats_optimized.empty or "Optimized posterior" in sfs_stages:
        stages.append("Optimized posterior")
    return stages


def _select_gof_stage(data: ParsedResults) -> str | None:
    stages = _available_gof_stages(data)
    if not stages:
        return None
    if len(stages) == 1:
        st.markdown(f"**Posterior or optimized posterior distribution:** `{stages[0]}`")
        return stages[0]
    return st.selectbox(
        "Posterior or optimized posterior distribution",
        stages,
        index=0,
    )


def _gof_stats_for_stage(data: ParsedResults, selected_stage: str) -> pd.DataFrame:
    if selected_stage == "Optimized posterior":
        return data.gof_stats_optimized.copy()
    return data.gof_stats.copy()


def _render_sfs_table(
    sfs_gof: pd.DataFrame,
    selected_stage: str,
    stage_rows: pd.DataFrame,
) -> None:
    has_freq_b = "freq_b" in stage_rows.columns and stage_rows["freq_b"].notna().any()
    display_rows = stage_rows.copy()
    display_rows["n_SNPs (obs)"] = display_rows["observed"]

    expected_labels = {
        "Posterior": "n_SNPs (exp, posterior)",
        "Optimized posterior": "n_SNPs (exp, optimized post.)",
    }
    for stage, column_label in expected_labels.items():
        stage_expected = sfs_gof[sfs_gof["stage"] == stage]
        if stage_expected.empty:
            continue
        display_rows[column_label] = display_rows["bin"].map(
            stage_expected.set_index("bin")["expected"]
        )

    table_cols = [
        c
        for c in (
            "bin",
            "freq_a",
            "freq_b",
            "n_SNPs (obs)",
            "n_SNPs (exp, posterior)",
            "n_SNPs (exp, optimized post.)",
            "observed_minus_expected",
            "p_value",
        )
        if c in display_rows.columns and (c != "freq_b" or has_freq_b)
    ]
    with st.expander("SFS GOF tidy table"):
        st.caption(
            "Residuals are shown as observed - expected. "
            "Residuals and p-values correspond to the selected stage: "
            f"{selected_stage}."
        )
        st.dataframe(display_rows[table_cols], width="stretch", hide_index=True)


def _render_sfs_1pop(stage_rows: pd.DataFrame) -> None:
    rows = stage_rows.dropna(subset=["freq_a"]).sort_values("freq_a")
    if rows.empty:
        st.warning("No 1-pop SFS bins could be parsed.")
        return

    fig = go.Figure()
    fig.add_trace(
        go.Bar(
            x=rows["freq_a"],
            y=rows["observed"],
            name="observed",
            marker_color="#2166ac",
        )
    )
    fig.add_trace(
        go.Bar(
            x=rows["freq_a"],
            y=rows["expected"],
            name="expected",
            marker_color="#e67e22",
        )
    )
    apply_readable_style(
        fig,
        barmode="group",
        height=420,
        xaxis_title="Frequency class",
        yaxis_title="Number of SNPs",
        legend=dict(orientation="h", yanchor="bottom", y=1.02),
    )
    st.plotly_chart(fig, width="stretch")

    residual = go.Figure(
        go.Bar(
            x=rows["freq_a"],
            y=rows["observed_minus_expected"],
            marker_color=[
                "#c0392b" if v < 0 else "#2980b9"
                for v in rows["observed_minus_expected"]
            ],
            customdata=rows["p_value"],
            hovertemplate=(
                "frequency=%{x}<br>"
                "observed - expected=%{y}<br>"
                "p-value=%{customdata}<extra></extra>"
            ),
        )
    )
    residual.add_hline(y=0, line_width=1, line_color="#333")
    apply_readable_style(
        residual,
        height=360,
        xaxis_title="Frequency class",
        yaxis_title="observed - expected",
    )
    st.plotly_chart(residual, width="stretch")


def _matrix_from_sfs(stage_rows: pd.DataFrame, value_col: str) -> pd.DataFrame:
    rows = stage_rows.dropna(subset=["freq_a", "freq_b"]).copy()
    rows["freq_a"] = rows["freq_a"].astype(int)
    rows["freq_b"] = rows["freq_b"].astype(int)
    matrix = rows.pivot(index="freq_b", columns="freq_a", values=value_col)
    return matrix.sort_index(ascending=True).sort_index(axis=1, ascending=True)


def _log10_count_matrix(matrix: pd.DataFrame) -> pd.DataFrame:
    return (matrix + 1).apply(
        lambda col: col.map(
            lambda value: float("nan") if pd.isna(value) else math.log10(float(value))
        )
    )


def _heatmap_from_matrix(
    matrix: pd.DataFrame,
    title: str,
    colorbar_title: str,
    colorscale: str = "Viridis",
) -> go.Figure:
    fig = go.Figure(
        go.Heatmap(
            z=matrix.to_numpy(),
            x=matrix.columns.astype(str),
            y=matrix.index.astype(str),
            colorscale=colorscale,
            colorbar=dict(title=colorbar_title, thickness=10, len=0.68, x=1.02),
            hovertemplate="freq A=%{x}<br>freq B=%{y}<br>value=%{z}<extra></extra>",
        )
    )
    apply_readable_style(
        fig,
        height=390,
        title=title,
        xaxis_title="Frequency in population A",
        yaxis_title="Frequency in population B",
        margin=dict(l=56, r=72, t=58, b=56),
    )
    return fig


def _render_sfs_2pop(stage_rows: pd.DataFrame) -> None:
    rows = stage_rows.dropna(subset=["freq_a", "freq_b"])
    if rows.empty:
        st.warning("No 2-pop joint SFS bins could be parsed.")
        return

    observed = _matrix_from_sfs(rows, "observed")
    expected = _matrix_from_sfs(rows, "expected")
    observed_log = _log10_count_matrix(observed)
    expected_log = _log10_count_matrix(expected)
    residual = _matrix_from_sfs(rows, "observed_minus_expected")
    pvals = _matrix_from_sfs(rows, "p_value")

    c1, c2 = st.columns(2)
    with c1:
        st.plotly_chart(
            _heatmap_from_matrix(
                observed_log,
                "Observed jSFS (log10(count + 1))",
                "log10<br>(count+1)",
            ),
            width="stretch",
        )
    with c2:
        st.plotly_chart(
            _heatmap_from_matrix(
                expected_log,
                "Expected jSFS (log10(count + 1))",
                "log10<br>(count+1)",
            ),
            width="stretch",
        )

    c3, c4 = st.columns(2)
    with c3:
        st.plotly_chart(
            _heatmap_from_matrix(
                residual,
                "Observed - expected",
                "obs-exp<br>SNPs",
                colorscale="RdBu",
            ),
            width="stretch",
        )
    with c4:
        st.plotly_chart(
            _heatmap_from_matrix(
                pvals,
                "P-values",
                "p-value",
            ),
            width="stretch",
        )


def _render_sfs_gof(data: ParsedResults, selected_stage: str) -> None:
    st.subheader("SFS goodness-of-fit")
    sfs_gof = data.sfs_gof
    if sfs_gof.empty:
        st.info("No SFS goodness-of-fit table found in this archive.")
        return

    stage_rows = sfs_gof[sfs_gof["stage"] == selected_stage].copy()
    if stage_rows.empty:
        st.warning(f"No SFS goodness-of-fit table found for {selected_stage}.")
        return

    if data.is_one_pop:
        _render_sfs_1pop(stage_rows)
    elif data.is_two_pop:
        _render_sfs_2pop(stage_rows)
    else:
        st.info("SFS goodness-of-fit is supported for 1- or 2-population archives.")
        return

    _render_sfs_table(sfs_gof, selected_stage or "unknown", stage_rows)


def _render_summary_gof(gof: pd.DataFrame, selected_stage: str) -> None:
    st.subheader("Goodness-of-fit (summary statistics)")

    if gof.empty:
        st.warning(f"No goodness-of-fit summary table found for {selected_stage}.")
        return

    alpha = st.slider("Highlight FDR below", 0.0, 0.2, 0.05, 0.01)

    def _style(row):
        p = row.get("pvals_fdr_corrected")
        if _is_significant_fdr(p, alpha):
            return ["background-color: #ffcccc"] * len(row)
        return [""] * len(row)

    styled = gof.style.apply(_style, axis=1)
    st.dataframe(styled, width="stretch", height=400)

    st.subheader("Observed vs expected (relative deviation)")
    st.caption(
        "Relative deviation: (mean_obs − mean_exp) / |mean_exp|. "
        f"Statistics with |mean_exp| < {_MEAN_EXP_EPS:g} are omitted from the plot."
    )

    gof_plot = gof.copy()
    required_plot_cols = {"mean_obs", "mean_exp", "stats"}
    missing_plot_cols = sorted(required_plot_cols - set(gof_plot.columns))
    if missing_plot_cols:
        st.warning(
            "Goodness-of-fit table is missing required column(s): "
            + ", ".join(missing_plot_cols)
        )
        return

    gof_plot["rel_dev"] = _relative_deviation(gof_plot["mean_obs"], gof_plot["mean_exp"])
    plot_rows = gof_plot.dropna(subset=["rel_dev"]).copy()
    plot_rows["abs_rel_dev"] = plot_rows["rel_dev"].abs()
    plot_rows = plot_rows.sort_values("abs_rel_dev", ascending=True)

    n_omitted = len(gof) - len(plot_rows)
    if n_omitted:
        st.caption(f"{n_omitted} statistic(s) omitted (|mean_exp| too close to zero).")

    if plot_rows.empty:
        st.warning("No statistics available for relative deviation plot.")
    else:
        fig = go.Figure(
            go.Bar(
                x=plot_rows["rel_dev"],
                y=plot_rows["stats"],
                orientation="h",
                marker_color=[
                    "#c0392b" if v < 0 else "#2980b9" for v in plot_rows["rel_dev"]
                ],
                hovertemplate=(
                    "stat=%{y}<br>"
                    "rel. dev.=%{x:.4f}<br>"
                    "<extra></extra>"
                ),
            )
        )
        fig.add_vline(x=0, line_width=1, line_color="#333")
        apply_readable_style(
            fig,
            height=max(400, 28 * len(plot_rows)),
            xaxis_title="(mean_obs − mean_exp) / |mean_exp|",
            yaxis_title="",
            margin=dict(l=120),
        )
        st.plotly_chart(fig, width="stretch")

    with st.expander("Raw mean_obs / mean_exp"):
        raw_cols = [c for c in ("stats", "mean_obs", "mean_exp", "pvals_fdr_corrected") if c in gof.columns]
        st.dataframe(gof[raw_cols], width="stretch")


def render(data: ParsedResults) -> None:
    if data.pca_coords is not None:
        st.subheader("PCA of summary statistics (2D)")
        pca = data.pca_coords
        required_pca_cols = {"origin", "Dim.1", "Dim.2"}
        missing_pca_cols = sorted(required_pca_cols - set(pca.columns))
        if missing_pca_cols:
            st.warning(
                "PCA file is missing required column(s): "
                + ", ".join(missing_pca_cols)
            )
        else:
            fig2 = _pca_scatter(pca)
            st.plotly_chart(fig2, width="stretch")
            n_obs = pca["origin"].map(_is_observed_origin).sum()
            st.caption(
                f"{len(pca)} points in table_coord_PCA_SS.txt "
                f"({n_obs} observed, {len(pca) - n_obs} simulated). "
                "Observed dataset is shown as a large star on top."
            )
            st.divider()

    selected_stage = _select_gof_stage(data)
    if selected_stage is None:
        st.info("No goodness-of-fit tables found in this archive.")
        return

    gof = _gof_stats_for_stage(data, selected_stage)
    _render_summary_gof(gof, selected_stage)

    st.divider()
    _render_sfs_gof(data, selected_stage)
