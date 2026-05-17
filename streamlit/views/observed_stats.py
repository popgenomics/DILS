"""Observed summary statistics (per-locus boxplots)."""

from __future__ import annotations

import pandas as pd
import plotly.express as px
import streamlit as st

from plot_style import apply_readable_style
from results.loci import locus_name_series, with_locus_name
from results.parsers import ParsedResults

_HOVER_LOCI = (
    "<b>%{customdata[0]}</b><br>"
    "statistic=%{x}<br>"
    "value=%{y:.6g}"
    "<extra></extra>"
)


def _boxplot_panel(
    df: pd.DataFrame,
    locus_names: pd.Series,
    value_cols: list[str],
    x_labels: list[str],
    y_title: str,
    show_points: bool,
    height: int = 420,
) -> None:
    rows = []
    for col, label in zip(value_cols, x_labels):
        if col not in df.columns:
            continue
        for val, locus_name in zip(df[col], locus_names):
            rows.append(
                {"statistic": label, "value": val, "locus_name": locus_name}
            )
    if not rows:
        st.warning("No columns available for this panel.")
        return
    long = pd.DataFrame(rows)
    fig = px.box(
        long,
        x="statistic",
        y="value",
        color="statistic",
        points="all" if show_points else False,
        custom_data=["locus_name"],
    )
    apply_readable_style(
        fig,
        showlegend=False,
        yaxis_title=y_title,
        xaxis_title="",
        height=height,
        margin=dict(l=48, r=24, t=36, b=96),
    )
    if show_points:
        fig.update_traces(
            jitter=0.3,
            pointpos=0,
            hovertemplate=_HOVER_LOCI,
        )
    else:
        fig.update_traces(hovertemplate="statistic=%{x}<br>value=%{y:.6g}<extra></extra>")
    st.plotly_chart(fig, width="stretch")


def render(data: ParsedResults) -> None:
    st.subheader("Observed summary statistics")
    df = data.abcstat_loci
    locus_names = locus_name_series(df, data.locus_infos)
    show_points = st.checkbox("Show individual loci", value=False)

    na, nb = data.name_a, data.name_b

    if data.is_one_pop:
        tab_sfs, tab_poly, tab_taj = st.tabs(
            ["Site frequencies", "Polymorphism", "Tajima's D"]
        )

        with tab_sfs:
            _boxplot_panel(
                df,
                locus_names,
                ["bialsites_avg", "sf_avg"],
                ["Biallelic sites", "Sf"],
                "Sites",
                show_points,
            )

        with tab_poly:
            _boxplot_panel(
                df,
                locus_names,
                ["piA_avg", "thetaA_avg"],
                [f"π {na}", f"θw {na}"],
                "Diversity index per site",
                show_points,
            )

        with tab_taj:
            _boxplot_panel(
                df,
                locus_names,
                ["DtajA_avg"],
                [f"Tajima's D {na}"],
                "Tajima's D",
                show_points,
            )
    else:
        tab_sfs, tab_poly, tab_taj, tab_div = st.tabs(
            [
                "Site frequencies",
                "Polymorphism",
                "Tajima's D",
                "Differentiation & divergence",
            ]
        )

        with tab_sfs:
            _boxplot_panel(
                df,
                locus_names,
                ["sf_avg", "sxA_avg", "sxB_avg", "ss_avg"],
                ["Sf", f"Sx {na}", f"Sx {nb}", "Ss"],
                "Proportion of sites",
                show_points,
            )

        with tab_poly:
            _boxplot_panel(
                df,
                locus_names,
                ["piA_avg", "piB_avg", "thetaA_avg", "thetaB_avg"],
                [f"π {na}", f"π {nb}", f"θw {na}", f"θw {nb}"],
                "Diversity index per site",
                show_points,
            )

        with tab_taj:
            _boxplot_panel(
                df,
                locus_names,
                ["DtajA_avg", "DtajB_avg"],
                [f"Tajima's D {na}", f"Tajima's D {nb}"],
                "Tajima's D",
                show_points,
            )

        with tab_div:
            div_cols = [
                c for c in ("divAB_avg", "netdivAB_avg", "FST_avg") if c in df.columns
            ]
            div_labels = ["Divergence", "Net divergence", "FST"][: len(div_cols)]
            _boxplot_panel(
                df,
                locus_names,
                div_cols,
                div_labels,
                "Differentiation / divergence",
                show_points,
            )

    with st.expander("Per-locus observed statistics (table)"):
        st.dataframe(
            with_locus_name(df, data.locus_infos),
            width="stretch",
            height=320,
        )
