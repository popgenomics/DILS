"""Multilocus hierarchical model comparison tab."""

from __future__ import annotations

import plotly.express as px
import streamlit as st

from plot_style import apply_readable_style
from results.parsers import ParsedResults
from results.schema import hierarchical_labels


def _is_top_level_migration(data: ParsedResults) -> bool:
    if data.hierarchical.shape[1] < 1 or data.hierarchical.shape[0] < 2:
        return False
    return str(data.hierarchical.iloc[1, 0]).strip().lower() == "migration"


def _hierarchical_winner(data: ParsedResults, label: str) -> str | None:
    if data.hierarchical.shape[0] < 2:
        return None
    first_winner = str(data.hierarchical.iloc[1, 0]).strip()
    labels = hierarchical_labels(first_winner, data.n_species)
    if label not in labels:
        return None
    index = labels.index(label)
    if index >= data.hierarchical.shape[1]:
        return None
    return str(data.hierarchical.iloc[1, index]).strip()


def _is_migration_heterogeneous(data: ParsedResults) -> bool:
    winner = _hierarchical_winner(data, "M-homo versus M-hetero")
    return winner == "Mhetero"


def _render_allocation_summary(data: ParsedResults) -> None:
    if (
        not data.is_two_pop
        or not _is_top_level_migration(data)
        or not _is_migration_heterogeneous(data)
    ):
        return
    if data.locus_specific.empty or "allocation" not in data.locus_specific.columns:
        return

    st.subheader("Allocation summary (locus-specific models)")
    counts = data.locus_specific["allocation"].value_counts().reset_index()
    counts.columns = ["Allocation", "Loci"]
    fig = px.bar(counts, x="Allocation", y="Loci", text="Loci")
    apply_readable_style(
        fig,
        height=320,
        xaxis_title="",
        yaxis_title="Number of loci",
    )
    st.plotly_chart(fig, width="stretch")


def _render_hierarchy_cards(rows: list[dict[str, str | float]]) -> None:
    st.caption("Hierarchical comparisons are shown in the order evaluated by DILS.")
    for start in range(0, len(rows), 3):
        cols = st.columns(min(3, len(rows) - start))
        for col, row in zip(cols, rows[start : start + 3]):
            with col:
                with st.container(border=True):
                    st.markdown(f"**{row['Comparison']}**")
                    st.markdown(
                        f"<div style='font-size:1.65rem; line-height:1.2; "
                        f"font-weight:600; margin:0.35rem 0;'>"
                        f"{row['Selected model / Value']}</div>",
                        unsafe_allow_html=True,
                    )
                    st.caption(f"PP = {row['Posterior probability']:.4f}")


def render(data: ParsedResults) -> None:
    st.subheader("Multilocus model comparison")

    h = data.hierarchical
    if h.shape[0] < 3:
        st.error("hierarchical_models.txt has fewer than 3 rows.")
        return

    first_winner = str(h.iloc[1, 0]).strip()
    labels = hierarchical_labels(first_winner, data.n_species)
    n_steps = min(len(labels), h.shape[1])

    if data.is_one_pop:
        st.info(f"Best demographic-size scenario: **{first_winner}**.")
    else:
        st.info(
            f"Top-level outcome: **{first_winner}** "
            f"({'gene flow' if first_winner.lower() == 'migration' else 'isolation'} scenario)."
        )

    rows = []
    for i in range(n_steps):
        rows.append(
            {
                "Comparison": labels[i],
                "Selected model / Value": str(h.iloc[1, i]),
                "Posterior probability": float(h.iloc[2, i]),
            }
        )
    _render_hierarchy_cards(rows)

    _render_allocation_summary(data)

    with st.expander("Raw hierarchical_models.txt"):
        st.dataframe(
            h,
            width="stretch",
            column_config={
                0: "Row",
            },
        )
