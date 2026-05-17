"""Upload & overview tab."""

from __future__ import annotations

import streamlit as st

from results.parsers import ParsedResults


def render(data: ParsedResults) -> None:
    st.subheader("Run overview")

    gi = data.general_infos
    nref_label = f"{data.nref:,.0f}" if data.nref is not None else "—"
    with st.container(border=True):
        if data.is_one_pop:
            c1, c2, c3 = st.columns(3)
            c1.metric("Population", data.name_a)
            c2.metric("Loci", data.n_loci)
            c3.metric("Nref", nref_label)
        else:
            c1, c2, c3, c4 = st.columns(4)
            c1.metric("Population A", data.name_a)
            c2.metric("Population B", data.name_b)
            c3.metric("Loci", data.n_loci)
            c4.metric("Nref", nref_label)

        st.caption(
            f"Archive: **{data.display_name}** · "
            f"Date: {gi.get('date', '—')} · "
            f"Mail: {gi.get('mail', '—')}"
        )

    with st.container(border=True):
        st.subheader("Analysis configuration")
        cfg_rows = [
            ("Region", data.config.get("region")),
            ("Light mode", data.config.get("lightMode")),
            ("Use SFS", data.config.get("useSFS")),
            ("Population growth", data.config.get("population_growth")),
            ("Mode barrier", data.config.get("modeBarrier")),
            ("Outgroup", data.config.get("nameOutgroup")),
            ("μ", data.config.get("mu")),
            ("ρ/θ", data.config.get("rho_over_theta")),
            ("Lmin", data.config.get("Lmin")),
            ("nMin", data.config.get("nMin")),
        ]
        if data.is_one_pop:
            two_pop_only = {"Use SFS", "Population growth", "Mode barrier"}
            cfg_rows = [row for row in cfg_rows if row[0] not in two_pop_only]
        st.table(
            {"Parameter": [r[0] for r in cfg_rows], "Value": [str(r[1]) for r in cfg_rows]}
        )

    with st.expander("Full config.yaml"):
        st.json(data.config)

