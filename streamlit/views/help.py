"""Static Help page for the Streamlit DILS interface."""

from __future__ import annotations

import streamlit as st


def _summary_statistics_rows() -> list[dict[str, str]]:
    return [
        {
            "Category": "General",
            "Statistic": "dataset",
            "Meaning": "Name of the target locus.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "sf_avg",
            "Meaning": "Fraction of sites with a fixed difference between the populations/species.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "sxA_avg; sxB_avg",
            "Meaning": "Fraction of sites with a polymorphism specific to each population/species.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "ss_avg",
            "Meaning": "Fraction of sites with a polymorphism shared between the populations/species.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "successive_ss_avg",
            "Meaning": "Maximal number of successive shared polymorphic sites in the target locus.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "ss_sf",
            "Meaning": "1 if the target locus has at least one shared polymorphism and one fixed difference; 0 otherwise.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "ss_noSf",
            "Meaning": "1 if the target locus has at least one shared polymorphism but no fixed difference; 0 otherwise.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "noSs_sf",
            "Meaning": "1 if the target locus has no shared polymorphism but at least one fixed difference; 0 otherwise.",
        },
        {
            "Category": "Summarized jSFS",
            "Statistic": "noSs_noSf",
            "Meaning": "1 if the target locus has no shared polymorphism and no fixed difference; 0 otherwise.",
        },
        {
            "Category": "Polymorphism",
            "Statistic": "piA_avg; piB_avg",
            "Meaning": "Pairwise nucleotide diversity pi for each population/species.",
        },
        {
            "Category": "Polymorphism",
            "Statistic": "thetaA_avg; thetaB_avg",
            "Meaning": "Watterson's theta for each population/species.",
        },
        {
            "Category": "Tajima's D",
            "Statistic": "DtajA_avg; DtajB_avg",
            "Meaning": "Tajima's D for each population/species.",
        },
        {
            "Category": "Differentiation and divergence",
            "Statistic": "divAB_avg",
            "Meaning": "Raw divergence Dxy between the populations/species.",
        },
        {
            "Category": "Differentiation and divergence",
            "Statistic": "netdivAB_avg",
            "Meaning": "Net divergence Da between populations/species, computed as Dxy - (piA + piB) / 2.",
        },
        {
            "Category": "Differentiation and divergence",
            "Statistic": "FST_avg",
            "Meaning": "FST measured as 1 - piS / piT, where piS is average within-population diversity and piT is total diversity.",
        },
    ]


def _parameter_rows() -> list[dict[str, str]]:
    return [
        {
            "Parameter": "Na",
            "Meaning": "Effective size of the ancestral population, in diploid individuals.",
        },
        {
            "Parameter": "N1; N2",
            "Meaning": "Effective size of population 1 and population 2, in diploid individuals.",
        },
        {
            "Parameter": "shape_N_a; shape_N_b",
            "Meaning": "Alpha and beta shape parameters of the Beta distribution used when effective population size is heterogeneous across loci.",
        },
        {
            "Parameter": "Tdem1; Tdem2",
            "Meaning": "Time of demographic change in population 1 or 2, in generations.",
        },
        {
            "Parameter": "founders1; founders2",
            "Meaning": "Number of founder individuals at the time of demographic change.",
        },
        {
            "Parameter": "Tsplit",
            "Meaning": "Time at which the ancestral population splits into two populations, in generations.",
        },
        {
            "Parameter": "Tsc",
            "Meaning": "Time of secondary contact, when two previously isolated populations start exchanging genes again.",
        },
        {
            "Parameter": "Tam",
            "Meaning": "Time at which ancestral migration stops.",
        },
        {
            "Parameter": "M12; M21",
            "Meaning": "Introgression rate from population 2 to 1 and from population 1 to 2, in number of migrants per generation.",
        },
        {
            "Parameter": "nBarriersM12; nBarriersM21",
            "Meaning": "Number of loci inferred as barriers to introgression in each direction under the bimodal M-hetero model.",
        },
        {
            "Parameter": "shape_M12_a; shape_M12_b",
            "Meaning": "Alpha and beta shape parameters of the Beta distribution for N.m from population 2 to 1.",
        },
        {
            "Parameter": "shape_M21_a; shape_M21_b",
            "Meaning": "Alpha and beta shape parameters of the Beta distribution for N.m from population 1 to 2.",
        },
    ]


def _render_overview() -> None:
    st.header("Overview")
    st.markdown(
        """
        DILS means **Demographic Inferences with Linked Selection**.

        DILS is a DNA-sequence analysis workflow designed to study the demographic
        history of sampled populations or closely related species using Approximate
        Bayesian Computation.

        From a single uploaded FASTA file containing sequenced genes or DNA fragments,
        DILS can:

        1. simulate alternative demographic and genomic models;
        2. select the best-supported model using an ABC random-forest approach;
        3. estimate the parameters of the best model using neural-network and
           random-forest approaches;
        4. evaluate whether the inferred model can reproduce the observed data using
           goodness-of-fit analyses.

        The main goal of DILS is to distinguish between isolation and migration models
        of divergence between sister gene pools. For two-population analyses, DILS can
        also infer whether some loci are associated with reduced introgression and may
        therefore behave as genomic barriers.

        DILS can also be used with one population/species to compare alternative models
        of demographic change, such as constant population size, expansion, or contraction.
        """
    )


def _render_workflow() -> None:
    st.header("How to use this interface")
    st.markdown(
        """
        1. Open **Submit DILS analysis**.
        2. Upload a FASTA file.
        3. Let the app detect population/species names from FASTA headers.
        4. Choose whether to run a 1-population or 2-population analysis.
        5. Select the population/species names to analyse.
        6. Set the analysis options and prior bounds.
        7. Click **Submit DILS analysis**.
        8. Refresh the job status until the analysis is completed.
        9. Download the results archive.
        10. Open the archive in **Results viewer**.

        The FASTA file is copied into the run directory. If configured by the
        administrator, the uploaded FASTA can be deleted automatically after a successful
        run once the final archive has been produced.

        The **Email address** field is optional unless notifications are enabled by the
        server administrator. When notifications are enabled, this address is used to
        notify the user when the analysis is submitted and when it completes or fails.
        Results remain downloadable from the interface.

        Computational resources such as CPU count and memory are configured by the
        server administrator.

        The final results archive is a `.tar.gz` file that can be opened in the
        **Results viewer**.
        """
    )


def _render_models() -> None:
    st.header("Compared demographic models")

    st.subheader("1 population/species")
    st.markdown(
        """
        - **Constant**: a single panmictic population with constant effective size over time.
        - **Expansion**: the current population is larger than in the past, with a
          demographic change at time `Tdem`.
        - **Contraction**: the current population is smaller than in the past, with a
          demographic change at time `Tdem`.
        """
    )

    st.subheader("2 populations/species")
    st.markdown(
        """
        - **SI, strict isolation**: an ancestral population splits into two populations
          at time `Tsplit`, with no subsequent gene flow.
        - **AM, ancestral migration**: the two daughter populations exchange genes after
          the split, then stop exchanging genes at time `Tam`.
        - **IM, isolation with migration**: the two daughter populations continuously
          exchange genes after the split until the present.
        - **SC, secondary contact**: the two daughter populations first evolve in
          isolation, then resume gene exchange at time `Tsc`.
        """
    )

    st.header("Compared genomic models")
    st.markdown(
        """
        For all demographic models, DILS compares alternative genomic models of effective
        population size:

        - **N-homo**: effective population size is homogeneous across loci.
        - **N-hetero**: effective population size varies across loci and is modelled with
          a Beta distribution.

        For demographic models with migration, DILS also compares alternative genomic
        models of introgression:

        - **M-homo**: introgression rate is homogeneous across loci.
        - **M-hetero**: introgression rate varies across loci.
        - Under a beta model, variation in introgression is modelled with a Beta distribution.
        - Under a bimodal model, loci can be allocated to classes with high or reduced
          introgression, which can be interpreted as candidate genomic barriers.
        """
    )


def _render_statistics() -> None:
    st.header("Summary statistics")
    st.table(_summary_statistics_rows())

    with st.expander("References"):
        st.markdown(
            """
            - Nei, M. (1987). *Molecular Evolutionary Genetics*. Columbia University Press.
            - Nei, M. & Li, W-H. (1979). Mathematical model for studying genetic variation
              in terms of restriction endonucleases. *PNAS*, 76: 5269-5273.
            - Tajima, F. (1983). Evolutionary relationship of DNA sequences in finite
              populations. *Genetics*, 105: 437-460.
            - Tajima, F. (1989). The effect of change in population size on DNA
              polymorphism. *Genetics*, 123: 597-601.
            - Watterson, G. A. (1975). On the number of segregating sites in genetical
              models without recombination. *Theoretical Population Biology*, 7: 256-276.
            - Wright, S. (1943). Isolation by distance. *Genetics*, 28: 114-138.
            """
        )


def _render_parameters() -> None:
    st.header("Model parameters")
    st.table(_parameter_rows())


def _render_fasta_format() -> None:
    st.header("FASTA input format")
    st.markdown(
        """
        DILS expects a single FASTA file containing all sequences for all loci,
        individuals, alleles, and populations/species.

        The same FASTA file may contain populations/species that are not used in a
        specific analysis. The user selects which populations/species to analyse in the
        interface after upload.

        Each sequence header must follow this structure:
        """
    )
    st.code(">locus|population_or_species|individual|allele", language="text")

    with st.expander("FASTA header example"):
        st.code(
            """>locus001|PopA|ind01|allele1
ACGTACGTACGT
>locus001|PopA|ind01|allele2
ACGTACGTACGA
>locus001|PopB|ind07|allele1
ACGTACCTACGT
>locus002|PopB|ind07|allele2
ACGTACCTACGA""",
            language="text",
        )
        st.markdown(
            """
            The interface reads the second field of each header to detect available
            population/species names. Spaces around `|` separators are tolerated, but
            headers without the four expected fields are ignored for population detection.
            """
        )


def render() -> None:
    _render_overview()
    _render_workflow()
    _render_models()
    _render_statistics()
    _render_parameters()
    _render_fasta_format()
