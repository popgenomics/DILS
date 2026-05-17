# DILS user manual

## 1. Introduction

DILS means **Demographic Inferences with Linked Selection**.

DILS is a DNA-sequence analysis workflow for studying the demographic history of sampled populations or closely related species using Approximate Bayesian Computation (ABC). It compares alternative demographic and genomic models, estimates parameters of the best-supported model, and evaluates whether the inferred model can reproduce the observed data.

DILS is designed to help distinguish the effects of demographic history, gene flow, and linked selection on patterns of polymorphism and divergence. In practice, it can be used for model comparison, parameter estimation, and goodness-of-fit analyses from multilocus FASTA data.

## 2. What DILS Does

DILS can:

- analyse one-population datasets to compare demographic change models such as constant size, expansion, and contraction;
- analyse two-population/species datasets to compare divergence models;
- distinguish isolation models from migration models;
- test whether effective population size varies across loci;
- test whether introgression varies across loci in migration models;
- identify loci potentially associated with barriers to gene flow in two-population migration models.

The workflow first compares demographic and genomic models, then estimates parameters for the best-supported model, and finally performs goodness-of-fit analyses.

## 3. Using the Streamlit Interface

The current web interface has three user-facing modes:

- **Submit DILS analysis**: upload data, configure an analysis, submit it, and monitor status.
- **Results viewer**: load a completed DILS `.tar.gz` results archive and explore outputs.
- **Help**: read a compact in-app reference.

Typical workflow:

1. Upload a FASTA file.
2. Let DILS detect population/species names from FASTA headers.
3. Choose a 1-population or 2-population analysis.
4. Select focal populations/species and, optionally, an outgroup.
5. Configure filtering and prior options.
6. Enter an email address if notifications are enabled by the server administrator.
7. Click **Submit DILS analysis**.
8. Refresh job status until the analysis completes.
9. Download the final `.tar.gz` archive.
10. Open the archive in **Results viewer**.

The YAML configuration is generated internally from the web form. Users normally do not edit it. Slurm submission is handled by the web app, and CPU/RAM resources are configured by administrators. Users do not edit Snakefiles.

## 4. FASTA Input Format

DILS expects one FASTA file containing all sequences for all loci, individuals, alleles, and populations/species.

The same FASTA file can contain populations/species that are not used in a specific analysis. After upload, the interface detects population/species names from headers and lets the user choose which ones to analyse.

Sequences for a given locus should be aligned homologous positions. Missing data should be encoded with `N`, and gaps can be encoded with `-`. If sequence data are missing for a locus, individual, allele, or population/species, keep the corresponding FASTA entry when possible and fill the unavailable sequence with `N` characters.

Each sequence header must follow this structure:

```text
>locus|population_or_species|individual|allele
```

Example:

```text
>Hmel210004_196|chi|chi.CJ560|allele1
NNNNNNNGGCCAGTATTATCTACGCACGTGTTAGACACCTCNACTGGTCAGCCAGGAAGTGGAATTTTCGTCGAATTATACAAA

>Hmel210004_196|chi|chi.CJ560|allele2
NNNNNNNGGCCAGTATTATCTACGCACGTGTTAGACACTTCNACTGGTCAGCCAGGAAGTGGAATTTTCGTCGAATTATACAAA
```

In this example:

- `Hmel210004_196` is the locus name;
- `chi` is the population/species name;
- `chi.CJ560` is the individual name;
- `allele1` and `allele2` identify alleles.

The interface uses the second field, `population_or_species`, to detect selectable populations/species.

## 5. Analysis Configuration

The web form generates the YAML configuration used by DILS. Users normally configure analyses through the interface rather than editing YAML manually.

Main fields:

- `region`: genomic region type, usually `coding` or `noncoding`.
- `nspecies`: number of populations/species analysed, `1` or `2`.
- `nameA`: focal population/species for one-population analyses, or first population/species for two-population analyses.
- `nameB`: second population/species for two-population analyses.
- `nameOutgroup`: optional outgroup; use `NA` when no outgroup is used.
- `useSFS`: whether to use the site frequency spectrum in two-population analyses.
- `lightMode`: fixed by the interface for the current workflow.
- `population_growth`: for two-population analyses, whether population size is constant or variable.
- `modeBarrier`: for two-population analyses, the modelling mode for heterogeneous introgression, such as beta or bimodal.
- filtering parameters: thresholds used to remove poorly covered or unsuitable loci.
- prior bounds: parameter ranges explored by ABC.

The generated YAML stores the complete run configuration, including the copied FASTA path and run identifier.

## 6. Filtering Options

Filtering controls which loci are retained for analysis.

- `max_N_tolerated`: maximum proportion of `N` or gaps tolerated in a sequence for a locus. Sequences above this threshold are ignored.
- `Lmin`: minimum number of treatable sites required for a locus.
- `nMin`: minimum number of sequences per locus and per population/species. If too few sequences remain after filtering, the locus is excluded.

For noncoding sequence, a site is an aligned nucleotide position across the selected individuals. For coding sequence, a site is treated in codon context; problematic codon alignments, non-synonymous polymorphisms in a codon, excessive ambiguity, or incompatible codon variation can make a position unsuitable.

If an outgroup is used, the number of sequences sampled at each locus is standardized for the selected populations/species.

## 7. Prior Options

Prior options define the parameter space explored by DILS.

- `mu`: mutation rate per site per generation.
- `rho_over_theta`: ratio of recombination over mutation. DILS does not rely on haplotype-based statistics, so the exact value mainly needs to be positive and biologically plausible.
- `N_min` / `N_max`: lower and upper bounds for effective population sizes.
- `Tchanges_min` / `Tchanges_max`: lower and upper bounds for demographic change time in one-population analyses.
- `Tsplit_min` / `Tsplit_max`: lower and upper bounds for split time in two-population analyses.
- `M_min` / `M_max`: lower and upper bounds for migration/introgression parameters in two-population models with gene flow.

If posterior estimates often accumulate at prior bounds, consider whether the prior range is too narrow or whether the model does not capture important structure in the data.

## 8. Compared Demographic Models

### One Population/Species

- **Constant**: a single panmictic population with constant effective size over time.
- **Expansion**: the current population is larger than in the past, with a demographic change at time `Tdem`.
- **Contraction**: the current population is smaller than in the past, with a demographic change at time `Tdem`.

### Two Populations/Species

- **SI, strict isolation**: an ancestral population splits into two populations at time `Tsplit`, with no subsequent gene flow.
- **AM, ancestral migration**: daughter populations exchange genes after the split, then stop exchanging genes at time `Tam`.
- **IM, isolation with migration**: daughter populations continuously exchange genes after the split until the present.
- **SC, secondary contact**: daughter populations first evolve in isolation, then resume gene exchange at time `Tsc`.

Outgroups can be used to unfold the site frequency spectrum and correct local mutation rates by divergence to the outgroup. It is better not to use an outgroup than to use a poor outgroup with extensive incomplete lineage sorting.

## 9. Compared Genomic Models

DILS relaxes the assumption that all loci share the same demographic history.

For all demographic models, DILS compares:

- **N-homo**: effective population size is homogeneous across loci.
- **N-hetero**: effective population size varies across loci, modelled with a Beta distribution.

For demographic models with migration, DILS also compares:

- **M-homo**: introgression rate is homogeneous across loci.
- **M-hetero**: introgression rate varies across loci.

Under a **beta model**, variation in introgression is modelled with a Beta distribution. Under a **bimodal model**, loci can be allocated to classes with high or reduced introgression. Loci inferred to have reduced introgression can be interpreted as candidate genomic barriers to gene flow, subject to biological validation.

## 10. Model Parameters

| Parameter | Meaning |
| --- | --- |
| `Na` | Effective size of the ancestral population, in diploid individuals. |
| `N1` / `N2` | Effective size of population 1 and population 2, in diploid individuals. |
| `shape_N_a` / `shape_N_b` | Alpha and beta shape parameters of the Beta distribution used when effective population size is heterogeneous across loci. |
| `Tdem1` / `Tdem2` | Time of demographic change in population 1 or 2, in generations. |
| `founders1` / `founders2` | Number of founder individuals at the time of demographic change. |
| `Tsplit` | Time at which the ancestral population splits into two populations, in generations. |
| `Tsc` | Time of secondary contact, when two previously isolated populations start exchanging genes again. |
| `Tam` | Time at which ancestral migration stops. |
| `M12` / `M21` | Introgression rate from population 2 to 1 and from population 1 to 2, in number of migrants per generation. |
| `nBarriersM12` / `nBarriersM21` | Number of loci inferred as barriers to introgression in each direction under the bimodal M-hetero model. |
| `shape_M12_a` / `shape_M12_b` | Alpha and beta shape parameters of the Beta distribution for introgression from population 2 to 1. |
| `shape_M21_a` / `shape_M21_b` | Alpha and beta shape parameters of the Beta distribution for introgression from population 1 to 2. |

## 11. Summary Statistics

| Category | Statistic | Meaning |
| --- | --- | --- |
| General | `dataset` | Name of the target locus. |
| Summarized jSFS | `sf_avg` | Fraction of sites with a fixed difference between the populations/species. |
| Summarized jSFS | `sxA_avg` / `sxB_avg` | Fraction of sites with a polymorphism specific to each population/species. |
| Summarized jSFS | `ss_avg` | Fraction of sites with a polymorphism shared between the populations/species. |
| Summarized jSFS | `successive_ss_avg` | Maximal number of successive shared polymorphic sites in the target locus. |
| Summarized jSFS | `ss_sf` | `1` if the target locus has at least one shared polymorphism and one fixed difference; `0` otherwise. |
| Summarized jSFS | `ss_noSf` | `1` if the target locus has at least one shared polymorphism but no fixed difference; `0` otherwise. |
| Summarized jSFS | `noSs_sf` | `1` if the target locus has no shared polymorphism but at least one fixed difference; `0` otherwise. |
| Summarized jSFS | `noSs_noSf` | `1` if the target locus has no shared polymorphism and no fixed difference; `0` otherwise. |
| Polymorphism | `piA_avg` / `piB_avg` | Pairwise nucleotide diversity π for each population/species. |
| Polymorphism | `thetaA_avg` / `thetaB_avg` | Watterson's θ for each population/species. |
| Tajima's D | `DtajA_avg` / `DtajB_avg` | Tajima's D for each population/species. |
| Differentiation and divergence | `divAB_avg` | Raw divergence Dxy between populations/species. |
| Differentiation and divergence | `netdivAB_avg` | Net divergence Da between populations/species, computed as Dxy - (piA + piB) / 2. |
| Differentiation and divergence | `FST_avg` | FST measured as 1 - πS / πT, where πS is average within-population diversity and πT is total diversity. |

Key references for these statistics include Nei (1987), Nei and Li (1979), Tajima (1983, 1989), Watterson (1975), and Wright (1943).

## 12. Submission, Status, and Resources

When a run is submitted, the app writes run files, generates the YAML configuration, and submits the analysis to Slurm. Users can refresh job status from the interface.

Common statuses:

- `submitted`: the Slurm job was accepted.
- `running`: Slurm reports a running or pending-like state.
- `completed`: the job completed successfully and the final archive exists.
- `failed`: the job failed, timed out, was cancelled, or exited non-zero.
- `completed_missing_archive`: Slurm completed successfully, but the expected results archive was not found.

CPU and memory resources are configured by administrators, not selected by users. The uploaded FASTA may be deleted automatically after successful completion if this is enabled by the administrator.

Cluster deployment details are documented separately in `docs/cluster_deployment.md`.

## 13. Email Notifications

Email notifications are optional and configured by server administrators.

If notifications are enabled, the **Email address** field is required before submission. Notifications may report:

- successful submission;
- successful completion;
- failure or missing archive.

Results remain downloadable from the interface. Depending on server configuration, notifications may be saved as test `.eml` files for administrator review instead of being sent as real emails.

## 14. Results Archive

The final output of a completed DILS analysis is a `.tar.gz` results archive.

After completion, the archive can be downloaded from the Submit page. The same archive can be loaded in the **Results viewer** without manual extraction.

Keep this archive if you want to reopen, share, or archive the analysis results.

## 15. Results Viewer

The **Results viewer** opens a completed DILS `.tar.gz` archive and displays result tabs.

- **Overview**: basic run information, selected populations/species, number of loci, `Nref`, and configuration.
- **Model comparison**: hierarchical model comparison and, when applicable, locus-specific allocation summaries for candidate barrier loci.
- **Observed statistics**: per-locus summary statistics displayed as plots and tables.
- **Parameters**: prior, posterior, and optimized posterior distributions; parameter summaries where available.
- **Goodness of fit**: PCA, summary-statistic goodness-of-fit, relative deviation plots, and SFS/jSFS goodness-of-fit.

If the Results viewer cannot load an archive, check that the uploaded file is the final DILS `.tar.gz` archive and not an extracted directory or unrelated compressed file.

## 16. Goodness-of-Fit

Goodness-of-fit analyses evaluate whether simulations under the fitted model can reproduce the observed data.

The Results viewer can display:

- PCA of observed, prior, posterior, and optimized posterior simulations;
- summary-statistic goodness-of-fit tables;
- observed versus expected relative deviation plots;
- SFS goodness-of-fit for one-population analyses;
- joint SFS heatmaps for two-population analyses;
- a selector for posterior versus optimized posterior distributions when both are available.

Goodness-of-fit does not prove that a model is absolutely true. It is a diagnostic: a poor fit suggests the model or priors may not capture important features of the data.

## 17. Example Workflow

A typical small test run proceeds as follows:

1. Open **Submit DILS analysis**.
2. Upload a FASTA file, for example a small `mytilus.fas` dataset if available in the deployment.
3. Confirm that population/species names are detected.
4. Choose a 1-population or 2-population analysis.
5. Select the focal population/species names.
6. Optionally select an outgroup if one is appropriate.
7. Review filtering and prior options.
8. Enter an email address if notifications are enabled.
9. Submit the analysis.
10. Refresh job status until it reaches `completed`.
11. Download the final `.tar.gz` archive.
12. Open the archive in **Results viewer**.

For routine analyses, use biologically meaningful prior bounds and filtering thresholds rather than relying only on defaults.

## 18. Troubleshooting for Users

| Problem | What to Check |
| --- | --- |
| FASTA upload fails | Confirm the file is a plain FASTA file and is within the server upload limit. |
| No populations are detected | Check that headers follow `>locus|population_or_species|individual|allele`. |
| Wrong population names appear | Check the second field of each FASTA header. This field defines the detected population/species name. |
| Job fails | Refresh status and, if available, contact the server administrator with the run ID. |
| Archive is missing | The job may have completed without producing the expected archive. Contact the server administrator with the run ID. |
| Notification not received | Confirm the email address was entered correctly and ask whether notifications are enabled on the server. Results remain downloadable from the interface. |
| Results viewer cannot load archive | Upload the final DILS `.tar.gz` archive, not an extracted folder or intermediate file. |

## 19. References

- Galtier, N., Roux, C., Rousselle, M., Romiguier, J., Figuet, E., Glemin, S., Bierne, N. & Duret, L. (2018). Codon usage bias in animals: disentangling the effects of natural selection, effective population size, and GC-biased gene conversion. *Molecular Biology and Evolution*, 35(5): 1092-1103.
- Hudson, R. (2002). Generating samples under a Wright-Fisher neutral model of genetic variation. *Bioinformatics*, 18: 337-338.
- Hudson, R. R. & Kaplan, N. L. (1985). Statistical properties of the number of recombination events in the history of a sample of DNA sequences. *Genetics*, 111(1): 147-164.
- Langmead, B. & Salzberg, S. L. (2012). Fast gapped-read alignment with Bowtie 2. *Nature Methods*, 9(4): 357.
- Li, H., Handsaker, B., Wysoker, A., Fennell, T., Ruan, J., Homer, N., Marth, G., Abecasis, G. & Durbin, R. (2009). The Sequence Alignment/Map format and SAMtools. *Bioinformatics*, 25(16): 2078-2079.
- Nei, M. (1987). *Molecular Evolutionary Genetics*. Columbia University Press.
- Nei, M. & Li, W-H. (1979). Mathematical model for studying genetic variation in terms of restriction endonucleases. *PNAS*, 76: 5269-5273.
- Ross-Ibarra, J., Wright, S. I., Foxe, J. P., Kawabe, A., DeRose-Wilson, L., et al. (2008). Patterns of polymorphism and demographic history in natural populations of *Arabidopsis lyrata*. *PLoS ONE*, 3(6): e2411.
- Roux, C., Fraisse, C., Romiguier, J., Anciaux, Y., Galtier, N. & Bierne, N. (2016). Shedding light on the grey zone of speciation along a continuum of genomic divergence. *PLoS Biology*, 14(12): e2000234.
- Tajima, F. (1983). Evolutionary relationship of DNA sequences in finite populations. *Genetics*, 105(2): 437-460.
- Tajima, F. (1989). The effect of change in population size on DNA polymorphism. *Genetics*, 123(3): 597-601.
- Tsagkogeorga, G., Cahais, V. & Galtier, N. (2012). The population genomics of a fast evolver: high levels of diversity, functional constraint, and molecular adaptation in the tunicate *Ciona intestinalis*. *Genome Biology and Evolution*, 4(8): 740-749.
- Watterson, G. A. (1975). On the number of segregating sites in genetical models without recombination. *Theoretical Population Biology*, 7(2): 256-276.
- Wright, S. (1943). Isolation by distance. *Genetics*, 28: 114-138.
