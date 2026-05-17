# DILS

DILS means **Demographic Inferences with Linked Selection**.

DILS is a workflow for demographic inference from multilocus DNA sequence data. It uses Approximate Bayesian Computation (ABC) for model comparison, parameter estimation, and goodness-of-fit analyses.

## Documentation

- User manual: [`docs/user_manual.md`](docs/user_manual.md)
- Cluster/admin deployment guide: [`docs/cluster_deployment.md`](docs/cluster_deployment.md)
- Streamlit configuration example: [`streamlit/settings.example.yaml`](streamlit/settings.example.yaml)

## Ways to Use DILS

DILS can currently be used in two main ways:

1. **Streamlit web interface**: upload a FASTA file, configure an analysis, submit it through Slurm, download the results archive, and inspect outputs in the Results viewer.
2. **Command-line Snakemake workflow**: run the DILS Snakefiles directly with a YAML configuration file.

The repository may still contain the older R/Shiny web interface under `webinterface/`. It is kept as legacy code and is not the recommended interface for new deployments.

## Streamlit Web Interface

The Streamlit interface lets users:

- submit a DILS analysis;
- monitor Slurm job status;
- download the final `.tar.gz` results archive;
- inspect results in the Results viewer;
- read the integrated Help page.

Local launch example:

```bash
streamlit run streamlit/app.py
```

For configuration, copy and edit:

```bash
cp streamlit/settings.example.yaml streamlit/settings.yaml
```

`streamlit/settings.yaml` is local configuration and is ignored by Git. Cluster deployment is documented in [`docs/cluster_deployment.md`](docs/cluster_deployment.md).

## Command-Line Snakemake Workflow

DILS can also be run directly with Snakemake and a YAML configuration file.

1-population analysis:

```bash
snakemake --snakefile bin/Snakefile_1pop -p -j 10 --configfile config_1pop.yaml
```

2-population analysis:

```bash
snakemake --snakefile bin/Snakefile_2pop -p -j 10 --configfile config_2pop.yaml
```

The Streamlit interface generates YAML internally. Command-line users should provide their own YAML file.

## YAML Configuration Examples

### 1-Population Example

```yaml
mail_address: user@example.org
infile: /path/to/input.fas
region: coding
nspecies: 1
nameA: PopulationA
nameOutgroup: NA
lightMode: TRUE
config_yaml: /path/to/config_1pop.yaml
timeStamp: my_1pop_run
max_N_tolerated: 0.2
Lmin: 100
nMin: 6
mu: 0.00000002763
rho_over_theta: 0.5
N_min: 1000
N_max: 500000
Tchanges_min: 100
Tchanges_max: 1000000
```

### 2-Population Example

```yaml
mail_address: user@example.org
infile: /path/to/input.fas
region: coding
nspecies: 2
nameA: PopulationA
nameB: PopulationB
nameOutgroup: NA
lightMode: TRUE
useSFS: 0
config_yaml: /path/to/config_2pop.yaml
timeStamp: my_2pop_run
population_growth: constant
modeBarrier: bimodal
max_N_tolerated: 0.2
Lmin: 100
nMin: 6
mu: 0.00000002763
rho_over_theta: 0.5
N_min: 1000
N_max: 500000
Tsplit_min: 10000
Tsplit_max: 1750000
M_min: 1
M_max: 40
```

## Runtime Dependencies

Runtime dependencies depend on whether DILS is used through the Streamlit interface or directly with Snakemake. In general, DILS requires:

- Python;
- PyPy where required by legacy scripts;
- R and required R packages;
- Snakemake;
- Slurm for cluster submission;
- Streamlit dependencies from `streamlit/requirements.txt`;
- C helper binaries such as `msnsam` and `RNAseqFGT`.

Cluster administrators should use [`docs/cluster_deployment.md`](docs/cluster_deployment.md) for deployment-specific details.

## Repository Layout

- `bin/`: DILS Snakemake workflows and analysis scripts.
- `streamlit/`: current Streamlit web interface.
- `docs/`: user and administrator documentation.
- `example/`: example input and results files.
- `webinterface/`: legacy R/Shiny interface.
- `msnsam/`, `RNAseqFGT_src/`: C sources and helper binaries.

## Citation / Support

For scientific background, usage details, and references, see the user manual:

[`docs/user_manual.md`](docs/user_manual.md)
