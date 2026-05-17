# DILS Streamlit cluster deployment guide

## 1. Purpose

This Streamlit app is a web interface around DILS, “Demographic Inferences with Linked Selection”.

In a cluster deployment, the app lets users:

1. upload a FASTA file;
2. detect population/species names from FASTA headers;
3. generate a DILS YAML configuration;
4. store run files in a configured runs directory;
5. submit Snakemake through Slurm;
6. track job status with `sacct`;
7. download the final `.tar.gz` results archive;
8. optionally delete the uploaded FASTA after successful completion.

The Streamlit app manages web inputs, run directories, metadata, and Slurm submission. DILS and Snakemake perform the scientific workflow.

## 2. Main components

| Component | Role |
| --- | --- |
| Streamlit app | User-facing web interface. Handles uploads, population selection, YAML generation, Slurm submission, status refresh, and archive download. |
| `DILS/bin` | Contains the DILS scripts and workflow files used by Snakemake. |
| `Snakefile_1pop` | Snakemake workflow used for one-population/species analyses. |
| `Snakefile_2pop` | Snakemake workflow used for two-population/species analyses. |
| Snakemake | Executes the DILS workflow inside the Slurm allocation. |
| Slurm / `sbatch` | Queues the Snakemake command as a batch job. |
| Slurm / `sacct` | Reports job state and exit code during status refresh. |
| Run directory | Per-analysis directory created by the Streamlit app under `runs_root`. |
| `metadata.json` | App-managed lifecycle record for one run. Stores paths, status, Slurm job ID, commands, and archive state. |
| Final archive | The `.tar.gz` file produced by DILS/Snakemake. This is what users download and open in the Results viewer. |

## 3. Expected repository layout

Relevant paths:

```text
DILS/
  bin/
    Snakefile_1pop
    Snakefile_2pop
    ...
streamlit/
  app.py
  settings.py
  settings.example.yaml
  run_manager.py
  views/
.streamlit/
  config.toml
streamlit_runs/   # local default only; deployment should configure another path
```

The deployment should use the repository as a read-only application directory where possible. Runtime files should be written to a separate shared or scratch location configured with `runs_root`.

## 4. Python and command dependencies

The Streamlit app imports and expects the Python environment to provide at least:

- `streamlit`
- `pandas`
- `plotly`
- `PyYAML`

The execution environment also needs these command-line tools available to the Streamlit server process:

- `snakemake`
- `sbatch`
- `sacct`

Cluster-specific DILS dependencies must be available to the Snakemake jobs in the environment used by Slurm. The Streamlit app submits a wrapped command through `sbatch`; it does not itself load modules or activate Conda environments unless the configured executable paths do that.

If your cluster uses modules or Conda, prefer wrapper scripts for `snakemake_executable`, `sbatch_executable`, or both. For example, a site wrapper can load required modules and then exec the real command.

## 5. Runtime dependencies for DILS/Snakemake jobs

The Slurm job environment must provide the tools required by the DILS Snakemake workflow, including:

- Snakemake;
- Python 3;
- `pypy`, if required by legacy DILS scripts;
- R;
- R packages used by DILS scripts, including at least `tidyverse`, `abcrf`, `randomForest`, `ggplot2`, and `viridisLite`;
- DILS helper scripts/binaries under `DILS/bin`, including `msnsam` if used by the current workflow;
- access to the same filesystem paths as the Streamlit server, especially the repository and `runs_root`.

If the cluster uses modules or Conda, provide wrapper scripts for `snakemake_executable` or configure the Slurm environment so the required tools are loaded before Snakemake starts.

## 6. Streamlit settings

Copy the example settings file:

```bash
cp streamlit/settings.example.yaml streamlit/settings.yaml
```

The app loads defaults from `streamlit/settings.py`, then overrides them with `streamlit/settings.yaml`. The `DILS_RUNS_ROOT` environment variable can override only `runs_root`.

Deployment-relevant settings:

| Setting | Meaning |
| --- | --- |
| `runs_root` | Directory where app-managed run directories are created. Use a shared scratch/project path, not the repository default. |
| `dils_bin_dir` | Path to `DILS/bin`. Relative paths are resolved from the repository root. |
| `snakefile_1pop` | Path to the one-population Snakefile. |
| `snakefile_2pop` | Path to the two-population Snakefile. |
| `sbatch_executable` | Command or wrapper used to submit Slurm jobs. |
| `sacct_executable` | Command or wrapper used to query Slurm accounting. |
| `snakemake_executable` | Command or wrapper executed inside the Slurm job. |
| `default_cpus` | Admin-configured CPU count used for both `sbatch --cpus-per-task` and `snakemake -j`. |
| `default_memory_gb` | Admin-configured Slurm memory request in GB, passed as `--mem=<default_memory_gb>G`. |
| `run_id_prefix` | Prefix for generated run IDs and DILS `timeStamp` values. |
| `local_dev_mode` | Enables local admin/dev pages and path/command previews. Set `false` for deployment. |
| `delete_uploaded_fasta_after_completion` | If `true`, delete the Streamlit-managed uploaded FASTA copy after successful completion and final archive detection. |
| `notifications_enabled` | Enables optional notification scaffolding. Default is `false`. |
| `notification_backend` | Notification backend. The current implementation supports only `file`. |
| `notification_from_email` | Sender address written into generated `.eml` files. |
| `notification_signature` | Signature text used in notification messages. |
| `app_public_url` | Optional public URL included in notification messages. |
| `manual_url` | Optional manual URL included in notification messages. |
| `support_contact_text` | Support/contact text included in notification messages. |

Example deployment settings:

```yaml
runs_root: /shared/project/dils_streamlit_runs
dils_bin_dir: DILS/bin
snakefile_1pop: DILS/bin/Snakefile_1pop
snakefile_2pop: DILS/bin/Snakefile_2pop
sbatch_executable: sbatch
sacct_executable: sacct
snakemake_executable: snakemake
default_cpus: 10
default_memory_gb: 10
run_id_prefix: dils
local_dev_mode: false
delete_uploaded_fasta_after_completion: true
notifications_enabled: false
notification_backend: file
notification_from_email: "dils <dils@univ-lyon1.fr>"
notification_signature: "The DILS team"
app_public_url: ""
manual_url: ""
support_contact_text: "If you encounter any problem or have suggestions for improvement, please contact the DILS team."
```

Users do not choose CPU or RAM in the web interface. Choose `default_cpus` and `default_memory_gb` according to cluster policy and expected DILS workload.

For conservative first deployment, set `delete_uploaded_fasta_after_completion: false`, verify completed runs and archive downloads, then enable cleanup after confirming local data-retention policy.

## 7. Run directory structure

For each submitted analysis, the app creates:

```text
<runs_root>/<run_id>/
  input/
    <uploaded_fasta>
  config/
    <run_id>.yaml
  logs/
    slurm/
      slurm-<run_id>-<job_id>.out
      slurm-<run_id>-<job_id>.err
    snakemake/
  work/
    <run_id>.tar.gz
  results/
  metadata/
    metadata.json
```

Important notes:

- The uploaded FASTA is copied to `input/`.
- The generated YAML points DILS to the copied FASTA.
- Slurm is launched with `cwd=<run_dir>/work`.
- The final archive is expected at `<run_dir>/work/<run_id>.tar.gz`.
- `results/` is currently created by the app but the final archive is detected in `work/`.
- `metadata/metadata.json` is the app’s source of truth for status display and archive download.

## 8. Slurm submission behavior

The app builds an `sbatch` command equivalent to:

```bash
sbatch \
  --parsable \
  --ntasks=1 \
  --cpus-per-task=<default_cpus> \
  --mem=<default_memory_gb>G \
  --job-name=<run_id> \
  --output=<run_dir>/logs/slurm/slurm-<run_id>-%j.out \
  --error=<run_dir>/logs/slurm/slurm-<run_id>-%j.err \
  --wrap='<snakemake command>'
```

The wrapped Snakemake command is equivalent to:

```bash
snakemake \
  --snakefile <Snakefile_1pop_or_2pop> \
  -p \
  -j <default_cpus> \
  --configfile <run_dir>/config/<run_id>.yaml
```

The Streamlit process must be allowed to call `sbatch`. The Slurm job must be able to read the repository, read the copied FASTA and YAML, write inside the run directory, and create the final archive.

## 9. Status refresh and metadata lifecycle

The app refreshes status with `sacct --parsable2 --noheader --jobs <job_id> --format=JobID,State,ExitCode`.

Main statuses:

| Status | Meaning |
| --- | --- |
| `created` | Run files were written but Slurm has not been launched. |
| `submitted` | `sbatch` accepted the job. |
| `running` | Slurm reports a running or pending-like state. |
| `completed` | Slurm reports `COMPLETED`, exit code is `0:0`, and the final archive exists. |
| `completed_missing_archive` | Slurm completed successfully but the expected final archive was not found. |
| `failed` | Slurm reports failure, cancellation, timeout, out-of-memory, node failure, or a non-zero completed exit code. |
| `submission_failed` | Run files exist, but `sbatch` failed before queuing the job. |

The app never marks a run as completed from Slurm state alone. The expected final archive must also exist.

## 10. Uploaded FASTA cleanup

If `delete_uploaded_fasta_after_completion: true`, the app can delete the uploaded FASTA copy after status refresh confirms all of the following:

- refreshed metadata status is `completed`;
- `final_archive_exists` is true;
- `final_archive_path` exists on disk;
- `uploaded_fasta_saved_path` exists on disk;
- `uploaded_fasta_deleted` is not already true;
- the FASTA path is inside the planned run `input/` directory and run directory.

The cleanup deletes only `metadata["uploaded_fasta_saved_path"]`.

It does not delete:

- the config YAML;
- metadata;
- logs;
- final archive;
- work directory;
- any user-provided path outside the app-managed run input directory.

On successful deletion, metadata records:

- `uploaded_fasta_deleted: true`
- `uploaded_fasta_deleted_at`
- `uploaded_fasta_deleted_path`
- `uploaded_fasta_cleanup_error: null`

On deletion failure, status refresh still succeeds and metadata records the cleanup error. Later refreshes may retry if the FASTA still exists and `uploaded_fasta_deleted` is not true.

## 11. Notification scaffolding

Notifications are optional and disabled by default with `notifications_enabled: false`.

The **Submit DILS analysis** page has an explicit **Email address** field. If `notifications_enabled: true`, this field is required and must contain `@`. If `notifications_enabled: false`, the field is optional and can be empty.

The submitted email address is written to:

- the generated YAML as `mail_address`;
- `metadata.json` as `mail_address`.

The current implementation supports only `notification_backend: file`. This backend is meant for local/admin testing before real mail delivery is added. It writes RFC-style `.eml` files under:

```text
<run_dir>/metadata/notifications/
```

No real email is sent in the current implementation. `sendmail` and SMTP are not implemented yet.

The recipient is read from `mail_address` in run metadata. If no recipient is available, notification writing is skipped cleanly and metadata records:

```text
notification_skipped_reason: missing recipient email
```

Future deployment work can add `sendmail` or SMTP backends without changing the DILS/Snakemake workflows.

## 12. Filesystem and permissions

Recommended deployment permissions:

- The Streamlit service account can read the repository.
- The Streamlit service account can create directories and files under `runs_root`.
- Slurm jobs submitted by the Streamlit service account can read and write the same `runs_root`.
- The final archive remains readable by the Streamlit service account after the job completes.
- `sacct` can see jobs submitted by the Streamlit service account.

Avoid using the repository-local `streamlit_runs/` default for production. Use a storage location with explicit quota, cleanup policy, backup policy if needed, and enough capacity for uploaded FASTA files plus DILS outputs.

## 13. Streamlit server configuration

The app entry point is:

```bash
python -m streamlit run streamlit/app.py
```

Cluster deployments typically run this behind an institutional reverse proxy or app gateway.

Recommended operational settings:

- Bind Streamlit to an internal interface unless the service is intentionally public.
- Let the reverse proxy provide TLS.
- Configure upload limits at the Streamlit/proxy level according to expected FASTA sizes.
- Protect the app with institutional authentication if exposed beyond trusted users.
- Set `local_dev_mode: false` for production so global existing-run browsing and dev tools are hidden.

If using `.streamlit/config.toml`, keep site-specific server settings there. The DILS app-specific settings belong in `streamlit/settings.yaml`.

## 14. Streamlit upload limit

`.streamlit/config.toml` controls the maximum upload size accepted by Streamlit. For large FASTA uploads, set `maxUploadSize` high enough for the expected input files:

```toml
[server]
maxUploadSize = 10240
```

The value is in megabytes. Also check any reverse proxy, gateway, or institutional web-service limit, because those may reject large uploads before the request reaches Streamlit.

## 15. What administrators should not modify

- Do not hard-code deployment paths inside Snakefiles.
- Do not edit `DILS/bin/Snakefile_1pop` or `DILS/bin/Snakefile_2pop` just to change run locations.
- Use `streamlit/settings.yaml` for deployment paths and executables.
- Keep the repository code separate from app-managed run data.
- Do not use `local_dev_mode: true` for external users.

## 16. Deployment checklist

Before opening to users:

1. Create and permission `runs_root`.
2. Create `streamlit/settings.yaml`.
3. Set `local_dev_mode: false`.
4. Choose `default_cpus` and `default_memory_gb` values consistent with cluster policy.
5. Confirm `sbatch`, `sacct`, and `snakemake` are available to the Streamlit process.
6. Confirm Slurm jobs can read the repository and write to `runs_root`.
7. Submit a small one-population test run.
8. Submit a small two-population test run.
9. Refresh job status until completion.
10. Confirm final `.tar.gz` archive detection.
11. Confirm archive download through the web interface.
12. If enabled, confirm uploaded FASTA cleanup only after successful completion.
13. Test with `notifications_enabled: false`, leave email empty, and confirm no notification directory is created.
14. Test `notification_backend: file` with a real test email address.
15. Confirm submitted/completed `.eml` files are written once.
16. Confirm repeated refresh does not duplicate notifications.
17. Review storage quotas and retention policy for run directories and final archives.

## 17. Minimal smoke test

Run a short end-to-end deployment test:

1. Start the Streamlit app.
2. Open the app in a browser.
3. Confirm that deployment mode only shows **Submit DILS analysis**, **Results viewer**, and **Help**.
4. Submit a small 1-pop analysis.
5. Confirm a Slurm job ID is returned.
6. Refresh job status until it becomes `completed`.
7. Confirm the final `.tar.gz` archive exists.
8. Download the archive.
9. Open the archive in **Results viewer**.
10. Repeat once with a small 2-pop analysis.
11. If `notifications_enabled: false`, confirm an empty email is allowed and no `.eml` is written.
12. If `notifications_enabled: true`, submit with a real test email and confirm submitted/completed `.eml` files are written once.
13. If `delete_uploaded_fasta_after_completion: true`, confirm the uploaded FASTA is removed only after successful completion.

## 18. Troubleshooting

| Symptom | Check |
| --- | --- |
| App cannot start | Verify Python environment has Streamlit, pandas, Plotly, and PyYAML. |
| Settings do not apply | Confirm `streamlit/settings.yaml` exists and is valid YAML. |
| Runs are written to the wrong location | Check `runs_root` and `DILS_RUNS_ROOT`. The environment variable overrides the YAML setting. |
| Submission fails | Check `sbatch_executable`, Slurm account/partition policy, and whether the Streamlit service account may submit jobs. |
| Status refresh fails | Check `sacct_executable`, Slurm accounting availability, and job visibility for the service account. |
| Job completes but app shows `completed_missing_archive` | Check Snakemake output and confirm whether `<run_dir>/work/<run_id>.tar.gz` was produced. |
| Download is unavailable | Confirm metadata has `status: completed`, `final_archive_exists: true`, and a valid `final_archive_path`. |
| FASTA was not deleted | Confirm cleanup setting is true, status is `completed`, archive exists, FASTA exists, and metadata does not already have `uploaded_fasta_deleted: true`. |

