#!/usr/bin/bash
## launch DILS for 1 populations
## the provided argument is for --configfile, expecting the yaml file
snakemake --snakefile /shared/mfs/data/home/croux/softwares/DILS/bin/Snakefile_1pop -p -j 200 --configfile ${1} --cluster-config /shared/mfs/data/home/croux/softwares/DILS/bin/cluster_1pop.json --cluster "sbatch --nodes={cluster.node} --ntasks={cluster.n} --cpus-per-task={cluster.cpusPerTask} --time={cluster.time}"
