#!/usr/bin/bash
## launch DILS for 2 populations
## the provided argument is for --configfile, expecting the yaml file
module load pypy/2.7-5.10.0
module load snakemake
snakemake --snakefile /shared/mfs/data/home/croux/softwares/DILS/bin/Snakefile_2pop -p -j 200 --configfile ${1} --cluster-config /shared/mfs/data/home/croux/softwares/DILS/bin/cluster_2pop.json --cluster "sbatch --nodes={cluster.node} --ntasks={cluster.n} --cpus-per-task={cluster.cpusPerTask} --time={cluster.time}"
