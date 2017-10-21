#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --nodes=1

R CMD BATCH --no-save kmeans_parallel.R job.out
