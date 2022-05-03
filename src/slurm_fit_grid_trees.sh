#!/bin/bash

# Example of running R script in a batch mode

#SBATCH -J fit_grid_trees
#SBATCH -a 1-18                          # can fit my 18 trees in parallel
#SBATCH -o ../log/out_grid_tree_%a
#SBATCH -e ../log/err_grid_tree_%a
#SBATCH -D ./
##SBATCH --mail-type=ALL
##SBATCH --mail-user=your_email@stanford.edu

# --- resource specification (which resources for how long) ---
#SBATCH -t 24:00:00
#SBATCH --partition=compute
#SBATCH --mem=6000 # memory in MB required by the job
#SBATCH --cpus-per-task=4
##SBATCH --ntasks=10
##SBATCH --exclusive

module load singularity
export SINGULARITY_BIND="/run,/ptmp,/scratch,/tmp,/opt/ohpc,${HOME}"

# Run R script
srun singularity exec /ptmp/containers/tobi-datascience-r_latest-2021-12-15-e1eefd13aa83.sif Rscript fit_grid_tree.R $SLURM_ARRAY_TASK_ID

