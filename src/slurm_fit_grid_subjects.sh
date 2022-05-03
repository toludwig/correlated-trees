#!/bin/bash

#SBATCH -J grid_subjects
#SBATCH -a 1-107                         # can fit my 107 subjects in parallel
#SBATCH -o ../log/out_grid_subject_%a
#SBATCH -e ../log/err_grid_subject_%a
#SBATCH -D ./
##SBATCH --mail-type=ALL
##SBATCH --mail-user=your_email@stanford.edu

# --- resource specification (which resources for how long) ---
#SBATCH -t 24:00:00
#SBATCH --partition=compute
#SBATCH --mem=6000 # memory in MB required by the job
#SBATCH --cpus-per-task=4
##SBATCH --ntasks=1
##SBATCH --exclusive

module load singularity
export SINGULARITY_BIND="/run,/ptmp,/scratch,/tmp,/opt/ohpc,${HOME}"

# Run R script
srun singularity exec /ptmp/containers/tobi-datascience-r_latest-2021-12-15-e1eefd13aa83.sif Rscript fit_grid_subject.R $SLURM_ARRAY_TASK_ID

