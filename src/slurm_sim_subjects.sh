#!/bin/bash

#SBATCH -J model_sim
#SBATCH -o ../log/out_model_sim
#SBATCH -e ../log/err_model_sim
#SBATCH -D ./
##SBATCH --mail-type=ALL
##SBATCH --mail-user=your_email@stanford.edu

# --- resource specification (which resources for how long) ---
#SBATCH -t 24:00:00
#SBATCH --partition=compute
#SBATCH --mem=1000 # memory in MB required by the job
#SBATCH --cpus-per-task=4
##SBATCH --ntasks=1
##SBATCH --exclusive

module load singularity
export SINGULARITY_BIND="/run,/ptmp,/scratch,/tmp,/opt/ohpc,${HOME}"

# Run R script
srun singularity exec /ptmp/containers/tobi-datascience-r_latest-2021-12-15-e1eefd13aa83.sif Rscript sim_subjects.R

