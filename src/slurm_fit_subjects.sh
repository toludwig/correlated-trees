#!/bin/bash

#SBATCH -J fit_subjects
#SBATCH -a 25,29,31,33,44,47,48,52,55,57,60,62 #1-107 #%50         # which subjects to fit
#SBATCH -o ../log/out_subject_%a
#SBATCH -e ../log/err_subject_%a
#SBATCH -D ./
##SBATCH --mail-type=ALL
##SBATCH --mail-user=your_email@stanford.edu

# --- resource specification (which resources for how long) ---
#SBATCH -t 24:00:00
#SBATCH --partition=compute
#SBATCH --mem=1000 # memory in MB required by the job
#SBATCH --cpus-per-task=1
##SBATCH --ntasks=1
##SBATCH --exclusive

module load singularity
export SINGULARITY_BIND="/run,/ptmp,/scratch,/tmp,/opt/ohpc,${HOME}"

RECOV_FLAG="FALSE" # FALSE for fitting actual data, TRUE for recovering from simulated data

LESION_MOD="5"

# Run R script
srun singularity exec /ptmp/containers/tobi-datascience-r_latest-2021-12-15-e1eefd13aa83.sif Rscript fit_subject.R $SLURM_ARRAY_TASK_ID $RECOV_FLAG $LESION_MOD

