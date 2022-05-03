#!/bin/bash

#PBS -N fit_subjects
#PBS -t 1-107         # which subjects to fit
#PBS -o ~/generative-bandits/log/out_subject_\$TASK_ID
#PBS -e ~/generative-bandits/log/err_subject_\$TASK_ID

# --- resource specification (which resources for how long) ---
#PBS -l walltime=24:00:00
#PBS -l mem=1000 # memory in MB required by the job

cd $PBS_O_WORKDIR

RECOV_FLAG="FALSE" # FALSE for fitting actual data, TRUE for recovering from simulated data
LESION_MOD="4"

# Run R script
Rscript fit_subject.R $PBS_ARRAYID $RECOV_FLAG $LESION_MOD > ../log/gamma1_subject_$PBS_ARRAYID

