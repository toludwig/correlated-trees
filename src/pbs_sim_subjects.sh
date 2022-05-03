#!/bin/bash

#PBS -N sim_subjects
#PBS -o ~/generative-bandits/log/out_model_sim
#PBS -e ~/generative-bandits/log/err_model_sim

# --- resource specification (which resources for how long) ---
#PBS -l walltime=24:00:00
#PBS -l mem=1000 # memory in MB required by the job

cd $PBS_O_WORKDIR

# Run R script
Rscript sim_subjects.R 2>&1 ../log/sim_subjects.out

