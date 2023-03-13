Correlated Trees
==================

Code for [Ludwig, Wu & Schulz, 2022](https://psyarxiv.com/72wda) 
where we studied __Exploration, Generalization, and Planning in Correlated Trees__.

## Correlated Trees
All modelling code can be found in `src`.
Correlated trees are constructed with rewards that are correlated between parent and child and between sibling nodes, defined by a diffusion kernel (see `src/tree.R`).
`src/sim_tree.R` simulates the n-step task, letting the models "walk down" a tree while collecting rewards.
Agents solving this task are defined in `src/tree_models.R`.

## Experiment + Analysis
`study` contains the website of our experiment ("Roulecity"), as it was run on Prolific.
`data_anonym` contains anonymised data from this study.

**For reproducing the results:**
1. Install all required R packages (see `./dependencies.txt`).
2. To rerun the model fitting, use the `src/fit_*.R` scripts.
When working on a cluster, you can use the `src/{slurm, pbs}_fit_*.sh` scripts, depending on your cluster's scheduling system.
They will produce .csv files with the best parameter fits in the `fit/` folder and log files in `log/` (if present).
The resulting .csv files only contain one subject / tree at a time.
To join them, run the shell script `fit/join_csvs.sh PREFIX` where PREFIX is the filename without the number and `.csv`.
3. To reproduce the figures, run `src/fig_behaviour.R` and `src/fig_model.R`, respectively.

You can also play around with the data in the markdown notebooks, 
`src/behaviour.R` and `src/model_fitting.R`, of which the latter includes Supplementary analyses.

## Naming convention
Note that we used b ("breadth"), d ("depth") and c ("correlation") for the three dimensions in the paper.
In the code these variables correspond to B ("breadth"), H (height") and A (alluding to the corresponding alpha parameter).
