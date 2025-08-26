#!/bin/bash
#SBATCH --output=%x_%j.txt
#SBATCH --error=%x_%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mail-user=whayes2@binghamton.edu
#SBATCH --mail-type=ALL

module load gnu14/14.2.0
module load R

Rscript param_recovery_Exp1.R $1
