#!/bin/bash
#SBATCH -D PATH/TO/DIR/MOD13Q1
#SBATCH -J pheno.1
#SBATCH -o PATH/TO/DIR/MOD13Q1/phenology_out-%j.txt
#SBATCH -e PATH/TO/DIR/MOD13Q1/phenology_error-%j.txt
#SBATCH -t 1-23:59
#SBATCH -N 1
#SBATCH -c 24
#SBATCH --mem=32000

$( while [ ! -f .done ]; do ps acux | grep R >> pheno1_log.txt; sleep 60; done) &

module load R

srun Rscript 101_phenomap.R '0_0'
