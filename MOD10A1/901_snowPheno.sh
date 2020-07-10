#!/bin/bash
#SBATCH -D PATH/TO/DIR/MOD10A1
#SBATCH -J pheno.01
#SBATCH -o PATH/TO/DIR/MOD10A1/phenology_out-%j.txt
#SBATCH -e PATH/TO/DIR/MOD10A1/phenology_error-%j.txt
#SBATCH -t 1-23:59
#SBATCH -N 1
#SBATCH -c 24
#SBATCH --mem=24000

$( while [ ! -f .done ]; do ps acux | grep R >> pheno01_log.txt; sleep 60; done) &

module load R

srun Rscript 003_Snowmelt_Phenology.R '2001' '24'