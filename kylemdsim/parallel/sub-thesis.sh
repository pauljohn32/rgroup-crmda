#!/bin/sh
#
#This is a submission script for Kyle's Thesis project
#
#These commands set up the Grid Environment for your job:
#PBS -N KyleTestRun
#PBS -l nodes=3:ppn=1
#PBS -l walltime=50:00:00
#PBS -M kylelang@ku.edu
#PBS -m bea

cd $PBS_O_WORKDIR

### This RUNS, and because I give it a machine list, it uses them. 
orterun --hostfile $PBS_NODEFILE -n 1 R --no-save --vanilla -f kyle-snowFT-111011-noNobs.R



##orterun -n 10 /tools/lib64/R/site-library/snow/RMPISNOW < snow-hello.R

