#!/bin/sh
#
#This is a submission script for Kyle's SMEP Replication
#
#These commands set up the Grid Environment for your job:
#PBS -N KyleSmepRep
#PBS -l nodes=12:ppn=5
#PBS -l walltime=200:00:00
#PBS -l mem=100gb
#PBS -M kylelang@ku.edu
#PBS -m bea

cd $PBS_O_WORKDIR

### This RUNS, and because I give it a machine list, it uses them. 
orterun --hostfile $PBS_NODEFILE -n 1 R --no-save --vanilla -f kyle-smepRep-011912.R



##orterun -n 10 /tools/lib64/R/site-library/snow/RMPISNOW < snow-hello.R

