#!/bin/sh
#
#This is an example script example.sh
#
#These commands set up the Grid Environment for your job:
#PBS -N SeedsForTasks 
#PBS -l nodes=12:ppn=2:noib
#PBS -l walltime=00:50:00
#PBS -M pauljohn@ku.edu
#PBS -m bea

cd $PBS_O_WORKDIR

### This RUNS, and because I give it a machine list, it uses them. 
orterun --hostfile $PBS_NODEFILE -n 1 R --no-save --vanilla -f controlledSeeds.R 

