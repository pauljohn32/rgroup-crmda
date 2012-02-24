#!/bin/sh
#
#This is an example script example.sh
#
#These commands set up the Grid Environment for your job:
#PBS -N BifactorSim 
#PBS -l nodes=5:ppn=2:noib,mem=40gb,vmem=40gb,pmem=4gb,pvmem=4gb
#PBS -l walltime=50:00:00
#PBS -M pauljohn@ku.edu
#PBS -m bea

cd $PBS_O_WORKDIR

### This RUNS, and because I give it a machine list, it uses them. 
orterun --hostfile $PBS_NODEFILE -n 1 R --no-save --vanilla -f bfasim-r2openbugs.R

