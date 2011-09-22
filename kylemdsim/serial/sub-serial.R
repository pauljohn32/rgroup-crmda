#!/bin/sh
#
#This is an example script example.sh
#
#These commands set up the Grid Environment for your job:
#PBS -N Rsimple 
#PBS -l nodes=1:ppn=2
#PBS -l walltime=50:10:00
#PBS -M pauljohn@ku.edu
#PBS -m bea
#PBS -q default

cd $PBS_O_WORKDIR

R --vanilla --no-save -f kyle-serial.R 

