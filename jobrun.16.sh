#!/bin/tcsh

#BSUB -P STDD0002
#BSUB -J Shortwave
#BSUB -o wave.16.%J.out
#BSUB -n 16
#BSUB -W 0:05
#BSUB -q premium
#BSUB -R "span[ptile=16]"
cd /glade/p/work/raghuraj/optimizations/sw/mpi_rad_optimized
mpirun.lsf ./kernel.exe
