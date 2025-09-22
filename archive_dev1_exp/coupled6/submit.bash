#!/bin/bash -e

#SBATCH --time           00:15:00       #Walltime
#SBATCH --job-name       fds
#SBATCH --qos            debug
#SBATCH --account        scion03617
#SBATCH --ntasks         4              #One task per mesh, NO MORE
#SBATCH --hint           nomultithread  #Hyperthreading decreases efficiency.
###SBATCH --mem=512MB          # Memory in MB
#SBATCH --output=%x-%j.out    # %x and %j are replaced by job name and ID
#SBATCH --error=%x-%j.err

##SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=alena.malyarenko@canterbury.ac.nz

input="/home/ama677/fds/exp/coupled6/coupled6.fds"

srun fds_impi_intel_linux_nc ${input}
