#!/bin/bash -e

#SBATCH --time           00:10:00       #Walltime
#SBATCH --job-name       fds
###SBATCH --qos            debug
#SBATCH --account        vuw03158
#SBATCH --ntasks         1              #One task per mesh, NO MORE
#SBATCH --hint           nomultithread  #Hyperthreading decreases efficiency.
###SBATCH --mem=512MB          # Memory in MB
#SBATCH --output=%x-%j.out    # %x and %j are replaced by job name and ID
#SBATCH --error=%x-%j.err

##SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=alena.malyarenko@canterbury.ac.nz

input="/nesi/project/vuw03158/fds/fds/exp/grass/Input/Level_Set_2m_multi_mesh.fds"

srun read
