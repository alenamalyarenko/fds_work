#!/bin/bash -e
#SBATCH --job-name=fds_post       # job name (shows up in the queue)
#SBATCH --account=scion03617    # Project Account
#SBATCH --time=01:30:00       # Walltime (HH:MM:SS)

##SBATCH --qos==debug
#SBATCH --mem=100GB             # memory (in MB)
#SBATCH --ntasks=1           # number of tasks (e.g. MPI)
#SBATCH --cpus-per-task=1     # number of cores per task (e.g. OpenMP)
#SBATCH --output=%x-%j.out    # %x and %j are replaced by job name and ID
#SBATCH --error=%x-%j.err

##SBATCH --mail-user=alena.malyarenko@canterbury.ac.nz
##SBATCH --mail-type=ALL

export TZ="Pacific/Auckland"

module load MATLAB/2022a
#export MLM_LICENSE_FILE=/opt/nesi/share/MATLAB/Licenses/vuw03158.lic

# Run the MATLAB script MATLAB_job.m 
matlab -nodisplay < maui_run_saver.m
