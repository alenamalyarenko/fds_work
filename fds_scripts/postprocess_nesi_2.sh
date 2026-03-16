#!/bin/bash -e

#SBATCH --time           02:00:00       #Walltime
#SBATCH --job-name       fds
##SBATCH --qos           debug
#SBATCH --account        scion03617
#SBATCH --ntasks         1              #One task per mesh, NO MORE
#SBATCH --hint           nomultithread  #Hyperthreading decreases efficiency.
#SBATCH --mem=600GB          # Memory in MB
#SBATCH --output=%x-%j.out    # %x and %j are replaced by job name and ID
#SBATCH --error=%x-%j.err
#SBATCH --partition=hugemem

##SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=alena.malyarenko@canterbury.ac.nz



module load intel/2022a
module load netCDF-Fortran/4.6.0-iimpi-2022a

ulimit -s unlimited

ifx -mcmodel=medium  -o partial_post.exe  glue_output_partial2.f90  -L/$EBROOTNETCDFMINFORTRAN/lib/ -I/$EBROOTNETCDFMINFORTRAN/include -lnetcdf -lnetcdff 

cd /nesi/nobackup/scion03617/ama677/std_v_1_52/
cp /home/ama677/FDS/fds_work/fds_scripts/partial_post.exe .


./partial_post.exe STD_V_1_5 1800 2000 48 40 240 14 8
 

