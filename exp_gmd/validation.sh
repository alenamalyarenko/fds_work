ulimit -s unlimited

# Experiment 1 - Idealised, no fire
# Standalone runs
#WoW, SEM
./makerun_uc.sh -m standalone -n 18 -p no STD_V_1_5
#WoW, no SEM
./makerun_uc.sh -m standalone -n 18 -p no STD_V_1_8

#Periodic, 0.24
./makerun_uc.sh -m standalone -n 9 -p no STD_V_1_6
#Periodic, 0.36
./makerun_uc.sh -m standalone -n 9 -p no STD_V_1_7


#Postprocessing standalone:
cd ../fds_scripts/
ifx -mcmodel=medium  -o partial_post.exe  glue_output_partial2.f90 -I/usr/local/netcdf-ifort/4.6.1/include -I/usr/local/netcdf-ifort/4.6.1/include -L/usr/local/netcdf-ifort/4.6.1/lib -lnetcdff -shared-intel -L/usr/lib -I/usr/include -lnetcdf -lm

cd /mnt/data2/data2/FDS/fds_work/test_runs_gmd/std_v_1_5/
cp /mnt/data2/data2/FDS/fds_work/fds_scripts/partial_post.exe .
./partial_post.exe STD_V_1_5 1800 2300 60 60 120 5 2

cd /mnt/data2/data2/FDS/fds_work/test_runs_gmd/std_v_1_8/
cp /mnt/data2/data2/FDS/fds_work/fds_scripts/partial_post.exe .
./partial_post.exe STD_V_1_8 1800 2300 60 60 120 5 2

cd /mnt/data2/data2/FDS/fds_work/test_runs_gmd/std_v_1_6/
cp /mnt/data2/data2/FDS/fds_work/fds_scripts/partial_post.exe .
./partial_post.exe STD_V_1_6 1800 2300 60 60 120 2 2

cd /mnt/data2/data2/FDS/fds_work/test_runs_gmd/std_v_1_7/
cp /mnt/data2/data2/FDS/fds_work/fds_scripts/partial_post.exe .
./partial_post.exe STD_V_1_7 1800 2300 60 60 120 2 2




#Coupled runs to standalone:
#./makerun_uc.sh CPL1_21 coupled ic_coup_c5.nc bc_coup_c5.nc
#./makerun_uc.sh CPL1_22 coupled ic_coup_c6.nc bc_coup_c6.nc

