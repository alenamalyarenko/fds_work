#Postprocessing standalone:
ulimit -s unlimited
cd ../fds_scripts/
ifx -mcmodel=medium  -o partial_post.exe  glue_output_partial2.f90 -I/usr/local/netcdf-ifort/4.6.1/include -I/usr/local/netcdf-ifort/4.6.1/include -L/usr/local/netcdf-ifort/4.6.1/lib -lnetcdff -shared-intel -L/usr/lib -I/usr/include -lnetcdf -lm

cd /mnt/data2/data2/FDS/fds_work/test_runs_gmd/std_v_1_5/
cp /mnt/data2/data2/FDS/fds_work/fds_scripts/partial_post.exe .
./partial_post.exe STD_V_1_5 0 3600 48 40 240 14 8

