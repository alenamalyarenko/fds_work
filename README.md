Installation guide: 

1) Netcdf-fortran library
   https://github.com/Unidata/netcdf-fortran
   https://docs.unidata.ucar.edu/netcdf-c/current/building_netcdf_fortran.html
      
2) FDS fork from Feb 2024
    git clone https://github.com/alenamalyarenko/fds

3) FDS work repo:
   git clone https://github.com/alenamalyarenko/fds_work

   You should have the following:
   /{Work folder}/fds
   /{Work folder}/fds_work

   The compilation is happening in fds_work. 

5) Local build on Ubuntu:
  cd /build_uc
  Modify makefile, Line 21: NETCDF_LIB=
  Note: in makefile, only one option is left and works with netcdf: impi_inter_linux_uc
  ./nc_compile

How compilation file is structured: 
* Clean old build
* take ../fds/Source
* Overwrite with code files from fds_wok/source_mods_uc
* Note keys.h - includes compilation flags
