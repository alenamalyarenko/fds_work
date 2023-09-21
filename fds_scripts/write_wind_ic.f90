program write_ic
        use netcdf
        integer :: i,j,k,ii,jj,kk
        integer :: ibar, jbar, kbar


        integer :: status, ncid, varid1,varid2
        integer :: dimid1, dimid2
         
        real :: U0, V0
        
        U0=10.0
        V0=10.0


       status= nf90_create('wind_n.nc', NF90_CLOBBER, ncid)
! Define dimensions
       status = nf90_def_dim(ncid, "x", 1, dimid1)


! Define variables

       status = nf90_def_var(ncid, "U0", NF90_FLOAT, (/dimid1/), varid1)
       status = nf90_def_var(ncid, "V0", NF90_FLOAT, (/dimid1/), varid2)


! End definitions
       status = nf90_enddef(ncid)

! Write data to the variable
       status = nf90_put_var(ncid, varid1, U0)
       status = nf90_put_var(ncid, varid2, V0)



! Close the NetCDF file
       status = nf90_close(ncid)


end program
