program read_grid
        use netcdf
        integer :: i,j,k,ii,jj,kk
        integer :: ibar, jbar, kbar
        real, dimension (30) :: xplt
        real, dimension (30) :: yplt
        real, dimension (20) :: zplt
        real, dimension (30,30,20) :: iblk, xgrid,ygrid,zgrid
        
        
        real, dimension (30,30) :: var         
        integer :: status, ncid, varid1,varid2,varid3,varid4
        integer :: dimid1, dimid2,dimid3

        ibar=30
        jbar=30
        kbar=20

        open (50, file='Level_Set_2m_1.xyz',action='read',form='unformatted')
        read(50), ii,jj,kk
        print*, ii,jj,kk
        
        read(50) (((xplt(i),i=0,ibar),j=0,jbar),k=0,kbar), (((yplt(j),i=0,ibar),j=0,jbar),k=0,kbar), &
                 (((zplt(k),i=0,ibar),j=0,jbar),k=0,kbar)
!                 (((iblk(i,j,k),i=0,ibar),j=0,jbar),k=0,kbar)

        do i=0,ibar
         print*, 'x',i,xplt(i)  
        enddo

        do j=0,jbar
         print*,'y',j,yplt(j)
        enddo

        do k=0,kbar
         print*,'z',k,zplt(k)
        enddo


        print*,'about to close'

        do i=0,ibar
         do j=0,jbar
          do k=0,kbar
           xgrid(i,j,k)=xplt(i)
           ygrid(i,j,k)=yplt(j)
           zgrid(i,j,k)=zplt(k)
          enddo
         enddo
        enddo

       status= nf90_create('grid_01.nc', NF90_CLOBBER, ncid)
! Define dimensions
       status = nf90_def_dim(ncid, "x", ibar+1, dimid1)
       status = nf90_def_dim(ncid, "y", jbar+1, dimid2)
       status = nf90_def_dim(ncid, "z", kbar+1, dimid3)

! Define variables
       status = nf90_def_var(ncid, "iblk", NF90_FLOAT, (/dimid1,dimid2,dimid3/), varid1)
       status = nf90_def_var(ncid, "x_m", NF90_FLOAT, (/dimid1,dimid2,dimid3/), varid2)
       status = nf90_def_var(ncid, "y_m", NF90_FLOAT, (/dimid1,dimid2,dimid3/), varid3)
       status = nf90_def_var(ncid, "z_m", NF90_FLOAT, (/dimid1,dimid2,dimid3/), varid4)


! End definitions
       status = nf90_enddef(ncid)

! Write data to the variable
! Assuming temperature_data contains your actual temperature values
       status = nf90_put_var(ncid, varid1, iblk)
       status = nf90_put_var(ncid, varid2, xgrid)
       status = nf90_put_var(ncid, varid3, ygrid)
       status = nf90_put_var(ncid, varid4, zgrid)


! Close the NetCDF file
       status = nf90_close(ncid)


       close(50)


end program
