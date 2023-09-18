program read_grid
        use netcdf
        integer :: i,j,k,ii,jj,kk
        integer :: ibar, jbar, kbar
        real, dimension (30) :: xplt
        real, dimension (30) :: yplt
        real, dimension (20) :: zplt
        real, dimension (30,30,20) :: iblk
        
        
        real, dimension (30,30) :: var         
        integer :: status, ncid, varid, dimid

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

        do i=0,30
         do j=0,30
          var(i,j)=i
         enddo
        enddo

       status= nf90_create('test.nc', NF90_CLOBBER, ncid)
! Define dimensions
       status = nf90_def_dim(ncid, "x", ibar, dimid)
       status = nf90_def_dim(ncid, "y", jbar, dimid)

! Define variables
       status = nf90_def_var(ncid, "test", NF90_FLOAT, (/dimid,dimid/), varid)

! End definitions
       status = nf90_enddef(ncid)

! Write data to the variable
! Assuming temperature_data contains your actual temperature values
       status = nf90_put_var(ncid, varid, var)

! Close the NetCDF file
       status = nf90_close(ncid)


       close(50)


end program
