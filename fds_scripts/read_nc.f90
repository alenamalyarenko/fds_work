program read_grid
        use netcdf
        integer :: i,j,k,ii,jj,kk
        integer :: ibar, jbar, kbar
        real, dimension (30) :: xplt
        real, dimension (30) :: yplt
        real, dimension (20) :: zplt
        real, dimension (30,30,20) :: iblk
        
        
        real, dimension (30,30) :: var         
        integer :: status, ncid

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

!        open(60,file='read.out',action='write',form='unformatted')
!        do i=0,ibar
!         do j=0,jbar
!          do k=0,kbar
!           write(60,*)i,j,k,iblk(i,j,k)
!          enddo
!         enddo 
!        enddo 
!        close(60)

        close(50)


end program
