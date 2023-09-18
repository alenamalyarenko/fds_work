program read_mesh_files
!use netcdf
!implicit none
INTEGER :: NM, II,JJ,KK
INTEGER :: IOR,IZERO,I,J,K,N,I1B,I2B,IW,NN,NF,IP
INTEGER :: NTSL
INTEGER :: ncid, status, varid, dimid
INTEGER:: IBAR=30, JBAR=30, KBAR=20, IBP1=31, JBP1=31, KBP1=21
REAL, DIMENSION (30) :: XPLT
REAL, DIMENSION (30) :: YPLT
REAL, DIMENSION (20) :: ZPLT
REAL, DIMENSION (30,30,20) :: IBLK
REAL, DIMENSION (30,30) :: TEST
CHARACTER(LEN=50) :: outfile 


! mesh number
NM=1


!IBAR=30
!JBAR=30
!KBAR=20
!IBP1=31
!JBP1=31
!KBP1=21

! Compute grid coords in single precision for output


!DO I=0,IBAR
!   XPLT(I) = REAL(X(I),FB)
!ENDDO
!DO J=0,JBAR
!  YPLT(J) = REAL(Y(J),FB)
!ENDDO
!DO K=0,KBAR
!   ZPLT(K) = REAL(Z(K),FB)
!ENDDO



! Initialize PLOT3D grid file (CHID.xyz)



   OPEN(50,FILE='Level_Set_2m_1.xyz',ACTION='READ',FORM='UNFORMATTED')

   READ(50), II,JJ,KK
   PRINT*,II,JJ,KK

   READ(50) (((XPLT(I),I=0,IBAR),J=0,JBAR),K=0,KBAR), &
            (((YPLT(J),I=0,IBAR),J=0,JBAR),K=0,KBAR), &
            (((ZPLT(K),I=0,IBAR),J=0,JBAR),K=0,KBAR), &
            (((IBLK(I,J,K),I=0,IBAR),J=0,JBAR),K=0,KBAR)  
   
   CLOSE(50)
   


   
   DO I=0,IBAR
      PRINT*,'X ', I,  XPLT(I)
   ENDDO
   DO J=0,JBAR
      PRINT*,'Y ',J,  YPLT(J)
   ENDDO
   DO K=0,KBAR
      PRINT*,'Z ',K,  ZPLT(K)
   ENDDO

!  DO I=0,IBAR
!    DO J=0,JBAR
!     TEST(I,J)=I;
!    ENDDO
!   ENDDO


!   DO I=0,IBAR
!    DO J=0,JBAR
!     DO K=0,KBAR
!      PRINT*, I,J,K, IBLK(I,J,K)
!     ENDDO
!    ENDDO
!   ENDDO   


!outfile = 'ex1.nc'

! status = nf90_create(outfile, NF90_CLOBBER, ncid)

! Define dimensions
!  status = nf90_def_dim(ncid, "x", nx, dimid)
!  status = nf90_def_dim(ncid, "y", ny, dimid)

! Define variables
!  status = nf90_def_var(ncid, "temperature", NF90_FLOAT, (/dimid,dimid/), varid)

! End definitions
! status = nf90_enddef(ncid)

! Write data to the variable
! Assuming temperature_data contains your actual temperature values
! status = nf90_put_var(ncid, varid, temperature_data)

! Close the NetCDF file
! status = nf90_close(ncid)

end program 


