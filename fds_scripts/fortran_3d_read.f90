program read_mesh_files
use netcdf
implicit none
INTEGER :: NM, II,JJ,KK
INTEGER :: IOR,IZERO,I,J,K,N,I1B,I2B,IW,NN,NF,IP,IQ
INTEGER :: NTSL
INTEGER:: IBAR=15, JBAR=15, KBAR=40, IBP1=16, JBP1=16, KBP1=41
REAL, DIMENSION (30) :: XPLT
REAL, DIMENSION (30) :: YPLT
REAL, DIMENSION (20) :: ZPLT
REAL, DIMENSION (16,16,41,5) :: QQ
REAL :: ZERO

 INTEGER:: status, dimid1, dimid2,dimid3,dimid4, varid1,varid2,varid3,varid4, varid5, ncid


   PRINT*, 'started'




! Initialize PLOT3D grid file (CHID.xyz)



   OPEN(50,FILE='Level_Set_2m_22_50p08.q',ACTION='READ',FORM='UNFORMATTED')

   READ(50), II,JJ,KK
   PRINT*,II,JJ,KK
   PRINT*, 'reading'
   
   READ(50), ZERO,ZERO,ZERO,ZERO
 PRINT*,ZERO
   READ(50) ((((QQ(I,J,K,IQ),I=0,IBAR),J=0,JBAR),K=0,KBAR),IQ=1,5)
   
   
   
   DO IQ=1,5
      PRINT*,'QQ ', IQ,  QQ(10,10,20,IQ)   
   ENDDO

 
 
       status= nf90_create('test_3d.nc', NF90_CLOBBER, ncid)
! Define dimensions
       status = nf90_def_dim(ncid, "x", ibp1, dimid1)
       status = nf90_def_dim(ncid, "y", jbp1, dimid2)
       status = nf90_def_dim(ncid, "z", kbp1, dimid3)
       status = nf90_def_dim(ncid, "var", 5, dimid4)

! Define variables
       status = nf90_def_var(ncid, "qq", NF90_FLOAT, (/dimid1,dimid2,dimid3,dimid4/), varid1)

! End definitions
       status = nf90_enddef(ncid)

! Write data to the variable
! Assuming temperature_data contains your actual temperature values
       status = nf90_put_var(ncid, varid1, QQ)

! Close the NetCDF file
       status = nf90_close(ncid)
 
 
 
 
 
   
   CLOSE(50)
   
   PRINT*, 'closed'


   


end program 


