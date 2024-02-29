program read_mesh_files
implicit none
INTEGER :: NM, II,JJ,KK
INTEGER :: IOR,IZERO,I,J,K,N,I1B,I2B,IW,NN,NF,IP
INTEGER :: NTSL
INTEGER:: IBAR=30, JBAR=30, KBAR=20, IBP1=31, JBP1=31, KBP1=21
REAL, DIMENSION (30) :: XPLT
REAL, DIMENSION (30) :: YPLT
REAL, DIMENSION (20) :: ZPLT
REAL, DIMENSION (30,30,20) :: IBLK



   PRINT*, 'started'




! Initialize PLOT3D grid file (CHID.xyz)



   OPEN(50,FILE='Level_Set_2m_1.xyz',ACTION='READ',FORM='UNFORMATTED')

   READ(50), II,JJ,KK
   PRINT*,II,JJ,KK
   PRINT*, 'reading'

   READ(50) (((XPLT(I),I=0,IBAR),J=0,JBAR),K=0,KBAR) , &
            (((YPLT(J),I=0,IBAR),J=0,JBAR),K=0,KBAR) , &
            (((ZPLT(K),I=0,IBAR),J=0,JBAR),K=0,KBAR) , &
            (((IBLK(I,J,K),I=0,IBAR),J=0,JBAR),K=0,KBAR)  
   
   CLOSE(50)
   
   PRINT*, 'closed'


   
   DO I=0,IBAR
      PRINT*,'X ', I,  XPLT(I)
   ENDDO


end program 


