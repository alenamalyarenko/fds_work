!take data from 7_3 and make BC for 7_5
! time-evolving V0, W0, W1
program make_wind_bc
use netcdf
implicit none


!%%%%%% change for each run:
INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=60, NT=601
INTEGER,PARAMETER:: I_UPPER=3, J_UPPER=3
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



REAL,ALLOCATABLE, DIMENSION(:,:,:):: t_in1, t_in2, t_in3
REAL,ALLOCATABLE, DIMENSION(:,:,:):: u_in1, v_in1, w_in1
REAL,ALLOCATABLE, DIMENSION(:,:,:):: u_in2, v_in2, w_in2
REAL,ALLOCATABLE, DIMENSION(:,:,:):: u_in3, v_in3, w_in3
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_all, u_all, v_all,w_all

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t
character(len=25)::filename,filename1,filename2,filename3
logical::res,SKIPIT
INTEGER::SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C,NM_c
character(len=25)::run_name

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz  
integer:: varid_uout,varid_vout,varid_wout,varid_tout

!	other
INTEGER :: NM,IOR,IZERO,I,J,K,N,T, meshes
INTEGER:: IBP1, JBP1, KBP1, IBP2, JBP2,KBP2,IBARout, JBARout, KBARout, NTout, JJ2,II2

INTEGER,ALLOCATABLE,DIMENSION(:)::GI1, GI2,GJ1,GJ2,GK1, GK2, MESH_I, MESH_J

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!to change for other runs:
run_name='Coupled7_3'
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!size varibales
 !added one more for both 0 and ibp1 cells
 IBP1=IBAR+1
 JBP1=JBAR+1
 KBP1=KBAR+1

 IBP2=IBAR+2 
 JBP2=JBAR+2 
 KBP2=KBAR+2 
             
 IBARout=IBAR*I_UPPER
 JBARout=JBAR*J_UPPER             
 KBARout=KBAR
 NTout=NT

 meshes=I_UPPER*J_UPPER

 ALLOCATE( T_IN1(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( U_IN1(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( V_IN1(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( W_IN1(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 !
 ALLOCATE( T_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( U_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( V_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( W_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 !
 ALLOCATE(GI1(1:MESHES))
 ALLOCATE(GI2(1:MESHES))
 ALLOCATE(GJ1(1:MESHES))
 ALLOCATE(GJ2(1:MESHES))
 ALLOCATE(GK1(1:MESHES))
 ALLOCATE(GK2(1:MESHES))
 ALLOCATE(MESH_I(1:MESHES))
 ALLOCATE(MESH_J(1:MESHES))

 !coordinates for gluing!!
 NM=0
 DO J=1,J_UPPER  
  DO I=1,I_UPPER  
   NM=NM+1
   GI1(NM)=0
   GJ1(NM)=0
   GK1(NM)=0
   GI2(NM)=0   
   GJ2(NM)=0  
   GK2(NM)=0      
   MESH_I(NM)=I
   MESH_J(NM)=J
   IF(NM==1) then         !full variables, 0:IBP1 -> 1:IBP2
    GI1(NM)=1      
    GJ1(NM)=1      
    GK1(NM)=1      
    GI2(NM)=IBP1      
    GJ2(NM)=IJP1       
    GK2(NM)=KBP1   
   endif
  enddo
 enddo  
 
 
 ! we need other meshes to know where this one is 
 NM=0
 DO J=1,J_UPPER
  DO I=1,I_UPPER
   NM=NM+1           
   IF (NM>1) THEN   
    i1=1
    i2=1
    j1=1
    j2=1
    k1=1
    k2=1  
    DO JJ2=1,MESH_j(NM)
     DO II2=1,MESH_I(NM)              
          !previos mesh number based on loop:
          !KK*9+JJ*3 + (II+1)=KK*(I_UPPER+1)*(J_UPPER+1)  JJ*(I_UPPER+1) + (II+1)
             
          NM4=KK2*(MR%I_UPPER +1)*(MR%J_UPPER +1)+JJ2*(MR%I_UPPER +1) + (II2+1)
          pnt2 => MESHES(NM4)
          IF (NM4.le.NM3) THEN
          
           IF ((pnt2%MI .eq. pnt%MI ).AND.(pnt2%MJ .lt. pnt%MJ ))  THEN
           j1=j1 + pnt2%JBAR
           j2=j2 + pnt2%JBAR
           !j1=j1 + pnt2%JBP1
           !j2=j2 + pnt2%JBP1
           ENDIF
           IF ((pnt2%MI .eq. pnt%MI ).AND.(pnt2%MJ .eq. pnt%MJ ))  THEN
            j2 = j2 + pnt2%JBAR
            !j2 = j2 + pnt2%JBP1
           ENDIF
           IF ((pnt2%MJ .eq. pnt%MJ ).AND.(pnt2%MI .lt. pnt%MI ))  THEN
            i1 = i1 + pnt2%IBAR
            i2 = i2 + pnt2%IBAR
            !i1 = i1 + pnt2%IBP1
            !i2 = i2 + pnt2%IBP1
           ENDIF
           IF ((pnt2%MJ .eq. pnt%MJ ).AND.(pnt2%MI .eq. pnt%MI ))  THEN
            i2 = i2 + pnt2%IBAR
            !i2 = i2 + pnt2%IBP1
           ENDIF
           IF ((pnt2%MI .eq. pnt%MI ).AND. (pnt2%MJ .eq. pnt%MJ ).AND. &
               (pnt2%MK .lt. pnt%MK )) THEN
            k1 = k1 + pnt2%KBAR
            k2 = k2 + pnt2%KBAR
            !k1 = k1 + pnt2%KBP1
            !k2 = k2 + pnt2%KBP1
           ENDIF
           IF ((pnt2%MI .eq. pnt%MI ).AND. (pnt2%MJ .eq. pnt%MJ ).AND. &
               (pnt2%MK .eq. pnt%MK )) THEN
            k2 = k2 + pnt2%KBAR
            !k2 = k2 + pnt2%KBP1
           ENDIF       
           

           
           
           pnt%GI1 = i1
           pnt%GI2 = i2              
           pnt%GJ1 = j1
           pnt%GJ2 = j2
           pnt%GK1 = k1
           pnt%GK2 = k2              
          ENDIF
          
         ENDDO
        ENDDO
                    
      ENDIF
     ENDDO 
    ENDDO 
   ENDDO 
 
 
 
 
 
 
! IS THERE A FILE WITH WITH TIME STAMP?
 count=0
 DO TIME=1,NT
  SECOND=TIME-1
  SKIPIT=.FALSE.  
  DO PAR=1,100
    WRITE(PART,'(I2.2)'),par-1  
    WRITE(SECOND_C,'(I0)'),SECOND

   FILENAME=TRIM(run_name)// '_1_' // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'
   inquire(file=filename, exist=res)    
   if (res) then  !file exists
    count=count+1   
    !print*,filename
    skipit=.true.
   
    NM=0
    DO J=1,J_UPPER 
    DO I=1,I_UPPER
      NM=NM+1   !matched how I've done it in read (global_mesh)
      WRITE(NM_C,'(I0)'),NM

      !make filenames
      FILENAME1=TRIM(run_name) // '_' // trim(NM_C) // '_' // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'

	  
	    !read output files
      status=nf90_open(filename1, nf90_nowrite, ncid_in)
      status=nf90_inq_varid(ncid_in, 'temp_a', varid_t)
      status=nf90_inq_varid(ncid_in, 'u_a',    varid_u )
      status=nf90_inq_varid(ncid_in, 'v_a',    varid_v)
      status=nf90_inq_varid(ncid_in, 'w_a',    varid_w) 
      status=nf90_get_var(ncid_in, varid_t, t_in1,start=(/1,1,1/), &
                 count = (/ IBAR+2, JBAR+2, KBAR+2/))           
      status=nf90_get_var(ncid_in, varid_u, u_in1,start=(/1,1,1/), &
                 count = (/ IBAR+2, JBAR+2, KBAR+2/))
      status=nf90_get_var(ncid_in, varid_v, v_in1,start=(/1,1,1/), &
                 count = (/ IBAR+2, JBAR+2, KBAR+2/))      
      status=nf90_get_var(ncid_in, varid_w, w_in1,start=(/1,1,1/), &
                 count = (/ IBAR+2, JBAR+2, KBAR+2/))    
      status = nf90_close(ncid_in)           
               
    


    !glue variables
    DO I=1,IBP1 !left ghost cell only for mesh 1  
    !print*,i
     DO J=1,IBP1
      DO K=1,KBP2 !both ghost cells in z
       t_all(i,j,k,count)=t_in1(i,j,k)
       u_all(i,j,k,count)=u_in1(i,j,k)
       v_all(i,j,k,count)=v_in1(i,j,k)
       w_all(i,j,k,count)=w_in1(i,j,k)
      ENDDO
     ENDDO 
    ENDDO

    DO I=2,IBP1 !no ghost cells for mesh 2     
     !print*,i+20
     DO J=1,IBP1       
      DO K=1,KBP2  !both ghost cells in z     
       t_all(i+20,j,k,count)=t_in2(i,j,k)
       u_all(i+20,j,k,count)=u_in2(i,j,k)
       v_all(i+20,j,k,count)=v_in2(i,j,k)
       w_all(i+20,j,k,count)=w_in2(i,j,k)
      ENDDO
     ENDDO 
    ENDDO
        
    DO I=2,IBP2 !right ghost cell only for mesh 3  
     !print*,i+40
     DO J=1,IBP1       
      DO K=1,KBP2  !both ghost cells in z    
       t_all(i+40,j,k,count)=t_in3(i,j,k)
       u_all(i+40,j,k,count)=u_in3(i,j,k)
       v_all(i+40,j,k,count)=v_in3(i,j,k)
       w_all(i+40,j,k,count)=w_in3(i,j,k)
      ENDDO
     ENDDO 
    ENDDO        


   endif  ! end if file exist
   if (skipit) then
    exit   
   endif
  ENDDO !end par loop
 ENDDO  ! end main time loop
 
 print*, 'finished loop'
 !print*, shape(t_all)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!write file with full varriables - out of time loop

! status= nf90_create('run_all.nc', NF90_CLOBBER, ncid_out)
! print *, trim(nf90_strerror(status))
!
! status = nf90_def_dim(ncid_out, "xc", ibarout+2, dimxc)  
! status = nf90_def_dim(ncid_out, "yc", jbarout+2, dimyc)  
! status = nf90_def_dim(ncid_out, "zc", kbarout+2, dimzc)
! status = nf90_def_dim(ncid_out, "time", count, dimt)  
!          
! status = nf90_def_var(ncid_out, "T", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_tout)
! status = nf90_def_var(ncid_out, "U", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_uout)
! status = nf90_def_var(ncid_out, "V", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_vout)
! status = nf90_def_var(ncid_out, "W", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_wout)
! 
! status = nf90_enddef(ncid_out)
!  
! status = nf90_put_var(ncid_out, varid_tout, t_all(1:62,1:62,1:62, 1:count))
! status = nf90_put_var(ncid_out, varid_uout, u_all(1:62,1:62,1:62, 1:count))
! status = nf90_put_var(ncid_out, varid_vout, v_all(1:62,1:62,1:62, 1:count))
! status = nf90_put_var(ncid_out, varid_wout, w_all(1:62,1:62,1:62, 1:count))
!
! status = nf90_close(ncid_out) 
!
! print*, 'wrote out large dataset'     



DEALLOCATE(T_IN1)
DEALLOCATE(U_IN1)   
DEALLOCATE(V_IN1) 
DEALLOCATE(W_IN1) 


!
DEALLOCATE(T_ALL)                              
DEALLOCATE(U_ALL)                              
DEALLOCATE(V_ALL)
DEALLOCATE(W_ALL)
!
DEALLOCATE(GI1)
DEALLOCATE(GI2)
DEALLOCATE(GJ1)
DEALLOCATE(GJ2)
DEALLOCATE(GK1)
DEALLOCATE(GK2)

 end program