!Glue 7_3 together
! 
! _a variables read from netcdf; something is wrong
! trying to fix whatever is wrong
! 2 July 2025 - this looks correct!


! 18 JUly 2025 : global variables do NOT match read.f90 anymore
! Here I only includes ghost cells arounf the edge of domain
! In read.f90 ghosts are included for each mesh 

program make_wind_bc
use netcdf
implicit none


!%%%%%% change for each run:
INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=60, NT=601 !601
!3x3 domain, numbers here go 0:2,0:2
INTEGER,PARAMETER:: I_UPPER=2, J_UPPER=2
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



REAL,ALLOCATABLE, DIMENSION(:,:,:):: t_in, u_in, v_in, w_in, h_in
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_all, u_all, v_all,w_all, h_all

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t, varid_h
character(len=25)::filename,filename1,filename2,filename3
logical::res,SKIPIT
INTEGER::SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C,NM_c
character(len=25)::run_name,end_file_name

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz  
integer:: varid_uout,varid_vout,varid_wout,varid_tout, varid_hout

!	other
INTEGER :: NM,IOR,IZERO,I,J,K,N,T, meshes
INTEGER:: IBP1, JBP1, KBP1, IBP2, JBP2,KBP2,IBARout, JBARout, KBARout, NTout, JJ2,II2

!global mesh
INTEGER,ALLOCATABLE,DIMENSION(:)::GI1, GI2,GJ1,GJ2,GK1, GK2, MESH_I, MESH_J
INTEGER:: NM3,NM4, i1,i2,j1,j2,k1,k2,ni,nj,nk,si,sj,sk 
integer:: nni,nnj,nnk

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!to change for other runs:
!run_name='Coupled7_7'
 CALL GET_COMMAND_ARGUMENT(1, run_name)

end_file_name= 'OUT_' // TRIM(run_name)//  '.nc'
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!size varibales
 !added one more for both 0 and ibp1 cells
 IBP1=IBAR+1
 JBP1=JBAR+1
 KBP1=KBAR+1

 IBP2=IBAR+2 
 JBP2=JBAR+2 
 KBP2=KBAR+2 
             
 IBARout=IBAR*(I_UPPER+1)
 JBARout=JBAR*(J_UPPER+1)
 KBARout=KBAR
 NTout=NT

 meshes=(I_UPPER+1)*(J_UPPER+1)

 ALLOCATE( T_IN(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( U_IN(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( V_IN(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( W_IN(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 ALLOCATE( H_IN(1:(ibar+2),1:(jbar+2),1:(kBAR+2)))
 !
 ALLOCATE( T_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( U_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( V_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( W_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( H_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 !
 ALLOCATE(GI1(1:MESHES))
 ALLOCATE(GI2(1:MESHES))
 ALLOCATE(GJ1(1:MESHES))
 ALLOCATE(GJ2(1:MESHES))
 ALLOCATE(GK1(1:MESHES))
 ALLOCATE(GK2(1:MESHES))
 ALLOCATE(MESH_I(1:MESHES))
 ALLOCATE(MESH_J(1:MESHES))        
 
 T_IN=0
 U_IN=0
 V_IN=0
 W_IN=0

 T_ALL=0
 U_ALL=0
 V_ALL=0
 W_ALL=0


 !coordinates for global mesh
 NM=0
 DO J=0,(J_UPPER)
  DO I=0,(I_UPPER)  
   NM=NM+1
   GI1(NM)=0
   GJ1(NM)=0
   GK1(NM)=0
   GI2(NM)=0   
   GJ2(NM)=0  
   GK2(NM)=0      
   MESH_I(NM)=I
   MESH_J(NM)=J
   IF(NM==1) then         !full variables, 1:IBAR
    GI1(NM)=1      
    GJ1(NM)=1      
    GK1(NM)=1      
    GI2(NM)=IBAR      
    GJ2(NM)=JBAR       
    GK2(NM)=KBAR   
   endif
  enddo
 enddo  
 
 
 ! we need other meshes to know where this one is         
 !adapted from read, line 805 (if defined global_mesh)
 NM3=0
 DO J=0,(J_UPPER)
  DO I=0,(I_UPPER)
   NM3=NM3+1           
   IF (NM3>1) THEN   
    i1=1
    i2=0
    j1=1
    j2=0
    k1=1
    k2=0  
    DO JJ2=0,MESH_J(NM3)
     DO II2=0,MESH_I(NM3)              
      !previos mesh number based on loop:
      !KK*9+JJ*3 + (II+1)=KK*(I_UPPER+1)*(J_UPPER+1)  JJ*(I_UPPER+1) + (II+1)
         
      NM4=JJ2*(I_UPPER +1) + (II2+1)
      IF (NM4.le.NM3) THEN
       IF ((mesh_i(NM4) .eq. mesh_i(NM3) ).AND.( mesh_j(NM4) .lt. mesh_j(NM3) ))  THEN
        j1=j1 + JBAR
        j2=j2 + JBAR
       ENDIF
       IF ((mesh_i(NM4) .eq. mesh_i(NM3) ).AND.( mesh_j(NM4) .eq. mesh_j(NM3) ))  THEN
        j2 = j2 + JBAR
       ENDIF
       IF ((mesh_j(NM4) .eq. mesh_j(NM3) ).AND.( mesh_i(NM4) .lt. mesh_i(NM3) ))  THEN
        i1 = i1 + IBAR
        i2 = i2 + IBAR        
       ENDIF        
       IF ((mesh_j(NM4) .eq. mesh_j(NM3) ).AND.( mesh_i(NM4) .eq. mesh_i(NM3) ))  THEN
        i2 = i2 + IBAR
       ENDIF
       IF ((mesh_i(NM4) .eq. mesh_i(NM3) ).AND.( mesh_j(NM4) .eq. mesh_j(NM3) ))  THEN
        
        k2 = k2 + KBAR        
       ENDIF
       GI1(NM3) = i1
       GI2(NM3) = i2              
       GJ1(NM3) = j1
       GJ2(NM3) = j2
       GK1(NM3) = k1
       GK2(NM3) = k2              
      ENDIF          
     ENDDO
    ENDDO                
   ENDIF
  ENDDO 
 ENDDO 
 
 
! check coordinates 
! NM=0
! DO J=0,(J_UPPER)
!  DO I=0,(I_UPPER)  
!   NM=NM+1
!    Print*, 'found mesh ',NM, ' and coordinates ', GI1(NM),GI2(NM),GJ1(NM),GJ2(NM),GK1(NM),GK2(NM)
!    !this gives: 1:20, 21:40, 41:60
!  enddo
! enddo 
 
 
 !ghost corrections:
 NM=0
 DO J=0,(J_UPPER)
  DO I=0,(I_UPPER)  
   NM=NM+1
   if (I==0) GI1(NM)=0
   if (I==I_UPPER) GI2(NM)=GI2(NM)+1
   
   if (J==0) GJ1(NM)=0
   if (J==J_UPPER) GJ2(NM)=GJ2(NM)+1
   
   GK1(NM)=0
   GK2(NM)=GK2(NM)+1
   
   
   ! Print*, 'found mesh ',NM, ' and  NEW coordinates ', GI1(NM),GI2(NM),GJ1(NM),GJ2(NM),GK1(NM),GK2(NM)
   ! this gives: 0:20, 21:40, 41:61
  enddo
 enddo 
  
 
 
 
 
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
   
    !this time stamp exists, now read all meshes in
    NM=0
    DO JJ2=0,J_UPPER 
     DO II2=0,I_UPPER
      NM=NM+1  
      !make filename
      WRITE(NM_C,'(I0)'),NM
      FILENAME1=TRIM(run_name) // '_' // trim(NM_C) // '_' // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'

	    !read output files
      status=nf90_open(filename1, nf90_nowrite, ncid_in)
      status=nf90_inq_varid(ncid_in, 'temp_a',  varid_t)
      status=nf90_inq_varid(ncid_in, 'u_a',     varid_u )
      status=nf90_inq_varid(ncid_in, 'v_a',     varid_v)
      status=nf90_inq_varid(ncid_in, 'w_a',     varid_w) 
      status=nf90_inq_varid(ncid_in, 'hrrpuv_a',varid_h) 
     
      ni=GI2(NM)-GI1(NM)+1
      nj=GJ2(NM)-GJ1(NM)+1
      nk=GK2(NM)-GK1(NM)+1
      
      if (II2==0) then
       si=1
      else
       si=2
      endif
      
      if (JJ2==0) then
       sj=1
      else
       sj=2
      endif
      sk=1
      

      status=nf90_get_var(ncid_in, varid_t, t_in(1:ni,1:nj,1:nk),start=(/si,sj,sk/), &
                count = (/ ni, nj,nk /))  
      !print *, trim(nf90_strerror(status))  , NM, GI1(NM)+1, GJ1(NM)+1,GK1(NM)+1        
      status=nf90_get_var(ncid_in, varid_u, u_in(1:ni,1:nj,1:nk),start=(/si,sj,sk/), &
                count = (/ ni, nj,nk /))
             
      status=nf90_get_var(ncid_in, varid_v, v_in(1:ni,1:nj,1:nk),start=(/si,sj,sk/), &
                count = (/ ni, nj,nk /))      
              
      status=nf90_get_var(ncid_in, varid_w, w_in(1:ni,1:nj,1:nk),start=(/si,sj,sk/), &
                count = (/ ni, nj,nk /))   
      status=nf90_get_var(ncid_in, varid_h, h_in(1:ni,1:nj,1:nk),start=(/si,sj,sk/), &
                count = (/ ni, nj,nk /))                   
      status = nf90_close(ncid_in)           
     
   
      
      !write into global variable variables
      !Global mesh coordinates:        0:20, 21:40, 41:61
      !Global +i for t-all when i==1:  1,22,42
      !Global +i for t-all when i==21: 21,42,62      
      
      nni=0
      DO I=1,ni
       nni=nni+1
       nnj=0
       DO J=1,nj
        nnj=nnj+1
        nnk=0
        DO K=1,nk 
         nnk=nnk+1
         t_all(GI1(NM)+nni,GJ1(NM)+nnj,GK1(NM)+nnk, count)=t_in(i,j,k)
         u_all(GI1(NM)+nni,GJ1(NM)+nnj,GK1(NM)+nnk, count)=u_in(i,j,k)
         v_all(GI1(NM)+nni,GJ1(NM)+nnj,GK1(NM)+nnk, count)=v_in(i,j,k)
         w_all(GI1(NM)+nni,GJ1(NM)+nnj,GK1(NM)+nnk, count)=w_in(i,j,k)
         h_all(GI1(NM)+nni,GJ1(NM)+nnj,GK1(NM)+nnk, count)=h_in(i,j,k)
        ENDDO
       ENDDO 
      ENDDO
      
     !exit this mesh
     ENDDO
    ENDDO
   endif  ! end if file exist, exit partial seconds loop
   if (skipit) then
    exit   
   endif
  ENDDO !end par loop
 ENDDO  ! end main time loop
 
 print*, 'made global variables'
 !do i=1,62
 !print*,i, t_all(i,1,1,1), t_all(i,2,1,1)
 !enddo

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!write file with full varriables - out of time loop
! need nf90_64bit_offset for files >2 GB
 status= nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 

 
 !print *, trim(nf90_strerror(status))

 status = nf90_def_dim(ncid_out, "xc", ibarout+2, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout+2, dimyc)  
 status = nf90_def_dim(ncid_out, "zc", kbarout+2, dimzc)
 status = nf90_def_dim(ncid_out, "time", count, dimt)  
          
 status = nf90_def_var(ncid_out, "T", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_tout)
 status = nf90_def_var(ncid_out, "U", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_uout)
 status = nf90_def_var(ncid_out, "V", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_vout)
 status = nf90_def_var(ncid_out, "W", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_wout)
 status = nf90_def_var(ncid_out, "H", NF90_FLOAT, (/dimxc, dimyc, dimzc, dimt/), varid_hout) 
  !print *,'define hrr', trim(nf90_strerror(status))
  
 status = nf90_enddef(ncid_out)
 !print *,'define mode end?', trim(nf90_strerror(status))
  
 status = nf90_put_var(ncid_out, varid_tout, t_all(1:62,1:62,1:62, 1:count))
 status = nf90_put_var(ncid_out, varid_uout, u_all(1:62,1:62,1:62, 1:count))
 status = nf90_put_var(ncid_out, varid_vout, v_all(1:62,1:62,1:62, 1:count))
 status = nf90_put_var(ncid_out, varid_wout, w_all(1:62,1:62,1:62, 1:count))
 status = nf90_put_var(ncid_out, varid_hout, h_all(1:62,1:62,1:62, 1:count))
 !print *,'write hrr', trim(nf90_strerror(status))
 status = nf90_close(ncid_out) 

 print*, 'wrote out large dataset'     



DEALLOCATE(T_IN)
DEALLOCATE(U_IN)   
DEALLOCATE(V_IN) 
DEALLOCATE(W_IN)
DEALLOCATE(H_IN) 


!
DEALLOCATE(T_ALL)                              
DEALLOCATE(U_ALL)                              
DEALLOCATE(V_ALL)
DEALLOCATE(W_ALL)
DEALLOCATE(H_ALL)
!
DEALLOCATE(GI1)
DEALLOCATE(GI2)
DEALLOCATE(GJ1)
DEALLOCATE(GJ2)
DEALLOCATE(GK1)
DEALLOCATE(GK2)




 end program
 
 SUBROUTINE HANDLE_ERR(STATUS)
INTEGER STATUS
IF (STATUS .NE. NF_NOERR) THEN
  PRINT *, NF_STRERROR(STATUS)
  STOP 'Stopped'
ENDIF
end
