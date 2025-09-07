!Glue 7_3 together
! 
! _a variables read from netcdf; something is wrong
! took coupled glue meshed 
! read timestep 0 from 7_6
! use as IC for 8_3 

program make_ic
use netcdf
implicit none


!%%%%%% change for each run:
INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=60, NT=601 !601
!3x3 domain, numbers here go 0:2,0:2
INTEGER,PARAMETER:: I_UPPER=2, J_UPPER=2
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



REAL,ALLOCATABLE, DIMENSION(:,:,:):: t_in, u_in, v_in, w_in


!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t, varid_h
character(len=25)::filename,filename1,filename2,filename3
logical::res,SKIPIT
INTEGER::SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C,NM_c
character(len=25)::run_name,ICFile

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz  
integer:: varid_uout,varid_vout,varid_wout,varid_tout, varid_hout
integer:: ncid, varid1,varid2,varid3

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

ICFile= 'IC_from_' // TRIM(run_name)//  '.nc'
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

 ALLOCATE( T_IN(1:(ibar),1:(jbar),1:(kBAR)))
 ALLOCATE( U_IN(1:(ibar+1),1:(jbar+1),1:(kBAR+1)))
 ALLOCATE( V_IN(1:(ibar+1),1:(jbar+1),1:(kBAR+1)))
 ALLOCATE( W_IN(1:(ibar+1),1:(jbar+1),1:(kBAR+1)))


 T_IN=0
 U_IN=0
 V_IN=0
 W_IN=0


NM=1;
GI1=0;
GJ1=0;
GK1=0;


! status=nf90_open(ICFile, nf90_nowrite, ncid)
! status=nf90_inq_varid(ncid, 'U', varid1)
! status=nf90_inq_varid(ncid, 'V', varid2)
! status=nf90_inq_varid(ncid, 'W', varid3)
!    
! if(status /= nf90_NoErr)  print *, 'temp', trim(nf90_strerror(status))     
!     
! Print*, 'init UVW sizes ',NM,  GI1, GJ1, GK1, IBAR, JBAR, KBAR                                        
! status=nf90_get_var(ncid, varid1, U_IN,start = (/ GI1+1, GJ1+1, GK1+1 /),  count = (/ IBAR, JBAR, KBAR /) ) 
! print * ,'U ', trim(nf90_strerror(status)), NM,GI1, GJ1, GK1
! status=nf90_get_var(ncid, varid2, V_IN,start = (/ GI1+1, GJ1+1, GK1+1 /),  count = (/ IBAR, JBAR, KBAR  /) ) 
! print *,'V ', trim(nf90_strerror(status)), NM,GI1, GJ1, GK1
! status=nf90_get_var(ncid, varid3, W_IN,start = (/ GI1+1, GJ1+1, GK1+1 /),  count = (/ IBAR, JBAR, KBAR /) ) 
! print *,'W ', trim(nf90_strerror(status)), NM,GI1, GJ1, GK1
!
! status=nf90_close(ncid)
!
! Print*, 'Read V Init ', NM, V_IN(1,1,1),V_IN(1,1,55),V_IN(1,1,56),V_IN(1,1,57),V_IN(1,1,58),V_IN(1,1,59),V_IN(1,1,60)
! Print*, 'Read U Init ', NM, U_IN(1,1,1),U_IN(1,1,55),U_IN(1,1,56),U_IN(1,1,57),U_IN(1,1,58),U_IN(1,1,59),U_IN(1,1,60)

  status=nf90_open(ICFile, nf90_nowrite, ncid)                                                                
  status=nf90_inq_varid(ncid, 'T', varid1)                                                                    
  status=nf90_get_var(ncid, varid1, T_IN,start = (/ GI1+1, GJ1+1, GK1+1 /),  count = (/ IBAR, JBAR, KBAR /) ) 
  if(status /= nf90_NoErr)  print *, 'temp', trim(nf90_strerror(status))                                        
  status=nf90_close(ncid)                                                                                       
   
    Print*, 'Read T Init ', NM, T_IN(1,1,1),T_IN(1,1,55),T_IN(1,1,56),T_IN(1,1,57),T_IN(1,1,58),T_IN(1,1,59),T_IN(1,1,60)
   
   
   
   

 end program
 
 SUBROUTINE HANDLE_ERR(STATUS)
INTEGER STATUS
IF (STATUS .NE. NF_NOERR) THEN
  PRINT *, NF_STRERROR(STATUS)
  STOP 'Stopped'
ENDIF
end
