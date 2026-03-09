!IC_palm_c1.f90
!use IC_frpm_palm_9_field and IC_coupled7_6a
! 13 Aug 2025
!
!
!Glue 7_3 together
! 
! _a variables read from netcdf; something is wrong
! took coupled glue meshed 
! read timestep 0 from 7_6
! use as IC for 8_3 
!add +1 to UVW variables (zero to IBAR in init needed)

program make_ic
use netcdf
implicit none


!%%%%%% change for each run:
! Standalone:
INTEGER:: IBAR, JBAR, KBAR
!3x3 domain, numbers here go 0:2,0:2
INTEGER:: I_UPPER, J_UPPER
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
character(len=5):: ibar1, jbar1, kbar1, i_upper1, j_upper1


REAL,ALLOCATABLE, DIMENSION(:,:,:):: t_all, u_all, v_all,w_all, h_all

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t, varid_h
character(len=25)::filename,filename1,filename2,filename3
logical::res,SKIPIT
INTEGER::SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C,NM_c
character(len=25)::run_name,end_file_name,in_file_name
character(len=3)::x11,y11,z11,t11

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
integer:: x1,y1,z1,t1

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CALL GET_COMMAND_ARGUMENT(1, run_name)
CALL GET_COMMAND_ARGUMENT(2, in_file_name)
CALL GET_COMMAND_ARGUMENT(3, IBAR1)
CALL GET_COMMAND_ARGUMENT(4, JBAR1)
CALL GET_COMMAND_ARGUMENT(5, KBAR1)
CALL GET_COMMAND_ARGUMENT(6, I_UPPER1)
CALL GET_COMMAND_ARGUMENT(7, J_UPPER1)

CALL GET_COMMAND_ARGUMENT(8,  x11)
CALL GET_COMMAND_ARGUMENT(9,  y11)
CALL GET_COMMAND_ARGUMENT(10, z11)
CALL GET_COMMAND_ARGUMENT(11, t11)



read(ibar1, *) IBAR
read(jbar1, *) JBAR
read(kbar1, *) KBAR
read(i_upper1, *) I_UPPER
read(j_upper1, *) J_upper
read(x11,*) x1
read(y11,*) y1
read(z11,*) z1
read(t11,*) t1

print*, 'read input'

Print*, run_name, trim(in_file_name), IBAR, JBAR, KBAR, I_UPPER, J_UPPER, x11, y11


!in_file_name='OUT_STD_V_1_2.nc'
end_file_name= 'ic_' // TRIM(run_name)//  '.nc'
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 print*, 'started'   


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
! NTout=NT

 meshes=(I_UPPER+1)*(J_UPPER+1)

 !
 ALLOCATE( T_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2) )) 
 ALLOCATE( U_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2) )) 
 ALLOCATE( V_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2) )) 
 ALLOCATE( W_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2) )) 
 ALLOCATE( H_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2) )) 
 !
 T_ALL=0
 U_ALL=0
 V_ALL=0
 W_ALL=0
 
  print*, 'setup finished'
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  status=nf90_open(TRIM(in_file_name), nf90_nowrite, ncid_in)
   print*, 'opened FDS output'   , trim(in_file_name)
 print*, IBAROUT+2
 call handle_err(status)
 print *, trim(nf90_strerror(status))
    
!x1=1   
!y1=1   
!z1=1     
!t1=1     

  
  status=nf90_inq_varid(ncid_in, 'U', varid_u)
  call handle_err(status)
  status=nf90_inq_varid(ncid_in, 'V', varid_v)
  call handle_err(status)
  status=nf90_inq_varid(ncid_in, 'W', varid_w)
  call handle_err(status)
  status=nf90_inq_varid(ncid_in, 'T', varid_t) 
  call handle_err(status)
  print *, trim(nf90_strerror(status))
 
  ! load all with both ghosts
  ! 158:218 
  print*, x1, y1,z1,t1
   
  status=nf90_get_var(ncid_in, varid_t, T_ALL(1:(IBARout+2), 1:(JBARout+2), 1:(KBARout+2)) ,start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,1 /))
  call handle_err(status)
  print *, trim(nf90_strerror(status))
  status=nf90_get_var(ncid_in, varid_w, W_ALL(1:(IBARout+2), 1:(JBARout+2), 2:(KBARout+2) ),start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+1) ,1 /)) 
  call handle_err(status)
  status=nf90_get_var(ncid_in, varid_u, U_ALL(1:(IBARout+2), 1:(JBARout+2), 2:(KBARout+2) ),start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+1) ,1 /))
  call handle_err(status)
  status=nf90_get_var(ncid_in, varid_v, V_ALL(1:(IBARout+2), 1:(JBARout+2), 2:(KBARout+2) ),start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+1) ,1 /))
  call handle_err(status)
  status=nf90_close(ncid_in)

  print*, 'loaded global variables', T_ALL(2,2,2)



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!write file with variables for the internal grid only 


 status= nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)

 print*, 'opened out file ', end_file_name


 status = nf90_def_dim(ncid_out, "xc", ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc", kbarout, dimzc) 
 
 status = nf90_def_dim(ncid_out, "x", ibarout+1, dimx)   !for UVW
 status = nf90_def_dim(ncid_out, "y", jbarout+1, dimy)  
 status = nf90_def_dim(ncid_out, "z", kbarout+1, dimz) 
 
          
 status = nf90_def_var(ncid_out, "T", NF90_FLOAT, (/dimxc, dimyc, dimzc /), varid_tout)
 status = nf90_def_var(ncid_out, "U", NF90_FLOAT, (/dimx,  dimy,  dimz /), varid_uout)
 status = nf90_def_var(ncid_out, "V", NF90_FLOAT, (/dimx,  dimy,  dimz /), varid_vout)
 status = nf90_def_var(ncid_out, "W", NF90_FLOAT, (/dimx,  dimy,  dimz /), varid_wout)

  
 status = nf90_enddef(ncid_out)

 print*, 'opened out file : defined variables'    
 
 !to write only internal cells, write out t_all(2:61,2:61,2:61,1)
 
 status = nf90_put_var(ncid_out, varid_tout, t_all(2:(IBARout+1),2:(JBARout+1),2:(KBARout+1))+273.15)
 status = nf90_put_var(ncid_out, varid_uout, u_all(1:(IBARout+1),1:(JBARout+1),1:(KBARout+1)))
 status = nf90_put_var(ncid_out, varid_vout, v_all(1:(IBARout+1),1:(JBARout+1),1:(KBARout+1)))
 status = nf90_put_var(ncid_out, varid_wout, w_all(1:(IBARout+1),1:(JBARout+1),1:(KBARout+1)))
 status = nf90_close(ncid_out) 

 print*, 'wrote out large dataset'     





!
DEALLOCATE(T_ALL)                              
DEALLOCATE(U_ALL)                              
DEALLOCATE(V_ALL)
DEALLOCATE(W_ALL)
DEALLOCATE(H_ALL)




 end program
 
 SUBROUTINE HANDLE_ERR(STATUS)
INTEGER STATUS
IF (STATUS .NE. NF_NOERR) THEN
  PRINT *, NF_STRERROR(STATUS)
  STOP 'Stopped'
ENDIF
end

