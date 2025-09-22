!take data from 7_3 and make BC for 7_4
program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N,T

INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=60, NT=10
INTEGER,PARAMETER:: IBARout=60, JBARout=60, KBARout=60, NTout=10
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2):: t_in1,t_in2,t_in3
REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2):: u_in1, v_in1, w_in1,u_in2, v_in2, w_in2,u_in3, v_in3, w_in3

REAL, DIMENSION(IBARout,KBARout,NTout)::  t_out
REAL, DIMENSION(IBARout,KBARout,NTout)::  u_out,v_out,w_out

REAL, DIMENSION(IBARout+2,JBARout+2,KBARout+2):: t_all
REAL, DIMENSION(IBARout+2,JBARout+2,KBARout+2):: u_all, v_all,w_all

!profiles
!real, dimension(60)::profile_t
!real, dimension(61):: profile_v

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t
character(len=25)::filename
logical::res,SKIPIT
INTEGER::LAST_TIME_STEP,SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz
integer:: varid_uout,varid_vout,varid_wout,varid_tout


PRINT*, 'started'

! added one more for both 0 and ibp1 cells
 IBP1=IBAR+1
 JBP1=JBAR+1
 KBP1=KBAR+1


 last_time_step=61

 count=0
 DO TIME=1,LAST_TIME_STEP
  SECOND=TIME-1
  SKIPIT=.FALSE.  
  DO PAR=1,100
    WRITE(PART,'(I2.2)'),par-1  
    WRITE(SECOND_C,'(I0)'),SECOND

   FILENAME=TRIM('Coupled7_3_1_') // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'
   !print*, part
   !print*, second_c
   !print*, filename
   inquire(file=filename, exist=res)    
   if (res) then  !file exists
    count=count+1   
    !print*,filename
    skipit=.true.
    
    
    
    
    
    
    
   endif
   if (skipit) then
    exit   
   endif
  ENDDO
 ENDDO
 
 !print*, count
 stop




!read output files
 filename='Coupled7_3_1_0p00.q.nc'
 inquire(file=filename, exist=res)
 print*,res
 status=nf90_open(filename, nf90_nowrite, ncid_in)
 !print *, trim(nf90_strerror(status))
 
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
            
 filename='Coupled7_3_2_0p00.q.nc'
 status=nf90_open(filename, nf90_nowrite, ncid_in)
 !print *, trim(nf90_strerror(status))
 
 status=nf90_inq_varid(ncid_in, 'temp_a', varid_t)
 status=nf90_inq_varid(ncid_in, 'u_a',    varid_u )
 status=nf90_inq_varid(ncid_in, 'v_a',    varid_v)
 status=nf90_inq_varid(ncid_in, 'w_a',    varid_w)
  
 status=nf90_get_var(ncid_in, varid_t, t_in2,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))           
 status=nf90_get_var(ncid_in, varid_u, u_in2,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))
 status=nf90_get_var(ncid_in, varid_v, v_in2,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))      
 status=nf90_get_var(ncid_in, varid_w, w_in2,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))                                               
 status = nf90_close(ncid_in)   
 
  filename='Coupled7_3_3_0p00.q.nc'
 status=nf90_open(filename, nf90_nowrite, ncid_in)
 !print *, trim(nf90_strerror(status))
 
 status=nf90_inq_varid(ncid_in, 'temp_a', varid_t)
 status=nf90_inq_varid(ncid_in, 'u_a',    varid_u )
 status=nf90_inq_varid(ncid_in, 'v_a',    varid_v)
 status=nf90_inq_varid(ncid_in, 'w_a',    varid_w)
  
 status=nf90_get_var(ncid_in, varid_t, t_in3,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))           
 status=nf90_get_var(ncid_in, varid_u, u_in3,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))
 status=nf90_get_var(ncid_in, varid_v, v_in3,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))      
 status=nf90_get_var(ncid_in, varid_w, w_in3,start=(/1,1,1/), &
            count = (/ IBAR+2, JBAR+2, KBAR+2/))                                               
 status = nf90_close(ncid_in)   
 
 
 print*,t_in1(0,0,0), t_in1(1,1,1),t_in1(ibar,jbar,kbar), t_in1(ibar+1,jbar+1,kbar+1),  t_in1(ibar+2,jbar+2,kbar+2)
 !stop 


!glue variables
 DO I=1,IBP1 !left ghost cell only for mesh 1  
 !print*,i
  DO J=1,IBP1
   DO K=1,KBP1
    t_all(i,j,k)=t_in1(i,j,k)
    u_all(i,j,k)=u_in1(i,j,k)
    v_all(i,j,k)=v_in1(i,j,k)
    w_all(i,j,k)=w_in1(i,j,k)
   ENDDO
  ENDDO 
 ENDDO

 DO I=2,IBP1 !no ghost cells for mesh 2     
  !print*,i+20
  DO J=1,IBP1       
   DO K=1,KBP1      
    t_all(i+20,j,k)=t_in2(i,j,k)
    u_all(i+20,j,k)=u_in2(i,j,k)
    v_all(i+20,j,k)=v_in2(i,j,k)
    w_all(i+20,j,k)=w_in2(i,j,k)
   ENDDO
  ENDDO 
 ENDDO
        
 DO I=2,IBAR+2 !right ghost cell only for mesh 3  
  !print*,i+40
  DO J=1,IBP1       
   DO K=1,KBP1      
    t_all(i+40,j,k)=t_in3(i,j,k)
    u_all(i+40,j,k)=u_in3(i,j,k)
    v_all(i+40,j,k)=v_in3(i,j,k)
    w_all(i+40,j,k)=w_in3(i,j,k)
   ENDDO
  ENDDO 
 ENDDO        



!face center variables for south border



!make out variables
 DO I=1,IBARout
  DO K=1,KBARout
   DO T=1,NT
    t_out(i,k,t)=0 
    u_out(i,k,t)=0 
    v_out(i,k,t)=0   
    w_out(i,k,t)=0
   ENDDO
  ENDDO 
 ENDDO



! write file with full varriables -
 status= nf90_create('bc_all.nc', NF90_CLOBBER, ncid_out)

 status = nf90_def_dim(ncid_out, "xc", ibarout+2, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout+2, dimyc)  
 status = nf90_def_dim(ncid_out, "zc", kbarout+2, dimzc)  
 
             
 status = nf90_def_var(ncid_out, "T", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_tout)
 status = nf90_def_var(ncid_out, "U", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_uout)
 status = nf90_def_var(ncid_out, "V", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_vout)
 status = nf90_def_var(ncid_out, "W", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_wout)
 
 status = nf90_enddef(ncid_out)
  
 status = nf90_put_var(ncid_out, varid_tout, t_all(1:62,1:62,1:62))
 status = nf90_put_var(ncid_out, varid_uout, u_all(1:62,1:62,1:62))
 status = nf90_put_var(ncid_out, varid_vout, v_all(1:62,1:62,1:62))
 status = nf90_put_var(ncid_out, varid_wout, w_all(1:62,1:62,1:62))

 status = nf90_close(ncid_out) 





! write bc file - face-centered values for UVW_eddy variables 
! status= nf90_create('bc_palm_74.nc', NF90_CLOBBER, ncid_out)
!
! status = nf90_def_dim(ncid_out, "xc", ibarout, dimxc)  
! status = nf90_def_dim(ncid_out, "yc", jbarout, dimyc)  
! status = nf90_def_dim(ncid_out, "zc", kbarout, dimzc)  
! 
!             
! status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_tout)
! status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_uout)
! status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_vout)
! status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_wout)
! 
! status = nf90_enddef(ncid_out)
!  
! status = nf90_put_var(ncid_out, varid_tout, t_out)
! status = nf90_put_var(ncid_out, varid_uout, u_out)
! status = nf90_put_var(ncid_out, varid_vout, v_out)
! status = nf90_put_var(ncid_out, varid_wout, w_out)
!
! status = nf90_close(ncid_out) 


end program 