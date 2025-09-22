!take data from 7_3 and make BC for 7_4
program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N,T

INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=60, NT=601
INTEGER,PARAMETER:: IBARout=60, JBARout=60, KBARout=60, NTout=601 !
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2):: t_in1,t_in2,t_in3
REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2):: u_in1, v_in1, w_in1,u_in2, v_in2, w_in2,u_in3, v_in3, w_in3

REAL, DIMENSION(IBARout,KBARout,NTout)::  t_out
REAL, DIMENSION(IBARout,KBARout,NTout)::  u_out,v_out,w_out

REAL, DIMENSION(IBARout+2,JBARout+2,KBARout+2,nt):: t_all
REAL, DIMENSION(IBARout+2,JBARout+2,KBARout+2,nt):: u_all, v_all,w_all

REAL, DIMENSION(IBARout+2,KBARout+2,nt):: VS0,VS1,WS0,WS1


!profiles
!real, dimension(60)::profile_t
!real, dimension(61):: profile_v

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t
character(len=25)::filename,filename1,filename2,filename3
logical::res,SKIPIT
INTEGER::LAST_TIME_STEP,SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz  
integer:: varid_uout,varid_vout,varid_wout,varid_tout


PRINT*, 'started'

 !added one more for both 0 and ibp1 cells
 IBP1=IBAR+1
 JBP1=JBAR+1
 KBP1=KBAR+1


 last_time_step=601  !add 1 to last timestep

 count=0
 DO TIME=1,LAST_TIME_STEP
  SECOND=TIME-1
  SKIPIT=.FALSE.  
  DO PAR=1,100
    WRITE(PART,'(I2.2)'),par-1  
    WRITE(SECOND_C,'(I0)'),SECOND

   FILENAME=TRIM('Coupled7_3_1_') // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'
   inquire(file=filename, exist=res)    
   if (res) then  !file exists
    count=count+1   
    !print*,filename
    skipit=.true.
   

    !make filenames
    FILENAME1=TRIM('Coupled7_3_1_') // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'
    FILENAME2=TRIM('Coupled7_3_2_') // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'
    FILENAME3=TRIM('Coupled7_3_3_') // TRIM(SECOND_C) // 'p' // TRIM(part) // '.q.nc'
	  
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
               
    
    status=nf90_open(filename2, nf90_nowrite, ncid_in)
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
    
    
    status=nf90_open(filename3, nf90_nowrite, ncid_in)
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

    !glue variables
    DO I=1,IBP1 !left ghost cell only for mesh 1  
    !print*,i
     DO J=1,IBP1
      DO K=1,KBP1
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
      DO K=1,KBP1      
       t_all(i+20,j,k,count)=t_in2(i,j,k)
       u_all(i+20,j,k,count)=u_in2(i,j,k)
       v_all(i+20,j,k,count)=v_in2(i,j,k)
       w_all(i+20,j,k,count)=w_in2(i,j,k)
      ENDDO
     ENDDO 
    ENDDO
        
    DO I=2,IBAR+2 !right ghost cell only for mesh 3  
     !print*,i+40
     DO J=1,IBP1       
      DO K=1,KBP1      
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
 print*, shape(t_all)


 !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 !make out variables - 2 real layers of variables
                                         
  print*, shape(t_all)
  print*,shape(v_all)
 
 do t=1,count
 
  !south border 
 
  DO I=1,IBARout+2
   DO K=1,KBARout+2
    VS0(i,k,t)=v_all(i,0,k,t)    !v(0:ibp1,0,0:ibp1) in model numbering
    VS1(i,k,t)=v_all(i,1,k,t)    !v(0:ibp1,0,0:ibp1) in model numbering
    WS0(i,k,t)=v_all(i,0,k,t)    !v(0:ibp1,0,0:ibp1) in model numbering
    WS1(i,k,t)=v_all(i,1,k,t)    !v(0:ibp1,0,0:ibp1) in model numbering    
   ENDDO
  ENDDO 
  
 
   !print*, 'made new variables'
  
 
 
 ENDDO !end timeloop
 
 status= nf90_create('bc_palm_7b.nc', NF90_CLOBBER, ncid_out)
 
 status = nf90_def_dim(ncid_out, "xc",  ibarout+2, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout+2, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout+2, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)   
             
 status = nf90_def_var(ncid_out, "VS0", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tout)
 status = nf90_def_var(ncid_out, "VS1", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_uout)
 status = nf90_def_var(ncid_out, "WS0", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vout)
 status = nf90_def_var(ncid_out, "WS1", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wout)
 
 status = nf90_enddef(ncid_out)
  
 status = nf90_put_var(ncid_out, varid_tout, VS0)
 status = nf90_put_var(ncid_out, varid_uout, VS1)
 status = nf90_put_var(ncid_out, varid_vout, WS0)
 status = nf90_put_var(ncid_out, varid_wout, WS1)
 
 status = nf90_close(ncid_out) 
 
 
 
   print*, 'made new file'
end program 
