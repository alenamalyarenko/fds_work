program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N,t

INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=60, NT=1
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  t_in
REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  u_in,v_in,w_in

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz
integer:: varid_uout,varid_vout,varid_wout,varid_tout




PRINT*, 'started'

! added one more for both 0 and ibp1 cells
IBP1=IBAR+1
JBP1=JBAR+1
KBP1=KBAR+1


!ic variables
!linear UV increase with height 
DO I=1,(IBAR+2)
 DO J=1,(JBAR+2)
  DO K=1,(KBAR+2)
   t_in(i,j,k)=30+273.13
   u_in(i,j,k)=0 !0.1*k
   v_in(i,j,k)=0 !0.1*k
   w_in(i,j,k)=0
  ENDDO
 ENDDO 
ENDDO

! ic file
       status= nf90_create('ic_palm_72.nc', NF90_CLOBBER, ncid_out)

       status = nf90_def_dim(ncid_out, "xc", ibar+2, dimxc)  
       status = nf90_def_dim(ncid_out, "yc", jbar+2, dimyc)  
       status = nf90_def_dim(ncid_out, "zc", kbar+2, dimzc)  
       
       status = nf90_def_dim(ncid_out, "x", ibar+2, dimx)  
       status = nf90_def_dim(ncid_out, "y", jbar+2, dimy)  
       status = nf90_def_dim(ncid_out, "z", kbar+2, dimz)         
       
       
             
       status = nf90_def_var(ncid_out, "TMP", NF90_FLOAT, (/dimxc, dimyc, dimzc/), varid_tout)
       
       status = nf90_def_var(ncid_out, "U", NF90_FLOAT, (/dimx, dimy, dimz/), varid_uout)
       status = nf90_def_var(ncid_out, "V", NF90_FLOAT, (/dimx, dimy, dimz/), varid_vout)
       status = nf90_def_var(ncid_out, "W", NF90_FLOAT, (/dimx, dimy, dimz/), varid_wout)
       
       status = nf90_enddef(ncid_out)
        
       
       status = nf90_put_var(ncid_out, varid_tout, t_in)
       status = nf90_put_var(ncid_out, varid_uout, u_in)
       status = nf90_put_var(ncid_out, varid_vout, v_in)
       status = nf90_put_var(ncid_out, varid_wout, w_in)

       status = nf90_close(ncid_out) 





end program 


