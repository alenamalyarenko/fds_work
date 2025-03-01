program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N

! 20*3
INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=20, NT=1
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  t_in
REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  u_in,v_in,w_in

!read from palm

!REAL, DIMENSION(20) :: xc_in

!REAL, DIMENSION(JBAR,KBAR):: UN, US,VN,VS,WN,WS,TN,TS
!REAL, DIMENSION(IBAR,KBAR):: UE, UW,VE,VW,WE,WW,TE,TW

INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t
integer:: varid_uout,varid_vout,varid_wout,varid_tout

integer:: x1,x2,y1,y2,z1,z2,t1


integer:: varid_un,varid_us,varid_vn,varid_vs,varid_wn,varid_ws,varid_tn,varid_ts
integer:: varid_ue,varid_uw,varid_ve,varid_vw,varid_we,varid_ww,varid_te,varid_tw

integer:: dimxc,dimyc,dimzc,dimxu,dimyv,dimzw,dimt
integer:: dimx,dimy,dimz

PRINT*, 'started'

! added one more for both 0 and ibp1 cells
IBP1=IBAR+1
JBP1=JBAR+1
KBP1=KBAR+1



! READ PALM profiles 

    status=nf90_open('DATA_3D_NETCDF_N03', nf90_nowrite, ncid_in)
    
x1=157
y1=157
z1=3
t1=3600

  
  status=nf90_inq_varid(ncid_in, 'u', varid_u )
  status=nf90_inq_varid(ncid_in, 'v', varid_v)
  status=nf90_inq_varid(ncid_in, 'w', varid_w)
  status=nf90_inq_varid(ncid_in, 'theta', varid_t) 
  
  ! 157:218,156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_u, u_in,start=(/x1,y1-1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,1 /))
  
  ! 156:217, 157:218 (62,62)
  status=nf90_get_var(ncid_in, varid_v, v_in,start=(/x1-1,y1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,1 /))
  
  ! 156:217, 156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_w, w_in,start=(/x1-1,y1-1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,1 /)) 
  
  ! 156:217, 156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_t, t_in,start=(/x1-1,y1-1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,1 /))
  
  status=nf90_close(ncid_in)


 
 
PRINT*, 'finished reading'

 

!OUTPUT FDS FILES
       status= nf90_create('ic_pal9.nc', NF90_CLOBBER, ncid_out)

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

PRINT*, 'finished'  






 
 
  


end program 


