program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N

! 20*3
INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=20, NT=1
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR,JBAR,KBAR)::  t_in
REAL, DIMENSION(IBAR+1,JBAR+1,KBAR+1)::  u_in,v_in,w_in

!read from palm
REAL, DIMENSION(258,91):: t_in0, u_in0,v_in0,w_in0
!REAL, DIMENSION(20) :: xc_in

REAL, DIMENSION(JBAR,KBAR):: UN, US,VN,VS,WN,WS,TN,TS
REAL, DIMENSION(IBAR,KBAR):: UE, UW,VE,VW,WE,WW,TE,TW

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

  status=nf90_open('DATA_1D_PR_NETCDF_N03', nf90_nowrite, ncid_in)


  
  status=nf90_inq_varid(ncid_in, 'u', varid_u )
  status=nf90_inq_varid(ncid_in, 'v', varid_v)
  status=nf90_inq_varid(ncid_in, 'w', varid_w)
  status=nf90_inq_varid(ncid_in, 'theta', varid_t) 
  
  status=nf90_get_var(ncid_in, varid_u, u_in0)
  status=nf90_get_var(ncid_in, varid_v, v_in0)
  status=nf90_get_var(ncid_in, varid_w, w_in0) 
  status=nf90_get_var(ncid_in, varid_t, t_in0)
  
  status=nf90_close(ncid_in)

 print*, t_in0(1,1), u_in0(1,2),v_in0(1,2),w_in0(1,2)
 
 
PRINT*, 'finished reading'

  do i=1,ibp1
   do j=1,jbp1
     do k=1,kbp1       
         u_in(i,j,k)=u_in0(2,k+1)
         v_in(i,j,k)=v_in0(2,k+1)
         w_in(i,j,k)=w_in0(2,k+1)
     enddo
   enddo  
  enddo


  do i=1,ibar
   do j=1,jbar
     do k=1,kbar
         t_in(i,j,k)=t_in0(2,k+1)
     enddo
   enddo  
  enddo

!OUTPUT FDS FILES
       status= nf90_create('ic_pal6.nc', NF90_CLOBBER, ncid_out)

       status = nf90_def_dim(ncid_out, "xc", ibar, dimxc)  
       status = nf90_def_dim(ncid_out, "yc", jbar, dimyc)  
       status = nf90_def_dim(ncid_out, "zc", kbar, dimzc)  
       
       status = nf90_def_dim(ncid_out, "x", ibp1, dimx)  
       status = nf90_def_dim(ncid_out, "y", jbp1, dimy)  
       status = nf90_def_dim(ncid_out, "z", kbp1, dimz)         
       
       
             
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


