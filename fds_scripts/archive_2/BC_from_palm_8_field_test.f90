program make_wind_bc
use netcdf
implicit none


INTEGER ::I,J,K,t

! 20*3
INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=20, NT=10
INTEGER, pARAMETER:: IBP1=61, JBP1=61, KBP1=21

!read from palm
REAL, DIMENSION(IBAR,JBAR,KBAR,NT)::  t_in
REAL, DIMENSION(IBAR+1,JBAR+1,KBAR+1,NT)::  u_in,v_in,w_in
integer:: x1,y1,z1,t1


!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz
integer:: varid_uout,varid_vout,varid_wout,varid_tout


!BC outout
REAL, DIMENSION(IBAR,KBAR,NT):: TN,TS
REAL, DIMENSION(IBP1,KBP1,NT):: UN, US,VN,VS,WN,WS
REAL, DIMENSION(JBAR,KBAR,NT):: TE,TW
REAL, DIMENSION(JBP1,KBP1,NT):: UE, UW,VE,VW,WE,WW

integer:: varid_un,varid_us,varid_vn,varid_vs,varid_wn,varid_ws,varid_tn,varid_ts
integer:: varid_ue,varid_uw,varid_ve,varid_vw,varid_we,varid_ww,varid_te,varid_tw


integer:: dimid

PRINT*, 'started'




! READ PALM profiles 

  status=nf90_open('DATA_3D_NETCDF_N03', nf90_nowrite, ncid_in)
    
  x1=158
  y1=158
  z1=1
  t1=1
  
  status=nf90_inq_varid(ncid_in, 'u', varid_u )       
  
  status=nf90_inq_dimid(ncid_in, 'u', dimid)
  PRINT*, dimid
  
  
  
!  status=nf90_inq_varid(ncid_in, 'v', varid_v)
!  status=nf90_inq_varid(ncid_in, 'w', varid_w)
!  status=nf90_inq_varid(ncid_in, 'theta', varid_t) 
  
  status=nf90_get_var(ncid_in, varid_u, u_in,start=(/x1,y1,z1,t1/), count = (/ IBP1, JBP1, KBP1 ,NT /))
!  status=nf90_get_var(ncid_in, varid_v, v_in,start=(/x1,y1,z1,t1/), count = (/ IBP1, JBP1, KBP1 ,NT /))
!  status=nf90_get_var(ncid_in, varid_w, w_in,start=(/x1,y1,z1,t1/), count = (/ IBP1, JBP1, KBP1 ,NT /)) 
  
!  status=nf90_get_var(ncid_in, varid_t, t_in,start=(/x1,y1,z1,t1/), count = (/ IBAR, JBAR, KBAR ,NT /))
  
  status=nf90_close(ncid_in)

  PRINT*, 'finished reading'
PRINT*, u_in(1,1,2,1), u_in(10,10,10,10)

stop

  
!! time loop   
!  do t=1,nt  
!   !SOUTH  
!   do i=1,ibp1
!      do k=1,kbp1       
!          US(i,k,t)=u_in(i,1,k+1,t)
!          VS(i,k,t)=v_in(i,1,k+1,t)
!          WS(i,k,t)=w_in(i,1,k+1,t)
!      enddo
!   enddo
!   do i=1,ibar
!      do k=1,kbar
!          TS(i,k,t)=t_in(i,1,k+1,t)
!      enddo
!   enddo
!   
!   !NORTH  
!   do i=1,ibp1
!      do k=1,kbp1       
!          UN(i,k,t)=u_in(i,jbp1,k+1,t)
!          VN(i,k,t)=v_in(i,jbp1,k+1,t)
!          WN(i,k,t)=w_in(i,jbp1,k+1,t)
!      enddo
!   enddo
!   do i=1,ibar
!      do k=1,kbar
!          TN(i,k,t)=t_in(i,jbar,k+1,t)
!      enddo
!   enddo
!   
!   
!   !EAST
!   do j=1,jbp1
!      do k=1,kbp1       
!          UE(j,k,t)=u_in(1,j,k+1,t)
!          VE(j,k,t)=v_in(1,j,k+1,t)
!          WE(j,k,t)=w_in(1,j,k+1,t)
!      enddo
!   enddo
!   do j=1,jbar
!      do k=1,kbar
!          TE(j,k,t)=t_in(1,j,k+1,t)
!      enddo
!   enddo
!   
!   !WEST
!   do j=1,jbp1
!      do k=1,kbp1       
!          UW(j,k,t)=u_in(ibp1,j,k+1,t)
!          VW(j,k,t)=v_in(ibp1,j,k+1,t)
!          WW(j,k,t)=w_in(ibp1,j,k+1,t)
!      enddo
!   enddo
!   do j=1,jbar
!      do k=1,kbar
!          TW(i,k,t)=t_in(ibar,j,k+1,t)
!      enddo
!   enddo
! enddo !timeloop



 

!!OUTPUT FDS FILES
!  status= nf90_create('bc_pal8.nc', NF90_CLOBBER, ncid_out)
!
!  status = nf90_def_dim(ncid_out, "xc", ibar, dimxc)  
!  status = nf90_def_dim(ncid_out, "yc", jbar, dimyc)  
!  status = nf90_def_dim(ncid_out, "zc", kbar, dimzc)  
!  status = nf90_def_dim(ncid_out, "x", ibp1, dimx)  
!  status = nf90_def_dim(ncid_out, "y", jbp1, dimy)  
!  status = nf90_def_dim(ncid_out, "z", kbp1, dimz)           
!  status = nf90_def_dim(ncid_out, "time", nt, dimt)       
!         
!  status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc,  dimzc, dimt/), varid_tn)  
!  status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc,  dimzc, dimt/), varid_ts)  
!  status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc,  dimzc, dimt/), varid_te)      
!  status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc,  dimzc, dimt/), varid_tw)    
!  
!  status = nf90_def_var(ncid_out, "UN", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_un)  
!  status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_us)  
!  status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_ue)      
!  status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_uw)     
!  
!  status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vn)    
!  status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vs)  
!  status = nf90_def_var(ncid_out, "VE", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_ve)  
!  status = nf90_def_var(ncid_out, "VW", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_vw)  
!
!  status = nf90_def_var(ncid_out, "WN", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_wn)  
!  status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws)  
!  status = nf90_def_var(ncid_out, "WE", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_we)  
!  status = nf90_def_var(ncid_out, "WW", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_ww)  
!  
!  status = nf90_enddef(ncid_out)
!   
!  status = nf90_put_var(ncid_out, varid_tn, TN)
!  status = nf90_put_var(ncid_out, varid_ts, TS)
!  status = nf90_put_var(ncid_out, varid_te, TE)
!  status = nf90_put_var(ncid_out, varid_tw, TW)
!  
!  status = nf90_put_var(ncid_out, varid_un, UN)
!  status = nf90_put_var(ncid_out, varid_us, US)
!  status = nf90_put_var(ncid_out, varid_ue, UE)
!  status = nf90_put_var(ncid_out, varid_uw, UW)  
!  
!  status = nf90_put_var(ncid_out, varid_vn, VN)   
!  status = nf90_put_var(ncid_out, varid_vs, VS)   
!  status = nf90_put_var(ncid_out, varid_ve, VE)   
!  status = nf90_put_var(ncid_out, varid_vw, VW)   
!
!  status = nf90_put_var(ncid_out, varid_wn, WN)     
!  status = nf90_put_var(ncid_out, varid_ws, WS)     
!  status = nf90_put_var(ncid_out, varid_we, WE)     
!  status = nf90_put_var(ncid_out, varid_ww, WW)     
!  
!  status = nf90_close(ncid_out) 
!
!  PRINT*, 'finished'  

end program 