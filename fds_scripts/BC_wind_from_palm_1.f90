program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N


INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=20, NT=1
INTEGER:: IBP1, JBP1, KBP1

REAL, DIMENSION(IBAR,JBAR,KBAR):: t_in


REAL, DIMENSION(JBAR,KBAR):: UN, US,VN,VS,WN,WS,TN,TS
REAL, DIMENSION(IBAR,KBAR):: UE, UW,VE,VW,WE,WW,TE,TW

INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t




integer:: varid_un,varid_us,varid_vn,varid_vs,varid_wn,varid_ws,varid_tn,varid_ts
integer:: varid_ue,varid_uw,varid_ve,varid_vw,varid_we,varid_ww,varid_te,varid_tw

integer:: dimxc,dimyc,dimzc,dimxu,dimyv,dimzw,dimt

PRINT*, 'started'

IBP1=IBAR+1
JBP1=JBAR+1
KBP1=KBAR+1



! READ PALM sections 

  status=nf90_open('DATA_3D_NETCDF_N03', nf90_nowrite, ncid_in)

  status=nf90_inq_varid(ncid_in, 'x', dimxc)
  status=nf90_inq_varid(ncid_in, 'y', dimyc)
  status=nf90_inq_varid(ncid_in, 'zu_3d', dimzc)
  
  status=nf90_inq_varid(ncid_in, 'xu', dimxu)
  status=nf90_inq_varid(ncid_in, 'yv', dimyv)
  status=nf90_inq_varid(ncid_in, 'zw_3d', dimzw)
  
  status=nf90_inq_varid(ncid_in, 'time', dimt)
  
  status=nf90_inq_varid(ncid_in, 'u', varid_u )
  status=nf90_inq_varid(ncid_in, 'v', varid_v)
  status=nf90_inq_varid(ncid_in, 'w', varid_w)
  status=nf90_inq_varid(ncid_in, 'theta', varid_t) 
  
  !in x dir: 2504(157) - 3464 (217)
  ! for ibp1: 2496 (157) - 3472 (218)
  

  !status=nf90_get_var(ncid_in, varid_u, u_in)
  !status=nf90_get_var(ncid_in, varid_v, v_in)
  !status=nf90_get_var(ncid_in, varid_w, w_in) 
  status=nf90_get_var(ncid_in, varid_t, t_in,start=(/157,157,2,3600/), &
                                           count = (/ IBAR, JBAR, KBAR ,1 /))
  
  print*,size(t_in)
  status=nf90_close(ncid_in)

 
 
 

PRINT*, 'finished reading'



!OUTPUT FDS FILES
       status= nf90_create('bc_palm_temp.nc', NF90_CLOBBER, ncid_out)

       status = nf90_def_dim(ncid_out, "xc", ibar, dimxc)  
       status = nf90_def_dim(ncid_out, "yc", jbar, dimyc)  
       status = nf90_def_dim(ncid_out, "zc", kbar, dimzc)  
             
       status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc,dimzc/), varid_tn)
       status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc,dimzc/), varid_ts)
       status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc,dimzc/), varid_te)
       status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc,dimzc/), varid_tw)
       
       status = nf90_enddef(ncid_out)
       
       TN(:,:)=t_in(:,JBAR,:)
       TS(:,:)=t_in(:,1,:)
       TE(:,:)=t_in(1,:,:)
       TW(:,:)=t_in(IBAR,:,:)
       
       status = nf90_put_var(ncid_out, varid_tn, TN)
       status = nf90_put_var(ncid_out, varid_ts, TS)
       status = nf90_put_var(ncid_out, varid_te, TE)
       status = nf90_put_var(ncid_out, varid_tw, TW)

       status = nf90_close(ncid_out) 


PRINT*, 'finished writing'


! 
!        status= nf90_create('bc_palm.nc', NF90_CLOBBER, ncid_out)
!
!       status = nf90_def_dim(ncid_out, "xc", ibar, dimxc)  
!       status = nf90_def_dim(ncid_out, "yc", jbar, dimyc)  
!       status = nf90_def_dim(ncid_out, "zc", kbar, dimzc)  
!       
!       status = nf90_def_dim(ncid_out, "xu", ibp1, dimxu)  
!       status = nf90_def_dim(ncid_out, "yv", jbp1, dimyu)  
!       status = nf90_def_dim(ncid_out, "zw", kbp1, dimzw)         
!       
!       
!       !status = nf90_def_dim(ncid_out, "time", nt, dimt)         
!
!       status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc,dimzc/), varid_tn)
!       status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc,dimzc/), varid_ts)
!       status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc,dimzc/), varid_te)
!       status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc,dimzc/), varid_tw)
!
!      
!       !status = nf90_def_var(ncid_out, "UN", NF90_FLOAT, (/dimxu,dimzc/), varid_un)
!       !status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimxu,dimzc/), varid_us)
!       !status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimyc,dimzc/), varid_ue)
!       !status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimyc,dimzc/), varid_uw)
!       !
!       !status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimxc,dimzc/), varid_vn)
!       !status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimxc,dimzc/), varid_vs)
!       !status = nf90_def_var(ncid_out, "VE", NF90_FLOAT, (/dimyu,dimzc/), varid_ve)
!       !status = nf90_def_var(ncid_out, "VW", NF90_FLOAT, (/dimyu,dimzc/), varid_vw)
!       !
!       !status = nf90_def_var(ncid_out, "WN", NF90_FLOAT, (/dimxc,dimzw/), varid_wn)
!       !status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimxc,dimzw/), varid_ws)
!       !status = nf90_def_var(ncid_out, "WE", NF90_FLOAT, (/dimyc,dimzw/), varid_we)
!       !status = nf90_def_var(ncid_out, "WW", NF90_FLOAT, (/dimyc,dimzw/), varid_ww)       
!       
!       
!       status = nf90_enddef(ncid_out)
!       
!       status = nf90_put_var(ncid_out, varid_tn, TN)
!       status = nf90_put_var(ncid_out, varid_ts, TS)
!       status = nf90_put_var(ncid_out, varid_te, TE)
!       status = nf90_put_var(ncid_out, varid_tw, TW)
!
!       !status = nf90_put_var(ncid_out, varid_un, UN)
!       !status = nf90_put_var(ncid_out, varid_us, US)
!       !status = nf90_put_var(ncid_out, varid_ue, UE)
!       !status = nf90_put_var(ncid_out, varid_uw, UW)       
!       !
!       !status = nf90_put_var(ncid_out, varid_vn, VN)
!       !status = nf90_put_var(ncid_out, varid_vs, VS)
!       !status = nf90_put_var(ncid_out, varid_ve, VE)
!       !status = nf90_put_var(ncid_out, varid_vw, VW)
!       !
!       !status = nf90_put_var(ncid_out, varid_wn, WN)
!       !status = nf90_put_var(ncid_out, varid_ws, WS)
!       !status = nf90_put_var(ncid_out, varid_we, WE)
!       !status = nf90_put_var(ncid_out, varid_ww, WW)    
!
!
!       status = nf90_close(ncid_out) 
 
 
  


end program 


