program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N

! 20*3
INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=20, NT=1
INTEGER:: IBP1, JBP1, KBP1

REAL, DIMENSION(IBAR,JBAR,KBAR):: t_in, u_in,v_in,w_in
REAL, DIMENSION(20) :: xc_in

REAL, DIMENSION(JBAR,KBAR):: UN, US,VN,VS,WN,WS,TN,TS
REAL, DIMENSION(IBAR,KBAR):: UE, UW,VE,VW,WE,WW,TE,TW

INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t
integer:: varid_uout,varid_vout,varid_wout,varid_tout

integer:: x1,x2,y1,y2,z1,z2,t1


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
  
x1=158
y1=158
z1=2
t1=3600

  status=nf90_get_var(ncid_in, dimxc, xc_in, start=(/158/),count=(/20/))

!  status=nf90_get_var(ncid_in, varid_u, u_in,start=(/x1,y1,z1,t1/), &
!                                           count = (/ IBAR, JBAR, KBAR ,1 /))
!  status=nf90_get_var(ncid_in, varid_v, v_in,start=(/x1,y1,z1,t1/), &
!                                           count = (/ IBAR, JBAR, KBAR ,1 /))
!  status=nf90_get_var(ncid_in, varid_w, w_in,start=(/x1,y1,z1,t1/), &
!                                           count = (/ IBAR, JBAR, KBAR ,1 /)) 
!  status=nf90_get_var(ncid_in, varid_t, t_in,start=(/x1,y1,z1,t1/), &
!                                           count = (/ IBAR, JBAR, KBAR ,1 /))
  
!  print*,size(t_in)
  print*, xc_in


  status=nf90_close(ncid_in)

 
 
 






 
 
  


end program 


