program make_wind_bc
use netcdf
implicit none


INTEGER ::I,J,K,t

! 20*3
INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=20, NT=60
INTEGER, PARAMETER:: IBP1=61, JBP1=61, KBP1=21
INTEGER, PARAMETER:: IBP2=62, JBP2=62, KBP2=22

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
!REAL, DIMENSION(IBP2,KBP2,NT):: TN,TS
REAL, DIMENSION(IBP2,KBP2,NT):: VN,VS,WN1,WN2,WS1,WS2

!REAL, DIMENSION(JBP2,KBP2,NT):: TE,TW
REAL, DIMENSION(JBP2,KBP2,NT):: UE, UW,WE1,WE2,WW1,WW2

integer:: varid_vn,varid_vs, varid_wn1,varid_wn2,varid_ws1,varid_ws2,   varid_tn,varid_ts
integer:: varid_ue,varid_uw, varid_we1,varid_we2,varid_ww1,varid_ww2,   varid_te,varid_tw

PRINT*, 'started'




! READ PALM 
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
  status=nf90_get_var(ncid_in, varid_u, u_in,start=(/x1,y1-1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,nt /))
  
  ! 156:217, 157:218 (62,62)
  status=nf90_get_var(ncid_in, varid_v, v_in,start=(/x1-1,y1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,nt /))
  
  ! 156:217, 156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_w, w_in,start=(/x1-1,y1-1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,nt /)) 
  
  ! 156:217, 156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_t, t_in,start=(/x1-1,y1-1,z1-1,t1/), count = (/ IBAR+2, JBAR+2, KBAR+2 ,nt /))
  
  status=nf90_close(ncid_in)


 
  PRINT*, 'finished reading'
  
! time loop   
  do t=1,nt  
   !SOUTH  
   do i=1,ibp2
      do k=1,kbp2       
          VS(i,k,t)=v_in(i,1,k,t)
          
          WS1(i,k,t)=w_in(i,1,k,t)
          WS2(i,k,t)=w_in(i,2,k,t)          
      enddo
   enddo

   
   !NORTH  
   do i=1,ibp2
      do k=1,kbp2       
          VN(i,k,t)=v_in(i,jbp1,k,t)
          
          WN1(i,k,t)=w_in(i,jbp1,k,t)
          WN2(i,k,t)=w_in(i,jbp2,k,t)           
      enddo
   enddo

   
   
   !EAST
   do j=1,jbp2
      do k=1,kbp2       
          UE(j,k,t)=u_in(ibp1,j,k,t)
         
          WE1(i,k,t)=w_in(ibp1,j,k,t)
          WE2(i,k,t)=w_in(ibp2,j,k,t)             
      enddo
   enddo

   
   !WEST
   do j=1,jbp2
      do k=1,kbp2       
          UW(j,k,t)=u_in(1,j,k,t)
          
          WW1(i,k,t)=w_in(1,j,k,t)
          WW2(i,k,t)=w_in(2,j,k,t)
      enddo
   enddo

 enddo !timeloop


PRINT*, UE(2,2,2),UW(2,2,2), u_in(2,1,2,2),u_in(2,jbar,2,2)
 

!OUTPUT FDS FILES
  status= nf90_create('bc_pal9.nc', NF90_CLOBBER, ncid_out)

!  status = nf90_def_dim(ncid_out, "xc", ibar, dimxc)  
!  status = nf90_def_dim(ncid_out, "yc", jbar, dimyc)  
!  status = nf90_def_dim(ncid_out, "zc", kbar, dimzc)  
  status = nf90_def_dim(ncid_out, "x", ibp2, dimx)  
  status = nf90_def_dim(ncid_out, "y", jbp2, dimy)  
  status = nf90_def_dim(ncid_out, "z", kbp2, dimz)           
  status = nf90_def_dim(ncid_out, "time", nt, dimt)       
         

  
  status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vn)  
  status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vs)     
  
  status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_uw)  
  status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_ue)      

  status = nf90_def_var(ncid_out, "WN1", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_wn1)  
  status = nf90_def_var(ncid_out, "WN2", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_wn2) 
  status = nf90_def_var(ncid_out, "WS1", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws1)  
  status = nf90_def_var(ncid_out, "WS2", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws2) 
  
  status = nf90_def_var(ncid_out, "WE1", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_we1)  
  status = nf90_def_var(ncid_out, "WE2", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_we2) 
  status = nf90_def_var(ncid_out, "WW1", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_ww1)  
  status = nf90_def_var(ncid_out, "WW2", NF90_FLOAT, (/dimy,  dimz, dimt/), varid_ww2) 

  
 
  
  status = nf90_enddef(ncid_out)
   
 
  status = nf90_put_var(ncid_out, varid_vn, VN)
  status = nf90_put_var(ncid_out, varid_vs, VS)
  status = nf90_put_var(ncid_out, varid_uw, UW)
  status = nf90_put_var(ncid_out, varid_ue, UE)
  
  status = nf90_put_var(ncid_out, varid_wn1, WN1)
  status = nf90_put_var(ncid_out, varid_wn2, WN2)
  status = nf90_put_var(ncid_out, varid_ws1, WS1)
  status = nf90_put_var(ncid_out, varid_ws2, WS2)    
  
  status = nf90_put_var(ncid_out, varid_we1, WE1)
  status = nf90_put_var(ncid_out, varid_we2, WE2)
  status = nf90_put_var(ncid_out, varid_ww1, WW1)
  status = nf90_put_var(ncid_out, varid_ww2, WW2)        
  
  status = nf90_close(ncid_out) 

  PRINT*, 'finished'  

end program 
