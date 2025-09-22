program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N,t

INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=10, NT=1
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  t_in
REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  u_in,v_in,w_in

REAL, DIMENSION(IBAR+2,KBAR+2,5):: t_s, v_s,w_s1,w_s2

REAL, DIMENSION(IBAR+2,KBAR+2,5):: t_n, v_n,w_n1,w_n2

!read from palm


INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t
integer:: varid_uout,varid_vout,varid_wout,varid_tout

integer:: x1,x2,y1,y2,z1,z2,t1


integer:: varid_un,varid_us,varid_vn,varid_vs,varid_wn1, varid_wn2,varid_ws1,varid_ws2,varid_tn,varid_ts
integer:: varid_ue,varid_uw,varid_ve,varid_vw,varid_we,varid_ww,varid_te,varid_tw

integer:: dimxc,dimyc,dimzc,dimxu,dimyv,dimzw,dimt
integer:: dimx,dimy,dimz


real,dimension(22,12,1)::BC_WS1,BC_WS2,BC_VS

PRINT*, 'started'

! added one more for both 0 and ibp1 cells
IBP1=IBAR+1
JBP1=JBAR+1
KBP1=KBAR+1



! south bc
do i=1,(ibar+2)
     do k=1,kbar+2
       do t = 1, 1
		      t_s(i,k,t)=30
		      v_s(i,k,t)=6
		      w_s2(i,k,t)=0
		      w_s1(i,k,t)=0 
		      
		      t_n(i,k,t)=30
		      v_n(i,k,t)=-6
		      w_n2(i,k,t)=0
		      w_n1(i,k,t)=0 		      
		      
		          
     enddo
   enddo  
enddo   



!OUTPUT FDS FILES - BC
       status= nf90_create('bc_test_02.nc', NF90_CLOBBER, ncid_out)


  status = nf90_def_dim(ncid_out, "x", ibar+2, dimx)  
  status = nf90_def_dim(ncid_out, "z", kbar+2, dimz)           
  status = nf90_def_dim(ncid_out, "time", 5, dimt)       
         
  status = nf90_def_var(ncid_out, "TS",  NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ts) 
  status = nf90_def_var(ncid_out, "VS",  NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vs)     
  status = nf90_def_var(ncid_out, "WS1", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws1)  
  status = nf90_def_var(ncid_out, "WS2", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws2) 
  
  status = nf90_def_var(ncid_out, "TN",  NF90_FLOAT, (/dimx,  dimz, dimt/), varid_tn)
  status = nf90_def_var(ncid_out, "VN",  NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vn)
  status = nf90_def_var(ncid_out, "WN1", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_wn1)
  status = nf90_def_var(ncid_out, "WN2", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_wn2)

  
  
  
  
  status = nf90_enddef(ncid_out)
  
  status = nf90_put_var(ncid_out, varid_ts,  t_s)
  status = nf90_put_var(ncid_out, varid_vs,  v_s)
  status = nf90_put_var(ncid_out, varid_ws1, w_s1)
  status = nf90_put_var(ncid_out, varid_ws2, w_s2)    
  
  status = nf90_put_var(ncid_out, varid_tn,  t_n)
  status = nf90_put_var(ncid_out, varid_vn,  v_n)
  status = nf90_put_var(ncid_out, varid_wn1, w_n1)
  status = nf90_put_var(ncid_out, varid_wn2, w_n2)      
        
  
  status = nf90_close(ncid_out) 


!!! test read
!  status=nf90_open('bc_test_01.nc', nf90_nowrite, ncid_in)
!  status=nf90_inq_varid(ncid_in, 'VS', varid_u )
!  status=nf90_get_var(ncid_in, varid_u, BC_VS,start=(/1,1,1/), count = (/ ibar+2,  kbar+2 ,1 /))
!  status=nf90_close(ncid_in)


! Print*, BC_VS(1,1,1), shape(BC_VS), ibar, kbar
 
  


end program 


