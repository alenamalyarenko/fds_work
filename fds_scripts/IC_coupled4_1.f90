program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N,t

INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=10, NT=1
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  t_in
REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2)::  u_in,v_in,w_in

REAL, DIMENSION(IBAR+2,KBAR+2,5):: t_s, v_s,w_s1,w_s2

!read from palm


INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t
integer:: varid_uout,varid_vout,varid_wout,varid_tout

integer:: x1,x2,y1,y2,z1,z2,t1


integer:: varid_un,varid_us,varid_vn,varid_vs,varid_wn,varid_ws1,varid_ws2,varid_tn,varid_ts
integer:: varid_ue,varid_uw,varid_ve,varid_vw,varid_we,varid_ww,varid_te,varid_tw

integer:: dimxc,dimyc,dimzc,dimxu,dimyv,dimzw,dimt
integer:: dimx,dimy,dimz

PRINT*, 'started'

! added one more for both 0 and ibp1 cells
IBP1=IBAR+1
JBP1=JBAR+1
KBP1=KBAR+1



! IC
do I=1,(ibar+2)
   do J=1,jbar+2
     do K=1,kbar+2
      t_in(i,j,k)=300
      u_in(i,j,k)=0
      v_in(i,j,k)=5
      w_in(i,j,k)=0
     
     enddo
   enddo  
enddo   






!OUTPUT FDS FILES
       status= nf90_create('ic_test_01.nc', NF90_CLOBBER, ncid_out)

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

PRINT*, 'finished ic'  


! south bc
do i=1,(ibar+2)
     do k=1,kbar+2
       do t = 1, 5
		      t_s(i,k,t)=301
		      v_s(i,k,t)=5.5
		      w_s2(i,k,t)=0
		      w_s1(i,k,t)=0     
     enddo
   enddo  
enddo   



!OUTPUT FDS FILES - BC
       status= nf90_create('bc_test_01.nc', NF90_CLOBBER, ncid_out)


  status = nf90_def_dim(ncid_out, "x", ibar+2, dimx)  
  status = nf90_def_dim(ncid_out, "z", kbar+2, dimz)           
  status = nf90_def_dim(ncid_out, "time", 5, dimt)       
         

  status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_vs)     
  status = nf90_def_var(ncid_out, "WS1", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws1)  
  status = nf90_def_var(ncid_out, "WS2", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ws2) 
  status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimx,  dimz, dimt/), varid_ts) 
  
  status = nf90_enddef(ncid_out)
  

  status = nf90_put_var(ncid_out, varid_vs, v_s)
  status = nf90_put_var(ncid_out, varid_ws1, w_s1)
  status = nf90_put_var(ncid_out, varid_ws2, w_s2) 
  status = nf90_put_var(ncid_out, varid_ts, t_s)    
        
  
  status = nf90_close(ncid_out) 





 
 
  


end program 


