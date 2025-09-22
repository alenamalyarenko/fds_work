program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N

! 20*3
INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=20, NT=1
INTEGER:: IBP1, JBP1, KBP1

REAL, DIMENSION(IBAR+1,JBAR+1,KBAR+1):: t_in, u_in,v_in,w_in




INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t
integer:: varid_uout,varid_vout,varid_wout,varid_tout



integer:: dimxc,dimyc,dimzc,dimxu,dimyv,dimzw,dimt

PRINT*, 'started'



PRINT*, 'finished reading'



!OUTPUT FDS FILES
       status= nf90_open('ic_pal2.nc', nf90_nowrite, ncid_out)

       status = nf90_inq_varid(ncid_out, "U",varid_t) 
            
       status = nf90_get_var(ncid_out, varid_t, t_in, start=(/0, 0, 0/), count=(/20,20,20/))
   
       

       
PRINT*, size(t_in)       
       

       status = nf90_close(ncid_out) 

PRINT*, 'finished'  






 
 
  


end program 


