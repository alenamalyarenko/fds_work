!take data from 7_3 and make IC for 7_4
program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N,t

INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=60, NT=1
INTEGER:: IBP1, JBP1, KBP1


REAL, DIMENSION(IBAR,JBAR,KBAR)::  t_in
REAL, DIMENSION(IBAR+1,JBAR+1,  KBAR+1)::  u_in, v_in, w_in

!profiles
real, dimension(60)::profile_t
real, dimension(61):: profile_v

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz
integer:: varid_uout,varid_vout,varid_wout,varid_tout




PRINT*, 'started'
PRINT*, shape(t_in)

! added one more for both 0 and ibp1 cells
IBP1=IBAR+1
JBP1=JBAR+1
KBP1=KBAR+1


profile_t=[20.080116, 20.002512, 19.957998,	19.924244,  19.896271, &
         	 19.872137,	19.850210, 19.829983,	19.811108,	19.793394, &
         	 19.776569,	19.760323, 19.744684,	19.729616,	19.714846, &
         	 19.700478,	19.686424, 19.672619,	19.659241,	19.645876, &
         	 19.632744,	19.619982, 19.607201,	19.594515,	19.582079, &
         	 19.569788,	19.557571, 19.545374,	19.533381,	19.521574, &
         	 19.509697,	19.497820, 19.486088,	19.474529,	19.463066, &
         	 19.451536,	19.440004, 19.428610,	19.417290,	19.406048, &
         	 19.394804,	19.383562, 19.372387,	19.361172,	19.350075, &
         	 19.339138,	19.328135, 19.317131,	19.306126,	19.295122, &
         	 19.284185,	19.273275, 19.262465,	19.251724,	19.240917, &
         	 19.230110,	19.219303, 19.208498,	19.197756,	19.192385]

profile_v=[4.0026298, 4.0026298,	4.9595480,	5.4550920,	5.7975311,	6.0641541, &
           6.2716322,	6.4437304,	6.5978031,	6.7310324,	6.8488493, &
           6.9589324,	7.0522189,	7.1423159,	7.2259359,	7.2948380, &
           7.3665338,	7.4365463,	7.4953790,	7.5515394,	7.6084247, &
           7.6595073,	7.7037301,	7.7492976,	7.7976718,	7.8387156, &
           7.8787007,	7.9201908,	7.9562182,	7.9897199,	8.0241718, &
           8.0558405,	8.0920620,	8.1225052,	8.1466227,	8.1784582, &
           8.2044640,	8.2325497,	8.2606821,	8.2852364,	8.3108511, &
           8.3336525,	8.3586779,	8.3799610,	8.4012432,	8.4255295, &
           8.4468880,	8.4649811,	8.4833851,	8.5034189,	8.5249033, &
           8.5479641,	8.5651178,	8.5784779,	8.5962954,	8.6154165, &
           8.6291580,	8.6450558,	8.6626663,	8.6787348, 	8.6873560]

!ic variables
!linear UV increase with height 
DO I=1,(IBAR)
 DO J=1,(JBAR)
  DO K=1,(KBAR)
   t_in(i,j,k)=profile_t(k)+273.15
  ENDDO
 ENDDO 
ENDDO

DO I=1,IBP1
 DO J=1,JBP1
  DO K=1,KBP1
   u_in(i,j,k)=0 
   w_in(i,j,k)=0 
   v_in(i,j,k)=profile_v(k)
  ENDDO
 ENDDO 
ENDDO




! ic file
       status= nf90_create('ic_palm_74.nc', NF90_CLOBBER, ncid_out)

       status = nf90_def_dim(ncid_out, "xc", ibar, dimxc)  
       status = nf90_def_dim(ncid_out, "yc", jbar, dimyc)  
       status = nf90_def_dim(ncid_out, "zc", kbar, dimzc)  
       
       status = nf90_def_dim(ncid_out, "x", ibar+1, dimx)  
       status = nf90_def_dim(ncid_out, "y", jbar+1, dimy)  
       status = nf90_def_dim(ncid_out, "z", kbar+1, dimz)         
       
       
             
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


