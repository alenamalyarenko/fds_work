program make_wind_bc
use netcdf
implicit none


INTEGER :: NM,IOR,IZERO,I,J,K,N

! 20*3
INTEGER,PARAMETER:: IBAR=20, JBAR=20, KBAR=20, NT=1
INTEGER:: IBP1, JBP1, KBP1

REAL, DIMENSION(IBAR+2,JBAR+2,KBAR+2):: t_in, u_in,v_in,w_in
!REAL, DIMENSION(20) :: xc_in

REAL, DIMENSION(JBAR,KBAR):: UN, US,VN,VS,WN,WS,TN,TS
REAL, DIMENSION(IBAR,KBAR):: UE, UW,VE,VW,WE,WW,TE,TW

INTEGER:: status, ncid_in, ncid_out

integer:: varid_u,varid_v,varid_w,varid_t
integer:: varid_uout,varid_vout,varid_wout,varid_tout

integer:: x1,x2,y1,y2,z1,z2,t1, mesh
integer,dimension(9):: GI1,GJ1, GK1


integer:: varid_un,varid_us,varid_vn,varid_vs,varid_wn,varid_ws,varid_tn,varid_ts
integer:: varid_ue,varid_uw,varid_ve,varid_vw,varid_we,varid_ww,varid_te,varid_tw

integer:: dimxc,dimyc,dimzc,dimxu,dimyv,dimzw,dimt

PRINT*, 'started'

! added one more for both 0 and ibp1 cells
IBP1=IBAR+2
JBP1=JBAR+2
KBP1=KBAR+2



GI1=[1,21,41,1,21,41,1,21,41]
GJ1=[1,1,1, 21,21,21,41,41,41]
GK1=[1,1,1,1,1,1,1,1,1]


do i=1,22
 do j=1,22
  do k=1,22
    t_in(i,j,k)=0.
  enddo
 enddo 
enddo  



!OUTPUT FDS FILES
   status= nf90_open('ic_pal3.nc', nf90_nowrite, ncid_out)

       status = nf90_inq_varid(ncid_out, "TMP",varid_t) 
            
        status = nf90_get_var(ncid_out, varid_t, t_in)
Print*, t_in(1,1,1)

       do mesh=1,1
       
       Print*, mesh, GI1(mesh),GJ1(mesh),GK1(mesh)
       status = nf90_get_var(ncid_out, varid_t, t_in, start=(/GI1(mesh), GJ1(mesh), GK1(mesh)/), count=(/22,22,22/)) 
       Print*, t_in(1,1,1)
       Print*, t_in(22,22,22)
       Print*, size(t_in)
       enddo

      
             
       
     
    status = nf90_close(ncid_out) 
       
   
PRINT*, 'finished'  






 
 
  


end program 


