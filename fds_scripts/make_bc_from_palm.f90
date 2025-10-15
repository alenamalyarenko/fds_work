! use BC_coupled_8_3.f90
! 13 Aug 2025 
!
!11 Sep 2025 - varied time step

program make_ic
use netcdf
implicit none


!%%%%%% change for each run:
! Standalone:
INTEGER,PARAMETER:: IBAR=60, JBAR=60, KBAR=120, NT=1000 !601
!3x3 domain, numbers here go 0:2,0:2
INTEGER,PARAMETER:: I_UPPER=2, J_UPPER=2
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



REAL,ALLOCATABLE, DIMENSION(:,:,:):: t_in, u_in, v_in, w_in, h_in
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_all, u_all, v_all,w_all, h_all
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_fc_ns, u_fc_ns, v_fc_ns,w_fc_ns
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_fc_ew, u_fc_ew, v_fc_ew,w_fc_ew
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_fc_tb, u_fc_tb, v_fc_tb,w_fc_tb

!file in
INTEGER:: status, ncid_in
integer:: varid_u,varid_v,varid_w,varid_t, varid_h
character(len=25)::filename,filename1,filename2,filename3
logical::res,SKIPIT
INTEGER::SECOND,count,par,time 
CHARACTER(len=3)::PART,SECOND_C,NM_c
character(len=25)::run_name,end_file_name,in_file_name

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz  
integer:: varid_usout,varid_vsout,varid_wsout,varid_tsout, varid_hsout
integer:: varid_unout,varid_vnout,varid_wnout,varid_tnout, varid_hnout
integer:: varid_ueout,varid_veout,varid_weout,varid_teout, varid_heout
integer:: varid_uwout,varid_vwout,varid_wwout,varid_twout, varid_hwout
integer:: varid_utout,varid_vtout,varid_wtout,varid_ttout, varid_htout

!	other
INTEGER :: NM,IOR,IZERO,I,J,K,N,T, meshes
INTEGER:: IBP1, JBP1, KBP1, IBP2, JBP2,KBP2,IBARout, JBARout, KBARout, NTout, JJ2,II2

!global mesh
INTEGER,ALLOCATABLE,DIMENSION(:)::GI1, GI2,GJ1,GJ2,GK1, GK2, MESH_I, MESH_J
INTEGER:: NM3,NM4, i1,i2,j1,j2,k1,k2,ni,nj,nk,si,sj,sk 
integer:: nni,nnj,nnk
integer:: x1,y1,z1,t1

!for BC file:
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TS,US,VS,WS
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TN,UN,VN,WN

REAL, ALLOCATABLE, DIMENSION(:,:,:):: TE,UE,VE,WE     
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TW,UW,VW,WW 

REAL, ALLOCATABLE, DIMENSION(:,:,:):: TT,UT,VT,WT     
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!to change for other runs:
run_name='palm_c3'

in_file_name='DATA_3D_NETCDF_N03'
end_file_name= 'bc_' // TRIM(run_name)//  '.nc'
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!size varibales
 !added one more for both 0 and ibp1 cells
 IBP1=IBAR+1
 JBP1=JBAR+1
 KBP1=KBAR+1

 IBP2=IBAR+2 
 JBP2=JBAR+2 
 KBP2=KBAR+2 
             
 IBARout=IBAR*(I_UPPER+1)
 JBARout=JBAR*(J_UPPER+1)
 KBARout=KBAR
 NTout=NT

 meshes=(I_UPPER+1)*(J_UPPER+1)

 !
 ALLOCATE( T_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( U_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( V_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( W_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 ALLOCATE( H_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 !
 T_ALL=0
 U_ALL=0
 V_ALL=0
 W_ALL=0
 !
 ALLOCATE( T_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( U_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( V_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( W_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 !
 ALLOCATE( T_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( U_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( V_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( W_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 !
 ALLOCATE( T_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( U_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( V_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( W_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 
 !
 ALLOCATE( TS(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( US(1:(IBARout),1:(KBARout),NTout))  
 ALLOCATE( VS(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WS(1:(IBARout),1:(KBARout),NTout))
 !
 ALLOCATE( TN(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( UN(1:(IBARout),1:(KBARout),NTout))  
 ALLOCATE( VN(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WN(1:(IBARout),1:(KBARout),NTout))
 !                                               
 ALLOCATE( TE(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( UE(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( VE(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( WE(1:(JBARout),1:(KBARout),NTout))    
 !                                               
 ALLOCATE( TW(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( UW(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( VW(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( WW(1:(JBARout),1:(KBARout),NTout)) 
 !                                              
 ALLOCATE( TT(1:(IBARout),1:(JBARout),NTout))    
 ALLOCATE( UT(1:(IBARout),1:(JBARout),NTout))    
 ALLOCATE( VT(1:(IBARout),1:(JBARout),NTout))    
 ALLOCATE( WT(1:(IBARout),1:(JBARout),NTout))    
 
 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  status=nf90_open(in_file_name, nf90_nowrite, ncid_in)

!Palm File Info:
!	zu_3d = 151 ;  DZ=9m; Z levels: 0, 4.5, 13.5, ...  1345.5
!	zw_3d = 151 ;
!	x = 384 ;      DX=16m,
!	xu = 384 ;
!	y = 384 ;      DY=16m,
!	yv = 384 ;
! File center: (192, 192)
! Start point for FDS Domain:
! 192-(60*3)/2=102

x1=102   
y1=102   
z1=2     
t1=1     

 
  status=nf90_inq_varid(ncid_in, 'u', varid_u )
  status=nf90_inq_varid(ncid_in, 'v', varid_v)
  status=nf90_inq_varid(ncid_in, 'w', varid_w)
  status=nf90_inq_varid(ncid_in, 'theta', varid_t) 
  
  ! 157:218,156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_u, U_ALL,start=(/x1,y1-1,z1,1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /))
  
  ! 156:217, 157:218 (62,62)
  status=nf90_get_var(ncid_in, varid_v, V_ALL,start=(/x1-1,y1,z1,1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /))
  
  ! 156:217, 156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_w, W_ALL,start=(/x1-1,y1-1,z1,1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /)) 
  
  ! 156:217, 156:217 (62,62)
  status=nf90_get_var(ncid_in, varid_t, T_ALL,start=(/x1-1,y1-1,z1,1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /))
  
  status=nf90_close(ncid_in)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 DO T=1,NT

 ! add +1 to t_all indeces as t_all is 1:(ibp1+1)
  DO I=1,IBARout
   DO J=0,JBARout
    DO K=1,KBARout 
     T_FC_NS(I,J,K,T)= 0.5*(t_all(i+1,j+1,k+1,t)  + t_all(i+1,j+2,k+1,t))
     U_FC_NS(I,J,K,T)= 0.25*(u_all(i+1,j+1,k+1,t) + u_all(i,j+1,k+1,t)   + &
                             u_all(i+1,j+2,k+1,t) + u_all(i,j+2,k+1,t))
     V_FC_NS(I,J,K,T)= v_all(i+1,j+1,k+1,t)
     W_FC_NS(I,J,K,T)= 0.25* (w_all(i+1,j+1,k+1,t)+ w_all(i+1,j+2,k+1,t) + &
                              w_all(i+1,j+1,k,t)  + w_all(i+1,j+2,k,t))
    ENDDO
   ENDDO
  ENDDO     




 ! add +1 to t_all indeces as t_all is 1:(ibp1+1)
  DO I=0,IBARout
   DO J=1,JBARout
    DO K=1,KBARout 
     T_FC_EW(I,J,K,T)= 0.5*(t_all(i+1,j+1,k+1,t)  + t_all(i+1,j+2,k+1,t))
     U_FC_EW(I,J,K,T)= 0.25*(u_all(i+1,j+1,k+1,t) + u_all(i,j+1,k+1,t)   + &
                             u_all(i+1,j+2,k+1,t) + u_all(i,j+2,k+1,t))
     V_FC_EW(I,J,K,T)= v_all(i+1,j+1,k+1,t)
     W_FC_EW(I,J,K,T)= 0.25* (w_all(i+1,j+1,k+1,t)+ w_all(i+1,j+2,k+1,t) + &
                              w_all(i+1,j+1,k,t)  + w_all(i+1,j+2,k,t))
    ENDDO
   ENDDO
  ENDDO     


 ! add +1 to t_all indeces as t_all is 1:(ibp1+1)
  DO I=1,IBARout
   DO J=1,JBARout
    DO K=0,KBARout 
     T_FC_TB(I,J,K,T)= 0.5*(t_all(i+1,j+1,k+1,t)  + t_all(i+1,j+1,k+2,t))   
     U_FC_TB(I,J,K,T)= 0.25*(u_all(i+1,j+1,k+1,t) + u_all(i+2,j+1,k+1,t)   + &
                             u_all(i+1,j+1,k+2,t) + u_all(i+2,j+1,k+2,t))
     V_FC_TB(I,J,K,T)= 0.25*(v_all(i+1,j+1,k+1,t) + v_all(i+1,j+2,k+1,t) + &
                             v_all(i+1,j+1,k+2,t) + v_all(i+1,j+2,k+2,t))    
     W_FC_TB(I,J,K,T)= w_all(i+1,j+1,k+1,t)
                              
    ENDDO
   ENDDO
  ENDDO    


  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  !make out variables -from face-centerted ones
                                          
  !south border   - from NS faces
 
  DO I=1,IBARout
   DO K=1,KBARout
    TS(i,k,t)=t_fc_ns(i,0,k,t)-273.15
    US(i,k,t)=u_fc_ns(i,0,k,t)    
    VS(i,k,t)=v_fc_ns(i,0,k,t)    
    WS(i,k,t)=w_fc_ns(i,0,k,t)       
   ENDDO
  ENDDO 
  
  ! north border
  DO I=1,IBARout
   DO K=1,KBARout
    TN(i,k,t)=t_fc_ns(i,JBARout,k,t)-273.15
    UN(i,k,t)=u_fc_ns(i,JBARout,k,t)    
    VN(i,k,t)=v_fc_ns(i,JBARout,k,t)    
    WN(i,k,t)=w_fc_ns(i,JBARout,k,t)       
   ENDDO
  ENDDO 
                
                
  DO J=1,JBARout
   DO K=1,KBARout
    TE(j,k,t)=t_fc_EW(IBARout,j,k,t)-273.15
    UE(j,k,t)=u_fc_EW(IBARout,j,k,t)    
    VE(j,k,t)=v_fc_EW(IBARout,j,k,t)    
    WE(j,k,t)=w_fc_EW(IBARout,j,k,t)       
   ENDDO
  ENDDO               
                      
   DO J=1,JBARout
   DO K=1,KBARout
    TW(j,k,t)=t_fc_EW(0,j,k,t)-273.15
    UW(j,k,t)=u_fc_EW(0,j,k,t)    
    VW(j,k,t)=v_fc_EW(0,j,k,t)    
    WW(j,k,t)=w_fc_EW(0,j,k,t)       
   ENDDO
  ENDDO                        
                  
                  
  ! roof????
  DO I=1,IBARout
   DO J=1,JBARout
    TT(i,j,t)=t_fc_tb(i,j,kbarout,t)-273.15
    UT(i,j,t)=u_fc_tb(i,j,kbarout,t)    
    VT(i,j,t)=v_fc_tb(i,j,kbarout,t)    
    WT(i,j,t)=w_fc_tb(i,j,kbarout,t)       
   ENDDO
  ENDDO                        
           
                  
            
 ENDDO !end timeloop  

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 !output
 status= nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)   
             
 status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tsout)
 status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_usout)
 status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vsout)
 status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wsout)
 
 status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tnout)   
 status = nf90_def_var(ncid_out, "UN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_unout)   
 status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vnout)   
 status = nf90_def_var(ncid_out, "WN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wnout)   
 
 status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_teout)   
 status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_ueout)   
 status = nf90_def_var(ncid_out, "VE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_veout)   
 status = nf90_def_var(ncid_out, "WE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_weout)   

 status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_twout)   
 status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_uwout)   
 status = nf90_def_var(ncid_out, "VW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_vwout)   
 status = nf90_def_var(ncid_out, "WW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_wwout)   
 
 status = nf90_def_var(ncid_out, "TT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_ttout)   
 status = nf90_def_var(ncid_out, "UT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_utout)   
 status = nf90_def_var(ncid_out, "VT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_vtout)   
 status = nf90_def_var(ncid_out, "WT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_wtout)   
 
 status = nf90_enddef(ncid_out)
  
 status = nf90_put_var(ncid_out, varid_tsout, TS(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_usout, US(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vsout, VS(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wsout, WS(1:IBARout,1:KBARout, 1:NT))
 
 status = nf90_put_var(ncid_out, varid_tnout, TN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_unout, UN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vnout, VN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wnout, WN(1:IBARout,1:KBARout, 1:NT))
 
 status = nf90_put_var(ncid_out, varid_teout, TE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_ueout, UE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_veout, VE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_weout, WE(1:JBARout,1:KBARout, 1:NT))
 
 status = nf90_put_var(ncid_out, varid_twout, TW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_uwout, UW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vwout, VW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wwout, WW(1:JBARout,1:KBARout, 1:NT))
 
 status = nf90_put_var(ncid_out, varid_ttout, TT(1:IBARout,1:JBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_utout, UT(1:IBARout,1:JBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vtout, VT(1:IBARout,1:JBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wtout, WT(1:IBARout,1:JBARout, 1:NT))
 
 
 status = nf90_close(ncid_out) 
 print*, 'made new bc file'


!
DEALLOCATE(T_ALL)                              
DEALLOCATE(U_ALL)                              
DEALLOCATE(V_ALL)
DEALLOCATE(W_ALL)
DEALLOCATE(H_ALL)
!
DEALLOCATE(T_FC_NS)                              
DEALLOCATE(U_FC_NS)                              
DEALLOCATE(V_FC_NS)
DEALLOCATE(W_FC_NS)
!
DEALLOCATE(T_FC_EW)                              
DEALLOCATE(U_FC_EW)                              
DEALLOCATE(V_FC_EW)
DEALLOCATE(W_FC_EW)
!
DEALLOCATE(T_FC_TB)                              
DEALLOCATE(U_FC_TB)                              
DEALLOCATE(V_FC_TB)
DEALLOCATE(W_FC_TB)
!
DEALLOCATE(TS)
DEALLOCATE(US)  
DEALLOCATE(VS)
DEALLOCATE(WS)

DEALLOCATE(TN)
DEALLOCATE(UN)  
DEALLOCATE(VN)
DEALLOCATE(WN)

!
DEALLOCATE(TE)
DEALLOCATE(UE)  
DEALLOCATE(VE)
DEALLOCATE(WE)

DEALLOCATE(TW)
DEALLOCATE(UW)  
DEALLOCATE(VW)
DEALLOCATE(WW)

DEALLOCATE(TT)
DEALLOCATE(UT)  
DEALLOCATE(VT)
DEALLOCATE(WT)

 end program

 SUBROUTINE HANDLE_ERR(STATUS)
INTEGER STATUS
IF (STATUS .NE. NF_NOERR) THEN
  PRINT *, NF_STRERROR(STATUS)
  STOP 'Stopped'
ENDIF
end


