! use BC_coupled_8_3.f90
! 13 Aug 2025 
!
!11 Sep 2025 - varied time step

program make_ic
use netcdf
implicit none


!%%%%%% change for each run:
! Standalone:
INTEGER:: IBAR=60, JBAR=60, KBAR=120, NT=1000 
!3x3 domain, numbers here go 0:2,0:2
INTEGER:: I_UPPER=2, J_UPPER=2
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
character(len=5):: ibar1, jbar1, kbar1,nt1, i_upper1, j_upper1
INTEGER:: COUNT5, COUNT10, COUNT60


REAL,ALLOCATABLE, DIMENSION(:,:,:):: t_in, u_in, v_in, w_in
REAL,ALLOCATABLE, DIMENSION(:,:,:,:):: t_all, u_all, v_all,w_all
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
character(len=25)::run_name,end_file_name,in_file_name,end_file_name1,end_file_name2,end_file_name3
CHARACTER(len=1)::var_name
character(len=3)::x11,y11,z11,t11

!file out
INTEGER::  ncid_out
integer:: dimxc,dimyc,dimzc,dimt, dimx,dimy,dimz  
integer:: varid_usout,varid_vsout,varid_wsout,varid_tsout
integer:: varid_unout,varid_vnout,varid_wnout,varid_tnout
integer:: varid_ueout,varid_veout,varid_weout,varid_teout
integer:: varid_uwout,varid_vwout,varid_wwout,varid_twout
integer:: varid_utout,varid_vtout,varid_wtout,varid_ttout

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


REAL, ALLOCATABLE, DIMENSION(:,:,:):: TS_10,US_10,VS_10,WS_10
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TN_10,UN_10,VN_10,WN_10
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TE_10,UE_10,VE_10,WE_10     
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TW_10,UW_10,VW_10,WW_10  
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TT_10,UT_10,VT_10,WT_10   

REAL, ALLOCATABLE, DIMENSION(:,:,:):: TS_60,US_60,VS_60,WS_60
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TN_60,UN_60,VN_60,WN_60
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TE_60,UE_60,VE_60,WE_60     
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TW_60,UW_60,VW_60,WW_60  
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TT_60,UT_60,VT_60,WT_60   


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!to change for other runs:


CALL GET_COMMAND_ARGUMENT(1, run_name)
CALL GET_COMMAND_ARGUMENT(2, in_file_name)
CALL GET_COMMAND_ARGUMENT(3, IBAR1)
CALL GET_COMMAND_ARGUMENT(4, JBAR1)
CALL GET_COMMAND_ARGUMENT(5, KBAR1)
CALL GET_COMMAND_ARGUMENT(6, nt1)
CALL GET_COMMAND_ARGUMENT(7, I_UPPER1)
CALL GET_COMMAND_ARGUMENT(8, J_UPPER1)

CALL GET_COMMAND_ARGUMENT(9,  x11)
CALL GET_COMMAND_ARGUMENT(10, y11)
CALL GET_COMMAND_ARGUMENT(11, z11)
CALL GET_COMMAND_ARGUMENT(12, t11)


read(ibar1, *) IBAR
read(jbar1, *) JBAR
read(kbar1, *) KBAR
read(nt1, *) NT

read(i_upper1, *) I_UPPER
read(j_upper1, *) J_upper

read(x11,*) x1
read(y11,*) y1
read(z11,*) z1
read(t11,*) t1

CALL GET_COMMAND_ARGUMENT(13, var_name)

!in_file_name='OUT_STD_V_1_2.nc'

print*, 'started for variable', var_name


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

!x1=1   
!y1=1   
!z1=1     
!t1=1  

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
 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 ! Change to single variable case
 
 select case (var_name)
 case('T')
 end_file_name= 'bc_' // TRIM(run_name)//  '_T.nc'
 end_file_name2= 'bc_' // TRIM(run_name)//  '_T_10s.nc'
 end_file_name3= 'bc_' // TRIM(run_name)//  '_T_1m.nc'
 ALLOCATE( T_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout))
 T_ALL=0
 ALLOCATE( T_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout))
 ALLOCATE( T_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( T_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout))
 ALLOCATE( TS(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( TN(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( TE(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( TW(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( TT(1:(IBARout),1:(JBARout),NTout))   
 
 ALLOCATE( TS_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( TN_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( TE_10(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( TW_10(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( TT_10(1:(IBARout),1:(JBARout),NTout))   
 
 ALLOCATE( TS_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( TN_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( TE_60(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( TW_60(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( TT_60(1:(IBARout),1:(JBARout),NTout))   
 
  
 status=nf90_open(TRIM(in_file_name), nf90_nowrite, ncid_in)
 status=nf90_inq_varid(ncid_in, 'T', varid_t) 
 ! 156:217, 156:217 (62,62)
 status=nf90_get_var(ncid_in, varid_t, T_ALL,start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /))
  call  HANDLE_ERR(STATUS)
 status=nf90_close(ncid_in)
 print*, 'loaded', T_ALL(10,10,1,1	)
 
 !
 DO T=1,NT 
 !print*, T
  DO J=0,JBARout
   DO I=1,IBARout
    DO K=1,KBARout  
     T_FC_NS(I,J,K,T)= 0.5*(t_all(i+1,j+1,k+1,t)  + t_all(i+1,j+2,k+1,t))
    ENDDO
   ENDDO
  ENDDO  
  DO I=0,IBARout
   DO J=1,JBARout
    DO K=1,KBARout 
      T_FC_EW(I,J,K,T)= 0.5*(t_all(i+1,j+1,k+1,t)  + t_all(i+1,j+2,k+1,t))
    ENDDO
   ENDDO
  ENDDO          
  DO I=1,IBARout
   DO J=1,JBARout
    DO K=0,KBARout   
     T_FC_TB(I,J,K,T)= 0.5*(t_all(i+1,j+1,k+1,t)  + t_all(i+1,j+1,k+2,t)) 
    ENDDO
   ENDDO
  ENDDO      
  !
  DO I=1,IBARout
   DO K=1,KBARout
    TS(i,k,t)=t_fc_ns(i,0,k,t)
    TN(i,k,t)=t_fc_ns(i,JBARout,k,t)
   ENDDO
  ENDDO 
  DO J=1,JBARout
   DO K=1,KBARout
    TE(j,k,t)=t_fc_EW(IBARout,j,k,t)
    TW(j,k,t)=t_fc_EW(0,j,k,t)
   ENDDO
  ENDDO 
  DO I=1,IBARout
   DO J=1,JBARout
    TT(i,j,t)=t_fc_tb(i,j,kbarout,t)
   ENDDO
  ENDDO   
  !
 ENDDO !TIME 
 !
 COUNT10=0
 DO T=1,NT,10
  COUNT10=COUNT10+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    TS_10(I,K,COUNT10)=TS(i,k,t)
    TN_10(I,K,COUNT10)=TN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    TE_10(J,K,COUNT10)=TE(j,k,t)   
    TW_10(J,K,COUNT10)=TW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    TT_10(I,J,COUNT10)=TT(i,j,t)   
   enddo
  enddo  
 ENDDO
 !
 COUNT60=0
 DO T=1,NT,60
  COUNT60=COUNT60+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    TS_60(I,K,COUNT60)=TS(i,k,t)
    TN_60(I,K,COUNT60)=TN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    TE_60(J,K,COUNT60)=TE(j,k,t)   
    TW_60(J,K,COUNT60)=TW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    TT_60(I,J,COUNT60)=TT(i,j,t)   
   enddo
  enddo  
 ENDDO
 
 ! 
 print*, 'starting output'
 !
 !output 
 status = nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout,   dimt)    
 !
 status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tsout)
 status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tnout)  
 status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_teout)   
 status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_twout)  
 status = nf90_def_var(ncid_out, "TT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_ttout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_tsout, TS(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_tnout, TN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_teout, TE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_twout, TW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_ttout, TT(1:IBARout,1:JBARout, 1:NT)) 
 status = nf90_close(ncid_out) 
 print*, 'made 1s file'
 !
 status = nf90_create(path=end_file_name2, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout,   dimt)    
 !
 status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tsout)
 status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tnout)  
 status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_teout)   
 status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_twout)  
 status = nf90_def_var(ncid_out, "TT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_ttout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_tsout, TS_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_tnout, TN_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_teout, TE_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_twout, TW_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_ttout, TT_10(1:IBARout,1:JBARout, 1:COUNT10)) 
 status = nf90_close(ncid_out) 
 print*, 'made 10s file'
 ! 
 status = nf90_create(path=end_file_name3, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout,   dimt)    
 !
 status = nf90_def_var(ncid_out, "TS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tsout)
 status = nf90_def_var(ncid_out, "TN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_tnout)  
 status = nf90_def_var(ncid_out, "TE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_teout)   
 status = nf90_def_var(ncid_out, "TW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_twout)  
 status = nf90_def_var(ncid_out, "TT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_ttout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_tsout, TS_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_tnout, TN_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_teout, TE_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_twout, TW_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_ttout, TT_60(1:IBARout,1:JBARout, 1:COUNT60)) 
 status = nf90_close(ncid_out) 
 print*, 'made 10s file'  
  
 !
 DEALLOCATE(T_ALL)  
 DEALLOCATE(T_FC_NS)   
 DEALLOCATE(T_FC_EW) 
 !DEALLOCATE(T_FC_TB)
 DEALLOCATE(TS)
 DEALLOCATE(TN)
 DEALLOCATE(TE)
 DEALLOCATE(TW)
 DEALLOCATE(TT)

 DEALLOCATE(TS_10)   
 DEALLOCATE(TN_10)   
 DEALLOCATE(TE_10)   
 DEALLOCATE(TW_10)   
 DEALLOCATE(TT_10)
 
 DEALLOCATE(TS_60)   
 DEALLOCATE(TN_60)   
 DEALLOCATE(TE_60)   
 DEALLOCATE(TW_60)   
 DEALLOCATE(TT_60)
 print*,'end deall'     
 ! 
    
 case('U')
 end_file_name= 'bc_' // TRIM(run_name)//  '_U.nc'
 end_file_name2= 'bc_' // TRIM(run_name)//  '_U_10s.nc'
 end_file_name3= 'bc_' // TRIM(run_name)//  '_U_1m.nc'
 ALLOCATE( U_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout))
 U_ALL=0
 ALLOCATE( U_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( U_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( U_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( US(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( UN(1:(IBARout),1:(KBARout),NTout)) 
 ALLOCATE( UE(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( UW(1:(JBARout),1:(KBARout),NTout))     
 ALLOCATE( UT(1:(IBARout),1:(JBARout),NTout))   
 
 ALLOCATE( US_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( UN_10(1:(IBARout),1:(KBARout),NTout)) 
 ALLOCATE( UE_10(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( UW_10(1:(JBARout),1:(KBARout),NTout))     
 ALLOCATE( UT_10(1:(IBARout),1:(JBARout),NTout))   
 
 ALLOCATE( US_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( UN_60(1:(IBARout),1:(KBARout),NTout)) 
 ALLOCATE( UE_60(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( UW_60(1:(JBARout),1:(KBARout),NTout))     
 ALLOCATE( UT_60(1:(IBARout),1:(JBARout),NTout))   
 
 status=nf90_open(TRIM(in_file_name), nf90_nowrite, ncid_in)
 status=nf90_inq_varid(ncid_in, 'U', varid_u )
 ! 157:218,156:217 (62,62)
 status=nf90_get_var(ncid_in, varid_u, U_ALL,start=(/x1,y1,z1,1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /))
 status=nf90_close(ncid_in)  
 print*, 'loaded'
 !
 DO T=1,NT
  DO I=1,IBARout
   DO J=0,JBARout
    DO K=1,KBARout 
     U_FC_NS(I,J,K,T)= 0.25*(u_all(i+1,j+1,k+1,t) + u_all(i,j+1,k+1,t)   + &
                             u_all(i+1,j+2,k+1,t) + u_all(i,j+2,k+1,t))
    ENDDO
   ENDDO
  ENDDO     
  DO I=0,IBARout
   DO J=1,JBARout
    DO K=1,KBARout
     U_FC_EW(I,J,K,T)= 0.25*(u_all(i+1,j+1,k+1,t) + u_all(i,j+1,k+1,t)   + &
                             u_all(i+1,j+2,k+1,t) + u_all(i,j+2,k+1,t))
    ENDDO
   ENDDO
  ENDDO     
  DO I=1,IBARout
   DO J=1,JBARout
    DO K=0,KBARout    
     U_FC_TB(I,J,K,T)= 0.25*(u_all(i+1,j+1,k+1,t) + u_all(i+2,j+1,k+1,t)   + &
                             u_all(i+1,j+1,k+2,t) + u_all(i+2,j+1,k+2,t))
     
    ENDDO
   ENDDO
  ENDDO   
  !
  DO I=1,IBARout
   DO K=1,KBARout
    US(i,k,t)=u_fc_ns(i,0,k,t)       
   ENDDO
  ENDDO 
  DO I=1,IBARout
   DO K=1,KBARout
    UN(i,k,t)=u_fc_ns(i,JBARout,k,t)            
   ENDDO
  ENDDO              
  DO J=1,JBARout
   DO K=1,KBARout
    UE(j,k,t)=u_fc_EW(IBARout,j,k,t)     
   ENDDO
  ENDDO                                 
  DO J=1,JBARout
   DO K=1,KBARout
    UW(j,k,t)=u_fc_EW(0,j,k,t)    
   ENDDO
  ENDDO                        
  DO I=1,IBARout
   DO J=1,JBARout
    UT(i,j,t)=u_fc_tb(i,j,kbarout,t)     
   ENDDO
  ENDDO                        
 ENDDO !end timeloop  
  !
 COUNT10=0
 DO T=1,NT,10
  COUNT10=COUNT10+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    US_10(I,K,COUNT10)=US(i,k,t)
    UN_10(I,K,COUNT10)=UN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    UE_10(J,K,COUNT10)=UE(j,k,t)   
    UW_10(J,K,COUNT10)=UW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    UT_10(I,J,COUNT10)=UT(i,j,t)   
   enddo
  enddo  
 ENDDO
 !
 COUNT60=0
 DO T=1,NT,60
  COUNT60=COUNT60+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    US_60(I,K,COUNT60)=US(i,k,t)
    UN_60(I,K,COUNT60)=UN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    UE_60(J,K,COUNT60)=UE(j,k,t)   
    UW_60(J,K,COUNT60)=UW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    UT_60(I,J,COUNT60)=UT(i,j,t)   
   enddo
  enddo  
 ENDDO
 !   
 print*, 'starting output'
 !
 status = nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)   
 status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_usout)
 status = nf90_def_var(ncid_out, "UN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_unout)    
 status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_ueout)   
 status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_uwout) 
 status = nf90_def_var(ncid_out, "UT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_utout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_usout, US(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_unout, UN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_ueout, UE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_uwout, UW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_utout, UT(1:IBARout,1:JBARout, 1:NT))
 status = nf90_close(ncid_out) 
 !
 status = nf90_create(path=end_file_name2, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)   
 status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_usout)
 status = nf90_def_var(ncid_out, "UN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_unout)    
 status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_ueout)   
 status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_uwout) 
 status = nf90_def_var(ncid_out, "UT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_utout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_usout, US_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_unout, UN_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_ueout, UE_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_uwout, UW_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_utout, UT_10(1:IBARout,1:JBARout, 1:COUNT10))
 status = nf90_close(ncid_out) 
 !
 status = nf90_create(path=end_file_name3, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc", jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)   
 status = nf90_def_var(ncid_out, "US", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_usout)
 status = nf90_def_var(ncid_out, "UN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_unout)    
 status = nf90_def_var(ncid_out, "UE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_ueout)   
 status = nf90_def_var(ncid_out, "UW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_uwout) 
 status = nf90_def_var(ncid_out, "UT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_utout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_usout, US_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_unout, UN_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_ueout, UE_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_uwout, UW_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_utout, UT_60(1:IBARout,1:JBARout, 1:COUNT60))
 status = nf90_close(ncid_out)   
 DEALLOCATE(U_ALL)  
 DEALLOCATE(U_FC_NS)   
 DEALLOCATE(U_FC_EW) 
 !DEALLOCATE(U_FC_TB)
 DEALLOCATE(US)
 DEALLOCATE(UN)
 DEALLOCATE(UE)
 DEALLOCATE(UW)
 DEALLOCATE(UT)

 DEALLOCATE(US_10)   
 DEALLOCATE(UN_10)   
 DEALLOCATE(UE_10)   
 DEALLOCATE(UW_10)   
 DEALLOCATE(UT_10)  
 
 DEALLOCATE(US_60)   
 DEALLOCATE(UN_60)   
 DEALLOCATE(UE_60)   
 DEALLOCATE(UW_60)   
 DEALLOCATE(UT_60)    
 
    
     
 !
 case('V')
 end_file_name= 'bc_' // TRIM(run_name)//  '_V.nc'
 end_file_name2= 'bc_' // TRIM(run_name)//  '_V_10s.nc'
 end_file_name3= 'bc_' // TRIM(run_name)//  '_V_1m.nc'
 ALLOCATE( V_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 V_ALL=0
 ALLOCATE( V_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( V_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout))
 ALLOCATE( V_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( VS(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( VN(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( VE(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( VW(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( VT(1:(IBARout),1:(JBARout),NTout)) 
 
 ALLOCATE( VS_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( VN_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( VE_10(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( VW_10(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( VT_10(1:(IBARout),1:(JBARout),NTout)) 
 
 ALLOCATE( VS_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( VN_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( VE_60(1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( VW_60(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( VT_60(1:(IBARout),1:(JBARout),NTout)) 
   
 status=nf90_open(TRIM(in_file_name), nf90_nowrite, ncid_in)
 status=nf90_inq_varid(ncid_in, 'V', varid_v)   
 ! 156:217, 157:218 (62,62)
 status=nf90_get_var(ncid_in, varid_v, V_ALL,start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /))
 status=nf90_close(ncid_in)   
 print*, 'loaded'
 !
 DO T=1,NT
   DO I=1,IBARout
   DO J=0,JBARout
    DO K=1,KBARout 
     V_FC_NS(I,J,K,T)= v_all(i+1,j+1,k+1,t)    
    ENDDO
   ENDDO
  ENDDO     
  DO I=0,IBARout
   DO J=1,JBARout
    DO K=1,KBARout 
     V_FC_EW(I,J,K,T)= v_all(i+1,j+1,k+1,t)
    ENDDO
   ENDDO
  ENDDO     
  DO I=1,IBARout
   DO J=1,JBARout
    DO K=0,KBARout 
     V_FC_TB(I,J,K,T)= 0.25*(v_all(i+1,j+1,k+1,t) + v_all(i+1,j+2,k+1,t) + &
                             v_all(i+1,j+1,k+2,t) + v_all(i+1,j+2,k+2,t))                        
    ENDDO
   ENDDO
  ENDDO    
  !
  DO I=1,IBARout
   DO K=1,KBARout   
    VS(i,k,t)=v_fc_ns(i,0,k,t)    
   ENDDO
  ENDDO 
  DO I=1,IBARout
   DO K=1,KBARout   
    VN(i,k,t)=v_fc_ns(i,JBARout,k,t)      
   ENDDO
  ENDDO        
  DO J=1,JBARout
   DO K=1,KBARout   
    VE(j,k,t)=v_fc_EW(IBARout,j,k,t)      
   ENDDO
  ENDDO                                
  DO J=1,JBARout
   DO K=1,KBARout 
    VW(j,k,t)=v_fc_EW(0,j,k,t)     
   ENDDO
  ENDDO                        
  DO I=1,IBARout
   DO J=1,JBARout
    VT(i,j,t)=v_fc_tb(i,j,kbarout,t)           
   ENDDO
  ENDDO  
 ENDDO !end timeloop 
 !
 COUNT10=0
 DO T=1,NT,10
  COUNT10=COUNT10+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    VS_10(I,K,COUNT10)=VS(i,k,t)
    VN_10(I,K,COUNT10)=VN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    VE_10(J,K,COUNT10)=VE(j,k,t)   
    VW_10(J,K,COUNT10)=VW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    VT_10(I,J,COUNT10)=VT(i,j,t)   
   enddo
  enddo  
 ENDDO
 !
 COUNT60=0
 DO T=1,NT,60
  COUNT60=COUNT60+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    VS_60(I,K,COUNT60)=VS(i,k,t)
    VN_60(I,K,COUNT60)=VN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    VE_60(J,K,COUNT60)=VE(j,k,t)   
    VW_60(J,K,COUNT60)=VW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    VT_60(I,J,COUNT60)=VT(i,j,t)   
   enddo
  enddo  
 ENDDO
 ! 
 print*, 'starting output'
 ! 
 status = nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout,   dimt)   
 status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vsout)
 status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vnout)    
 status = nf90_def_var(ncid_out, "VE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_veout)   
 status = nf90_def_var(ncid_out, "VW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_vwout) 
 status = nf90_def_var(ncid_out, "VT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_vtout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_vsout, VS(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vnout, VN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_veout, VE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vwout, VW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_vtout, VT(1:IBARout,1:JBARout, 1:NT))
 status = nf90_close(ncid_out) 
 ! 
 status = nf90_create(path=end_file_name2, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout,   dimt)   
 status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vsout)
 status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vnout)    
 status = nf90_def_var(ncid_out, "VE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_veout)   
 status = nf90_def_var(ncid_out, "VW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_vwout) 
 status = nf90_def_var(ncid_out, "VT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_vtout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_vsout, VS_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_vnout, VN_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_veout, VE_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_vwout, VW_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_vtout, VT_10(1:IBARout,1:JBARout, 1:COUNT10))
 status = nf90_close(ncid_out)  
 !
  status = nf90_create(path=end_file_name3, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout,   dimt)   
 status = nf90_def_var(ncid_out, "VS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vsout)
 status = nf90_def_var(ncid_out, "VN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_vnout)    
 status = nf90_def_var(ncid_out, "VE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_veout)   
 status = nf90_def_var(ncid_out, "VW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_vwout) 
 status = nf90_def_var(ncid_out, "VT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_vtout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_vsout, VS_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_vnout, VN_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_veout, VE_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_vwout, VW_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_vtout, VT_60(1:IBARout,1:JBARout, 1:COUNT60))
 status = nf90_close(ncid_out)  
    
 DEALLOCATE(V_ALL)  
 DEALLOCATE(V_FC_NS)   
 DEALLOCATE(V_FC_EW) 
 !DEALLOCATE(V_FC_TB)
 DEALLOCATE(VS)
 DEALLOCATE(VN)
 DEALLOCATE(VE)
 DEALLOCATE(VW)
 DEALLOCATE(VT)

 DEALLOCATE(VS_10)   
 DEALLOCATE(VN_10)   
 DEALLOCATE(VE_10)   
 DEALLOCATE(VW_10)   
 DEALLOCATE(VT_10)   
 
 DEALLOCATE(VS_60)   
 DEALLOCATE(VN_60)   
 DEALLOCATE(VE_60)   
 DEALLOCATE(VW_60)   
 DEALLOCATE(VT_60)   
   
 !
 case('W')
 end_file_name= 'bc_' // TRIM(run_name)//  '_W.nc'
 end_file_name2= 'bc_' // TRIM(run_name)//  '_W_10s.nc'
 end_file_name3= 'bc_' // TRIM(run_name)//  '_W_1m.nc'
 ALLOCATE( W_ALL(1:(IBARout+2),1:(JBARout+2),1:(KBARout+2),NTout)) 
 W_ALL=0
 ALLOCATE( W_FC_NS(1:(IBARout),0:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( W_FC_EW(0:(IBARout),1:(JBARout),1:(KBARout),NTout)) 
 ALLOCATE( W_FC_TB(0:(IBARout),1:(JBARout),1:(KBARout),NTout))
 ALLOCATE( WS(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WN(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WE(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( WW(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( WT(1:(IBARout),1:(JBARout),NTout))   
 
 ALLOCATE( WS_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WN_10(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WE_10(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( WW_10(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( WT_10(1:(IBARout),1:(JBARout),NTout))  
 
 ALLOCATE( WS_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WN_60(1:(IBARout),1:(KBARout),NTout))
 ALLOCATE( WE_60(1:(JBARout),1:(KBARout),NTout))    
 ALLOCATE( WW_60(1:(JBARout),1:(KBARout),NTout))  
 ALLOCATE( WT_60(1:(IBARout),1:(JBARout),NTout))   
  
 status=nf90_open(TRIM(in_file_name), nf90_nowrite, ncid_in)
 status=nf90_inq_varid(ncid_in, 'W', varid_w)
 ! 156:217, 156:217 (62,62)
 status=nf90_get_var(ncid_in, varid_w, W_ALL,start=(/x1,y1,z1,t1/), count = (/ (IBARout+2), (JBARout+2), (KBARout+2) ,NT /)) 
 status=nf90_close(ncid_in)
 print*, 'loaded'
 !
 DO T=1,NT
  DO I=1,IBARout
   DO J=0,JBARout
    DO K=1,KBARout 
     W_FC_NS(I,J,K,T)= 0.25* (w_all(i+1,j+1,k+1,t)+ w_all(i+1,j+2,k+1,t) + &
                              w_all(i+1,j+1,k,t)  + w_all(i+1,j+2,k,t))
    ENDDO
   ENDDO
  ENDDO  
  DO I=0,IBARout
   DO J=1,JBARout
    DO K=1,KBARout 
     W_FC_EW(I,J,K,T)= 0.25* (w_all(i+1,j+1,k+1,t)+ w_all(i+1,j+2,k+1,t) + &
                              w_all(i+1,j+1,k,t)  + w_all(i+1,j+2,k,t))
    ENDDO
   ENDDO
  ENDDO  
  DO I=1,IBARout
   DO J=1,JBARout
    DO K=0,KBARout  
     W_FC_TB(I,J,K,T)= w_all(i+1,j+1,k+1,t)  
    ENDDO
   ENDDO
  ENDDO    
  !
  DO I=1,IBARout
   DO K=1,KBARout
    WS(i,k,t)=w_fc_ns(i,0,k,t)       
   ENDDO
  ENDDO   
  DO I=1,IBARout
   DO K=1,KBARout  
    WN(i,k,t)=w_fc_ns(i,JBARout,k,t)       
   ENDDO
  ENDDO 
  DO J=1,JBARout
   DO K=1,KBARout
    WE(j,k,t)=w_fc_EW(IBARout,j,k,t)       
   ENDDO
  ENDDO  
  DO J=1,JBARout
   DO K=1,KBARout
    WW(j,k,t)=w_fc_EW(0,j,k,t)       
   ENDDO
  ENDDO  
  DO I=1,IBARout
   DO J=1,JBARout
    WT(i,j,t)=w_fc_tb(i,j,kbarout,t)  
   ENDDO
  ENDDO  
 ENDDO !end timeloop
 !
 COUNT10=0
 DO T=1,NT,10
  COUNT10=COUNT10+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    WS_10(I,K,COUNT10)=WS(i,k,t)
    WN_10(I,K,COUNT10)=WN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    WE_10(J,K,COUNT10)=WE(j,k,t)   
    WW_10(J,K,COUNT10)=WW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    WT_10(I,J,COUNT10)=WT(i,j,t)   
   enddo
  enddo  
 ENDDO
 !
 COUNT60=0
 DO T=1,NT,60
  COUNT60=COUNT60+1
  DO I=1,IBARout     
   DO K=1,KBARout    
    WS_60(I,K,COUNT60)=WS(i,k,t)
    WN_60(I,K,COUNT60)=WN(i,k,t)   
   enddo
  enddo  
  DO J=1,JBARout         
   DO K=1,KBARout        
    WE_60(J,K,COUNT60)=WE(j,k,t)   
    WW_60(J,K,COUNT60)=WW(j,k,t)   
   enddo
  enddo  
  DO I=1,IBARout         
   DO J=1,JBARout        
    WT_60(I,J,COUNT60)=WT(i,j,t)   
   enddo
  enddo  
 ENDDO
 !   
 print*, 'starting output'
 ! 
 status = nf90_create(path=end_file_name, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)  
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)  
 status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wsout)
 status = nf90_def_var(ncid_out, "WN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wnout)   
 status = nf90_def_var(ncid_out, "WW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_wwout) 
 status = nf90_def_var(ncid_out, "WE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_weout)  
 status = nf90_def_var(ncid_out, "WT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_wtout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_wsout, WS(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wnout, WN(1:IBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_weout, WE(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wwout, WW(1:JBARout,1:KBARout, 1:NT))
 status = nf90_put_var(ncid_out, varid_wtout, WT(1:IBARout,1:JBARout, 1:NT))
 status = nf90_close(ncid_out) 
  ! 
 status = nf90_create(path=end_file_name2, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)  
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)  
 status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wsout)
 status = nf90_def_var(ncid_out, "WN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wnout)   
 status = nf90_def_var(ncid_out, "WW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_wwout) 
 status = nf90_def_var(ncid_out, "WE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_weout)  
 status = nf90_def_var(ncid_out, "WT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_wtout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_wsout, WS_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_wnout, WN_10(1:IBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_weout, WE_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_wwout, WW_10(1:JBARout,1:KBARout, 1:COUNT10))
 status = nf90_put_var(ncid_out, varid_wtout, WT_10(1:IBARout,1:JBARout, 1:COUNT10))
 status = nf90_close(ncid_out) 
 ! 
 status = nf90_create(path=end_file_name3, cmode=or(nf90_noclobber,nf90_64bit_offset), ncid=ncid_out)  
 status = nf90_def_dim(ncid_out, "xc",  ibarout, dimxc)  
 status = nf90_def_dim(ncid_out, "yc",  jbarout, dimyc)  
 status = nf90_def_dim(ncid_out, "zc",  kbarout, dimzc)
 status = nf90_def_dim(ncid_out, "time",ntout, dimt)  
 status = nf90_def_var(ncid_out, "WS", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wsout)
 status = nf90_def_var(ncid_out, "WN", NF90_FLOAT, (/dimxc, dimzc, dimt/), varid_wnout)   
 status = nf90_def_var(ncid_out, "WW", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_wwout) 
 status = nf90_def_var(ncid_out, "WE", NF90_FLOAT, (/dimyc, dimzc, dimt/), varid_weout)  
 status = nf90_def_var(ncid_out, "WT", NF90_FLOAT, (/dimxc, dimyc, dimt/), varid_wtout) 
 status = nf90_enddef(ncid_out)
 status = nf90_put_var(ncid_out, varid_wsout, WS_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_wnout, WN_60(1:IBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_weout, WE_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_wwout, WW_60(1:JBARout,1:KBARout, 1:COUNT60))
 status = nf90_put_var(ncid_out, varid_wtout, WT_60(1:IBARout,1:JBARout, 1:COUNT60))
 status = nf90_close(ncid_out) 
 !
 
 DEALLOCATE(W_ALL)
 DEALLOCATE(W_FC_NS)
 DEALLOCATE(W_FC_EW)
 !DEALLOCATE(W_FC_TB)
 DEALLOCATE(WS)
 DEALLOCATE(WN)
 DEALLOCATE(WE)
 DEALLOCATE(WW)
 DEALLOCATE(WT)
 DEALLOCATE(WS_10)
 DEALLOCATE(WN_10)
 DEALLOCATE(WE_10)
 DEALLOCATE(WW_10)
 DEALLOCATE(WT_10)
 
 DEALLOCATE(WS_60)
 DEALLOCATE(WN_60)
 DEALLOCATE(WE_60)
 DEALLOCATE(WW_60)
 DEALLOCATE(WT_60)
 
 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end select
end program

SUBROUTINE HANDLE_ERR(STATUS)
 INTEGER STATUS
 IF (STATUS .NE. NF_NOERR) THEN
  PRINT *, NF_STRERROR(STATUS)
  STOP 'Stopped'
 ENDIF
end

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
