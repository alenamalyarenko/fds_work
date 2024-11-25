!> \brief Assert tangential velocity boundary conditions
!> \param T Current time (s)
!> \param NM Mesh number
!> \param APPLY_TO_ESTIMATED_VARIABLES Flag indicating that estimated (starred) variables are to be used




! included in velo.f90
SUBROUTINE VELOCITY_BC_COUPLED(T,NM)
use netcdf
USE COUPLED_FILES, ONLY: OBFile
USE MESH_POINTERS

REAL(EB), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NM
	
INTEGER :: N	
	
REAL(EB) :: MUA,TSI,WGT,T_NOW,RAMP_T,OMW,MU_WALL,RHO_WALL,SLIP_COEF,VEL_T, &
            UUP(2),UUM(2),DXX(2),MU_DUIDXJ(-2:2),DUIDXJ(-2:2),PROFILE_FACTOR,VEL_GAS,VEL_GHOST, &
            MU_DUIDXJ_USE(2),DUIDXJ_USE(2),VEL_EDDY,U_TAU,Y_PLUS,U_NORM
INTEGER :: NOM(2),IIO(2),JJO(2),KKO(2),IE,II,JJ,KK,IEC,IOR,IWM,IWP,ICMM,ICMP,ICPM,ICPP,ICD,ICDO,IVL,I_SGN, &
           VELOCITY_BC_INDEX,IIGM,JJGM,KKGM,IIGP,JJGP,KKGP,SURF_INDEXM,SURF_INDEXP,ITMP,ICD_SGN,ICDO_SGN, &
           BOUNDARY_TYPE_M,BOUNDARY_TYPE_P,IS,IS2,IWPI,IWMI,VENT_INDEX


TYPE (VENTS_TYPE), POINTER :: VT
TYPE (MESH_TYPE), POINTER :: M

integer:: I,J,K
integer:: ncid, varid1,varid2,varid3, status, timestamp
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
REAL, DIMENSION(20,60,1)::U0,V0,W0	
	

          			
T_NOW = CURRENT_TIME()

! Assign local names to variables

CALL POINT_TO_MESH(NM)



timestamp=int(T)+1
!timestamp=1
!Print*, 'testprint ',  NM,T,  timestamp



VENT_LOOP: DO N=1,N_VENT
    VT => VENTS(N)
    COUPLED_VENT_IF: IF (VT%N_EDDY==(-1)) THEN !coupled vent
    status=nf90_open(OBFile, nf90_nowrite, ncid)
    !print *,'opened file', NM, trim(nf90_strerror(status))
    
   	IF (VENTS(N)%IOR==2) THEN
       !Print*, 'found south vent'

       !figure out vent / mesh overlap indeces
       !print*, NM, MI,MJ,MK, 'vent number ', N, 'vents ',VT%I1,VT%I2,VT%J1,VT%J2
       !vent coordinates are 0:20; UVW_eddy varibales are 1:20!
 
 
       status=nf90_inq_varid(ncid, 'US', varid1)
       !print *,'inq US', NM, trim(nf90_strerror(status))       
       status=nf90_inq_varid(ncid, 'VS', varid2)
       !print *,'inq VS',NM, trim(nf90_strerror(status))       
       status=nf90_inq_varid(ncid, 'WS', varid3)
       !print *,'inq WS',NM, trim(nf90_strerror(status))
 
       status=nf90_get_var(ncid, varid1, U0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read US', NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid2, V0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read VS',NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid3, W0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read WS',NM, trim(nf90_strerror(status))

       !Print*, 'eddy loop', NM, KBAR, IBAR, size( VT%U_EDDY,1),size( VT%U_EDDY,2)
       DO K=1,KBAR
        DO I=1,IBAR
         VT%U_EDDY(I,K)=U0(I,K,1) 
         VT%V_EDDY(I,K)=V0(I,K,1) 
         VT%W_EDDY(I,K)=W0(I,K,1) 
        ENDDO
       ENDDO 
       !Print*, 'assigned variables', NM ,timestamp, V0(10,1,1), V0(10,60,1), VT%V_EDDY(10,1),VT%V_EDDY(10,60) 
    ENDIF 
    
    
    
   	IF (VENTS(N)%IOR==-2) THEN
 	   !Print*, 'found north vent'
    ENDIF     
    
    
     
   	IF (VENTS(N)%IOR==1) THEN
    ENDIF   	    
    
    IF (VENTS(N)%IOR==-1) THEN
    ENDIF    
    
     
   status=nf90_close(ncid)   
   !print *,'closed netcdf',NM, trim(nf90_strerror(status))
   
   ENDIF COUPLED_VENT_IF
ENDDO VENT_LOOP

!print*, 'out of vent loop', NM



T_USED(4)=T_USED(4)+CURRENT_TIME()-T_NOW

END SUBROUTINE VELOCITY_BC_COUPLED
