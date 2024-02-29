!> \brief Assert tangential velocity boundary conditions
!> \param T Current time (s)
!> \param NM Mesh number
!> \param APPLY_TO_ESTIMATED_VARIABLES Flag indicating that estimated (starred) variables are to be used

! included at the end of main
SUBROUTINE VELOCITY_BC_COUPLED(T,NM, APPLY_TO_ESTIMATED_VARIABLES)

USE MESH_POINTERS
USE MATH_FUNCTIONS, ONLY: EVALUATE_RAMP
USE TURBULENCE, ONLY: WALL_MODEL
USE PHYSICAL_FUNCTIONS, ONLY: GET_CONDUCTIVITY,GET_SPECIFIC_HEAT
USE CC_SCALARS, ONLY : CC_VELOCITY_BC,GET_OPENBC_TANGENTIAL_CUTFACE_VEL


USE COUPLED_FILES
use netcdf

REAL(EB), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NM
LOGICAL, INTENT(IN) :: APPLY_TO_ESTIMATED_VARIABLES
	
INTEGER :: N	
	
REAL(EB) :: MUA,TSI,WGT,T_NOW,RAMP_T,OMW,MU_WALL,RHO_WALL,SLIP_COEF,VEL_T, &
            UUP(2),UUM(2),DXX(2),MU_DUIDXJ(-2:2),DUIDXJ(-2:2),PROFILE_FACTOR,VEL_GAS,VEL_GHOST, &
            MU_DUIDXJ_USE(2),DUIDXJ_USE(2),VEL_EDDY,U_TAU,Y_PLUS,U_NORM
INTEGER :: NOM(2),IIO(2),JJO(2),KKO(2),IE,II,JJ,KK,IEC,IOR,IWM,IWP,ICMM,ICMP,ICPM,ICPP,ICD,ICDO,IVL,I_SGN, &
           VELOCITY_BC_INDEX,IIGM,JJGM,KKGM,IIGP,JJGP,KKGP,SURF_INDEXM,SURF_INDEXP,ITMP,ICD_SGN,ICDO_SGN, &
           BOUNDARY_TYPE_M,BOUNDARY_TYPE_P,IS,IS2,IWPI,IWMI,VENT_INDEX
LOGICAL :: ALTERED_GRADIENT(-2:2),SYNTHETIC_EDDY_METHOD,HVAC_TANGENTIAL,INTERPOLATED_EDGE,&
           UPWIND_BOUNDARY,INFLOW_BOUNDARY
REAL(EB), POINTER, DIMENSION(:,:,:) :: UU,VV,WW,RHOP,VEL_OTHER
REAL(EB), POINTER, DIMENSION(:,:,:,:) :: ZZP
TYPE (OMESH_TYPE), POINTER :: OM
TYPE (VENTS_TYPE), POINTER :: VT
TYPE (WALL_TYPE), POINTER :: WCM,WCP,WCX
TYPE (BOUNDARY_PROP1_TYPE), POINTER :: WCM_B1,WCP_B1,WCX_B1
TYPE (EDGE_TYPE), POINTER :: ED

integer:: ncid, varid1,varid2,varid3, status
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
INTEGER  :: I,J,K,IMIN,IMAX,JMIN,JMAX,KMIN,KMAX
	
!  PRINT*, 'Entered velocity bc'

T_NOW = CURRENT_TIME()

! Assign local names to variables

CALL POINT_TO_MESH(NM)


!Print*, 'ch1'
! Point to the appropriate velocity field

IF (APPLY_TO_ESTIMATED_VARIABLES) THEN
   UU => US
   VV => VS
   WW => WS
   RHOP => RHOS
   ZZP => ZZS
ELSE
   UU => U
   VV => V
   WW => W
   RHOP => RHO
   ZZP => ZZ
ENDIF

!N_VENT=>M%N_VENT
!VENTS=>M%VENTS

!Print*, 'ch2'

VENT_LOOP: DO N=1,N_VENT
   !VT => NM%VENTS(N)
!   Print*, N, 'ch3'
   
   IF (VENTS(N)%BOUNDARY_TYPE==OPEN_BOUNDARY) THEN
!   	Print*,N,  'ch4'
   	Print*, VENTS(N)%IOR
   	IF (VENTS(N)%IOR==1) THEN
   		
   		Print*, N,'found xmin - E vent ',T
   IMIN = 0
   IMAX = IBAR
   JMIN = 0
   JMAX = JBAR
   KMIN = 0
   KMAX = KBAR
 status=nf90_open(OBFile, nf90_nowrite, ncid)
 
 status=nf90_inq_varid(ncid, 'UE', varid1)
 status=nf90_inq_varid(ncid, 'VE', varid2)
 status=nf90_inq_varid(ncid, 'WE', varid3)
                                          
 status=nf90_get_var(ncid, varid1, UE,start = (/ 1, 1,int(T) /),  count = (/ IBP1, JBP1, 1 /) ) 
 status=nf90_get_var(ncid, varid2, VE,start = (/ 1, 1,int(T) /),  count = (/ IBP1, JBP1, 1 /) ) 
 status=nf90_get_var(ncid, varid3, WE,start = (/ 1, 1,int(T) /),  count = (/ IBP1, JBP1, 1 /) ) 



    ENDIF   	
    
    IF (VENTS(N)%IOR==-1) THEN
   		
   		Print*, N,'found xmax - W vent '

    ENDIF
   	
   	
   	IF (VENTS(N)%IOR==2) THEN
   		
   		Print*, N,'found ymin South vent '

    ENDIF
    
   	IF (VENTS(N)%IOR==-2) THEN
   		
   		Print*, N,'found ymax North vent '

    ENDIF    
    
   ENDIF 


ENDDO VENT_LOOP

T_USED(4)=T_USED(4)+CURRENT_TIME()-T_NOW

! Print*, 'Finished velocity bc'

END SUBROUTINE VELOCITY_BC_COUPLED