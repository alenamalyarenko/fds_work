!> \brief Assert tangential velocity boundary conditions
!> \param T Current time (s)
!> \param NM Mesh number
!> \param APPLY_TO_ESTIMATED_VARIABLES Flag indicating that estimated (starred) variables are to be used




! included in velo.f90
SUBROUTINE VELOCITY_BC_COUPLED(T,NM, APPLY_TO_ESTIMATED_VARIABLES)

USE MESH_POINTERS
USE MATH_FUNCTIONS, ONLY: EVALUATE_RAMP
USE TURBULENCE, ONLY: WALL_MODEL
USE PHYSICAL_FUNCTIONS, ONLY: GET_CONDUCTIVITY,GET_SPECIFIC_HEAT
USE CC_SCALARS, ONLY : CC_VELOCITY_BC,GET_OPENBC_TANGENTIAL_CUTFACE_VEL


!USE COUPLED_FILES
!use netcdf
!USE MISC_FUNCTIONS, ONLY : WRITE_SUMMARY_INFO,VERBOSE_PRINTOUT
USE GLOBAL_CONSTANTS, ONLY: LU_ERR

	
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

TYPE (MESH_TYPE), POINTER :: M
integer:: ncid, varid1,varid2,varid3,varid4, status
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
INTEGER  :: I,J,K,IMIN,IMAX,JMIN,JMAX,KMIN,KMAX

integer:: ncid_out,varid_vs, varid_ws1,varid_ws2
integer::  dimx,dimy,dimz
	
!real,dimension(22,12,1)::BC_WS1,BC_WS2,BC_VS
	
	
IF (SOLID_PHASE_ONLY) RETURN
IF (PERIODIC_TEST==12) RETURN
IF (PERIODIC_TEST==13) RETURN
		
T_NOW = CURRENT_TIME()

! Assign local names to variables

CALL POINT_TO_MESH(NM)

!ALLOCATE(BC_WS1(IBAR+2,KBAR+2,1))
!ALLOCATE(BC_WS2(IBAR+2,KBAR+2,1))
!ALLOCATE(BC_VS(IBAR+2,KBAR+2,1))
!ALLOCATE(TSS(IBAR+2,KBAR+2,1))

!ALLOCATE(BC_WN1(IBAR+2,KBAR+2,1))
!ALLOCATE(BC_WN2(IBAR+2,KBAR+2,1))
!ALLOCATE(BC_VN(IBAR+2,KBAR+2,1))
!ALLOCATE(TNN(IBAR+2,KBAR+2,1))


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


VENT_LOOP: DO N=1,N_VENT
    VT => VENTS(N)
    COUPLED_VENT_IF: IF (VT%N_EDDY==(-1)) THEN !coupled vent

    !figure out orientation
    
   	IF (VENTS(N)%IOR==2) THEN
   		  ! IF (VERBOSE) CALL VERBOSE_PRINTOUT(NM, N,'found ymin South vent ',  VT%I1 ,  VT%I2 ,VT%J1, VT%J2 , VT%K1 ,VT%K2 )
   		    IF (VERBOSE) THEN
   		    	! WRITE(LU_ERR,*) NM, N,'found ymin South vent ',  VT%I1 ,  VT%I2 ,VT%J1, VT%J2 , VT%K1 ,VT%K2
   		    	!Print*, 'testprint'
   		    ENDIF 	
   		
   		
   		! W(0) W(1) V(0) ; (IBP2,KBP2,NT); 1:62,1:22,3000

   


!status=nf90_open(OBFile, nf90_nowrite, ncid)
!status=nf90_inq_varid(ncid, 'WS1', varid1) 
!status=nf90_inq_varid(ncid, 'WS2', varid2) 
!status=nf90_inq_varid(ncid, 'VS', varid3)  
!status=nf90_inq_varid(ncid, 'TS', varid4)  
 
                                          
! x z time            
! to assign to 0:IBAR we read 1:IBP1 ;                               
!status=nf90_get_var(ncid, varid1, BC_WS1,start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
!status=nf90_get_var(ncid, varid2, BC_WS2,start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
!status=nf90_get_var(ncid, varid3, BC_VS, start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
!status=nf90_get_var(ncid, varid4, TS, start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
!status = nf90_close(ncid) 


!    do i= VT%I1, VT%I2  !0 - 21
!    	do k=VT%K1 ,VT%K2 ! 0 - 11
!    	      !BC_VS has values in 1-22, 1-12 so everything is +1
!    	     ! print*, 'coordinates',i,k, 'value',BC_VS(i+1,k+1,1)
!    	      VV(i,VT%J1,k)  =BC_VS (i+1,k+1,1)     !0
!    	      WW(i,VT%J1,k)  =BC_WS1(i+1,k+1,1)     !0
!     	      WW(i,VT%J1+1,k)=BC_WS1(i+1,k+1,1)     !1
!    	enddo
!    enddo


    ENDIF !south vent
    
    
    
   	IF (VENTS(N)%IOR==-2) THEN
     !IF (VERBOSE) CALL VERBOSE_PRINTOUT(NM, N,'found ymax North vent ',  VT%I1 ,  VT%I2 ,VT%J1, VT%J2 , VT%K1 ,VT%K2 )

! status=nf90_open(OBFile, nf90_nowrite, ncid)
! status=nf90_inq_varid(ncid, 'WN1', varid1) 
! status=nf90_inq_varid(ncid, 'WN2', varid2) 
! status=nf90_inq_varid(ncid, 'VN', varid3)  
! status=nf90_inq_varid(ncid, 'TN', varid4)  
 
                                          
! x z time            
! to assign to 0:IBAR we read 1:IBP1 ;                               
! status=nf90_get_var(ncid, varid1, BC_WN1,start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
! status=nf90_get_var(ncid, varid2, BC_WN2,start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
! status=nf90_get_var(ncid, varid3, BC_VN, start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
! !status=nf90_get_var(ncid, varid4, TN, start = (/ 1, 1,1 /),  count = (/ 22, 12, 1 /) ) 
! status = nf90_close(ncid) 


!   do i= VT%I1, VT%I2  !0 - 21
!   	do k=VT%K1 ,VT%K2 ! 0 - 11
!   	      !BC_VS has values in 1-22, 1-12 so everything is +1
!   	     ! print*, 'coordinates',i,k, 'value',BC_VS(i+1,k+1,1)
!   	      VV(i,VT%J1,k)  =BC_VN (i+1,k+1,1)     !0
!   	      WW(i,VT%J1,k)  =BC_WN1(i+1,k+1,1)     !0
!    	      WW(i,VT%J1+1,k)=BC_WN1(i+1,k+1,1)     !1
!   	enddo
!   enddo


    ENDIF     
    
    
     
   	IF (VENTS(N)%IOR==1) THEN
         !IF (VERBOSE) CALL VERBOSE_PRINTOUT( NM, N,'found xmin - W vent ',     VT%I1 ,  VT%I2 ,VT%J1, VT%J2 , VT%K1 ,VT%K2 )
    ENDIF   	    
    IF (VENTS(N)%IOR==-1) THEN
        !IF (VERBOSE) CALL VERBOSE_PRINTOUT( NM, N,'found xmax - E vent ',     VT%I1 ,  VT%I2 ,VT%J1, VT%J2 , VT%K1 ,VT%K2 )
    ENDIF    
  
    
   ENDIF COUPLED_VENT_IF


ENDDO VENT_LOOP

!what I think is happening to south boundary in edge loop:
	
! Define the appropriate gas and ghost velocity
!WW(0)= VEL_GHOST - we have done that

!Loop over all 4 normal directions and compute vorticity and stress tensor components for each ??
!ED%OMEGA(ICD_SGN)  ?
!ED%TAU(ICD_SGN)  ?



T_USED(4)=T_USED(4)+CURRENT_TIME()-T_NOW

!IF(CC_IBM) CALL CC_VELOCITY_BC(T,NM,APPLY_TO_ESTIMATED_VARIABLES,DO_IBEDGES=.TRUE.)


!DEALLOCATE(BC_WS1)
!DEALLOCATE(BC_WS2)
!DEALLOCATE(BC_VS)

!DEALLOCATE(BC_WN1)
!DEALLOCATE(BC_WN2)
!DEALLOCATE(BC_VN)

END SUBROUTINE VELOCITY_BC_COUPLED
