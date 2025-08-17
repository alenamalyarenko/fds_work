
!> \brief Assert tangential velocity boundary conditions
!> \param T Current time (s)
!> \param NM Mesh number
!> \param APPLY_TO_ESTIMATED_VARIABLES Flag indicating that estimated (starred) variables are to be used

SUBROUTINE VELOCITY_BC(T,NM,APPLY_TO_ESTIMATED_VARIABLES)

USE MATH_FUNCTIONS, ONLY: EVALUATE_RAMP
USE TURBULENCE, ONLY: WALL_MODEL
USE PHYSICAL_FUNCTIONS, ONLY: GET_CONDUCTIVITY,GET_SPECIFIC_HEAT
USE CC_SCALARS, ONLY : CC_VELOCITY_BC,GET_OPENBC_TANGENTIAL_CUTFACE_VEL

REAL(EB), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NM
LOGICAL, INTENT(IN) :: APPLY_TO_ESTIMATED_VARIABLES
REAL(EB) :: MUA,TSI,WGT,T_NOW,RAMP_T,OMW,MU_WALL,RHO_WALL,SLIP_COEF,VEL_T, &
            UUP(2),UUM(2),DXX(2),MU_DUIDXJ(-2:2),DUIDXJ(-2:2),PROFILE_FACTOR,VEL_GAS,VEL_GHOST, &
            MU_DUIDXJ_USE(2),DUIDXJ_USE(2),VEL_EDDY,U_TAU,Y_PLUS,U_NORM,U_WIND_LOC,V_WIND_LOC,W_WIND_LOC
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
TYPE(SURFACE_TYPE), POINTER :: SF

IF (SOLID_PHASE_ONLY) RETURN
IF (PERIODIC_TEST==12) RETURN
IF (PERIODIC_TEST==13) RETURN

T_NOW = CURRENT_TIME()

! Assign local names to variables

CALL POINT_TO_MESH(NM)

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



! Loop over all cell edges and determine the appropriate velocity BCs

EDGE_LOOP: DO IE=1,EDGE_COUNT(NM)

#if defined coupled_debug
!Print*, 'Start of velocity_bc', NM, 'Edge ', IE
#endif

   ED => EDGE(IE)

   ED%OMEGA    = -1.E6_EB
   ED%TAU      = -1.E6_EB
   ED%U_AVG    = -1.E6_EB
   ED%V_AVG    = -1.E6_EB
   ED%W_AVG    = -1.E6_EB
   INTERPOLATED_EDGE = .FALSE.

   ! Throw out edges that are completely surrounded by blockages or the exterior of the domain

   ICMM = ED%CELL_INDEX_MM
   ICPM = ED%CELL_INDEX_PM
   ICMP = ED%CELL_INDEX_MP
   ICPP = ED%CELL_INDEX_PP

   IF ((CELL(ICMM)%EXTERIOR .OR. CELL(ICMM)%SOLID) .AND. &
       (CELL(ICPM)%EXTERIOR .OR. CELL(ICPM)%SOLID) .AND. &
       (CELL(ICMP)%EXTERIOR .OR. CELL(ICMP)%SOLID) .AND. &
       (CELL(ICPP)%EXTERIOR .OR. CELL(ICPP)%SOLID)) CYCLE EDGE_LOOP

   ! Unpack indices for the edge

   II     = ED%I
   JJ     = ED%J
   KK     = ED%K
   IEC    = ED%AXIS
   NOM(1) = ED%NOM_1
   IIO(1) = ED%IIO_1
   JJO(1) = ED%JJO_1
   KKO(1) = ED%KKO_1
   NOM(2) = ED%NOM_2
   IIO(2) = ED%IIO_2
   JJO(2) = ED%JJO_2
   KKO(2) = ED%KKO_2

   ! Get the velocity components at the appropriate cell faces

   COMPONENT: SELECT CASE(IEC)
      CASE(1) COMPONENT
         UUP(1)  = VV(II,JJ,KK+1)
         UUM(1)  = VV(II,JJ,KK)
         UUP(2)  = WW(II,JJ+1,KK)
         UUM(2)  = WW(II,JJ,KK)
         DXX(1)  = DY(JJ)
         DXX(2)  = DZ(KK)
      CASE(2) COMPONENT
         UUP(1)  = WW(II+1,JJ,KK)
         UUM(1)  = WW(II,JJ,KK)
         UUP(2)  = UU(II,JJ,KK+1)
         UUM(2)  = UU(II,JJ,KK)
         DXX(1)  = DZ(KK)
         DXX(2)  = DX(II)
      CASE(3) COMPONENT
         UUP(1)  = UU(II,JJ+1,KK)
         UUM(1)  = UU(II,JJ,KK)
         UUP(2)  = VV(II+1,JJ,KK)
         UUM(2)  = VV(II,JJ,KK)
         DXX(1)  = DX(II)
         DXX(2)  = DY(JJ)
   END SELECT COMPONENT

   ! Indicate that the velocity gradients in the two orthogonal directions have not been changed yet

   ALTERED_GRADIENT = .FALSE.

   ! Loop over all possible orientations of edge and reassign velocity gradients if appropriate

   SIGN_LOOP: DO I_SGN=-1,1,2
      ORIENTATION_LOOP: DO IS=1,3

         IF (IS==IEC) CYCLE ORIENTATION_LOOP

         ! IOR is the orientation of the wall cells adjacent to the edge

         IOR = I_SGN*IS

         ! IS2 is the other coordinate direction besides IOR.

         SELECT CASE(IEC)
            CASE(1)
               IF (IS==2) IS2 = 3
               IF (IS==3) IS2 = 2
            CASE(2)
               IF (IS==1) IS2 = 3
               IF (IS==3) IS2 = 1
            CASE(3)
               IF (IS==1) IS2 = 2
               IF (IS==2) IS2 = 1
            END SELECT

         ! Determine Index_Coordinate_Direction
         ! IEC=1, ICD=1 refers to DWDY; ICD=2 refers to DVDZ
         ! IEC=2, ICD=1 refers to DUDZ; ICD=2 refers to DWDX
         ! IEC=3, ICD=1 refers to DVDX; ICD=2 refers to DUDY

         IF (IS>IEC) ICD = IS-IEC
         IF (IS<IEC) ICD = IS-IEC+3
         ICD_SGN = I_SGN * ICD

         ! IWM and IWP are the wall cell indices of the boundary on either side of the edge.

         IF (IOR<0) THEN
            IWM  = CELL(ICMM)%WALL_INDEX(-IOR)
            IWMI = CELL(ICMM)%WALL_INDEX( IS2)
            IF (ICD==1) THEN
               IWP  = CELL(ICMP)%WALL_INDEX(-IOR)
               IWPI = CELL(ICMP)%WALL_INDEX(-IS2)
            ELSE ! ICD==2
               IWP  = CELL(ICPM)%WALL_INDEX(-IOR)
               IWPI = CELL(ICPM)%WALL_INDEX(-IS2)
            ENDIF
         ELSE
            IF (ICD==1) THEN
               IWM  = CELL(ICPM)%WALL_INDEX(-IOR)
               IWMI = CELL(ICPM)%WALL_INDEX( IS2)
            ELSE ! ICD==2
               IWM  = CELL(ICMP)%WALL_INDEX(-IOR)
               IWMI = CELL(ICMP)%WALL_INDEX( IS2)
            ENDIF
            IWP  = CELL(ICPP)%WALL_INDEX(-IOR)
            IWPI = CELL(ICPP)%WALL_INDEX(-IS2)
         ENDIF

         ! If both adjacent wall cells are undefined, cycle out of the loop.

         IF (IWM==0 .AND. IWP==0) CYCLE ORIENTATION_LOOP

         ! If there is a solid wall separating the two adjacent wall cells, cycle out of the loop.

         IF ((WALL(IWMI)%BOUNDARY_TYPE==SOLID_BOUNDARY .AND. SURFACE(WALL(IWM)%SURF_INDEX)%VELOCITY_BC_INDEX/=FREE_SLIP_BC) .OR. &
             (WALL(IWPI)%BOUNDARY_TYPE==SOLID_BOUNDARY .AND. SURFACE(WALL(IWP)%SURF_INDEX)%VELOCITY_BC_INDEX/=FREE_SLIP_BC)) &
            CYCLE ORIENTATION_LOOP

         ! If only one adjacent wall cell is defined, use its properties.

         IF (IWM>0) THEN
            WCM => WALL(IWM)
         ELSE
            WCM => WALL(IWP)
         ENDIF

         IF (IWP>0) THEN
            WCP => WALL(IWP)
         ELSE
            WCP => WALL(IWM)
         ENDIF

         WCM_B1 => BOUNDARY_PROP1(WCM%B1_INDEX)
         WCP_B1 => BOUNDARY_PROP1(WCP%B1_INDEX)

         ! If both adjacent wall cells are NULL, cycle out.

         BOUNDARY_TYPE_M = WCM%BOUNDARY_TYPE
         BOUNDARY_TYPE_P = WCP%BOUNDARY_TYPE

         IF (BOUNDARY_TYPE_M==NULL_BOUNDARY .AND. BOUNDARY_TYPE_P==NULL_BOUNDARY) CYCLE ORIENTATION_LOOP

         ! Set up synthetic eddy method

         SYNTHETIC_EDDY_METHOD = .FALSE.
#ifdef coupled_bc         
         COUPLED_ATM_BOUNDARY=.FALSE. 
#endif         
!print*, 'SEM condition 0',NM, II,JJ,KK, IEC         
!print*, 'SEM condition 1', IWM, IWP         
         IF (IWM>0 .AND. IWP>0) THEN
!print*, 'SEM condition 2', WCM%VENT_INDEX, WCP%VENT_INDEX        	
            IF (WCM%VENT_INDEX==WCP%VENT_INDEX) THEN
               IF (WCM%VENT_INDEX>0) THEN
                  VT=>VENTS(WCM%VENT_INDEX)
                  IF (VT%N_EDDY>0) SYNTHETIC_EDDY_METHOD=.TRUE.  
#ifdef coupled_bc                  
                  IF (VT%N_EDDY<0) COUPLED_ATM_BOUNDARY=.TRUE. 
                  !Print*, 'Coupled BC activated'	               
#endif                  	
               ENDIF
            ENDIF
         ENDIF

        
       

         VEL_EDDY = 0._EB
         SYNTHETIC_EDDY_IF_1: IF (SYNTHETIC_EDDY_METHOD) THEN
            U_WIND_LOC = 0._EB
            V_WIND_LOC = 0._EB
            W_WIND_LOC = 0._EB
            IS_SELECT_1: SELECT CASE(IS) ! unsigned vent orientation
               CASE(1) ! yz plane
                  SELECT CASE(IEC) ! edge orientation
                     CASE(2)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           U_WIND_LOC = 0.5_EB*(U_WIND(KK)+U_WIND(KK+1))
                           W_WIND_LOC = 0.5_EB*(W_WIND(KK)+W_WIND(KK+1))
                        ENDIF
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%U_EDDY(JJ,KK)+VT%U_EDDY(JJ,KK+1)) + U_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%W_EDDY(JJ,KK)+VT%W_EDDY(JJ,KK+1)) + W_WIND_LOC
                     CASE(3)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           V_WIND_LOC = V_WIND(KK)
                           U_WIND_LOC = U_WIND(KK)
                        ENDIF
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%V_EDDY(JJ,KK)+VT%V_EDDY(JJ+1,KK)) + V_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%U_EDDY(JJ,KK)+VT%U_EDDY(JJ+1,KK)) + U_WIND_LOC
                  END SELECT
               CASE(2) ! zx plane
                  SELECT CASE(IEC)
                     CASE(3)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           V_WIND_LOC = V_WIND(KK)
                           U_WIND_LOC = U_WIND(KK)
                        ENDIF
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%V_EDDY(II,KK)+VT%V_EDDY(II+1,KK)) + V_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%U_EDDY(II,KK)+VT%U_EDDY(II+1,KK)) + U_WIND_LOC
                     CASE(1)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           W_WIND_LOC = 0.5_EB*(W_WIND(KK)+W_WIND(KK+1))
                           V_WIND_LOC = 0.5_EB*(V_WIND(KK)+V_WIND(KK+1))
                        ENDIF
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%W_EDDY(II,KK)+VT%W_EDDY(II,KK+1)) + W_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%V_EDDY(II,KK)+VT%V_EDDY(II,KK+1)) + V_WIND_LOC
                  END SELECT
               CASE(3) ! xy plane
                  SELECT CASE(IEC)
                     CASE(1)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           W_WIND_LOC = W_WIND(KK)
                           V_WIND_LOC = V_WIND(KK)
                        ENDIF
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%W_EDDY(II,JJ)+VT%W_EDDY(II,JJ+1)) + W_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%V_EDDY(II,JJ)+VT%V_EDDY(II,JJ+1)) + V_WIND_LOC
                     CASE(2)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           U_WIND_LOC = U_WIND(KK)
                           W_WIND_LOC = W_WIND(KK)
                        ENDIF
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%U_EDDY(II,JJ)+VT%U_EDDY(II+1,JJ)) + U_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%W_EDDY(II,JJ)+VT%W_EDDY(II+1,JJ)) + W_WIND_LOC
                  END SELECT
            END SELECT IS_SELECT_1
         ENDIF SYNTHETIC_EDDY_IF_1
         
#if defined atm_variables
Print*, 'start of _atm in velocity_bc 1', NM, VEL_EDDY
Print*, 'start of _atm in velocity_bc 2', VT%N_EDDY

         ! IF (COUPLED_ATM_BOUNDARY) THEN
          IF (VT%N_EDDY<0) THEN	
            IS_SELECT_2: SELECT CASE(IS) ! unsigned vent orientation
               CASE(1) ! yz plane
                  SELECT CASE(IEC) ! edge orientation
                     CASE(2) 
                        print*, 'check 1 - velocity BC east?'
                        IF (OPEN_WIND_BOUNDARY) THEN
                           U_WIND_LOC = 0.5_EB*(U_WIND(KK)+U_WIND(KK+1))
                           W_WIND_LOC = 0.5_EB*(W_WIND(KK)+W_WIND(KK+1))
                        ENDIF
                        
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%UE_ATM(JJ,KK)+VT%UE_ATM(JJ,KK+1)) + U_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%WE_ATM(JJ,KK)+VT%WE_ATM(JJ,KK+1)) + W_WIND_LOC
                     CASE(3)   
                     print*, 'check 2  - velocity BC west?'
                        IF (OPEN_WIND_BOUNDARY) THEN
                           V_WIND_LOC = V_WIND(KK)
                           U_WIND_LOC = U_WIND(KK)
                        ENDIF   
                        
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%VW_ATM(JJ,KK)+VT%VW_ATM(JJ+1,KK)) + V_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%UW_ATM(JJ,KK)+VT%UW_ATM(JJ+1,KK)) + U_WIND_LOC
                  END SELECT
               CASE(2) ! zx plane
                  SELECT CASE(IEC)
                     CASE(3)      ! this loop for south coupled border 
                      !print*, 'check 3', VT%V_ATM(II,KK), VT%U_ATM(II,KK)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           V_WIND_LOC = V_WIND(KK)
                           U_WIND_LOC = U_WIND(KK)
                        ENDIF      
                       
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%VS_ATM(II,KK)+VT%VS_ATM(II+1,KK)) + V_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%US_ATM(II,KK)+VT%US_ATM(II+1,KK)) + U_WIND_LOC
                     CASE(1)   
                     !print*, 'check 4',VT%W_ATM(II,KK), VT%V_ATM(II,KK)
                        IF (OPEN_WIND_BOUNDARY) THEN
                           W_WIND_LOC = 0.5_EB*(W_WIND(KK)+W_WIND(KK+1))
                           V_WIND_LOC = 0.5_EB*(V_WIND(KK)+V_WIND(KK+1))
                        ENDIF      
                        
                        IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%WN_ATM(II,KK)+VT%WN_ATM(II,KK+1)) + W_WIND_LOC
                        IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%VN_ATM(II,KK)+VT%VN_ATM(II,KK+1)) + V_WIND_LOC
                  END SELECT
               CASE(3) ! xy plane
                  SELECT CASE(IEC)
                     CASE(1)   
                        IF (OPEN_WIND_BOUNDARY) THEN
                           W_WIND_LOC = W_WIND(KK)
                           V_WIND_LOC = V_WIND(KK)
                        ENDIF   
                        !IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%W_ATM(II,JJ)+VT%W_ATM(II,JJ+1)) + W_WIND_LOC
                        !IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%V_ATM(II,JJ)+VT%V_ATM(II,JJ+1)) + V_WIND_LOC
                     CASE(2)    
                        IF (OPEN_WIND_BOUNDARY) THEN
                           U_WIND_LOC = U_WIND(KK)
                           W_WIND_LOC = W_WIND(KK)
                        ENDIF  
                        !IF (ICD==1) VEL_EDDY = 0.5_EB*(VT%U_ATM(II,JJ)+VT%U_ATM(II+1,JJ)) + U_WIND_LOC    
                        !IF (ICD==2) VEL_EDDY = 0.5_EB*(VT%W_ATM(II,JJ)+VT%W_ATM(II+1,JJ)) + W_WIND_LOC        
                  END SELECT
            END SELECT IS_SELECT_2
            Print*, 'end of _atm in velocity_bc', NM,  VEL_EDDY
         ENDIF

#endif         
         

         ! OPEN boundary conditions, both varieties, with and without a wind

         OPEN_AND_WIND_BC: IF ((IWM==0 .OR. WALL(IWM)%BOUNDARY_TYPE==OPEN_BOUNDARY) .AND. &
                               (IWP==0 .OR. WALL(IWP)%BOUNDARY_TYPE==OPEN_BOUNDARY) .AND. .NOT.SYNTHETIC_EDDY_METHOD) THEN

            VENT_INDEX = MAX(WCM%VENT_INDEX,WCP%VENT_INDEX)
            VT => VENTS(VENT_INDEX)
            UPWIND_BOUNDARY = .FALSE.
            INFLOW_BOUNDARY = .FALSE.

            IF (OPEN_WIND_BOUNDARY) THEN
               SELECT CASE(IEC)
                  CASE(1)
                     IF (JJ==0    .AND. IOR== 2) U_NORM = 0.5_EB*(VV(II,   0,KK) + VV(II,   0,KK+1))
                     IF (JJ==JBAR .AND. IOR==-2) U_NORM = 0.5_EB*(VV(II,JBAR,KK) + VV(II,JBAR,KK+1))
                     IF (KK==0    .AND. IOR== 3) U_NORM = 0.5_EB*(WW(II,JJ,0)    + WW(II,JJ+1,   0))
                     IF (KK==KBAR .AND. IOR==-3) U_NORM = 0.5_EB*(WW(II,JJ,KBAR) + WW(II,JJ+1,KBAR))
                  CASE(2)
                     IF (II==0    .AND. IOR== 1) U_NORM = 0.5_EB*(UU(   0,JJ,KK) + UU(   0,JJ,KK+1))
                     IF (II==IBAR .AND. IOR==-1) U_NORM = 0.5_EB*(UU(IBAR,JJ,KK) + UU(IBAR,JJ,KK+1))
                     IF (KK==0    .AND. IOR== 3) U_NORM = 0.5_EB*(WW(II,JJ,   0) + WW(II+1,JJ,   0))
                     IF (KK==KBAR .AND. IOR==-3) U_NORM = 0.5_EB*(WW(II,JJ,KBAR) + WW(II+1,JJ,KBAR))
                  CASE(3)
                     IF (II==0    .AND. IOR== 1) U_NORM = 0.5_EB*(UU(   0,JJ,KK) + UU(   0,JJ+1,KK))
                     IF (II==IBAR .AND. IOR==-1) U_NORM = 0.5_EB*(UU(IBAR,JJ,KK) + UU(IBAR,JJ+1,KK))
                     IF (JJ==0    .AND. IOR== 2) U_NORM = 0.5_EB*(VV(II,   0,KK) + VV(II+1,   0,KK))
                     IF (JJ==JBAR .AND. IOR==-2) U_NORM = 0.5_EB*(VV(II,JBAR,KK) + VV(II+1,JBAR,KK))
               END SELECT
               IF ((IOR==1.AND.U_WIND(KK)>=0._EB) .OR. (IOR==-1.AND.U_WIND(KK)<=0._EB)) UPWIND_BOUNDARY = .TRUE.
               IF ((IOR==2.AND.V_WIND(KK)>=0._EB) .OR. (IOR==-2.AND.V_WIND(KK)<=0._EB)) UPWIND_BOUNDARY = .TRUE.
               IF ((IOR==3.AND.W_WIND(KK)>=0._EB) .OR. (IOR==-3.AND.W_WIND(KK)<=0._EB)) UPWIND_BOUNDARY = .TRUE.
               IF ((IOR==1.AND.U_NORM>=0._EB) .OR. (IOR==-1.AND.U_NORM<=0._EB)) INFLOW_BOUNDARY = .TRUE.
               IF ((IOR==2.AND.U_NORM>=0._EB) .OR. (IOR==-2.AND.U_NORM<=0._EB)) INFLOW_BOUNDARY = .TRUE.
               IF ((IOR==3.AND.U_NORM>=0._EB) .OR. (IOR==-3.AND.U_NORM<=0._EB)) INFLOW_BOUNDARY = .TRUE.
            ENDIF

            WIND_NO_WIND_IF: IF (.NOT.UPWIND_BOUNDARY .OR. .NOT.INFLOW_BOUNDARY) THEN  ! For regular OPEN boundary, (free-slip) BCs

               SELECT CASE(IEC)
                  CASE(1)
                     IF (JJ==0    .AND. IOR== 2) WW(II,0,KK)    = WW(II,1,KK)
                     IF (JJ==JBAR .AND. IOR==-2) WW(II,JBP1,KK) = WW(II,JBAR,KK)
                     IF (KK==0    .AND. IOR== 3) VV(II,JJ,0)    = VV(II,JJ,1)
                     IF (KK==KBAR .AND. IOR==-3) VV(II,JJ,KBP1) = VV(II,JJ,KBAR)
                  CASE(2)
                     IF (II==0    .AND. IOR== 1) WW(0,JJ,KK)    = WW(1,JJ,KK)
                     IF (II==IBAR .AND. IOR==-1) WW(IBP1,JJ,KK) = WW(IBAR,JJ,KK)
                     IF (KK==0    .AND. IOR== 3) UU(II,JJ,0)    = UU(II,JJ,1)
                     IF (KK==KBAR .AND. IOR==-3) UU(II,JJ,KBP1) = UU(II,JJ,KBAR)
                  CASE(3)
                     IF (II==0    .AND. IOR== 1) VV(0,JJ,KK)    = VV(1,JJ,KK)
                     IF (II==IBAR .AND. IOR==-1) VV(IBP1,JJ,KK) = VV(IBAR,JJ,KK)
                     IF (JJ==0    .AND. IOR== 2) UU(II,0,KK)    = UU(II,1,KK)
                     IF (JJ==JBAR .AND. IOR==-2) UU(II,JBP1,KK) = UU(II,JBAR,KK)
               END SELECT

            ELSE WIND_NO_WIND_IF  ! For upwind, inflow boundaries, use the specified wind field for tangential velocity components

               SELECT CASE(IEC)
                  CASE(1)      
                     IF (JJ==0    .AND. IOR== 2) WW(II,0,KK)    = W_WIND(KK)
                     IF (JJ==JBAR .AND. IOR==-2) WW(II,JBP1,KK) = W_WIND(KK)
                     IF (KK==0    .AND. IOR== 3) VV(II,JJ,0)    = V_WIND(KK)
                     IF (KK==KBAR .AND. IOR==-3) VV(II,JJ,KBP1) = V_WIND(KK)
                
                  CASE(2)
                     IF (II==0    .AND. IOR== 1) WW(0,JJ,KK)    = W_WIND(KK)
                     IF (II==IBAR .AND. IOR==-1) WW(IBP1,JJ,KK) = W_WIND(KK)
                     IF (KK==0    .AND. IOR== 3) UU(II,JJ,0)    = U_WIND(KK)
                     IF (KK==KBAR .AND. IOR==-3) UU(II,JJ,KBP1) = U_WIND(KK)
                  CASE(3)
                     IF (II==0    .AND. IOR== 1) VV(0,JJ,KK)    = V_WIND(KK)
                     IF (II==IBAR .AND. IOR==-1) VV(IBP1,JJ,KK) = V_WIND(KK)
                     IF (JJ==0    .AND. IOR== 2) UU(II,0,KK)    = U_WIND(KK)
                     IF (JJ==JBAR .AND. IOR==-2) UU(II,JBP1,KK) = U_WIND(KK)
               END SELECT

            ENDIF WIND_NO_WIND_IF

            IF (CC_IBM) CALL GET_OPENBC_TANGENTIAL_CUTFACE_VEL(APPLY_TO_ESTIMATED_VARIABLES,UPWIND_BOUNDARY,&
                                                               INFLOW_BOUNDARY,IEC,II,JJ,KK,IOR,UU,VV,WW)

            IF (IWM/=0 .AND. IWP/=0) THEN
               CYCLE EDGE_LOOP  ! Do no further processing of this edge if both cell faces are OPEN
            ELSE
               CYCLE ORIENTATION_LOOP
            ENDIF

         ENDIF OPEN_AND_WIND_BC

#if defined coupled_debug         
!Print*, 'end of open_and_wind_bc',  NM
#endif
         ! Define the appropriate gas and ghost velocity

         IF (ICD==1) THEN ! Used to pick the appropriate velocity component
            IVL=2
         ELSE !ICD==2
            IVL=1
         ENDIF

         IF (IOR<0) THEN
            VEL_GAS   = UUM(IVL)
            VEL_GHOST = UUP(IVL)
            IIGM = CELL(ICMM)%I
            JJGM = CELL(ICMM)%J
            KKGM = CELL(ICMM)%K
            IF (ICD==1) THEN
               IIGP = CELL(ICMP)%I
               JJGP = CELL(ICMP)%J
               KKGP = CELL(ICMP)%K
            ELSE ! ICD==2
               IIGP = CELL(ICPM)%I
               JJGP = CELL(ICPM)%J
               KKGP = CELL(ICPM)%K
            ENDIF
         ELSE
            VEL_GAS   = UUP(IVL)
            VEL_GHOST = UUM(IVL)
            IF (ICD==1) THEN
               IIGM = CELL(ICPM)%I
               JJGM = CELL(ICPM)%J
               KKGM = CELL(ICPM)%K
            ELSE ! ICD==2
               IIGM = CELL(ICMP)%I
               JJGM = CELL(ICMP)%J
               KKGM = CELL(ICMP)%K
            ENDIF
            IIGP = CELL(ICPP)%I
            JJGP = CELL(ICPP)%J
            KKGP = CELL(ICPP)%K
         ENDIF

         ! Decide whether or not to process edge using data interpolated from another mesh

         INTERPOLATION_IF: IF (NOM(ICD)==0 .OR. &
                   (BOUNDARY_TYPE_M==SOLID_BOUNDARY .OR. BOUNDARY_TYPE_P==SOLID_BOUNDARY) .OR. &
                   (BOUNDARY_TYPE_M/=INTERPOLATED_BOUNDARY .AND. BOUNDARY_TYPE_P/=INTERPOLATED_BOUNDARY) .OR. &
                   (SYNTHETIC_EDDY_METHOD .AND. (BOUNDARY_TYPE_M==OPEN_BOUNDARY .OR. BOUNDARY_TYPE_P==OPEN_BOUNDARY)) ) THEN

            ! Determine appropriate velocity BC by assessing each adjacent wall cell. If the BCs are different on each
            ! side of the edge, choose the one with the specified velocity or velocity gradient, if there is one.
            ! If not, choose the max value of boundary condition index, simply for consistency.

            SURF_INDEXM = WCM%SURF_INDEX
            SURF_INDEXP = WCP%SURF_INDEX
            IF (SURFACE(SURF_INDEXM)%SPECIFIED_NORMAL_VELOCITY .OR. SURFACE(SURF_INDEXM)%SPECIFIED_NORMAL_GRADIENT) THEN
               SF=>SURFACE(SURF_INDEXM)
            ELSEIF (SURFACE(SURF_INDEXP)%SPECIFIED_NORMAL_VELOCITY .OR. SURFACE(SURF_INDEXP)%SPECIFIED_NORMAL_GRADIENT) THEN
               SF=>SURFACE(SURF_INDEXP)
            ELSE
               SF=>SURFACE(MAX(SURF_INDEXM,SURF_INDEXP))
            ENDIF
            VELOCITY_BC_INDEX = SF%VELOCITY_BC_INDEX
            IF (WCM%VENT_INDEX==WCP%VENT_INDEX .AND. WCP%VENT_INDEX > 0) THEN
               IF(VENTS(WCM%VENT_INDEX)%NODE_INDEX>0 .AND. WCM_B1%U_NORMAL >= 0._EB) VELOCITY_BC_INDEX=FREE_SLIP_BC
            ENDIF
            IF (SYNTHETIC_EDDY_METHOD) VELOCITY_BC_INDEX=NO_SLIP_BC

            ! Compute the viscosity by averaging the two adjacent gas cells

            MUA = 0.5_EB*(MU(IIGM,JJGM,KKGM) + MU(IIGP,JJGP,KKGP))

            ! Check for HVAC tangential velocity

            HVAC_TANGENTIAL = .FALSE.
            IF (WCM%VENT_INDEX>0 .OR. WCP%VENT_INDEX>0) THEN
               IF (WCM%VENT_INDEX>0) THEN
                  WCX => WCM
               ELSE
                  WCX => WCP
               ENDIF
               VT => VENTS(WCX%VENT_INDEX)
               WCX_B1 => BOUNDARY_PROP1(WCX%B1_INDEX)
               IF (VT%NODE_INDEX>0 .AND. WCX_B1%U_NORMAL_S<0._EB) THEN
                  VELOCITY_BC_INDEX = NO_SLIP_BC
                  IF (ALL(VT%UVW>-1.E12_EB)) HVAC_TANGENTIAL = .TRUE.  ! User-specified tangential components of velocity
               ENDIF
            ENDIF

            ! Determine if there is a tangential velocity component

            IF (.NOT.SF%SPECIFIED_TANGENTIAL_VELOCITY .AND. .NOT.SYNTHETIC_EDDY_METHOD .AND. .NOT.HVAC_TANGENTIAL) THEN

               VEL_T = 0._EB

            ELSEIF (HVAC_TANGENTIAL) THEN

               VEL_T = 0._EB
               SELECT CASE(IEC) ! edge orientation
                  CASE (1)
                     IF (ICD==1) VEL_T = ABS(WCX_B1%U_NORMAL_S/VT%UVW(ABS(VT%IOR)))*VT%UVW(3)
                     IF (ICD==2) VEL_T = ABS(WCX_B1%U_NORMAL_S/VT%UVW(ABS(VT%IOR)))*VT%UVW(2)
                  CASE (2)
                     IF (ICD==1) VEL_T = ABS(WCX_B1%U_NORMAL_S/VT%UVW(ABS(VT%IOR)))*VT%UVW(1)
                     IF (ICD==2) VEL_T = ABS(WCX_B1%U_NORMAL_S/VT%UVW(ABS(VT%IOR)))*VT%UVW(3)
                  CASE (3)
                     IF (ICD==1) VEL_T = ABS(WCX_B1%U_NORMAL_S/VT%UVW(ABS(VT%IOR)))*VT%UVW(2)
                     IF (ICD==2) VEL_T = ABS(WCX_B1%U_NORMAL_S/VT%UVW(ABS(VT%IOR)))*VT%UVW(1)
               END SELECT

            ELSE

               PROFILE_FACTOR = 1._EB
               IF (ABS(SF%T_IGN-T_BEGIN)<=SPACING(SF%T_IGN) .AND. SF%RAMP(TIME_VELO)%INDEX>=1) THEN
                  TSI = T
               ELSE
                  TSI=T-SF%T_IGN
               ENDIF
               IF (SF%PROFILE/=0 .AND. SF%VEL>TWO_EPSILON_EB) &
                  PROFILE_FACTOR = ABS(0.5_EB*(WCM_B1%U_NORMAL_0+WCP_B1%U_NORMAL_0)/SF%VEL)
               IF (SF%RAMP(VELO_PROF_Z)%INDEX>0) PROFILE_FACTOR = EVALUATE_RAMP(ZC(KK),SF%RAMP(VELO_PROF_Z)%INDEX)
               RAMP_T = EVALUATE_RAMP(TSI,SF%RAMP(TIME_VELO)%INDEX,TAU=SF%RAMP(TIME_VELO)%TAU)
               IF (IEC==1 .OR. (IEC==2 .AND. ICD==2)) VEL_T = RAMP_T*(PROFILE_FACTOR*(SF%VEL_T(2) + VEL_EDDY))
               IF (IEC==3 .OR. (IEC==2 .AND. ICD==1)) VEL_T = RAMP_T*(PROFILE_FACTOR*(SF%VEL_T(1) + VEL_EDDY))

            ENDIF

            ! Choose the appropriate boundary condition to apply

            BOUNDARY_CONDITION: SELECT CASE(VELOCITY_BC_INDEX)

               CASE (FREE_SLIP_BC) BOUNDARY_CONDITION

                  VEL_GHOST = VEL_GAS
                  DUIDXJ(ICD_SGN) = I_SGN*(VEL_GAS-VEL_GHOST)/DXX(ICD)
                  MU_DUIDXJ(ICD_SGN) = MUA*DUIDXJ(ICD_SGN)
                  ALTERED_GRADIENT(ICD_SGN) = .TRUE.

               CASE (NO_SLIP_BC) BOUNDARY_CONDITION

                  VEL_GHOST = 2._EB*VEL_T - VEL_GAS
                  DUIDXJ(ICD_SGN) = I_SGN*(VEL_GAS-VEL_GHOST)/DXX(ICD)
                  MU_DUIDXJ(ICD_SGN) = MUA*DUIDXJ(ICD_SGN)
                  ALTERED_GRADIENT(ICD_SGN) = .TRUE.

               CASE (WALL_MODEL_BC) BOUNDARY_CONDITION

                  ITMP = MIN(I_MAX_TEMP,NINT(0.5_EB*(TMP(IIGM,JJGM,KKGM)+TMP(IIGP,JJGP,KKGP))))
                  MU_WALL = MU_RSQMW_Z(ITMP,1)/RSQ_MW_Z(1)
                  RHO_WALL = 0.5_EB*( RHOP(IIGM,JJGM,KKGM) + RHOP(IIGP,JJGP,KKGP) )

                  CALL WALL_MODEL(SLIP_COEF,U_TAU,Y_PLUS,MU_WALL/RHO_WALL,SF%ROUGHNESS,0.5_EB*DXX(ICD),VEL_GAS-VEL_T)

                  ! SLIP_COEF = -1, no slip,   VEL_GHOST = 2*VEL_T - VEL_GAS
                  ! SLIP_COEF =  0, half slip, VEL_GHOST = VEL_T
                  ! SLIP_COEF =  1, free slip, VEL_GHOST = VEL_GAS

                  IF ((IWM==0.OR.IWP==0) .AND. .NOT.ED%EXTERNAL) SLIP_COEF = 0._EB  ! Corner
                  VEL_GHOST = VEL_T + SLIP_COEF*(VEL_GAS-VEL_T)
                  DUIDXJ(ICD_SGN) = I_SGN*(VEL_GAS-VEL_GHOST)/DXX(ICD)
                  MU_DUIDXJ(ICD_SGN) = RHO_WALL*U_TAU**2 * SIGN(1._EB,DUIDXJ(ICD_SGN))
                  ALTERED_GRADIENT(ICD_SGN) = .TRUE.

               CASE (BOUNDARY_FUEL_MODEL_BC) BOUNDARY_CONDITION

                  RHO_WALL = 0.5_EB*( RHOP(IIGM,JJGM,KKGM) + RHOP(IIGP,JJGP,KKGP) )
                  VEL_T = SQRT(UU(IIGM,JJGM,KKGM)**2 + VV(IIGM,JJGM,KKGM)**2)
                  VEL_GHOST = VEL_GAS
                  DUIDXJ(ICD_SGN) = 0._EB
                  MU_DUIDXJ(ICD_SGN) = I_SGN*0.5_EB*RHO_WALL*SF%DRAG_COEFFICIENT*SF%SHAPE_FACTOR*SF%LAYER_THICKNESS(1)*&
                                       SF%PACKING_RATIO(1)*SF%SURFACE_VOLUME_RATIO(1)*VEL_GAS*VEL_T
                  ALTERED_GRADIENT(ICD_SGN) = .TRUE.

            END SELECT BOUNDARY_CONDITION

         ELSE INTERPOLATION_IF  ! Use data from another mesh

            INTERPOLATED_EDGE = .TRUE.
            OM => OMESH(ABS(NOM(ICD)))

            IF (PREDICTOR) THEN
               SELECT CASE(IEC)
                  CASE(1)
                     IF (ICD==1) THEN
                        VEL_OTHER => OM%WS
                     ELSE ! ICD=2
                        VEL_OTHER => OM%VS
                     ENDIF
                  CASE(2)
                     IF (ICD==1) THEN
                        VEL_OTHER => OM%US
                     ELSE ! ICD=2
                        VEL_OTHER => OM%WS
                     ENDIF
                  CASE(3)
                     IF (ICD==1) THEN
                        VEL_OTHER => OM%VS
                     ELSE ! ICD=2
                        VEL_OTHER => OM%US
                     ENDIF
               END SELECT
            ELSE
               SELECT CASE(IEC)
                  CASE(1)
                     IF (ICD==1) THEN
                        VEL_OTHER => OM%W
                     ELSE ! ICD=2
                        VEL_OTHER => OM%V
                     ENDIF
                  CASE(2)
                     IF (ICD==1) THEN
                        VEL_OTHER => OM%U
                     ELSE ! ICD=2
                        VEL_OTHER => OM%W
                     ENDIF
                  CASE(3)
                     IF (ICD==1) THEN
                        VEL_OTHER => OM%V
                     ELSE ! ICD=2
                        VEL_OTHER => OM%U
                     ENDIF
               END SELECT
            ENDIF

            WGT = ED%EDGE_INTERPOLATION_FACTOR(ICD)
            OMW = 1._EB-WGT

            SELECT CASE(IEC)
               CASE(1)
                  IF (ICD==1) THEN
                     VEL_GHOST = WGT*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)) + OMW*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)-1)
                  ELSE ! ICD=2
                     VEL_GHOST = WGT*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)) + OMW*VEL_OTHER(IIO(ICD),JJO(ICD)-1,KKO(ICD))
                  ENDIF
               CASE(2)
                  IF (ICD==1) THEN
                     VEL_GHOST = WGT*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)) + OMW*VEL_OTHER(IIO(ICD)-1,JJO(ICD),KKO(ICD))
                  ELSE ! ICD=2
                     VEL_GHOST = WGT*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)) + OMW*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)-1)
                  ENDIF
               CASE(3)
                  IF (ICD==1) THEN
                     VEL_GHOST = WGT*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)) + OMW*VEL_OTHER(IIO(ICD),JJO(ICD)-1,KKO(ICD))
                  ELSE ! ICD==2
                     VEL_GHOST = WGT*VEL_OTHER(IIO(ICD),JJO(ICD),KKO(ICD)) + OMW*VEL_OTHER(IIO(ICD)-1,JJO(ICD),KKO(ICD))
                  ENDIF
            END SELECT

         ENDIF INTERPOLATION_IF

         ! Set ghost cell values at edge of computational domain

         SELECT CASE(IEC)
            CASE(1)
               IF (JJ==0    .AND. IOR== 2) WW(II,JJ,KK)   = VEL_GHOST
               IF (JJ==JBAR .AND. IOR==-2) WW(II,JJ+1,KK) = VEL_GHOST
               IF (KK==0    .AND. IOR== 3) VV(II,JJ,KK)   = VEL_GHOST
               IF (KK==KBAR .AND. IOR==-3) VV(II,JJ,KK+1) = VEL_GHOST
               IF (CORRECTOR) THEN
                 IF (ICD==1) THEN
                    ED%W_AVG = 0.5_EB*(VEL_GHOST+VEL_GAS)
                 ELSE ! ICD=2
                    ED%V_AVG = 0.5_EB*(VEL_GHOST+VEL_GAS)
                 ENDIF
               ENDIF
            CASE(2)
               IF (II==0    .AND. IOR== 1) WW(II,JJ,KK)   = VEL_GHOST
               IF (II==IBAR .AND. IOR==-1) WW(II+1,JJ,KK) = VEL_GHOST
               IF (KK==0    .AND. IOR== 3) UU(II,JJ,KK)   = VEL_GHOST
               IF (KK==KBAR .AND. IOR==-3) UU(II,JJ,KK+1) = VEL_GHOST
               IF (CORRECTOR) THEN
                 IF (ICD==1) THEN
                    ED%U_AVG = 0.5_EB*(VEL_GHOST+VEL_GAS)
                 ELSE ! ICD=2
                    ED%W_AVG = 0.5_EB*(VEL_GHOST+VEL_GAS)
                 ENDIF
               ENDIF
            CASE(3)
               IF (II==0    .AND. IOR== 1) VV(II,JJ,KK)   = VEL_GHOST
               IF (II==IBAR .AND. IOR==-1) VV(II+1,JJ,KK) = VEL_GHOST
               IF (JJ==0    .AND. IOR== 2) UU(II,JJ,KK)   = VEL_GHOST
               IF (JJ==JBAR .AND. IOR==-2) UU(II,JJ+1,KK) = VEL_GHOST
               IF (CORRECTOR) THEN
                 IF (ICD==1) THEN
                    ED%V_AVG = 0.5_EB*(VEL_GHOST+VEL_GAS)
                 ELSE ! ICD=2
                    ED%U_AVG = 0.5_EB*(VEL_GHOST+VEL_GAS)
                 ENDIF
               ENDIF
         END SELECT

      ENDDO ORIENTATION_LOOP
   ENDDO SIGN_LOOP

   ! Cycle out of the EDGE_LOOP if no tangential gradients have been altered.

   IF (.NOT.ANY(ALTERED_GRADIENT)) CYCLE EDGE_LOOP

   ! If the edge is on an interpolated boundary, and all cells around it are not solid, cycle

   IF (INTERPOLATED_EDGE) THEN
      IF (.NOT.CELL(ICMM)%SOLID .AND. .NOT.CELL(ICPM)%SOLID .AND. &
          .NOT.CELL(ICMP)%SOLID .AND. .NOT.CELL(ICPP)%SOLID) CYCLE EDGE_LOOP
   ENDIF

   ! Loop over all 4 normal directions and compute vorticity and stress tensor components for each

   SIGN_LOOP_2: DO I_SGN=-1,1,2
      ORIENTATION_LOOP_2: DO ICD=1,2
         IF (ICD==1) THEN
            ICDO=2
         ELSE ! ICD=2
            ICDO=1
         ENDIF
         ICD_SGN = I_SGN*ICD
         IF (ALTERED_GRADIENT(ICD_SGN)) THEN
               DUIDXJ_USE(ICD) =    DUIDXJ(ICD_SGN)
            MU_DUIDXJ_USE(ICD) = MU_DUIDXJ(ICD_SGN)
         ELSEIF (ALTERED_GRADIENT(-ICD_SGN)) THEN
               DUIDXJ_USE(ICD) =    DUIDXJ(-ICD_SGN)
            MU_DUIDXJ_USE(ICD) = MU_DUIDXJ(-ICD_SGN)
         ELSE
            CYCLE ORIENTATION_LOOP_2
         ENDIF
         ICDO_SGN = I_SGN*ICDO
         IF (ALTERED_GRADIENT(ICDO_SGN)) THEN
               DUIDXJ_USE(ICDO) =    DUIDXJ(ICDO_SGN)
            MU_DUIDXJ_USE(ICDO) = MU_DUIDXJ(ICDO_SGN)
         ELSEIF (ALTERED_GRADIENT(-ICDO_SGN)) THEN
               DUIDXJ_USE(ICDO) =    DUIDXJ(-ICDO_SGN)
            MU_DUIDXJ_USE(ICDO) = MU_DUIDXJ(-ICDO_SGN)
         ELSE
               DUIDXJ_USE(ICDO) = 0._EB
            MU_DUIDXJ_USE(ICDO) = 0._EB
         ENDIF
         ED%OMEGA(ICD_SGN) =    DUIDXJ_USE(1) -    DUIDXJ_USE(2)
         ED%TAU(ICD_SGN)   = MU_DUIDXJ_USE(1) + MU_DUIDXJ_USE(2)
      ENDDO ORIENTATION_LOOP_2
   ENDDO SIGN_LOOP_2

ENDDO EDGE_LOOP

#if defined coupled_debug
Print*, 'end of velocity_bc',  NM
#endif



T_USED(4)=T_USED(4)+CURRENT_TIME()-T_NOW

IF(CC_IBM) CALL CC_VELOCITY_BC(T,NM,APPLY_TO_ESTIMATED_VARIABLES,DO_IBEDGES=.TRUE.)

END SUBROUTINE VELOCITY_BC
