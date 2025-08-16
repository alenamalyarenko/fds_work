! included in velo.f90
SUBROUTINE ATM_BC_COUPLED(T,NM)
use netcdf
USE COUPLED_FILES
USE MESH_POINTERS

REAL(EB), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NM

TYPE (VENTS_TYPE), POINTER :: VT
TYPE (MESH_TYPE), POINTER :: M
	
REAL(EB) :: T_NOW

integer:: I,J,K,N
!integer:: ncid, varid1,varid2,varid3,varid4, status, timestamp !move ncid to module to avoid closing the file
integer::  varid1,varid2,varid3,varid4, status, timestamp0, timestamp1
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
REAL,ALLOCATABLE, DIMENSION(:,:):: T0,U0,V0,W0

#if defined bc_time_interp
REAL,ALLOCATABLE, DIMENSION(:,:):: T1,U1,V1,W1
real:: wght0, wght1	,  locTime, modTime, tmpTime
	
!     Implicit function:
Real:: F90MODULO, arg1, arg2
! statement function to emulate Fortran 90 MODULO
! this modulo has the same sign as arg2 (and absolute value < |arg2|)
      F90MODULO(arg1,arg2) = MOD(MOD(arg1,arg2)+arg2,arg2)	
#endif

T_NOW = CURRENT_TIME()

! Assign local names to variables

CALL POINT_TO_MESH(NM)

!allocate variables
ALLOCATE( T0(1:IBAR,1:KBAR))
ALLOCATE( U0(1:IBAR,1:KBAR))
ALLOCATE( V0(1:IBAR,1:KBAR)) 	
ALLOCATE( W0(1:IBAR,1:KBAR))   

#if defined bc_time_interp	
ALLOCATE( T1(1:IBAR,1:KBAR))
ALLOCATE( U1(1:IBAR,1:KBAR))
ALLOCATE( V1(1:IBAR,1:KBAR)) 	
ALLOCATE( W1(1:IBAR,1:KBAR))   	
      timestamp1 = 0

      wght0 = 0.
      wght1 = 0.
#endif
      timestamp0 = 0

#if defined bc_time_interp
! IN: 
!     cycleLength :: length of the periodic cycle (in s), zero if non-periodic
!     recSpacing  :: time record spacing
!     deltaT      :: time-step
!     currentTime :: current time
!     myThid      :: my Thread Id number


!figure out 2 timestamps
!timestamp0=int(T)
!timestamp1=int(T)+1
!wght0= (T-timestamp0)/recSpacing
!wght1= 1-wght0  

!! spacing in forcing file
!recSpacing=1
!
 locTime = T !- recSpacing*0.5
        modTime = F90MODULO(locTime,recSpacing)
!!    time-record before (tRec1) and after (tRec2) current time:
        timestamp0 = 1 + NINT( (locTime-modTime)/recSpacing )
        timestamp1 = 1 + timestamp0    
!!figure out 2 weights
wght1=modTime / recSpacing
wght0=1.0 - wght1
             
             
           


	

!Print*, 'timestamps ',T, locTime, modTime
!Print*, 'timestamps ',T, timestamp0,  timestamp1, wght0, wght1

#else
timestamp0=int(T)+1
#endif


VENT_LOOP: DO N=1,N_VENT
    VT => VENTS(N)
    COUPLED_VENT_IF: IF (VT%N_EDDY==(-1)) THEN !coupled vent
    
    IF (T==0) THEN
    status=nf90_open(OBFile , nf90_nowrite, ncid)
    ENDIF
    !print *,'opened file', NM, trim(nf90_strerror(status))
    
   	IF (VENTS(N)%IOR==2) THEN
       !Print*, 'found south vent'

       !figure out vent / mesh overlap indeces
       !print*, NM, MI,MJ,MK, 'vent number ', N, 'vents ',VT%I1,VT%I2,VT%J1,VT%J2, 'global mesh', GI1, GJ1,GK1
       !vent coordinates are 0:20; UVW_eddy varibales are 1:20 (face centered) 
 
       status=nf90_inq_varid(ncid, 'US', varid1)
       !print *,'inq US', NM, trim(nf90_strerror(status))       
       status=nf90_inq_varid(ncid, 'VS', varid2)
       !print *,'inq VS',NM, trim(nf90_strerror(status))       
       status=nf90_inq_varid(ncid, 'WS', varid3)
       !print *,'inq WS',NM, trim(nf90_strerror(status))
       status=nf90_inq_varid(ncid, 'TS', varid4)
       !print *,'inq TS',NM, trim(nf90_strerror(status))
        
 
       !EDDY Variables - face-averaged - early timestamp
       status=nf90_get_var(ncid, varid1, U0,start = (/ VT%GI1+1, VT%GK1+1,timestamp0 /),  count = (/ IBAR, KBAR,1 /) )        
       !print *,'read US', NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid2, V0,start = (/ VT%GI1+1, VT%GK1+1,timestamp0 /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read VS',NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid3, W0,start = (/ VT%GI1+1, VT%GK1+1,timestamp0 /),  count = (/ IBAR, KBAR,1 /) )
       !print *,'read WS',NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid4, T0,start = (/ VT%GI1+1, VT%GK1+1,timestamp0 /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read TS',NM, trim(nf90_strerror(status))
       
       if (status /= nf90_noerr) then
       	!call handle_err(status) 
       	stop
       endif
#if defined bc_time_interp       
       !EDDY Variables - face-averaged - Later timestamp
       status=nf90_get_var(ncid, varid1, U1,start = (/ VT%GI1+1, VT%GK1+1,timestamp1 /),  count = (/ IBAR, KBAR,1 /) )        
       !print *,'read US', NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid2, V1,start = (/ VT%GI1+1, VT%GK1+1,timestamp1 /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read VS',NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid3, W1,start = (/ VT%GI1+1, VT%GK1+1,timestamp1 /),  count = (/ IBAR, KBAR,1 /) )
       !print *,'read WS',NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid4, T1,start = (/ VT%GI1+1, VT%GK1+1,timestamp1 /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read TS',NM, trim(nf90_strerror(status))
#endif

  

       !Print*, 'eddy loop', NM,GI1, GK1, size( VT%U_ATM,1),size( VT%U_ATM,2)
       DO K=1,KBAR
        DO I=1,IBAR
#if defined bc_time_interp        
         VT%U_ATM(I,K)=U0(I,K)*wght0+U1(I,K)*wght1 
         VT%V_ATM(I,K)=V0(I,K)*wght0+V1(I,K)*wght1 
         VT%W_ATM(I,K)=W0(I,K)*wght0+W1(I,K)*wght1 
         VT%T_ATM(I,K)=T0(I,K)*wght0+T1(I,K)*wght1 +273.15
#else
         VT%U_ATM(I,K)=U0(I,K)
         VT%V_ATM(I,K)=V0(I,K)
         VT%W_ATM(I,K)=W0(I,K)
         VT%T_ATM(I,K)=T0(I,K)  +273.15

#endif         
        ENDDO
       ENDDO 
#ifdef coupled_debug       
       print*,'atm_bc prescribed',     VT%U_ATM(I,K),     size( VT%U_ATM,1),size( VT%U_ATM,2), VT%T_ATM(I,K)
       print*,'atm_bc readin', NM, V0(1,1),V0(1,55),V0(1,56),V0(1,57),V0(1,58),V0(1,59),V0(1,60)
       print*,'atm_bc readin', NM, T0(1,1),T0(1,55),T0(1,56),T0(1,57),T0(1,58),T0(1,59),T0(1,60) 
#endif    
      
    ENDIF 

    
    
    
   	IF (VENTS(N)%IOR==-2) THEN
 	   !Print*, 'found north vent'
    ENDIF     
     
   	IF (VENTS(N)%IOR==1) THEN
    ENDIF   	    
    
    IF (VENTS(N)%IOR==-1) THEN
    ENDIF    
    
   
   IF (T>=T_END) THEN
   status=nf90_close(ncid)   
   ENDIF
   !print *,'closed netcdf',NM, trim(nf90_strerror(status))
   
   ENDIF COUPLED_VENT_IF
ENDDO VENT_LOOP

!print*, 'out of vent loop', NM

!deallocate variables
DEALLOCATE(T0)
DEALLOCATE(U0)
DEALLOCATE(V0) 	
DEALLOCATE(W0) 

DEALLOCATE(T1)
DEALLOCATE(U1)
DEALLOCATE(V1) 	
DEALLOCATE(W1) 

T_USED(4)=T_USED(4)+CURRENT_TIME()-T_NOW

END SUBROUTINE ATM_BC_COUPLED
