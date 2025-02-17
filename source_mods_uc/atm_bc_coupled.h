! included in velo.f90
SUBROUTINE ATM_BC_COUPLED(T,NM)
use netcdf
USE COUPLED_FILES, ONLY: OBFile
USE MESH_POINTERS

REAL(EB), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NM

TYPE (VENTS_TYPE), POINTER :: VT
TYPE (MESH_TYPE), POINTER :: M
	
REAL(EB) :: T_NOW

integer:: I,J,K,N
integer:: ncid, varid1,varid2,varid3,varid4, status, timestamp
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
REAL,ALLOCATABLE, DIMENSION(:,:):: T0,U0,V0,W0

       			
T_NOW = CURRENT_TIME()

! Assign local names to variables

CALL POINT_TO_MESH(NM)

!allocate variables
ALLOCATE( T0(1:IBAR,1:KBAR))
ALLOCATE( U0(1:IBAR,1:KBAR))
ALLOCATE( V0(1:IBAR,1:KBAR)) 	
ALLOCATE( W0(1:IBAR,1:KBAR))   

timestamp=int(T)+1

VENT_LOOP: DO N=1,N_VENT
    VT => VENTS(N)
    COUPLED_VENT_IF: IF (VT%N_EDDY==(-1)) THEN !coupled vent
    status=nf90_open(OBFile, nf90_nowrite, ncid)
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
       !no temp yet
       !status=nf90_inq_varid(ncid, 'TS', varid4)
       !print *,'inq TS',NM, trim(nf90_strerror(status))
        
 
       !EDDY Variables - face-averaged
       status=nf90_get_var(ncid, varid1, U0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read US', NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid2, V0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) ) 
       !print *,'read VS',NM, trim(nf90_strerror(status))
       status=nf90_get_var(ncid, varid3, W0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) )
       !print *,'read WS',NM, trim(nf90_strerror(status))
       !no temp yet
       !status=nf90_get_var(ncid, varid4, T0,start = (/ GI1, GK1,timestamp /),  count = (/ IBAR, KBAR,1 /) ) 
       

       !Print*, 'eddy loop', NM,GI1, GK1, size( VT%U_ATM,1),size( VT%U_ATM,2)
       DO K=1,KBAR
        DO I=1,IBAR
         VT%U_ATM(I,K)=U0(I,K) 
         VT%V_ATM(I,K)=V0(I,K) 
         VT%W_ATM(I,K)=W0(I,K) 
         VT%T_ATM(I,K)=T0(I,K) +273.15
        ENDDO
       ENDDO 
              
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

!deallocate variables
DEALLOCATE(T0)
DEALLOCATE(U0)
DEALLOCATE(V0) 	
DEALLOCATE(W0) 

T_USED(4)=T_USED(4)+CURRENT_TIME()-T_NOW

END SUBROUTINE ATM_BC_COUPLED
