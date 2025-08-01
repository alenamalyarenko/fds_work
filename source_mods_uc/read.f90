#ifndef GITHASH_PP
#define GITHASH_PP "unknown"
#endif
#include 'keys.h'

!> \brief Subroutines that read the various NAMELIST lines in the FDS input file

MODULE READ_INPUT

USE PRECISION_PARAMETERS
USE MESH_VARIABLES
USE GLOBAL_CONSTANTS
USE TRAN
USE MESH_POINTERS
USE OUTPUT_DATA
USE COMP_FUNCTIONS, ONLY: CHECKREAD, SHUTDOWN, CHECK_XB, SCAN_INPUT_FILE
USE MEMORY_FUNCTIONS, ONLY: ChkMemErr,REALLOCATE2D
USE COMP_FUNCTIONS, ONLY: GET_INPUT_FILE
USE MISC_FUNCTIONS, ONLY: SEARCH_CONTROLLER,WRITE_SUMMARY_INFO
USE HVAC_ROUTINES, ONLY: READ_HVAC,PROC_HVAC
USE COMPLEX_GEOMETRY, ONLY: READ_GEOM
USE MPI_F08
USE THERMO_PROPS

IMPLICIT NONE (TYPE,EXTERNAL)
PRIVATE
#if defined init_file_in
PUBLIC READ_DATA,READ_STOP,VERSION_INFO, READ_IC
#else
PUBLIC READ_DATA,READ_STOP,VERSION_INFO
#endif

CHARACTER(LABEL_LENGTH) :: ID,MB,DB,ODE_SOLVER
CHARACTER(MESSAGE_LENGTH) :: MESSAGE,FYI
CHARACTER(LABEL_LENGTH) :: FUEL_RADCAL_ID='METHANE',WATER_VAPOR='WATER VAPOR'
CHARACTER(LABEL_LENGTH), ALLOCATABLE, DIMENSION(:) :: REAC_FUEL !< Array of reaction FUEL names
LOGICAL :: EX,THICKEN_OBSTRUCTIONS,BAD,IDEAL=.FALSE.,TARGET_PARTICLES_INCLUDED=.FALSE.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: SIMPLE_FUEL_DEFINED
LOGICAL, ALLOCATABLE, DIMENSION(:) :: DUPLICATE_FUEL !< FUEL for the reaction is present on more than one reaction
REAL(EB) :: XB(6),TEXTURE_ORIGIN(3)
REAL(EB) :: PBX,PBY,PBZ
REAL(EB) :: MW_MIN,MW_MAX
REAL(EB) :: REAC_ATOM_ERROR,REAC_MASS_ERROR,HUMIDITY=-1._EB,RADIATIVE_FRACTION
REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: SS_CP,SS_G_F,SS_K,SS_MU,SS_D
REAL(EB) :: SOOT_C_FRACTION, SOOT_H_FRACTION, SOOT_O_FRACTION, SOOT_N_FRACTION !< Atom fractions of soot

INTEGER  :: I,J,K,IZERO,IOS,N_INIT_RESERVED,MAX_LEAK_PATHS,I_DUM(10),IERROR,N_CONE_RAMP=0,WATER_INDEX
INTEGER :: FUEL_SMIX_INDEX  ! Simple chemistry fuel index
TYPE(MESH_TYPE), POINTER :: M=>NULL()
TYPE(OBSTRUCTION_TYPE), POINTER :: OB=>NULL()
TYPE(VENTS_TYPE), POINTER :: VT=>NULL()
TYPE(SURFACE_TYPE), POINTER :: SF=>NULL()
TYPE(MATERIAL_TYPE), POINTER :: ML=>NULL()
TYPE(REACTION_TYPE), POINTER :: RN=>NULL()
LOGICAL :: RETURN_BEFORE_STOP_FILE=.FALSE., RETURN_BEFORE_SIM_MODE=.FALSE.
INTEGER :: N_HT3D_SURF_LINES=0,N_HT3D_OBST=0
CHARACTER(LABEL_LENGTH), DIMENSION(20) :: HT3D_SURF_LIST='null'
TYPE HT3D_OBST_TYPE
   INTEGER :: GROUP_INDEX=0
   REAL(EB) :: XS,XF,YS,YF,ZS,ZF
END TYPE
TYPE(HT3D_OBST_TYPE), DIMENSION(:), ALLOCATABLE, TARGET :: HT3D_OBST


CONTAINS


!> \brief Read the FDS input file

SUBROUTINE READ_DATA(DT)
USE PROPERTY_DATA, ONLY: MAKE_PERIODIC_TABLE,SIMPLE_SPECIES_MW
REAL(EB) :: DT,VEL_CHAR

! Create an array of output QUANTITY names that are included in the various NAMELIST groups

CALL DEFINE_OUTPUT_QUANTITIES

! Set up atomic data and species data

CALL MAKE_PERIODIC_TABLE
CALL DEFINE_THERMO_PROPS
CALL GET_PROP_INDEX(WATER_VAPOR,WATER_INDEX)
CALL SIMPLE_SPECIES_MW

! Get the name of the input file by reading the command line argument

CALL GET_INPUT_FILE

! Stop FDS if the input file cannot be found in the current directory

INQUIRE(FILE=FN_INPUT,EXIST=EX)
IF (.NOT.EX) THEN
   IF (MY_RANK==0) WRITE(LU_ERR,'(A,A,A)') 'ERROR(102): Input file ',TRIM(FN_INPUT),' not found in the current directory.'
   STOP_STATUS = VERSION_STOP ; RETURN
ENDIF

IF (MY_RANK==0) WRITE(LU_ERR,'(/A/)') ' Reading FDS input file ...'

! Allocate the global orientation vector

N_ORIENTATION_VECTOR = 0
ALLOCATE(ORIENTATION_VECTOR(3,0:10))
ORIENTATION_VECTOR(1:3,0) = (/0._EB,0._EB,-1._EB/)

! Open the input file

OPEN(LU_INPUT,FILE=FN_INPUT,ACTION='READ')

! Read the input file, NAMELIST group by NAMELIST group

CALL READ_CATF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_HEAD    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_MISC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_MOVE    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_MULT    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_MESH    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_MESH    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_TRAN    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_TIME(DT); CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_PRES    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_COMB    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_REAC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_SPEC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_REAC_1  ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_RADI    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_PROP    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_DEVC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_PART    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_CTRL    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_MATL    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_SURF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_CSVF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_OBST    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_GEOM    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_VENT    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_ZONE    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_HVAC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_WIND    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_SURF_1  ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_INIT    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_DUMP    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_RAMP    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_SPEC_1  ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_SMIX    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_SPEC_2  ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_REAC_2  ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_MATL    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_SURF_2  ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_HVAC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_CLIP    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_WALL    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_PART    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_INIT    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_TABL    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_CTRL    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_PROP    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_DEVC    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL PROC_OBST    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_PROF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_SLCF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_ISOF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_BNDF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_SM3D    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
CALL READ_RADF    ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
#if defined init_file_in
!here we call it without the output 
CALL READ_IC      ; CALL CHECK_STOP_STATUS ; IF (STOP_STATUS/=NO_STOP) RETURN
#endif
! Deallocate arrays if allocated
IF (ALLOCATED(DUPLICATE_FUEL)) DEALLOCATE (DUPLICATE_FUEL)
IF (ALLOCATED (REAC_FUEL)) DEALLOCATE (REAC_FUEL)

! Close the input file, and never open it again

CLOSE (LU_INPUT)

! Set QUANTITY ambient values

CALL SET_QUANTITIES_AMBIENT

! Compute the starting time step if the user has not specified it.

IF (DT<=0._EB) THEN
   VEL_CHAR = MAX( 0.1_EB, 0.2_EB*SQRT(GRAV*(ZF_MAX-ZS_MIN)) )
   IF (ABS(U0)>TWO_EPSILON_EB .OR. ABS(V0)>TWO_EPSILON_EB .OR. ABS(W0)>TWO_EPSILON_EB) &
      VEL_CHAR = MAX(VEL_CHAR,SQRT(U0**2+V0**2+W0**2))
   DT = CFL_MAX*CHARACTERISTIC_CELL_SIZE/VEL_CHAR
ENDIF

CONTAINS

!> \brief Return to main if any MPI processes have hit an ERROR

SUBROUTINE CHECK_STOP_STATUS
INTEGER :: IERR
IF (N_MPI_PROCESSES>1) CALL MPI_ALLREDUCE(MPI_IN_PLACE,STOP_STATUS,INTEGER_ONE,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,IERR)
END SUBROUTINE CHECK_STOP_STATUS

END SUBROUTINE READ_DATA


!> \brief Get the name of the input file by reading the command line argument

SUBROUTINE VERSION_INFO
INTEGER :: MPILIBLENGTH,IERR
CHARACTER(LEN=MPI_MAX_LIBRARY_VERSION_STRING) :: MPILIBVERSION

CALL GET_INPUT_FILE

! If no input file is given, just print out the version number and stop

IF (FN_INPUT(1:1)==' ') THEN
   IF (MY_RANK==0) THEN
      CALL WRITE_SUMMARY_INFO(LU_ERR,.FALSE.)
      WRITE(LU_ERR,'(A)')  ' Consult FDS Users Guide Chapter, Running FDS, for further instructions.'
   ENDIF
   STOP ! this routine is only called before MPI is initialized so safe to STOP here
ENDIF
IF (FN_INPUT(1:2)=='-V' .OR. FN_INPUT(1:2)=='-v') THEN
   IF (MY_RANK==0) THEN
      CALL MPI_GET_LIBRARY_VERSION(MPILIBVERSION,MPILIBLENGTH,IERR)
      WRITE(LU_ERR,'(A,A)') 'FDS revision       : ',TRIM(GITHASH_PP)
      WRITE(LU_ERR,'(A,A)') 'MPI library version: ',TRIM(MPILIBVERSION)
   ENDIF
   STOP ! this routine is only called before MPI is initialized so safe to STOP here
ENDIF

END SUBROUTINE VERSION_INFO


!> \brief Read the CATF (CATenate File) lines in the FDS input file

SUBROUTINE READ_CATF

USE COMP_FUNCTIONS, ONLY: FDS_SLEEP
INTEGER :: N_CATF_LINES, OFI, TFI
INTEGER, PARAMETER :: LU_CATF2 = 999, LU_STOP1=998, LU_STOP2=997
INTEGER, PARAMETER :: MAX_OTHER_FILES=20 ! Maximum number of fires in the OTHER_FILES namelist field.
CHARACTER(MESSAGE_LENGTH), DIMENSION(MAX_OTHER_FILES) :: OTHER_FILES = 'null'
CHARACTER(MESSAGE_LENGTH) :: BUFFER
CHARACTER(250) :: FN_CATF='null'
INTEGER :: IERR

NAMELIST /CATF/ OTHER_FILES

! First retrieve original CHID:

RETURN_BEFORE_STOP_FILE=.TRUE.
CALL READ_HEAD
RETURN_BEFORE_STOP_FILE=.FALSE.

! Check how many &CATF input lines are being defined:

N_CATF_LINES=0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_OFILES_LOOP1: DO
   CALL CHECKREAD('CATF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_OFILES_LOOP1
   READ(LU_INPUT,'(A)') BUFFER
   N_CATF_LINES = N_CATF_LINES + 1
ENDDO COUNT_OFILES_LOOP1
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
IF (N_CATF_LINES==0) RETURN

! Check that &CATF other files exist:

COUNT_OFILES_LOOP2: DO
   CALL CHECKREAD('CATF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_OFILES_LOOP2
   READ(LU_INPUT,NML=CATF,END=11,ERR=12,IOSTAT=IOS)
   12 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with CATF line.') ; RETURN ; ENDIF
   ! OPEN and copy other files into LU_CATF:
   OFI=0
   CPY_LOOP1: DO
      OFI = OFI + 1
      IF(TRIM(OTHER_FILES(OFI))=='null') EXIT CPY_LOOP1
      ! Inquire if other file exists:
      INQUIRE(FILE=TRIM(OTHER_FILES(OFI)),EXIST=EX)
      IF (.NOT.EX) THEN
         WRITE(BUFFER,'(A)') 'ERROR(103): CATF file '//TRIM(OTHER_FILES(OFI))//' not found.'
         CALL SHUTDOWN(TRIM(BUFFER)) ; RETURN
      ENDIF
   ENDDO CPY_LOOP1
ENDDO COUNT_OFILES_LOOP2
11 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Here at least one &CATF line has been found:
! Open CHID_cat.fds file which will concatenate all input files:

FN_CATF = TRIM(CHID)//'_cat.fds'

! Now check state of OVERWRITE:

RETURN_BEFORE_SIM_MODE=.TRUE.
CALL READ_MISC
RETURN_BEFORE_SIM_MODE=.FALSE.

! Inquire if FN_CATF is present, if so stop to avoid overwriting the input file potentially used previously.

INQUIRE(FILE=TRIM(FN_CATF),EXIST=EX)
IF (EX .AND. .NOT.OVERWRITE) THEN
   WRITE(BUFFER,'(A)') &
   'ERROR(104): OVERWRITE=F and concatenated file '//TRIM(FN_CATF)//' exists. Also remove '//TRIM(CHID)//'_cat.out'
   CALL SHUTDOWN(TRIM(BUFFER)) ; RETURN
ENDIF

IF (MY_RANK==0) THEN
   OPEN(LU_CATF,FILE=FN_CATF,ACTION='WRITE')
   ! Write new header for LU_CATF:
   IF (LEN_TRIM(FYI)>0) THEN
      WRITE(LU_CATF,'(A)')&
      "&HEAD CHID='"//TRIM(CHID)//"_cat', TITLE='(Concatenated) "//TRIM(TITLE)//"', FYI='"//TRIM(FYI)//"' /"
   ELSE
      WRITE(LU_CATF,'(A)')&
      "&HEAD CHID='"//TRIM(CHID)//"_cat', TITLE='(Concatenated) "//TRIM(TITLE)//"' /"
   ENDIF

   ! Also, inquire if file TRIM(CHID)//'.stop' exists, if so make a TRIM(CHID)//'_cat.stop' with same contents.
   INQUIRE(FILE=TRIM(CHID)//'.stop',EXIST=EX)
   IF (EX) THEN
      OPEN(LU_STOP1,FILE=TRIM(CHID)//'.stop',STATUS='OLD',ACTION='READ')
      OPEN(LU_STOP2,FILE=TRIM(CHID)//'_cat.stop',STATUS='REPLACE',ACTION='WRITE')
      DO
         READ(LU_STOP1,'(A)',END=20,IOSTAT=IOS) BUFFER
         IF(IOS/=0) EXIT
         WRITE(LU_STOP2,'(A)') TRIM(BUFFER)
      ENDDO
20    CLOSE(LU_STOP1)
      CLOSE(LU_STOP2)
   ENDIF
ENDIF

! Load CHID file into LU_CATF:

CALL COPY_FILE_TO_CAT(LU_INPUT,LU_CATF,0)

IF (N_MPI_PROCESSES > 1) CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

! One &CATF line by one add the corresponding OTHER_FILES into LU_CATF:

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
TFI=0
COPY_OFILES_LOOP: DO
   CALL CHECKREAD('CATF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COPY_OFILES_LOOP
   OTHER_FILES(:) = 'null'
   READ(LU_INPUT,NML=CATF,END=13,ERR=14,IOSTAT=IOS)
   14 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with CATF line.') ; RETURN ; ENDIF
   ! OPEN and copy other files into LU_CATF:
   OFI=0
   CPY_LOOP: DO
      TFI = TFI + 1
      OFI = OFI + 1
      IF(TRIM(OTHER_FILES(OFI))=='null') EXIT CPY_LOOP
      ! If it exists open it and copy its contents without the &HEAD line (if any) up to the first &TAIL /
      ! appearance or the EOF.
      OPEN(LU_CATF2,FILE=TRIM(OTHER_FILES(OFI)),ACTION='READ')
      IF (MY_RANK==0) THEN
         IF (TFI>1) WRITE(LU_CATF,'(A)')
         WRITE(LU_CATF,'(A)')'# Start of file '//TRIM(OTHER_FILES(OFI))//' :'
      ENDIF
      CALL COPY_FILE_TO_CAT(LU_CATF2,LU_CATF,OFI)
      CLOSE(LU_CATF2)
   ENDDO CPY_LOOP
ENDDO COPY_OFILES_LOOP
13 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_MPI_PROCESSES > 1) CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

! Process 0 closes LU_CATF and reopens FN_CATF as LU_INPUT:

IF (MY_RANK==0) THEN
   WRITE(LU_CATF,'(A)')
   WRITE(LU_CATF,'(A)') '&TAIL /'
   CLOSE(LU_CATF)
ENDIF

CLOSE(LU_INPUT)

CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

IF (MY_RANK==0) THEN
   OPEN(LU_INPUT,FILE=FN_CATF,STATUS='OLD',ACTION='READ')
ENDIF

! Finally other processes reopen FN_CATF as LU_INPUT:

IF (MY_RANK/=0) THEN
   TFI = 0
   DO
      TFI = TFI + 1
      IF (TFI > 100) EXIT
      INQUIRE(FILE=FN_CATF,EXIST=EX)
      IF (EX) THEN
         OPEN(LU_INPUT,FILE=FN_CATF,STATUS='OLD',ACTION='READ')
         EXIT
      ENDIF
      CALL FDS_SLEEP(1)
   ENDDO
ENDIF

CONTAINS

!> \brief Copy lines from input ASCII file LU_INFILE handle, to LU_OUTFILE with output handle.  HEAD or CATF lines are skipped.

SUBROUTINE COPY_FILE_TO_CAT(LU_INFILE,LU_OUTFILE,FILENUM)

INTEGER, INTENT(IN) :: LU_INFILE,LU_OUTFILE,FILENUM
INTEGER :: MESSAGE_LENGTH_EXT = 2*MESSAGE_LENGTH
CHARACTER(2*MESSAGE_LENGTH+1) :: BUFFER2 ! This size should be the same as MESSAGE_LENGTH_EXT+1.

COPY_IFILE_LOOP: DO
   ! Non Advancing READ, test if size of record larger than size of BUFFER2 and if end of file:
   READ(LU_INFILE,'(A)',ADVANCE='NO',EOR=11,END=10) BUFFER2
   IF (FILENUM==0) THEN
      WRITE(BUFFER,'(A,I3,A)') 'ERROR(105): Input file '//TRIM(CHID)//'.fds has line with > ',MESSAGE_LENGTH_EXT,' characters.'
   ELSE
      WRITE(BUFFER,'(A,I3,A)') 'ERROR(106): CATF file '//TRIM(OTHER_FILES(FILENUM))//'.fds has line with > ',&
                               MESSAGE_LENGTH_EXT,' characters. Split it.'
   ENDIF
   CALL SHUTDOWN(BUFFER); RETURN
   ! Advancing READ:
11 CONTINUE
   BACKSPACE(LU_INFILE); READ(LU_INFILE,'(A)') BUFFER2
   IF (BUFFER2(1:5)=='&HEAD') CYCLE COPY_IFILE_LOOP
   IF (BUFFER2(1:5)=='&CATF') CYCLE COPY_IFILE_LOOP
   IF (BUFFER2(1:5)=='&TAIL') EXIT COPY_IFILE_LOOP ! Do not copy the tail line to LU_CATF
   IF(MY_RANK==0) WRITE(LU_OUTFILE,'(A)') TRIM(BUFFER2)
ENDDO COPY_IFILE_LOOP
10 RETURN

END SUBROUTINE COPY_FILE_TO_CAT

END SUBROUTINE READ_CATF


!> \brief Read the HEAD NAMELIST line, which contains the job name

SUBROUTINE READ_HEAD
INTEGER :: NAMELENGTH
NAMELIST /HEAD/ CHID,FYI,TITLE
CHARACTER(80) :: BAD_TEXT

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
CALL SCAN_INPUT_FILE(LU_INPUT,IOS,BAD_TEXT)
IF (IOS==0) THEN
   WRITE(MESSAGE,'(3A)') 'ERROR(107): Hidden carriage return character in line starting with: ',BAD_TEXT(2:15),'...'
   CALL SHUTDOWN(MESSAGE)
ENDIF

CHID    = 'null'
TITLE   = '      '
FYI     = '      '

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
HEAD_LOOP: DO
   CALL CHECKREAD('HEAD',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT HEAD_LOOP
   READ(LU_INPUT,HEAD,END=13,ERR=14,IOSTAT=IOS)
   14 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with HEAD line') ; RETURN ; ENDIF
ENDDO HEAD_LOOP
13 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

CLOOP: DO I=1,CHID_LENGTH-1
   IF (CHID(I:I)=='.') THEN ; CALL SHUTDOWN('ERROR(108): No periods allowed in CHID') ; RETURN ; ENDIF
   IF (CHID(I:I)==' ') EXIT CLOOP
ENDDO CLOOP

IF (TRIM(CHID)=='null') THEN
   NAMELENGTH = LEN_TRIM(FN_INPUT)
   ROOTNAME: DO I=NAMELENGTH,2,-1
      IF (FN_INPUT(I:I)=='.') THEN
         WRITE(CHID,'(A)') FN_INPUT(1:I-1)
         EXIT ROOTNAME
      ENDIF
   END DO ROOTNAME
ENDIF

IF (RETURN_BEFORE_STOP_FILE) RETURN

! Define and look for a stop file

FN_STOP = TRIM(CHID)//'.stop'
INQUIRE(FILE=FN_STOP,EXIST=EX)
IF (EX) THEN
   STOP_AT_ITER=READ_STOP() ! READ_STOP() returns 0 if there is nothing in the .stop file
   IF (STOP_AT_ITER<=0) THEN
      WRITE(MESSAGE,'(A,A,A)') "ERROR(109): Remove the file, ",TRIM(FN_STOP),", from the current directory"
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ELSE
      WRITE(LU_ERR,'(A,A,A)') "NOTE: The file, ",TRIM(FN_STOP),", was detected."
      WRITE(LU_ERR,'(A,I3,A)')"This FDS run will stop after ",STOP_AT_ITER," iterations."
   ENDIF
ENDIF

END SUBROUTINE READ_HEAD


!> \brief If a stop file exists and contains a positive integer, stop the fds run when it computes that number of iterations

INTEGER FUNCTION READ_STOP()

   READ_STOP=0

   OPEN(UNIT=LU_STOP,FILE=FN_STOP,FORM='FORMATTED',STATUS='OLD',IOSTAT=IERROR)
   IF (IERROR==0) THEN
      READ(LU_STOP,*,END=10,IOSTAT=IERROR) READ_STOP
      IF (IERROR/=0) READ_STOP=0
   ENDIF
10 CLOSE(LU_STOP)

END FUNCTION READ_STOP


!> \brief Read the MESH namelist lines

SUBROUTINE READ_MESH

INTEGER :: IJK(3),NM,CURRENT_MPI_PROCESS,MPI_PROCESS,RGB(3),N_MESH_NEW,N,II,JJ,KK,NMESHES_READ,NNN,JBAR_OLD_VALUE
LOGICAL :: CYLINDRICAL_OLD_VALUE
REAL(EB) :: XB1,XB2,XB3,XB4,XB5,XB6
CHARACTER(25) :: COLOR
CHARACTER(LABEL_LENGTH) :: MULT_ID,TRNX_ID,TRNY_ID,TRNZ_ID
NAMELIST /MESH/ CHECK_MESH_ALIGNMENT,COLOR,CYLINDRICAL,FYI,ID,IJK,MPI_PROCESS,MULT_ID,RGB,TRNX_ID,TRNY_ID,TRNZ_ID,XB
TYPE (MESH_TYPE), POINTER :: M
TYPE (MULTIPLIER_TYPE), POINTER :: MR
#if defined global_mesh
INTEGER :: II2, JJ2,KK2, NM3, NM4
TYPE (MESH_TYPE), POINTER :: pnt, pnt2      
real :: i1,i2,j1,j2,k1,k2
#endif

NMESHES = 0
NMESHES_READ = 0
CYLINDRICAL_OLD_VALUE = .FALSE.
JBAR_OLD_VALUE = 0

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_MESH_LOOP: DO
   CALL CHECKREAD('MESH',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_MESH_LOOP
   MULT_ID = 'null'
   READ(LU_INPUT,MESH,END=15,ERR=16,IOSTAT=IOS)
   NMESHES_READ = NMESHES_READ + 1
   IF (NMESHES_READ>1 .AND. ((CYLINDRICAL_OLD_VALUE.NEQV.CYLINDRICAL) .OR. (IJK(2)==1).NEQV.(JBAR_OLD_VALUE==1))) THEN
      WRITE(MESSAGE,'(A,A,A,I0)') 'ERROR(110): All meshes must be CYLINDRICAL and/or all meshes must have IJK(2) set to 1'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ELSE
      CYLINDRICAL_OLD_VALUE = CYLINDRICAL
      JBAR_OLD_VALUE = IJK(2)
   ENDIF
   N_MESH_NEW = 0
   IF (MULT_ID=='null') THEN
      N_MESH_NEW = 1
   ELSE
      DO N=1,N_MULT
         MR => MULTIPLIER(N)
         IF (MULT_ID==MR%ID) N_MESH_NEW = MR%N_COPIES
      ENDDO
      IF (N_MESH_NEW==0) THEN
         WRITE(MESSAGE,'(A,A,A,I0,A)') 'ERROR(111): MULT_ID ',TRIM(MULT_ID),' on MESH line ',NMESHES_READ,' not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
   NMESHES      = NMESHES + N_MESH_NEW
16 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with MESH line, line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

ENDDO COUNT_MESH_LOOP
15 CONTINUE

! Stop the calculation if the number of MPI processes is greater than the number of meshes

IF (NMESHES<N_MPI_PROCESSES) THEN
   WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(112): The number of MPI processes, ',N_MPI_PROCESSES,&
                                ', exceeds the number of meshes, ',NMESHES
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

! Allocate parameters associated with the mesh.

ALLOCATE(MESHES(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','MESHES',IZERO)
ALLOCATE(PROCESS(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','PROCESS',IZERO)
ALLOCATE(MESH_NAME(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','MESH_NAME',IZERO)
ALLOCATE(CHANGE_TIME_STEP_INDEX(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','CHANGE_TIME_STEP_INDEX',IZERO)
CHANGE_TIME_STEP_INDEX = 0
ALLOCATE(MAX_CELL_ASPECT_RATIO(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','MAX_CELL_ASPECT_RATIO',IZERO)
MAX_CELL_ASPECT_RATIO = 1._EB
ALLOCATE(SETUP_PRESSURE_ZONES_INDEX(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','SETUP_PRESSURE_ZONES_INDEX',IZERO)
SETUP_PRESSURE_ZONES_INDEX = 0
ALLOCATE(RADIATION_COMPLETED(NMESHES),STAT=IZERO) ; CALL ChkMemErr('READ','RADIATION_COMPLETED',IZERO) ; RADIATION_COMPLETED=.TRUE.

! Read in the Mesh lines from Input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (NMESHES<1) THEN ; CALL SHUTDOWN('ERROR(113): No MESH line(s) defined.') ; RETURN ; ENDIF

NM = 0

MESH_LOOP: DO N=1,NMESHES_READ

   ! Set MESH defaults

   IJK(1)= 10
   IJK(2)= 10
   IJK(3)= 10
   XB(1) = 0._EB
   XB(2) = 1._EB
   XB(3) = 0._EB
   XB(4) = 1._EB
   XB(5) = 0._EB
   XB(6) = 1._EB
   RGB   = -1
   COLOR = 'null'
   ID = 'null'
   MPI_PROCESS = -1
   MULT_ID = 'null'
   TRNX_ID = 'null'
   TRNY_ID = 'null'
   TRNZ_ID = 'null'

   ! Read the MESH line

   CALL CHECKREAD('MESH',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT MESH_LOOP
   READ(LU_INPUT,MESH)

   ! Reorder XB coordinates if necessary

   CALL CHECK_XB(XB)

   ! Multiply meshes if need be

   MR => MULTIPLIER(0)
   DO NNN=1,N_MULT
      IF (MULT_ID==MULTIPLIER(NNN)%ID) MR => MULTIPLIER(NNN)
   ENDDO

   K_MULT_LOOP: DO KK=MR%K_LOWER,MR%K_UPPER
      J_MULT_LOOP: DO JJ=MR%J_LOWER,MR%J_UPPER
         I_MULT_LOOP: DO II=MR%I_LOWER,MR%I_UPPER

            IF (MR%SKIP(II,JJ,KK)) CYCLE I_MULT_LOOP

            IF (.NOT.MR%SEQUENTIAL) THEN
               XB1 = XB(1) + MR%DX0 + II*MR%DXB(1)
               XB2 = XB(2) + MR%DX0 + II*MR%DXB(2)
               XB3 = XB(3) + MR%DY0 + JJ*MR%DXB(3)
               XB4 = XB(4) + MR%DY0 + JJ*MR%DXB(4)
               XB5 = XB(5) + MR%DZ0 + KK*MR%DXB(5)
               XB6 = XB(6) + MR%DZ0 + KK*MR%DXB(6)
            ELSE
               XB1 = XB(1) + MR%DX0 + II*MR%DXB(1)
               XB2 = XB(2) + MR%DX0 + II*MR%DXB(2)
               XB3 = XB(3) + MR%DY0 + II*MR%DXB(3)
               XB4 = XB(4) + MR%DY0 + II*MR%DXB(4)
               XB5 = XB(5) + MR%DZ0 + II*MR%DXB(5)
               XB6 = XB(6) + MR%DZ0 + II*MR%DXB(6)
            ENDIF

            ! Increase the MESH counter by 1

            NM = NM + 1

            ! Determine which PROCESS to assign the MESH to

            IF (MPI_PROCESS>-1) THEN
               CURRENT_MPI_PROCESS = MPI_PROCESS
               IF (CURRENT_MPI_PROCESS>N_MPI_PROCESSES-1) THEN
                  IF (N_MPI_PROCESSES > 1) THEN
                     WRITE(MESSAGE,'(A,I0,A)') 'ERROR(114): MPI_PROCESS for MESH ',NM,' greater than total number of processes'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ELSE
                     ! Prevents fatal error when testing a run on a single core with MPI_PROCESS set for meshes
                     WRITE(MESSAGE,'(A,I0,A)') 'WARNING: MPI_PROCESS set for MESH ',NM,' and only one MPI process exists'
                     IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
                     CURRENT_MPI_PROCESS=0
                  ENDIF
               ENDIF
            ELSE
               IF (N_MPI_PROCESSES>1 .AND. NM>N_MPI_PROCESSES) THEN
                  WRITE(MESSAGE,'(A,A)') 'ERROR(115): Number of meshes exceeds number of MPI processes. ',&
                     ' Set MPI_PROCESS on each MESH line so that each MESH is assigned to a specific MPI process'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               CURRENT_MPI_PROCESS = MIN(NM-1,N_MPI_PROCESSES-1)
            ENDIF

            ! Fill in MESH related variables

            M => MESHES(NM)
            M%TRNX_ID = TRNX_ID
            M%TRNY_ID = TRNY_ID
            M%TRNZ_ID = TRNZ_ID
            M%IBAR = IJK(1)
            M%JBAR = IJK(2)
            M%KBAR = IJK(3)
            IBAR_MAX = MAX(IBAR_MAX,M%IBAR)
            JBAR_MAX = MAX(JBAR_MAX,M%JBAR)
            KBAR_MAX = MAX(KBAR_MAX,M%KBAR)
            M%N_EXTERNAL_WALL_CELLS = 2*M%IBAR*M%JBAR+2*M%IBAR*M%KBAR+2*M%JBAR*M%KBAR

            IF (M%JBAR==1) TWO_D = .TRUE.
            IF (TWO_D .AND. M%JBAR/=1) THEN
               WRITE(MESSAGE,'(A)') 'ERROR(116): IJK(2) must be 1 for all grids in 2D Calculation'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF

            ! Associate the MESH with the PROCESS

            IF (MY_RANK==CURRENT_MPI_PROCESS) THEN
               LOWER_MESH_INDEX = MIN(LOWER_MESH_INDEX,NM)
               UPPER_MESH_INDEX = MAX(UPPER_MESH_INDEX,NM)
            ENDIF

            PROCESS(NM) = CURRENT_MPI_PROCESS
            IF (NM>1) THEN
               IF (PROCESS(NM)-PROCESS(NM-1)>1 .OR. PROCESS(NM-1)>PROCESS(NM)) THEN
                  WRITE(MESSAGE, '(A)') 'ERROR(117): MPI_PROCESS must be continuous and monotonically increasing.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
            ELSE
               IF (PROCESS(NM)/=0) THEN
                  WRITE(MESSAGE, '(A)') 'ERROR(118): MESH 1 must be assigned to MPI_PROCESS 0.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
            ENDIF
            IF (MY_RANK==0 .AND. VERBOSE) WRITE(LU_ERR,'(A,I0,A,I0)') ' Mesh ',NM,' is assigned to MPI Process ',PROCESS(NM)

            ! Mesh boundary colors

            IF (ANY(RGB<0) .AND. COLOR=='null') COLOR = 'BLACK'
            IF (COLOR /= 'null') CALL COLOR2RGB(RGB,COLOR)
            ALLOCATE(M%RGB(3))
            M%RGB = RGB

            ! Mesh Geometry and Name

            WRITE(MESH_NAME(NM),'(A,I7.7)') 'MESH_',NM
            IF (ID/='null') MESH_NAME(NM) = ID

            ! Process Physical Coordinates

            IF (XB2-XB1<TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A,I0)') 'ERROR(119): XB(1)=XB(2) on MESH ', NM
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (XB4-XB3<TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A,I0)') 'ERROR(119): XB(3)=XB(4) on MESH ', NM
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (XB6-XB5<TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A,I0)') 'ERROR(119): XB(5)=XB(6) on MESH ', NM
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (CYLINDRICAL .AND. XB1<-TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A,I0)') 'ERROR(120): XB(1)<0 with CYLINDRICAL on MESH ', NM
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (CYLINDRICAL .AND. .NOT.TWO_D) THEN
               WRITE(MESSAGE,'(A,I0)') 'ERROR(121): J>1 with CYLINDRICAL on MESH ', NM
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF

            M%XS    = XB1
            M%XF    = XB2
            M%YS    = XB3
            M%YF    = XB4
            M%ZS    = XB5
            M%ZF    = XB6
            XS_MIN  = MIN(XS_MIN,M%XS)
            XF_MAX  = MAX(XF_MAX,M%XF)
            YS_MIN  = MIN(YS_MIN,M%YS)
            YF_MAX  = MAX(YF_MAX,M%YF)
            ZS_MIN  = MIN(ZS_MIN,M%ZS)
            ZF_MAX  = MAX(ZF_MAX,M%ZF)
            M%DXI   = (M%XF-M%XS)/REAL(M%IBAR,EB)
            M%DETA  = (M%YF-M%YS)/REAL(M%JBAR,EB)
            M%DZETA = (M%ZF-M%ZS)/REAL(M%KBAR,EB)
            M%RDXI  = 1._EB/M%DXI
            M%RDETA = 1._EB/M%DETA
            M%RDZETA= 1._EB/M%DZETA
            M%IBM1  = M%IBAR-1
            M%JBM1  = M%JBAR-1
            M%KBM1  = M%KBAR-1
            M%IBP1  = M%IBAR+1
            M%JBP1  = M%JBAR+1
            M%KBP1  = M%KBAR+1

            IF (TWO_D) THEN
               M%CELL_SIZE = SQRT(M%DXI*M%DZETA)
            ELSE
               M%CELL_SIZE = (M%DXI*M%DETA*M%DZETA)**ONTH
            ENDIF

            CHARACTERISTIC_CELL_SIZE = MIN( CHARACTERISTIC_CELL_SIZE , M%CELL_SIZE )

         ENDDO I_MULT_LOOP
      ENDDO J_MULT_LOOP
   ENDDO K_MULT_LOOP

#if defined global_mesh
   NM3=0  
   K_MULT_LOOP2: DO KK=MR%K_LOWER,MR%K_UPPER
      J_MULT_LOOP2: DO JJ=MR%J_LOWER,MR%J_UPPER
         I_MULT_LOOP2: DO II=MR%I_LOWER,MR%I_UPPER
            NM3=NM3+1
            M => MESHES(NM3)

            M%MI = II
            M%MJ = JJ
            M%MK = KK
            
            M%GK1 = 0
            M%GK2 = 0
            M%GI1 = 0
            M%GI2 = 0            
            M%GJ1 = 0
            M%GJ2 = 0      
            
            IF (NM3.eq.1) THEN
            ! we can find corner coordinates
            	! add +1 on each side for zero and IBP1
            	! corrected 18 July to include ghosts

            M%GI1 = 0
            M%GI2 = M%IBAR +1         
            M%GJ1 = 0
            M%GJ2 = M%JBAR +1
            M%GK1 = 0
            M%GK2 = M%KBAR  +1            
            ENDIF
            
!# if defined coupled_bc            
!            IF ((II.eq.0) .OR.(JJ.eq.0).OR.(KK.eq.0)) THEN
!             M%COUPLED=1
!            ELSE
!             M%COUPLED=0
!            ENDIF
!# endif            
            
       ENDDO I_MULT_LOOP2
      ENDDO J_MULT_LOOP2
   ENDDO K_MULT_LOOP2
   
               
  ! we need other meshes to know where this one is 
   NM3=0
   K_MULT_LOOP3: DO KK=MR%K_LOWER,MR%K_UPPER
    J_MULT_LOOP3: DO JJ=MR%J_LOWER,MR%J_UPPER
     I_MULT_LOOP3: DO II=MR%I_LOWER,MR%I_UPPER
      NM3=NM3+1           
      pnt => MESHES(NM3)
      IF (NM3>1) THEN

       !corrected 18 July 2025 to match coupled_glue_meshed which I trust ?!
       i1=0
       i2=1
       j1=0
       j2=1
       k1=0
       k2=1

       DO KK2=(MR%K_LOWER),pnt%MK  
        DO JJ2=(MR%J_LOWER),pnt%MJ
         DO II2=(MR%I_LOWER),pnt%MI                
          !previos mesh number based on loop:
          !KK*9+JJ*3 + (II+1)=KK*(I_UPPER+1)*(J_UPPER+1)  JJ*(I_UPPER+1) + (II+1)
             
          NM4=KK2*(MR%I_UPPER +1)*(MR%J_UPPER +1)+JJ2*(MR%I_UPPER +1) + (II2+1)
          pnt2 => MESHES(NM4)
          IF (NM4.le.NM3) THEN
          
           IF ((pnt2%MI .eq. pnt%MI ).AND.(pnt2%MJ .lt. pnt%MJ ))  THEN
           j1=j1 + pnt2%JBAR
           j2=j2 + pnt2%JBAR
           !j1=j1 + pnt2%JBP1
           !j2=j2 + pnt2%JBP1
           ENDIF
           IF ((pnt2%MI .eq. pnt%MI ).AND.(pnt2%MJ .eq. pnt%MJ ))  THEN
            j2 = j2 + pnt2%JBAR
            !j2 = j2 + pnt2%JBP1
           ENDIF
           IF ((pnt2%MJ .eq. pnt%MJ ).AND.(pnt2%MI .lt. pnt%MI ))  THEN
            i1 = i1 + pnt2%IBAR
            i2 = i2 + pnt2%IBAR
            !i1 = i1 + pnt2%IBP1
            !i2 = i2 + pnt2%IBP1
           ENDIF
           IF ((pnt2%MJ .eq. pnt%MJ ).AND.(pnt2%MI .eq. pnt%MI ))  THEN
            i2 = i2 + pnt2%IBAR
            !i2 = i2 + pnt2%IBP1
           ENDIF
           IF ((pnt2%MI .eq. pnt%MI ).AND. (pnt2%MJ .eq. pnt%MJ ).AND. &
               (pnt2%MK .lt. pnt%MK )) THEN
            k1 = k1 + pnt2%KBAR
            k2 = k2 + pnt2%KBAR
            !k1 = k1 + pnt2%KBP1
            !k2 = k2 + pnt2%KBP1
           ENDIF
           IF ((pnt2%MI .eq. pnt%MI ).AND. (pnt2%MJ .eq. pnt%MJ ).AND. &
               (pnt2%MK .eq. pnt%MK )) THEN
            k2 = k2 + pnt2%KBAR
            !k2 = k2 + pnt2%KBP1
           ENDIF       
           

           
           
           pnt%GI1 = i1
           pnt%GI2 = i2              
           pnt%GJ1 = j1
           pnt%GJ2 = j2
           pnt%GK1 = k1
           pnt%GK2 = k2              
          ENDIF
         ENDDO
        ENDDO
       ENDDO !end of NM4 loops             
      ENDIF  !if NM3>1
      
#if defined vent_debug
!print*, 'set up global mesh', NM3, pnt%GI1, pnt%GI2,pnt%GJ1,pnt%GJ2,pnt%GK1,pnt%GK2  
#endif

     
      
     ENDDO I_MULT_LOOP3
    ENDDO J_MULT_LOOP3
   ENDDO K_MULT_LOOP3

#endif 




ENDDO MESH_LOOP

! Check if there are too many MPI processes assigned to the job

IF (PROCESS(NMESHES) < N_MPI_PROCESSES-1) THEN
   WRITE(MESSAGE,'(A)') 'ERROR(122): Too many MPI processes have been assigned to this job.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

! Check for bad mesh ordering if MPI_PROCESS used

DO NM=1,NMESHES
   IF (NM==1) CYCLE
   IF (PROCESS(NM) < PROCESS(NM-1)) THEN
      WRITE(MESSAGE,'(A)') 'ERROR(117): MPI_PROCESS must be continuous and monotonically increasing.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO

END SUBROUTINE READ_MESH


!> \brief Determine mesh neighborhoods for MPI communications

SUBROUTINE PROC_MESH

USE COMP_FUNCTIONS, ONLY : SEARCH_INPUT_FILE
INTEGER :: NM,NM2,I,II,III,N_GROUPS=0
INTEGER, ALLOCATABLE, DIMENSION(:) :: NEIGHBOR_LIST
LOGICAL :: OVERLAPPING_X,OVERLAPPING_Y,OVERLAPPING_Z,POSSIBLY_PERIODIC,PERIODIC_FOUND_IN_FILE
TYPE (MESH_TYPE), POINTER :: M,M2
TYPE (HT3D_OBST_TYPE), POINTER :: HO,HO2,HO3
TYPE MESH_COMM_TYPE
   INTEGER :: N_MESHES=0
   INTEGER, DIMENSION(100) :: LIST=0
END TYPE MESH_COMM_TYPE
TYPE(MESH_COMM_TYPE), ALLOCATABLE, DIMENSION(:), TARGET :: MESH_COMM
TYPE(MESH_COMM_TYPE), POINTER :: MC

! Read the SURF lines and make a list, HT3D_SURF_LIST, of those that are HT3D or VARIABLE_THICKNESS

CALL READ_SURF(QUICK_READ=.TRUE.)

! If there are HT3D solids, determine the indices of the meshes that contain connected HT3D OBSTs

IF_HT3D: IF (N_HT3D_SURF_LINES>0) THEN

   ! Read the OBST lines and make a list, HT3D_OBST, of those that have HT3D or VARIABLE_THICKNESS SURF lines

   ALLOCATE(HT3D_OBST(100))  ! Initial allocation -- this array can be reallocated if needed
   CALL READ_OBST(QUICK_READ=.TRUE.)

   ! Assign each HT3D_OBST an integer, GROUP_INDEX, which indicates which group of obstructions it is connected to.
   ! N_GROUPS is the number of these blocks of connected OBSTs.

   N_GROUPS = 1

   HT3D_OBST_LOOP: DO I=1,N_HT3D_OBST
      HO => HT3D_OBST(I)
      IF (I==1) THEN
         HO%GROUP_INDEX = 1
      ELSE
         HO%GROUP_INDEX = 1000000
         DO II=I-1,1,-1
            HO2 => HT3D_OBST(II)
            IF (HO%XS>HO2%XF.OR.HO%XF<HO2%XS.OR.HO%YS>HO2%YF.OR.HO%YF<HO2%YS.OR.HO%ZS>HO2%ZF.OR.HO%ZF<HO2%ZS) CYCLE
            IF (HO2%GROUP_INDEX>HO%GROUP_INDEX) THEN  ! If current OBST is connected to one already assigned a GROUP_INDEX,
                                                      ! and that GROUP_INDEX is greater than that of the OBST, reduce the
                                                      ! N_GROUPS and GROUP_INDEXs of OBSTs with higher values
               N_GROUPS = N_GROUPS - 1
               DO III=1,I-1
                  HO3 => HT3D_OBST(III)
                  IF (HO3%GROUP_INDEX>HO%GROUP_INDEX) HO3%GROUP_INDEX = HO3%GROUP_INDEX - 1
               ENDDO
            ENDIF
            HO%GROUP_INDEX = MIN(HO%GROUP_INDEX,HO2%GROUP_INDEX)
            IF (HO%GROUP_INDEX==N_GROUPS) CYCLE HT3D_OBST_LOOP
         ENDDO
         IF (HO%GROUP_INDEX>100000) THEN  ! The current OBST is not connected to any previous OBST. Assign it a new GROUP_INDEX.
            N_GROUPS = N_GROUPS + 1
            HO%GROUP_INDEX = N_GROUPS
         ENDIF
      ENDIF
   ENDDO HT3D_OBST_LOOP

   ! Now determine all the MESH indices corresponding to each group of connected OBSTs

   ALLOCATE(MESH_COMM(N_GROUPS))

   DO I=1,N_HT3D_OBST
      HO => HT3D_OBST(I)
      MC => MESH_COMM(HO%GROUP_INDEX)
      MESH_LOOP: DO NM=1,NMESHES
         M => MESHES(NM)
         IF (HO%XS>M%XF.OR.HO%XF<M%XS.OR.HO%YS>M%YF.OR.HO%YF<M%YS.OR.HO%ZS>M%ZF.OR.HO%ZF<M%ZS) CYCLE MESH_LOOP
         DO II=1,MC%N_MESHES
            IF (NM==MC%LIST(II)) CYCLE MESH_LOOP
         ENDDO
         MC%N_MESHES = MC%N_MESHES + 1
         MC%LIST(MC%N_MESHES) = NM
      ENDDO MESH_LOOP
   ENDDO

   DEALLOCATE(HT3D_OBST)

ENDIF IF_HT3D

! MESH_SEPARATION_DISTANCE is a very small length used to determine if there are periodic boundaries. NEIGHBOR_SEPARATION_DISANCE
! is the distance beyond which no information or message passing is assumed between the meshes. Its value is deliberately
! complicated to avoid having two meshes separated by exactly that same distance.

MESH_SEPARATION_DISTANCE = MIN(1.E-3_EB,0.05_EB*CHARACTERISTIC_CELL_SIZE)
IF (NEIGHBOR_SEPARATION_DISTANCE<0._EB) NEIGHBOR_SEPARATION_DISTANCE = 4.56789_EB*CHARACTERISTIC_CELL_SIZE

! Search through the input file for any mention of the word PERIODIC. If not found, this simplifies neighbor selection.

CALL SEARCH_INPUT_FILE(LU_INPUT,'PERIODIC',PERIODIC_FOUND_IN_FILE)

! For MESHES controlled by a given MPI process, only allocate other MESHES that are "close" as defined by the two parameters above.

ALLOCATE(NEIGHBOR_LIST(10000))

DO NM=1,NMESHES

   M => MESHES(NM)
   M%N_NEIGHBORING_MESHES = 0
   NEIGHBOR_LIST = 0

   ! Add adjacent meshes to the neighborhood of MESH NM

   DO NM2=1,NMESHES
      M2 => MESHES(NM2)
      OVERLAPPING_X = .TRUE.
      OVERLAPPING_Y = .TRUE.
      OVERLAPPING_Z = .TRUE.
      POSSIBLY_PERIODIC = .FALSE.
      IF (M2%XS>M%XF+NEIGHBOR_SEPARATION_DISTANCE .OR.  M2%XF<M%XS-NEIGHBOR_SEPARATION_DISTANCE) OVERLAPPING_X = .FALSE.
      IF (M2%YS>M%YF+NEIGHBOR_SEPARATION_DISTANCE .OR.  M2%YF<M%YS-NEIGHBOR_SEPARATION_DISTANCE) OVERLAPPING_Y = .FALSE.
      IF (M2%ZS>M%ZF+NEIGHBOR_SEPARATION_DISTANCE .OR.  M2%ZF<M%ZS-NEIGHBOR_SEPARATION_DISTANCE) OVERLAPPING_Z = .FALSE.
      IF (((ABS(M2%XS-XS_MIN)<MESH_SEPARATION_DISTANCE .AND. ABS( M%XF-XF_MAX)<MESH_SEPARATION_DISTANCE)  .OR.  &
           (ABS( M%XS-XS_MIN)<MESH_SEPARATION_DISTANCE .AND. ABS(M2%XF-XF_MAX)<MESH_SEPARATION_DISTANCE)) .AND. &
          OVERLAPPING_Y .AND. OVERLAPPING_Z) POSSIBLY_PERIODIC = .TRUE.
      IF (((ABS(M2%YS-YS_MIN)<MESH_SEPARATION_DISTANCE .AND. ABS( M%YF-YF_MAX)<MESH_SEPARATION_DISTANCE)  .OR.  &
           (ABS( M%YS-YS_MIN)<MESH_SEPARATION_DISTANCE .AND. ABS(M2%YF-YF_MAX)<MESH_SEPARATION_DISTANCE)) .AND. &
          OVERLAPPING_X .AND. OVERLAPPING_Z) POSSIBLY_PERIODIC = .TRUE.
      IF (((ABS(M2%ZS-ZS_MIN)<MESH_SEPARATION_DISTANCE .AND. ABS( M%ZF-ZF_MAX)<MESH_SEPARATION_DISTANCE)  .OR.  &
           (ABS( M%ZS-ZS_MIN)<MESH_SEPARATION_DISTANCE .AND. ABS(M2%ZF-ZF_MAX)<MESH_SEPARATION_DISTANCE)) .AND. &
          OVERLAPPING_X .AND. OVERLAPPING_Y) POSSIBLY_PERIODIC = .TRUE.
      IF (.NOT.PERIODIC_FOUND_IN_FILE) POSSIBLY_PERIODIC = .FALSE.
      IF ((.NOT.OVERLAPPING_X .OR. .NOT.OVERLAPPING_Y .OR. .NOT.OVERLAPPING_Z) .AND. .NOT.POSSIBLY_PERIODIC) CYCLE
      M%N_NEIGHBORING_MESHES = M%N_NEIGHBORING_MESHES + 1
      NEIGHBOR_LIST(M%N_NEIGHBORING_MESHES) = NM2
   ENDDO

   ! Add meshes containing the HT3D_OBST groups to the neighborhood of MESH NM

   DO I=1,N_GROUPS
      MC => MESH_COMM(I)
      IF (ANY(MC%LIST==NM,DIM=1)) THEN
         DO II=1,MC%N_MESHES
            IF (ANY(NEIGHBOR_LIST==MC%LIST(II))) CYCLE
            M%N_NEIGHBORING_MESHES = M%N_NEIGHBORING_MESHES + 1
            NEIGHBOR_LIST(M%N_NEIGHBORING_MESHES) = MC%LIST(II)
         ENDDO
      ENDIF
   ENDDO

   ! Save the list of neighboring meshes into an array

   ALLOCATE(M%NEIGHBORING_MESH(M%N_NEIGHBORING_MESHES))
   DO I=1,M%N_NEIGHBORING_MESHES
      M%NEIGHBORING_MESH(I) = NEIGHBOR_LIST(I)
   ENDDO
ENDDO

DEALLOCATE(NEIGHBOR_LIST)

END SUBROUTINE PROC_MESH


!> \brief Read the TRAN namelist lines and compute the polynomial transform function for the vertical coordinate

SUBROUTINE READ_TRAN

USE MATH_FUNCTIONS, ONLY : GAUSSJ
CHARACTER(LABEL_LENGTH) :: ID
REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: A,XX
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ND
REAL(EB) :: PC,CC,COEF,XI,ETA,ZETA
INTEGER  IEXP,IC,IDERIV,N,K,IOS,I,MESH_NUMBER,NIPX,NIPY,NIPZ,NIPXS,NIPYS,NIPZS,NIPXF,NIPYF,NIPZF,NM
LOGICAL :: PROCESS_TRANS
TYPE (MESH_TYPE), POINTER :: M=>NULL()
TYPE (TRAN_TYPE), POINTER :: T=>NULL()
NAMELIST /TRNX/ CC,FYI,ID,IDERIV,MESH_NUMBER,PC
NAMELIST /TRNY/ CC,FYI,ID,IDERIV,MESH_NUMBER,PC
NAMELIST /TRNZ/ CC,FYI,ID,IDERIV,MESH_NUMBER,PC

! Scan the input file, counting the number of NAMELIST entries

ALLOCATE(TRANS(NMESHES))

MESH_LOOP: DO NM=1,NMESHES

   M => MESHES(NM)

   ! Only read and process the TRNX, TRNY and TRNZ lines if the current MPI
   ! process (MY_RANK) controls mesh NM or one of its neighbors.

   PROCESS_TRANS = .FALSE.
   DO N=1,M%N_NEIGHBORING_MESHES
      IF (MY_RANK==PROCESS(M%NEIGHBORING_MESH(N))) PROCESS_TRANS = .TRUE.
   ENDDO

   IF (.NOT.PROCESS_TRANS) CYCLE MESH_LOOP

   T => TRANS(NM)

   DO N=1,3
      T%NOC(N) = 0
      REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
      TRNLOOP: DO
         ID = 'null'
         SELECT CASE (N)
            CASE(1)
               CALL CHECKREAD('TRNX',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
               IF (IOS==1) EXIT TRNLOOP
               MESH_NUMBER = 1
               READ(LU_INPUT,NML=TRNX,END=17,ERR=18,IOSTAT=IOS)
               IF (ID/='null') THEN
                  MESH_NUMBER = HUGE(1)
                  IF (TRIM(M%TRNX_ID)==TRIM(ID)) MESH_NUMBER=NM
               ENDIF
               IF (MESH_NUMBER>0 .AND. MESH_NUMBER/=NM) CYCLE TRNLOOP
            CASE(2)
               CALL CHECKREAD('TRNY',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
               IF (IOS==1) EXIT TRNLOOP
               MESH_NUMBER = 1
               READ(LU_INPUT,NML=TRNY,END=17,ERR=18,IOSTAT=IOS)
               IF (ID/='null') THEN
                  MESH_NUMBER = HUGE(1)
                  IF (TRIM(M%TRNY_ID)==TRIM(ID)) MESH_NUMBER=NM
               ENDIF
               IF (MESH_NUMBER>0 .AND. MESH_NUMBER/=NM) CYCLE TRNLOOP
            CASE(3)
               CALL CHECKREAD('TRNZ',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
               IF (IOS==1) EXIT TRNLOOP
               MESH_NUMBER = 1
               READ(LU_INPUT,NML=TRNZ,END=17,ERR=18,IOSTAT=IOS)
               IF (ID/='null') THEN
                  MESH_NUMBER = HUGE(1)
                  IF (TRIM(M%TRNZ_ID)==TRIM(ID)) MESH_NUMBER=NM
               ENDIF
               IF (MESH_NUMBER>0 .AND. MESH_NUMBER/=NM) CYCLE TRNLOOP
         END SELECT
         T%NOC(N) = T%NOC(N) + 1
         18 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with TRN* line') ; RETURN ; ENDIF
      ENDDO TRNLOOP
      17 CONTINUE
   ENDDO

   T%NOCMAX = MAX(T%NOC(1),T%NOC(2),T%NOC(3))
   ALLOCATE(A(T%NOCMAX+1,T%NOCMAX+1))
   ALLOCATE(XX(T%NOCMAX+1,3))
   ALLOCATE(ND(T%NOCMAX+1,3))
   ALLOCATE(T%C1(0:T%NOCMAX+1,3))
   T%C1               = 0._EB
   T%C1(1,1:3)        = 1._EB
   ALLOCATE(T%C2(0:T%NOCMAX+1,3))
   ALLOCATE(T%C3(0:T%NOCMAX+1,3))
   ALLOCATE(T%CCSTORE(T%NOCMAX,3))
   ALLOCATE(T%PCSTORE(T%NOCMAX,3))
   ALLOCATE(T%IDERIVSTORE(T%NOCMAX,3))

   T%ITRAN  = 0

   ICLOOP_1: DO IC=1,3
      REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
      NLOOP: DO N=1,T%NOC(IC)
         IDERIV = -1
         ID = 'null'
         IC_SELECT: SELECT CASE(IC)
            CASE(1)
               LOOP1: DO
                  CALL CHECKREAD('TRNX',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
                  IF (IOS==1) EXIT NLOOP
                  MESH_NUMBER = 1
                  READ(LU_INPUT,TRNX,END=1,ERR=2)
                  IF (ID/='null') THEN
                     MESH_NUMBER = HUGE(1)
                     IF (TRIM(M%TRNX_ID)==TRIM(ID)) MESH_NUMBER=NM
                  ENDIF
                  IF (MESH_NUMBER==0 .OR. MESH_NUMBER==NM) EXIT LOOP1
               ENDDO LOOP1
            CASE(2)
               LOOP2: DO
                  CALL CHECKREAD('TRNY',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
                  IF (IOS==1) EXIT NLOOP
                  MESH_NUMBER = 1
                  READ(LU_INPUT,TRNY,END=1,ERR=2)
                  IF (ID/='null') THEN
                     MESH_NUMBER = HUGE(1)
                     IF (TRIM(M%TRNY_ID)==TRIM(ID)) MESH_NUMBER=NM
                  ENDIF
                  IF (MESH_NUMBER==0 .OR. MESH_NUMBER==NM) EXIT LOOP2
               ENDDO LOOP2
            CASE(3)
               LOOP3: DO
                  CALL CHECKREAD('TRNZ',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
                  IF (IOS==1) EXIT NLOOP
                  MESH_NUMBER = 1
                  READ(LU_INPUT,TRNZ,END=1,ERR=2)
                  IF (ID/='null') THEN
                     MESH_NUMBER = HUGE(1)
                     IF (TRIM(M%TRNZ_ID)==TRIM(ID)) MESH_NUMBER=NM
                  ENDIF
                  IF (MESH_NUMBER==0 .OR. MESH_NUMBER==NM) EXIT LOOP3
               ENDDO LOOP3
         END SELECT IC_SELECT
         T%CCSTORE(N,IC) = CC
         T%PCSTORE(N,IC) = PC
         T%IDERIVSTORE(N,IC) = IDERIV
         IF (IDERIV>=0) T%ITRAN(IC) = 1
         IF (IDERIV<0)  T%ITRAN(IC) = 2
      2 CONTINUE
      ENDDO NLOOP
      1 CONTINUE
   ENDDO ICLOOP_1

   ICLOOP_2: DO IC=1,3

      SELECT CASE (T%ITRAN(IC))

         CASE (1)  ! polynomial transformation

            ND(1,IC)  = 0
            SELECT CASE(IC)
               CASE(1)
                  XX(1,IC)    = M%XF-M%XS
                  T%C1(1,IC)  = M%XF-M%XS
               CASE(2)
                  XX(1,IC)    = M%YF-M%YS
                  T%C1(1,IC)  = M%YF-M%YS
               CASE(3)
                  XX(1,IC)    = M%ZF-M%ZS
                  T%C1(1,IC)  = M%ZF-M%ZS
            END SELECT

            NNLOOP:  DO N=2,T%NOC(IC)+1
               IDERIV = T%IDERIVSTORE(N-1,IC)
               IF (IC==1) CC = T%CCSTORE(N-1,IC)-M%XS
               IF (IC==2) CC = T%CCSTORE(N-1,IC)-M%YS
               IF (IC==3) CC = T%CCSTORE(N-1,IC)-M%ZS
               IF (IC==1 .AND. IDERIV==0) PC = T%PCSTORE(N-1,IC)-M%XS
               IF (IC==2 .AND. IDERIV==0) PC = T%PCSTORE(N-1,IC)-M%YS
               IF (IC==3 .AND. IDERIV==0) PC = T%PCSTORE(N-1,IC)-M%ZS
               IF (IC==1 .AND. IDERIV>0) PC = T%PCSTORE(N-1,IC)
               IF (IC==2 .AND. IDERIV>0) PC = T%PCSTORE(N-1,IC)
               IF (IC==3 .AND. IDERIV>0) PC = T%PCSTORE(N-1,IC)
               ND(N,IC) = IDERIV
               XX(N,IC) = CC
               T%C1(N,IC) = PC
            ENDDO NNLOOP

            DO K=1,T%NOC(IC)+1
               DO N=1,T%NOC(IC)+1
                  COEF = IFAC(K,ND(N,IC))
                  IEXP = K-ND(N,IC)
                  IF (IEXP<0) A(N,K) = 0._EB
                  IF (IEXP==0) A(N,K) = COEF
                  IF (IEXP>0) A(N,K) = COEF*XX(N,IC)**IEXP
               ENDDO
            ENDDO

            IERROR = 0
            CALL GAUSSJ(A,T%NOC(IC)+1,T%NOCMAX+1,T%C1(1:T%NOCMAX+1,IC),1,1,IERROR)
            IF (IERROR/=0) THEN ; CALL SHUTDOWN('ERROR(124): Problem with grid transformation.') ; RETURN ; ENDIF

         CASE (2)  ! linear transformation

            T%C1(0,IC) = 0._EB
            T%C2(0,IC) = 0._EB
            DO N=1,T%NOC(IC)
               IF (IC==1) CC = T%CCSTORE(N,IC)-M%XS
               IF (IC==2) CC = T%CCSTORE(N,IC)-M%YS
               IF (IC==3) CC = T%CCSTORE(N,IC)-M%ZS
               IF (IC==1) PC = T%PCSTORE(N,IC)-M%XS
               IF (IC==2) PC = T%PCSTORE(N,IC)-M%YS
               IF (IC==3) PC = T%PCSTORE(N,IC)-M%ZS
               T%C1(N,IC) = CC
               T%C2(N,IC) = PC
            ENDDO

            SELECT CASE(IC)
               CASE(1)
                  T%C1(T%NOC(1)+1,1) = M%XF-M%XS
                  T%C2(T%NOC(1)+1,1) = M%XF-M%XS
               CASE(2)
                  T%C1(T%NOC(2)+1,2) = M%YF-M%YS
                  T%C2(T%NOC(2)+1,2) = M%YF-M%YS
               CASE(3)
                  T%C1(T%NOC(3)+1,3) = M%ZF-M%ZS
                  T%C2(T%NOC(3)+1,3) = M%ZF-M%ZS
            END SELECT

            DO N=1,T%NOC(IC)+1
               IF (T%C1(N,IC)-T%C1(N-1,IC)<TWO_EPSILON_EB) THEN
                  CALL SHUTDOWN('ERROR(125): Do not specify endpoints in linear grid transformation.')
                  RETURN
               ENDIF
               T%C3(N,IC) = (T%C2(N,IC)-T%C2(N-1,IC))/(T%C1(N,IC)-T%C1(N-1,IC))
            ENDDO
      END SELECT
   ENDDO ICLOOP_2

   DEALLOCATE(A)
   DEALLOCATE(XX)
   DEALLOCATE(ND)

   ! Set up grid stretching arrays

   ALLOCATE(M%R(0:M%IBAR),STAT=IZERO)
   CALL ChkMemErr('READ','R',IZERO)
   ALLOCATE(M%RC(0:M%IBAR+1),STAT=IZERO)
   CALL ChkMemErr('READ','RC',IZERO)
   M%RC = 1._EB
   ALLOCATE(M%RRN(0:M%IBP1),STAT=IZERO)
   CALL ChkMemErr('READ','RRN',IZERO)
   M%RRN = 1._EB
   ALLOCATE(M%X(0:M%IBAR),STAT=IZERO)
   CALL ChkMemErr('READ','X',IZERO)
   ALLOCATE(M%XC(0:M%IBP1),STAT=IZERO)
   CALL ChkMemErr('READ','XC',IZERO)
   ALLOCATE(M%HX(0:M%IBP1),STAT=IZERO)
   CALL ChkMemErr('READ','HX',IZERO)
   ALLOCATE(M%DX(0:M%IBP1),STAT=IZERO)
   CALL ChkMemErr('READ','DX',IZERO)
   ALLOCATE(M%RDX(0:M%IBP1),STAT=IZERO)
   CALL ChkMemErr('READ','RDX',IZERO)
   ALLOCATE(M%DXN(0:M%IBAR),STAT=IZERO)
   CALL ChkMemErr('READ','DXN',IZERO)
   ALLOCATE(M%RDXN(0:M%IBAR),STAT=IZERO)
   CALL ChkMemErr('READ','RDXN',IZERO)
   ALLOCATE(M%Y(0:M%JBAR),STAT=IZERO)
   CALL ChkMemErr('READ','Y',IZERO)
   ALLOCATE(M%YC(0:M%JBP1),STAT=IZERO)
   CALL ChkMemErr('READ','YC',IZERO)
   ALLOCATE(M%HY(0:M%JBP1),STAT=IZERO)
   CALL ChkMemErr('READ','HY',IZERO)
   ALLOCATE(M%DY(0:M%JBP1),STAT=IZERO)
   CALL ChkMemErr('READ','DY',IZERO)
   ALLOCATE(M%RDY(0:M%JBP1),STAT=IZERO)
   CALL ChkMemErr('READ','RDY',IZERO)
   ALLOCATE(M%DYN(0:M%JBAR),STAT=IZERO)
   CALL ChkMemErr('READ','DYN',IZERO)
   ALLOCATE(M%RDYN(0:M%JBAR),STAT=IZERO)
   CALL ChkMemErr('READ','RDYN',IZERO)
   ALLOCATE(M%Z(0:M%KBAR),STAT=IZERO)
   CALL ChkMemErr('READ','Z',IZERO)
   ALLOCATE(M%ZC(0:M%KBP1),STAT=IZERO)
   CALL ChkMemErr('READ','ZC',IZERO)
   ALLOCATE(M%HZ(0:M%KBP1),STAT=IZERO)
   CALL ChkMemErr('READ','HZ',IZERO)
   ALLOCATE(M%DZ(0:M%KBP1),STAT=IZERO)
   CALL ChkMemErr('READ','DZ',IZERO)
   ALLOCATE(M%RDZ(0:M%KBP1),STAT=IZERO)
   CALL ChkMemErr('READ','RDZ',IZERO)
   ALLOCATE(M%DZN(0:M%KBAR),STAT=IZERO)
   CALL ChkMemErr('READ','DZN',IZERO)
   ALLOCATE(M%RDZN(0:M%KBAR),STAT=IZERO)
   CALL ChkMemErr('READ','RDZN',IZERO)

   ! Define X grid stretching terms

   M%DXMIN = 1000000._EB

   DO I=0,M%IBAR
      XI     = I*M%DXI
      M%X(I) = M%XS + G(XI,1,NM)
      IF (CYLINDRICAL) THEN
         M%R(I) = M%X(I)
      ELSE
         M%R(I) = 1._EB
      ENDIF
      IF (I>0) THEN
         M%DX(I) = M%X(I) - M%X(I-1)
         M%HX(I) = M%DX(I)/M%DXI
         M%DXMIN = MIN(M%DXMIN,M%DX(I))
         IF (M%HX(I)<=0._EB) THEN
            WRITE(MESSAGE,'(A,I0)')  'ERROR(126): x transformation not monotonic, MESH ',NM
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         M%RDX(I) = 1._EB/M%DX(I)
      ENDIF
   ENDDO
   M%X(0)       = M%XS
   M%X(M%IBAR)  = M%XF
   M%HX(0)      = M%HX(1)
   M%HX(M%IBP1) = M%HX(M%IBAR)
   IF (T%NOC(1)==0) M%HX = 1._EB
   M%DX(0)      = M%DX(1)
   M%DX(M%IBP1) = M%DX(M%IBAR)
   M%RDX(0)     = 1._EB/M%DX(1)
   M%RDX(M%IBP1)= 1._EB/M%DX(M%IBAR)

   DO I=0,M%IBAR
      M%DXN(I)  = 0.5_EB*(M%DX(I)+M%DX(I+1))
      M%RDXN(I) = 1._EB/M%DXN(I)
      IF (I>0) M%XC(I) = 0.5_EB*(M%X(I)+M%X(I-1))
   ENDDO
   M%XC(0)      = M%XS - 0.5_EB*M%DX(0)
   M%XC(M%IBP1) = M%XF + 0.5_EB*M%DX(M%IBP1)

   IF (CYLINDRICAL) THEN
      DO I=1,M%IBAR
         M%RRN(I) = 2._EB/(M%R(I)+M%R(I-1))
         M%RC(I)  = 0.5_EB*(M%R(I)+M%R(I-1))
      ENDDO
      M%RRN(0)    = M%RRN(1)
      M%RRN(M%IBP1) = M%RRN(M%IBAR)
   ENDIF

   ! Define Y grid stretching terms

   M%DYMIN = 1000000._EB

   DO J=0,M%JBAR
      ETA    = J*M%DETA
      M%Y(J) = M%YS + G(ETA,2,NM)
      IF (J>0) THEN
         M%DY(J) = M%Y(J) - M%Y(J-1)
         M%HY(J) = M%DY(J)/M%DETA
         M%DYMIN = MIN(M%DYMIN,M%DY(J))
         IF (M%HY(J)<=0._EB) THEN
            WRITE(MESSAGE,'(A,I0)')  'ERROR(126): y transformation not monotonic, MESH ',NM
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         M%RDY(J) = 1._EB/M%DY(J)
      ENDIF
   ENDDO
   M%Y(0)       = M%YS
   M%Y(M%JBAR)  = M%YF
   M%HY(0)      = M%HY(1)
   M%HY(M%JBP1) = M%HY(M%JBAR)
   IF (T%NOC(2)==0) M%HY = 1._EB
   M%DY(0)      = M%DY(1)
   M%DY(M%JBP1) = M%DY(M%JBAR)
   M%RDY(0)     = 1._EB/M%DY(1)
   M%RDY(M%JBP1)= 1._EB/M%DY(M%JBAR)

   DO J=0,M%JBAR
      M%DYN(J)  = 0.5_EB*(M%DY(J)+M%DY(J+1))
      M%RDYN(J) = 1._EB/M%DYN(J)
      IF (J>0) M%YC(J) = 0.5_EB*(M%Y(J)+M%Y(J-1))
   ENDDO
   M%YC(0)      = M%YS - 0.5_EB*M%DY(0)
   M%YC(M%JBP1) = M%YF + 0.5_EB*M%DY(M%JBP1)

   ! Define Z grid stretching terms

   M%DZMIN = 1000000._EB

   DO K=0,M%KBAR
      ZETA   = K*M%DZETA
      M%Z(K) = M%ZS + G(ZETA,3,NM)
      IF (K>0) THEN
         M%DZ(K) = M%Z(K) - M%Z(K-1)
         M%HZ(K) = M%DZ(K)/M%DZETA
         M%DZMIN = MIN(M%DZMIN,M%DZ(K))
         IF (M%HZ(K)<=0._EB) THEN
            WRITE(MESSAGE,'(A,I0)') 'ERROR(126): z transformation not monotonic, MESH ',NM
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         M%RDZ(K) = 1._EB/M%DZ(K)
      ENDIF
   ENDDO
   M%Z(0)       = M%ZS
   M%Z(M%KBAR)  = M%ZF
   M%HZ(0)      = M%HZ(1)
   M%HZ(M%KBP1) = M%HZ(M%KBAR)
   IF (T%NOC(3)==0) M%HZ = 1._EB
   M%DZ(0)      = M%DZ(1)
   M%DZ(M%KBP1) = M%DZ(M%KBAR)
   M%RDZ(0)     = 1._EB/M%DZ(1)
   M%RDZ(M%KBP1)= 1._EB/M%DZ(M%KBAR)

   DO K=0,M%KBAR
      M%DZN(K)  = 0.5_EB*(M%DZ(K)+M%DZ(K+1))
      M%RDZN(K) = 1._EB/M%DZN(K)
      IF (K>0) M%ZC(K) = 0.5_EB*(M%Z(K)+M%Z(K-1))
   ENDDO
   M%ZC(0)      = M%ZS - 0.5_EB*M%DZ(0)
   M%ZC(M%KBP1) = M%ZF + 0.5_EB*M%DZ(M%KBP1)
   DZS_MAX = MAX(DZS_MAX,M%DZ(0))
   DZF_MAX = MAX(DZF_MAX,M%DZ(M%KBP1))

   ! Set default value for USE_ATMOSPHERIC_INTERPOLATION, may be overwritten by user on WIND line

   IF (STRATIFICATION .AND. ANY(M%DZ>1.999_EB)) USE_ATMOSPHERIC_INTERPOLATION=.TRUE.

   ! Set up arrays that will return coordinate positions

   NIPX   = 500*M%IBAR
   NIPY   = 500*M%JBAR
   NIPZ   = 500*M%KBAR
   NIPXS  = NINT(NIPX*M%DX(0)/(M%XF-M%XS))
   NIPXF  = NINT(NIPX*M%DX(M%IBP1)/(M%XF-M%XS))
   NIPYS  = NINT(NIPY*M%DY(0)/(M%YF-M%YS))
   NIPYF  = NINT(NIPY*M%DY(M%JBP1)/(M%YF-M%YS))
   NIPZS  = NINT(NIPZ*M%DZ(0)/(M%ZF-M%ZS))
   NIPZF  = NINT(NIPZ*M%DZ(M%KBP1)/(M%ZF-M%ZS))
   M%RDXINT = REAL(NIPX,EB)/(M%XF-M%XS)
   M%RDYINT = REAL(NIPY,EB)/(M%YF-M%YS)
   M%RDZINT = REAL(NIPZ,EB)/(M%ZF-M%ZS)

   ALLOCATE(M%CELLSI(-NIPXS:NIPX+NIPXF),STAT=IZERO)
   CALL ChkMemErr('READ','CELLSI',IZERO)
   ALLOCATE(M%CELLSJ(-NIPYS:NIPY+NIPYF),STAT=IZERO)
   CALL ChkMemErr('READ','CELLSJ',IZERO)
   ALLOCATE(M%CELLSK(-NIPZS:NIPZ+NIPZF),STAT=IZERO)
   CALL ChkMemErr('READ','CELLSK',IZERO)

   M%CELLSI_LO=-NIPXS
   M%CELLSI_HI=NIPX+NIPXF
   DO I=M%CELLSI_LO,M%CELLSI_HI
      M%CELLSI(I) = GINV(REAL(I,EB)/M%RDXINT,1,NM)*M%RDXI
      M%CELLSI(I) = MAX(M%CELLSI(I),-0.9_EB)
      M%CELLSI(I) = MIN(M%CELLSI(I),REAL(M%IBAR)+0.9_EB)
   ENDDO

   M%CELLSJ_LO=-NIPYS
   M%CELLSJ_HI=NIPY+NIPYF
   DO J=M%CELLSJ_LO,M%CELLSJ_HI
      M%CELLSJ(J) = GINV(REAL(J,EB)/M%RDYINT,2,NM)*M%RDETA
      M%CELLSJ(J) = MAX(M%CELLSJ(J),-0.9_EB)
      M%CELLSJ(J) = MIN(M%CELLSJ(J),REAL(M%JBAR)+0.9_EB)
   ENDDO

   M%CELLSK_LO=-NIPZS
   M%CELLSK_HI=NIPZ+NIPZF
   DO K=M%CELLSK_LO,M%CELLSK_HI
      M%CELLSK(K) = GINV(REAL(K,EB)/M%RDZINT,3,NM)*M%RDZETA
      M%CELLSK(K) = MAX(M%CELLSK(K),-0.9_EB)
      M%CELLSK(K) = MIN(M%CELLSK(K),REAL(M%KBAR)+0.9_EB)
   ENDDO

   ! compute maximum cell aspect ratio

   DO K=1,M%KBAR
      DO J=1,M%JBAR
         DO I=1,M%IBAR
            MAX_CELL_ASPECT_RATIO(NM) = MAX( MAX_CELL_ASPECT_RATIO(NM), MAX(M%DX(I),M%DY(J),M%DZ(K))/MIN(M%DX(I),M%DY(J),M%DZ(K)) )
         ENDDO
      ENDDO
   ENDDO

ENDDO MESH_LOOP


CONTAINS

INTEGER FUNCTION IFAC(II,NN)
INTEGER, INTENT(IN) :: II,NN
INTEGER :: III
IFAC = 1
DO III=II-NN+1,II
   IFAC = IFAC*III
ENDDO
END FUNCTION IFAC

END SUBROUTINE READ_TRAN


!> \brief Read the TIME namelist line
!> \param DT Time step (s)

SUBROUTINE READ_TIME(DT)

REAL(EB), INTENT(OUT) :: DT
NAMELIST /TIME/ DT,DT_END_FILL,DT_END_MINIMUM,DT_EXTERNAL,FYI,LIMITING_DT_RATIO,LOCK_TIME_STEP,&
                RESTRICT_TIME_STEP,T_BEGIN,T_END,TIME_SHRINK_FACTOR,WALL_INCREMENT

DT                   = -1._EB
DT_EXTERNAL          = 0._EB
TIME_SHRINK_FACTOR   = 1._EB
T_BEGIN              = 0._EB
T_END                = 1._EB

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
READ_TIME_LOOP: DO
   CALL CHECKREAD('TIME',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_TIME_LOOP
   READ(LU_INPUT,TIME,END=21,ERR=22,IOSTAT=IOS)
   22 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with TIME line') ; RETURN ; ENDIF
ENDDO READ_TIME_LOOP
21 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (T_END<=T_BEGIN) SETUP_ONLY = .TRUE.
T_END = T_BEGIN + (T_END-T_BEGIN)/TIME_SHRINK_FACTOR

! No need for CHECK_MESH_ALIGNMENT if not SETUP_ONLY

IF (T_END>TWO_EPSILON_EB) CHECK_MESH_ALIGNMENT=.FALSE. ! overwrite user-specified value

END SUBROUTINE READ_TIME


!> \brief Read the MOVE namelist line

SUBROUTINE READ_MOVE

REAL(EB) :: SCALE,SCALEX,SCALEY,SCALEZ,DX,DY,DZ,X0,Y0,Z0,AXIS(3),ROTATION_ANGLE,T34(12)
INTEGER :: N
CHARACTER(LABEL_LENGTH) :: ID
TYPE(MOVEMENT_TYPE), POINTER :: MV
NAMELIST /MOVE/ AXIS,SCALE,SCALEX,SCALEY,SCALEZ,DX,DY,DZ,FYI,ID,ROTATION_ANGLE,X0,Y0,Z0,T34

N_MOVE = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_MOVE_LOOP: DO
   CALL CHECKREAD('MOVE',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_MOVE_LOOP
   READ(LU_INPUT,NML=MOVE,END=9,ERR=10,IOSTAT=IOS)
   N_MOVE = N_MOVE + 1
   10 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with MOVE number ',N_MOVE,', line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_MOVE_LOOP
9 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ALLOCATE(MOVEMENT(N_MOVE),STAT=IZERO)
CALL ChkMemErr('READ','MOVEMENT',IZERO)

READ_MOVE_LOOP: DO N=1,N_MOVE

   ROTATION_ANGLE   = 0._EB
   AXIS    = (/0._EB,0._EB,1._EB/)
   ID      = 'null'
   SCALE   = 1._EB
   SCALEX  = 1._EB
   SCALEY  = 1._EB
   SCALEZ  = 1._EB
   DX      = 0._EB
   DY      = 0._EB
   DZ      = 0._EB
   X0      = 0._EB
   Y0      = 0._EB
   Z0      = 0._EB
   T34     = 0._EB

   CALL CHECKREAD('MOVE',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_MOVE_LOOP
   READ(LU_INPUT,MOVE)

   MV => MOVEMENT(N)
   MV%AXIS  = AXIS/SQRT(DOT_PRODUCT(AXIS,AXIS))
   MV%INDEX = N
   MV%ID = ID
   MV%X0 = X0
   MV%Y0 = Y0
   MV%Z0 = Z0
   MV%SCALE  = SCALE
   MV%SCALEX = SCALEX
   MV%SCALEY = SCALEY
   MV%SCALEZ = SCALEZ
   MV%DX = DX
   MV%DY = DY
   MV%DZ = DZ
   MV%ROTATION_ANGLE = -ROTATION_ANGLE
   MV%T34(1:3,1:4)   = RESHAPE(T34, (/3, 4/) )

ENDDO READ_MOVE_LOOP

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

END SUBROUTINE READ_MOVE


!> \brief Read the MULT namelist line(s)

SUBROUTINE READ_MULT

REAL(EB) :: DX,DY,DZ,DXB(6),DX0,DY0,DZ0
CHARACTER(LABEL_LENGTH) :: ID
INTEGER :: N,I1,I2,J1,J2,K1,K2,I_LOWER,I_UPPER,J_LOWER,J_UPPER,K_LOWER,K_UPPER,N_LOWER,N_UPPER,&
           I_LOWER_SKIP,I_UPPER_SKIP,J_LOWER_SKIP,J_UPPER_SKIP,K_LOWER_SKIP,K_UPPER_SKIP,N_LOWER_SKIP,N_UPPER_SKIP
TYPE(MULTIPLIER_TYPE), POINTER :: MR=>NULL()
NAMELIST /MULT/ DX,DXB,DX0,DY,DY0,DZ,DZ0,FYI,ID,&
                I_LOWER,I_LOWER_SKIP,I_UPPER,I_UPPER_SKIP,&
                J_LOWER,J_LOWER_SKIP,J_UPPER,J_UPPER_SKIP,&
                K_LOWER,K_LOWER_SKIP,K_UPPER,K_UPPER_SKIP,&
                N_LOWER,N_LOWER_SKIP,N_UPPER_SKIP,N_UPPER

N_MULT = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_MULT_LOOP: DO
   CALL CHECKREAD('MULT',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_MULT_LOOP
   READ(LU_INPUT,NML=MULT,END=9,ERR=10,IOSTAT=IOS)
   N_MULT = N_MULT + 1
   10 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with MULT number ',N_MULT,', line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_MULT_LOOP
9 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ALLOCATE(MULTIPLIER(0:N_MULT),STAT=IZERO)
CALL ChkMemErr('READ','MULTIPLIER',IZERO)

READ_MULT_LOOP: DO N=0,N_MULT

   ID      = 'null'
   IF (N==0) ID = 'MULT DEFAULT'
   DX      = 0._EB
   DY      = 0._EB
   DZ      = 0._EB
   DX0     = 0._EB
   DY0     = 0._EB
   DZ0     = 0._EB
   DXB     = 0._EB
   I_LOWER = 0
   I_UPPER = 0
   J_LOWER = 0
   J_UPPER = 0
   K_LOWER = 0
   K_UPPER = 0
   N_LOWER = 0
   N_UPPER = 0
   I_LOWER_SKIP = -999
   I_UPPER_SKIP = 999
   J_LOWER_SKIP = -999
   J_UPPER_SKIP = 999
   K_LOWER_SKIP = -999
   K_UPPER_SKIP = 999
   N_LOWER_SKIP = -999
   N_UPPER_SKIP = 999

   IF (N>0) THEN
      CALL CHECKREAD('MULT',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT READ_MULT_LOOP
      READ(LU_INPUT,MULT)
   ENDIF

   MR => MULTIPLIER(N)
   MR%ID      = ID
   MR%DXB     = DXB
   MR%DX0     = DX0
   MR%DY0     = DY0
   MR%DZ0     = DZ0
   IF (ABS(DX)>TWO_EPSILON_EB) MR%DXB(1:2) = DX
   IF (ABS(DY)>TWO_EPSILON_EB) MR%DXB(3:4) = DY
   IF (ABS(DZ)>TWO_EPSILON_EB) MR%DXB(5:6) = DZ

   MR%I_LOWER = I_LOWER
   MR%I_UPPER = I_UPPER
   MR%J_LOWER = J_LOWER
   MR%J_UPPER = J_UPPER
   MR%K_LOWER = K_LOWER
   MR%K_UPPER = K_UPPER
   MR%N_COPIES = (I_UPPER-I_LOWER+1)*(J_UPPER-J_LOWER+1)*(K_UPPER-K_LOWER+1)

   IF (N_LOWER/=0 .OR. N_UPPER/=0) THEN
      MR%SEQUENTIAL = .TRUE.
      MR%I_LOWER  = N_LOWER
      MR%I_UPPER  = N_UPPER
      MR%J_LOWER  = 0
      MR%J_UPPER  = 0
      MR%K_LOWER  = 0
      MR%K_UPPER  = 0
      MR%N_COPIES = (N_UPPER-N_LOWER+1)
      I_LOWER_SKIP = N_LOWER_SKIP
      I_UPPER_SKIP = N_UPPER_SKIP
   ENDIF

   ALLOCATE(MR%SKIP(MR%I_LOWER:MR%I_UPPER,MR%J_LOWER:MR%J_UPPER,MR%K_LOWER:MR%K_UPPER),STAT=IZERO)
   CALL ChkMemErr('READ_MULT','SKIP',IZERO)
   MR%SKIP = .FALSE.
   IF (I_LOWER_SKIP>=MR%I_LOWER .OR. J_LOWER_SKIP>=MR%J_LOWER .OR. K_LOWER_SKIP>=MR%K_LOWER .OR. &
       I_UPPER_SKIP<=MR%I_UPPER .OR. J_UPPER_SKIP<=MR%J_UPPER .OR. K_UPPER_SKIP<=MR%K_UPPER) THEN
      I1 = MAX(MR%I_LOWER,I_LOWER_SKIP)
      I2 = MIN(MR%I_UPPER,I_UPPER_SKIP)
      J1 = MAX(MR%J_LOWER,J_LOWER_SKIP)
      J2 = MIN(MR%J_UPPER,J_UPPER_SKIP)
      K1 = MAX(MR%K_LOWER,K_LOWER_SKIP)
      K2 = MIN(MR%K_UPPER,K_UPPER_SKIP)
      MR%SKIP(I1:I2,J1:J2,K1:K2) = .TRUE.
      MR%N_COPIES = MR%N_COPIES - (I2-I1+1)*(J2-J1+1)*(K2-K1+1)
   ENDIF

ENDDO READ_MULT_LOOP
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

END SUBROUTINE READ_MULT


!> \brief Read the MISC namelist line

SUBROUTINE READ_MISC

USE MATH_FUNCTIONS, ONLY: GET_RAMP_INDEX
REAL(EB) :: MAXIMUM_VISIBILITY
CHARACTER(LABEL_LENGTH) :: RAMP_GX,RAMP_GY,RAMP_GZ,TURBULENCE_MODEL,&
                           SIMULATION_MODE,FLUX_LIMITER,LES_FILTER_TYPE

NAMELIST /MISC/ AEROSOL_AL2O3,AEROSOL_SCRUBBING,AGGLOMERATION,ALIGNMENT_TOLERANCE,ALLOW_SURFACE_PARTICLES, &
                ALLOW_UNDERSIDE_PARTICLES,BNDF_DEFAULT,CC_IBM,CCVOL_LINK,C_DEARDORFF,&
                CFL_MAX,CFL_MIN,CFL_VELOCITY_NORM,CHECK_HT,CHECK_VN, &
                CNF_CUTOFF,CONSTANT_SPECIFIC_HEAT_RATIO,&
                C_SMAGORINSKY,C_VREMAN,C_WALE,DEPOSITION,EXTERNAL_FILENAME,&
                FIXED_LES_FILTER_WIDTH,FLUX_LIMITER,FREEZE_VELOCITY,FYI,GAMMA,GRAVITATIONAL_DEPOSITION,&
                GRAVITATIONAL_SETTLING,GVEC,H_F_REFERENCE_TEMPERATURE,&
                HUMIDITY,HVAC_LOCAL_PRESSURE,HVAC_MASS_TRANSPORT_CELL_L,HVAC_PRES_RELAX,HVAC_QFAN,IBLANK_SMV,I_MAX_TEMP,&
                LES_FILTER_TYPE,LEVEL_SET_ELLIPSE,LEVEL_SET_MODE,&
                MAXIMUM_VISIBILITY,MAX_LEAK_PATHS,MAX_RAMPS,&
                MINIMUM_ZONE_VOLUME,MPI_TIMEOUT,NEIGHBOR_SEPARATION_DISTANCE,NORTH_BEARING,&
                NOISE,NOISE_VELOCITY,NO_PRESSURE_ZONES,NUCLEATION_SITES,ORIGIN_LAT,ORIGIN_LON,&
                OVERWRITE,PARTICLE_CFL,PARTICLE_CFL_MAX,PARTICLE_CFL_MIN,PERIODIC_TEST,POSITIVE_ERROR_TEST,&
                POROUS_FLOOR,PR,PROFILING,&
                P_INF,RAMP_GX,RAMP_GY,RAMP_GZ,RESTART,RESTART_CHID,SC,&
                RND_SEED,SHARED_FILE_SYSTEM,SIMULATION_MODE,SMOKE3D_16,SMOKE_ALBEDO,SOLID_PHASE_ONLY,SOOT_DENSITY,SOOT_OXIDATION,&
                TAU_DEFAULT,TENSOR_DIFFUSIVITY,TERRAIN_IMAGE,TEXTURE_ORIGIN,&
                THERMOPHORETIC_DEPOSITION,THERMOPHORETIC_SETTLING,THICKEN_OBSTRUCTIONS,&
                TMPA,TURBULENCE_MODEL,TURBULENT_DEPOSITION,UVW_FILE,&
                VERBOSE,VISIBILITY_FACTOR,VN_MAX,VN_MIN,Y_CO2_INFTY,Y_O2_INFTY,&
                RADIATION,STRATIFICATION,SUPPRESSION

! Physical constants

TMPA         = 20._EB                                              ! Ambient temperature (C)

! Empirical heat transfer constants

PR_ONTH      = PR_AIR**ONTH

! Miscellaneous constants

RESTART_CHID   = CHID
IBLANK_SMV     = .TRUE.
SIMULATION_MODE = 'VLES'

TEXTURE_ORIGIN(1) = 0._EB
TEXTURE_ORIGIN(2) = 0._EB
TEXTURE_ORIGIN(3) = 0._EB

! LES parameters

PR                   = -1.0_EB  ! Turbulent Prandtl number
SC                   = -1.0_EB  ! Turbulent Schmidt number

! Misc

RAMP_GX              = 'null'
RAMP_GY              = 'null'
RAMP_GZ              = 'null'
GVEC(1)              = 0._EB        ! x-component of gravity
GVEC(2)              = 0._EB        ! y-component of gravity
GVEC(3)              = -GRAV        ! z-component of gravity
THICKEN_OBSTRUCTIONS = .FALSE.
N_TERRAIN_IMAGES     = 0
DO I = 1, MAX_TERRAIN_IMAGES
   TERRAIN_IMAGE(I) = 'null'
END DO
MAXIMUM_VISIBILITY   = 30._EB ! m
TURBULENCE_MODEL     = 'null'
MAX_LEAK_PATHS       = 200
FLUX_LIMITER         = 'null'
LES_FILTER_TYPE      = 'null'
SMOKE3D_16           = .FALSE.

! Initial read of the MISC line

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
MISC_LOOP: DO
   CALL CHECKREAD('MISC',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT MISC_LOOP
   READ(LU_INPUT,MISC,END=23,ERR=24,IOSTAT=IOS)
   24 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with MISC line.') ; RETURN ; ENDIF
   N_TERRAIN_IMAGES = 0
   DO I = 1, MAX_TERRAIN_IMAGES
      IF( TERRAIN_IMAGE(I) /= 'null' ) N_TERRAIN_IMAGES = N_TERRAIN_IMAGES + 1
   END DO
ENDDO MISC_LOOP
23 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (RETURN_BEFORE_SIM_MODE) RETURN

! Allocate HV_H2O and set humidity data

ALLOCATE(H_V_H2O(0:I_MAX_TEMP))
CALL CALC_H2O_HV

! Check whether user has specified CFL_VELOCITY_NORM

IF (CFL_VELOCITY_NORM > -1) CFL_VELOCITY_NORM_USER_SPECIFIED=.TRUE.

! Choose simulation mode

IF (SIMULATION_MODE=='DNS') THEN
   SIM_MODE = DNS_MODE
   CFL_VELOCITY_NORM = 1
   CFL_MAX = 0.5
   CFL_MIN = 0.4
   VN_MAX = 0.5
   VN_MIN = 0.4
   I_FLUX_LIMITER = CHARM_LIMITER
   IF (TURBULENCE_MODEL/='null') THEN
      WRITE(MESSAGE,'(A,A,A)')  'ERROR(127): TURBULENCE_MODEL,',TRIM(TURBULENCE_MODEL),', is not appropriate for DNS.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ELSEIF (SIMULATION_MODE=='LES') THEN
   SIM_MODE = LES_MODE
   CFL_VELOCITY_NORM = 1
   VN_MAX = 0.8
   VN_MIN = 0.6
   I_FLUX_LIMITER = CHARM_LIMITER
   TURBULENCE_MODEL = 'DEARDORFF'
ELSEIF (SIMULATION_MODE=='VLES') THEN
   SIM_MODE = VLES_MODE
   CFL_VELOCITY_NORM = 2 ! global default
   I_FLUX_LIMITER = SUPERBEE_LIMITER
   TURBULENCE_MODEL = 'DEARDORFF'
ELSEIF (SIMULATION_MODE=='SVLES') THEN
   SIM_MODE = SVLES_MODE
   I_FLUX_LIMITER = SUPERBEE_LIMITER
   TURBULENCE_MODEL = 'DEARDORFF'
   CFL_VELOCITY_NORM = 3
   CONSTANT_SPECIFIC_HEAT_RATIO = .TRUE.
ELSE
   WRITE(MESSAGE,'(A,A,A)')  'ERROR(128): SIMULATION_MODE, ',TRIM(SIMULATION_MODE),', is not an option.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

! Tensor diffusivity requires LES mode

IF (TENSOR_DIFFUSIVITY .AND. SIM_MODE/=LES_MODE) THEN
   WRITE(MESSAGE,'(A)')  "ERROR(123): TENSOR_DIFFUSIVITY requires SIMULATION_MODE='LES'."
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

! Re-read the line to pick up any user-specified options

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
CALL CHECKREAD('MISC',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
IF (IOS==0) READ(LU_INPUT,MISC)
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Temperature conversions

TMPA  = TMPA + TMPM
TMPA4 = TMPA**4

! Establish starting values for min/max temperature and density

TMPMIN = MAX(1._EB , MIN(TMPA,TMPM)-10._EB)
TMPMAX = 3000._EB
RHOMAX = 0._EB

! Miscellaneous

TEX_ORI = TEXTURE_ORIGIN
GRAV = SQRT(DOT_PRODUCT(GVEC,GVEC))

! Velocity, force, and gravity ramps

I_RAMP_GX   = 0
I_RAMP_GY   = 0
I_RAMP_GZ   = 0
N_RAMP      = 0
ALLOCATE(RAMP_ID(MAX_RAMPS))
RAMP_ID='null'
ALLOCATE(RAMP_TYPE(MAX_RAMPS))
IF (RAMP_GX/='null') CALL GET_RAMP_INDEX(RAMP_GX,'TIME',I_RAMP_GX)
IF (RAMP_GY/='null') CALL GET_RAMP_INDEX(RAMP_GY,'TIME',I_RAMP_GY)
IF (RAMP_GZ/='null') CALL GET_RAMP_INDEX(RAMP_GZ,'TIME',I_RAMP_GZ)

! Prandtl and Schmidt numbers

IF (SIM_MODE==DNS_MODE) THEN
   IF (PR<0._EB) PR = 0.7_EB
   IF (SC<0._EB) SC = 1.0_EB
ELSE
   IF (PR<0._EB) PR = 0.5_EB
   IF (SC<0._EB) SC = 0.5_EB
ENDIF

RSC = 1._EB/SC
RPR = 1._EB/PR

! Check for a restart file

IF (RESTART .AND. RESTART_CHID == CHID) APPEND = .TRUE.
IF (RESTART) NOISE  = .FALSE.

! Min and Max values of flux limiter

IF (I_FLUX_LIMITER<0 .OR. I_FLUX_LIMITER>5) THEN
   WRITE(MESSAGE,'(A)')  'ERROR on MISC: Permissible values for I_FLUX_LIMITER=0:5'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

! Level Set parameters

IF (LEVEL_SET_MODE>0) THEN
   TERRAIN_CASE = .TRUE.
   NO_PRESSURE_ZONES = .TRUE.
ENDIF

SELECT CASE(LEVEL_SET_MODE)
   CASE(1)
      LEVEL_SET_COUPLED_WIND = .FALSE.
      LEVEL_SET_COUPLED_FIRE = .FALSE.
      RADIATION              = .FALSE.
      FREEZE_VELOCITY        = .TRUE.
      SOLID_PHASE_ONLY       = .TRUE.
   CASE(2)
      LEVEL_SET_COUPLED_WIND = .TRUE.
      LEVEL_SET_COUPLED_FIRE = .FALSE.
      RADIATION              = .FALSE.
      FREEZE_VELOCITY        = .FALSE.
      SOLID_PHASE_ONLY       = .FALSE.
   CASE(3)
      LEVEL_SET_COUPLED_WIND = .TRUE.
      LEVEL_SET_COUPLED_FIRE = .FALSE.
      RADIATION              = .FALSE.
      FREEZE_VELOCITY        = .FALSE.
      SOLID_PHASE_ONLY       = .FALSE.
   CASE(4)
      LEVEL_SET_COUPLED_WIND = .TRUE.
      LEVEL_SET_COUPLED_FIRE = .TRUE.
      RADIATION              = .TRUE.
      FREEZE_VELOCITY        = .FALSE.
      SOLID_PHASE_ONLY       = .FALSE.
   CASE(5)
      LEVEL_SET_COUPLED_WIND = .TRUE.
      LEVEL_SET_COUPLED_FIRE = .TRUE.
      RADIATION              = .TRUE.
      FREEZE_VELOCITY        = .FALSE.
      SOLID_PHASE_ONLY       = .FALSE.
END SELECT

! Turbulence model

SELECT CASE (TRIM(TURBULENCE_MODEL))
   CASE ('CONSTANT SMAGORINSKY')
      TURB_MODEL=CONSMAG
   CASE ('DYNAMIC SMAGORINSKY')
      TURB_MODEL=DYNSMAG
   CASE ('DEARDORFF')
      TURB_MODEL=DEARDORFF
   CASE ('VREMAN')
      TURB_MODEL=VREMAN
   CASE ('WALE')
      TURB_MODEL=WALE
   CASE ('null')
      TURB_MODEL=NO_TURB_MODEL
   CASE DEFAULT
      WRITE(MESSAGE,'(A,A,A)')  'ERROR(129): TURBULENCE_MODEL, ',TRIM(TURBULENCE_MODEL),', is not recognized.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
END SELECT

! LES filter type

IF (FIXED_LES_FILTER_WIDTH>0._EB) LES_FILTER_TYPE = 'FIXED'

SELECT CASE (TRIM(LES_FILTER_TYPE))
   CASE DEFAULT
      LES_FILTER_WIDTH_TYPE = MEAN_LES_FILTER
   CASE ('MAX')
      LES_FILTER_WIDTH_TYPE = MAX_LES_FILTER
   CASE ('FIXED')
      LES_FILTER_WIDTH_TYPE = FIXED_LES_FILTER
END SELECT

! Flux limiter

SELECT CASE (TRIM(FLUX_LIMITER))
   CASE ('null')
      ! set above, see SIMULATION_MODE
   CASE ('CENTRAL')
      I_FLUX_LIMITER=CENTRAL_LIMITER
   CASE ('GODUNOV')
      I_FLUX_LIMITER=GODUNOV_LIMITER
   CASE ('SUPERBEE')
      I_FLUX_LIMITER=SUPERBEE_LIMITER
   CASE ('MINMOD')
      I_FLUX_LIMITER=MINMOD_LIMITER
   CASE ('CHARM')
      I_FLUX_LIMITER=CHARM_LIMITER
   CASE ('MP5')
      I_FLUX_LIMITER=MP5_LIMITER
   CASE DEFAULT
      WRITE(MESSAGE,'(A,A,A)')  'ERROR(130): FLUX_LIMITER, ',TRIM(FLUX_LIMITER),', is not recognized.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
END SELECT

! Set the lower limit of the extinction coefficient

EC_LL = VISIBILITY_FACTOR/MAXIMUM_VISIBILITY

IF (HUMIDITY<0._EB) HUMIDITY=40._EB

FUEL_SMIX_INDEX=2

H_F_REFERENCE_TEMPERATURE = H_F_REFERENCE_TEMPERATURE + TMPM

IF (HVAC_MASS_TRANSPORT_CELL_L > 0._EB) HVAC_MASS_TRANSPORT = .TRUE.

END SUBROUTINE READ_MISC


!> \brief Read the WIND namelist line

SUBROUTINE READ_WIND
use netcdf
USE MATH_FUNCTIONS, ONLY: GET_RAMP_INDEX,NORMAL,RANDOM_WIND_FLUCTUATIONS
USE PHYSICAL_FUNCTIONS, ONLY: MONIN_OBUKHOV_SIMILARITY,MONIN_OBUKHOV_STABILITY_CORRECTIONS
REAL(EB) :: CORIOLIS_VECTOR(3)=0._EB,FORCE_VECTOR(3)=0._EB,L,ZZZ,ZETA,Z_0,SPEED,DIRECTION,&
            Z_REF,U_STAR,THETA_0,THETA_STAR,TMP,U,THETA_REF,TMP_REF,P_REF,RHO_REF,ZSW,ZFW,&
            TAU_THETA,SIGMA_THETA,PRESSURE_GRADIENT_FORCE,PSI_M,PSI_H
CHARACTER(LABEL_LENGTH) :: RAMP_PGF_T,RAMP_FVX_T,RAMP_FVY_T,RAMP_FVZ_T,RAMP_TMP0_Z,&
                           RAMP_DIRECTION_T,RAMP_DIRECTION_Z,RAMP_SPEED_T,RAMP_SPEED_Z
TYPE(RESERVED_RAMPS_TYPE), POINTER :: RRP,RRPX
INTEGER, PARAMETER :: N_MO_PTS=51 ! number of Monin-Obukhov ramp points
integer:: ncid, varid1,varid2,status
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
NAMELIST /WIND/ CORIOLIS_VECTOR,DIRECTION,FORCE_VECTOR,FYI,GEOSTROPHIC_WIND,GROUND_LEVEL,INITIAL_SPEED,L,LAPSE_RATE,LATITUDE,&
                PRESSURE_GRADIENT_FORCE,RAMP_DIRECTION_T,RAMP_DIRECTION_Z,&
                RAMP_PGF_T,RAMP_FVX_T,RAMP_FVY_T,RAMP_FVZ_T,RAMP_SPEED_T,RAMP_SPEED_Z,RAMP_TMP0_Z,&
                SIGMA_THETA,SPEED,STRATIFICATION,TAU_THETA,THETA_STAR,TMP_REF,U_STAR,U0,&
                USE_ATMOSPHERIC_INTERPOLATION,V0,W0,Z_0,Z_REF

! Default values

DIRECTION               = 270._EB   ! westerly wind
LAPSE_RATE              = 0._EB     ! K/m
L                       = 0._EB     ! m
PRESSURE_GRADIENT_FORCE = -1._EB    ! Pa/m
RAMP_DIRECTION_T        = 'null'
RAMP_DIRECTION_Z        = 'null'
RAMP_SPEED_T            = 'null'
RAMP_SPEED_Z            = 'null'
RAMP_TMP0_Z             = 'null'
RAMP_PGF_T              = 'null'
RAMP_FVX_T              = 'null'
RAMP_FVY_T              = 'null'
RAMP_FVZ_T              = 'null'
SIGMA_THETA             = -1._EB
SPEED                   = -1._EB    ! m/s
TAU_THETA               = 300._EB
THETA_STAR              = 0._EB     ! K
TMP_REF                 = -1._EB    ! C
U_STAR                  = -1._EB    ! m/s
U0                      = 0._EB     ! m/s
V0                      = 0._EB     ! m/s
W0                      = 0._EB     ! m/s
Z_0                     = -1._EB    ! m
Z_REF                   = 2._EB     ! m

! Read the WIND line

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
WIND_LOOP: DO
   CALL CHECKREAD('WIND',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT WIND_LOOP
   READ(LU_INPUT,WIND,END=23,ERR=24,IOSTAT=IOS)
   24 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with WIND line') ; RETURN ; ENDIF
ENDDO WIND_LOOP
23 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! If nothing specified on SURF, then use 0.03 m as default

IF (Z_0<-TWO_EPSILON_EB) Z_0 = 0.03_EB

! Optional fluctuation of the prevailing wind DIRECTION, Theta'(t+dt) = R^2*Theta'(t) + Normal(0,sqrt(1-R^2)*SIGMA_THETA)

IF (SIGMA_THETA>0._EB) THEN
   CALL RANDOM_WIND_FLUCTUATIONS(SIGMA_THETA,TAU_THETA)
   RRP => RESERVED_RAMPS(N_RESERVED_RAMPS)
   RRP%DEPENDENT_DATA = DIRECTION + RRP%DEPENDENT_DATA
   RAMP_DIRECTION_T = 'RSRVD FLUCTUATING WIND DIRECTION'
   RRP%ID = RAMP_DIRECTION_T
ENDIF

! Determine the appropriate wind speed if the user specifies SPEED or U_STAR.

IF (U_STAR>0._EB .AND. ABS(L)>TWO_EPSILON_EB) THEN
   CALL MONIN_OBUKHOV_STABILITY_CORRECTIONS(PSI_M,PSI_H,Z_REF,L)
   SPEED = (U_STAR/VON_KARMAN_CONSTANT)*(LOG((Z_REF-GROUND_LEVEL)/Z_0)-PSI_M)
ENDIF

IF (SPEED>0._EB .OR. INITIAL_SPEED>0._EB) THEN
   IF (SPEED>0._EB) OPEN_WIND_BOUNDARY = .TRUE.
   IF (RAMP_DIRECTION_T/='null' .OR. RAMP_DIRECTION_Z/='null') THEN
      U0 = MAX(SPEED,INITIAL_SPEED)
      V0 = MAX(SPEED,INITIAL_SPEED)
   ELSE
      U0 = -MAX(SPEED,INITIAL_SPEED)*SIN(DIRECTION*DEG2RAD)
      V0 = -MAX(SPEED,INITIAL_SPEED)*COS(DIRECTION*DEG2RAD)
   ENDIF
ENDIF

! Pressure gradient force

IF (PRESSURE_GRADIENT_FORCE>0._EB) THEN
   IF (RAMP_DIRECTION_T/='null') THEN
      FVEC(1) = PRESSURE_GRADIENT_FORCE
      FVEC(2) = PRESSURE_GRADIENT_FORCE
   ELSE
      FVEC(1) = -PRESSURE_GRADIENT_FORCE*SIN(DIRECTION*DEG2RAD)
      FVEC(2) = -PRESSURE_GRADIENT_FORCE*COS(DIRECTION*DEG2RAD)
   ENDIF
ELSE
   FVEC = FORCE_VECTOR
ENDIF

! Coriolis force

OVEC = CORIOLIS_VECTOR
IF (LATITUDE>-90.1_EB .AND. LATITUDE<90.1_EB) THEN
   OVEC(1) = 0._EB
   OVEC(2) = EARTH_OMEGA*COS(LATITUDE*DEG2RAD)
   OVEC(3) = EARTH_OMEGA*SIN(LATITUDE*DEG2RAD)
ENDIF

! Velocity, force, and gravity ramps

I_RAMP_DIRECTION_T = 0
I_RAMP_DIRECTION_Z = 0
I_RAMP_SPEED_T     = 0
I_RAMP_SPEED_Z     = 0
I_RAMP_TMP0_Z = 0
I_RAMP_PGF_T= 0
I_RAMP_FVX_T= 0
I_RAMP_FVY_T= 0
I_RAMP_FVZ_T= 0

IF (RAMP_SPEED_T/='null') CALL GET_RAMP_INDEX(RAMP_SPEED_T,'TIME',I_RAMP_SPEED_T)
IF (RAMP_SPEED_Z/='null') CALL GET_RAMP_INDEX(RAMP_SPEED_Z,'PROFILE',I_RAMP_SPEED_Z)
IF (RAMP_DIRECTION_T/='null') CALL GET_RAMP_INDEX(RAMP_DIRECTION_T,'TIME',I_RAMP_DIRECTION_T)
IF (RAMP_DIRECTION_Z/='null') CALL GET_RAMP_INDEX(RAMP_DIRECTION_Z,'PROFILE',I_RAMP_DIRECTION_Z)
IF (RAMP_TMP0_Z/='null') CALL GET_RAMP_INDEX(RAMP_TMP0_Z,'PROFILE',I_RAMP_TMP0_Z)
IF (RAMP_PGF_T/='null') CALL GET_RAMP_INDEX(RAMP_PGF_T,'TIME',I_RAMP_PGF_T)
IF (RAMP_FVX_T/='null') CALL GET_RAMP_INDEX(RAMP_FVX_T,'TIME',I_RAMP_FVX_T)
IF (RAMP_FVY_T/='null') CALL GET_RAMP_INDEX(RAMP_FVY_T,'TIME',I_RAMP_FVY_T)
IF (RAMP_FVZ_T/='null') CALL GET_RAMP_INDEX(RAMP_FVZ_T,'TIME',I_RAMP_FVZ_T)

IF (STRATIFICATION) THEN

   IF (HVAC_SOLVE) THEN
      ZSW = MIN(ZS_MIN-DZS_MAX,NODE_Z_MIN)
      ZFW = MAX(ZF_MAX+DZF_MAX,NODE_Z_MAX)
   ELSE
      ZSW = ZS_MIN
      ZFW = ZF_MAX
   ENDIF

   IF (RAMP_TMP0_Z=='null' .AND. ABS(L)<1.E-10_EB) THEN
      N_RESERVED_RAMPS = N_RESERVED_RAMPS + 1
      RRP => RESERVED_RAMPS(N_RESERVED_RAMPS)
      ALLOCATE(RRP%INDEPENDENT_DATA(2))
      ALLOCATE(RRP%DEPENDENT_DATA(2))
      RRP%INDEPENDENT_DATA(1) = ZSW
      RRP%INDEPENDENT_DATA(2) = ZFW
      RRP%DEPENDENT_DATA(1)   = (TMPA+LAPSE_RATE*(ZSW-GROUND_LEVEL))/TMPA
      RRP%DEPENDENT_DATA(2)   = (TMPA+LAPSE_RATE*(ZFW-GROUND_LEVEL))/TMPA
      RRP%NUMBER_DATA_POINTS = 2
      RAMP_TMP0_Z = 'RSRVD TEMPERATURE PROFILE'
      RRP%ID = RAMP_TMP0_Z
      CALL GET_RAMP_INDEX(RAMP_TMP0_Z,'PROFILE',I_RAMP_TMP0_Z)
   ENDIF

   IF (ABS(L)>1.E-10_EB) THEN
      N_RESERVED_RAMPS = N_RESERVED_RAMPS + 1
      RRP => RESERVED_RAMPS(N_RESERVED_RAMPS)
      RRP%NUMBER_DATA_POINTS = N_MO_PTS
      N_RESERVED_RAMPS = N_RESERVED_RAMPS + 1
      RRPX => RESERVED_RAMPS(N_RESERVED_RAMPS)
      RRPX%NUMBER_DATA_POINTS = N_MO_PTS
      ALLOCATE(RRP%INDEPENDENT_DATA(N_MO_PTS))
      ALLOCATE(RRP%DEPENDENT_DATA(N_MO_PTS))
      ALLOCATE(RRPX%INDEPENDENT_DATA(N_MO_PTS))
      ALLOCATE(RRPX%DEPENDENT_DATA(N_MO_PTS))
      CALL MONIN_OBUKHOV_STABILITY_CORRECTIONS(PSI_M,PSI_H,Z_REF,L)
      IF (U_STAR<0._EB) THEN
         U_STAR = VON_KARMAN_CONSTANT*SPEED/(LOG((Z_REF-GROUND_LEVEL)/Z_0)-PSI_M)
      ENDIF
      IF (TMP_REF<0._EB) THEN
         TMP_REF = TMPA
      ELSE
         TMP_REF = TMP_REF + TMPM  ! C to K
      ENDIF
      RHO_REF = 1.2_EB
      P_REF = P_INF - RHO_REF*GRAV*(Z_REF-GROUND_LEVEL)
      THETA_REF = TMP_REF*(P_INF/P_REF)**0.286_EB
      IF (ABS(THETA_STAR)<1.E-10_EB) THEN
         THETA_0 = THETA_REF/(1._EB+U_STAR**2*(LOG((Z_REF-GROUND_LEVEL)/Z_0)-PSI_H)/(GRAV*VON_KARMAN_CONSTANT**2*L))
         THETA_STAR = U_STAR**2*THETA_0/(GRAV*VON_KARMAN_CONSTANT*L)
      ELSE
         THETA_0 = THETA_REF - THETA_STAR*(LOG((Z_REF-GROUND_LEVEL)/Z_0)-PSI_H)/VON_KARMAN_CONSTANT
      ENDIF
      TMPA = THETA_0  ! Make the ground temperature the new ambient temperature
      DO I=1,N_MO_PTS
         ZETA = ZSW + (I-1)*(ZFW-ZSW)/(REAL(N_MO_PTS,EB)-1._EB)
         ZZZ  = Z_0*EXP(LOG((ZFW-GROUND_LEVEL)/Z_0)*(ZETA-ZSW)/(ZFW-ZSW))
         CALL MONIN_OBUKHOV_SIMILARITY(ZZZ,Z_0,L,U_STAR,THETA_STAR,THETA_0,U,TMP)
         RRP%INDEPENDENT_DATA(I) = GROUND_LEVEL + ZZZ
         RRP%DEPENDENT_DATA(I)   = TMP/TMPA
         RRPX%INDEPENDENT_DATA(I) = GROUND_LEVEL + ZZZ
         IF (SPEED>TWO_EPSILON_EB) THEN
            RRPX%DEPENDENT_DATA(I) = MAX(0._EB,U/SPEED)
         ELSE
            RRPX%DEPENDENT_DATA(I) = 0._EB
         ENDIF
      ENDDO
      RAMP_TMP0_Z = 'RSRVD TEMPERATURE PROFILE'
      CALL GET_RAMP_INDEX(RAMP_TMP0_Z,'PROFILE',I_RAMP_TMP0_Z)
      RRP%ID = RAMP_TMP0_Z
      RAMP_SPEED_Z = 'RSRVD VELOCITY PROFILE'
      CALL GET_RAMP_INDEX(RAMP_SPEED_Z,'PROFILE',I_RAMP_SPEED_Z)
      RRPX%ID = RAMP_SPEED_Z
   ENDIF

   ! Add a RAMP for the vertical profile of pressure (the values are computed in INIT)

   N_RESERVED_RAMPS = N_RESERVED_RAMPS + 1
   CALL GET_RAMP_INDEX('RSRVD PRESSURE PROFILE','PROFILE',I_RAMP_P0_Z)
   RRP => RESERVED_RAMPS(N_RESERVED_RAMPS)
   ALLOCATE(RRP%INDEPENDENT_DATA(2))
   ALLOCATE(RRP%DEPENDENT_DATA(2))
   RRP%INDEPENDENT_DATA(1) = ZSW
   RRP%INDEPENDENT_DATA(2) = ZFW
   RRP%DEPENDENT_DATA(1) = 0._EB     ! Dummy values to be filled in later
   RRP%DEPENDENT_DATA(2) = 1._EB     ! Dummy values to be filled in later
   RRP%NUMBER_DATA_POINTS = 2
   RRP%ID = 'RSRVD PRESSURE PROFILE'

ENDIF

! Min value of temperature

IF (LAPSE_RATE < 0._EB) TMPMIN = MIN(TMPMIN,TMPA+LAPSE_RATE*(ZFW-GROUND_LEVEL))

! Set up pressure gradient force (FVEC) based on specified geostrophic wind components

IF (ANY(ABS(GEOSTROPHIC_WIND)>TWO_EPSILON_EB)) THEN
   IF (ALL(ABS(OVEC)<TWO_EPSILON_EB)) THEN
      WRITE(MESSAGE,'(A)') 'ERROR(131): GEOSTROPHIC_WIND requires Coriolis force, set LATITUDE on WIND line.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   FVEC(1) = - GEOSTROPHIC_WIND(2)*RHOA*2._EB*EARTH_OMEGA*SIN(LATITUDE*DEG2RAD)
   FVEC(2) =   GEOSTROPHIC_WIND(1)*RHOA*2._EB*EARTH_OMEGA*SIN(LATITUDE*DEG2RAD)
ENDIF

END SUBROUTINE READ_WIND


!> \brief Read the DUMP namelist line, parameters associated with output files

SUBROUTINE READ_DUMP

USE MATH_FUNCTIONS, ONLY: GET_RAMP_INDEX
USE OUTPUT_CLOCKS
INTEGER :: N,SIG_FIGS,SIG_FIGS_EXP
CHARACTER(LABEL_LENGTH) :: RAMP_BNDF,RAMP_CPU,RAMP_CTRL,RAMP_DEVC,RAMP_FLUSH,RAMP_GEOM,RAMP_HRR,RAMP_HVAC,RAMP_ISOF,RAMP_MASS,&
                           RAMP_PART,RAMP_PL3D,RAMP_PROF,RAMP_RADF,RAMP_RESTART,RAMP_SLCF,RAMP_SL3D,RAMP_SMOKE3D,RAMP_UVW,&
                           RAMP_TMP,RAMP_SPEC
NAMELIST /DUMP/ CFL_FILE,CLIP_RESTART_FILES,COLUMN_DUMP_LIMIT,CTRL_COLUMN_LIMIT,DEVC_COLUMN_LIMIT,&
                DIAGNOSTICS_INTERVAL,&
                DT_BNDF,DT_CPU,DT_CTRL,DT_DEVC,DT_FLUSH,DT_HRR,DT_HVAC,DT_ISOF,DT_MASS,DT_PART,DT_PL3D,DT_PROF,&
                DT_RADF,DT_RESTART,DT_SL3D,DT_SLCF,DT_SMOKE3D,DT_UVW,DT_TMP,DT_SPEC,&
                FLUSH_FILE_BUFFERS,GET_CUTCELLS_VERBOSE,HRR_GAS_ONLY,MASS_FILE,MAXIMUM_PARTICLES,MMS_TIMER,&
                NFRAMES,PLOT3D_PART_ID,PLOT3D_QUANTITY,PLOT3D_SPEC_ID,PLOT3D_VELO_INDEX,&
                RAMP_BNDF,RAMP_CPU,RAMP_CTRL,RAMP_DEVC,RAMP_FLUSH,RAMP_HRR,RAMP_HVAC,RAMP_ISOF,RAMP_MASS,&
                RAMP_PART,RAMP_PL3D,RAMP_PROF,RAMP_RADF,RAMP_RESTART,RAMP_SLCF,RAMP_SL3D,RAMP_SMOKE3D,&
                RAMP_SPEC,RAMP_TMP,RAMP_UVW,RENDER_FILE,RESULTS_DIR,SIG_FIGS,SIG_FIGS_EXP,SMOKE3D,STATUS_FILES,&
                SUPPRESS_DIAGNOSTICS,TURB_INIT_CLOCK,VELOCITY_ERROR_FILE,WRITE_XYZ

! Set defaults

MAXIMUM_PARTICLES  = 1000000
MMS_TIMER          = 1.E10_EB
NFRAMES            = 1000
PLOT3D_QUANTITY(1) = 'TEMPERATURE'
PLOT3D_QUANTITY(2) = 'U-VELOCITY'
PLOT3D_QUANTITY(3) = 'V-VELOCITY'
PLOT3D_QUANTITY(4) = 'W-VELOCITY'
PLOT3D_QUANTITY(5) = 'HRRPUV'
PLOT3D_PART_ID     = 'null'
PLOT3D_SPEC_ID     = 'null'
PLOT3D_VELO_INDEX  = 0
RAMP_SLCF          = 'null'
RENDER_FILE        = 'null'
RESULTS_DIR        = ''
SIG_FIGS           = 8
SIG_FIGS_EXP       = 3
IF (NMESHES>32) THEN
   SUPPRESS_DIAGNOSTICS = .TRUE.
ELSE
   SUPPRESS_DIAGNOSTICS = .FALSE.
ENDIF

DT_BNDF      = -1._EB                  ; RAMP_BNDF    = 'null' ; DT_BNDF_SPECIFIED    = DT_BNDF
DT_CPU       =  HUGE(EB)               ; RAMP_CPU     = 'null' ; DT_CPU_SPECIFIED     = DT_CPU
DT_CTRL      = -1._EB                  ; RAMP_CTRL    = 'null' ; DT_CTRL_SPECIFIED    = DT_CTRL
DT_DEVC      = -1._EB                  ; RAMP_DEVC    = 'null' ; DT_DEVC_SPECIFIED    = DT_DEVC
DT_FLUSH     = -1._EB                  ; RAMP_FLUSH   = 'null' ; DT_FLUSH_SPECIFIED   = DT_FLUSH
DT_GEOM      =  HUGE(EB)               ; RAMP_GEOM    = 'null' ; DT_GEOM_SPECIFIED    = DT_GEOM
DT_HRR       = -1._EB                  ; RAMP_HRR     = 'null' ; DT_HRR_SPECIFIED     = DT_HRR
DT_HVAC      = -1._EB                  ; RAMP_HVAC    = 'null' ; DT_HVAC_SPECIFIED    = DT_HVAC
DT_ISOF      = -1._EB                  ; RAMP_ISOF    = 'null' ; DT_ISOF_SPECIFIED    = DT_ISOF
DT_MASS      = -1._EB                  ; RAMP_MASS    = 'null' ; DT_MASS_SPECIFIED    = DT_MASS
DT_PART      = -1._EB                  ; RAMP_PART    = 'null' ; DT_PART_SPECIFIED    = DT_PART
DT_PL3D      =  HUGE(EB)               ; RAMP_PL3D    = 'null' ; DT_PL3D_SPECIFIED    = DT_PL3D
DT_PROF      = -1._EB                  ; RAMP_PROF    = 'null' ; DT_PROF_SPECIFIED    = DT_PROF
DT_RADF      =  HUGE(EB)               ; RAMP_RADF    = 'null' ; DT_RADF_SPECIFIED    = DT_RADF
DT_RESTART   =  HUGE(EB)               ; RAMP_RESTART = 'null' ; DT_RESTART_SPECIFIED = DT_RESTART
DT_SLCF      = -1._EB                  ; RAMP_SLCF    = 'null' ; DT_SLCF_SPECIFIED    = DT_SLCF
DT_SL3D      =  (T_END-T_BEGIN)/5._EB  ; RAMP_SL3D    = 'null' ; DT_SL3D_SPECIFIED    = DT_SL3D
DT_SMOKE3D   = -1._EB                  ; RAMP_SMOKE3D = 'null' ; DT_SMOKE3D_SPECIFIED = DT_SMOKE3D
DT_UVW       =  HUGE(EB)               ; RAMP_UVW     = 'null' ; DT_UVW_SPECIFIED     = DT_UVW
DT_TMP       =  HUGE(EB)               ; RAMP_TMP     = 'null' ; DT_TMP_SPECIFIED     = DT_TMP
DT_SPEC      =  HUGE(EB)               ; RAMP_SPEC    = 'null' ; DT_SPEC_SPECIFIED    = DT_SPEC
DIAGNOSTICS_INTERVAL = 100

! Read the DUMP line

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
DUMP_LOOP: DO
   CALL CHECKREAD('DUMP',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT DUMP_LOOP
   READ(LU_INPUT,DUMP,END=23,ERR=24,IOSTAT=IOS)
   24 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with DUMP line.') ; RETURN ; ENDIF
ENDDO DUMP_LOOP
23 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Keep track of whether the output timing intervals are specified by the user or not

IF (DT_BNDF_SPECIFIED   /= DT_BNDF)    THEN ; DT_BNDF_SPECIFIED   = DT_BNDF    ; ELSE ; DT_BNDF_SPECIFIED   = -1._EB ; ENDIF
IF (DT_CPU_SPECIFIED    /= DT_CPU)     THEN ; DT_CPU_SPECIFIED    = DT_CPU     ; ELSE ; DT_CPU_SPECIFIED    = -1._EB ; ENDIF
IF (DT_CTRL_SPECIFIED   /= DT_CTRL)    THEN ; DT_CTRL_SPECIFIED   = DT_CTRL    ; ELSE ; DT_CTRL_SPECIFIED   = -1._EB ; ENDIF
IF (DT_DEVC_SPECIFIED   /= DT_DEVC)    THEN ; DT_DEVC_SPECIFIED   = DT_DEVC    ; ELSE ; DT_DEVC_SPECIFIED   = -1._EB ; ENDIF
IF (DT_FLUSH_SPECIFIED  /= DT_FLUSH)   THEN ; DT_FLUSH_SPECIFIED  = DT_FLUSH   ; ELSE ; DT_FLUSH_SPECIFIED  = -1._EB ; ENDIF
IF (DT_GEOM_SPECIFIED   /= DT_GEOM)    THEN ; DT_GEOM_SPECIFIED   = DT_GEOM    ; ELSE ; DT_GEOM_SPECIFIED   = -1._EB ; ENDIF
IF (DT_HRR_SPECIFIED    /= DT_HRR)     THEN ; DT_HRR_SPECIFIED    = DT_HRR     ; ELSE ; DT_HRR_SPECIFIED    = -1._EB ; ENDIF
IF (DT_HVAC_SPECIFIED   /= DT_HVAC)    THEN ; DT_HVAC_SPECIFIED   = DT_HVAC    ; ELSE ; DT_HVAC_SPECIFIED   = -1._EB ; ENDIF
IF (DT_ISOF_SPECIFIED   /= DT_ISOF)    THEN ; DT_ISOF_SPECIFIED   = DT_ISOF    ; ELSE ; DT_ISOF_SPECIFIED   = -1._EB ; ENDIF
IF (DT_MASS_SPECIFIED   /= DT_MASS)    THEN ; DT_MASS_SPECIFIED   = DT_MASS    ; ELSE ; DT_MASS_SPECIFIED   = -1._EB ; ENDIF
IF (DT_PART_SPECIFIED   /= DT_PART)    THEN ; DT_PART_SPECIFIED   = DT_PART    ; ELSE ; DT_PART_SPECIFIED   = -1._EB ; ENDIF
IF (DT_PL3D_SPECIFIED   /= DT_PL3D)    THEN ; DT_PL3D_SPECIFIED   = DT_PL3D    ; ELSE ; DT_PL3D_SPECIFIED   = -1._EB ; ENDIF
IF (DT_PROF_SPECIFIED   /= DT_PROF)    THEN ; DT_PROF_SPECIFIED   = DT_PROF    ; ELSE ; DT_PROF_SPECIFIED   = -1._EB ; ENDIF
IF (DT_RADF_SPECIFIED   /= DT_RADF)    THEN ; DT_RADF_SPECIFIED   = DT_RADF    ; ELSE ; DT_RADF_SPECIFIED   = -1._EB ; ENDIF
IF (DT_RESTART_SPECIFIED/= DT_RESTART) THEN ; DT_RESTART_SPECIFIED= DT_RESTART ; ELSE ; DT_RESTART_SPECIFIED= -1._EB ; ENDIF
IF (DT_SLCF_SPECIFIED   /= DT_SLCF)    THEN ; DT_SLCF_SPECIFIED   = DT_SLCF    ; ELSE ; DT_SLCF_SPECIFIED   = -1._EB ; ENDIF
IF (DT_SL3D_SPECIFIED   /= DT_SL3D)    THEN ; DT_SL3D_SPECIFIED   = DT_SL3D    ; ELSE ; DT_SL3D_SPECIFIED   = -1._EB ; ENDIF
IF (DT_SMOKE3D_SPECIFIED/= DT_SMOKE3D) THEN ; DT_SMOKE3D_SPECIFIED= DT_SMOKE3D ; ELSE ; DT_SMOKE3D_SPECIFIED= -1._EB ; ENDIF
IF (DT_UVW_SPECIFIED    /= DT_UVW)     THEN ; DT_UVW_SPECIFIED    = DT_UVW     ; ELSE ; DT_UVW_SPECIFIED    = -1._EB ; ENDIF
IF (DT_TMP_SPECIFIED    /= DT_TMP)     THEN ; DT_TMP_SPECIFIED    = DT_TMP     ; ELSE ; DT_TMP_SPECIFIED    = -1._EB ; ENDIF
IF (DT_SPEC_SPECIFIED   /= DT_SPEC)    THEN ; DT_SPEC_SPECIFIED   = DT_SPEC    ; ELSE ; DT_SPEC_SPECIFIED   = -1._EB ; ENDIF

! Timing RAMPs

IF (RAMP_BNDF   /='null') CALL GET_RAMP_INDEX(RAMP_BNDF   ,'TIME',RAMP_BNDF_INDEX)
IF (RAMP_CPU    /='null') CALL GET_RAMP_INDEX(RAMP_CPU    ,'TIME',RAMP_CPU_INDEX)
IF (RAMP_CTRL   /='null') CALL GET_RAMP_INDEX(RAMP_CTRL   ,'TIME',RAMP_CTRL_INDEX)
IF (RAMP_DEVC   /='null') CALL GET_RAMP_INDEX(RAMP_DEVC   ,'TIME',RAMP_DEVC_INDEX)
IF (RAMP_FLUSH  /='null') CALL GET_RAMP_INDEX(RAMP_FLUSH  ,'TIME',RAMP_FLSH_INDEX)
IF (RAMP_GEOM   /='null') CALL GET_RAMP_INDEX(RAMP_GEOM   ,'TIME',RAMP_GEOM_INDEX)
IF (RAMP_HRR    /='null') CALL GET_RAMP_INDEX(RAMP_HRR    ,'TIME',RAMP_HRR_INDEX)
IF (RAMP_HVAC   /='null') CALL GET_RAMP_INDEX(RAMP_HRR    ,'TIME',RAMP_HVAC_INDEX)
IF (RAMP_ISOF   /='null') CALL GET_RAMP_INDEX(RAMP_ISOF   ,'TIME',RAMP_ISOF_INDEX)
IF (RAMP_MASS   /='null') CALL GET_RAMP_INDEX(RAMP_MASS   ,'TIME',RAMP_MASS_INDEX)
IF (RAMP_PART   /='null') CALL GET_RAMP_INDEX(RAMP_PART   ,'TIME',RAMP_PART_INDEX)
IF (RAMP_PL3D   /='null') CALL GET_RAMP_INDEX(RAMP_PL3D   ,'TIME',RAMP_PL3D_INDEX)
IF (RAMP_PROF   /='null') CALL GET_RAMP_INDEX(RAMP_PROF   ,'TIME',RAMP_PROF_INDEX)
IF (RAMP_RADF   /='null') CALL GET_RAMP_INDEX(RAMP_RADF   ,'TIME',RAMP_RADF_INDEX)
IF (RAMP_RESTART/='null') CALL GET_RAMP_INDEX(RAMP_RESTART,'TIME',RAMP_RSRT_INDEX)
IF (RAMP_SLCF   /='null') CALL GET_RAMP_INDEX(RAMP_SLCF   ,'TIME',RAMP_SLCF_INDEX)
IF (RAMP_SL3D   /='null') CALL GET_RAMP_INDEX(RAMP_SL3D   ,'TIME',RAMP_SL3D_INDEX)
IF (RAMP_SMOKE3D/='null') CALL GET_RAMP_INDEX(RAMP_SMOKE3D,'TIME',RAMP_SM3D_INDEX)
IF (RAMP_UVW    /='null') CALL GET_RAMP_INDEX(RAMP_UVW    ,'TIME',RAMP_UVW_INDEX)
IF (RAMP_TMP    /='null') CALL GET_RAMP_INDEX(RAMP_TMP    ,'TIME',RAMP_TMP_INDEX)
IF (RAMP_SPEC   /='null') CALL GET_RAMP_INDEX(RAMP_SPEC   ,'TIME',RAMP_SPEC_INDEX)

! Set format of real number output

WRITE(FMT_R,'(A,I2.2,A,I2.2,A,I1.1)') 'ES',SIG_FIGS+SIG_FIGS_EXP+4,'.',SIG_FIGS-1,'E',SIG_FIGS_EXP

! Check Plot3D QUANTITIES

PLOOP: DO N=1,5
   CALL GET_QUANTITY_INDEX(PLOT3D_SMOKEVIEW_LABEL(N),PLOT3D_SMOKEVIEW_BAR_LABEL(N),PLOT3D_QUANTITY_INDEX(N),I_DUM(1), &
                           PLOT3D_Y_INDEX(N),PLOT3D_Z_INDEX(N),PLOT3D_PART_INDEX(N),I_DUM(2),I_DUM(3),I_DUM(4),I_DUM(5),'PLOT3D', &
                           PLOT3D_QUANTITY(N),'null',PLOT3D_SPEC_ID(N),PLOT3D_PART_ID(N),'null','null','null','null',&
                           -1._EB,I_DUM(6))
   IF (OUTPUT_QUANTITY(PLOT3D_QUANTITY_INDEX(N))%INTEGRATED_PARTICLES) PL3D_PARTICLE_FLUX = .TRUE.
ENDDO PLOOP

! Check SMOKE3D viability

IF (TWO_D .OR. SOLID_PHASE_ONLY) SMOKE3D = .FALSE.

END SUBROUTINE READ_DUMP


!> \brief Read the SPEC namelist lines containing parameters for gas species

SUBROUTINE READ_SPEC

USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX
USE PHYSICAL_FUNCTIONS, ONLY : WATER_VAPOR_MASS_FRACTION, GET_SPECIFIC_GAS_CONSTANT
USE MISC_FUNCTIONS, ONLY: GET_SPEC_OR_SMIX_INDEX
USE PROPERTY_DATA, ONLY: GAS_PROPS,FED_PROPS,ELEMENT
USE SOOT_ROUTINES, ONLY: PARTICLE_RADIUS, INITIALIZE_AGGLOMERATION
REAL(EB) :: MW,SIGMALJ,EPSILONKLJ,VISCOSITY,CONDUCTIVITY,DIFFUSIVITY,MASS_EXTINCTION_COEFFICIENT, &
            SPECIFIC_HEAT,REFERENCE_ENTHALPY,REFERENCE_TEMPERATURE,FIC_CONCENTRATION,FLD_LETHAL_DOSE,&
            SPECIFIC_HEAT_LIQUID,DENSITY_LIQUID,VAPORIZATION_TEMPERATURE,HEAT_OF_VAPORIZATION,MELTING_TEMPERATURE,&
            H_V_REFERENCE_TEMPERATURE,MEAN_DIAMETER,CONDUCTIVITY_SOLID,DENSITY_SOLID,ENTHALPY_OF_FORMATION,&
            MASS_FRACTION_COND_0,MASS_FRACTION_0,TURBULENT_SCHMIDT_NUMBER,&
            CONVERSION,PR_GAS,CONDUCTIVITY_LIQUID,VISCOSITY_LIQUID,BETA_LIQUID,THERMOPHORETIC_DIAMETER,C,H,O,N
REAL(EB):: MASS_FRACTION(MAX_SPECIES),VOLUME_FRACTION(MAX_SPECIES),MIN_DIAMETER,MAX_DIAMETER
REAL(EB), ALLOCATABLE, DIMENSION(:) :: ZZ_GET
INTEGER  :: N_SPEC_READ=0,N1,NN,NNN,NS2,NR,NR2,N_SUB_SPECIES,NS,N_BINS,Y_S,Z_S
INTEGER  :: N_COPY=0,N_TOTAL_BINS=0,N_LUMPED=0,N_CONDENSATION=0,N_PRIMITIVE=0,N_COPY_PRIMITIVE=0,N_PREDEFINED=0,&
            N_PREDEFINED_SMIX=0,N_SIMPLE_FUEL=0,DEFINED_BACKGROUND=0
INTEGER  :: PROD_COUNTER=0
INTEGER, ALLOCATABLE, DIMENSION(:) :: Y_INDEX, N_DEFINE
LOGICAL  :: LUMPED_COMPONENT_ONLY,AEROSOL,BACKGROUND,PRIMITIVE,COPY_LUMPED,FOUND,KEEP_READING=.TRUE.
CHARACTER(LABEL_LENGTH) :: RAMP_CP,RAMP_CP_L,RAMP_K,RAMP_MU,RAMP_D,RADCAL_ID,RAMP_G_F,SPEC_ID(MAX_SPECIES)

CHARACTER(LABEL_LENGTH), ALLOCATABLE, DIMENSION(:) :: PREDEFINED_SMIX_ID,PREDEFINED_SPEC_ID,SPEC_ID_READ
CHARACTER(FORMULA_LENGTH) :: FORMULA
TYPE(SPECIES_TYPE), POINTER :: SS=>NULL()
TYPE(SPECIES_MIXTURE_TYPE), POINTER :: SM=>NULL()
NAMELIST /SPEC/ AEROSOL,BACKGROUND,BETA_LIQUID,C,CONDUCTIVITY,CONDUCTIVITY_LIQUID,CONDUCTIVITY_SOLID,COPY_LUMPED, &
                DENSITY_LIQUID,DENSITY_SOLID,DIFFUSIVITY,ENTHALPY_OF_FORMATION,EPSILONKLJ,FIC_CONCENTRATION,FLD_LETHAL_DOSE, &
                FORMULA,FYI,H,HEAT_OF_VAPORIZATION,H_V_REFERENCE_TEMPERATURE,ID,LUMPED_COMPONENT_ONLY,&
                MASS_EXTINCTION_COEFFICIENT,MASS_FRACTION,MASS_FRACTION_COND_0,MASS_FRACTION_0,MAX_DIAMETER,MEAN_DIAMETER,&
                MELTING_TEMPERATURE,MIN_DIAMETER,MW,N,N_BINS,O,PR_GAS,PRIMITIVE,RADCAL_ID,&
                RAMP_CP,RAMP_CP_L,RAMP_D,RAMP_G_F,RAMP_K,RAMP_MU,REFERENCE_ENTHALPY,REFERENCE_TEMPERATURE,&
                SIGMALJ,SPEC_ID,SPECIFIC_HEAT,SPECIFIC_HEAT_LIQUID,THERMOPHORETIC_DIAMETER,TURBULENT_SCHMIDT_NUMBER,&
                VAPORIZATION_TEMPERATURE,VISCOSITY,VISCOSITY_LIQUID,VOLUME_FRACTION

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Count types of species and lumped species defined in input
DEFINED_BACKGROUND = 0
COUNT_SPEC_LINES: DO
   CALL SET_SPEC_DEFAULT
   READ(LU_INPUT,NML=SPEC,END=19,ERR=20,IOSTAT=IOS)
   N_SPEC_READ = N_SPEC_READ+1
20 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with SPECies number ',N_SPEC_READ+1,', line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SPEC_ID(1)=='null') THEN
      N_PRIMITIVE = N_PRIMITIVE + 1
      IF (N_BINS>0) THEN
         N_TOTAL_BINS = N_TOTAL_BINS + N_BINS
         N_AGGLOMERATION_SPECIES = N_AGGLOMERATION_SPECIES + 1
         IF (N_BINS < 2) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(132): SPEC ',N_SPEC_READ,': N_BINS must be >=2.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (.NOT. AEROSOL) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(133): SPEC ',N_SPEC_READ,': AEROSOL must be .TRUE. to use N_BINS.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (ABS(MEAN_DIAMETER - 1.E-6_EB) < TWO_EPSILON_EB) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(134): SPEC ',N_SPEC_READ,': Do not specify MEAN_DIAMETER and N_BINS.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (MAX_DIAMETER < 0._EB) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(135): SPEC ',N_SPEC_READ,': MAX_DIAMETER not set'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (MIN_DIAMETER < 0._EB) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(135): SPEC ',N_SPEC_READ,': MIN_DIAMETER not set'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (MAX_DIAMETER <= MIN_DIAMETER) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(136): SPEC ',N_SPEC_READ,': MAX_DIAMETER <= MIN_DIAMETER.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (.NOT. LUMPED_COMPONENT_ONLY) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(137): SPEC ',N_SPEC_READ,': LUMPED_COMPONENT_ONLY must be .TRUE. to use N_BINS'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF
      IF (.NOT. LUMPED_COMPONENT_ONLY) N_LUMPED = N_LUMPED+1
      IF (AEROSOL) THEN
         IF (CHECK_CONDENSABLE(VAPORIZATION_TEMPERATURE,ID)) THEN
            N_CONDENSATION = N_CONDENSATION + 1
            IF (N_BINS>0) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(138): SPEC ',N_SPEC_READ,': Cannot set N_BINS for a condensable species'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (LUMPED_COMPONENT_ONLY) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(139): SPEC ',N_SPEC_READ,': Condensable species cannot be LUMPED_COMPONENT_ONLY'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (ANY(SPEC_ID/='null')) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(140): SPEC ',N_SPEC_READ,': Condensable species cannot be a lumped species.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDIF
      ENDIF
   ELSE
      IF (PRIMITIVE) THEN
         N_COPY_PRIMITIVE = N_COPY_PRIMITIVE + 1
         IF (SPEC_ID(1)/='null' .AND. SPEC_ID(2)/='null') THEN
            WRITE(MESSAGE,'(A,I0,A)') &
               'ERROR(141): Species ',N_SPEC_READ,' is primitive but more than one SPEC_ID given.'
            CALL SHUTDOWN(MESSAGE)          ; RETURN
         ENDIF
      ELSE
         IF (COPY_LUMPED) THEN
            IF (N_BINS >0) THEN
               WRITE(MESSAGE,'(A,A,A)') 'ERROR(142): SPEC ' ,TRIM(ID),', cannot specify both COPY_LUMPED and N_BINS.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (BACKGROUND) THEN
               WRITE(MESSAGE,'(A,A,A)') 'ERROR(143): SPEC ' ,TRIM(ID),', cannot specify both COPY_LUMPED and BACKGROUND.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            N_COPY = N_COPY + 1
         ELSE
            N_LUMPED = N_LUMPED + 1
         ENDIF
      ENDIF
   ENDIF
   IF (BACKGROUND) THEN
      IF (DEFINED_BACKGROUND == 0) THEN
         DEFINED_BACKGROUND = N_LUMPED + N_TOTAL_BINS + N_COPY + N_CONDENSATION
      ELSE
         WRITE(MESSAGE,'(A)') 'ERROR(144): Only one BACKGROUND SPECies can be defined.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
ENDDO COUNT_SPEC_LINES
19 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ALLOCATE(SPEC_ID_READ(N_SPEC_READ))

! Get species names and do error checking
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
DO N1=1,N_SPEC_READ
   CALL SET_SPEC_DEFAULT
   READ(LU_INPUT,NML=SPEC,IOSTAT=IOS)
   SPEC_ID_READ(N1) = ID
   IF (BACKGROUND) THEN
      IF (LUMPED_COMPONENT_ONLY) THEN
         WRITE(MESSAGE,'(A)') 'ERROR(145): Cannot define a LUMPED_COMPONENT_ONLY species as the BACKGROUND species'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SIMPLE_CHEMISTRY) THEN
         WRITE(MESSAGE,'(A)') 'ERROR(146): Can not define a BACKGROUND species or redefine AIR when using simple chemistry.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
   IF (ID=='null') THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(147): Species ',N_SPEC_READ, ' needs a name (ID=...)'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (ID=='AIR' .AND. .NOT. BACKGROUND) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(148): SPEC ',TRIM(ID),' cannot redefine AIR unless it is the BACKGROUND species.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SPECIFIC_HEAT > 0._EB .AND. RAMP_CP/='null') THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(149): SPEC ',TRIM(ID),' cannot specify both SPECIFIC_HEAT and RAMP_CP.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SPECIFIC_HEAT_LIQUID > 0._EB .AND. RAMP_CP_L/='null') THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(150): SPEC ',TRIM(ID),' cannot specify both SPECIFIC_HEAT_LIQUID and RAMP_CP_L.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (CONDUCTIVITY > 0._EB .AND. RAMP_K/='null') THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(151): SPEC ',TRIM(ID),' cannot specify both CONDUCTIVITY and RAMP_K.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (DIFFUSIVITY > 0._EB .AND. RAMP_D/='null') THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(152): SPEC ',TRIM(ID),' cannot specify both DIFFUSIVITY and RAMP_D.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (VISCOSITY > 0._EB .AND. RAMP_MU/='null') THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(153): SPEC ',TRIM(ID),' cannot specify both VISCOSITY and RAMP_MU.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (REFERENCE_ENTHALPY > -1.E20_EB .AND. ENTHALPY_OF_FORMATION > -1.E20_EB) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(154): SPEC ',TRIM(ID),', cannot define both REFERENCE_ENTHALPY and ENTHALPY_OF_FORMATION.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   DO NN = 1,N1-1
      IF (ID==SPEC_ID_READ(NN)) THEN
         WRITE(MESSAGE,'(A,I0,A,I0,A)') 'ERROR(155): Species ',N1,' has the same ID as species ',NN, '.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO
   IF (LUMPED_COMPONENT_ONLY .AND. MASS_FRACTION_0>0._EB) THEN
      WRITE(MESSAGE,'(A)') 'ERROR(156): Cannot define MASS_FRACTION_0 for a LUMPED_COMPONENT_ONLY species'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF ((HEAT_OF_VAPORIZATION >  0._EB .AND. SPECIFIC_HEAT_LIQUID <= 0._EB .AND. RAMP_CP_L=='null') .OR. &
       (HEAT_OF_VAPORIZATION <= 0._EB .AND. SPECIFIC_HEAT_LIQUID >  0._EB .AND. RAMP_CP_L/='null')) THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(157): SPEC ' ,N1, &
                                ': If one of SPECIFIC_HEAT_LIQUID (or RAMP_CL_L) or HEAT_OF_VAPORIZATION defined, both must be'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (N_BINS==0 .AND. AEROSOL .AND. MEAN_DIAMETER < 0._EB) THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(158): SPEC ',N_SPEC_READ,': No MEAN_DIAMETER given.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (ANY(MASS_FRACTION>0._EB) .AND. ANY(VOLUME_FRACTION>0._EB)) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(159): SPEC ' ,TRIM(ID),', cannot specify both MASS_FRACTION and VOLUME_FRACTION.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (FORMULA/='null' .AND. (C >0._EB .OR. H>0._EB .OR. O>0._EB .OR. N>0._EB)) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(160): SPEC ' ,TRIM(ID),', cannot specify both FORMULA and C,H,O, or N.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO

! Determined predefined species
PREDEF_SPEC_IF: IF (SIMPLE_CHEMISTRY) THEN
   IF (.NOT. ANY(SPEC_ID_READ=='NITROGEN')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='OXYGEN')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='WATER VAPOR')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='CARBON DIOXIDE')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='CARBON MONOXIDE')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='SOOT')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='HYDROGEN')) N_PREDEFINED = N_PREDEFINED + 1
   IF (.NOT. ANY(SPEC_ID_READ=='HYDROGEN CYANIDE')) N_PREDEFINED = N_PREDEFINED + 1
   ALLOCATE(PREDEFINED_SPEC_ID(N_PREDEFINED))
   N_PREDEFINED = 0
   IF (.NOT. ANY(SPEC_ID_READ=='NITROGEN')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'NITROGEN'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='OXYGEN')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'OXYGEN'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='CARBON DIOXIDE')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'CARBON DIOXIDE'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='WATER VAPOR')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'WATER VAPOR'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='CARBON MONOXIDE')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'CARBON MONOXIDE'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='SOOT')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'SOOT'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='HYDROGEN')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'HYDROGEN'
   ENDIF
   IF (.NOT. ANY(SPEC_ID_READ=='HYDROGEN CYANIDE')) THEN
      N_PREDEFINED = N_PREDEFINED + 1
      PREDEFINED_SPEC_ID(N_PREDEFINED) = 'HYDROGEN CYANIDE'
   ENDIF
   DO NR=1,N_REACTIONS
      IF (.NOT. REACTION(NR)%SIMPLE_CHEMISTRY .OR. REACTION(NR)%PAIR_INDEX < NR) CYCLE
      IF (ALL(SPEC_ID_READ/=REACTION(NR)%FUEL)) N_SIMPLE_FUEL = N_SIMPLE_FUEL + 1
   ENDDO
ELSE PREDEF_SPEC_IF ! No simple chemistry
   IF (DEFINED_BACKGROUND==0) THEN
      IF (.NOT. ANY(SPEC_ID_READ=='NITROGEN')) N_PREDEFINED = N_PREDEFINED + 1
      IF (.NOT. ANY(SPEC_ID_READ=='OXYGEN')) N_PREDEFINED = N_PREDEFINED + 1
      IF (.NOT. ANY(SPEC_ID_READ=='CARBON DIOXIDE')) N_PREDEFINED = N_PREDEFINED + 1
      IF (.NOT. ANY(SPEC_ID_READ=='WATER VAPOR')) N_PREDEFINED = N_PREDEFINED + 1
      ALLOCATE(PREDEFINED_SPEC_ID(N_PREDEFINED))
      N_PREDEFINED = 0
      IF (.NOT. ANY(SPEC_ID_READ=='NITROGEN')) THEN
         N_PREDEFINED = N_PREDEFINED + 1
         PREDEFINED_SPEC_ID(N_PREDEFINED) = 'NITROGEN'
      ENDIF
      IF (.NOT. ANY(SPEC_ID_READ=='OXYGEN')) THEN
         N_PREDEFINED = N_PREDEFINED + 1
         PREDEFINED_SPEC_ID(N_PREDEFINED) = 'OXYGEN'
      ENDIF
      IF (.NOT. ANY(SPEC_ID_READ=='CARBON DIOXIDE')) THEN
         N_PREDEFINED = N_PREDEFINED + 1
         PREDEFINED_SPEC_ID(N_PREDEFINED) = 'CARBON DIOXIDE'
      ENDIF
      IF (.NOT. ANY(SPEC_ID_READ=='WATER VAPOR')) THEN
         N_PREDEFINED = N_PREDEFINED + 1
         PREDEFINED_SPEC_ID(N_PREDEFINED) = 'WATER VAPOR'
      ENDIF
   ENDIF
ENDIF PREDEF_SPEC_IF

N_SPECIES = N_PRIMITIVE + N_PREDEFINED + N_COPY_PRIMITIVE + N_SIMPLE_FUEL

! Determine predefined lumped species
PREDEF_SMIX_ID: IF (SIMPLE_CHEMISTRY) THEN
   N_PREDEFINED_SMIX = 1
   DO NR=1,N_REACTIONS
      IF (.NOT. REACTION(NR)%SIMPLE_CHEMISTRY .OR. REACTION(NR)%PAIR_INDEX < NR) CYCLE
      IF (ALL(SPEC_ID_READ/=REACTION(NR)%FUEL)) N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 1
      IF (REACTION(NR)%N_SIMPLE_CHEMISTRY_REACTIONS==1) N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 1
      IF (REACTION(NR)%N_SIMPLE_CHEMISTRY_REACTIONS==2) N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 2
   ENDDO
   ALLOCATE(PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX))
   N_PREDEFINED_SMIX = 1
   PREDEFINED_SMIX_ID(1) = 'AIR'
   DO NR=1,N_REACTIONS
      IF (.NOT. REACTION(NR)%SIMPLE_CHEMISTRY .OR. REACTION(NR)%PAIR_INDEX < NR) CYCLE
      PROD_COUNTER = PROD_COUNTER + 1
      IF (ALL(SPEC_ID_READ/=REACTION(NR)%FUEL)) THEN
         IF (.NOT. SIMPLE_FUEL_DEFINED(NR)) THEN
            WRITE(MESSAGE,'(A,A,A)') 'ERROR(161): Simple chemistry FUEL, ',TRIM(REACTION(NR)%FUEL),', not defined on REAC or SPEC.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 1
         PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX)=REACTION(NR)%FUEL
      ENDIF
      IF (REACTION(NR)%N_SIMPLE_CHEMISTRY_REACTIONS==1) THEN
         N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 1
         IF (PROD_COUNTER == 1) THEN
            PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX)='PRODUCTS'
         ELSE
            WRITE(PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX),'(A,I0)') 'PRODUCTS ',PROD_COUNTER
         ENDIF
      ENDIF
      IF (REACTION(NR)%N_SIMPLE_CHEMISTRY_REACTIONS==2) THEN
         N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 1
         IF (PROD_COUNTER == 1) THEN
            PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX)='INTERMEDIATE PRODUCTS'
         ELSE
            WRITE(PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX),'(A,I0)') 'INTERMEDIATE PRODUCTS ',PROD_COUNTER
         ENDIF
         N_PREDEFINED_SMIX = N_PREDEFINED_SMIX + 1
         IF (PROD_COUNTER == 1) THEN
            PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX)='PRODUCTS'
         ELSE
            WRITE(PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX),'(A,I0)') 'PRODUCTS ',PROD_COUNTER
         ENDIF
      ENDIF
   ENDDO
ELSE PREDEF_SMIX_ID ! No simple chemistry
   IF (DEFINED_BACKGROUND==0) THEN
      N_PREDEFINED_SMIX = 1
      ALLOCATE(PREDEFINED_SMIX_ID(N_PREDEFINED_SMIX))
      PREDEFINED_SMIX_ID(1) = 'AIR'
   ENDIF
ENDIF PREDEF_SMIX_ID

IF (N_PREDEFINED_SMIX > 0) THEN
   DO N1=1,N_SPEC_READ
      IF (ANY(PREDEFINED_SMIX_ID==SPEC_ID_READ(N1))) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(162): SPEC ',TRIM(ID),' has the same name as a predefined lumped species.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO
ENDIF

N_TRACKED_SPECIES = N_LUMPED + N_COPY + N_TOTAL_BINS + N_PREDEFINED_SMIX + N_CONDENSATION
N_TOTAL_SCALARS = N_TRACKED_SPECIES + N_PASSIVE_SCALARS

! Allocate the primitive species array.
ALLOCATE(SPECIES(N_SPECIES),STAT=IZERO)
CALL ChkMemErr('READ','SPECIES',IZERO)
ALLOCATE(Y_INDEX(N_SPECIES))
ALLOCATE(N_DEFINE(N_TRACKED_SPECIES))
ALLOCATE(SPECIES_MIXTURE(N_TOTAL_SCALARS),STAT=IZERO)
CALL ChkMemErr('READ','SPECIES_MIXTURE',IZERO)

IF (N_AGGLOMERATION_SPECIES > 0) THEN
   ALLOCATE(N_PARTICLE_BINS(N_AGGLOMERATION_SPECIES))
   ALLOCATE(AGGLOMERATION_SPEC_INDEX(N_AGGLOMERATION_SPECIES))
   AGGLOMERATION_SPEC_INDEX = -1
   ALLOCATE(AGGLOMERATION_SMIX_INDEX(N_AGGLOMERATION_SPECIES))
   AGGLOMERATION_SMIX_INDEX = -1
   ALLOCATE(MIN_PARTICLE_DIAMETER(N_AGGLOMERATION_SPECIES))
   ALLOCATE(MAX_PARTICLE_DIAMETER(N_AGGLOMERATION_SPECIES))
   N_AGGLOMERATION_SPECIES = 0
ENDIF

! Setup SPEC arrays

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
NR2 = 1
FOUND = .FALSE.
PRIMITIVE_LOOP: DO N1=1,N_SPECIES - N_COPY_PRIMITIVE
   IF (N1 <= N_PREDEFINED) THEN
      CALL SET_SPEC_DEFAULT
      ID = PREDEFINED_SPEC_ID(N1)
      LUMPED_COMPONENT_ONLY = .TRUE.
   ELSEIF (N1>N_PREDEFINED .AND. N1<=N_SPECIES-N_COPY_PRIMITIVE-N_SIMPLE_FUEL) THEN
      DO WHILE (KEEP_READING)
         CALL SET_SPEC_DEFAULT
         READ(LU_INPUT,NML=SPEC,IOSTAT=IOS)
         IF (TRIM(SPEC_ID(1))=='null') EXIT
      ENDDO
   ELSE
      CALL SET_SPEC_DEFAULT
      FOUND = .FALSE.
      DO WHILE (.NOT. FOUND)
         IF (REACTION(NR2)%SIMPLE_CHEMISTRY .AND. REACTION(NR2)%PAIR_INDEX > NR2) THEN
            IF (ALL(SPEC_ID_READ/=REACTION(NR2)%FUEL)) THEN
               FOUND = .TRUE.
               ID = REACTION(NR2)%FUEL
               MW = REACTION(NR2)%MW_FUEL
               NR2 = NR2 + 1
               EXIT
            ENDIF
         ENDIF
         NR2 = NR2 + 1
      ENDDO
   ENDIF

   SS => SPECIES(N1)
   SS%K_USER                      = CONDUCTIVITY
   SS%CONDUCTIVITY_SOLID          = CONDUCTIVITY_SOLID
   SS%D_USER                      = DIFFUSIVITY
   SS%DENSITY_SOLID               = DENSITY_SOLID
   SS%EPSK                        = EPSILONKLJ
   SS%FIC_CONCENTRATION           = FIC_CONCENTRATION*1.E-6_EB
   SS%FLD_LETHAL_DOSE             = FLD_LETHAL_DOSE*1.E-6_EB
   SS%FORMULA                     = FORMULA
   SS%H_F                         = ENTHALPY_OF_FORMATION*1000._EB
   SS%ID                          = ID
   SS%RADCAL_ID                   = RADCAL_ID
   SS%MASS_EXTINCTION_COEFFICIENT = MAX(0._EB,MASS_EXTINCTION_COEFFICIENT)
   SS%MEAN_DIAMETER               = MEAN_DIAMETER
   SS%THERMOPHORETIC_DIAMETER     = THERMOPHORETIC_DIAMETER
   SS%MU_USER                     = VISCOSITY
   SS%MW                          = MW
   SS%PR_USER                     = PR_GAS
   SS%RAMP_CP                     = RAMP_CP
   SS%RAMP_CP_L                   = RAMP_CP_L
   SS%RAMP_D                      = RAMP_D
   SS%RAMP_K                      = RAMP_K
   SS%RAMP_G_F                    = RAMP_G_F
   IF (SS%RAMP_G_F/='null') SS%EXPLICIT_G_F = .TRUE.
   SS%RAMP_MU                     = RAMP_MU
   IF (REFERENCE_TEMPERATURE < -TMPM) REFERENCE_TEMPERATURE = 25._EB
   SS%REFERENCE_TEMPERATURE       = REFERENCE_TEMPERATURE + TMPM
   SS%SC_T_USER                   = TURBULENT_SCHMIDT_NUMBER
   SS%SIG                         = SIGMALJ
   SS%SPECIFIC_HEAT               = SPECIFIC_HEAT*1000._EB
   SS%REFERENCE_ENTHALPY          = REFERENCE_ENTHALPY*1000._EB
   SS%YY0                         = MAX(0._EB,MASS_FRACTION_0)

   SS%DENSITY_LIQUID              = DENSITY_LIQUID
   SS%BETA_LIQUID                 = BETA_LIQUID
   SS%K_LIQUID                    = CONDUCTIVITY_LIQUID
   SS%MU_LIQUID                   = VISCOSITY_LIQUID

   SS%SPECIFIC_HEAT_LIQUID        = SPECIFIC_HEAT_LIQUID*1000._EB
   SS%HEAT_OF_VAPORIZATION        = HEAT_OF_VAPORIZATION*1000._EB
   SS%TMP_MELT = MELTING_TEMPERATURE + TMPM
   IF (H_V_REFERENCE_TEMPERATURE < -TMPM) H_V_REFERENCE_TEMPERATURE = MELTING_TEMPERATURE
   SS%H_V_REFERENCE_TEMPERATURE = H_V_REFERENCE_TEMPERATURE + 273.15_EB
   SS%TMP_V = VAPORIZATION_TEMPERATURE + TMPM

   IF (AEROSOL) SS%CONDENSABLE = CHECK_CONDENSABLE(SS%TMP_V,SS%ID)

   IF (N_BINS > 0) THEN
      N_AGGLOMERATION_SPECIES = N_AGGLOMERATION_SPECIES + 1
      N_PARTICLE_BINS(N_AGGLOMERATION_SPECIES) = N_BINS
      MAX_PARTICLE_DIAMETER(N_AGGLOMERATION_SPECIES) = MAX_DIAMETER
      MIN_PARTICLE_DIAMETER(N_AGGLOMERATION_SPECIES) = MIN_DIAMETER
      AGGLOMERATION_SPEC_INDEX(N_AGGLOMERATION_SPECIES)=N1
      SS%AGGLOMERATING = .TRUE.
   ENDIF

   SS%PROP_ID = ID
   CALL GET_PROP_INDEX(SS%PROP_ID,SS%PROP_INDEX,SS%EXPLICIT_G_F)
   IF ((SS%SPECIFIC_HEAT > 0._EB .OR. SS%RAMP_CP /='null') .AND. SS%RAMP_G_F=='null') SS%EXPLICIT_G_F = .FALSE.

   CALL GAS_PROPS(SS%PROP_INDEX,SS%SIG,SS%EPSK,SS%PR_USER,SS%MW,SS%FORMULA,SS%LISTED,SS%ATOMS,SS%H_F_LISTED,SS%RADCAL_ID)
   IF (SS%SPECIFIC_HEAT > 0._EB .OR. SS%RAMP_CP /='null' .OR. SS%REFERENCE_ENTHALPY > -1.E21_EB .OR. SS%H_F > -1.E21_EB) &
      SS%H_F_LISTED = -1.E30_EB
   IF (SS%PROP_ID == 'SOOT') THEN
      MW_SOOT = SS%MW
      SOOT_C_FRACTION = SS%ATOMS(6)/SUM(SS%ATOMS)
      SOOT_H_FRACTION = SS%ATOMS(1)/SUM(SS%ATOMS)
      SOOT_N_FRACTION = SS%ATOMS(7)/SUM(SS%ATOMS)
      SOOT_O_FRACTION = SS%ATOMS(8)/SUM(SS%ATOMS)
   ENDIF
   IF (.NOT.SS%LISTED) THEN
      WRITE(MESSAGE,'(A,A,A)') 'WARNING: SPEC ',TRIM(ID),' is not in the table of pre-defined species. '// &
                               'Any unassigned SPEC variables in the input were assigned the properties of nitrogen.'
      IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
   ENDIF

   IF (SS%REFERENCE_ENTHALPY > -1.E21_EB .OR. SS%H_F > -1.E21_EB .OR. SS%SPECIFIC_HEAT > 0._EB) SS%EXPLICIT_H_F = .TRUE.

   CALL FED_PROPS(SS%PROP_ID,SS%FLD_LETHAL_DOSE,SS%FIC_CONCENTRATION)

   IF (TRIM(SS%FORMULA)=='null') THEN
      WRITE(SS%FORMULA,'(A,I0)') 'SPEC_',N1
      IF (C >0._EB .OR. H>0._EB .OR. O>0._EB .OR. N>0._EB) THEN
         SS%MW = 0._EB
         IF (C > 0._EB) THEN
            SS%ATOMS(6) = C
            SS%MW = SS%MW + C * ELEMENT(6)%MASS
         ENDIF
         IF (H > 0._EB) THEN
            SS%ATOMS(1) = H
            SS%MW = SS%MW + H * ELEMENT(1)%MASS
         ENDIF
         IF (O > 0._EB) THEN
            SS%ATOMS(8) = O
            SS%MW = SS%MW + O * ELEMENT(8)%MASS
         ENDIF
         IF (N > 0._EB) THEN
            SS%ATOMS(7) = N
            SS%MW = SS%MW + N * ELEMENT(7)%MASS
         ENDIF
      ENDIF
   ENDIF

   ! For simple chemistry Determine if the species is the one specified on the REAC line(s)

   IF (SIMPLE_CHEMISTRY) THEN
      DO NR=1,N_REACTIONS
         IF (TRIM(ID)==TRIM(REACTION(NR)%FUEL)) THEN
            REACTION(NR)%FUEL_SMIX_INDEX = N1
            WRITE(FORMULA,'(A,I0)') 'SPEC_',N1
            IF (TRIM(SS%FORMULA)==TRIM(FORMULA) .AND. .NOT. (C>0._EB .OR. H>0._EB .OR. N>0._EB .OR. O>0._EB)) &
               SS%MW = REACTION(NR)%MW_FUEL
         ENDIF
      ENDDO
   ENDIF

   SS%RCON = R0/SS%MW
   SS%MODE = GAS_SPECIES

   ! Special processing of certain species

   SELECT CASE (ID)
      CASE('WATER VAPOR')
         H2O_INDEX = N1
         IF (MASS_FRACTION_0 > 0._EB .AND. LUMPED_COMPONENT_ONLY) THEN
            WRITE(MESSAGE,'(A)') 'WARNING: MASS_FRACTION_0 specified for WATER VAPOR with LUMPED_COMPONENT_ONLY = .TRUE.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         ENDIF
         IF (N_PREDEFINED_SMIX > 0) Y_H2O_INFTY = WATER_VAPOR_MASS_FRACTION(HUMIDITY,MIN(373.15_EB,TMPA),P_INF)
      CASE('CARBON DIOXIDE')
         CO2_INDEX = N1
      CASE('CARBON MONOXIDE')
         CO_INDEX = N1
      CASE('OXYGEN')
         O2_INDEX = N1
      CASE('NITROGEN')
         N2_INDEX = N1
      CASE('HYDROGEN')
         H2_INDEX = N1
      CASE('HYDROGEN CYANIDE')
         HCN_INDEX = N1
      CASE('NITRIC OXIDE')
         NO_INDEX = N1
      CASE('NITROGEN DIOXIDE')
         NO2_INDEX = N1
      CASE('SOOT')
         SOOT_INDEX = N1
         IF (MASS_EXTINCTION_COEFFICIENT < 0._EB) SS%MASS_EXTINCTION_COEFFICIENT = 8700._EB
   END SELECT

   IF (SS%RADCAL_ID=='SOOT' .AND. SOOT_INDEX==0) SOOT_INDEX = N1
   IF (SS%ID=='SOOT' .AND. AEROSOL_AL2O3) SS%DENSITY_SOLID = 4000.
   IF (AEROSOL) SS%MODE = AEROSOL_SPECIES

   ! Get ramps
   IF (SS%RAMP_CP/='null') THEN
      CALL GET_RAMP_INDEX(SS%RAMP_CP,'TEMPERATURE',NR)
      SS%RAMP_CP_INDEX = NR
   ENDIF
   IF (SS%RAMP_CP_L/='null') THEN
      CALL GET_RAMP_INDEX(SS%RAMP_CP_L,'TEMPERATURE',NR)
      SS%RAMP_CP_L_INDEX = NR
   ENDIF
   IF (SS%RAMP_D/='null') THEN
      CALL GET_RAMP_INDEX(SS%RAMP_D,'TEMPERATURE',NR)
      SS%RAMP_D_INDEX = NR
   ENDIF
   IF (SS%RAMP_G_F/='null') THEN
      CALL GET_RAMP_INDEX(SS%RAMP_G_F,'TEMPERATURE',NR)
      SS%RAMP_G_F_INDEX = NR
   ENDIF
   IF (SS%RAMP_K/='null') THEN
      CALL GET_RAMP_INDEX(SS%RAMP_K,'TEMPERATURE',NR)
      SS%RAMP_K_INDEX = NR
   ENDIF
   IF (SS%RAMP_MU/='null') THEN
      CALL GET_RAMP_INDEX(SS%RAMP_MU,'TEMPERATURE',NR)
      SS%RAMP_MU_INDEX = NR
   ENDIF

ENDDO PRIMITIVE_LOOP

! Setup copies of primitive SPEC

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
IF (N_COPY_PRIMITIVE > 0) THEN
   DO N1=1,N_COPY_PRIMITIVE
      DO WHILE (KEEP_READING)
         READ(LU_INPUT,NML=SPEC,IOSTAT=IOS)
         IF (PRIMITIVE) EXIT
      ENDDO
      NN = FINDLOC(SPECIES%ID,ID,1)
      SPECIES(N_SPECIES - N_COPY_PRIMITIVE - N_SIMPLE_FUEL + N1) = SPECIES(NN)
      SPECIES(N_SPECIES - N_COPY_PRIMITIVE - N_SIMPLE_FUEL + N1)%ID = ID
   ENDDO
ENDIF

IF (N_AGGLOMERATION_SPECIES > 0) CALL INITIALIZE_AGGLOMERATION

! Setup SPECIES_MIXTURE Array

! First do lumped species in input file in case a simple chemistry fuel is defined on SPEC

! Setup array for defining lumped to put background first if defined
DO NS=1,N_TRACKED_SPECIES
   IF (NS <DEFINED_BACKGROUND) THEN
      N_DEFINE(NS) = NS +1
   ELSEIF (NS==DEFINED_BACKGROUND) THEN
      N_DEFINE(NS) = 1
   ELSE
      N_DEFINE(NS) = NS
   ENDIF
ENDDO

IF (N_TRACKED_SPECIES > N_PREDEFINED_SMIX) THEN
   N_AGGLOMERATION_SPECIES = 0
   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   N1 = N_PREDEFINED_SMIX+1
   DO WHILE (N1<=N_TRACKED_SPECIES-N_COPY)
      DO WHILE (KEEP_READING)
         CALL SET_SPEC_DEFAULT
         READ(LU_INPUT,NML=SPEC,IOSTAT=IOS)
         IF (LUMPED_COMPONENT_ONLY .AND. N_BINS<=0) CYCLE
         EXIT
      ENDDO
      IF (N_BINS>0) THEN
         N_AGGLOMERATION_SPECIES = N_AGGLOMERATION_SPECIES + 1
         AGGLOMERATION_SMIX_INDEX(N_AGGLOMERATION_SPECIES) = N_DEFINE(N1)
         SPECIES_MIXTURE(N_DEFINE(N1))%AGGLOMERATION_INDEX=N_AGGLOMERATION_SPECIES
         SPEC_ID(1) = ID
         DO NNN=1,N_PARTICLE_BINS(N_AGGLOMERATION_SPECIES)
            MEAN_DIAMETER = 2._EB*PARTICLE_RADIUS(N_AGGLOMERATION_SPECIES,NNN)
            WRITE(ID,'(A,A,I0)') TRIM(SPECIES(AGGLOMERATION_SPEC_INDEX(N_AGGLOMERATION_SPECIES))%ID),'_',NNN
            MASS_FRACTION(1)=1._EB
            CALL DEFINE_MIXTURE(N_DEFINE(N1))
            N1 = N1 + 1
         ENDDO
      ELSE
         IF (TRIM(ID)=='WATER VAPOR') H2O_SMIX_INDEX = N_DEFINE(N1)
         CALL DEFINE_MIXTURE(N_DEFINE(N1))
         IF (SPECIES_MIXTURE(N_DEFINE(N1))%SINGLE_SPEC_INDEX > 0) THEN
            IF (SPECIES(SPECIES_MIXTURE(N_DEFINE(N1))%SINGLE_SPEC_INDEX)%CONDENSABLE) THEN
               SPECIES_MIXTURE(N_DEFINE(N1))%CONDENSATION_SMIX_INDEX = (N_DEFINE(N1+1))
               SPECIES_MIXTURE(N_DEFINE(N1))%DEPOSITING=.FALSE.
               SPEC_ID(1) = SPECIES_MIXTURE(N_DEFINE(N1))%ID
               MASS_FRACTION(1)=1._EB
               N1 = N1 + 1
               WRITE(ID,'(A,A)') TRIM(ID),'_COND'
               CALL DEFINE_MIXTURE(N_DEFINE(N1))
               SPECIES_MIXTURE(N_DEFINE(N1))%DEPOSITING=.TRUE.
               SPECIES_MIXTURE(N_DEFINE(N1))%EVAPORATION_SMIX_INDEX = N_DEFINE(N1-1)
               SPECIES_MIXTURE(N_DEFINE(N1))%SINGLE_SPEC_INDEX=SPECIES_MIXTURE(N_DEFINE(N1-1))%SINGLE_SPEC_INDEX
               SM%ZZ0 = MAX(0._EB,MASS_FRACTION_COND_0)
               IF (SPECIES(SPECIES_MIXTURE(N_DEFINE(N1))%SINGLE_SPEC_INDEX)%AWM_INDEX < 0) THEN
                  N_SURFACE_DENSITY_SPECIES = N_SURFACE_DENSITY_SPECIES + 1
                  SPECIES(SPECIES_MIXTURE(N_DEFINE(N1))%SINGLE_SPEC_INDEX)%AWM_INDEX = N_SURFACE_DENSITY_SPECIES
               ENDIF
            ENDIF
         ENDIF
         N1 = N1 + 1
      ENDIF
   ENDDO
ENDIF

! Setup copies of lumped species

IF (N_COPY > 0) THEN
   N1=N_TRACKED_SPECIES-N_COPY
   DO NN=1,N_COPY
      DO WHILE (KEEP_READING)
         CALL SET_SPEC_DEFAULT
         READ(LU_INPUT,NML=SPEC,IOSTAT=IOS)
         IF (COPY_LUMPED) EXIT
      ENDDO
      NNN = FINDLOC(SPECIES_MIXTURE%ID,SPEC_ID(1),1)
      SPECIES_MIXTURE(N1+NN) = SPECIES_MIXTURE(NNN)
      SPECIES_MIXTURE(N1+NN)%ID = ID
   ENDDO
ENDIF

IF (SIMPLE_CHEMISTRY) THEN
   DO NR=1,N_REACTIONS
      IF (.NOT. REACTION(NR)%SIMPLE_CHEMISTRY .OR. REACTION(NR)%PAIR_INDEX < NR) CYCLE
      IF (ANY(SPECIES_MIXTURE(N_PREDEFINED_SMIX+1:N_TRACKED_SPECIES)%ID==REACTION(NR)%FUEL)) THEN
         N1 = FINDLOC(SPECIES_MIXTURE%ID,REACTION(NR)%FUEL,1)
         REACTION(NR)%FUEL_SMIX_INDEX = N1
         SM => SPECIES_MIXTURE(N1)
         IF (ANY(SM%ATOMS(2:5)>0._EB) .OR. ANY(SM%ATOMS(9:)>0._EB)) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(163): REAC ',NR,': FORMULA limited to C,H,O,N for simple chemistry.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ELSE
            REACTION(NR)%C = SM%ATOMS(6)
            REACTION(NR)%H = SM%ATOMS(1)
            REACTION(NR)%O = SM%ATOMS(8)
            REACTION(NR)%N = SM%ATOMS(7)
            REACTION(NR)%MW_FUEL = SM%MW
         ENDIF
      ELSE
         IF (REACTION(NR)%C<=TWO_EPSILON_EB .AND. REACTION(NR)%H<=TWO_EPSILON_EB) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(164): REAC ',NR,': Specify fuel chemistry using C and/or H for simple chemistry.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF
   ENDDO
ENDIF

IF (N_PREDEFINED_SMIX > 0) THEN
   N1 = 1
   CALL SETUP_PREDEFINED_SMIX(N1,0)
   IF (SIMPLE_CHEMISTRY) THEN
      N1 = 2
      DO NR=1,N_REACTIONS
         IF (.NOT. REACTION(NR)%SIMPLE_CHEMISTRY .OR. REACTION(NR)%PAIR_INDEX < NR) CYCLE
         CALL SETUP_PREDEFINED_SMIX(N1,NR)
      ENDDO
   ENDIF
ENDIF

REWIND (LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Normalize the initial mass fractions of the lumped species if necessary
IF (SUM(SPECIES_MIXTURE(2:N_TRACKED_SPECIES)%ZZ0) > 1._EB) &
   SPECIES_MIXTURE(2:N_TRACKED_SPECIES)%ZZ0 = SPECIES_MIXTURE(2:N_TRACKED_SPECIES)%ZZ0/ &
                                          SUM(SPECIES_MIXTURE(2:N_TRACKED_SPECIES)%ZZ0)

SPECIES_MIXTURE(1)%ZZ0 = 1._EB - SUM(SPECIES_MIXTURE(2:N_TRACKED_SPECIES)%ZZ0)

DEPOSITION = ANY(SPECIES_MIXTURE%DEPOSITING) .AND. DEPOSITION

!Deallocate species inputs
!##### Figure this out
DEALLOCATE(Y_INDEX)
DEALLOCATE(N_DEFINE)

! Setup the array to convert the tracked species array to array of all primitive species

ALLOCATE(Z2Y(N_SPECIES,N_TRACKED_SPECIES),STAT=IZERO)
CALL ChkMemErr('READ','Z2Y',IZERO)
Z2Y = 0._EB

DO N1=1,N_TRACKED_SPECIES
   SM => SPECIES_MIXTURE(N1)
   DO NN=1,N_SPECIES
      Z2Y(NN,N1) = SM%MASS_FRACTION(NN)
   ENDDO
ENDDO

! Set up the arrays of molecular weights

ALLOCATE(MWR_Z(N_TRACKED_SPECIES),STAT=IZERO)
CALL ChkMemErr('READ','MW_AVG_Y',IZERO)

MWR_Z = 1._EB/SPECIES_MIXTURE%MW

ALLOCATE(ZZ_GET(N_TRACKED_SPECIES))
ZZ_GET = SPECIES_MIXTURE%ZZ0
CALL GET_SPECIFIC_GAS_CONSTANT(ZZ_GET,RSUM0)
DEALLOCATE(ZZ_GET)

MW_MIN = MINVAL(SPECIES_MIXTURE(1:N_TRACKED_SPECIES)%MW)
MW_MAX = MAXVAL(SPECIES_MIXTURE(1:N_TRACKED_SPECIES)%MW)

! Compute background density from other background quantities

RHOA = P_INF/(TMPA*RSUM0)

! Compute constant-temperature specific heats

GM1OG = (GAMMA-1._EB)/GAMMA
CP_GAMMA = SPECIES_MIXTURE(1)%RCON/GM1OG
CPOPR = CP_GAMMA/PR

! Check for user-specified turbulent Schmidt number and, if present, transfer value from SPECIES to SPECIES_MIXTURE

DO N1 = 1,N_TRACKED_SPECIES
   IF (SPECIES_MIXTURE(N1)%SINGLE_SPEC_INDEX>0) THEN
      SS=>SPECIES(SPECIES_MIXTURE(N1)%SINGLE_SPEC_INDEX)
      IF (SS%SC_T_USER>0._EB) SPECIES_MIXTURE(N1)%SC_T_USER=SS%SC_T_USER
   ENDIF
ENDDO

!If SOOT_OXIDATION is enabled make sure to save accumulation on the wall and check that it is an AEROSOL species
IF (.NOT. SOOT_OXIDATION) RETURN

IF (SIMPLE_CHEMISTRY) THEN
   WRITE(MESSAGE,'(A)') 'ERROR(165): Cannot use simple chemistry with SOOT_OXIDATION.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

IF (SOOT_INDEX < 0) THEN
   WRITE(MESSAGE,'(A)') 'ERROR(166): SOOT_OXIDATION set without SOOT as a species.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

SS => SPECIES(SOOT_INDEX)

IF (SS%MODE /= AEROSOL_SPECIES)  THEN
   WRITE(MESSAGE,'(A)') 'ERROR(167): SOOT_OXIDATION set without SOOT defined as an AEROSOL species'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

ALLOCATE (NU_SOOT_OX(1:N_TRACKED_SPECIES))
NU_SOOT_OX = 0._EB

DO N1 = 1, N_TRACKED_SPECIES
   IF (SPECIES_MIXTURE(N1)%SINGLE_SPEC_INDEX==O2_INDEX) NU_SOOT_OX(N1) = &
      -(SS%ATOMS(1) * 0.25_EB + SS%ATOMS(6) - SS%ATOMS(8)*0.5_EB) * MW_O2 / SS%MW
   IF (SPECIES_MIXTURE(N1)%SINGLE_SPEC_INDEX==H2O_INDEX) NU_SOOT_OX(N1) = SS%ATOMS(1) * 0.5_EB * MW_H2O / SS%MW
   IF (SPECIES_MIXTURE(N1)%SINGLE_SPEC_INDEX==CO2_INDEX) NU_SOOT_OX(N1) = SS%ATOMS(6) * MW_CO2 / SS%MW
END DO

CALL GET_SPEC_OR_SMIX_INDEX('SOOT',Y_S,Z_S)

N_SURFACE_DENSITY_SPECIES = N_SURFACE_DENSITY_SPECIES + 1
SPECIES(Y_S)%AWM_INDEX = N_SURFACE_DENSITY_SPECIES

IF (N_AGGLOMERATION_SPECIES > 0) THEN
   DO N1=1,N_TRACKED_SPECIES
      IF(SPECIES_MIXTURE(N1)%SINGLE_SPEC_INDEX==SOOT_INDEX) THEN
         N_SURFACE_DENSITY_SPECIES = N_SURFACE_DENSITY_SPECIES + 1
         SPECIES_MIXTURE(N1)%AWM_INDEX = N_SURFACE_DENSITY_SPECIES
      ENDIF
   ENDDO
ENDIF

CONTAINS


!> \brief Create a species mixture

SUBROUTINE DEFINE_MIXTURE(NN)
USE PROPERTY_DATA, ONLY: GET_FORMULA_WEIGHT
INTEGER, INTENT(IN) :: NN

CONVERSION = 0._EB

SM => SPECIES_MIXTURE(NN)

IF (SPEC_ID(1)=='null') THEN
   SPEC_ID(1) = ID
   VOLUME_FRACTION(1) = 1.0_EB
ELSE
   SM%K_USER                      = CONDUCTIVITY
   SM%D_USER                      = DIFFUSIVITY
   SM%EPSK                        = EPSILONKLJ
   SM%FIC_CONCENTRATION           = FIC_CONCENTRATION
   SM%FLD_LETHAL_DOSE             = FLD_LETHAL_DOSE
   SM%MU_USER                     = VISCOSITY
   SM%PR_USER                     = PR_GAS
   SM%SC_T_USER                   = TURBULENT_SCHMIDT_NUMBER
   SM%RAMP_CP                     = RAMP_CP
   SM%RAMP_D                      = RAMP_D
   SM%RAMP_G_F                    = RAMP_G_F
   SM%RAMP_K                      = RAMP_K
   SM%RAMP_MU                     = RAMP_MU
   IF (REFERENCE_TEMPERATURE < -TMPM) REFERENCE_TEMPERATURE = 25._EB
   SM%REFERENCE_TEMPERATURE       = REFERENCE_TEMPERATURE + TMPM
   SM%SIG                         = SIGMALJ
   SM%SPECIFIC_HEAT               = SPECIFIC_HEAT*1000._EB
   SM%REFERENCE_ENTHALPY          = REFERENCE_ENTHALPY*1000._EB
   SM%H_F                         = ENTHALPY_OF_FORMATION*1000._EB

   IF (SM%H_F > -1.E21 .OR. SM%REFERENCE_ENTHALPY > -1.E21) SM%EXPLICIT_H_F = .TRUE.

   IF ((SM%RAMP_CP/='null' .OR. SM%SPECIFIC_HEAT > 0._EB) .AND. &
       (SM%REFERENCE_ENTHALPY < -1.E20_EB .AND. SM%H_F < -1.E20_EB)) SM%REFERENCE_ENTHALPY = 0._EB

   ! Get ramps
   IF (SM%RAMP_CP/='null') THEN
      CALL GET_RAMP_INDEX(SM%RAMP_CP,'TEMPERATURE',NR)
      SM%RAMP_CP_INDEX = NR
   ENDIF
   IF (SM%RAMP_D/='null') THEN
      CALL GET_RAMP_INDEX(SM%RAMP_D,'TEMPERATURE',NR)
      SM%RAMP_D_INDEX = NR
   ENDIF
   IF (SM%RAMP_G_F/='null') THEN
      CALL GET_RAMP_INDEX(SM%RAMP_D,'TEMPERATURE',NR)
      SM%RAMP_G_F_INDEX = NR
   ENDIF
   IF (SM%RAMP_K/='null') THEN
      CALL GET_RAMP_INDEX(SM%RAMP_K,'TEMPERATURE',NR)
      SM%RAMP_K_INDEX = NR
   ENDIF
   IF (SM%RAMP_MU/='null') THEN
      CALL GET_RAMP_INDEX(SM%RAMP_MU,'TEMPERATURE',NR)
      SM%RAMP_MU_INDEX = NR
   ENDIF
ENDIF

SM%ID = ID
SM%ZZ0 = MAX(0._EB,MASS_FRACTION_0)

! Count the number of species included in the mixture

N_SUB_SPECIES = 0
COUNT_SPEC: DO NS=1,N_SPECIES
   IF (TRIM(SPEC_ID(NS)) /= 'null') THEN
      N_SUB_SPECIES = N_SUB_SPECIES + 1
   ELSE
      EXIT
   ENDIF
ENDDO COUNT_SPEC

IF (N_SUB_SPECIES == 1) THEN
   MASS_FRACTION=0._EB
   MASS_FRACTION(1)=1._EB
   VOLUME_FRACTION=0._EB
ENDIF

! Allocate arrays to store the species id, mass, volume fractions

ALLOCATE (SM%SPEC_ID(N_SPECIES),STAT=IZERO)
ALLOCATE (SM%VOLUME_FRACTION(N_SPECIES),STAT=IZERO)
ALLOCATE (SM%MASS_FRACTION(N_SPECIES),STAT=IZERO)

SM%SPEC_ID         = 'null'
SM%VOLUME_FRACTION = 0._EB
SM%MASS_FRACTION   = 0._EB

Y_INDEX = -1

DO NS = 1,N_SUB_SPECIES
   FIND_SPEC_ID: DO NS2 = 1,N_SPECIES
      IF (TRIM(SPECIES(NS2)%ID) == TRIM(SPEC_ID(NS))) THEN
         SM%SPEC_ID(NS2) = SPECIES(NS2)%ID
         Y_INDEX(NS)  = NS2
         IF (N_SUB_SPECIES==1) THEN
            SM%FORMULA = SPECIES(NS2)%FORMULA
            SM%SINGLE_SPEC_INDEX=NS2
         ENDIF
         IF (SPECIES(NS2)%MODE == AEROSOL_SPECIES) THEN
            IF (N_SUB_SPECIES == 1) THEN
               SM%DEPOSITING = .TRUE.
               SM%MEAN_DIAMETER = MEAN_DIAMETER
               IF (THERMOPHORETIC_DIAMETER > 0._EB) THEN
                  SM%THERMOPHORETIC_DIAMETER = THERMOPHORETIC_DIAMETER
               ELSE
                  SM%THERMOPHORETIC_DIAMETER = MEAN_DIAMETER
               ENDIF
               IF (ABS(DENSITY_SOLID-SOOT_DENSITY) <=TWO_EPSILON_EB .AND. &
                     ABS(DENSITY_SOLID-SPECIES(NS2)%DENSITY_SOLID) <=TWO_EPSILON_EB) THEN
                     SM%DENSITY_SOLID = DENSITY_SOLID
               ELSE
                     SM%DENSITY_SOLID = SPECIES(NS2)%DENSITY_SOLID
               ENDIF
               SM%CONDUCTIVITY_SOLID=SPECIES(NS2)%CONDUCTIVITY_SOLID
            ELSE
               WRITE(MESSAGE,'(A,A,A)') 'WARNING: Cannot do deposition with a lumped species.  Species ',TRIM(SM%ID),&
                                        ' will not have deposition'
               IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
            ENDIF
         ENDIF
         EXIT FIND_SPEC_ID
      ENDIF
   ENDDO FIND_SPEC_ID

   IF (Y_INDEX(NS)<0) THEN
      WRITE(MESSAGE,'(A,A,A,I0,A)') 'ERROR(168): SPEC ' ,TRIM(SM%ID),': Sub-species ',NS,' not found.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (MASS_FRACTION(NS)>0._EB)     CONVERSION = CONVERSION + MASS_FRACTION(NS)   / SPECIES(Y_INDEX(NS))%MW
   IF (VOLUME_FRACTION(NS)>0._EB)   CONVERSION = CONVERSION + VOLUME_FRACTION(NS) * SPECIES(Y_INDEX(NS))%MW
   IF (NN > N_PREDEFINED_SMIX .AND. MASS_FRACTION(NS)<=0._EB .AND. VOLUME_FRACTION(NS)<=0._EB) THEN
      WRITE(MESSAGE,'(A,A,A,I0,A)') 'ERROR(169): SPEC ' ,TRIM(SM%ID),': Mass or volume fraction for sub species ',NS,' not found.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

ENDDO

IF (ANY(MASS_FRACTION>0._EB)) THEN
   DO NS = 1,N_SUB_SPECIES
      IF (SM%MASS_FRACTION(Y_INDEX(NS)) > 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(170): SPEC ' ,TRIM(SM%ID),', cannot have duplicate species in SPEC_ID.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      SM%VOLUME_FRACTION(Y_INDEX(NS)) = MASS_FRACTION(NS) / SPECIES(Y_INDEX(NS))%MW / CONVERSION
      SM%MASS_FRACTION(Y_INDEX(NS))   = MASS_FRACTION(NS)
   ENDDO
ENDIF

IF (ANY(VOLUME_FRACTION>0._EB)) THEN
   DO NS = 1,N_SUB_SPECIES
      IF (SM%VOLUME_FRACTION(Y_INDEX(NS)) > 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(170): SPEC ' ,TRIM(SM%ID),', cannot have duplicate species in SPEC_ID.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      SM%MASS_FRACTION(Y_INDEX(NS))   = VOLUME_FRACTION(NS) * SPECIES(Y_INDEX(NS))%MW / CONVERSION
      SM%VOLUME_FRACTION(Y_INDEX(NS)) = VOLUME_FRACTION(NS)
   ENDDO
ENDIF

! Normalize mass and volume fractions, plus stoichiometric coefficient

SM%MASS_FRACTION = SM%MASS_FRACTION / SUM(SM%MASS_FRACTION)
SM%ADJUST_NU = SUM(SM%VOLUME_FRACTION)
SM%VOLUME_FRACTION = SM%VOLUME_FRACTION / SUM(SM%VOLUME_FRACTION)

! Calculate the molecular weight and extinction coefficient

SM%MW = 0._EB
SM%MASS_EXTINCTION_COEFFICIENT = 0._EB

DO NS = 1,N_SPECIES
   IF (SM%MASS_FRACTION(NS) <TWO_EPSILON_EB) CYCLE
   IF (MASS_EXTINCTION_COEFFICIENT > 0._EB) THEN
      SM%MASS_EXTINCTION_COEFFICIENT = MASS_EXTINCTION_COEFFICIENT
   ELSE
      SM%MASS_EXTINCTION_COEFFICIENT = SM%MASS_EXTINCTION_COEFFICIENT+SM%MASS_FRACTION(NS)*SPECIES(NS)%MASS_EXTINCTION_COEFFICIENT
   ENDIF
   IF (MW > 0._EB) THEN
      SM%MW = MW
   ELSE
      SM%MW = SM%MW + SM%VOLUME_FRACTION(NS) * SPECIES(NS)%MW
   ENDIF
   IF (SPECIES(NS)%FORMULA(1:5)=='SPEC_') SM%VALID_ATOMS = .FALSE.
   IF (NN > N_PREDEFINED_SMIX .AND. FORMULA /= 'null') THEN
      CALL GET_FORMULA_WEIGHT(FORMULA,SM%MW,SM%ATOMS)
   ELSE
      SM%ATOMS = SM%ATOMS + SM%VOLUME_FRACTION(NS)*SPECIES(NS)%ATOMS
   ENDIF
ENDDO

SM%RCON = R0/SM%MW

END SUBROUTINE DEFINE_MIXTURE


!> \brief Set default SPECies parameters

SUBROUTINE SET_SPEC_DEFAULT

AEROSOL                     = .FALSE.
BACKGROUND                  = .FALSE.
BETA_LIQUID                 = -1._EB
CONDUCTIVITY                = -1._EB
CONDUCTIVITY_LIQUID         = -1._EB
CONDUCTIVITY_SOLID          = 0.26_EB !W/m/K Ben-Dor, et al. 2002. (~10 x air)
COPY_LUMPED                 = .FALSE.
DENSITY_SOLID               = SOOT_DENSITY
DIFFUSIVITY                 = -1._EB
EPSILONKLJ                  =  0._EB
FIC_CONCENTRATION           =  0._EB
FLD_LETHAL_DOSE             =  0._EB
FORMULA                     = 'null'
FYI                         = 'null'
ENTHALPY_OF_FORMATION       = -1.E30_EB  ! J/mol
ID                          = 'null'
LUMPED_COMPONENT_ONLY       = .FALSE.
RADCAL_ID                   = 'null'
MASS_EXTINCTION_COEFFICIENT = -1._EB  ! m2/kg
MASS_FRACTION               =  0._EB
MASS_FRACTION_0             = -1._EB
MASS_FRACTION_COND_0        = 0._EB
MEAN_DIAMETER               =  -1._EB
THERMOPHORETIC_DIAMETER     =  0.03E-6_EB
MW                          =  0._EB
PR_GAS                      = -1._EB
PRIMITIVE                   = .FALSE.
REFERENCE_TEMPERATURE       = -300._EB ! C
SIGMALJ                     =  0._EB
SPEC_ID                     = 'null'
SPECIFIC_HEAT               = -1._EB
REFERENCE_ENTHALPY          = -1.E30_EB
TURBULENT_SCHMIDT_NUMBER    = -1._EB
VISCOSITY                   = -1._EB
VISCOSITY_LIQUID            = -1._EB
VOLUME_FRACTION             =  0._EB

DENSITY_LIQUID              = -1._EB
HEAT_OF_VAPORIZATION        = -1._EB     ! kJ/kg
H_V_REFERENCE_TEMPERATURE   = -300._EB
MELTING_TEMPERATURE         = -300._EB   ! C
SPECIFIC_HEAT_LIQUID        = -1._EB     ! kJ/kg-K
VAPORIZATION_TEMPERATURE    = -300._EB   ! C

RAMP_CP                     = 'null'
RAMP_CP_L                   = 'null'
RAMP_D                      = 'null'
RAMP_G_F                    = 'null'
RAMP_K                      = 'null'
RAMP_MU                     = 'null'

N_BINS                      = -1
MIN_DIAMETER                = -1._EB !m
MAX_DIAMETER                = -1._EB !m

C                           = -1._EB
H                           = -1._EB
O                           = -1._EB
N                           = -1._EB

END SUBROUTINE SET_SPEC_DEFAULT


!> \brief Set up the SMIX line either for the SIMPLE_CHEMISTRY mode or for a primitive species
!> \param N SMIX index
!> \param NR REACTION index
SUBROUTINE SETUP_PREDEFINED_SMIX(N,NR)

INTEGER, INTENT(INOUT) :: N
INTEGER, INTENT(IN) :: NR
INTEGER :: SUFFIX,NN
REAL(EB) :: FUEL_MW
TYPE(REACTION_TYPE), POINTER :: RN,RN2

MASS_FRACTION = 0._EB

BACKGROUND_IF: IF (N==1) THEN ! Mixture is AIR
   CALL SET_SPEC_DEFAULT
   ID               = 'AIR'
   FORMULA          = 'Z0'
   SPEC_ID(1)       = 'WATER VAPOR'
   SPEC_ID(2)       = 'OXYGEN'
   SPEC_ID(3)       = 'CARBON DIOXIDE'
   SPEC_ID(4)       = 'NITROGEN'
   MASS_FRACTION(1) = Y_H2O_INFTY
   MASS_FRACTION(2) = Y_O2_INFTY*(1._EB-Y_H2O_INFTY)
   MASS_FRACTION(3) = Y_CO2_INFTY*(1._EB-Y_H2O_INFTY)
   MASS_FRACTION(4) = 1._EB-SUM(MASS_FRACTION)
   CALL DEFINE_MIXTURE(N)
ELSE BACKGROUND_IF ! Mixture is fuel or products
   RN=>REACTION(NR)
   RN%AIR_SMIX_INDEX = 1
   N_SIMPLE_CHEM_RXN: SELECT CASE (RN%N_SIMPLE_CHEMISTRY_REACTIONS)
      CASE (1) N_SIMPLE_CHEM_RXN
         ! Setup fuel if not defined by a SPEC
         IF (ALL(SPEC_ID_READ/=RN%FUEL)) THEN
            CALL SET_SPEC_DEFAULT
            ID               = RN%FUEL
            FORMULA          = 'Z1'
            SPEC_ID(1)       = RN%FUEL
            MASS_FRACTION(1) = 1._EB
            CALL DEFINE_MIXTURE(N)
            RN%FUEL_SMIX_INDEX=N
            FUEL_MW = SPECIES_MIXTURE(N)%MW
            CALL DEFINE_MIXTURE(N)
            N = N + 1
         ELSE
            NN = FINDLOC(SPECIES_MIXTURE%ID,RN%FUEL,1)
            FUEL_MW = SPECIES_MIXTURE(NN)%MW
            RN%FUEL_SMIX_INDEX=NN
         ENDIF
         CALL SET_SPEC_DEFAULT
         ID                 = RN%SPEC_ID_NU_READ(3)
         SUFFIX = LEN(TRIM(RN%SPEC_ID_NU_READ(3)))
         IF (SUFFIX > 8) THEN
            SUFFIX = SUFFIX - 8
            FORMULA         = 'Z2'//RN%SPEC_ID_NU_READ(3)(8:7+SUFFIX)
         ELSE
            FORMULA         = 'Z2'
         ENDIF
         SPEC_ID(1)         = 'CARBON MONOXIDE'
         SPEC_ID(2)         = 'SOOT'
         SPEC_ID(3)         = 'WATER VAPOR'
         SPEC_ID(4)         = 'CARBON DIOXIDE'
         SPEC_ID(5)         = 'HYDROGEN CYANIDE'
         SPEC_ID(6)         = 'NITROGEN'
         RN%NU_CO           = (FUEL_MW/MW_CO)   * RN%CO_YIELD
         RN%NU_HCN          = (FUEL_MW/MW_HCN)  * RN%HCN_YIELD
         RN%NU_SOOT         = (FUEL_MW/MW_SOOT) * RN%SOOT_YIELD
         RN%NU_H2O          = 0.5_EB*RN%H - 0.5_EB*(RN%NU_SOOT*SOOT_H_FRACTION + RN%NU_HCN)
         IF (ABS(RN%NU_H2O) < TWO_EPSILON_EB) RN%NU_H2O = 0._EB
         RN%NU_CO2          = RN%C - RN%NU_CO - RN%NU_HCN - RN%NU_SOOT*SOOT_C_FRACTION
         IF (ABS(RN%NU_CO2) < TWO_EPSILON_EB) RN%NU_CO2 = 0._EB
         RN%NU_O2           = RN%NU_CO2 + 0.5_EB*(RN%NU_CO + RN%NU_H2O - RN%O + RN%NU_SOOT*SOOT_O_FRACTION)
         RN%NU_N2           = 0.5_EB*(RN%N - RN%NU_HCN - RN%NU_SOOT*SOOT_N_FRACTION)
         IF (RN%NU_CO2 <0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(171): REAC, Not enough carbon for the CO_YIELD, SOOT_YIELD, and/or HCN_YIELD.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (RN%NU_H2O <0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(172): REAC, Not enough hydrogen for the SOOT_YIELD and/or HCN_YIELD.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (RN%NU_N2 <0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(173): REAC, Not enough nitrogen for the HCN_YIELD.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

         VOLUME_FRACTION(1) = RN%NU_CO
         VOLUME_FRACTION(2) = RN%NU_SOOT
         VOLUME_FRACTION(3) = RN%NU_H2O + SPECIES_MIXTURE(1)%VOLUME_FRACTION(H2O_INDEX)*RN%NU_O2 / &
                                          SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(4) = RN%NU_CO2 + SPECIES_MIXTURE(1)%VOLUME_FRACTION(CO2_INDEX)*RN%NU_O2 / &
                                          SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(5) = RN%NU_HCN
         VOLUME_FRACTION(6) = RN%NU_N2  + SPECIES_MIXTURE(1)%VOLUME_FRACTION(N2_INDEX)*RN%NU_O2 / &
                                          SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION    = VOLUME_FRACTION/SUM(VOLUME_FRACTION)
         CALL DEFINE_MIXTURE(N)
         RN%PROD_SMIX_INDEX = N
         N = N + 1
      CASE (2) N_SIMPLE_CHEM_RXN
         ! Setup fuel if not defined by a SPEC
         IF (ALL(SPEC_ID_READ/=RN%FUEL)) THEN
            CALL SET_SPEC_DEFAULT
            ID               = RN%FUEL
            FORMULA          = 'Z1'
            SPEC_ID(1)       = RN%FUEL
            MASS_FRACTION(1) = 1._EB
            CALL DEFINE_MIXTURE(N)
            RN%FUEL_SMIX_INDEX=N
            FUEL_MW = SPECIES_MIXTURE(N)%MW
            CALL DEFINE_MIXTURE(N)
            N = N + 1
         ELSE
            NN = FINDLOC(SPECIES_MIXTURE%ID,RN%FUEL,1)
            FUEL_MW = SPECIES_MIXTURE(NN)%MW
            RN%FUEL_SMIX_INDEX=NN
         ENDIF

         ! Setup intermediate producs
         CALL SET_SPEC_DEFAULT
         ID                 = RN%SPEC_ID_NU_READ(3)
         SUFFIX = LEN(TRIM(RN%SPEC_ID_NU_READ(3)))
         IF (SUFFIX > 21) THEN
            SUFFIX = SUFFIX - 21
            FORMULA         = 'Z3'//RN%SPEC_ID_NU_READ(3)(21:20+SUFFIX)
         ELSE
            FORMULA         = 'Z3'
         ENDIF
         FORMULA            = 'Z3'
         SPEC_ID(1)         = 'CARBON MONOXIDE'
         SPEC_ID(2)         = 'SOOT'
         SPEC_ID(3)         = 'WATER VAPOR'
         SPEC_ID(4)         = 'CARBON DIOXIDE'
         SPEC_ID(5)         = 'HYDROGEN CYANIDE'
         SPEC_ID(6)         = 'NITROGEN'
         SPEC_ID(7)         = 'HYDROGEN'
         RN%NU_HCN          = RN%N*RN%FUEL_N_TO_HCN_FRACTION
         RN%NU_CO           = RN%C*RN%FUEL_C_TO_CO_FRACTION
         IF (ABS(RN%NU_CO)<TWO_EPSILON_EB) RN%NU_CO = 0._EB
         RN%NU_SOOT         = (RN%C-RN%NU_CO-RN%NU_HCN)/SOOT_C_FRACTION
         IF (ABS(RN%NU_SOOT)<TWO_EPSILON_EB) RN%NU_SOOT = 0._EB
         RN%NU_H2           = RN%H*RN%FUEL_H_TO_H2_FRACTION*0.5_EB
         IF (ABS(RN%NU_H2)<TWO_EPSILON_EB) RN%NU_H2 = 0._EB
         RN%NU_H2O          = 0.5_EB*(RN%H - 2._EB*RN%NU_H2 - RN%NU_HCN - RN%NU_SOOT*SOOT_H_FRACTION)
         IF (ABS(RN%NU_H2O)<TWO_EPSILON_EB) RN%NU_H2O = 0._EB
         RN%NU_CO2          = RN%C - RN%NU_CO - RN%NU_SOOT*SOOT_C_FRACTION - RN%NU_HCN
         IF (ABS(RN%NU_CO2)<TWO_EPSILON_EB) RN%NU_CO2 = 0._EB
         RN%NU_O2           = RN%NU_CO2 + 0.5_EB*(RN%NU_CO + RN%NU_H2O - RN%O + RN%NU_SOOT*SOOT_O_FRACTION)
         RN%NU_N2           = (RN%N - RN%NU_HCN - RN%NU_SOOT*SOOT_N_FRACTION)*0.5_EB
         IF (ABS(RN%NU_N2)<TWO_EPSILON_EB) RN%NU_N2 = 0._EB
         IF (RN%NU_N2 <0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(174): REAC, Not enough nitrogen for the FUEL_N_TO_HCN_FRACTION.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (RN%NU_CO2 <0._EB) THEN
            WRITE(MESSAGE,'(A)') &
               'ERROR(175): REAC, Not enough carbon for FUEL_C_TO_CO_FRACTION and/or FUEL_N_TO_HCN_FRACTION.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (RN%NU_H2O <0._EB) THEN
            WRITE(MESSAGE,'(A)') &
               'ERROR(176): REAC, Not enough hydrogen for the FUEL_H_TO_H2_FRACTION and/or FUEL_N_TO_HCN_FRACTION'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         VOLUME_FRACTION(1) = RN%NU_CO
         VOLUME_FRACTION(2) = RN%NU_SOOT
         VOLUME_FRACTION(3) = RN%NU_H2O + SPECIES_MIXTURE(1)%VOLUME_FRACTION(H2O_INDEX)*RN%NU_O2 / &
                                          SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(4) = RN%NU_CO2 + SPECIES_MIXTURE(1)%VOLUME_FRACTION(CO2_INDEX)*RN%NU_O2 / &
                                          SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(5) = RN%NU_HCN
         VOLUME_FRACTION(6) = RN%NU_N2  + SPECIES_MIXTURE(1)%VOLUME_FRACTION(N2_INDEX)*RN%NU_O2 / &
                                          SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(7) = RN%NU_H2
         VOLUME_FRACTION    = VOLUME_FRACTION/SUM(VOLUME_FRACTION)
         CALL DEFINE_MIXTURE(N)
         RN%PROD_SMIX_INDEX = N

         ! Setup final producs
         RN2=>REACTION(REACTION(NR)%PAIR_INDEX)
         RN2%FUEL_SMIX_INDEX = N
         N = N + 1
         CALL SET_SPEC_DEFAULT
         ID                 = RN2%SPEC_ID_NU_READ(3)
         SUFFIX = LEN(TRIM(RN%SPEC_ID_NU_READ(3)))
         IF (SUFFIX > 8) THEN
            SUFFIX = SUFFIX - 8
            FORMULA         = 'Z2'//RN2%SPEC_ID_NU_READ(3)(8:7+SUFFIX)
         ELSE
            FORMULA         = 'Z2'
         ENDIF
         SPEC_ID(1)         = 'CARBON MONOXIDE'
         SPEC_ID(2)         = 'SOOT'
         SPEC_ID(3)         = 'WATER VAPOR'
         SPEC_ID(4)         = 'CARBON DIOXIDE'
         SPEC_ID(5)         = 'HYDROGEN CYANIDE'
         SPEC_ID(6)         = 'NITROGEN'
         RN2%NU_CO           = (FUEL_MW/MW_CO)   * RN%CO_YIELD
         RN2%NU_HCN          = (FUEL_MW/MW_HCN)  * RN%HCN_YIELD
         RN2%NU_SOOT         = (FUEL_MW/MW_SOOT) * RN%SOOT_YIELD
         RN2%NU_H2O          = 0.5_EB*RN%H - 0.5_EB*(RN2%NU_SOOT*SOOT_H_FRACTION + RN2%NU_HCN)
         IF (ABS(RN2%NU_H2O) < TWO_EPSILON_EB) RN%NU_H2O = 0._EB
         RN2%NU_CO2          = RN%C - RN2%NU_CO - RN2%NU_HCN - RN2%NU_SOOT*SOOT_C_FRACTION
         IF (ABS(RN2%NU_CO2) < TWO_EPSILON_EB) RN2%NU_CO2 = 0._EB
         RN2%NU_O2           = RN2%NU_CO2 + 0.5_EB*(RN2%NU_CO + RN2%NU_H2O - RN%O + RN2%NU_SOOT*SOOT_O_FRACTION)
         RN2%NU_N2           = 0.5_EB*(RN%N - RN2%NU_HCN - RN2%NU_SOOT*SOOT_N_FRACTION)
         IF (RN2%NU_CO2 <0._EB) THEN
            WRITE(MESSAGE,'(A)') &
               'ERROR(171): REAC, Not enough carbon for the CO_YIELD, SOOT_YIELD, and/or HCN_YIELD.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (RN2%NU_H2O <0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(172): REAC, Not enough hydrogen for the SOOT_YIELD and/or HCN_YIELD.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (RN2%NU_N2 <0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(173): REAC, Not enough nitrogen in the fuel for the specified HCN_YIELD.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

         VOLUME_FRACTION(1) = RN2%NU_CO
         VOLUME_FRACTION(2) = RN2%NU_SOOT
         VOLUME_FRACTION(3) = RN2%NU_H2O + SPECIES_MIXTURE(1)%VOLUME_FRACTION(H2O_INDEX)*RN2%NU_O2 / &
                                           SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(4) = RN2%NU_CO2 + SPECIES_MIXTURE(1)%VOLUME_FRACTION(CO2_INDEX)*RN2%NU_O2 / &
                                           SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION(5) = RN2%NU_HCN
         VOLUME_FRACTION(6) = RN2%NU_N2  + SPECIES_MIXTURE(1)%VOLUME_FRACTION(N2_INDEX)*RN2%NU_O2 / &
                                           SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         VOLUME_FRACTION    = VOLUME_FRACTION/SUM(VOLUME_FRACTION)
         CALL DEFINE_MIXTURE(N)
         RN2%PROD_SMIX_INDEX = N
         N = N + 1
   END SELECT N_SIMPLE_CHEM_RXN
ENDIF BACKGROUND_IF

END SUBROUTINE SETUP_PREDEFINED_SMIX

LOGICAL FUNCTION CHECK_CONDENSABLE(SS_TMP_V,SS2_ID)
USE PROPERTY_DATA, ONLY: THERMO_TABLE_LIQUID,GET_PROP_INDEX
REAL(EB), INTENT(IN):: SS_TMP_V
CHARACTER(LABEL_LENGTH), INTENT(IN) :: SS2_ID
REAL(EB):: C_P_L,H_V,TMP_REF,TMP_MELT,TMP_V,DENSITY,MU_LIQUID,K_LIQUID,BETA_LIQUID
LOGICAL:: FUEL2,EXPLICIT_G_F
INTEGER:: SS2_INDEX

CALL GET_PROP_INDEX(SS2_ID,SS2_INDEX,EXPLICIT_G_F)

CALL THERMO_TABLE_LIQUID (1,C_P_L,H_V,TMP_REF,TMP_MELT,TMP_V,SS2_INDEX,FUEL2,DENSITY,MU_LIQUID,K_LIQUID,BETA_LIQUID)

CHECK_CONDENSABLE = .FALSE.
IF (SS_TMP_V > 0._EB .OR. TMP_V > 0._EB) CHECK_CONDENSABLE = .TRUE.

END FUNCTION CHECK_CONDENSABLE

END SUBROUTINE READ_SPEC


!> \brief Setup arrays of primitive species properties to be used in PROC_SMIX

SUBROUTINE PROC_SPEC_1
USE MATH_FUNCTIONS, ONLY: EVALUATE_RAMP
USE PROPERTY_DATA, ONLY: CALC_GAS_PROPS,THERMO_TABLE_LIQUID
USE THERMO_PROPS, ONLY: THERMO_DATA
INTEGER :: N,ITMP,IZERO,IT
REAL(EB) :: H_REF,H_REF_2,H_CORR,DS,S
REAL(EB) :: CP_TEMP,H_V,TMP_REF,TMP_MELT,TMP_V,DENSITY,MU_LIQUID,K_LIQUID,BETA_LIQUID
TYPE(SPECIES_TYPE), POINTER :: SS=>NULL()

ALLOCATE(SS_CP(N_SPECIES,0:I_MAX_TEMP),STAT=IZERO)
SS_CP = 0._EB
ALLOCATE(SS_D(N_SPECIES,0:I_MAX_TEMP),STAT=IZERO)
SS_D = 0._EB
ALLOCATE(SS_G_F(N_SPECIES,0:I_MAX_TEMP),STAT=IZERO)
SS_G_F = 0._EB
DO N=1,N_SPECIES
   ALLOCATE(SPECIES(N)%H_G(0:I_MAX_TEMP),STAT=IZERO)
   SPECIES(N)%H_G = 0._EB
   ALLOCATE(SPECIES(N)%G_F(0:I_MAX_TEMP),STAT=IZERO)
   SPECIES(N)%G_F = 0._EB
ENDDO
ALLOCATE(SS_K(N_SPECIES,0:I_MAX_TEMP),STAT=IZERO)
SS_K = 0._EB
ALLOCATE(SS_MU(N_SPECIES,0:I_MAX_TEMP),STAT=IZERO)
SS_MU = 0._EB

SPEC_LOOP: DO N=1,N_SPECIES

   SS => SPECIES(N)
   SS%H_F = SS%H_F / SS%MW * 1000._EB ! Convert to J/kg
   SS%H_F_LISTED = SS%H_F_LISTED / SS%MW * 1000._EB ! Convert to J/kg
   SPECIES(N)%H_G(0) = 0._EB

   DO I = 1, I_MAX_TEMP
      CALL CALC_GAS_PROPS(I,N,SS_D(N,I),SS_MU(N,I),SS_K(N,I),SS_CP(N,I),SS_G_F(N,I))
      IF (SS%RAMP_CP_INDEX > 0)     SS_CP(N,I)  = EVALUATE_RAMP(REAL(I,EB),SS%RAMP_CP_INDEX,TAU=1._EB)*1000._EB
      IF (SS%RAMP_D_INDEX > 0)      SS_D(N,I)   = EVALUATE_RAMP(REAL(I,EB),SS%RAMP_D_INDEX,TAU=1._EB)
      IF (SS%RAMP_G_F_INDEX > 0)    SS_G_F(N,I) = EVALUATE_RAMP(REAL(I,EB),SS%RAMP_G_F_INDEX,TAU=1._EB)
      IF (SS%RAMP_K_INDEX>0)        SS_K(N,I)   = EVALUATE_RAMP(REAL(I,EB),SS%RAMP_K_INDEX,TAU=1._EB)/SQRT(SS%MW)
      IF (SS%RAMP_MU_INDEX > 0)     SS_MU(N,I)  = EVALUATE_RAMP(REAL(I,EB),SS%RAMP_MU_INDEX,TAU=1._EB)/SQRT(SS%MW)

      IF (I==1) THEN
         SS_CP(N,0) = SS_CP(N,1)
         SS_D(N,0) = SS_D(N,1)
         SS_G_F(N,0) = SS_G_F(N,1)
         SS_K(N,0) = SS_K(N,1)
         SS_MU(N,0) = SS_MU(N,1)
      ENDIF
      SPECIES(N)%H_G(I) = SPECIES(N)%H_G(I-1) + 0.5*(SS_CP(N,I-1)+SS_CP(N,I))
      IF (SPECIES(N)%EXPLICIT_G_F) SPECIES(N)%G_F(I) = SS_G_F(N,I)
   ENDDO

   H_CORR = 0._EB
   IF (SS%REFERENCE_ENTHALPY > -1.E21_EB) THEN
      ITMP = INT(SS%REFERENCE_TEMPERATURE)
      H_REF = SPECIES(N)%H_G(ITMP) + (SS%REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * &
              (SPECIES(N)%H_G(ITMP+1)-SPECIES(N)%H_G(ITMP))
      SPECIES(N)%H_G(:) = SPECIES(N)%H_G(:) + SS%REFERENCE_ENTHALPY - H_REF
      ITMP = INT(H_F_REFERENCE_TEMPERATURE)
      SS%H_F = SPECIES(N)%H_G(ITMP) + (H_F_REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * &
               (SPECIES(N)%H_G(ITMP+1)-SPECIES(N)%H_G(ITMP))
   ELSE
      IF (SS%H_F > -1.E21_EB .OR. SS%H_F_LISTED > -1.E21_EB) THEN
      ITMP = INT(H_F_REFERENCE_TEMPERATURE)
      H_REF = SPECIES(N)%H_G(ITMP) + (H_F_REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * &
              (SPECIES(N)%H_G(ITMP+1)-SPECIES(N)%H_G(ITMP))
         IF (SS%H_F > -1.E21_EB) THEN
            SPECIES(N)%H_G(:) = SPECIES(N)%H_G(:) + SS%H_F - H_REF
         ELSE
            IF (.NOT. SS%SPECIFIC_HEAT > 0._EB .AND. .NOT. SS%RAMP_CP_INDEX > 0 .AND. .NOT. CONSTANT_SPECIFIC_HEAT_RATIO) THEN
               SS%H_F = SS%H_F_LISTED
               SPECIES(N)%H_G(:) = SPECIES(N)%H_G(:) + SS%H_F_LISTED - H_REF
            ELSE
               ITMP = INT(H_F_REFERENCE_TEMPERATURE)
               SS%H_F = SPECIES(N)%H_G(ITMP) + (H_F_REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * &
                        (SPECIES(N)%H_G(ITMP+1)-SPECIES(N)%H_G(ITMP))
            ENDIF
         ENDIF
      ELSE
         ITMP = INT(H_F_REFERENCE_TEMPERATURE)
         SS%H_F = SPECIES(N)%H_G(ITMP) + (H_F_REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * &
                  (SPECIES(N)%H_G(ITMP+1)-SPECIES(N)%H_G(ITMP))
      ENDIF
   ENDIF

! If G_F is defined in THERMO_DATA adjust G_F for temperature values outside the polynomial range
   IF (SPECIES(N)%PROP_INDEX > 0) THEN
      IT = INT(THERMO_DATA(SPECIES(N)%PROP_INDEX)%T(1))
      IF (IT > 0) THEN
        S = (SS%G_F(IT)-SS%H_G(IT))/REAL(IT,EB)
        DS = (SS%G_F(IT+1)-SS%G_F(IT+1))/REAL(IT+1,EB)-S
        DO I=0,IT-1
           SS%G_F(I) = SS%H_G(I) - REAL(I,EB)*(S - REAL(IT-I,EB)*DS)
        ENDDO
      ENDIF
      IT = INT(MAXVAL(THERMO_DATA(SPECIES(N)%PROP_INDEX)%T))
      IF (IT < I_MAX_TEMP) THEN
        S = (SS%G_F(IT)-SS%H_G(IT))/REAL(IT,EB)
        DS = S-(SS%G_F(IT)-SS%G_F(IT-1))/REAL(IT-1,EB)
        DO I=IT+1,I_MAX_TEMP
           SS%G_F(I) = SS%H_G(I) - REAL(I,EB)*(S + REAL(I-IT,EB)*DS)
        ENDDO
      ENDIF
      SS_G_F(N,:) = SS%G_F(:)
   ENDIF

   ! IF there are liquid properties, set up liquid values.

   CALL THERMO_TABLE_LIQUID (1,CP_TEMP,H_V,TMP_REF,TMP_MELT,TMP_V,SS%PROP_INDEX,SS%ISFUEL,DENSITY,MU_LIQUID,K_LIQUID,BETA_LIQUID)

   LIQUID_IF: IF (CP_TEMP > 0._EB .OR. SS%TMP_MELT >= 0._EB .OR. SS%TMP_V > 0._EB .OR. SS%DENSITY_LIQUID > 0._EB .OR. &
      SS%SPECIFIC_HEAT_LIQUID > 0._EB .OR. SS%RAMP_CP_L_INDEX > 0) THEN
      IF (SS%HEAT_OF_VAPORIZATION < 0._EB) THEN
         SS%HEAT_OF_VAPORIZATION = H_V
         SS%H_V_REFERENCE_TEMPERATURE = TMP_REF
      ENDIF
      IF (SS%TMP_MELT < 0._EB) SS%TMP_MELT = TMP_MELT
      IF (SS%TMP_V < 0._EB) SS%TMP_V = TMP_V
      IF (SS%DENSITY_LIQUID < 0._EB) SS%DENSITY_LIQUID = DENSITY
      IF (SS%MU_LIQUID < 0._EB) SS%MU_LIQUID = MU_LIQUID
      IF (SS%K_LIQUID < 0._EB) SS%K_LIQUID = K_LIQUID
      IF (SS%BETA_LIQUID < 0._EB) SS%BETA_LIQUID = BETA_LIQUID

      IF (CP_TEMP < 0._EB .AND. SS%SPECIFIC_HEAT_LIQUID < 0._EB .AND. SS%RAMP_CP_L_INDEX < 0) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(180): SPEC ',TRIM(SS%ID),' does not have a SPECIFIC_HEAT_LIQUID or a RAMP_CP_L.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SS%TMP_MELT < 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(181): SPEC ',TRIM(SS%ID),' does not have a MELTING_TEMPERATURE.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SS%TMP_V < 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(182): SPEC ',TRIM(SS%ID),' does not have a VAPORIZATION_TEMPERATURE.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SS%TMP_V <= SS%TMP_MELT) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(183): SPEC ',TRIM(SS%ID),' MELTING_TEMPERATURE must be less than VAPORIZATION_TEMPERATURE.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SS%HEAT_OF_VAPORIZATION < 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(184): SPEC ',TRIM(SS%ID),' does not have a HEAT_OF_VAPORIZATION.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SS%DENSITY_LIQUID < 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(185): SPEC ',TRIM(SS%ID),' does not have a DENSITY_LIQUID.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SS%SPECIFIC_HEAT_LIQUID < 0._EB .AND. SS%RAMP_CP_L_INDEX < 0 .AND. CP_TEMP < 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(180): SPEC ',TRIM(SS%ID),' does not have a SPECIFIC_HEAT_LIQUID or RAMP_CP_L.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      TMPMIN = MIN(TMPMIN,SS%TMP_MELT)

      ALLOCATE(SS%C_P_L(0:I_MAX_TEMP),STAT=IZERO)
      CALL ChkMemErr('PROC_SPEC','SS%C_P_L',IZERO)
      SS%C_P_L=SS%SPECIFIC_HEAT_LIQUID
      ALLOCATE(SS%C_P_L_BAR(0:I_MAX_TEMP),STAT=IZERO)
      CALL ChkMemErr('PROC_SPEC','SS%C_P_L_BAR',IZERO)
      ALLOCATE(SS%H_L(0:I_MAX_TEMP),STAT=IZERO)
      CALL ChkMemErr('PROC_SPEC','SS%H_L',IZERO)
      ALLOCATE(SS%H_V(0:I_MAX_TEMP),STAT=IZERO)
      CALL ChkMemErr('PROC_SPEC','SS%H_V',IZERO)

      SS%H_L = 0._EB

      DO I=1,I_MAX_TEMP
         IF (SS%SPECIFIC_HEAT_LIQUID > 0._EB) THEN
            SS%C_P_L(I) = SS%SPECIFIC_HEAT_LIQUID
         ELSEIF (SS%RAMP_CP_L_INDEX > 0) THEN
            SS%C_P_L(I) = EVALUATE_RAMP(REAL(J,EB),SS%RAMP_CP_L_INDEX,TAU=1._EB)*1000._EB
         ELSE
            CALL THERMO_TABLE_LIQUID(I,SS%C_P_L(I),H_V,TMP_REF,TMP_MELT,TMP_V,SS%PROP_INDEX,SS%ISFUEL,DENSITY,MU_LIQUID,K_LIQUID,&
                                     BETA_LIQUID)
         ENDIF
         IF (I==1) SS%C_P_L(0) = SS%C_P_L(1)
         SS%H_L(I) = SS%H_L(I-1) + 0.5_EB*(SS%C_P_L(I-1) + SS%C_P_L(I))
      ENDDO

      ITMP = INT(SS%H_V_REFERENCE_TEMPERATURE)
      H_REF = SS%H_L(ITMP) + (SS%H_V_REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * (SS%H_L(ITMP+1)-SS%H_L(ITMP))
      H_REF_2 = SPECIES(N)%H_G(ITMP) + (SS%H_V_REFERENCE_TEMPERATURE-REAL(ITMP,EB)) * (SPECIES(N)%H_G(ITMP+1)-SPECIES(N)%H_G(ITMP))
      H_CORR = SS%HEAT_OF_VAPORIZATION - (H_REF_2 - H_REF)

      SS%H_L = SS%H_L - H_CORR

      DO I=1,I_MAX_TEMP
         SS%H_V(I) = SPECIES(N)%H_G(I) - SS%H_L(I)
         SS%C_P_L_BAR(I) = SS%H_L(I) / REAL(I,EB)
      ENDDO
      SS%H_V(0) = SPECIES(N)%H_G(0) - SS%H_L(0)
      SS%C_P_L_BAR(0) = SS%H_L(0)

      SS%PR_LIQUID = SS%MU_LIQUID*SS%C_P_L(NINT(TMPA))/SS%K_LIQUID
   ENDIF LIQUID_IF

ENDDO SPEC_LOOP

END SUBROUTINE PROC_SPEC_1

!> \brief Create the Z to Y transformation matrix and fill up the gas property tables

SUBROUTINE PROC_SMIX

USE MATH_FUNCTIONS, ONLY: EVALUATE_RAMP
USE PROPERTY_DATA, ONLY: CALC_MIX_PROPS
REAL(EB), ALLOCATABLE, DIMENSION(:) :: RSQ_MW_Y
REAL(EB) :: MU_TMP_Z,CP_TMP_Z,K_TMP_Z,D_TMP_Z,G_F_TMP_Z
REAL(EB) :: H1
INTEGER :: N,NS,ITMP
LOGICAL :: SS_ALL_H_F
TYPE(SPECIES_MIXTURE_TYPE), POINTER :: SM=>NULL()

! Set up RSQ Arrays

ALLOCATE(RSQ_MW_Z(N_TRACKED_SPECIES),STAT=IZERO)
CALL ChkMemErr('READ','RSQ_MW_Z',IZERO)

RSQ_MW_Z = 1._EB/SQRT(SPECIES_MIXTURE%MW)

ALLOCATE(RSQ_MW_Y(N_SPECIES),STAT=IZERO)
CALL ChkMemErr('READ','RSQ_MW_Y',IZERO)

RSQ_MW_Y=1._EB/SQRT(SPECIES%MW)

ALLOCATE(CPBAR_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','CPBAR_Z',IZERO)
CPBAR_Z = 0._EB

ALLOCATE(H_SENS_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','H_SENS_Z',IZERO)
H_SENS_Z = 0._EB

ALLOCATE(K_RSQMW_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','K_RSQMW_Z',IZERO)
K_RSQMW_Z = 0._EB

ALLOCATE(MU_RSQMW_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','MU_RSQMW_Z',IZERO)
MU_RSQMW_Z = 0._EB

ALLOCATE(CP_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','CP_Z',IZERO)
CP_Z = 0._EB

ALLOCATE(D_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','D_Z',IZERO)
D_Z = 0._EB

ALLOCATE(G_F_Z(0:I_MAX_TEMP,N_TOTAL_SCALARS))
CALL ChkMemErr('READ','G_F_Z',IZERO)
G_F_Z = 0._EB

! Loop through temperatures from 1 K to I_MAX_TEMP K to get temperature-specific gas properties.

SPEC_LOOP: DO N=1,N_TRACKED_SPECIES
   SM => SPECIES_MIXTURE(N)
   IF (SM%EVAPORATING) THEN
      IF (.NOT. ALLOCATED(SPECIES(SM%SINGLE_SPEC_INDEX)%C_P_L)) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(187): SPEC ',TRIM(SM%ID),' is used for droplets and does not have liquid properties.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
   D_TMP_Z  = -1._EB
   MU_TMP_Z = -1._EB
   K_TMP_Z  = -1._EB
   CP_TMP_Z = -1._EB
   G_F_TMP_Z = -1._EB
   SM%EXPLICIT_G_F = .TRUE.
   IF (.NOT. SM%RAMP_G_F_INDEX > 0) THEN
      DO J = 1, N_SPECIES
         IF (SM%MASS_FRACTION(J) >0._EB .AND. .NOT. SPECIES(J)%EXPLICIT_G_F) THEN
            SM%EXPLICIT_G_F = .FALSE.
            EXIT
         ENDIF
      ENDDO
   ENDIF

   TABLE_LOOP: DO J=1,I_MAX_TEMP

      CALL CALC_MIX_PROPS(J,D_TMP_Z,MU_TMP_Z,K_TMP_Z,CP_TMP_Z,SM%EPSK,SM%SIG,SM%D_USER,SM%MU_USER,SM%K_USER,SM%MW,&
                          SM%SPECIFIC_HEAT,SM%PR_USER)
      IF (SM%RAMP_CP_INDEX>0)  CP_TMP_Z  = EVALUATE_RAMP(REAL(J,EB),SM%RAMP_CP_INDEX)*1000._EB
      IF (SM%RAMP_D_INDEX>0)   D_TMP_Z   = EVALUATE_RAMP(REAL(J,EB),SM%RAMP_D_INDEX,TAU=1._EB)
      IF (SM%RAMP_G_F_INDEX>0) G_F_TMP_Z = EVALUATE_RAMP(REAL(J,EB),SM%RAMP_G_F_INDEX,TAU=1._EB)
      IF (SM%RAMP_K_INDEX>0)   K_TMP_Z   = EVALUATE_RAMP(REAL(J,EB),SM%RAMP_K_INDEX,TAU=1._EB)*RSQ_MW_Z(N)
      IF (SM%RAMP_MU_INDEX>0)  MU_TMP_Z  = EVALUATE_RAMP(REAL(J,EB),SM%RAMP_MU_INDEX,TAU=1._EB)*RSQ_MW_Z(N)

      ! For each tracked species, store the mass-weighted property values

      IF (D_TMP_Z > 0._EB) THEN
         D_Z(J,N) = D_TMP_Z
      ELSE
         D_Z(J,N) = SPECIES_MIXTURE(N)%MW*SUM(Z2Y(:,N)*SS_D(:,J)/SPECIES(:)%MW)
      ENDIF
      IF (CP_TMP_Z > 0._EB) THEN
         CP_Z(J,N) = CP_TMP_Z
      ELSE
         CP_Z(J,N) = SUM(Z2Y(:,N) * SS_CP(:,J))
      ENDIF
      IF (J==1) CP_Z(0,N) = CP_Z(1,N)
      H_SENS_Z(J,N) = H_SENS_Z(J-1,N) + 0.5_EB*(CP_Z(J,N)+CP_Z(J-1,N))
      IF (MU_TMP_Z > 0._EB) THEN
         MU_RSQMW_Z(J,N) = MU_TMP_Z
      ELSE
         MU_RSQMW_Z(J,N) = SUM(Z2Y(:,N) * SS_MU(:,J)) / SUM(Z2Y(:,N) * RSQ_MW_Y(:)) * RSQ_MW_Z(N)
      ENDIF
      IF (K_TMP_Z > 0._EB) THEN
         K_RSQMW_Z(J,N)  = K_TMP_Z
      ELSE
         K_RSQMW_Z(J,N)  = SUM(Z2Y(:,N) * SS_K(:,J)) / SUM(Z2Y(:,N) * RSQ_MW_Y(:)) * RSQ_MW_Z(N)
      ENDIF
      IF (G_F_TMP_Z > 0._EB) THEN
         G_F_Z(J,N) = G_F_TMP_Z
      ELSE
         G_F_Z(J,N) = SUM(Z2Y(:,N) * SS_G_F(:,J))
      ENDIF
   ENDDO TABLE_LOOP

   ! Find enthalpy at 0 K
   IF (SM%REFERENCE_ENTHALPY > -1.E21_EB) THEN
      ITMP = INT(SM%REFERENCE_TEMPERATURE)
      H1 = H_SENS_Z(ITMP,N) + (SM%REFERENCE_TEMPERATURE - REAL(ITMP,EB)) * (H_SENS_Z(ITMP+1,N)-H_SENS_Z(ITMP,N))
      CPBAR_Z(0,N) = SM%REFERENCE_ENTHALPY - H1
   ELSEIF (SM%H_F > -1.E21_EB) THEN
      ITMP = INT(H_F_REFERENCE_TEMPERATURE)
      H1 = H_SENS_Z(ITMP,N) + (H_F_REFERENCE_TEMPERATURE - REAL(ITMP,EB)) * (H_SENS_Z(ITMP+1,N)-H_SENS_Z(ITMP,N))
      SM%H_F = SM%H_F / SM%MW * 1000._EB
      CPBAR_Z(0,N) = SM%H_F - H1
   ELSE
      ITMP = INT(H_F_REFERENCE_TEMPERATURE)
      H1 = H_SENS_Z(ITMP,N) + (H_F_REFERENCE_TEMPERATURE - REAL(ITMP,EB)) * (H_SENS_Z(ITMP+1,N)-H_SENS_Z(ITMP,N))
      SM%H_F = SUM(Z2Y(:,N) * SPECIES(:)%H_F)
      CPBAR_Z(0,N) = SM%H_F - H1
   ENDIF

   ! Define CPBAR_Z
   DO J = 1,I_MAX_TEMP
      CPBAR_Z(J,N) = (CPBAR_Z(0,N)+H_SENS_Z(J,N))/REAL(J,EB)
   ENDDO

   ! Define H_F if REFERENCE_ENTHALPY was defined
   IF (SM%REFERENCE_ENTHALPY > -1.E21_EB) THEN
      ITMP = INT(H_F_REFERENCE_TEMPERATURE)
      H1 = H_SENS_Z(ITMP,N) + (H_F_REFERENCE_TEMPERATURE - REAL(ITMP,EB)) * (H_SENS_Z(ITMP+1,N)-H_SENS_Z(ITMP,N))
      SM%H_F = CPBAR_Z(0,N) + H1
   ENDIF
   ! Adjust H_SENS_Z to 0 at the H_F_REFERENCE_TEMPERATURE
   IF (.NOT. CONSTANT_SPECIFIC_HEAT_RATIO) THEN
      ITMP = INT(H_F_REFERENCE_TEMPERATURE)
      H1 = H_SENS_Z(ITMP,N) + (H_F_REFERENCE_TEMPERATURE - REAL(ITMP,EB)) * (H_SENS_Z(ITMP+1,N)-H_SENS_Z(ITMP,N))
      H_SENS_Z(:,N) = H_SENS_Z(:,N) - H1
   ENDIF

   IF (SM%EXPLICIT_H_F) THEN
      SM%H_F_HOC = SM%H_F
   ELSE
      SS_ALL_H_F = .TRUE.
      H1 = 0._EB
      DO NS=1,N_SPECIES
         IF (SM%MASS_FRACTION(NS) > 0._EB) THEN
            IF (SPECIES(NS)%H_F_LISTED > -1.E21_EB .OR. SPECIES(NS)%EXPLICIT_H_F) THEN
               IF (SPECIES(NS)%EXPLICIT_H_F) THEN
                  H1 = H1 + SM%MASS_FRACTION(NS) * SPECIES(NS)%H_F
               ELSE
                  H1 = H1 + SM%MASS_FRACTION(NS) * SPECIES(NS)%H_F_LISTED
               ENDIF
            ELSE
               SS_ALL_H_F = .FALSE.
               EXIT
            ENDIF
         ENDIF
      ENDDO
      IF (SS_ALL_H_F) SM%H_F_HOC = H1
   ENDIF
ENDDO SPEC_LOOP

D_Z(0,:) = D_Z(1,:)
MU_RSQMW_Z(0,:) = MU_RSQMW_Z(1,:)
K_RSQMW_Z(0,:) = K_RSQMW_Z(1,:)

DEALLOCATE(SS_CP)
DEALLOCATE(SS_D)
DEALLOCATE(SS_G_F)
DEALLOCATE(SS_K)
DEALLOCATE(SS_MU)

DEALLOCATE(RSQ_MW_Y)

END SUBROUTINE PROC_SMIX


!> \brief Process SPECies input data

SUBROUTINE PROC_SPEC_2

USE RADCONS, ONLY : MIE_NDG
INTEGER :: N,ITMP,IZERO
TYPE(SPECIES_TYPE),POINTER:: SS=>NULL()
TYPE(SPECIES_MIXTURE_TYPE),POINTER:: SM=>NULL()

SPEC_LOOP: DO N=1,N_TRACKED_SPECIES
   IF (.NOT. SPECIES_MIXTURE(N)%EVAPORATION_SMIX_INDEX > 0) CYCLE SPEC_LOOP
   SM => SPECIES_MIXTURE(N)
   SS => SPECIES(SPECIES_MIXTURE(N)%SINGLE_SPEC_INDEX)
   IF (.NOT. SS%CONDENSABLE) CYCLE SPEC_LOOP

   SS%DENSITY_SOLID = SS%DENSITY_LIQUID
   SS%CONDUCTIVITY_SOLID = SS%K_LIQUID

   CPBAR_Z(:,N) = SS%C_P_L_BAR
   CP_Z(:,N) = SS%C_P_L
   ITMP = INT(H_F_REFERENCE_TEMPERATURE)
   SM%H_F = SS%H_L(ITMP) + (H_F_REFERENCE_TEMPERATURE-REAL(ITMP,EB))*(SS%H_L(ITMP+1)-SS%H_L(ITMP))

   ! Allocate MIE arrays for condensed phase
   ALLOCATE(SM%WQABS(0:MIE_NDG,1:NUMBER_SPECTRAL_BANDS),STAT=IZERO)
   CALL ChkMemErr('PROC_SPEC_2','WQABS',IZERO)
   SM%WQABS = 0._EB
   ALLOCATE(SM%WQSCA(0:MIE_NDG,1:NUMBER_SPECTRAL_BANDS),STAT=IZERO)
   CALL ChkMemErr('PROC_SPEC_2','WQSCA',IZERO)
   SM%WQSCA = 0._EB
   ALLOCATE(SM%R50(0:MIE_NDG),STAT=IZERO)
   CALL ChkMemErr('PROC_SPEC_2','R50',IZERO)
   SM%R50 = 0._EB

END DO SPEC_LOOP

END SUBROUTINE PROC_SPEC_2


!> \brief Read the COMBustion namelist line

SUBROUTINE READ_COMB

NAMELIST /COMB/ CHECK_REALIZABILITY,COMPUTE_ADIABATIC_FLAME_TEMPERATURE,EXTINCTION_MODEL,FINITE_RATE_MIN_TEMP,&
                FIXED_MIX_TIME,FREE_BURN_TEMPERATURE,&
                INITIAL_UNMIXED_FRACTION,MAX_CHEMISTRY_SUBSTEPS,N_FIXED_CHEMISTRY_SUBSTEPS,&
                ODE_SOLVER,RICHARDSON_ERROR_TOLERANCE,SUPPRESSION,TAU_CHEM,TAU_FLAME,ZZ_MIN_GLOBAL

ODE_SOLVER         = 'null'

! Read the COMB line

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COMB_LOOP: DO
   CALL CHECKREAD('COMB',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COMB_LOOP
   READ(LU_INPUT,COMB,END=23,ERR=24,IOSTAT=IOS)
   24 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with COMB line.') ; RETURN ; ENDIF
ENDDO COMB_LOOP
23 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Extinction Model

IF (TRIM(EXTINCTION_MODEL)/='null') THEN
   SELECT CASE (TRIM(EXTINCTION_MODEL))
      CASE ('EXTINCTION 1')
         EXTINCT_MOD = EXTINCTION_1
      CASE ('EXTINCTION 2')
         EXTINCT_MOD = EXTINCTION_2
      CASE DEFAULT
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(188): EXTINCTION_MODEL, ',TRIM(EXTINCTION_MODEL),', is not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT
ELSE
   IF (SIM_MODE==VLES_MODE .OR. SIM_MODE==SVLES_MODE) THEN
      EXTINCT_MOD = EXTINCTION_1
      EXTINCTION_MODEL = 'EXTINCTION 1'
   ELSE
      EXTINCT_MOD = EXTINCTION_2
      EXTINCTION_MODEL = 'EXTINCTION 2'
   ENDIF
ENDIF

FINITE_RATE_MIN_TEMP = FINITE_RATE_MIN_TEMP + TMPM

! Convert C to K for EXTINCTION 1 temperature cut-off

FREE_BURN_TEMPERATURE = FREE_BURN_TEMPERATURE + TMPM

! Don't let MAX_CHEMISTRY_SUBSTEPS change the user-specified number of substeps

IF (N_FIXED_CHEMISTRY_SUBSTEPS > 0) MAX_CHEMISTRY_SUBSTEPS = N_FIXED_CHEMISTRY_SUBSTEPS

END SUBROUTINE READ_COMB


!> \brief Read the REAC namelist line(s), gas phase reaction parameters

SUBROUTINE READ_REAC

USE PROPERTY_DATA, ONLY : ELEMENT,GET_FORMULA_WEIGHT,GAS_PROPS,&
                          LOOKUP_CHI_R,LOOKUP_CRITICAL_FLAME_TEMPERATURE,LOOKUP_LOWER_OXYGEN_LIMIT
USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX
CHARACTER(LABEL_LENGTH) :: FUEL,RADCAL_ID='null',SPEC_ID_NU(MAX_SPECIES),SPEC_ID_N_S(MAX_SPECIES),&
                           THIRD_EFF_ID(MAX_SPECIES),RAMP_CHI_R
CHARACTER(FORMULA_LENGTH) :: FORMULA
CHARACTER(255) :: EQUATION,TEMP_EQUATION=''
CHARACTER(LABEL_LENGTH), ALLOCATABLE, DIMENSION(:) :: REAC_FUEL_2
INTEGER :: NR,NS,NS2,NFR,PRIORITY,PROD_COUNTER=0, NEW_REAC=0,N_SIMPLE_CHEMISTRY_REACTIONS,IZ,FUEL_INDEX,N_REVERSE=0
REAL(EB) :: SOOT_YIELD,CO_YIELD,HCN_YIELD,EPUMO2,A,LOWER_OXYGEN_LIMIT, &
            CRITICAL_FLAME_TEMPERATURE,HEAT_OF_COMBUSTION,HOC_COMPLETE,E,C,H,N,O, &
            N_T,NU(MAX_SPECIES),N_S(MAX_SPECIES),THIRD_EFF(MAX_SPECIES),&
            FUEL_C_TO_CO_FRACTION,FUEL_H_TO_H2_FRACTION,FUEL_N_TO_HCN_FRACTION,AUTO_IGNITION_TEMPERATURE
REAL(EB) :: E_TMP=0._EB,S_TMP=0._EB,ATOM_COUNTS(118),MW_FUEL=0._EB,H_F=0._EB,PR_TMP=0._EB
LOGICAL :: L_TMP,CHECK_ATOM_BALANCE,REVERSE,THIRD_BODY
TYPE(REACTION_TYPE), POINTER, DIMENSION(:) :: REAC_TEMP
REAL(EB), DIMENSION(6,MAX_AIT_EXCLUSION_ZONES) :: AIT_EXCLUSION_ZONE
CHARACTER(LABEL_LENGTH), DIMENSION(MAX_AIT_EXCLUSION_ZONES) :: AIT_EXCLUSION_ZONE_DEVC_ID,AIT_EXCLUSION_ZONE_CTRL_ID
NAMELIST /REAC/ A,AIT_EXCLUSION_ZONE,AIT_EXCLUSION_ZONE_CTRL_ID,AIT_EXCLUSION_ZONE_DEVC_ID,AUTO_IGNITION_TEMPERATURE,&
                C,CHECK_ATOM_BALANCE,CO_YIELD,CRITICAL_FLAME_TEMPERATURE,E,EPUMO2,EQUATION,FORMULA,FUEL,&
                FUEL_C_TO_CO_FRACTION,FUEL_H_TO_H2_FRACTION,FUEL_N_TO_HCN_FRACTION,&
                FUEL_RADCAL_ID,FYI,H,HCN_YIELD,HEAT_OF_COMBUSTION,HOC_COMPLETE,&
                ID,IDEAL,LOWER_OXYGEN_LIMIT,N,N_S,N_SIMPLE_CHEMISTRY_REACTIONS,N_T,NU,O,PRIORITY,RADIATIVE_FRACTION,&
                RAMP_CHI_R,REAC_ATOM_ERROR,REAC_MASS_ERROR,REVERSE,SOOT_YIELD,&
                SPEC_ID_N_S,SPEC_ID_NU,THIRD_BODY,THIRD_EFF_ID,THIRD_EFF
TYPE(REACTION_TYPE),POINTER :: RN2=>NULL()

ATOM_COUNTS = 0._EB
N_REACTIONS = 0
NFR = 0 ! Number of fast reactions

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

COUNT_REAC_LOOP: DO
   CALL CHECKREAD('REAC',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_REAC_LOOP
   CALL SET_REAC_DEFAULTS
   READ(LU_INPUT,REAC,END=435,ERR=434,IOSTAT=IOS)
   N_REACTIONS = N_REACTIONS + 1
   IF (REVERSE) N_REVERSE = N_REVERSE+1
   434 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with REAC ',N_REACTIONS+1
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO COUNT_REAC_LOOP

435 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_REACTIONS==0) RETURN

ALLOCATE(REACTION(N_REACTIONS+N_REVERSE),STAT=IZERO)
ALLOCATE(SIMPLE_FUEL_DEFINED(N_REACTIONS),STAT=IZERO)
SIMPLE_FUEL_DEFINED = .FALSE.

! Read and store the reaction parameters
N_REVERSE = 0
REAC_READ_LOOP: DO NR=1,N_REACTIONS

   ! Read the REAC line

   CALL CHECKREAD('REAC',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT REAC_READ_LOOP
   CALL SET_REAC_DEFAULTS
   READ(LU_INPUT,REAC)

   RN => REACTION(NR)

   IF ((A > 0._EB .OR. E > 0._EB) .AND. (C>TWO_EPSILON_EB .OR. H>TWO_EPSILON_EB)) THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(189): REAC ',NR,' cannot use both finite rate REAC and simple chemistry.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   A_E_IF: IF (A < 0._EB .AND. E < 0._EB .AND. TRIM(SPEC_ID_NU(1))=='null' .AND. TRIM(EQUATION)=='null') THEN
      RN%SIMPLE_CHEMISTRY = .TRUE.
      SIMPLE_CHEMISTRY = .TRUE.

      ! Ensure that there is a specified fuel

      IF (FUEL=='null' .AND. ID/='null') FUEL = ID ! Backward compatibility
      IF (FUEL=='null') THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(190): REAC ',NR,' requires a FUEL.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      ! Setup simple chemistry reactions
      C_H_IF: IF(C<=TWO_EPSILON_EB .AND. H<=TWO_EPSILON_EB) THEN
         IF (TRIM(FORMULA)=='null') THEN
            MW_FUEL = -1._EB
            CALL GET_PROP_INDEX(FUEL,FUEL_INDEX)
            CALL GAS_PROPS(FUEL_INDEX,S_TMP,E_TMP,PR_TMP,MW_FUEL,FORMULA,L_TMP,ATOM_COUNTS,H_F,RADCAL_ID)
         ELSE
            CALL GET_FORMULA_WEIGHT(FORMULA,MW_FUEL,ATOM_COUNTS)
            L_TMP = .TRUE.
         ENDIF
         IF (L_TMP) THEN
            SIMPLE_FUEL_DEFINED(NR) = .TRUE.
            IF (ANY(ATOM_COUNTS(2:5)>0._EB) .OR. ANY(ATOM_COUNTS(9:)>0._EB)) THEN
               WRITE(MESSAGE,'(A)') 'ERROR(191): Fuel FORMULA for SIMPLE_CHEMISTRY can only contain C,H,O, and N.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ELSE
               C = ATOM_COUNTS(6)
               H = ATOM_COUNTS(1)
               O = ATOM_COUNTS(8)
               N = ATOM_COUNTS(7)
            ENDIF
            IF (C<=TWO_EPSILON_EB .AND. H<=TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A)') 'ERROR(192): Specify fuel chemistry using C and/or H when using simple chemistry'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDIF
      ELSE C_H_IF
         SIMPLE_FUEL_DEFINED = .TRUE.
         MW_FUEL = ELEMENT(6)%MASS*C+ELEMENT(1)%MASS*H+ELEMENT(8)%MASS*O+ELEMENT(7)%MASS*N
      ENDIF C_H_IF

      ! Define variables for 2-step if selected.
      RN%N_SIMPLE_CHEMISTRY_REACTIONS = N_SIMPLE_CHEMISTRY_REACTIONS
      IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==2) THEN
         IF (FUEL_C_TO_CO_FRACTION >= 0._EB .AND. FUEL_C_TO_CO_FRACTION <= 1._EB) THEN
            RN%FUEL_C_TO_CO_FRACTION = FUEL_C_TO_CO_FRACTION
         ELSE
            WRITE(MESSAGE,'(A)') 'ERROR(193): FUEL_C_TO_CO_FRACTION must be between 0 and 1.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (FUEL_H_TO_H2_FRACTION >= 0._EB .AND. FUEL_H_TO_H2_FRACTION <= 1._EB) THEN
            RN%FUEL_H_TO_H2_FRACTION = FUEL_H_TO_H2_FRACTION
         ELSE
            WRITE(MESSAGE,'(A)') 'ERROR(194): FUEL_H_TO_H2O_FRACTION must be between 0 and 1.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (FUEL_N_TO_HCN_FRACTION >= 0._EB .AND. FUEL_N_TO_HCN_FRACTION <= 1._EB) THEN
            RN%FUEL_N_TO_HCN_FRACTION = FUEL_N_TO_HCN_FRACTION
         ELSE
            WRITE(MESSAGE,'(A)') 'ERROR(195): FUEL_N_TO_HCN_FRACTION must be between 0 and 1.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF
   ENDIF A_E_IF

   IF (A > 0._EB .OR.  E > 0._EB) SUPPRESSION = .FALSE.

   IF (.NOT. RN%SIMPLE_CHEMISTRY .AND. TRIM(SPEC_ID_NU(1))=='null' .AND. TRIM(EQUATION)=='null') THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(196): REAC ',NR,' SPEC_ID_NU and NU arrays or EQUATION must be defined.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   RN%A_IN                      = A
   RN%A_PRIME                   = A
   RN%C                         = C
   RN%CHECK_ATOM_BALANCE        = CHECK_ATOM_BALANCE
   RN%CO_YIELD                  = CO_YIELD
   IF (CRITICAL_FLAME_TEMPERATURE<0._EB) CALL LOOKUP_CRITICAL_FLAME_TEMPERATURE(FUEL,CRITICAL_FLAME_TEMPERATURE)
   RN%CRITICAL_FLAME_TEMPERATURE = CRITICAL_FLAME_TEMPERATURE + TMPM ! Convert C to K
   RN%AUTO_IGNITION_TEMPERATURE  = AUTO_IGNITION_TEMPERATURE  + TMPM ! Convert C to K
   DO IZ=1,MAX_AIT_EXCLUSION_ZONES
      IF (AIT_EXCLUSION_ZONE(1,IZ)>-1.E5) THEN
         RN%N_AIT_EXCLUSION_ZONES = RN%N_AIT_EXCLUSION_ZONES + 1
         RN%AIT_EXCLUSION_ZONE(IZ)%X1 = AIT_EXCLUSION_ZONE(1,IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%X2 = AIT_EXCLUSION_ZONE(2,IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%Y1 = AIT_EXCLUSION_ZONE(3,IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%Y2 = AIT_EXCLUSION_ZONE(4,IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%Z1 = AIT_EXCLUSION_ZONE(5,IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%Z2 = AIT_EXCLUSION_ZONE(6,IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%DEVC_ID = AIT_EXCLUSION_ZONE_DEVC_ID(IZ)
         RN%AIT_EXCLUSION_ZONE(IZ)%CTRL_ID = AIT_EXCLUSION_ZONE_CTRL_ID(IZ)
      ENDIF
   ENDDO
   RN%E                         = E*1000._EB
   RN%E_IN                      = E
   RN%EQUATION                  = EQUATION
   RN%EPUMO2                    = EPUMO2*1000._EB
   RN%FUEL                      = FUEL
   RN%FYI                       = FYI
   RN%H                         = H
   RN%HCN_YIELD                 = HCN_YIELD
   RN%HEAT_OF_COMBUSTION        = HEAT_OF_COMBUSTION*1000._EB
   RN%HOC_COMPLETE              = HOC_COMPLETE*1000._EB
   RN%ID                        = ID
   IF (LOWER_OXYGEN_LIMIT<0._EB) CALL LOOKUP_LOWER_OXYGEN_LIMIT(FUEL,LOWER_OXYGEN_LIMIT)
   RN%Y_O2_MIN                  = LOWER_OXYGEN_LIMIT*MW_O2/(LOWER_OXYGEN_LIMIT*MW_O2+(1._EB-LOWER_OXYGEN_LIMIT)*MW_N2)
   RN%MW_FUEL                   = MW_FUEL
   RN%N                         = N
   RN%N_T                       = N_T
   RN%PRIORITY                  = PRIORITY
   MAX_PRIORITY                 = MAX(MAX_PRIORITY,PRIORITY)
   RN%O                         = O
   RN%RAMP_CHI_R                = RAMP_CHI_R
   RN%SOOT_YIELD                = SOOT_YIELD
   RN%THIRD_BODY                = THIRD_BODY

   IF (RN%RAMP_CHI_R/='null') CALL GET_RAMP_INDEX(RN%RAMP_CHI_R,'TIME',RN%RAMP_CHI_R_INDEX)

   IF (RN%A_IN<0._EB .AND. RN%E<0._EB .AND. .NOT.RN%REVERSE) THEN
      RN%FAST_CHEMISTRY=.TRUE.
      NFR = NFR + 1
   ENDIF

   IF (RADIATIVE_FRACTION < 0._EB) THEN
      IF (SIM_MODE==DNS_MODE) THEN
         RN%CHI_R = 0._EB
      ELSE
         CALL LOOKUP_CHI_R(FUEL,RN%CHI_R)
      ENDIF
   ELSE
      RN%CHI_R = RADIATIVE_FRACTION
   ENDIF

   ! Determine the number of stoichiometric coefficients for this reaction

   SIMPLE_IF: IF (.NOT. RN%SIMPLE_CHEMISTRY) THEN
      NS2 = 0
      DO NS=1,MAX_SPECIES
         IF (TRIM(SPEC_ID_NU(NS))/='null') THEN
            NS2=NS2+1
         ELSE
            EXIT
         ENDIF
      ENDDO
      RN%N_SMIX = NS2
      NS2 = 0
      IF(TRIM(RN%EQUATION)/='null') RN%N_SMIX = MAX_SPECIES
      DO NS=1,MAX_SPECIES
         IF (TRIM(SPEC_ID_N_S(NS))/='null') THEN
            NS2=NS2+1
         ELSE
            EXIT
         ENDIF
      ENDDO
      RN%N_SPEC_READ = NS2
      IF (RN%THIRD_BODY) THEN
         RN%N_THIRD = 0
         DO NS=1,MAX_SPECIES
            IF (THIRD_EFF_ID(NS)/='null') THEN
               IF (THIRD_EFF(NS) < 0._EB) THEN
                  WRITE(MESSAGE,'(A,I0,A)') 'ERROR(197): REAC ',NR,' THIRD_EFF values must be >= 0.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               RN%N_THIRD = RN%N_THIRD + 1
            ENDIF
         ENDDO
         IF (RN%N_THIRD > 0) THEN
            ALLOCATE(RN%THIRD_EFF_READ(RN%N_THIRD))
            RN%THIRD_EFF_READ(1:RN%N_THIRD) = THIRD_EFF(1:RN%N_THIRD)
            ALLOCATE(RN%THIRD_EFF_ID_READ(RN%N_THIRD))
            RN%THIRD_EFF_ID_READ(1:RN%N_THIRD) = THIRD_EFF_ID(1:RN%N_THIRD)
         ENDIF
      ENDIF

      IF (RN%N_SPEC_READ > 0) THEN
         ALLOCATE(RN%N_S_READ(RN%N_SPEC_READ))
         RN%N_S_READ(1:RN%N_SPEC_READ) = N_S(1:RN%N_SPEC_READ)
         ALLOCATE(RN%SPEC_ID_N_S_READ(RN%N_SPEC_READ))
         RN%SPEC_ID_N_S_READ = 'null'
         RN%SPEC_ID_N_S_READ(1:RN%N_SPEC_READ)=SPEC_ID_N_S(1:RN%N_SPEC_READ)
      ENDIF
      ALLOCATE(RN%NU_READ(RN%N_SMIX))
      RN%NU_READ(1:RN%N_SMIX) = NU(1:RN%N_SMIX)
      ALLOCATE(RN%SPEC_ID_NU_READ(RN%N_SMIX))
      RN%SPEC_ID_NU_READ = 'null'
      RN%SPEC_ID_NU_READ(1:RN%N_SMIX)=SPEC_ID_NU(1:RN%N_SMIX)
   ELSE SIMPLE_IF
      RN%N_SMIX = 3
      RN%N_SPEC_READ = 0
      ALLOCATE(RN%NU_READ(RN%N_SMIX))
      ALLOCATE(RN%SPEC_ID_NU_READ(RN%N_SMIX))
      PROD_COUNTER = PROD_COUNTER + 1
      IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==1) THEN
         RN%SPEC_ID_NU_READ(1) = RN%FUEL
         RN%SPEC_ID_NU_READ(2) = 'AIR'
         IF (PROD_COUNTER == 1) THEN
            RN%SPEC_ID_NU_READ(3) = 'PRODUCTS'
         ELSE
            WRITE(RN%SPEC_ID_NU_READ(3),'(A,I0)') 'PRODUCTS ',PROD_COUNTER
         ENDIF
      ELSE ! RN%N_SIMPLE_CHEMISTRY_REACTIONS=2
         NEW_REAC = NEW_REAC + 1
         RN%PAIR_INDEX = N_REACTIONS+NEW_REAC
         RN%SPEC_ID_NU_READ(1) = RN%FUEL
         RN%SPEC_ID_NU_READ(2) = 'AIR'

         ! Set up paired reaction INTERMEDIATE PRODUCTS + AIR -> PRODUCTS
         ALLOCATE(REAC_TEMP(N_REACTIONS+NEW_REAC-1),STAT=IZERO)
         REAC_TEMP(1:NR) = REACTION(1:NR)
         IF (NEW_REAC > 1) REAC_TEMP(N_REACTIONS+1:N_REACTIONS+NEW_REAC-1) = REACTION(N_REACTIONS+1:N_REACTIONS+NEW_REAC-1)
         DEALLOCATE(REACTION)
         ALLOCATE(REACTION(N_REACTIONS+NEW_REAC),STAT=IZERO)
         REACTION(1:NR) = REAC_TEMP(1:NR)
         IF (NEW_REAC > 1) REACTION(N_REACTIONS+1:N_REACTIONS+NEW_REAC-1) = REAC_TEMP(N_REACTIONS+1:N_REACTIONS+NEW_REAC-1)
         DEALLOCATE(REAC_TEMP)
         RN2 => REACTION(REACTION(NR)%PAIR_INDEX)
         RN2 = REACTION(NR)
         RN2%N_SIMPLE_CHEMISTRY_REACTIONS = -2
         RN2%PAIR_INDEX = NR
         RN2%PRIORITY = 2
         MAX_PRIORITY = MAX(MAX_PRIORITY,RN2%PRIORITY)
         IF (PROD_COUNTER ==1) THEN
            RN2%FUEL = 'INTERMEDIATE PRODUCTS'
            RN2%SPEC_ID_NU_READ(3) = 'PRODUCTS'
         ELSE
            WRITE(RN2%FUEL,'(A,I0)') 'INTERMEDIATE PRODUCTS ',PROD_COUNTER
            WRITE(RN2%SPEC_ID_NU_READ(3),'(A,I0)') 'PRODUCTS ',PROD_COUNTER
         ENDIF
         REACTION(NR)%SPEC_ID_NU_READ(3)  = RN2%FUEL
         RN2%SPEC_ID_NU_READ(1) = RN2%FUEL
         RN2%SPEC_ID_NU_READ(2) = 'AIR'
         IF (REACTION(NR)%ID=='null') THEN
            RN2%ID='null'
         ELSE
            RN2%ID=TRIM(REACTION(NR)%ID)//'_2'
         ENDIF
      ENDIF
   ENDIF SIMPLE_IF

   IF (REVERSE) THEN
      N_REVERSE = N_REVERSE + 1
      RN2 => REACTION(N_REACTIONS + N_REVERSE + NEW_REAC)
      RN2 = RN
      ! For reverse either negative NU or flip EQUATION over = sign for the reverse reaction
      IF (RN2%EQUATION=='null') THEN
         RN2%NU_READ = -1._EB*RN2%NU_READ
      ELSE
         NS = INDEX(EQUATION,'=')
         IF (NS==0) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(198): Problem with REAC ',NR,' invalid EQUATION specified.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         TEMP_EQUATION(1:LEN(TRIM(RN2%EQUATION))-NS)=EQUATION(NS+1:LEN(TRIM(RN2%EQUATION)))
         TEMP_EQUATION(LEN(TRIM(RN2%EQUATION))-NS+1:LEN(TRIM(RN2%EQUATION))-NS+1)='='
         TEMP_EQUATION(LEN(TRIM(RN2%EQUATION))-NS+2:LEN(TRIM(RN2%EQUATION)))=RN2%EQUATION(1:NS-1)
         RN2%EQUATION = TEMP_EQUATION
      ENDIF
      RN2%REVERSE =REVERSE
      RN2%ID = TRIM(RN2%ID)//'_R'
      RN2%REVERSE_INDEX = NR
   ENDIF
ENDDO REAC_READ_LOOP

N_REACTIONS = N_REACTIONS + NEW_REAC + N_REVERSE

ALLOCATE (DUPLICATE_FUEL(1:N_REACTIONS))
DUPLICATE_FUEL = .FALSE.
ALLOCATE (REAC_FUEL(1:N_REACTIONS))
ALLOCATE (REAC_FUEL_2(1:N_REACTIONS))
REAC_FUEL = REACTION%FUEL

DO NR=1,N_REACTIONS
   REAC_FUEL_2 = REAC_FUEL
   REAC_FUEL_2(NR) = 'null'
   IF (ANY(REAC_FUEL_2==REACTION(NR)%FUEL)) DUPLICATE_FUEL(NR) = .TRUE.
   IF (REACTION(NR)%SIMPLE_CHEMISTRY .AND. DUPLICATE_FUEL(NR)) THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(199): REAC ',NR,' uses simple chemistry and has a duplicate fuel to another reaction.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO

DEALLOCATE (REAC_FUEL_2)

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

CONTAINS

SUBROUTINE SET_REAC_DEFAULTS

A                           = -1._EB
AUTO_IGNITION_TEMPERATURE   = -TMPM
AIT_EXCLUSION_ZONE          = -1.E6_EB
AIT_EXCLUSION_ZONE_CTRL_ID  = 'null'
AIT_EXCLUSION_ZONE_DEVC_ID  = 'null'
C                           = 0._EB
CHECK_ATOM_BALANCE          = .TRUE.
CO_YIELD                    = 0._EB
CRITICAL_FLAME_TEMPERATURE  = -1._EB     ! Values for various fuels and default are in data.f90
E                           = -1._EB     ! J/mol
EPUMO2                      = 13100._EB  ! kJ/kg
EQUATION                    = 'null'
FORMULA                     = 'null'
FUEL                        = 'null'
FUEL_C_TO_CO_FRACTION       = TWTH
FUEL_H_TO_H2_FRACTION       = 0._EB
FUEL_N_TO_HCN_FRACTION      = 0._EB
FYI                         = 'null'
H                           = 0._EB
HEAT_OF_COMBUSTION          = -2.E20_EB
HCN_YIELD                   = 0._EB
HOC_COMPLETE                = -1._EB
ID                          = 'null'
LOWER_OXYGEN_LIMIT          = -1._EB
N                           = 0._EB
NU                          = 0._EB
N_S                         = -999._EB
N_T                         = 0._EB
N_SIMPLE_CHEMISTRY_REACTIONS= 1
O                           = 0._EB
PRIORITY                    = 1
RADIATIVE_FRACTION          = -1._EB
RAMP_CHI_R                  = 'null'
REAC_ATOM_ERROR             = 1.E-4_EB
REAC_MASS_ERROR             = 1.E-4_EB
REVERSE                     = .FALSE.
SOOT_YIELD                  = 0.0_EB
SPEC_ID_NU                  = 'null'
SPEC_ID_N_S                 = 'null'
THIRD_BODY                  = .FALSE.
THIRD_EFF_ID                = 'null'
THIRD_EFF                   = -1._EB

END SUBROUTINE SET_REAC_DEFAULTS


END SUBROUTINE READ_REAC


SUBROUTINE PROC_REAC_1
USE PROPERTY_DATA, ONLY : PARSE_EQUATION, SHUTDOWN_ATOM
REAL(EB) :: MASS_PRODUCT,MASS_REACTANT,REACTION_BALANCE(118),NU_Y(N_SPECIES)
INTEGER :: NS,NS2,NR
LOGICAL :: NAME_FOUND,SKIP_ATOM_BALANCE
TYPE (SPECIES_MIXTURE_TYPE), POINTER :: SM
TYPE (REACTION_TYPE), POINTER :: RN=>NULL(),RN2=>NULL()

IF (N_REACTIONS <=0) RETURN

! The following information is what the user would have entered into the input file in the more general case
DO NR=1,N_REACTIONS
   RN => REACTION(NR)
   !Set up one or two step simple chemistry
   SIMPLE_CHEM_IF: IF (RN%SIMPLE_CHEMISTRY) THEN
      ! Second simple chemistry reaction setup when first is setup
      IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==-2) THEN
         CYCLE
      ELSEIF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==1) THEN
         IF (RN%NU_O2<=0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(200): Fuel for simple chemistry has NU_O2<=0 and requires air for combustion.'
            CALL SHUTDOWN(MESSAGE)       ; RETURN
         ENDIF
         RN%NU_READ(1)      = -1._EB
         RN%NU_READ(2)      = -RN%NU_O2/SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         RN%NU_READ(3)      = -(RN%NU_READ(1)*SPECIES_MIXTURE(RN%FUEL_SMIX_INDEX)%MW+RN%NU_READ(2)*SPECIES_MIXTURE(1)%MW)/ &
                              SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%MW
         RN%N_SMIX          = 3
      ELSE ! RN%N_SIMPLE_CHEMISTRY_REACTIONS=2

         ! Setup FUEL + AIR -> INTERMEDIATE PRODUCTS
         IF (RN%NU_O2<=0._EB) THEN
            WRITE(MESSAGE,'(A)') 'ERROR(200): Fuel for simple chemistry has NU_O2<=0 and requires air for combustion.'
            CALL SHUTDOWN(MESSAGE)       ; RETURN
         ENDIF
         RN%NU_READ(1)      = -1._EB
         RN%NU_READ(2)      = -RN%NU_O2/SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         RN%NU_READ(3)      = -(RN%NU_READ(1)*SPECIES_MIXTURE(RN%FUEL_SMIX_INDEX)%MW+RN%NU_READ(2)*SPECIES_MIXTURE(1)%MW)/ &
                              SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%MW
         RN%N_SMIX          = 3
         ! Setup INTERMEDIATE PRODUCTS + AIR -> PRODUCTS
         RN2 => REACTION(RN%PAIR_INDEX)
         RN2%NU_READ(1)      = -RN%NU_READ(3)
         RN2%NU_READ(2)      = -(RN2%NU_O2-RN%NU_O2)/SPECIES_MIXTURE(1)%VOLUME_FRACTION(O2_INDEX)
         RN2%NU_READ(3)      = -(RN%NU_READ(1)*SPECIES_MIXTURE(RN%FUEL_SMIX_INDEX)%MW+&
                                 (RN%NU_READ(2)+RN2%NU_READ(2))*SPECIES_MIXTURE(1)%MW)/SPECIES_MIXTURE(RN2%PROD_SMIX_INDEX)%MW
         RN2%N_SMIX          = 3
         RN2%CHI_R           = RN%CHI_R
      ENDIF
   ENDIF SIMPLE_CHEM_IF
ENDDO

REAC_LOOP: DO NR=1,N_REACTIONS

   RN => REACTION(NR)

   IF (TRIM(RN%EQUATION)/='null') THEN
      IF(ANY(ABS(RN%NU_READ)>TWO_EPSILON_EB)) THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(201): REAC ',NR,'. Cannot set NUs if an EQUATION is specified.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      CALL PARSE_EQUATION(NR)
      RN%N_SMIX = 0
      DO NS=1,MAX_SPECIES
         IF(ABS(RN%NU_READ(NS))>TWO_EPSILON_EB) THEN
            RN%N_SMIX = RN%N_SMIX+1
         ENDIF
      ENDDO
   ENDIF

   ! Allocate the arrays that are going to carry the mixture stoichiometry to the rest of the code

   ALLOCATE(RN%NU(1:N_TRACKED_SPECIES))
   ALLOCATE(RN%NU_MW_O_MW_F(1:N_TRACKED_SPECIES))
   RN%NU          = 0._EB

   ! Transfer SPEC_ID_NU, SPEC_ID_N, NU, and N_S that were indexed by the order they were read in
   ! to now be indexed by the SMIX or SPEC index
   NU_Y = 0._EB
   DO NS=1,RN%N_SMIX
      IF (TRIM(RN%SPEC_ID_NU_READ(NS))=='null') CYCLE
      NAME_FOUND = .FALSE.
      DO NS2=1,N_TRACKED_SPECIES
         IF (TRIM(RN%SPEC_ID_NU_READ(NS))==TRIM(SPECIES_MIXTURE(NS2)%ID)) THEN
            NAME_FOUND = .TRUE.
            RN%NU(NS2) = RN%NU(NS2) + RN%NU_READ(NS)
            IF (RN%NU_READ(NS)<0._EB) THEN
               NU_Y(:) = NU_Y(:) + RN%NU_READ(NS) * SPECIES_MIXTURE(NS2)%VOLUME_FRACTION(:)
            ELSE
               IF (RN%NU(NS2) >=0._EB) NU_Y(:) = NU_Y(:) + RN%NU_READ(NS) * SPECIES_MIXTURE(NS2)%VOLUME_FRACTION(:)
            ENDIF
            EXIT
         ENDIF
      ENDDO
      IF (.NOT. NAME_FOUND) THEN
         WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(202): REAC ',NR,'. Tracked species ',TRIM(RN%SPEC_ID_NU_READ(NS)),' not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO

   RN%N_SMIX_FR = 0._EB
   RN%N_SMIX_R = 0._EB
   DO NS=1,N_TRACKED_SPECIES
      IF (ABS(RN%NU(NS)) > TWO_EPSILON_EB) RN%N_SMIX_FR = RN%N_SMIX_FR + 1
      IF (RN%NU(NS) < 0._EB) RN%N_SMIX_R = RN%N_SMIX_R + 1
   ENDDO
   ALLOCATE(RN%NU_MW_O_MW_F_FR(RN%N_SMIX_FR))
   ALLOCATE(RN%NU_INDEX(RN%N_SMIX_FR))
   ALLOCATE(RN%REACTANT_INDEX(RN%N_SMIX_R))

   IF (.NOT. RN%SIMPLE_CHEMISTRY) THEN
      IF (RN%FUEL=='null') THEN
         FIND_FUEL: DO NS=1,N_TRACKED_SPECIES
            IF (RN%NU(NS)<-TWO_EPSILON_EB) THEN
               RN%FUEL = SPECIES_MIXTURE(NS)%ID
               RN%FUEL_SMIX_INDEX = NS
               EXIT FIND_FUEL
            ENDIF
         ENDDO FIND_FUEL
      ELSE
         FIND_FUEL2: DO NS=1,N_TRACKED_SPECIES
            IF (RN%FUEL== SPECIES_MIXTURE(NS)%ID) THEN
               RN%FUEL_SMIX_INDEX = NS
               EXIT FIND_FUEL2
            ENDIF
         ENDDO FIND_FUEL2
      ENDIF
   ENDIF

   IF (TRIM(RN%FUEL)=='null') THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(190): REAC ',NR,' requires a FUEL.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   RN%C0_EXP = SUM(NU_Y)

   ! Set RN%N_S = NU for reactant species
   DO NS=1,N_SPECIES
      IF (NU_Y(NS) < 0._EB) THEN
         NU_Y(NS) = -NU_Y(NS)
      ELSE
         NU_Y(NS) = 0._EB
      ENDIF
   ENDDO

   DO NS=1,RN%N_SPEC_READ
      IF (TRIM(RN%SPEC_ID_N_S_READ(NS))=='null') CYCLE
      NAME_FOUND = .FALSE.
      DO NS2=1,N_SPECIES
         IF (TRIM(RN%SPEC_ID_N_S_READ(NS))==TRIM(SPECIES(NS2)%ID)) THEN
            NU_Y(NS2) = RN%N_S_READ(NS)
            NAME_FOUND = .TRUE.
            EXIT
         ENDIF
      ENDDO
      IF (.NOT. NAME_FOUND) THEN
         WRITE(MESSAGE,'(A,I0,A,A,A)') &
            'ERROR(204): REAC ',NR,'. Primitive species ',TRIM(RN%SPEC_ID_N_S_READ(NS)),' not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO

   RN%N_SPEC=0
   DO NS=1,N_SPECIES
      IF (ABS(NU_Y(NS)) > TWO_EPSILON_EB) RN%N_SPEC = RN%N_SPEC + 1
   ENDDO

   ALLOCATE(RN%N_S_INDEX(RN%N_SPEC))
   ALLOCATE(RN%N_S(RN%N_SPEC))

   NS2 = 0
   DO NS=1,N_SPECIES
      IF (ABS(NU_Y(NS)) > TWO_EPSILON_EB) THEN
         NS2 = NS2 + 1
         RN%N_S_INDEX(NS2) = NS
         RN%N_S(NS2) = NU_Y(NS)
      ENDIF
   ENDDO

   ! Normalize the stoichiometric coefficients by that of the fuel.
   RN%NU_FUEL_0 = -RN%NU(RN%FUEL_SMIX_INDEX)
   RN%NU = -RN%NU/RN%NU(RN%FUEL_SMIX_INDEX)

   ! Find AIR index

   GET_AIR_INDEX_LOOP: DO NS = 1,N_TRACKED_SPECIES
      IF (RN%NU(NS) < 0._EB .AND. NS /= RN%FUEL_SMIX_INDEX) THEN
         RN%AIR_SMIX_INDEX = NS
         EXIT GET_AIR_INDEX_LOOP
      ENDIF
   ENDDO GET_AIR_INDEX_LOOP

   ! Adjust mol/cm^3/s based rate to kg/m^3/s rate

   RN%RHO_EXPONENT = 0._EB

   DO NS=1,RN%N_SPEC
      ! FDS Tech Guide, Eq. (5.37), product term
      RN%A_PRIME      = RN%A_PRIME * (1000._EB*SPECIES(RN%N_S_INDEX(NS))%MW)**(-RN%N_S(NS))
      RN%RHO_EXPONENT = RN%RHO_EXPONENT + RN%N_S(NS)
   ENDDO

   RN%RHO_EXPONENT = RN%RHO_EXPONENT - 1._EB ! subtracting 1 accounts for division by rho in Eq. (5.40)
   RN%A_PRIME = RN%A_PRIME * 1000._EB*SPECIES_MIXTURE(RN%FUEL_SMIX_INDEX)%MW ! conversion terms in Eq. (5.37)

   ! Adjust mol/cm^3/s based rate to kg/m^3/s rate for FAST_CHEMISTRY (this will get removed when we overhaul combustion)
   ! Fictitious Arrhenius rate is dC_F/dt = -1E10*C_F*C_A

   IF (RN%FAST_CHEMISTRY) THEN
      IF (RN%AIR_SMIX_INDEX > -1) THEN
         RN%RHO_EXPONENT = 1._EB
         RN%A_PRIME = 1.E10_EB*(1000._EB*SPECIES_MIXTURE(RN%AIR_SMIX_INDEX)%MW)**(-1._EB)
      ELSE
         RN%RHO_EXPONENT = 0._EB
         RN%A_PRIME = 1.E10_EB
      ENDIF
   ENDIF

   ! Compute the primitive species reaction coefficients

   ALLOCATE(RN%NU_SPECIES(N_SPECIES))
   RN%NU_SPECIES = 0._EB
   DO NS=1,N_TRACKED_SPECIES
      SM => SPECIES_MIXTURE(NS)
      IF (.NOT. RN%SIMPLE_CHEMISTRY) RN%NU(NS) = RN%NU(NS)*SM%ADJUST_NU
      DO NS2 = 1,N_SPECIES
         RN%NU_SPECIES(NS2) =  RN%NU_SPECIES(NS2) + RN%NU(NS)*SM%VOLUME_FRACTION(NS2)
      ENDDO
      IF (SM%ID=='WATER VAPOR')  I_WATER = NS
   ENDDO

   ! Check atom balance of the reaction

   IF (.NOT. RN%SIMPLE_CHEMISTRY .AND. RN%CHECK_ATOM_BALANCE) THEN
      SKIP_ATOM_BALANCE = .FALSE.
      REACTION_BALANCE = 0._EB
      DO NS=1,N_TRACKED_SPECIES
         IF (ABS(RN%NU(NS))>TWO_EPSILON_EB .AND. .NOT. SPECIES_MIXTURE(NS)%VALID_ATOMS) SKIP_ATOM_BALANCE = .TRUE.
         REACTION_BALANCE = REACTION_BALANCE + RN%NU(NS)*SPECIES_MIXTURE(NS)%ATOMS
      ENDDO
      IF (ANY(ABS(REACTION_BALANCE)>REAC_ATOM_ERROR) .AND. .NOT. SKIP_ATOM_BALANCE) THEN
         CALL SHUTDOWN_ATOM(REACTION_BALANCE,NR,REAC_ATOM_ERROR)
         CALL SHUTDOWN(' ') ; RETURN
      ENDIF
   ENDIF

   ! Check the mass balance of the reaction

   MASS_REACTANT = 0._EB
   MASS_PRODUCT  = 0._EB

   DO NS=1,N_TRACKED_SPECIES
      IF (RN%NU(NS) < -TWO_EPSILON_EB) MASS_REACTANT = MASS_REACTANT + RN%NU(NS)*SPECIES_MIXTURE(NS)%MW
      IF (RN%NU(NS) >  TWO_EPSILON_EB) MASS_PRODUCT  = MASS_PRODUCT  + RN%NU(NS)*SPECIES_MIXTURE(NS)%MW
   ENDDO
   IF (ABS(MASS_PRODUCT) < TWO_EPSILON_EB .OR. ABS(MASS_REACTANT) < TWO_EPSILON_EB) THEN
      IF (ABS(MASS_PRODUCT) <TWO_EPSILON_EB) WRITE(MESSAGE,'(A,I0,A)') 'ERROR(205): REAC ',NR,'. Products not specified.'
      IF (ABS(MASS_REACTANT)<TWO_EPSILON_EB) WRITE(MESSAGE,'(A,I0,A)') 'ERROR(206): REAC ',NR,'. Reactants not specified.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (ABS(MASS_PRODUCT+MASS_REACTANT)/ABS(MASS_PRODUCT) > REAC_MASS_ERROR) THEN
      WRITE(MESSAGE,'(A,I0,A,F8.3,A,F8.3)') 'ERROR(207): REAC ',NR,'. Mass of products, ',MASS_PRODUCT, &
         ', does not equal mass of reactants,',-MASS_REACTANT
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Mass stoichiometric coefficient of oxidizer

   DO NS=1,N_TRACKED_SPECIES
      RN%NU_MW_O_MW_F(NS) = RN%NU(NS)*SPECIES_MIXTURE(NS)%MW/SPECIES_MIXTURE(RN%FUEL_SMIX_INDEX)%MW
      IF (RN%NU(NS)< 0._EB .AND. NS /= RN%FUEL_SMIX_INDEX) THEN
         RN%S = -RN%NU_MW_O_MW_F(NS)
      ENDIF
      IF (RN%NU(NS) > 0._EB) RN%PROD_SMIX_INDEX = NS
   ENDDO

   NS2 = 0
   DO NS=1,N_TRACKED_SPECIES
      IF (ABS(RN%NU_MW_O_MW_F(NS)) > TWO_EPSILON_EB) THEN
         NS2 = NS2 + 1
         RN%NU_INDEX(NS2) = NS
         RN%NU_MW_O_MW_F_FR(NS2) = RN%NU_MW_O_MW_F(NS)
      ENDIF
   ENDDO

   NS2 = 0
   DO NS=1,N_TRACKED_SPECIES
      IF (RN%NU(NS) < 0._EB) THEN
         NS2 = NS2 + 1
         RN%REACTANT_INDEX(NS2) = NS
      ENDIF
   ENDDO

   ! Set THIRD_BODY efficiencies
   IF (RN%N_THIRD>0) THEN
      ALLOCATE(RN%THIRD_EFF(N_SPECIES))
      RN%THIRD_EFF = 1._EB
      DO NS=1,RN%N_THIRD
         NAME_FOUND = .FALSE.
         THIRD1: DO NS2 = 1,N_SPECIES
            IF (RN%THIRD_EFF_ID_READ(NS)==SPECIES(NS2)%ID) THEN
               NAME_FOUND=.TRUE.
               RN%THIRD_EFF(NS2) = RN%THIRD_EFF_READ(NS)
               EXIT THIRD1
            ENDIF
         ENDDO THIRD1
         IF (.NOT. NAME_FOUND) THEN
            WRITE(MESSAGE,'(A,I0,A,A,A)') &
               'ERROR(208): REAC ',NR,'. THIRD_EFF primitive species ',TRIM(RN%THIRD_EFF_ID_READ(NS)),' not found.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDDO
   ENDIF

ENDDO REAC_LOOP

! Select integrator

IF (TRIM(ODE_SOLVER)/='null') THEN
   SELECT CASE (TRIM(ODE_SOLVER))
      CASE ('EXPLICIT EULER') ; COMBUSTION_ODE_SOLVER = EXPLICIT_EULER
      CASE ('RK2')            ; COMBUSTION_ODE_SOLVER = RK2
      CASE ('RK3')            ; COMBUSTION_ODE_SOLVER = RK3
      CASE ('RK2 RICHARDSON') ; COMBUSTION_ODE_SOLVER = RK2_RICHARDSON
      CASE DEFAULT
         WRITE(MESSAGE,'(A)') 'ERROR(209): Name of ODE_SOLVER is not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT
ELSE
   FAST_CHEM_LOOP: DO NR = 1,N_REACTIONS
      RN => REACTION(NR)
      IF (.NOT. RN%FAST_CHEMISTRY) THEN
         COMBUSTION_ODE_SOLVER = RK2_RICHARDSON
         EXIT FAST_CHEM_LOOP
      ELSE
         COMBUSTION_ODE_SOLVER = EXPLICIT_EULER
      ENDIF
   ENDDO FAST_CHEM_LOOP
ENDIF

END SUBROUTINE PROC_REAC_1


SUBROUTINE PROC_REAC_2

INTEGER :: NS,NR,HF_COUNT,J,IZ
REAL(EB) :: H_F_OLD(1:N_TRACKED_SPECIES),D_H_F
LOGICAL :: REDEFINE_H_F(1:N_TRACKED_SPECIES),LISTED_FUEL
TYPE (SPECIES_MIXTURE_TYPE), POINTER :: SM,SMF,SMF2
TYPE (SPECIES_TYPE), POINTER :: SS
TYPE (REACTION_TYPE), POINTER :: RN2

IF (N_REACTIONS <=0) RETURN

REDEFINE_H_F = .FALSE.
LISTED_FUEL = .FALSE.
H_F_OLD = SPECIES_MIXTURE%H_F
REAC_LOOP: DO NR=1,N_REACTIONS

   RN => REACTION(NR)
   SMF => SPECIES_MIXTURE(RN%FUEL_SMIX_INDEX)

   LISTED_FUEL = .FALSE.
   IF (SMF%H_F_HOC > -1.E21_EB) LISTED_FUEL = .TRUE.


   IF (RN%SIMPLE_CHEMISTRY) THEN
      IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS < 0) CYCLE
      DO NS=1,N_SPECIES
         IF (SMF%MASS_FRACTION(NS) > 0_EB .AND. .NOT. SPECIES(NS)%LISTED .AND. SPECIES(NS)%RADCAL_ID=='null') &
            SPECIES(NS)%RADCAL_ID=FUEL_RADCAL_ID
      ENDDO
   ENDIF

   HF_COUNT = 0
   DO NS = 1,N_TRACKED_SPECIES
      IF (RN%NU(NS) /= 0._EB) THEN
         IF (.NOT. NS==RN%FUEL_SMIX_INDEX .AND. .NOT. SPECIES_MIXTURE(NS)%H_F_HOC > -1.E21_EB) HF_COUNT = HF_COUNT +1
      ENDIF
   ENDDO

   SIMPLE_CHEMISTRY_IF: IF (.NOT. RN%SIMPLE_CHEMISTRY) THEN ! Complex chemistry reaction

      IF (RN%HEAT_OF_COMBUSTION > -1.E21) THEN ! User specified heat of combustion
         IF (HF_COUNT > 1) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(210): REAC ',NR,'. Missing an ENTHALPY_OF_FORMATION.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         ! Find heat of formation of lumped fuel to satisfy specified heat of combustion

         IF (REDEFINE_H_F(RN%FUEL_SMIX_INDEX)) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'WARNING: H_F for FUEL for REACtion ',NR,' was redefined multiple times.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         ENDIF
         REDEFINE_H_F(RN%FUEL_SMIX_INDEX) = .TRUE.
         SMF%H_F = RN%HEAT_OF_COMBUSTION * ABS(RN%NU(RN%FUEL_SMIX_INDEX)) * SMF%MW
         DO NS = 1,N_TRACKED_SPECIES
            IF  (NS == RN%FUEL_SMIX_INDEX) CYCLE
            SM=>SPECIES_MIXTURE(NS)
            SMF%H_F = SMF%H_F + RN%NU(NS) * SM%H_F * SM%MW
         ENDDO
         SMF%H_F = -SMF%H_F/ (RN%NU(RN%FUEL_SMIX_INDEX) * SMF%MW)
         IF (SMF%SINGLE_SPEC_INDEX>0) SPECIES(SMF%SINGLE_SPEC_INDEX)%H_F = SMF%H_F
      ELSE ! Use H_F_HOC values
         IF (HF_COUNT > 0 .OR. .NOT. LISTED_FUEL) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(210): REAC ',NR,'. Missing an ENTHALPY_OF_FORMATION.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         RN%HEAT_OF_COMBUSTION = 0._EB
         DO NS = 1,N_TRACKED_SPECIES
            SM=>SPECIES_MIXTURE(NS)
            RN%HEAT_OF_COMBUSTION = RN%HEAT_OF_COMBUSTION - RN%NU(NS) * SM%H_F_HOC * SM%MW
         ENDDO
         RN%HEAT_OF_COMBUSTION = -RN%HEAT_OF_COMBUSTION / (RN%NU(RN%FUEL_SMIX_INDEX)*SMF%MW)
      ENDIF
      RN%HOC_COMPLETE = RN%HEAT_OF_COMBUSTION

   ELSE SIMPLE_CHEMISTRY_IF ! Simple chemistry reaction

      ! Heat of Combustion calculation
      IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS == 1) THEN
         RN2 => REACTION(NR)
      ELSE
         RN2 => REACTION(RN%PAIR_INDEX)
         SMF2 => SPECIES_MIXTURE(RN2%FUEL_SMIX_INDEX)
      ENDIF

      HOC_IF: IF (RN%HEAT_OF_COMBUSTION > -1.E21) THEN ! User specified heat of combustion
         IF (IDEAL) THEN
            RN2%HOC_COMPLETE = RN2%HEAT_OF_COMBUSTION * SMF%MW !J/kg -> J/kmol
            RN2%HOC_COMPLETE = RN2%HOC_COMPLETE + ( &
                               RN2%NU_CO   * (SPECIES(CO2_INDEX)%H_F * MW_CO2 - SPECIES(CO_INDEX)%H_F * MW_CO) &
                             + RN2%NU_SOOT * SPECIES(CO2_INDEX)%H_F * SOOT_C_FRACTION * MW_CO2 &
                             + RN2%NU_SOOT * SPECIES(H2O_INDEX)%H_F * SOOT_H_FRACTION * 0.5_EB * MW_H2O &
                             + RN2%NU_HCN  * (SPECIES(CO2_INDEX)%H_F *MW_CO2 - SPECIES(HCN_INDEX)%H_F *MW_HCN) &
                             + RN2%NU_HCN  * (SPECIES(H2O_INDEX)%H_F*0.5_EB * MW_H2O - SPECIES(HCN_INDEX)%H_F) * MW_HCN)
            RN2%HOC_COMPLETE = RN2%HOC_COMPLETE / SMF%MW !J/kmol -> J/kg
         ELSE
            RN2%HOC_COMPLETE = RN2%HEAT_OF_COMBUSTION
         ENDIF
         ! Find heat of formation of lumped fuel to satisfy specified heat of combustion
         IF (REDEFINE_H_F(RN%FUEL_SMIX_INDEX)) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'WARNING: H_F for FUEL for REACtion ',NR,' was redefined multiple times.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         ENDIF
         REDEFINE_H_F(RN%FUEL_SMIX_INDEX) = .TRUE.
         SM => SPECIES_MIXTURE(1)
         IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==1) THEN
            RN%HEAT_OF_COMBUSTION = RN2%HOC_COMPLETE
            SMF%H_F = RN%HEAT_OF_COMBUSTION - RN%S*SM%H_F + (1._EB+RN%S)*SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%H_F
            IF (SMF%SINGLE_SPEC_INDEX>0) SPECIES(SMF%SINGLE_SPEC_INDEX)%H_F = SMF%H_F
            RN%EPUMO2 = RN%HEAT_OF_COMBUSTION * SMF%MW * RN%NU(RN%FUEL_SMIX_INDEX) / (RN%NU(1)*SM%MW*SM%MASS_FRACTION(O2_INDEX))
         ELSE
            RN%HOC_COMPLETE = RN2%HOC_COMPLETE
            RN2%HEAT_OF_COMBUSTION = SPECIES_MIXTURE(RN2%FUEL_SMIX_INDEX)%H_F + RN2%S*SM%H_F - &
                                     (1._EB+RN2%S)*SPECIES_MIXTURE(RN2%PROD_SMIX_INDEX)%H_F
            RN%HEAT_OF_COMBUSTION  = RN%HOC_COMPLETE - (1._EB+RN%S)*RN2%HEAT_OF_COMBUSTION
            SMF%H_F = RN%HEAT_OF_COMBUSTION - RN%S*SM%H_F + (1._EB+RN%S)*SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%H_F
            IF (SMF%SINGLE_SPEC_INDEX>0) SPECIES(SMF%SINGLE_SPEC_INDEX)%H_F = SMF%H_F
            RN%EPUMO2  = RN%HEAT_OF_COMBUSTION *SMF%MW  *RN%NU(RN%FUEL_SMIX_INDEX)  /(RN%NU(1) *SM%MW*SM%MASS_FRACTION(O2_INDEX))
            RN2%EPUMO2 = RN2%HEAT_OF_COMBUSTION*SMF2%MW *RN2%NU(RN2%FUEL_SMIX_INDEX)/(RN2%NU(1)*SM%MW*SM%MASS_FRACTION(O2_INDEX))
         ENDIF

      ELSE HOC_IF ! Heat of combustion not specified, use EPUMO2 or H_F is fuel is listed
         LISTED_FUEL_IF: IF (.NOT. LISTED_FUEL ) THEN
            SM => SPECIES_MIXTURE(1)
            RN2%HOC_COMPLETE = RN2%EPUMO2 * RN2%NU_O2 * SPECIES(O2_INDEX)%MW / SMF%MW
            IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==1) THEN
               RN%HEAT_OF_COMBUSTION = RN2%HOC_COMPLETE
               SMF%H_F = RN%HEAT_OF_COMBUSTION - RN%S*SM%H_F + (1._EB+RN%S)*SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%H_F
               IF (SMF%SINGLE_SPEC_INDEX>0) SPECIES(SMF%SINGLE_SPEC_INDEX)%H_F = SMF%H_F
            ELSE
               RN%HOC_COMPLETE = RN2%HOC_COMPLETE
               RN2%HEAT_OF_COMBUSTION = SPECIES_MIXTURE(RN2%FUEL_SMIX_INDEX)%H_F + RN2%S*SM%H_F - &
                                        (1._EB+RN2%S)*SPECIES_MIXTURE(RN2%PROD_SMIX_INDEX)%H_F
               RN%HEAT_OF_COMBUSTION  = RN%HOC_COMPLETE - (1._EB+RN%S)*RN2%HEAT_OF_COMBUSTION
               SMF%H_F = RN%HEAT_OF_COMBUSTION - RN%S*SM%H_F + (1._EB+RN%S)*SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%H_F
               IF (SMF%SINGLE_SPEC_INDEX>0) SPECIES(SMF%SINGLE_SPEC_INDEX)%H_F = SMF%H_F
               RN%EPUMO2  = RN%HEAT_OF_COMBUSTION *SMF%MW  *RN%NU(RN%FUEL_SMIX_INDEX)  /(RN%NU(1) *SM%MW*SM%MASS_FRACTION(O2_INDEX))
               RN2%EPUMO2 = RN2%HEAT_OF_COMBUSTION*SMF2%MW *RN2%NU(RN2%FUEL_SMIX_INDEX)/(RN2%NU(1)*SM%MW*SM%MASS_FRACTION(O2_INDEX))
            ENDIF
         ELSE LISTED_FUEL_IF ! Listed Fuel
            RN%HEAT_OF_COMBUSTION  = SMF%H_F_HOC+RN%S*SPECIES_MIXTURE(1)%H_F_HOC - &
                                     (1._EB+RN%S)*SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%H_F_HOC
            SM => SPECIES_MIXTURE(1)
            RN%EPUMO2 = RN%HEAT_OF_COMBUSTION*SMF%MW*RN%NU(RN%FUEL_SMIX_INDEX)/(RN%NU(1)*SM%MW*SM%MASS_FRACTION(O2_INDEX))
            IF (SMF%H_F_HOC /= SMF%H_F) THEN
               REDEFINE_H_F(RN%FUEL_SMIX_INDEX) = .TRUE.
               SMF%H_F = RN%HEAT_OF_COMBUSTION - RN%S*SM%H_F + (1._EB+RN%S)*SPECIES_MIXTURE(RN%PROD_SMIX_INDEX)%H_F
            ENDIF
            IF (RN%N_SIMPLE_CHEMISTRY_REACTIONS==2) THEN
               RN2%HEAT_OF_COMBUSTION=SMF2%H_F+RN2%S*SPECIES_MIXTURE(1)%H_F_HOC - &
                                      (1._EB+RN2%S)*SPECIES_MIXTURE(RN2%PROD_SMIX_INDEX)%H_F_HOC
               SM => SPECIES_MIXTURE(1)
               RN2%EPUMO2 = RN2%HEAT_OF_COMBUSTION*SMF2%MW*RN2%NU(RN2%FUEL_SMIX_INDEX)/(RN2%NU(1)*SM%MW*SM%MASS_FRACTION(O2_INDEX))
               RN%HOC_COMPLETE = RN%HEAT_OF_COMBUSTION + (1._EB+RN%S)*RN2%HEAT_OF_COMBUSTION
               RN2%HOC_COMPLETE = RN%HOC_COMPLETE
            ELSE
               RN%HOC_COMPLETE = RN%HEAT_OF_COMBUSTION
            ENDIF

         ENDIF LISTED_FUEL_IF
      ENDIF HOC_IF
   ENDIF SIMPLE_CHEMISTRY_IF

   ! Check if all tracked species have G_F for reversible reaction
   IF (RN%REVERSE) THEN
      DO NS = 1, N_TRACKED_SPECIES
         IF (ABS(RN%NU(NS)) > TWO_EPSILON_EB .AND. .NOT. SPECIES_MIXTURE(NS)%EXPLICIT_G_F) THEN
            WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(212): REAC ',NR,'. Reversible reaction species, ',&
                                          TRIM(SPECIES_MIXTURE(NS)%ID),' missing G_F.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDDO
      ALLOCATE(RN%DELTA_G(0:I_MAX_TEMP))
      DO J=0,I_MAX_TEMP
         ! kJ/mol -> J/kmol which is used for R0
         RN%DELTA_G(J) = -1.E6_EB*DOT_PRODUCT(G_F_Z(J,1:N_TRACKED_SPECIES),RN%NU*RN%NU_FUEL_0)/R0
      ENDDO
   ENDIF

   ! AIT exclusion zone control

   DO IZ=1,RN%N_AIT_EXCLUSION_ZONES
      IF (RN%AIT_EXCLUSION_ZONE(IZ)%DEVC_ID/='null' .OR. RN%AIT_EXCLUSION_ZONE(IZ)%CTRL_ID/='null') THEN
            CALL SEARCH_CONTROLLER('DEVC',RN%AIT_EXCLUSION_ZONE(IZ)%CTRL_ID,RN%AIT_EXCLUSION_ZONE(IZ)%DEVC_ID,&
               RN%AIT_EXCLUSION_ZONE(IZ)%DEVC_INDEX,RN%AIT_EXCLUSION_ZONE(IZ)%CTRL_INDEX,IZ)
      ENDIF
   ENDDO

ENDDO REAC_LOOP

! Correct CPBAR_Z array and liquid C_P_L_BAR and H_L arrays if H_F has changed
IF (ANY(REDEFINE_H_F)) THEN
   DO NS=1,N_TRACKED_SPECIES
      IF (.NOT.REDEFINE_H_F(NS)) CYCLE
      D_H_F = SPECIES_MIXTURE(NS)%H_F - H_F_OLD(NS)
      CPBAR_Z(0,NS) = CPBAR_Z(0,NS) + D_H_F
      DO J=1,I_MAX_TEMP
         CPBAR_Z(J,NS) = CPBAR_Z(J,NS) + D_H_F/REAL(J,EB)
      ENDDO
      IF (SPECIES_MIXTURE(NS)%SINGLE_SPEC_INDEX > 0) THEN
         SS => SPECIES(SPECIES_MIXTURE(NS)%SINGLE_SPEC_INDEX)
         IF (ALLOCATED(SS%C_P_L_BAR)) THEN
            SS%H_L = SS%H_L + D_H_F
            SS%C_P_L_BAR(0) = SS%H_L(0)
            DO J=1,I_MAX_TEMP
               SS%C_P_L_BAR(J) = SS%H_L(J)/REAL(J,EB)
            ENDDO
         ENDIF
      ENDIF
   ENDDO
ENDIF

DEALLOCATE(SIMPLE_FUEL_DEFINED)

END SUBROUTINE PROC_REAC_2


SUBROUTINE READ_PART

USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX,GET_TABLE_INDEX
USE DEVICE_VARIABLES, ONLY : PROPERTY_TYPE
USE RADCONS, ONLY : MIE_NDG
INTEGER :: SAMPLING_FACTOR,N,NN,NR,ILPC,IPC,RGB(3),N_STRATA,N_LAGRANGIAN_CLASSES_READ,ADHERE_TO_SOLID, &
           NEW_PARTICLE_INCREMENT
REAL(EB) :: DIAMETER, GAMMA_D,AGE,INITIAL_TEMPERATURE,HEAT_OF_COMBUSTION, &
            VERTICAL_VELOCITY,HORIZONTAL_VELOCITY,MAXIMUM_DIAMETER,MINIMUM_DIAMETER,SURFACE_DIAMETER,SIGMA_D, &
            SURFACE_TENSION,BREAKUP_RATIO,BREAKUP_GAMMA_D,BREAKUP_SIGMA_D,&
            DENSE_VOLUME_FRACTION,REAL_REFRACTIVE_INDEX,COMPLEX_REFRACTIVE_INDEX,RUNNING_AVERAGE_FACTOR,&
            RUNNING_AVERAGE_FACTOR_WALL,KILL_DIAMETER,&
            EMBER_DENSITY_THRESHOLD,EMBER_VELOCITY_THRESHOLD,PRIMARY_BREAKUP_LENGTH,&
            PRIMARY_BREAKUP_DRAG_REDUCTION_FACTOR,HEAT_TRANSFER_COEFFICIENT_GAS,HEAT_TRANSFER_COEFFICIENT_SOLID,&
            MASS_TRANSFER_COEFFICIENT
REAL(EB) :: DRAG_COEFFICIENT(3),FREE_AREA_FRACTION,PERMEABILITY(3),POROUS_VOLUME_FRACTION,SHAPE_FACTOR
REAL(EB), DIMENSION(3) :: ORIENTATION
CHARACTER(LABEL_LENGTH) :: SPEC_ID,DEVC_ID,CTRL_ID,QUANTITIES(1:10),QUANTITIES_SPEC_ID(1:10),SURF_ID,DRAG_LAW,PROP_ID,EVAP_MODEL, &
                 RADIATIVE_PROPERTY_TABLE='null',CNF_RAMP_ID='null',BREAKUP_CNF_RAMP_ID='null',DISTRIBUTION,BREAKUP_DISTRIBUTION
CHARACTER(25) :: COLOR
LOGICAL :: TARGET_ONLY,MASSLESS,STATIC,MONODISPERSE,BREAKUP,CHECK_DISTRIBUTION,DEBUG,&
           TURBULENT_DISPERSION,EMBER_PARTICLE,TRACK_EMBERS
TYPE(LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()
NAMELIST /PART/ ADHERE_TO_SOLID,AGE,BREAKUP,BREAKUP_CNF_RAMP_ID,BREAKUP_DISTRIBUTION,BREAKUP_GAMMA_D,BREAKUP_RATIO,&
                BREAKUP_SIGMA_D,CHECK_DISTRIBUTION,CNF_RAMP_ID,COLOR,COMPLEX_REFRACTIVE_INDEX,&
                CTRL_ID,DEBUG,DENSE_VOLUME_FRACTION,&
                DEVC_ID,DIAMETER,DISTRIBUTION,DRAG_COEFFICIENT,DRAG_LAW,&
                EMBER_DENSITY_THRESHOLD,EMBER_PARTICLE,EMBER_VELOCITY_THRESHOLD,EVAP_MODEL,&
                FREE_AREA_FRACTION,FYI,GAMMA_D,HEAT_OF_COMBUSTION,HEAT_TRANSFER_COEFFICIENT_GAS,HEAT_TRANSFER_COEFFICIENT_SOLID,&
                HORIZONTAL_VELOCITY,ID,INITIAL_TEMPERATURE,KILL_DIAMETER,MASSLESS,&
                MASS_TRANSFER_COEFFICIENT,MAXIMUM_DIAMETER,&
                MINIMUM_DIAMETER,MONODISPERSE,&
                N_STRATA,NEW_PARTICLE_INCREMENT,ORIENTATION,PERMEABILITY,POROUS_VOLUME_FRACTION,&
                PRIMARY_BREAKUP_DRAG_REDUCTION_FACTOR,PRIMARY_BREAKUP_LENGTH,PROP_ID,QUANTITIES,&
                QUANTITIES_SPEC_ID,RADIATIVE_PROPERTY_TABLE,REAL_REFRACTIVE_INDEX,RGB,RUNNING_AVERAGE_FACTOR,&
                RUNNING_AVERAGE_FACTOR_WALL, &
                SAMPLING_FACTOR,SHAPE_FACTOR,SIGMA_D,SPEC_ID,STATIC,&
                SURFACE_DIAMETER,SURFACE_TENSION,SURF_ID,TARGET_ONLY,TURBULENT_DISPERSION,TRACK_EMBERS,VERTICAL_VELOCITY

! Determine total number of PART lines in the input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
N_LAGRANGIAN_CLASSES = 0
N_LAGRANGIAN_CLASSES_READ = 0

COUNT_PART_LOOP: DO
   CALL CHECKREAD('PART',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_PART_LOOP
   READ(LU_INPUT,PART,END=219,ERR=220,IOSTAT=IOS)
   N_LAGRANGIAN_CLASSES_READ = N_LAGRANGIAN_CLASSES_READ + 1
   220 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with PART line.') ; RETURN ; ENDIF
ENDDO COUNT_PART_LOOP
219 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

N_LAGRANGIAN_CLASSES = N_LAGRANGIAN_CLASSES_READ

! Add reserved INIT lines to account for devices for 'RADIATIVE HEAT FLUX GAS' or 'ADIABATIC SURFACE TEMPERATURE GAS'

IF (TARGET_PARTICLES_INCLUDED) N_LAGRANGIAN_CLASSES = N_LAGRANGIAN_CLASSES + 1

! Allocate the derived type array to hold information about the particle classes

IF (N_LAGRANGIAN_CLASSES>0) PARTICLE_FILE = .TRUE.
ALLOCATE(LAGRANGIAN_PARTICLE_CLASS(N_LAGRANGIAN_CLASSES),STAT=IZERO)
CALL ChkMemErr('READ','N_LAGRANGIAN_CLASSES',IZERO)

N_LP_ARRAY_INDICES = 0
IPC = 0
ILPC = 0

READ_PART_LOOP: DO N=1,N_LAGRANGIAN_CLASSES

   ! Read the PART line from the input file or set up special PARTICLE_CLASS class for water PARTICLEs or tracers

   IF (N<=N_LAGRANGIAN_CLASSES_READ) THEN

      CALL CHECKREAD('PART',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT READ_PART_LOOP
      CALL SET_PART_DEFAULTS
      READ(LU_INPUT,PART)

   ELSEIF (TARGET_PARTICLES_INCLUDED) THEN

      ! Create a class of particles that is just a target

      CALL SET_PART_DEFAULTS
      WRITE(ID,'(A)') 'RESERVED TARGET PARTICLE'
      TARGET_ONLY = .TRUE.
      STATIC = .TRUE.
      ORIENTATION(1:3) = (/1._EB , 0._EB , 0._EB/)  ! This is just a dummy orientation

   ENDIF

   LPC => LAGRANGIAN_PARTICLE_CLASS(N)

   ! Identify the different types of Lagrangian particles, like massless tracers, targets, droplets, etc.

   IF (SURF_ID/='null') THEN
      SOLID_PARTICLES = .TRUE.
      IF (CNF_RAMP_ID=='null') MONODISPERSE = .TRUE.
      LPC%SOLID_PARTICLE = .TRUE.
      IF (SAMPLING_FACTOR<=0) SAMPLING_FACTOR = 1
      IF (DIAMETER>0._EB) THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(213): PART ',N,' cannot have both a specified DIAMETER and a SURF_ID.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF

   IF (TARGET_ONLY) THEN
      LPC%MASSLESS_TARGET = .TRUE.
      IF (SPEC_ID/='null') THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(XX): PART ',N,' cannot use SPEC_ID with TARGET_ONLY.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (N<=N_LAGRANGIAN_CLASSES_READ .AND. SURF_ID=='null') THEN
         IF (N<=N_LAGRANGIAN_CLASSES_READ) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(XX): PART ',N,' TARGET_ONLY requires a SURF_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF
      IF (N>N_LAGRANGIAN_CLASSES_READ) SURF_ID  = 'MASSLESS TARGET'
      SOLID_PARTICLES = .TRUE.
      IF (CNF_RAMP_ID=='null') MONODISPERSE = .TRUE.
      STATIC = .TRUE.
      IF (SAMPLING_FACTOR<=0) SAMPLING_FACTOR = 1
   ENDIF

   IF (SPEC_ID/='null') THEN
      SURF_ID = 'DROPLET'
      LPC%LIQUID_DROPLET = .TRUE.
      IF (ADHERE_TO_SOLID==0) ADHERE_TO_SOLID = 1
      IF (SAMPLING_FACTOR<=0) SAMPLING_FACTOR = 10
      IF (DIAMETER<=0._EB .AND. CNF_RAMP_ID=='null') THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(214): PART ',N,' requires a specified DIAMETER.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (MASSLESS) THEN
         WRITE(MESSAGE,'(A)') 'ERROR(215): Cannot have MASSLESS=.TRUE. with evaporating PARTICLEs'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF

   IF (MASSLESS) THEN
      LPC%MASSLESS_TRACER = .TRUE.
      LPC%INCLUDE_BOUNDARY_PROP1_TYPE = .FALSE.
      DIAMETER = 0._EB
      SURF_ID  = 'MASSLESS TRACER'
      IF (SAMPLING_FACTOR<=0) SAMPLING_FACTOR = 1
   ENDIF

   ! If particle class has no ID at this point, stop.

   IF (SURF_ID=='null') THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(216): PART ',N,' needs a SURF_ID.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Set default colors for Smokeview. Water droplets are BLUE. Fuel droplets are YELLOW. Everything else is BLACK.

   IF (TRIM(SPEC_ID)=='WATER VAPOR') THEN
      IF (ANY(RGB<0) .AND. COLOR=='null') COLOR='SKY BLUE 5'
   ENDIF

   DO NR=1,N_REACTIONS
      IF (REACTION(NR)%SIMPLE_CHEMISTRY) THEN
         IF(TRIM(SPEC_ID)==TRIM(REACTION(NR)%FUEL)) THEN
            IF (ANY(RGB<0) .AND. COLOR=='null') COLOR='YELLOW'
         ENDIF
      ENDIF
   ENDDO

   IF (ANY(RGB<0) .AND. COLOR=='null') COLOR = 'BLACK'

   IF (COLOR /= 'null') CALL COLOR2RGB(RGB,COLOR)

   ! Determine if the SPEC_ID is OK

   LPC%SPEC_ID = SPEC_ID
   IF (LPC%LIQUID_DROPLET) THEN
      DO NN=1,N_TRACKED_SPECIES
         IF (TRIM(SPECIES_MIXTURE(NN)%ID)==TRIM(LPC%SPEC_ID)) THEN
            LPC%Z_INDEX = NN
            SPECIES_MIXTURE(NN)%EVAPORATING = .TRUE.
            EXIT
         ENDIF
      ENDDO
      IF(LPC%Z_INDEX < 0) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(217): PART SPEC_ID ',TRIM(LPC%SPEC_ID),' not found'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SPECIES_MIXTURE(LPC%Z_INDEX)%SINGLE_SPEC_INDEX < 0) THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(218): PART ',N,'.  Particles cannot evaporate to a lumped species.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ELSE
         LPC%Y_INDEX = SPECIES_MIXTURE(LPC%Z_INDEX)%SINGLE_SPEC_INDEX
      ENDIF
      IF (SPECIES(LPC%Y_INDEX)%DENSITY_LIQUID > 0._EB) LPC%DENSITY=SPECIES(LPC%Y_INDEX)%DENSITY_LIQUID

   ENDIF

   ! Arrays for particle size distribution

   IF (MONODISPERSE) THEN
      LPC%N_STRATA = 1
   ELSE
      LPC%N_STRATA = N_STRATA
   ENDIF

   IF (DIAMETER > 0._EB .OR. CNF_RAMP_ID/='null') THEN
      ALLOCATE(LPC%CNF(0:NDC),STAT=IZERO)
      CALL ChkMemErr('READ','CNF',IZERO)
      ALLOCATE(LPC%CVF(0:NDC),STAT=IZERO)
      CALL ChkMemErr('READ','CVF',IZERO)
      ALLOCATE(LPC%R_CNF(0:NDC),STAT=IZERO)
      CALL ChkMemErr('READ','R_CNF',IZERO)
      ALLOCATE(LPC%STRATUM_INDEX_LOWER(LPC%N_STRATA),STAT=IZERO)
      CALL ChkMemErr('READ','STRATUM_INDEX_LOWER',IZERO)
      ALLOCATE(LPC%STRATUM_INDEX_UPPER(LPC%N_STRATA),STAT=IZERO)
      CALL ChkMemErr('READ','STRATUM_INDEX_UPPER',IZERO)
      ALLOCATE(LPC%W_CNF(LPC%N_STRATA),STAT=IZERO)
      CALL ChkMemErr('READ','W_CNF',IZERO)
   ENDIF

   ! Arrays related to particle break-up model

   IF (BREAKUP) THEN
      ALLOCATE(LPC%BREAKUP_CNF(0:NDC),STAT=IZERO)
      CALL ChkMemErr('READ','BREAKUP_CNF',IZERO)
      ALLOCATE(LPC%BREAKUP_R_CNF(0:NDC),STAT=IZERO)
      CALL ChkMemErr('READ','BREAKUP_R_CNF',IZERO)
      ALLOCATE(LPC%BREAKUP_CVF(0:NDC),STAT=IZERO)
      CALL ChkMemErr('READ','BREAKUP_CVF',IZERO)
   ENDIF

   ! Radiative property table

   IF (RADIATIVE_PROPERTY_TABLE /= 'null') THEN
      CALL GET_TABLE_INDEX(RADIATIVE_PROPERTY_TABLE,PART_RADIATIVE_PROPERTY,LPC%RADIATIVE_PROPERTY_INDEX)
      LPC%RADIATIVE_PROPERTY_TABLE_ID = RADIATIVE_PROPERTY_TABLE
   ELSE
      LPC%RADIATIVE_PROPERTY_INDEX = 0
   ENDIF

   ! Assign property data to LAGRANGIAN_PARTICLE_CLASS class

   LPC%ID                               = ID
   LPC%BREAKUP                          = BREAKUP
   LPC%BREAKUP_RATIO                    = BREAKUP_RATIO
   LPC%BREAKUP_GAMMA                    = BREAKUP_GAMMA_D
   IF ( BREAKUP_SIGMA_D > 0._EB ) THEN
      LPC%BREAKUP_SIGMA                 = BREAKUP_SIGMA_D
   ELSE
      ! per tech guide, sigma*gamma=1.15 smoothly joins Rosin-Rammler and lognormal distribustions
      LPC%BREAKUP_SIGMA                 = 1.15_EB/BREAKUP_GAMMA_D
   ENDIF
   LPC%CTRL_ID                          = CTRL_ID
   LPC%DENSE_VOLUME_FRACTION            = DENSE_VOLUME_FRACTION
   LPC%DEVC_ID                          = DEVC_ID
   LPC%TMP_INITIAL                      = INITIAL_TEMPERATURE + TMPM
   LPC%SAMPLING_FACTOR                  = SAMPLING_FACTOR
   LPC%RGB                              = RGB
   LPC%DEBUG                            = DEBUG
   LPC%DIAMETER                         = DIAMETER*1.E-6_EB
   LPC%MEAN_DROPLET_VOLUME              = FOTHPI*(0.5_EB*LPC%DIAMETER)**3 ! recomputed for distributions
   LPC%MAXIMUM_DIAMETER                 = MAXIMUM_DIAMETER*1.E-6_EB
   IF (MINIMUM_DIAMETER<0._EB) THEN
      LPC%MINIMUM_DIAMETER              = 0.005_EB*LPC%DIAMETER
   ELSE
      LPC%MINIMUM_DIAMETER              = MINIMUM_DIAMETER*1.E-6_EB
   ENDIF
   IF (LPC%MINIMUM_DIAMETER > 0._EB .AND. LPC%MINIMUM_DIAMETER < 0.01E-6_EB) THEN
      WRITE(MESSAGE,'(A,A,A)') 'WARNING: PART ID ',TRIM(LPC%ID),&
         '. MINIMUM_DIAMETER is < 0.01 micron; numerical issues may result.'
      IF (MY_RANK==0) WRITE(LU_ERR,'(A)') MESSAGE
   ENDIF
   LPC%KILL_RADIUS                      = 0.5_EB*KILL_DIAMETER*1.E-6
   IF (LPC%LIQUID_DROPLET) THEN !Set KILL_RADIUS for SURF_ID in PROC_PART
      IF (CNF_RAMP_ID=='null' .AND. LPC%DIAMETER < 0._EB) THEN
         WRITE(MESSAGE,'(A,A)') 'Liquid droplet needs a DIAMETER or CNF_RAMP_ID for particle class: ',LPC%ID
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (LPC%KILL_RADIUS<0._EB .AND. CNF_RAMP_ID=='null') THEN
         IF (MONODISPERSE) THEN         ! Kill if volume of droplet <= 0.005*volume of droplet with DIAMETER or MINIMUM_DIAMETER
            LPC%KILL_RADIUS             = (0.005_EB*(0.5_EB*LPC%DIAMETER)**3)**ONTH
         ELSE
            LPC%KILL_RADIUS             = (0.005_EB*(0.5_EB*LPC%MINIMUM_DIAMETER)**3)**ONTH
         ENDIF
      ELSE
         IF (.NOT. MONODISPERSE .AND. KILL_DIAMETER >= LPC%MINIMUM_DIAMETER) THEN
            WRITE(MESSAGE,'(A,A)') 'KILL DIAMETER >= MINIMUM_DIAMETER for particle class ',LPC%ID
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (MONODISPERSE .AND. KILL_DIAMETER >= DIAMETER) THEN
            WRITE(MESSAGE,'(A,A)') 'KILL DIAMETER >= DIAMETER for particle class ',LPC%ID
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF
   ENDIF
   LPC%MONODISPERSE                     = MONODISPERSE
   IF (NEW_PARTICLE_INCREMENT>0) LPC%NEW_PARTICLE_INCREMENT = NEW_PARTICLE_INCREMENT
   LPC%PROP_ID                          = PROP_ID
   LPC%QUANTITIES                       = QUANTITIES
   LPC%QUANTITIES_SPEC_ID               = QUANTITIES_SPEC_ID
   LPC%GAMMA                            = GAMMA_D
   IF ( SIGMA_D > 0._EB ) THEN
      LPC%SIGMA                         = SIGMA_D
   ELSE
      LPC%SIGMA                         = 1.15_EB/GAMMA_D
   END IF
   LPC%DISTRIBUTION                     = DISTRIBUTION
   LPC%CHECK_DISTRIBUTION               = CHECK_DISTRIBUTION
   LPC%BREAKUP_DISTRIBUTION             = BREAKUP_DISTRIBUTION
   LPC%CNF_RAMP_ID                      = CNF_RAMP_ID
   LPC%BREAKUP_CNF_RAMP_ID              = BREAKUP_CNF_RAMP_ID

   IF(LPC%CNF_RAMP_ID/='null') THEN
        CALL GET_RAMP_INDEX(LPC%CNF_RAMP_ID,'DIAMETER',LPC%CNF_RAMP_INDEX)
   ENDIF
   IF(LPC%BREAKUP_CNF_RAMP_ID/='null') THEN
        CALL GET_RAMP_INDEX(LPC%BREAKUP_CNF_RAMP_ID,'DIAMETER',LPC%BREAKUP_CNF_RAMP_INDEX)
   ENDIF

   LPC%REAL_REFRACTIVE_INDEX = REAL_REFRACTIVE_INDEX
   LPC%COMPLEX_REFRACTIVE_INDEX = COMPLEX_REFRACTIVE_INDEX
   IF (LPC%REAL_REFRACTIVE_INDEX <= 0._EB .OR. LPC%COMPLEX_REFRACTIVE_INDEX < 0._EB) THEN
      WRITE(MESSAGE,'(A,A)') 'Bad refractive index on PART line ',LPC%ID
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   LPC%HEAT_OF_COMBUSTION = HEAT_OF_COMBUSTION*1000._EB
   LPC%FTPR               = FOTHPI*LPC%DENSITY
   LPC%LIFETIME           = AGE
   LPC%TURBULENT_DISPERSION = TURBULENT_DISPERSION
   LPC%STATIC             = STATIC
   LPC%SPEC_ID            = SPEC_ID
   LPC%SURF_ID            = SURF_ID
   LPC%SURF_INDEX         = -1
   LPC%SURFACE_DIAMETER   = SURFACE_DIAMETER*1.E-6_EB
   LPC%SURFACE_TENSION    = SURFACE_TENSION
   LPC%ADJUST_EVAPORATION  = 1._EB   ! If H_O_C>0. this parameter will have to be reset later
   LPC%MASS_TRANSFER_COEFFICIENT = MASS_TRANSFER_COEFFICIENT
   LPC%HEAT_TRANSFER_COEFFICIENT_GAS = HEAT_TRANSFER_COEFFICIENT_GAS
   LPC%HEAT_TRANSFER_COEFFICIENT_SOLID = HEAT_TRANSFER_COEFFICIENT_SOLID
   IF (ADHERE_TO_SOLID==1) THEN
      LPC%ADHERE_TO_SOLID = .TRUE.
   ELSE
      LPC%ADHERE_TO_SOLID = .FALSE.
   ENDIF
   LPC%VERTICAL_VELOCITY   = VERTICAL_VELOCITY
   LPC%HORIZONTAL_VELOCITY = HORIZONTAL_VELOCITY
   LPC%DRAG_COEFFICIENT    = DRAG_COEFFICIENT
   LPC%SHAPE_FACTOR        = SHAPE_FACTOR
   LPC%EMBER_PARTICLE      = EMBER_PARTICLE
   LPC%TRACK_EMBERS        = TRACK_EMBERS
   LPC%EMBER_DENSITY_THRESHOLD = EMBER_DENSITY_THRESHOLD
   LPC%EMBER_VELOCITY_THRESHOLD = EMBER_VELOCITY_THRESHOLD
   LPC%PRIMARY_BREAKUP_TIME = PRIMARY_BREAKUP_LENGTH ! user enters LENGTH, later divide by PARTICLE_VELOCITY to get TIME
   LPC%PRIMARY_BREAKUP_DRAG_REDUCTION_FACTOR = PRIMARY_BREAKUP_DRAG_REDUCTION_FACTOR

   ! Count and process the number of orientations for the particle

   LPC%N_ORIENTATION = 0

   IF (ANY(ABS(ORIENTATION(1:3))>TWO_EPSILON_EB)) LPC%N_ORIENTATION = LPC%N_ORIENTATION + 1

   IF (LPC%N_ORIENTATION>0) THEN
      LPC%INCLUDE_BOUNDARY_RADIA_TYPE = .TRUE.
      N_ORIENTATION_VECTOR = N_ORIENTATION_VECTOR + 1
      LPC%ORIENTATION_INDEX = N_ORIENTATION_VECTOR
      IF (N_ORIENTATION_VECTOR>UBOUND(ORIENTATION_VECTOR,DIM=2)) THEN
         ORIENTATION_VECTOR => REALLOCATE2D(ORIENTATION_VECTOR,1,3,0,N_ORIENTATION_VECTOR+10)
      ENDIF
      ORIENTATION_VECTOR(1:3,N_ORIENTATION_VECTOR) = ORIENTATION(1:3)/ NORM2(ORIENTATION)
   ENDIF
   LPC%FREE_AREA_FRACTION = FREE_AREA_FRACTION
   LPC%POROUS_VOLUME_FRACTION = POROUS_VOLUME_FRACTION

   ! Drag laws

   IF (ANY(DRAG_COEFFICIENT>0._EB) .AND. (DRAG_LAW=='SPHERE' .OR.  DRAG_LAW=='CYLINDER')) THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(219): PART line ',N,'. Do not specify a DRAG_COEFFICIENT for a SPHERE or CYLINDER DRAG_LAW'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (DRAG_COEFFICIENT(1)>=0._EB .AND. DRAG_LAW=='null') DRAG_LAW = 'USER'

   SELECT CASE(DRAG_LAW)
      CASE('SPHERE','null')
         LPC%DRAG_LAW = SPHERE_DRAG
      CASE('CYLINDER')
         LPC%DRAG_LAW = CYLINDER_DRAG
      CASE('DISK')
         LPC%DRAG_LAW = DISK_DRAG
      CASE('USER')
         LPC%DRAG_LAW = USER_DRAG
      CASE('SCREEN')
         IF (LPC%N_ORIENTATION/=1) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(220): PART ',N,'. Must specify exactly one ORIENTATION for SCREEN drag law.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (LPC%FREE_AREA_FRACTION < 0._EB) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(221): PART ',N,'. Must specify FREE_AREA_FRACTION for SCREEN drag law.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         LPC%DRAG_LAW = SCREEN_DRAG
         LPC%PERMEABILITY(1:3) = 3.44E-9_EB*LPC%FREE_AREA_FRACTION**1.6_EB
         LPC%DRAG_COEFFICIENT(1:3) = 4.30E-2_EB*LPC%FREE_AREA_FRACTION**2.13_EB
      CASE('POROUS MEDIA')
         IF (ANY(DRAG_COEFFICIENT<TWO_EPSILON_EB) .OR. ANY(PERMEABILITY<TWO_EPSILON_EB)) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(222): PART ',N,'. Specify all compoents for DRAG_COEFFICIENT and PERMEABILTIY.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         LPC%DRAG_LAW = POROUS_DRAG
         LPC%PERMEABILITY = PERMEABILITY
      CASE DEFAULT
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(223): PART ',N,'. Unrecognized drag law.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT

   ! Determine the number of slots to create in the particle evaporation and radiation arrays

   IF (LPC%LIQUID_DROPLET .OR. LPC%SOLID_PARTICLE) THEN
      PARTICLE_DRAG = .TRUE.
      N_LP_ARRAY_INDICES = N_LP_ARRAY_INDICES + 1
      LPC%ARRAY_INDEX =  N_LP_ARRAY_INDICES
      LPC%RUNNING_AVERAGE_FACTOR = RUNNING_AVERAGE_FACTOR
      LPC%RUNNING_AVERAGE_FACTOR_WALL = RUNNING_AVERAGE_FACTOR_WALL
      IF (LPC%SOLID_PARTICLE .AND. RUNNING_AVERAGE_FACTOR<0._EB) LPC%RUNNING_AVERAGE_FACTOR = 0.0_EB
      IF (LPC%LIQUID_DROPLET .AND. RUNNING_AVERAGE_FACTOR<0._EB) LPC%RUNNING_AVERAGE_FACTOR = 0.5_EB
      IF (LPC%SOLID_PARTICLE .AND. RUNNING_AVERAGE_FACTOR_WALL<0._EB) LPC%RUNNING_AVERAGE_FACTOR_WALL = 0.0_EB
      IF (LPC%LIQUID_DROPLET .AND. RUNNING_AVERAGE_FACTOR_WALL<0._EB) LPC%RUNNING_AVERAGE_FACTOR_WALL = 0.5_EB
   ENDIF

   SELECT CASE (EVAP_MODEL)
      CASE('RANZ-MARSHALL NO B-NUMBER')
         LPC%EVAP_MODEL = RM_NO_B
      CASE('RANZ-MARSHALL B-NUMBER')
         LPC%EVAP_MODEL = RM_B
      CASE('RANZ-MARSHALL LEWIS B-NUMBER')
         LPC%EVAP_MODEL = RM_LEWIS_B
      CASE('RANZ-MARSHALL FLUX-LIMITED LEWIS B-NUMBER')
         LPC%EVAP_MODEL = RM_FL_LEWIS_B
      CASE DEFAULT
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(224): PART line ',N,'. Invalid EVAP_MODEL.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT

ENDDO READ_PART_LOOP

! Allocate radiation arrays

PLOOP2: DO ILPC=1,N_LAGRANGIAN_CLASSES
   LPC=>LAGRANGIAN_PARTICLE_CLASS(ILPC)
   IF (LPC%LIQUID_DROPLET) THEN
      ALLOCATE(LPC%WQABS(0:MIE_NDG,1:NUMBER_SPECTRAL_BANDS))
      CALL ChkMemErr('INIT','WQABS',IZERO)
      LPC%WQABS = 0._EB
      ALLOCATE(LPC%WQSCA(0:MIE_NDG,1:NUMBER_SPECTRAL_BANDS))
      CALL ChkMemErr('INIT','WQSCA',IZERO)
      LPC%WQSCA = 0._EB
      ALLOCATE(LPC%R50(0:MIE_NDG))
      CALL ChkMemErr('INIT','R50',IZERO)
      LPC%R50 = 0._EB
   ENDIF
ENDDO PLOOP2

! Determine output quantities

DO ILPC=1,N_LAGRANGIAN_CLASSES
   LPC=>LAGRANGIAN_PARTICLE_CLASS(ILPC)
   LPC%N_QUANTITIES = 0
   IF (ANY(LPC%QUANTITIES/='null')) THEN
      QUANTITIES_LOOP: DO N=1,10
         IF (LPC%QUANTITIES(N)=='null') CYCLE QUANTITIES_LOOP
         LPC%N_QUANTITIES = LPC%N_QUANTITIES + 1
         CALL GET_QUANTITY_INDEX(LPC%SMOKEVIEW_LABEL(LPC%N_QUANTITIES),LPC%SMOKEVIEW_BAR_LABEL(LPC%N_QUANTITIES), &
                                 LPC%QUANTITIES_INDEX(LPC%N_QUANTITIES),I_DUM(1), &
                                 LPC%QUANTITIES_Y_INDEX(LPC%N_QUANTITIES),LPC%QUANTITIES_Z_INDEX(LPC%N_QUANTITIES),&
                                 I_DUM(4),I_DUM(5),I_DUM(6),I_DUM(7),I_DUM(8),'PART', &
                                 LPC%QUANTITIES(N),'null',LPC%QUANTITIES_SPEC_ID(N),'null','null','null','null','null',&
                                 -1._EB,I_DUM(9))
      ENDDO QUANTITIES_LOOP
   ENDIF
ENDDO

CONTAINS


SUBROUTINE SET_PART_DEFAULTS

BREAKUP                  = .FALSE.
BREAKUP_RATIO            = 3._EB/7._EB  ! ratio of child Sauter mean to parent size in Bag breakup regime
BREAKUP_GAMMA_D          = 2.4_EB
BREAKUP_SIGMA_D          = -99999.9_EB
CTRL_ID                  = 'null'
DEBUG                    = .FALSE.
DENSE_VOLUME_FRACTION    = 1.E-5_EB     ! Limiting volume fraction for drag reduction
DEVC_ID                  = 'null'
EVAP_MODEL               = 'RANZ-MARSHALL B-NUMBER'
INITIAL_TEMPERATURE      = -TMPM - 1._EB
MASS_TRANSFER_COEFFICIENT = -1._EB   ! kg/m2/s
HEAT_TRANSFER_COEFFICIENT_GAS = -1._EB   ! W/m2/K
HEAT_TRANSFER_COEFFICIENT_SOLID = -1._EB ! W/m2/K
HEAT_OF_COMBUSTION       = -1._EB       ! kJ/kg
DIAMETER                 = -1._EB
MAXIMUM_DIAMETER         = 1.E9_EB      ! microns, sets the largest particle generated when using a size distribution
MINIMUM_DIAMETER         = -1._EB       ! microns, sets the smallest particle generated when using a size distribution
KILL_DIAMETER            = -1._EB       ! microns, sets the diameter where a particle is killed
MONODISPERSE             = .FALSE.
N_STRATA                 = 6
GAMMA_D                  = 2.4_EB
SIGMA_D                  = -99999.9_EB
AGE                      = 1.E6_EB      ! s
ID                       = 'null'
PROP_ID                  = 'null'
ORIENTATION              = 0._EB
QUANTITIES               = 'null'
QUANTITIES_SPEC_ID       = 'null'
RADIATIVE_PROPERTY_TABLE = 'null'
RGB                      = -1
SPEC_ID                  = 'null'
SURF_ID                  = 'null'
SURFACE_DIAMETER         = -1._EB
SURFACE_TENSION          = 72.8E-3_EB  ! N/m, applies for water
COLOR                    = 'null'
SAMPLING_FACTOR          = -1
STATIC                   = .FALSE.
MASSLESS                 = .FALSE.
TARGET_ONLY              = .FALSE.
TURBULENT_DISPERSION     = .FALSE.
REAL_REFRACTIVE_INDEX    = 1.33_EB
RUNNING_AVERAGE_FACTOR   = -1._EB
RUNNING_AVERAGE_FACTOR_WALL = -1._EB
COMPLEX_REFRACTIVE_INDEX = 0.01_EB
ADHERE_TO_SOLID          = 0
VERTICAL_VELOCITY        = 0.5_EB
HORIZONTAL_VELOCITY      = 0.2_EB
DRAG_LAW                 = 'null'
DRAG_COEFFICIENT         = -1._EB
PERMEABILITY             = -1._EB
DISTRIBUTION             = 'ROSIN-RAMMLER-LOGNORMAL'
CNF_RAMP_ID              = 'null'
CHECK_DISTRIBUTION       = .FALSE.
BREAKUP_DISTRIBUTION     = 'ROSIN-RAMMLER-LOGNORMAL'
BREAKUP_CNF_RAMP_ID      = 'null'
FREE_AREA_FRACTION       = 0.5_EB
SHAPE_FACTOR             = 0.25_EB
EMBER_PARTICLE           = .FALSE.
TRACK_EMBERS             = .TRUE.
EMBER_DENSITY_THRESHOLD  = 0._EB
EMBER_VELOCITY_THRESHOLD = 1000._EB
PRIMARY_BREAKUP_LENGTH   = -1._EB
PRIMARY_BREAKUP_DRAG_REDUCTION_FACTOR = 1._EB
POROUS_VOLUME_FRACTION = -1._EB
FREE_AREA_FRACTION = -1._EB
NEW_PARTICLE_INCREMENT = -1
TARGET_ONLY              = .FALSE.

END SUBROUTINE SET_PART_DEFAULTS

END SUBROUTINE READ_PART


SUBROUTINE PROC_PART

INTEGER :: N,NN
REAL(EB) :: AREA_FACTOR,MASS,VOLUME,R_O,R_I
TYPE(LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()
TYPE(SPECIES_TYPE),POINTER:: SS=>NULL()
TYPE(SURFACE_TYPE),POINTER:: SF=>NULL()

IF (N_LAGRANGIAN_CLASSES == 0) RETURN

PART_LOOP: DO N=1,N_LAGRANGIAN_CLASSES

   LPC => LAGRANGIAN_PARTICLE_CLASS(N)
   SF  => SURFACE(LPC%SURF_INDEX)

   ! Assign device or controller

   CALL SEARCH_CONTROLLER('PART',LPC%CTRL_ID,LPC%DEVC_ID,LPC%DEVC_INDEX,LPC%CTRL_INDEX,N)

   ! Get density if the particles are liquid droplets or have mass

   IF (LPC%LIQUID_DROPLET .AND. LPC%DENSITY < 0._EB) THEN
      LPC%DENSITY = SPECIES(LPC%Y_INDEX)%DENSITY_LIQUID
      LPC%FTPR = FOTH*PI*LPC%DENSITY
   ENDIF

   IF (SF%THERMAL_BC_INDEX==THERMALLY_THICK) THEN
      LPC%INCLUDE_BOUNDARY_ONE_D_TYPE = .TRUE.
      MASS = 0._EB
      VOLUME = 0._EB
      AREA_FACTOR = 1._EB
      DO NN=1,SF%N_CELLS_INI
         SELECT CASE (SF%GEOMETRY)
            CASE (SURF_CARTESIAN)
               IF (LPC%DRAG_LAW/=SCREEN_DRAG .AND. LPC%DRAG_LAW/=POROUS_DRAG) AREA_FACTOR = 2._EB*SF%LENGTH*SF%WIDTH
               MASS = MASS + AREA_FACTOR*(SF%X_S(NN)-SF%X_S(NN-1))*SUM(SF%RHO_0(NN,1:SF%N_MATL))
               VOLUME = VOLUME + AREA_FACTOR*(SF%X_S(NN)-SF%X_S(NN-1))
            CASE (SURF_CYLINDRICAL)
               IF (LPC%DRAG_LAW/=SCREEN_DRAG .AND. LPC%DRAG_LAW/=POROUS_DRAG) AREA_FACTOR = PI*SF%LENGTH
               R_I = SF%INNER_RADIUS + SF%THICKNESS - SF%X_S(NN)
               R_O = SF%INNER_RADIUS + SF%THICKNESS - SF%X_S(NN-1)
               MASS   = MASS   + AREA_FACTOR*(R_O**2-R_I**2)*SUM(SF%RHO_0(NN,1:SF%N_MATL))
               VOLUME = VOLUME + AREA_FACTOR*(R_O**2-R_I**2)
            CASE (SURF_SPHERICAL)
               AREA_FACTOR = FOTHPI
               R_I = SF%INNER_RADIUS + SF%THICKNESS - SF%X_S(NN)
               R_O = SF%INNER_RADIUS + SF%THICKNESS - SF%X_S(NN-1)
               MASS   = MASS   + AREA_FACTOR*(R_O**3-R_I**3)*SUM(SF%RHO_0(NN,1:SF%N_MATL))
               VOLUME = VOLUME + AREA_FACTOR*(R_O**3-R_I**3)
         END SELECT
      ENDDO
      LPC%KILL_RADIUS = SF%MINIMUM_LAYER_THICKNESS
      LPC%DENSITY = MASS/VOLUME
      LPC%INITIAL_MASS = MASS
      LPC%FTPR = FOTH*PI*LPC%DENSITY
   ENDIF

   ! Exclude some convective heat transfer models from being applied to a particle

   IF (SF%HEAT_TRANSFER_MODEL==LOGLAW_HTC_MODEL) THEN
      CALL SHUTDOWN('ERROR(225): HEAT_TRANSFER_MODEL not appropriate for PART')
      RETURN
   ENDIF

   ! If COLOR is not assigned to the PART class, use the SURF color if it has been specified

   IF (ALL(LPC%RGB==0) .AND. .NOT.ALL(SF%RGB==SURFACE(INERT_SURF_INDEX)%RGB)) LPC%RGB=SF%RGB

   ! Set the flag to do particle exchanges between meshes

   IF (NMESHES>1) MPI_PARTICLE_EXCHANGE=.TRUE.

   ! Only process DROPLETs

   SURF_OR_SPEC: IF (LPC%SURF_INDEX==DROPLET_SURF_INDEX) THEN

      SS => SPECIES(LPC%Y_INDEX)

      IF (LPC%DENSITY < 0._EB) LPC%DENSITY = SS%DENSITY_LIQUID


   ENDIF SURF_OR_SPEC

   ! Adjust the evaporation rate of fuel PARTICLEs to account for difference in HoC.

   IF (LPC%HEAT_OF_COMBUSTION > 0._EB) THEN
      REAC_DO: DO NN=1,N_REACTIONS
         IF (TRIM(SPECIES_MIXTURE(LPC%Z_INDEX)%ID) == TRIM(REACTION(NN)%FUEL)) THEN
            LPC%ADJUST_EVAPORATION = LPC%HEAT_OF_COMBUSTION/REACTION(NN)%HOC_COMPLETE
            EXIT REAC_DO
         ENDIF
      ENDDO REAC_DO
   ENDIF

   IF (LPC%CNF_RAMP_INDEX > 0) THEN
      IF (LPC%MINIMUM_DIAMETER < 0._EB) THEN
         DO NN = 1,RAMPS(LPC%CNF_RAMP_INDEX)%NUMBER_DATA_POINTS
            IF (RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN) > CNF_CUTOFF) THEN
               IF (NN==1) THEN
                  LPC%MINIMUM_DIAMETER = RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN)
               ELSE
                  LPC%MINIMUM_DIAMETER= RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN-1) + &
                     (RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN) - RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN-1)) * &
                     (CNF_CUTOFF                                     - RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN-1)) / &
                     (RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN)   - RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN-1))
               ENDIF
               LPC%MINIMUM_DIAMETER = LPC%MINIMUM_DIAMETER * 1.E-6_EB
               EXIT
            ENDIF
         ENDDO
         IF (LPC%MINIMUM_DIAMETER < 0.01E-6_EB) THEN
            WRITE(MESSAGE,'(A,A,A)') 'WARNING: PART ID ',TRIM(LPC%ID),&
               '. Diameter of CNF ramp at CNF_CUTOFF is < 0.01 micron; numerical issues may result.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') MESSAGE
         ENDIF
      ENDIF
      LPC%KILL_RADIUS      = (0.005_EB*(0.5_EB*LPC%MINIMUM_DIAMETER)**3)**ONTH
      DO NN = 1,RAMPS(LPC%CNF_RAMP_INDEX)%NUMBER_DATA_POINTS
         IF (RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN) > 0.5_EB) THEN
            IF (NN==1) THEN
               LPC%DIAMETER = RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN)
            ELSE
               LPC%DIAMETER = RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN-1) + &
                            (RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN) - RAMPS(LPC%CNF_RAMP_INDEX)%INDEPENDENT_DATA(NN-1)) * &
                            (0.5_EB                                        - RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN-1)) / &
                            (RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN)  - RAMPS(LPC%CNF_RAMP_INDEX)%DEPENDENT_DATA(NN-1))
            ENDIF
            LPC%DIAMETER = LPC%DIAMETER * 1.E-6_EB
            EXIT
         ENDIF
      ENDDO
   ENDIF
ENDDO PART_LOOP

END SUBROUTINE PROC_PART


SUBROUTINE READ_PROP

USE DEVICE_VARIABLES
USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX,GET_TABLE_INDEX
USE PHYSICAL_FUNCTIONS, ONLY : SPRAY_ANGLE_DISTRIBUTION
USE MISC_FUNCTIONS, ONLY: GET_SPEC_OR_SMIX_INDEX
REAL(EB) :: ACTIVATION_OBSCURATION,ACTIVATION_TEMPERATURE,ALPHA_C,ALPHA_E,BETA_C,BETA_E, &
            HEAT_TRANSFER_COEFFICIENT,DIAMETER,DENSITY,SPECIFIC_HEAT, &
            C_FACTOR,CHARACTERISTIC_VELOCITY,ORIFICE_DIAMETER,EMISSIVITY, &
            PARTICLE_VELOCITY,FLOW_RATE,FLOW_TAU,GAUGE_EMISSIVITY,GAUGE_TEMPERATURE,INITIAL_TEMPERATURE,K_FACTOR,&
            LENGTH,SPRAY_ANGLE(2,2),OFFSET,OPERATING_PRESSURE,RTI,PDPA_START,PDPA_END,PDPA_RADIUS,MASS_FLOW_RATE,&
            SPRAY_PATTERN_MU,SPRAY_PATTERN_BETA,HISTOGRAM_LIMITS(2),P0,PX(3),PXX(3,3),TIME_CONSTANT
INTEGER ::I,N,NN,PDPA_M,PDPA_N,PARTICLES_PER_SECOND,VELOCITY_COMPONENT,HISTOGRAM_NBINS,FED_ACTIVITY
LOGICAL :: PDPA_INTEGRATE,PDPA_NORMALIZE,HISTOGRAM_NORMALIZE,HISTOGRAM,HISTOGRAM_CUMULATIVE,SPARK
CHARACTER(LABEL_LENGTH) :: SMOKEVIEW_ID(SMOKEVIEW_OBJECTS_DIMENSION),QUANTITY='null',PART_ID='null',FLOW_RAMP='null', &
                 SPRAY_PATTERN_TABLE='null',SPEC_ID='null',&
                 PRESSURE_RAMP='null',SMOKEVIEW_PARAMETERS(SMOKEVIEW_OBJECTS_DIMENSION), &
                 SPRAY_PATTERN_SHAPE='GAUSSIAN'
TYPE (PROPERTY_TYPE), POINTER :: PY=>NULL()

NAMELIST /PROP/ ACTIVATION_OBSCURATION,ACTIVATION_TEMPERATURE,ALPHA_C,ALPHA_E,BETA_C,BETA_E, &
                CHARACTERISTIC_VELOCITY,C_FACTOR,DENSITY,DIAMETER,EMISSIVITY,FED_ACTIVITY,FLOW_RAMP,FLOW_RATE,FLOW_TAU, &
                GAUGE_EMISSIVITY,GAUGE_TEMPERATURE,HEAT_TRANSFER_COEFFICIENT,HISTOGRAM,HISTOGRAM_CUMULATIVE, &
                HISTOGRAM_LIMITS,HISTOGRAM_NBINS,HISTOGRAM_NORMALIZE,ID, &
                INITIAL_TEMPERATURE,K_FACTOR,LENGTH,MASS_FLOW_RATE,OFFSET,OPERATING_PRESSURE,ORIFICE_DIAMETER,P0,&
                PARTICLES_PER_SECOND,PARTICLE_VELOCITY,PART_ID,PDPA_END,&
                PDPA_INTEGRATE,PDPA_M,PDPA_N,PDPA_NORMALIZE,PDPA_RADIUS,&
                PDPA_START,PRESSURE_RAMP,PX,PXX,QUANTITY,RTI,SMOKEVIEW_ID,SMOKEVIEW_PARAMETERS,SPARK,&
                SPEC_ID,SPECIFIC_HEAT,SPRAY_ANGLE,&
                SPRAY_PATTERN_BETA,SPRAY_PATTERN_MU,SPRAY_PATTERN_SHAPE,SPRAY_PATTERN_TABLE,TIME_CONSTANT,VELOCITY_COMPONENT

! Count the PROP lines in the input file. Note how many of these are cables.

N_PROP=0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_PROP_LOOP: DO
   CALL CHECKREAD('PROP',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_PROP_LOOP
   READ(LU_INPUT,PROP,ERR=34,IOSTAT=IOS)
   N_PROP = N_PROP + 1
   34 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with PROP number ', N_PROP+1,', line number ',INPUT_FILE_LINE_NUMBER
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_PROP_LOOP

! Allocate the PROPERTY derived types

ALLOCATE(PROPERTY(0:N_PROP),STAT=IZERO)
CALL ChkMemErr('READ','PROPERTY',IZERO)

! Read the PROP lines in the order listed in the input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

READ_PROP_LOOP: DO N=0,N_PROP

   CALL CHECKREAD('PROP',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   CALL SET_PROP_DEFAULTS          ! Reset PROP NAMELIST parameters to default values
   IF (N > 0) READ(LU_INPUT,PROP)

   ! Pack PROP parameters into the appropriate property derived types

   PY => PROPERTY(N)
   PY%ACTIVATION_OBSCURATION   = ACTIVATION_OBSCURATION
   PY%ACTIVATION_TEMPERATURE   = ACTIVATION_TEMPERATURE   ! NOTE: Act_Temp remains in degrees C. It is just a SETPOINT.
   PY%ALPHA_C                  = ALPHA_C
   IF (LENGTH>0._EB) PY%ALPHA_C = LENGTH
   PY%ALPHA_E                  = ALPHA_E
   PY%BETA_C                   = BETA_C
   PY%BETA_E                   = BETA_E
   PY%DENSITY                  = DENSITY
   PY%DIAMETER                 = DIAMETER
   PY%EMISSIVITY               = EMISSIVITY
   PY%HEAT_TRANSFER_COEFFICIENT= HEAT_TRANSFER_COEFFICIENT
   PY%SPECIFIC_HEAT            = SPECIFIC_HEAT*1000._EB/TIME_SHRINK_FACTOR
   PY%C_FACTOR                 = C_FACTOR
   PY%CHARACTERISTIC_VELOCITY  = CHARACTERISTIC_VELOCITY
   PY%GAUGE_EMISSIVITY         = GAUGE_EMISSIVITY
   PY%GAUGE_TEMPERATURE        = GAUGE_TEMPERATURE + TMPM
   PY%ID                       = ID
   PY%INITIAL_TEMPERATURE      = INITIAL_TEMPERATURE + TMPM
   PY%PARTICLES_PER_SECOND     = PARTICLES_PER_SECOND
   PY%OFFSET                   = OFFSET
   PY%OPERATING_PRESSURE       = OPERATING_PRESSURE
   PY%PART_ID                  = PART_ID
   PY%QUANTITY                 = QUANTITY
   IF (PY%PART_ID/='null' .AND. PY%QUANTITY == 'null' ) PY%QUANTITY = 'NOZZLE FLOW RATE'
   PY%RTI                      = RTI
   IF (SMOKEVIEW_ID(1)/='null') THEN
      PY%SMOKEVIEW_ID          = SMOKEVIEW_ID
      PY%N_SMOKEVIEW_IDS = 0
      DO NN=1,SMOKEVIEW_OBJECTS_DIMENSION
         IF (SMOKEVIEW_ID(NN)/='null') PY%N_SMOKEVIEW_IDS = PY%N_SMOKEVIEW_IDS + 1
      ENDDO
   ELSE
      PY%N_SMOKEVIEW_IDS = 1
      SELECT CASE(PY%QUANTITY)
         CASE DEFAULT
            PY%SMOKEVIEW_ID(1) = 'sensor'
         CASE('SPRINKLER LINK TEMPERATURE')
            PY%SMOKEVIEW_ID(1) = 'sprinkler_pendent'
         CASE('NOZZLE FLOW RATE')
            PY%SMOKEVIEW_ID(1) = 'nozzle'
         CASE('LINK TEMPERATURE')
            PY%SMOKEVIEW_ID(1) = 'heat_detector'
         CASE('spot obscuration','CHAMBER OBSCURATION')
            PY%SMOKEVIEW_ID(1) = 'smoke_detector'
         CASE('THERMOCOUPLE')
            PY%SMOKEVIEW_ID(1) = 'thermocouple'
      END SELECT
   ENDIF
   PY%SMOKEVIEW_PARAMETERS = SMOKEVIEW_PARAMETERS
   PY%N_SMOKEVIEW_PARAMETERS = 0
   DO I=1,SMOKEVIEW_OBJECTS_DIMENSION
      IF (PY%SMOKEVIEW_PARAMETERS(I)/='null') PY%N_SMOKEVIEW_PARAMETERS = PY%N_SMOKEVIEW_PARAMETERS + 1
   ENDDO
   PY%SPARK                = SPARK
   PY%SPEC_ID              = SPEC_ID
   IF (PART_ID/='null' .AND. SPRAY_PATTERN_TABLE /= 'null') THEN
      CALL GET_TABLE_INDEX(SPRAY_PATTERN_TABLE,SPRAY_PATTERN,PY%SPRAY_PATTERN_INDEX)
      PY%TABLE_ID = SPRAY_PATTERN_TABLE
   ELSE
      PY%SPRAY_PATTERN_INDEX = 0
   ENDIF
   IF (ABS(SPRAY_ANGLE(1,1)-SPRAY_ANGLE(2,1))<TWO_EPSILON_EB .OR. ABS(SPRAY_ANGLE(1,2)-SPRAY_ANGLE(2,2))<TWO_EPSILON_EB) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(231): PROP ',TRIM(PY%ID),' values for SPRAY_ANGLE cannot be the same.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   PY%SPRAY_ANGLE = SPRAY_ANGLE*DEG2RAD
   IF(ANY(PY%SPRAY_ANGLE(1:2,2)<0)) PY%SPRAY_ANGLE(1:2,2)=PY%SPRAY_ANGLE(1:2,1)
   SPRAY_PATTERN_MU=SPRAY_PATTERN_MU*DEG2RAD
   IF (PART_ID/='null' .AND. SPRAY_PATTERN_TABLE == 'null' .AND. PDPA_RADIUS<TWO_EPSILON_EB) THEN
      ALLOCATE(PY%SPRAY_LON_CDF(0:NDC2),PY%SPRAY_LON(0:NDC2),PY%SPRAY_LAT(0:NDC2),PY%SPRAY_LAT_CDF(0:NDC2,0:NDC2))
      IF(SPRAY_PATTERN_MU<0._EB) THEN
         IF(SPRAY_ANGLE(1,1)>0._EB) THEN
            SPRAY_PATTERN_MU=0.5_EB*SUM(PY%SPRAY_ANGLE(1:2,1))
         ELSE
            SPRAY_PATTERN_MU=0._EB
         ENDIF
      ENDIF
      CALL SPRAY_ANGLE_DISTRIBUTION(PY%SPRAY_LON,PY%SPRAY_LAT,PY%SPRAY_LON_CDF,PY%SPRAY_LAT_CDF, &
                                    SPRAY_PATTERN_BETA,SPRAY_PATTERN_MU,PY%SPRAY_ANGLE,SPRAY_PATTERN_SHAPE,NDC2)
   ENDIF
   PY%TIME_CONSTANT = TIME_CONSTANT

   ! PDPA model

   PY%PDPA_START       = PDPA_START
   PY%PDPA_END         = PDPA_END
   PY%PDPA_RADIUS      = PDPA_RADIUS
   PY%PDPA_M           = PDPA_M
   PY%PDPA_N           = PDPA_N
   PY%PDPA_INTEGRATE   = PDPA_INTEGRATE
   PY%PDPA_NORMALIZE   = PDPA_NORMALIZE
   IF (TRIM(PY%QUANTITY) == 'NUMBER CONCENTRATION') THEN
      PY%PDPA_M        = 0
      PY%PDPA_N        = 0
   ENDIF
   IF ((TRIM(PY%QUANTITY) == 'MASS CONCENTRATION') .OR. &
       (TRIM(PY%QUANTITY) == 'ENTHALPY')           .OR. &
       (TRIM(PY%QUANTITY) == 'PARTICLE FLUX X')    .OR. &
       (TRIM(PY%QUANTITY) == 'PARTICLE FLUX Y')    .OR. &
       (TRIM(PY%QUANTITY) == 'PARTICLE FLUX Z')) THEN
      PY%PDPA_M        = 3
      PY%PDPA_N        = 0
   ENDIF

   ! Histograms

   PY%HISTOGRAM             = HISTOGRAM
   PY%HISTOGRAM_NBINS       = HISTOGRAM_NBINS
   PY%HISTOGRAM_LIMITS      = HISTOGRAM_LIMITS
   PY%HISTOGRAM_CUMULATIVE  = HISTOGRAM_CUMULATIVE
   PY%HISTOGRAM_NORMALIZE   = HISTOGRAM_NORMALIZE
   IF (HISTOGRAM) THEN
      IF (HISTOGRAM_NBINS<2) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(232): PROP ',TRIM(PY%ID),', HISTOGRAM needs HISTOGRAM_NBINS>2.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
       IF (ABS(HISTOGRAM_LIMITS(1)-HISTOGRAM_LIMITS(2)) < TWO_EPSILON_EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(233): PROP ',TRIM(PY%ID),', HISTOGRAM needs HISTOGRAM_LIMITS.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

   ENDIF

   PY%FED_ACTIVITY = FED_ACTIVITY
   IF(FED_ACTIVITY < 1 .OR. FED_ACTIVITY > 3) THEN
      WRITE(MESSAGE,'(A,A,A,I0)') 'ERROR(234): PROP ',TRIM(PY%ID),', FED_ACTIVITY out of range: ',FED_ACTIVITY
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   PATCH_VELOCITY_IF: IF (VELOCITY_COMPONENT>0) THEN
      IF(VELOCITY_COMPONENT > 3) THEN
         WRITE(MESSAGE,'(A,A,A,I0)') 'ERROR(235): PROP ',TRIM(PY%ID),', VELOCITY_COMPONENT > 3: ',VELOCITY_COMPONENT
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF(P0<-1.E9_EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(236): PROP ',TRIM(PY%ID),', VELOCITY_PATCH requires P0.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      PY%I_VEL = VELOCITY_COMPONENT
      PY%P0 = P0  ! value at origin of Taylor expansion
      DO J=1,3
         PY%PX(J) = PX(J)  ! first derivative of P evaluated at origin
         DO I=1,3
            IF (I>J) PXX(I,J)=PXX(J,I) ! make symmetric
            PY%PXX(I,J) = PXX(I,J) ! second derivative of P evaluated at origin
         ENDDO
      ENDDO
   ENDIF PATCH_VELOCITY_IF

   ! Set flow variables
   PY%MASS_FLOW_RATE =MASS_FLOW_RATE
   PY%FLOW_RATE      =FLOW_RATE

   IF (PART_ID/='null' .AND. PRESSURE_RAMP /= 'null') THEN
      CALL GET_RAMP_INDEX(PRESSURE_RAMP,'PRESSURE',PY%PRESSURE_RAMP_INDEX)
   ELSE
      PY%PRESSURE_RAMP_INDEX = 0
   ENDIF

   ! Check sufficient input

   IF (PY%PRESSURE_RAMP_INDEX == 0 .AND. FLOW_RATE > 0._EB) THEN
      IF (K_FACTOR < 0._EB) K_FACTOR = 10.0_EB
   ENDIF

   IF (PART_ID /='null' .AND. ABS(PDPA_RADIUS) <= TWO_EPSILON_EB) THEN
      IF (MASS_FLOW_RATE > 0._EB) THEN
         PY%MASS_FLOW_RATE = MASS_FLOW_RATE
         IF (ABS(PARTICLE_VELOCITY) <= TWO_EPSILON_EB) THEN
            WRITE(MESSAGE,'(A,A,A)') 'ERROR(237): PROP ',TRIM(PY%ID),', specify PARTICLE_VELOCITY with MASS_FLOW_RATE.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ELSE
            PY%PARTICLE_VELOCITY  = PARTICLE_VELOCITY
         ENDIF
      ELSE
         IF ((FLOW_RATE>0._EB .AND. K_FACTOR<=0._EB .AND. OPERATING_PRESSURE<=0._EB) .OR. &
            (FLOW_RATE<0._EB .AND. K_FACTOR>=0._EB .AND. OPERATING_PRESSURE<=0._EB) .OR. &
            (FLOW_RATE<0._EB .AND. K_FACTOR<=0._EB .AND. OPERATING_PRESSURE>0._EB)) THEN
            WRITE(MESSAGE,'(A,A,A)') 'ERROR(238): Problem with PROP ',TRIM(PY%ID),', too few flow parameters.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (K_FACTOR < 0._EB .AND. OPERATING_PRESSURE > 0._EB)  K_FACTOR           = FLOW_RATE/SQRT(OPERATING_PRESSURE)
         IF (FLOW_RATE < 0._EB .AND. OPERATING_PRESSURE > 0._EB) FLOW_RATE          = K_FACTOR*SQRT(OPERATING_PRESSURE)
         IF (OPERATING_PRESSURE < 0._EB .AND. K_FACTOR > 0._EB)  OPERATING_PRESSURE = (FLOW_RATE/K_FACTOR)**2
         PY%K_FACTOR           = K_FACTOR
         PY%FLOW_RATE          = FLOW_RATE
         PY%OPERATING_PRESSURE = OPERATING_PRESSURE

         IF (PARTICLE_VELOCITY<=TWO_EPSILON_EB .AND. ORIFICE_DIAMETER<=TWO_EPSILON_EB .AND. &
            PRESSURE_RAMP=='null' .AND. SPRAY_PATTERN_TABLE=='null') THEN
            WRITE(MESSAGE,'(A,A,A)') 'WARNING: PROP ',TRIM(PY%ID),' PARTICLE velocity is not defined.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         ENDIF

         IF (PARTICLE_VELOCITY > 0._EB) THEN
            PY%PARTICLE_VELOCITY  = PARTICLE_VELOCITY
         ELSEIF ((ORIFICE_DIAMETER > 0._EB) .AND. (FLOW_RATE > 0._EB)) THEN
            PY%PARTICLE_VELOCITY  = (FLOW_RATE/60._EB/1000._EB)/(PI*(ORIFICE_DIAMETER/2._EB)**2)
         ENDIF
      ENDIF
   ENDIF
   IF (FLOW_RAMP /= 'null') THEN
      CALL GET_RAMP_INDEX(FLOW_RAMP,'TIME',PY%FLOW_RAMP_INDEX)
   ELSE
      PY%FLOW_RAMP_INDEX = 0
   ENDIF
   IF (ABS(FLOW_TAU) > TWO_EPSILON_EB) THEN
      PY%FLOW_TAU = FLOW_TAU/TIME_SHRINK_FACTOR
      IF (FLOW_TAU > 0._EB) PY%FLOW_RAMP_INDEX = TANH_RAMP
      IF (FLOW_TAU < 0._EB) PY%FLOW_RAMP_INDEX = TSQR_RAMP
   ENDIF

   ! Check for SPEC_ID

   IF (PY%SPEC_ID/='null') THEN
      CALL GET_SPEC_OR_SMIX_INDEX(PY%SPEC_ID,PY%Y_INDEX,PY%Z_INDEX)
      IF (PY%Z_INDEX>=0 .AND. PY%Y_INDEX>=1) THEN
         IF (TRIM(PY%QUANTITY)=='DIFFUSIVITY') THEN
            PY%Y_INDEX=-999
         ELSE
            PY%Z_INDEX=-999
         ENDIF
      ENDIF
      IF (PY%Y_INDEX<1 .AND. PY%Z_INDEX<0) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(239): PROP SPEC_ID ',TRIM(PY%SPEC_ID),' not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF

ENDDO READ_PROP_LOOP


CONTAINS


SUBROUTINE SET_PROP_DEFAULTS

ACTIVATION_OBSCURATION   = 3.24_EB     ! %/m
ACTIVATION_TEMPERATURE   = -273.15_EB  ! C
ALPHA_C                  = 1.8_EB      ! m, Heskestad Length Scale
ALPHA_E                  = 0.0_EB
BETA_C                   = -1.0_EB
BETA_E                   = -1.0_EB
DENSITY                  = 8908._EB    ! kg/m3 (Nickel)
DIAMETER                 = 0.001       ! m
EMISSIVITY               = 0.85_EB
HEAT_TRANSFER_COEFFICIENT= -1._EB      ! W/m2/K
SPECIFIC_HEAT            = 0.44_EB     ! kJ/kg/K (Nickel)
C_FACTOR                 = 0.0_EB
CHARACTERISTIC_VELOCITY  = 1.0_EB      ! m/s
PARTICLE_VELOCITY         = 0._EB       ! m/s
PARTICLES_PER_SECOND      = 5000
FLOW_RATE                = -1._EB      ! L/min
MASS_FLOW_RATE           = -1._EB
FLOW_RAMP                = 'null'
FLOW_TAU                 = 0._EB
GAUGE_EMISSIVITY         = 1._EB
GAUGE_TEMPERATURE        = TMPA - TMPM
INITIAL_TEMPERATURE      = TMPA - TMPM
ID                       = 'null'
K_FACTOR                 = -1.0_EB     ! L/min/bar**0.5
LENGTH                   = -1.0_EB
MASS_FLOW_RATE           = -1._EB      ! kg/s
OFFSET                   = 0.05_EB     ! m
OPERATING_PRESSURE       = -1.0_EB     ! bar
ORIFICE_DIAMETER         = 0.0_EB      ! m
PART_ID                  = 'null'
PDPA_START               = T_BEGIN
PDPA_END                 = T_END + 1.0_EB
PDPA_RADIUS              = 0.0_EB
PDPA_M                   = 0
PDPA_N                   = 0
PDPA_INTEGRATE           = .TRUE.
PDPA_NORMALIZE           = .TRUE.
HISTOGRAM                = .FALSE.
HISTOGRAM_CUMULATIVE     = .FALSE.
HISTOGRAM_NBINS          = -1
HISTOGRAM_LIMITS         = 0._EB
HISTOGRAM_CUMULATIVE     = .FALSE.
HISTOGRAM_NORMALIZE      = .TRUE.
PRESSURE_RAMP            = 'null'
P0                       = -1.E10_EB
PX                       = 0._EB
PXX                      = 0._EB
QUANTITY                 = 'null'
RTI                      = 100._EB     ! (ms)**0.5
SMOKEVIEW_ID             = 'null'
SMOKEVIEW_PARAMETERS     = 'null'
SPARK                    = .FALSE.
SPEC_ID                  = 'null'
SPRAY_ANGLE(1,1)           = 60._EB      ! degrees
SPRAY_ANGLE(2,1)           = 75._EB      ! degrees
SPRAY_ANGLE(1,2)           = -999._EB      ! degrees
SPRAY_ANGLE(2,2)           = -998._EB      ! degrees
SPRAY_PATTERN_TABLE      = 'null'
SPRAY_PATTERN_SHAPE      = 'GAUSSIAN'
SPRAY_PATTERN_MU         = -1._EB
SPRAY_PATTERN_BETA       = 5.0_EB
TIME_CONSTANT            = -1._EB
FED_ACTIVITY             = 2 ! light work
VELOCITY_COMPONENT       = 0
END SUBROUTINE SET_PROP_DEFAULTS

END SUBROUTINE READ_PROP



SUBROUTINE PROC_PROP

USE DEVICE_VARIABLES
REAL(EB) :: TOTAL_FLOWRATE, SUBTOTAL_FLOWRATE
INTEGER :: N,NN,N_V_FACTORS,ILPC
LOGICAL :: TABLE_NORMED(1:N_TABLE)
TYPE (PROPERTY_TYPE), POINTER :: PY=>NULL()
TYPE (TABLES_TYPE),  POINTER :: TA=>NULL()
TYPE (LAGRANGIAN_PARTICLE_CLASS_TYPE),POINTER :: LPC=>NULL()

TABLE_NORMED = .FALSE.

PROP_LOOP: DO N=0,N_PROP
   PY => PROPERTY(N)

   ! Assign PART_INDEX to Device PROPERTY array

   IF (PY%PART_ID/='null') THEN

      DO ILPC=1,N_LAGRANGIAN_CLASSES
         LPC => LAGRANGIAN_PARTICLE_CLASS(ILPC)
         IF (LPC%ID==PY%PART_ID) THEN
            PY%PART_INDEX = ILPC
            EXIT
         ENDIF
      ENDDO

      IF (PY%PART_INDEX<0) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(240): PART_ID for PROP ',TRIM(PY%ID),' not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (LPC%ID==PY%PART_ID .AND. LPC%MASSLESS_TRACER) THEN
         IF ( .NOT.(TRIM(PY%QUANTITY)=='NUMBER CONCENTRATION' .OR. &
                    TRIM(PY%QUANTITY)=='U-VELOCITY'           .OR. &
                    TRIM(PY%QUANTITY)=='V-VELOCITY'           .OR. &
                    TRIM(PY%QUANTITY)=='W-VELOCITY'           .OR. &
                    TRIM(PY%QUANTITY)=='VELOCITY')                 ) THEN
            WRITE(MESSAGE,'(A,A,A)') 'ERROR(241): PART_ID for PROP ',TRIM(PY%ID),' cannot refer to MASSLESS particles.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF

      PARTICLE_FILE=.TRUE.

      ! Initial nozzle velocity for primary breakup model

      IF (PY%PARTICLE_VELOCITY>TWO_EPSILON_EB) LPC%PRIMARY_BREAKUP_TIME = LPC%PRIMARY_BREAKUP_TIME/PY%PARTICLE_VELOCITY

   ENDIF

   ! Set up spinkler distributrion if needed

   IF (PY%SPRAY_PATTERN_INDEX > 0) THEN
      TA => TABLES(PY%SPRAY_PATTERN_INDEX)
      ALLOCATE(PY%TABLE_ROW(1:TA%NUMBER_ROWS))
      TOTAL_FLOWRATE=0._EB
      SUBTOTAL_FLOWRATE=0._EB
      DO NN=1,TA%NUMBER_ROWS
         IF (TA%TABLE_DATA(NN,6) <=0._EB) THEN
            WRITE(MESSAGE,'(A,A,A,I0)') 'ERROR(242): Spray Pattern Table ',TRIM(PY%TABLE_ID),' massflux<=0 for line ',NN
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         TOTAL_FLOWRATE = TOTAL_FLOWRATE + TA%TABLE_DATA(NN,6)
      ENDDO
      IF (TABLE_NORMED(PY%SPRAY_PATTERN_INDEX)) THEN
         DO NN=1,TA%NUMBER_ROWS
            SUBTOTAL_FLOWRATE = SUBTOTAL_FLOWRATE + TA%TABLE_DATA(NN,6)
            PY%TABLE_ROW(NN) = SUBTOTAL_FLOWRATE/TOTAL_FLOWRATE
         ENDDO
      ELSE
         DO NN=1,TA%NUMBER_ROWS
            TA%TABLE_DATA(NN,1) = TA%TABLE_DATA(NN,1) * DEG2RAD
            TA%TABLE_DATA(NN,2) = TA%TABLE_DATA(NN,2) * DEG2RAD
            TA%TABLE_DATA(NN,3) = TA%TABLE_DATA(NN,3) * DEG2RAD
            TA%TABLE_DATA(NN,4) = TA%TABLE_DATA(NN,4) * DEG2RAD
            SUBTOTAL_FLOWRATE = SUBTOTAL_FLOWRATE + TA%TABLE_DATA(NN,6)
            PY%TABLE_ROW(NN) = SUBTOTAL_FLOWRATE/TOTAL_FLOWRATE
         ENDDO
         TABLE_NORMED(PY%SPRAY_PATTERN_INDEX) = .TRUE.
      ENDIF
      PY%TABLE_ROW(TA%NUMBER_ROWS) = 1._EB
   END IF

   ! Set up pressure dependence

   IF (PY%PRESSURE_RAMP_INDEX > 0) THEN
      IF (PY%SPRAY_PATTERN_INDEX > 0) THEN
         N_V_FACTORS = TA%NUMBER_ROWS
      ELSE
         N_V_FACTORS = 1
      ENDIF
      ALLOCATE(PY%V_FACTOR(1:N_V_FACTORS))
      IF (PY%SPRAY_PATTERN_INDEX > 0) THEN
         DO NN=1,TA%NUMBER_ROWS
            PY%V_FACTOR(NN) = TA%TABLE_DATA(NN,5)/SQRT(PY%OPERATING_PRESSURE)
         ENDDO
      ELSE
         PY%V_FACTOR = PY%PARTICLE_VELOCITY/SQRT(PY%OPERATING_PRESSURE)
      ENDIF
   ENDIF

ENDDO PROP_LOOP

END SUBROUTINE PROC_PROP



SUBROUTINE READ_MATL

USE COMP_FUNCTIONS, ONLY : SEARCH_INPUT_FILE
USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX
CHARACTER(LABEL_LENGTH) :: CONDUCTIVITY_RAMP,SPECIFIC_HEAT_RAMP
CHARACTER(LABEL_LENGTH) :: SPEC_ID(MAX_SPECIES,MAX_REACTIONS),PART_ID(MAX_LPC,MAX_REACTIONS)
REAL(EB) :: EMISSIVITY,CONDUCTIVITY,SPECIFIC_HEAT,DENSITY,ABSORPTION_COEFFICIENT,BOILING_TEMPERATURE, &
            PEAK_REACTION_RATE,MW,&
            REFERENCE_ENTHALPY,REFERENCE_ENTHALPY_TEMPERATURE
REAL(EB), DIMENSION(MAX_MATERIALS,MAX_REACTIONS) :: NU_MATL,HEAT_OF_COMBUSTION
REAL(EB), DIMENSION(MAX_REACTIONS) :: A,E,HEATING_RATE,PYROLYSIS_RANGE,HEAT_OF_REACTION, &
          N_S,N_T,N_O2,REFERENCE_RATE,REFERENCE_TEMPERATURE, &
          GAS_DIFFUSION_DEPTH,MAX_REACTION_RATE
REAL(EB), DIMENSION(MAX_SPECIES,MAX_REACTIONS) :: NU_SPEC
REAL(EB) :: NU_PART(MAX_LPC,MAX_REACTIONS)
LOGICAL :: ADJUST_H,ALLOW_SHRINKING, ALLOW_SWELLING,ADD_MATL,FOUND,SURFACE_OXIDATION_MODEL
CHARACTER(LABEL_LENGTH), DIMENSION(MAX_MATERIALS,MAX_REACTIONS) :: MATL_ID
CHARACTER(LABEL_LENGTH), ALLOCATABLE, DIMENSION(:) :: SEARCH_PHRASE,MATL_NAME_RESERVED
INTEGER :: N,NN,NNN,IOS,NR,N_REACTIONS,N_MATL_RESERVED,N_MATL_READ
NAMELIST /MATL/ A,ABSORPTION_COEFFICIENT,ADJUST_H,ALLOW_SHRINKING,ALLOW_SWELLING,BOILING_TEMPERATURE,&
                CONDUCTIVITY,CONDUCTIVITY_RAMP,DENSITY,E,EMISSIVITY,FYI,&
                GAS_DIFFUSION_DEPTH,HEATING_RATE,HEAT_OF_COMBUSTION,HEAT_OF_REACTION,ID,MATL_ID,&
                MAX_REACTION_RATE,MW,N_O2,N_REACTIONS,N_S,N_T,NU_MATL,NU_PART,NU_SPEC,PART_ID,&
                PYROLYSIS_RANGE,REFERENCE_ENTHALPY,REFERENCE_ENTHALPY_TEMPERATURE,&
                REFERENCE_RATE,REFERENCE_TEMPERATURE,&
                SPECIFIC_HEAT,SPECIFIC_HEAT_RAMP,SPEC_ID,SURFACE_OXIDATION_MODEL

! Count the MATL lines in the input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
N_MATL = 0
COUNT_MATL_LOOP: DO
   CALL CHECKREAD('MATL',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_MATL_LOOP
   READ(LU_INPUT,MATL,ERR=34,IOSTAT=IOS)
   N_MATL = N_MATL + 1
   MATL_NAME(N_MATL) = ID
   34 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with MATL number ', N_MATL+1,', line number ',INPUT_FILE_LINE_NUMBER
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_MATL_LOOP

N_MATL_READ = N_MATL

! Add reserved materials if necessary

N_MATL_RESERVED = 1
ALLOCATE(SEARCH_PHRASE(N_MATL_RESERVED)) ; ALLOCATE(MATL_NAME_RESERVED(N_MATL_RESERVED))
SEARCH_PHRASE(1) = 'MOISTURE_FRACTION' ; MATL_NAME_RESERVED(1) = 'MOISTURE'

DO NN=1,N_MATL_RESERVED
   CALL SEARCH_INPUT_FILE(LU_INPUT,TRIM(SEARCH_PHRASE(NN)),FOUND)
   IF (FOUND) THEN
      ADD_MATL = .TRUE.
      DO N=1,N_MATL
         IF (MATL_NAME(N)==MATL_NAME_RESERVED(NN)) ADD_MATL = .FALSE.
      ENDDO
      IF (ADD_MATL) THEN
         N_MATL = N_MATL + 1
         MATL_NAME(N_MATL) = MATL_NAME_RESERVED(NN)
      ENDIF
   ENDIF
ENDDO

! Allocate the MATERIAL derived type

ALLOCATE(MATERIAL(1:N_MATL),STAT=IZERO)
CALL ChkMemErr('READ','MATERIAL',IZERO)
! Read the MATL lines in the order listed in the input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

READ_MATL_LOOP: DO N=1,N_MATL

   ML => MATERIAL(N)

   ! Read user defined MATL lines or reserved MATL's

   IF (N <= N_MATL_READ) THEN
      CALL CHECKREAD('MATL',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      CALL SET_MATL_DEFAULTS
      READ(LU_INPUT,MATL)
      IF (ID=='MOISTURE') MOISTURE_INDEX = N
   ELSE
      CALL SET_MATL_DEFAULTS
      SELECT CASE(MATL_NAME(N))
         CASE('MOISTURE')
            ID                  = 'MOISTURE'
            DENSITY             = 1000._EB
            CONDUCTIVITY        = 0.62_EB
            SPECIFIC_HEAT       = 4.184_EB
            A(1)                = 600000._EB
            E(1)                = 48200._EB
            N_T(1)              = -0.5_EB
            SPEC_ID(1,1)        = 'WATER VAPOR'
            NU_SPEC(1,1)        = 1._EB
            HEAT_OF_REACTION(1) = 2295._EB
            MOISTURE_INDEX      = N
      END SELECT
   ENDIF

   ! Do some error checking on the inputs

   NOT_BOILING: IF (BOILING_TEMPERATURE>4000._EB) THEN

      IF ( ( ANY(REFERENCE_TEMPERATURE>-TMPM) .OR. ANY(A>=0._EB) .OR. ANY(E>=0._EB) .OR. &
             ANY(ABS(HEAT_OF_REACTION)<HUGE(1._EB))) .AND. N_REACTIONS==0) THEN
         N_REACTIONS = 1
      ENDIF

      DO NR=1,N_REACTIONS
         IF (HEAT_OF_REACTION(NR) <=-HUGE(1._EB)) THEN
            HEAT_OF_REACTION(NR) = 0._EB
            ADJUST_H = .FALSE.
         ENDIF
         IF (REFERENCE_TEMPERATURE(NR)<-TMPM  .AND. (E(NR)< 0._EB .OR. A(NR)<0._EB)) THEN
            WRITE(MESSAGE,'(A,A,A,I0,A)') 'ERROR(251): MATL ',TRIM(ID),', REAC ',NR,'. Set REFERENCE_TEMPERATURE or E, A'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (ABS(SUM(NU_MATL(:,NR)))<=TWO_EPSILON_EB .AND. ABS(SUM(NU_SPEC(:,NR)))<=TWO_EPSILON_EB &
             .AND. ABS(SUM(NU_PART(:,NR)))<=TWO_EPSILON_EB) THEN
            WRITE(MESSAGE,'(A,A,A,I0,A)') 'WARNING: MATL ',TRIM(ID),', REAC ',NR,'. No product yields (NUs) set'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         ENDIF
      ENDDO

   ELSE NOT_BOILING ! Is liquid

      N_REACTIONS = 1
      IF (ABS(HEAT_OF_REACTION(1))<=TWO_EPSILON_EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(252): MATL ',TRIM(ID),', HEAT_OF_REACTION should be greater than 0.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

   ENDIF NOT_BOILING

   ! Error checking for thermal properties

   IF (ABS(DENSITY) <=TWO_EPSILON_EB ) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(253): Problem with MATL ',TRIM(ID),': DENSITY=0.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (ABS(CONDUCTIVITY) <=TWO_EPSILON_EB .AND. CONDUCTIVITY_RAMP == 'null' ) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(254): Problem with MATL ',TRIM(ID),': CONDUCTIVITY = 0.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (ABS(SPECIFIC_HEAT)<=TWO_EPSILON_EB .AND. SPECIFIC_HEAT_RAMP == 'null' ) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(255): Problem with MATL ',TRIM(ID),': SPECIFIC_HEAT = 0.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SPECIFIC_HEAT > 10._EB) WRITE(LU_ERR,'(A,A)') 'WARNING: SPECIFIC_HEAT units are kJ/kg/K check MATL ',TRIM(ID)

   IF (BOILING_TEMPERATURE<50000._EB) N_REACTIONS = 1

   ! Pack MATL parameters into the MATERIAL derived type

   ALLOCATE(ML%A(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%A',IZERO)
   ML%A(1:N_REACTIONS)                 = A(1:N_REACTIONS)
   ALLOCATE(ML%ADJUST_BURN_RATE(N_TRACKED_SPECIES,MAX(1,N_REACTIONS)),STAT=IZERO)
   CALL ChkMemErr('READ','ML%ADJUST_BURN_RATE',IZERO)
   ML%ADJUST_BURN_RATE                 = 1._EB
   ALLOCATE(ML%ADJUST_BURN_RATE_P(N_SPECIES,MAX(1,N_REACTIONS)),STAT=IZERO)
   CALL ChkMemErr('READ','ML%ADJUST_BURN_RATE_P',IZERO)
   ML%ADJUST_BURN_RATE_P               = 1._EB
   ML%ADJUST_H                         = ADJUST_H
   ML%ALLOW_SHRINKING                  = ALLOW_SHRINKING
   ML%ALLOW_SWELLING                   = ALLOW_SWELLING
   ALLOCATE(ML%C_S(0:I_MAX_TEMP),STAT=IZERO)
   CALL ChkMemErr('READ','ML%C_S',IZERO)
   ML%C_S                              = 1000._EB*SPECIFIC_HEAT/TIME_SHRINK_FACTOR
   ALLOCATE(ML%E(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%E',IZERO)
   ML%E(1:N_REACTIONS)                 = 1000._EB*E(1:N_REACTIONS)
   ML%EMISSIVITY                       = EMISSIVITY
   ML%FYI                              = FYI
   ALLOCATE(ML%GAS_DIFFUSION_DEPTH(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%GAS_DIFFUSION_DEPTH',IZERO)
   ML%GAS_DIFFUSION_DEPTH(1:N_REACTIONS) = GAS_DIFFUSION_DEPTH(1:N_REACTIONS)
   ML%HEAT_OF_COMBUSTION   = 1000._EB*HEAT_OF_COMBUSTION
   ALLOCATE(ML%H_R(N_REACTIONS,0:I_MAX_TEMP),STAT=IZERO)
   CALL ChkMemErr('READ','ML%H_R',IZERO)
   DO NR=1,N_REACTIONS
      ML%H_R(NR,:) = 1000._EB*HEAT_OF_REACTION(NR)
   ENDDO
   ML%ID                               = ID
   ML%KAPPA_S                          = ABSORPTION_COEFFICIENT
   ALLOCATE(ML%K_S(0:I_MAX_TEMP),STAT=IZERO)
   CALL ChkMemErr('READ','ML%K_S',IZERO)
   ML%K_S                              = CONDUCTIVITY
   ALLOCATE(ML%MAX_REACTION_RATE(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%MAX_REACTION_RATE',IZERO)
   ML%MAX_REACTION_RATE(1:N_REACTIONS) = MAX_REACTION_RATE(1:N_REACTIONS)
   ML%MW                               = MW
   ML%N_REACTIONS                      = N_REACTIONS
   ALLOCATE(ML%N_O2(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%N_O2',IZERO)
   ML%N_O2(1:N_REACTIONS)              = N_O2(1:N_REACTIONS)
   ALLOCATE(ML%N_S(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%N_S',IZERO)
   ML%N_S(1:N_REACTIONS)               = N_S(1:N_REACTIONS)
   ALLOCATE(ML%N_T(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%N_T',IZERO)
   ML%N_T(1:N_REACTIONS)               = N_T(1:N_REACTIONS)
   ML%NU_SPEC                          = NU_SPEC
   ML%SPEC_ID                          = SPEC_ID
   ML%RAMP_C_S                         = SPECIFIC_HEAT_RAMP
   ML%RAMP_K_S                         = CONDUCTIVITY_RAMP
   ML%RHO_S                            = DENSITY      ! This is bulk density of pure material.
   ML%REFERENCE_ENTHALPY_TEMPERATURE = REFERENCE_ENTHALPY_TEMPERATURE + TMPM
   ML%REFERENCE_ENTHALPY               = REFERENCE_ENTHALPY*1000._EB
   ML%RESIDUE_MATL_NAME                = MATL_ID
   ALLOCATE(ML%HEATING_RATE(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%HEATING_RATE',IZERO)
   ML%HEATING_RATE(1:N_REACTIONS)      = HEATING_RATE(1:N_REACTIONS)/60._EB
   ALLOCATE(ML%PYROLYSIS_RANGE(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%PYROLYSIS_RANGE',IZERO)
   ML%PYROLYSIS_RANGE(1:N_REACTIONS)   = PYROLYSIS_RANGE(1:N_REACTIONS)
   ML%TMP_BOIL                         = BOILING_TEMPERATURE + TMPM
   ALLOCATE(ML%TMP_REF(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%TEMP_REF',IZERO)
   ML%TMP_REF(1:N_REACTIONS)           = REFERENCE_TEMPERATURE(1:N_REACTIONS) + TMPM
   ALLOCATE(ML%RATE_REF(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%RATE_REF',IZERO)
   ML%RATE_REF(1:N_REACTIONS)          = REFERENCE_RATE(1:N_REACTIONS)
   ALLOCATE(ML%NU_GAS(N_TRACKED_SPECIES,N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%NU_GAS',IZERO)
   ML%NU_GAS=0._EB
   ALLOCATE(ML%NU_GAS_P(N_SPECIES,N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%NU_GAS_P',IZERO)
   ML%NU_GAS_P=0._EB
   ALLOCATE(ML%NU_GAS_M(N_SPECIES,N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%NU_GAS_P',IZERO)
   ML%NU_GAS_M=0._EB
   ALLOCATE(ML%N_RESIDUE(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%N_RESIDUE',IZERO)
   ML%N_RESIDUE = 0
   ALLOCATE(ML%N_LPC(N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%N_LPC',IZERO)
   ML%N_LPC=0

   ! Decide which pyrolysis model to use

   IF (BOILING_TEMPERATURE<50000._EB) THEN
      ML%PYROLYSIS_MODEL = PYROLYSIS_LIQUID
   ELSEIF (N_REACTIONS > 0 .AND. SURFACE_OXIDATION_MODEL) THEN  ! Special model for surface oxidation of thin elements
      WALL_INCREMENT     = 1  ! Do pyrolysis every time step
      ML%PYROLYSIS_MODEL = PYROLYSIS_SURFACE_OXIDATION
      ML%N_O2 = 1._EB ! Enforce first order in oxygen mass fraction for this model
   ELSE
      ML%PYROLYSIS_MODEL = PYROLYSIS_SOLID
   ENDIF

   ! No pyrolysis

   IF (N_REACTIONS==0) ML%PYROLYSIS_MODEL = PYROLYSIS_NONE

   ! If oxygen is consumed in the charring process, set a global variable for
   ! use in calculating the heat release rate based on oxygen consumption

   IF (N_REACTIONS > 0 .AND. SURFACE_OXIDATION_MODEL) THEN
      IF (O2_INDEX <= 0) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(256): MATL ',TRIM(ID),', SURFACE_OXIDATION_MODEL set but OXYGEN not a defined species.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      CHAR_OXIDATION = .TRUE.
   ENDIF

   ! Conductivity and specific heat temperature ramps

   IF (ML%RAMP_K_S/='null') CALL GET_RAMP_INDEX(ML%RAMP_K_S,'TEMPERATURE',ML%I_RAMP_K_S)
   IF (ML%RAMP_C_S/='null') CALL GET_RAMP_INDEX(ML%RAMP_C_S,'TEMPERATURE',ML%I_RAMP_C_S)

   ! Determine A and E if REFERENCE_TEMPERATURE is specified

   CALL ChkMemErr('READ','ML%N_RESIDUE',IZERO)
   DO NR=1,ML%N_REACTIONS
      IF (ML%TMP_REF(NR) > 0._EB) THEN
         IF (ML%RATE_REF(NR) > 0._EB) THEN
            PEAK_REACTION_RATE = ML%RATE_REF(NR)
         ELSE
            PEAK_REACTION_RATE = 2._EB*ML%HEATING_RATE(NR)/ML%PYROLYSIS_RANGE(NR)
         ENDIF
         ML%E(NR) = EXP(1._EB)*PEAK_REACTION_RATE*R0*ML%TMP_REF(NR)**2/ML%HEATING_RATE(NR)
         ML%A(NR) = EXP(1._EB)*PEAK_REACTION_RATE*EXP(ML%E(NR)/(R0*ML%TMP_REF(NR)))
      ENDIF

      DO NN=1,MAX_MATERIALS
         IF (ML%RESIDUE_MATL_NAME(NN,NR)/='null') ML%N_RESIDUE(NR) = ML%N_RESIDUE(NR) + 1
      ENDDO
      DO NN=1,MAX_LPC
         IF (PART_ID(NN,NR)/='null') THEN
            ML%N_LPC(NR) = ML%N_LPC(NR) + 1
            LAGRANGIAN_PARTICLE_CLASS(NN)%INCLUDE_BOUNDARY_ONE_D_TYPE = .TRUE.
         ENDIF
      ENDDO
   ENDDO

   ALLOCATE(ML%RESIDUE_MATL_INDEX(MAXVAL(ML%N_RESIDUE),ML%N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%RESIDUE_MATL_INDEX',IZERO)
   ALLOCATE(ML%NU_RESIDUE(MAXVAL(ML%N_RESIDUE),ML%N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%NU_RESIDUE',IZERO)
   ML%NU_RESIDUE = 0._EB
   DO NR=1,ML%N_REACTIONS
      DO NN=1,ML%N_RESIDUE(NR)
         DO NNN=1,N_MATL
            IF (MATL_NAME(NNN)==ML%RESIDUE_MATL_NAME(NN,NR)) THEN
               ML%RESIDUE_MATL_INDEX(NN,NR) = NNN
               EXIT
            ENDIF
         ENDDO
         IF (ML%RESIDUE_MATL_INDEX(NN,NR)==0) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(257): MATL ',TRIM(ML%ID),' Residue ',TRIM(ML%RESIDUE_MATL_NAME(NN,NR)),' not defined.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         ML%NU_RESIDUE(NN,NR) = NU_MATL(NN,NR)
      ENDDO
   ENDDO

   ALLOCATE(ML%LPC_INDEX(MAXVAL(ML%N_LPC),ML%N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%LPC_INDEX',IZERO)
   ALLOCATE(ML%NU_LPC(MAXVAL(ML%N_LPC),ML%N_REACTIONS),STAT=IZERO)
   CALL ChkMemErr('READ','ML%NU_LPC',IZERO)
   ML%NU_LPC = 0._EB
   DO NR=1,ML%N_REACTIONS
      DO NN=1,ML%N_LPC(NR)
         DO NNN=1,N_LAGRANGIAN_CLASSES
            IF (LAGRANGIAN_PARTICLE_CLASS(NNN)%ID==PART_ID(NN,NR)) THEN
               IF (LAGRANGIAN_PARTICLE_CLASS(NNN)%MASSLESS_TRACER) THEN
                  WRITE(MESSAGE,'(A,A,A,A,A)') 'ERROR(258): PARTicle ',TRIM(PART_ID(NN,NR)),&
                                         ' corresponding to MATL ',TRIM(ML%ID),' cannot be MASSLESS.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               ML%LPC_INDEX(NN,NR) = NNN
               EXIT
            ENDIF
         ENDDO
         IF (ML%LPC_INDEX(NN,NR)==0) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(259): MATL ',TRIM(ML%ID),' PART_ID ',TRIM(PART_ID(NN,NR)),' not defined.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         ML%NU_LPC(NN,NR) = NU_PART(NN,NR)
         IF (ML%NU_LPC(NN,NR) <= 0._EB) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(260): MATL ',TRIM(ML%ID),' PART_ID ',TRIM(PART_ID(NN,NR)),' has a NU_PART<=0.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDDO
   ENDDO

   IF (ML%PYROLYSIS_MODEL == PYROLYSIS_LIQUID) ML%TMP_REF(1) = ML%TMP_BOIL

ENDDO READ_MATL_LOOP

! Check for duplicate names

IF (N_MATL>1) THEN
   DO N=1,N_MATL-1
      DO NN=N+1,N_MATL
         IF(MATL_NAME(N)==MATL_NAME(NN)) THEN
            WRITE(MESSAGE,'(A,A)') 'ERROR(261): Duplicate material name: ',TRIM(MATL_NAME(N))
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDDO
   ENDDO
ENDIF

CONTAINS

SUBROUTINE SET_MATL_DEFAULTS

A                      = -1._EB      ! 1/s
ABSORPTION_COEFFICIENT = 5.0E4_EB    ! 1/m, corresponds to 99.3% drop within 1E-4 m distance.
ADJUST_H               = .TRUE.
ALLOW_SHRINKING        = .TRUE.
ALLOW_SWELLING         = .TRUE.
BOILING_TEMPERATURE    = 50000._EB    ! C
CONDUCTIVITY           = 0.0_EB      ! W/m/K
CONDUCTIVITY_RAMP      = 'null'
DENSITY                = 0._EB       ! kg/m3
E                      = -1._EB      ! J/mol
EMISSIVITY             = 0.9_EB
FYI                    = 'null'
GAS_DIFFUSION_DEPTH    = 0.001_EB    ! m
HEAT_OF_COMBUSTION     = -1._EB      ! kJ/kg
HEAT_OF_REACTION       = -HUGE(1._EB) ! kJ/kg
ID                     = 'null'
MAX_REACTION_RATE      = HUGE(1._EB)
MW                     = -1._EB
N_REACTIONS            = 0
N_O2                   = 0._EB
NU_PART                = 0._EB
N_S                    = 1._EB
N_T                    = 0._EB
NU_SPEC                = 0._EB
NU_MATL                = 0._EB
PART_ID                = 'null'
REFERENCE_ENTHALPY     = 0._EB
REFERENCE_ENTHALPY_TEMPERATURE  = -273.15_EB
REFERENCE_RATE         = -1._EB
REFERENCE_TEMPERATURE  = -1000._EB
MATL_ID                = 'null'
SPECIFIC_HEAT          = 0.0_EB      ! kJ/kg/K
SPECIFIC_HEAT_RAMP     = 'null'
SPEC_ID                = 'null'
SURFACE_OXIDATION_MODEL= .FALSE.
HEATING_RATE           = 5._EB       ! K/min
PYROLYSIS_RANGE        = 80._EB      ! K or C

END SUBROUTINE SET_MATL_DEFAULTS

END SUBROUTINE READ_MATL



!> \brief Process solid phase material parameters and do some additional set-up work

SUBROUTINE PROC_MATL

USE MATH_FUNCTIONS, ONLY: EVALUATE_RAMP,INTERPOLATE1D_UNIFORM,LINEAR_SYSTEM_SOLVE
USE PHYSICAL_FUNCTIONS, ONLY: GET_TMP_REF
INTEGER :: I,N,NN,NL,NLPC,NS,NS2,NR,NR2,Z_INDEX(N_TRACKED_SPECIES,MAX_REACTIONS),IERR,ITMP,I_GRAD,&
           MATL_MATRIX_SIZE,REAC_COUNTER,TEMP_COUNTER,TEMP_MATL(N_MATL,MAX_REACTIONS)
REAL(EB) :: ANS,H_ADJUST,NU_INERT,H_R_CALC(0:I_MAX_TEMP),SUM_NU(N_MATL,MAX_REACTIONS),DTMP,THICKNESS,VOL,X1
INTEGER, ALLOCATABLE, DIMENSION(:) :: MATL_MATRIX_POINTER
REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: MATL_COEF_MATRIX
REAL(EB), ALLOCATABLE, DIMENSION(:) :: MATL_COEF_VECTOR,MATL_SOLUTION_VECTOR,RHO_H,RHO
TYPE(LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()
TYPE(MATERIAL_TYPE), POINTER :: ML2=>NULL()

PROC_MATL_LOOP: DO N=1,N_MATL

   ML => MATERIAL(N)

   ! Convert ML%NU_SPEC(I_ORDINAL,I_REACTION) and ML%SPEC_ID(I_ORDINAL,I_REACTION) to ML%NU_GAS(I_SPECIES,I_REACTION)
   !***** make reaction generalized for HRRPUA and MLRPUA and for BURNING RATE
   Z_INDEX = -1

   DO NR=1,ML%N_REACTIONS
      DO NS=1,MAX_SPECIES

         IF (TRIM(ML%SPEC_ID(NS,NR))=='null' .AND. ABS(ML%NU_SPEC(NS,NR))>TWO_EPSILON_EB) THEN
            WRITE(MESSAGE,'(A,A,A,I0,A,I0)') 'ERROR(262): MATL ',TRIM(MATL_NAME(N)),' requires a SPEC_ID for yield ',&
                 NS, 'of reaction ', NR
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (TRIM(ML%SPEC_ID(NS,NR))=='null') EXIT
         IF (NS==2 .AND. ML%PYROLYSIS_MODEL==PYROLYSIS_LIQUID) THEN
            WRITE(MESSAGE,'(A,A,A)') 'ERROR(263): MATL ',TRIM(MATL_NAME(N)),' can only specify one SPEC_ID for a liquid.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         DO NS2=1,N_TRACKED_SPECIES
            IF (TRIM(ML%SPEC_ID(NS,NR))==TRIM(SPECIES_MIXTURE(NS2)%ID)) THEN
               Z_INDEX(NS,NR) = NS2
               ML%NU_GAS(Z_INDEX(NS,NR),NR) = ML%NU_SPEC(NS,NR)
               EXIT
            ENDIF
         ENDDO
         ! Adjust burn rate if heat of combustion is different from the gas phase reaction value
         IF (ML%HEAT_OF_COMBUSTION(NS,NR) > 0._EB) THEN
            REAC_DO: DO NR2 = 1,N_REACTIONS
               IF(TRIM(ML%SPEC_ID(NS,NR))==TRIM(REACTION(NR2)%FUEL) .AND. REACTION(NR2)%HOC_COMPLETE > 0._EB) THEN
                  ML%ADJUST_BURN_RATE(Z_INDEX(NS,NR),NR) = ML%HEAT_OF_COMBUSTION(NS,NR) / REACTION(NR2)%HOC_COMPLETE
                  IF (ABS(ML%ADJUST_BURN_RATE(Z_INDEX(NS,NR),NR)-1._EB) > TWO_EPSILON_EB) THEN
                     DO NS2=1,N_SPECIES
                        IF (SPECIES_MIXTURE(Z_INDEX(NS,NR))%MASS_FRACTION(NS2) > 0._EB) &
                           ML%ADJUST_BURN_RATE_P(NS2,NR) = ML%ADJUST_BURN_RATE(Z_INDEX(NS,NR),NR)
                     ENDDO
                  ENDIF
                  EXIT REAC_DO
               ENDIF
            ENDDO REAC_DO
         ENDIF
         IF (Z_INDEX(NS,NR)==-1) THEN
            WRITE(MESSAGE,'(A,A,A,A,A)') 'ERROR(264): MATL ',TRIM(MATL_NAME(N)),' SPEC_ID ',TRIM(ML%SPEC_ID(NS,NR)),' not tracked.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDDO
      IF (ANY(ABS(ML%NU_GAS(:,NR))>TWO_EPSILON_EB)) THEN
         DO NS=1,N_TRACKED_SPECIES
            IF (ML%NU_GAS(NS,NR) > 0._EB) &
               ML%NU_GAS_P(:,NR) = ML%NU_GAS_P(:,NR) + ML%NU_GAS(NS,NR)*SPECIES_MIXTURE(NS)%MASS_FRACTION(:)
            IF (ML%NU_GAS(NS,NR) < 0._EB) &
               ML%NU_GAS_M(:,NR) = ML%NU_GAS_M(:,NR) - ML%NU_GAS(NS,NR)*SPECIES_MIXTURE(NS)%MASS_FRACTION(:)
         ENDDO
      ENDIF
   ENDDO

   ! If RAMPs are present, populate arrays for C_S and K_S

   IF (ML%I_RAMP_K_S > 0) THEN
      DO I=0,I_MAX_TEMP
         ML%K_S(I)=EVALUATE_RAMP(REAL(I,EB),ML%I_RAMP_K_S)
      ENDDO
   ENDIF

   ! Check units of specific heat

   IF (ML%I_RAMP_C_S > 0) THEN
      IF (.NOT.RAMPS(ML%I_RAMP_C_S)%DEP_VAR_UNITS_CONVERTED) THEN
         RAMPS(ML%I_RAMP_C_S)%INTERPOLATED_DATA(:) = RAMPS(ML%I_RAMP_C_S)%INTERPOLATED_DATA(:)*1000._EB/TIME_SHRINK_FACTOR
         RAMPS(ML%I_RAMP_C_S)%DEP_VAR_UNITS_CONVERTED = .TRUE.
      ENDIF
      IF (RAMPS(ML%I_RAMP_C_S)%DEPENDENT_DATA(1) > 10._EB .AND. MY_RANK==0) &
         WRITE(LU_ERR,'(A,A)') 'WARNING: SPECIFIC_HEAT units are kJ/kg/K check MATL ',TRIM(ID)
      DO I=0,I_MAX_TEMP
         ML%C_S(I)=EVALUATE_RAMP(REAL(I,EB),ML%I_RAMP_C_S)
      ENDDO
   ENDIF

   ! Check units of heat of reaction

   ALLOCATE(ML%H(0:I_MAX_TEMP)) ; ML%H(0) = 0._EB
   DO I=1,I_MAX_TEMP
      ML%H(I) = ML%H(I-1) + ML%C_S(I)
   ENDDO
   CALL INTERPOLATE1D_UNIFORM(0,ML%H,ML%REFERENCE_ENTHALPY_TEMPERATURE,ANS)
   H_ADJUST = ML%REFERENCE_ENTHALPY - ANS
   ML%H = ML%H + H_ADJUST

ENDDO PROC_MATL_LOOP

! Construct and solve linear system to adjust MATL enthalpies to conserve energy. For reaction i for MATL m the equation is
! H_m(ML%T_REF(i))+H_m,adj + H_R,m,i = Sum(nu_n (H_n(ML%T_REF(i)+H_n,adj))
! Where H_x,adj is the value to add or subtract from the existing temperature dependendent enthalpy (H_x) to balance the equation.
! x could be a SPEC or a MATL; however, here we assume H_SPEC(T) are all correct as changing H_SPEC might impact REAC which has
! already adjusted H_SPEC to get the RN%HEAT_OF_COMBUSTION.

TEMP_MATL = 0
TEMP_COUNTER = 0
SUM_NU = 0._EB

! Make sure all reactions have a TMP_REF

DO N=1,N_MATL
   ML=>MATERIAL(N)
   IF (ML%N_REACTIONS==0 .OR. .NOT. ML%ADJUST_H) CYCLE
   DO NR=1,ML%N_REACTIONS
      IF (ML%TMP_REF(NR) < 0._EB) CALL GET_TMP_REF(N,NR)
      SUM_NU(N,NR) = SUM(ML%NU_LPC(:,NR)) + SUM(ML%NU_GAS(:,NR)) + SUM(ML%NU_RESIDUE(:,NR))
      IF (SUM_NU(N,NR)-1._EB < -TWO_EPSILON_EB) THEN
         IF (MY_RANK==0) WRITE(LU_ERR,'(A,A)') 'WARNING: Sum of NU inputs sum to less than 1 for MATL ',TRIM(ML%ID)
         TEMP_COUNTER = TEMP_COUNTER + 1
         TEMP_MATL(N,NR) = TEMP_COUNTER
      ELSEIF (SUM_NU(N,NR) - 1._EB > 0.001_EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(265): MATL ',TRIM(ML%ID),' Sum of NU inputs more than 1.'
         CALL SHUTDOWN(MESSAGE); RETURN
      ENDIF
   ENDDO
ENDDO

! Count number of MATL that participate in reactions and build pointer vector for the solution array

ALLOCATE(MATL_MATRIX_POINTER(N_MATL))
MATL_MATRIX_POINTER=TEMP_COUNTER
MATL_MATRIX_SIZE=TEMP_COUNTER

DO N=1,N_MATL
   ML=>MATERIAL(N)
   IF (ML%N_REACTIONS==0 .OR. .NOT. ML%ADJUST_H) CYCLE
   IF (MATL_MATRIX_POINTER(N) == TEMP_COUNTER) THEN
      MATL_MATRIX_SIZE = MATL_MATRIX_SIZE + 1
      MATL_MATRIX_POINTER(N) = MATL_MATRIX_SIZE
   ENDIF
   DO NR=1,ML%N_REACTIONS
      DO NN=1,ML%N_RESIDUE(NR)
         IF (ABS(ML%NU_RESIDUE(NN,NR))>TWO_EPSILON_EB .AND. MATERIAL(ML%RESIDUE_MATL_INDEX(NN,NR))%ADJUST_H) THEN
            IF (MATL_MATRIX_POINTER(ML%RESIDUE_MATL_INDEX(NN,NR))>TEMP_COUNTER) CYCLE
            MATL_MATRIX_SIZE = MATL_MATRIX_SIZE + 1
            MATL_MATRIX_POINTER(ML%RESIDUE_MATL_INDEX(NN,NR)) = MATL_MATRIX_SIZE
         ENDIF
      ENDDO
      IF (ANY(ML%LPC_INDEX(:,NR)>0)) THEN
         DO NLPC =1,N_LAGRANGIAN_CLASSES
            IF (ML%LPC_INDEX(NLPC,NR)<=0) CYCLE
            LPC => LAGRANGIAN_PARTICLE_CLASS(ML%LPC_INDEX(NLPC,NR))
            IF (LPC%LIQUID_DROPLET) CYCLE
            ! get material for each layer
            SF=>SURFACE(LPC%SURF_INDEX)
            DO NL=1,SF%N_LAYERS
               DO NN=1,SF%N_MATL
                  IF (SURFACE(LPC%SURF_INDEX)%MATL_MASS_FRACTION(NL,NN) > 0._EB .AND. MATERIAL(SF%MATL_INDEX(NN))%ADJUST_H) THEN
                     IF (MATL_MATRIX_POINTER(SF%MATL_INDEX(NN))>TEMP_COUNTER) CYCLE
                     MATL_MATRIX_SIZE = MATL_MATRIX_SIZE + 1
                     MATL_MATRIX_POINTER(SF%MATL_INDEX(NN)) = MATL_MATRIX_SIZE
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDIF
   ENDDO
ENDDO

IF (MATL_MATRIX_SIZE == 0) RETURN

ALLOCATE(MATL_COEF_MATRIX(SUM(MATERIAL%N_REACTIONS),MATL_MATRIX_SIZE))
ALLOCATE(MATL_COEF_VECTOR(SUM(MATERIAL%N_REACTIONS)))
MATL_COEF_MATRIX = 0._EB
MATL_COEF_VECTOR = 0._EB

! This builds the matrix and vector.
REAC_COUNTER=0
DO N=1,N_MATL
   ML=>MATERIAL(N)
   IF (MATL_MATRIX_POINTER(N)==0) CYCLE
   DO NR=1,ML%N_REACTIONS
      REAC_COUNTER = REAC_COUNTER + 1
      MATL_COEF_MATRIX(REAC_COUNTER,MATL_MATRIX_POINTER(N))=1._EB
      IF (TEMP_MATL(N,NR)>0) MATL_COEF_MATRIX(REAC_COUNTER,TEMP_MATL(N,NR))=-(1._EB-SUM_NU(N,NR))
      CALL INTERPOLATE1D_UNIFORM(0,ML%H,ML%TMP_REF(NR),ANS)
      ! If there is unaccounted for mass treat it as the same enthalpy as the initial material
      MATL_COEF_VECTOR(REAC_COUNTER) = -ANS*SUM_NU(N,NR) - ML%H_R(NR,INT(ML%TMP_REF(NR)))
      DO NS=1,N_TRACKED_SPECIES
         IF (ABS(ML%NU_GAS(NS,NR))>TWO_EPSILON_EB) THEN
            CALL INTERPOLATE1D_UNIFORM(0,CPBAR_Z(:,NS),ML%TMP_REF(NR),ANS)
            MATL_COEF_VECTOR(REAC_COUNTER) = MATL_COEF_VECTOR(REAC_COUNTER) + ML%NU_GAS(NS,NR)*ANS*ML%TMP_REF(NR)
         ENDIF
      ENDDO
      DO NN=1,ML%N_RESIDUE(NR)
         IF (ABS(ML%NU_RESIDUE(NN,NR))>TWO_EPSILON_EB) THEN
            IF (ML%ADJUST_H) &
               MATL_COEF_MATRIX(REAC_COUNTER,MATL_MATRIX_POINTER(ML%RESIDUE_MATL_INDEX(NN,NR))) = -ML%NU_RESIDUE(NN,NR)
            CALL INTERPOLATE1D_UNIFORM(0,MATERIAL(ML%RESIDUE_MATL_INDEX(NN,NR))%H,ML%TMP_REF(NR),ANS)
            MATL_COEF_VECTOR(REAC_COUNTER) = MATL_COEF_VECTOR(REAC_COUNTER )+ ML%NU_RESIDUE(NN,NR)*ANS
         ENDIF
      ENDDO
      IF (ANY(ML%LPC_INDEX(:,NR)>0)) THEN
         DO NLPC =1,N_LAGRANGIAN_CLASSES
            IF (ML%LPC_INDEX(NLPC,NR)<=0) CYCLE
            LPC => LAGRANGIAN_PARTICLE_CLASS(ML%LPC_INDEX(NLPC,NR))
            LIQUID_IF: IF (LPC%LIQUID_DROPLET) THEN
               CALL INTERPOLATE1D_UNIFORM(LBOUND(SPECIES(LPC%Y_INDEX)%H_L,1),SPECIES(LPC%Y_INDEX)%H_L,ML%TMP_REF(NR),ANS)
               MATL_COEF_VECTOR(REAC_COUNTER) = MATL_COEF_VECTOR(REAC_COUNTER) + ML%NU_LPC(NLPC,NR)*ANS*ML%TMP_REF(NR)
            ELSE LIQUID_IF
               SF=>SURFACE(LPC%SURF_INDEX)
               SELECT CASE(SF%GEOMETRY)
                  CASE(SURF_CARTESIAN)   ; I_GRAD = 1
                  CASE(SURF_CYLINDRICAL) ; I_GRAD = 2
                  CASE(SURF_SPHERICAL)   ; I_GRAD = 3
               END SELECT
               ALLOCATE(RHO_H(SF%N_MATL))
               RHO_H = 0._EB
               ALLOCATE(RHO(SF%N_MATL))
               RHO = 0._EB
               ITMP = MIN(I_MAX_TEMP-1,INT(ML%TMP_REF(NR)))
               DTMP = ML%TMP_REF(NR)-REAL(ITMP,EB)
               THICKNESS = SUM(SF%LAYER_THICKNESS)
               X1 = THICKNESS+SF%INNER_RADIUS
               DO I=1,SF%N_LAYERS
                  VOL = X1**I_GRAD-(X1 - SF%LAYER_THICKNESS(I))**I_GRAD
                  X1 = X1 - SF%LAYER_THICKNESS(I)
                  MATL_RHO: DO NN=1,SF%N_MATL
                     IF (SF%MATL_MASS_FRACTION(I,NN)<=TWO_EPSILON_EB) CYCLE MATL_RHO
                     ML2  => MATERIAL(SF%MATL_INDEX(NN))
                     ANS = ML2%H(ITMP)+DTMP*(ML2%H(ITMP+1)-ML2%H(ITMP))
                     RHO_H(NN) = RHO_H(NN) + SF%MATL_MASS_FRACTION(I,NN) * SF%LAYER_DENSITY(I) * ANS * VOL
                     RHO(NN) = RHO(NN) + SF%MATL_MASS_FRACTION(I,NN) * SF%LAYER_DENSITY(I) * VOL
                  ENDDO MATL_RHO
               ENDDO
               RHO_H = RHO_H/RHO
               RHO = RHO/SUM(RHO)
               DO NN=1,SF%N_MATL
                  ML2  => MATERIAL(SF%MATL_INDEX(NN))
                  IF (ML2%ADJUST_H) &
                     MATL_COEF_MATRIX(REAC_COUNTER,MATL_MATRIX_POINTER(SF%MATL_INDEX(NN))) = -ML%NU_LPC(NLPC,NR)*RHO(NN)
                  MATL_COEF_VECTOR(REAC_COUNTER) = MATL_COEF_VECTOR(REAC_COUNTER)+ ML%NU_LPC(NLPC,NR)*RHO_H(NN)
               ENDDO
               DEALLOCATE(RHO_H)
               DEALLOCATE(RHO)
            ENDIF LIQUID_IF
         ENDDO
      ENDIF
   ENDDO
ENDDO

ALLOCATE (MATL_SOLUTION_VECTOR(MATL_MATRIX_SIZE))
MATL_SOLUTION_VECTOR = 0._EB
CALL LINEAR_SYSTEM_SOLVE(MATL_MATRIX_SIZE,REAC_COUNTER,MATL_COEF_MATRIX,MATL_COEF_VECTOR,MATL_SOLUTION_VECTOR,IERR)

SELECT CASE(IERR)
   CASE(103)
      WRITE(MESSAGE,'(A,A,A)') 'Singular matrix -- One or more groups of MATL ',&
                               'reactions are linear combinations. No adjustment to MATL reference enthalpies.'
      IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
      RETURN
   CASE(200)
      WRITE(MESSAGE,'(A,A,A)') 'Overdetermined matrix -- Total number of MATL ',&
                             'reactions > total number of MATL with reactions. Least square solution ',&
                             'performed to find MATL reference enthalpies.'
      IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
END SELECT

! Adjust MATL H
DO N=1,N_MATL
   ML => MATERIAL(N)
   IF (MATL_MATRIX_POINTER(N) > 0) THEN
      ML%H = MATL_SOLUTION_VECTOR(MATL_MATRIX_POINTER(N)) + ML%H
      CALL INTERPOLATE1D_UNIFORM(0,ML%H,ML%REFERENCE_ENTHALPY_TEMPERATURE,ML%REFERENCE_ENTHALPY)
   ENDIF
ENDDO
! Create H_R array
DO N=1,N_MATL
   ML=>MATERIAL(N)
   IF (.NOT. ML%ADJUST_H) CYCLE
   IF (ML%N_REACTIONS > 0) THEN
      DO NR=1,ML%N_REACTIONS
         NU_INERT = 1._EB-SUM(ML%NU_RESIDUE(:,NR))-SUM(ML%NU_GAS(:,NR))
         DO I=0,I_MAX_TEMP
           H_R_CALC(I) = -(1._EB-NU_INERT)*ML%H(I)
            DO NS=1,N_TRACKED_SPECIES
               IF (ABS(ML%NU_GAS(NS,NR))>TWO_EPSILON_EB) THEN
                  IF (I==0) H_R_CALC(I) = H_R_CALC(I) + ML%NU_GAS(NS,NR)*CPBAR_Z(I,NS)
                  IF (I>0)  H_R_CALC(I) = H_R_CALC(I) + ML%NU_GAS(NS,NR)*CPBAR_Z(I,NS)*REAL(I,EB)
               ENDIF
            ENDDO
            DO NN=1,ML%N_RESIDUE(NR)
               IF (ABS(ML%NU_RESIDUE(NN,NR))>TWO_EPSILON_EB) &
                  H_R_CALC(I) = H_R_CALC(I) + ML%NU_RESIDUE(NN,NR)*MATERIAL(ML%RESIDUE_MATL_INDEX(NN,NR))%H(I)
            ENDDO
         ENDDO
         CALL INTERPOLATE1D_UNIFORM(0,H_R_CALC,ML%TMP_REF(NR),ANS)
         H_ADJUST = ANS-ML%H_R(NR,INT(ML%TMP_REF(NR)))
         DO I=0,I_MAX_TEMP
            ML%H_R(NR,I) = ML%H_R(NR,I) - (ANS-H_R_CALC(I)) + H_ADJUST
         ENDDO
      ENDDO
   ENDIF
ENDDO

END SUBROUTINE PROC_MATL


!> \brief Read the SURF namelist lines

SUBROUTINE READ_SURF(QUICK_READ)

USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX
USE DEVICE_VARIABLES, ONLY : PROPERTY_TYPE
LOGICAL, INTENT(IN), OPTIONAL :: QUICK_READ
CHARACTER(LABEL_LENGTH) :: PART_ID,RAMP_MF(MAX_SPECIES),RAMP_Q(MAX_QDOTPP_REF),RAMP_V,RAMP_T,RAMP_T_I,&
                 MATL_ID(MAX_LAYERS,MAX_MATERIALS),PROFILE,BACKING,GEOMETRY,RAMP_EF,RAMP_PART,NAME_LIST(MAX_MATERIALS*MAX_LAYERS),&
                 SPEC_ID(MAX_SPECIES),RAMP_TMP_BACK,RAMP_TMP_GAS_BACK,RAMP_TMP_GAS_FRONT,&
                 RAMP_V_X,RAMP_V_Y,RAMP_V_Z,NEAR_WALL_TURBULENCE_MODEL,SURF_DEFAULT,&
                 RAMP_HEAT_TRANSFER_COEFFICIENT,RAMP_HEAT_TRANSFER_COEFFICIENT_BACK
CHARACTER(LABEL_LENGTH), DIMENSION(10) :: INIT_IDS
LOGICAL :: ADIABATIC,BURN_AWAY,FREE_SLIP,NO_SLIP,CONVERT_VOLUME_TO_MASS,HORIZONTAL,DIRICHLET_FRONT,DIRICHLET_BACK, BLOWING, &
           INERT_Q_REF
CHARACTER(LABEL_LENGTH) :: TEXTURE_MAP,HEAT_TRANSFER_MODEL,LEAK_PATH_ID(2)
CHARACTER(25) :: COLOR
REAL(EB) :: TAU_Q,TAU_V,TAU_T,TAU_MF(MAX_SPECIES),HRRPUA,MLRPUA,TEXTURE_WIDTH,TEXTURE_HEIGHT,VEL_T(2),&
            TAU_EF,E_COEFFICIENT,VOLUME_FLOW,&
            TMP_FRONT,TMP_FRONT_INITIAL,TMP_INNER,THICKNESS(MAX_LAYERS),VEL,VEL_BULK,INTERNAL_HEAT_SOURCE(MAX_LAYERS),&
            MASS_FLUX(MAX_SPECIES),Z0,PLE,CONVECTIVE_HEAT_FLUX,PARTICLE_MASS_FLUX,&
            TRANSPARENCY,EXTERNAL_FLUX,TMP_BACK,TMP_GAS_BACK,TMP_GAS_FRONT,MASS_FLUX_TOTAL,MASS_FLUX_VAR,&
            STRETCH_FACTOR(MAX_LAYERS),CONVECTION_LENGTH_SCALE,&
            MATL_MASS_FRACTION(MAX_LAYERS,MAX_MATERIALS),CELL_SIZE(MAX_LAYERS),CELL_SIZE_FACTOR(MAX_LAYERS),&
            EXTINCTION_TEMPERATURE,IGNITION_TEMPERATURE,HEAT_OF_VAPORIZATION,NET_HEAT_FLUX,LAYER_DIVIDE,&
            ROUGHNESS,RADIUS,INNER_RADIUS,LENGTH,WIDTH,DT_INSERT,HEAT_TRANSFER_COEFFICIENT,HEAT_TRANSFER_COEFFICIENT_BACK,&
            TAU_PART,EMISSIVITY,EMISSIVITY_BACK,EMISSIVITY_DEFAULT,SPREAD_RATE,XYZ(3),MINIMUM_LAYER_THICKNESS,VEL_GRAD,&
            MASS_FRACTION(MAX_SPECIES),MASS_TRANSFER_COEFFICIENT,NUSSELT_C0,NUSSELT_C1,NUSSELT_C2,NUSSELT_M,&
            PARTICLE_SURFACE_DENSITY,&
            MOISTURE_FRACTION(MAX_LAYERS),SURFACE_VOLUME_RATIO(MAX_LAYERS),MASS_PER_VOLUME(MAX_LAYERS),SHAPE_FACTOR,&
            SUM_D,&
            DRAG_COEFFICIENT,MINIMUM_BURNOUT_TIME,DELTA_TMP_MAX,BURN_DURATION,&
            REFERENCE_HEAT_FLUX(MAX_QDOTPP_REF),REFERENCE_HEAT_FLUX_TIME_INTERVAL,MINIMUM_SCALING_HEAT_FLUX,&
            MAXIMUM_SCALING_HEAT_FLUX,REFERENCE_THICKNESS(MAX_QDOTPP_REF),&
            AREA_MULTIPLIER,Z_0,PARTICLE_EXTRACTION_VELOCITY,RENODE_DELTA_T(MAX_LAYERS),NEAR_WALL_EDDY_VISCOSITY
INTEGER :: NPPC,N,IOS,NL,NN,NNN,NNNN,N_LIST,N_LIST2,LEAK_PATH(2),DUCT_PATH(2),RGB(3),NR,IL
INTEGER ::  N_LAYER_CELLS_MAX(MAX_LAYERS),VEG_LSET_FUEL_INDEX,SUBSTEP_POWER,INDEX_LIST(MAX_MATERIALS**2)
REAL(EB) :: VEG_LSET_IGNITE_TIME,VEG_LSET_QCON,VEG_LSET_ROS_HEAD,VEG_LSET_ROS_FLANK,VEG_LSET_ROS_BACK, &
            VEG_LSET_WIND_EXP,VEG_LSET_BETA,VEG_LSET_HT,VEG_LSET_SIGMA,VEG_LSET_ROS_00, &
            VEG_LSET_M1,VEG_LSET_M10,VEG_LSET_M100,VEG_LSET_MLW,VEG_LSET_MLH,VEG_LSET_SURF_LOAD,VEG_LSET_FIREBASE_TIME,&
            VEG_LSET_CHAR_FRACTION,VEL_PART,INIT_PER_AREA
LOGICAL :: DEFAULT,VEG_LSET_SPREAD,VEG_LSET_TAN2,TGA_ANALYSIS,COMPUTE_EMISSIVITY,&
           COMPUTE_EMISSIVITY_BACK,VARIABLE_THICKNESS,HT3D,THERM_THICK
LOGICAL, ALLOCATABLE, DIMENSION(:) :: DUPLICATE
! Ember generating variables
REAL(EB) :: EMBER_GENERATION_HEIGHT(2),EMBER_IGNITION_POWER_MEAN,EMBER_IGNITION_POWER_SIGMA,EMBER_TRACKING_RATIO,EMBER_YIELD

NAMELIST /SURF/ ADIABATIC,AREA_MULTIPLIER,BACKING,BLOWING,BURN_AWAY,BURN_DURATION,&
                CELL_SIZE,CELL_SIZE_FACTOR,COLOR,&
                CONVECTION_LENGTH_SCALE,CONVECTIVE_HEAT_FLUX,CONVERT_VOLUME_TO_MASS,DEFAULT,DELTA_TMP_MAX,DRAG_COEFFICIENT,&
                DT_INSERT,E_COEFFICIENT,&
                EMBER_GENERATION_HEIGHT,EMBER_IGNITION_POWER_MEAN,EMBER_IGNITION_POWER_SIGMA,EMBER_TRACKING_RATIO,EMBER_YIELD,&
                EMISSIVITY,EMISSIVITY_BACK,EXTERNAL_FLUX,EXTINCTION_TEMPERATURE,&
                FREE_SLIP,INERT_Q_REF,FYI,GEOMETRY,HEAT_OF_VAPORIZATION,HEAT_TRANSFER_COEFFICIENT,HEAT_TRANSFER_COEFFICIENT_BACK,&
                HEAT_TRANSFER_MODEL,HORIZONTAL,HRRPUA,VARIABLE_THICKNESS,HT3D,ID,IGNITION_TEMPERATURE,&
                INIT_IDS,INIT_PER_AREA,&
                INNER_RADIUS,INTERNAL_HEAT_SOURCE,LAYER_DIVIDE,&
                LEAK_PATH,LEAK_PATH_ID,LENGTH,MASS_FLUX,MASS_FLUX_TOTAL,MASS_FLUX_VAR,MASS_FRACTION,&
                MASS_TRANSFER_COEFFICIENT, &
                MATL_ID,MATL_MASS_FRACTION,MASS_PER_VOLUME,MINIMUM_BURNOUT_TIME,MINIMUM_LAYER_THICKNESS,MLRPUA,MOISTURE_FRACTION,&
                N_LAYER_CELLS_MAX,NEAR_WALL_EDDY_VISCOSITY,NEAR_WALL_TURBULENCE_MODEL,NET_HEAT_FLUX,&
                NO_SLIP,NPPC,NUSSELT_C0,NUSSELT_C1,NUSSELT_C2,NUSSELT_M,&
                PARTICLE_EXTRACTION_VELOCITY,PARTICLE_MASS_FLUX,PARTICLE_SURFACE_DENSITY,PART_ID,&
                PLE,PROFILE,RADIUS,RAMP_EF,RAMP_HEAT_TRANSFER_COEFFICIENT,RAMP_HEAT_TRANSFER_COEFFICIENT_BACK,RAMP_MF,&
                RAMP_PART,RAMP_Q,RAMP_T,RAMP_T_I,RAMP_TMP_BACK,RAMP_TMP_GAS_BACK,RAMP_TMP_GAS_FRONT,&
                RAMP_V,RAMP_V_X,RAMP_V_Y,RAMP_V_Z,&
                REFERENCE_HEAT_FLUX,REFERENCE_HEAT_FLUX_TIME_INTERVAL,MINIMUM_SCALING_HEAT_FLUX,MAXIMUM_SCALING_HEAT_FLUX,&
                REFERENCE_THICKNESS,&
                RENODE_DELTA_T,RGB,ROUGHNESS,SHAPE_FACTOR,SPEC_ID,&
                SPREAD_RATE,STRETCH_FACTOR,SUBSTEP_POWER,SURFACE_VOLUME_RATIO,&
                TAU_EF,TAU_MF,TAU_PART,TAU_Q,TAU_T,TAU_V,TEXTURE_HEIGHT,TEXTURE_MAP,TEXTURE_WIDTH,&
                TGA_ANALYSIS,TGA_FINAL_TEMPERATURE,TGA_HEATING_RATE,THICKNESS,&
                TMP_BACK,TMP_FRONT,TMP_FRONT_INITIAL,TMP_GAS_BACK,TMP_GAS_FRONT,TMP_INNER,TRANSPARENCY,&
                VEG_LSET_BETA,VEG_LSET_CHAR_FRACTION,VEG_LSET_FIREBASE_TIME,VEG_LSET_FUEL_INDEX,VEG_LSET_HT,VEG_LSET_IGNITE_TIME,&
                VEG_LSET_M1,VEG_LSET_M10,VEG_LSET_M100,VEG_LSET_MLW,VEG_LSET_MLH,VEG_LSET_QCON,&
                VEG_LSET_ROS_00,VEG_LSET_ROS_BACK,VEG_LSET_ROS_FLANK,VEG_LSET_ROS_HEAD,VEG_LSET_SIGMA,&
                VEG_LSET_SURF_LOAD,VEG_LSET_TAN2,VEG_LSET_WIND_EXP,&
                VEL,VEL_BULK,VEL_GRAD,VEL_PART,VEL_T,VOLUME_FLOW,WIDTH,XYZ,Z0,Z_0

! Count the SURF lines in the input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
N_SURF = 0

COUNT_SURF_LOOP: DO
   CALL CHECKREAD('SURF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_SURF_LOOP
   ID = 'null'
   HT3D = .FALSE.
   VARIABLE_THICKNESS = .FALSE.
   READ(LU_INPUT,SURF,ERR=34,IOSTAT=IOS)
   IF (ID=='null') THEN
      WRITE(MESSAGE,'(A)') 'ERROR(301): SURF line must have an ID.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (PRESENT(QUICK_READ) .AND. (HT3D .OR. VARIABLE_THICKNESS)) THEN
      N_HT3D_SURF_LINES = N_HT3D_SURF_LINES + 1
      HT3D_SURF_LIST(N_HT3D_SURF_LINES) = ID
   ENDIF
   N_SURF = N_SURF + 1
   34 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with SURF number ',N_SURF+1,', line number ',INPUT_FILE_LINE_NUMBER
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_SURF_LOOP

! Special case where SURF lines are scanned, looking for presence of HT3D

IF (PRESENT(QUICK_READ)) RETURN

! Allocate the SURFACE derived type, leaving space for SURF entries not defined explicitly by the user

N_SURF_RESERVED = 9
ALLOCATE(SURFACE(0:N_SURF+N_SURF_RESERVED),STAT=IZERO)
CALL ChkMemErr('READ','SURFACE',IZERO)

! Count the SURF lines in the input file

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
NN = 0 ; ID = 'null'
COUNT_SURF_LOOP_AGAIN: DO
   CALL CHECKREAD('SURF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_SURF_LOOP_AGAIN
   READ(LU_INPUT,SURF)
   NN = NN+1
   SURFACE(NN)%ID = ID
   DO NNN=1,NN-1
      IF (SURFACE(NNN)%ID==SURFACE(NN)%ID) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(302): SURF ID ',TRIM(SURFACE(NN)%ID),' is used more than once.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO
ENDDO COUNT_SURF_LOOP_AGAIN

! Add extra surface types to the list that has already been compiled

SURF_DEFAULT                 = 'INERT'
INERT_SURF_INDEX             = 0
OPEN_SURF_INDEX              = N_SURF + 1
MIRROR_SURF_INDEX            = N_SURF + 2
INTERPOLATED_SURF_INDEX      = N_SURF + 3
PERIODIC_SURF_INDEX          = N_SURF + 4
HVAC_SURF_INDEX              = N_SURF + 5
MASSLESS_TRACER_SURF_INDEX   = N_SURF + 6
DROPLET_SURF_INDEX           = N_SURF + 7
MASSLESS_TARGET_SURF_INDEX   = N_SURF + 8
PERIODIC_FLOW_ONLY_SURF_INDEX= N_SURF + 9

N_SURF = N_SURF + N_SURF_RESERVED

SURFACE(INERT_SURF_INDEX)%ID             = 'INERT'
SURFACE(OPEN_SURF_INDEX)%ID              = 'OPEN'
SURFACE(MIRROR_SURF_INDEX)%ID            = 'MIRROR'
SURFACE(INTERPOLATED_SURF_INDEX)%ID      = 'INTERPOLATED'
SURFACE(PERIODIC_SURF_INDEX)%ID          = 'PERIODIC'
SURFACE(HVAC_SURF_INDEX)%ID              = 'HVAC'
SURFACE(MASSLESS_TRACER_SURF_INDEX)%ID   = 'MASSLESS TRACER'
SURFACE(DROPLET_SURF_INDEX)%ID           = 'DROPLET'
SURFACE(MASSLESS_TARGET_SURF_INDEX)%ID   = 'MASSLESS TARGET'
SURFACE(PERIODIC_FLOW_ONLY_SURF_INDEX)%ID= 'PERIODIC FLOW ONLY'

SURFACE(0)%USER_DEFINED                               = .FALSE.
SURFACE(N_SURF-N_SURF_RESERVED+1:N_SURF)%USER_DEFINED = .FALSE.

! Read the SURF lines

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
READ_SURF_LOOP: DO N=0,N_SURF

   SF => SURFACE(N)

   ! Allocate arrays associated with the SURF line

   ALLOCATE(SF%MASS_FRACTION(1:N_TRACKED_SPECIES),STAT=IZERO)
   CALL ChkMemErr('READ','SURFACE',IZERO) ; SF%MASS_FRACTION = 0._EB
   ALLOCATE(SF%MASS_FLUX(1:N_TRACKED_SPECIES),STAT=IZERO)
   CALL ChkMemErr('READ','SURFACE',IZERO) ; SF%MASS_FLUX = 0._EB
   ALLOCATE(SF%RAMP(-N_SURF_RAMPS:N_TRACKED_SPECIES),STAT=IZERO)
   CALL ChkMemErr('READ','SURFACE',IZERO)

   ! Read the user defined SURF lines

   CALL SET_SURF_DEFAULTS

   IF (SF%USER_DEFINED) THEN
      CALL CHECKREAD('SURF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      READ(LU_INPUT,SURF)
   ENDIF

   ! Check to make sure that a DEFAULT SURF has an ID

   IF (DEFAULT) THEN
      IF (ID=='null') ID = 'DEFAULT SURF'
      SURF_DEFAULT = TRIM(ID)
   ENDIF

   ! Set up a dummy surface for VARIABLE_THICKNESS and HT3D. The properties will be changed later.

   If ((VARIABLE_THICKNESS .OR. HT3D) .AND. THICKNESS(1)>TWO_EPSILON_EB .AND. MATL_ID(1,1)/='null') SF%LINING = .TRUE.
   If ((VARIABLE_THICKNESS .OR. HT3D) .AND. THICKNESS(1)<TWO_EPSILON_EB) THICKNESS(1) = 0.1_EB
   If ((VARIABLE_THICKNESS .OR. HT3D) .AND. MATL_ID(1,1)=='null') MATL_ID(1,1) = MATERIAL(1)%ID

   ! Load RAMP parameters into appropriate array

   SF%RAMP(1:N_TRACKED_SPECIES)%ID   = RAMP_MF(1:N_TRACKED_SPECIES)
   SF%RAMP(1:N_TRACKED_SPECIES)%TAU  = TAU_MF(1:N_TRACKED_SPECIES)
   SF%RAMP(1:N_TRACKED_SPECIES)%TYPE = 'TIME'

   DO NN=1,MAX_QDOTPP_REF
      SF%RAMP(TIME_HEAT-NN+1)%ID    = RAMP_Q(NN)
      SF%RAMP(TIME_HEAT-NN+1)%TYPE  = 'TIME'
   ENDDO

   SF%RAMP(TIME_HEAT)%TAU = TAU_Q/TIME_SHRINK_FACTOR

   SF%RAMP(TIME_VELO)%ID  = RAMP_V ; SF%RAMP(TIME_VELO)%TAU    = TAU_V/TIME_SHRINK_FACTOR    ; SF%RAMP(TIME_VELO)%TYPE  = 'TIME'
   SF%RAMP(TIME_TEMP)%ID  = RAMP_T ; SF%RAMP(TIME_TEMP)%TAU    = TAU_T/TIME_SHRINK_FACTOR    ; SF%RAMP(TIME_TEMP)%TYPE  = 'TIME'
   SF%RAMP(TIME_EFLUX)%ID = RAMP_EF ; SF%RAMP(TIME_EFLUX)%TAU  = TAU_EF/TIME_SHRINK_FACTOR   ; SF%RAMP(TIME_EFLUX)%TYPE = 'TIME'
   SF%RAMP(TIME_PART)%ID  = RAMP_PART ; SF%RAMP(TIME_PART)%TAU = TAU_PART/TIME_SHRINK_FACTOR ; SF%RAMP(TIME_PART)%TYPE  = 'TIME'
   SF%RAMP(VELO_PROF_X)%ID = RAMP_V_X ; SF%RAMP(VELO_PROF_X)%TYPE= 'PROFILE'
   SF%RAMP(VELO_PROF_Y)%ID = RAMP_V_Y ; SF%RAMP(VELO_PROF_Y)%TYPE= 'PROFILE'
   SF%RAMP(VELO_PROF_Z)%ID = RAMP_V_Z ; SF%RAMP(VELO_PROF_Z)%TYPE= 'PROFILE'
   SF%RAMP(TIME_TGF)%ID = RAMP_TMP_GAS_FRONT ; SF%RAMP(TIME_TGF)%TYPE = 'TIME'
   SF%RAMP(TIME_TGB)%ID = RAMP_TMP_GAS_BACK  ; SF%RAMP(TIME_TGB)%TYPE = 'TIME'
   SF%RAMP(TIME_TB)%ID  = RAMP_TMP_BACK      ; SF%RAMP(TIME_TB)%TYPE  = 'TIME'

   ! Translate various forestry/vegetation terms into FDS parameters

   SF%PACKING_RATIO(:)        = 0._EB
   SF%SURFACE_VOLUME_RATIO(:) = SURFACE_VOLUME_RATIO(:)
   SF%MOISTURE_FRACTION(:)    = MOISTURE_FRACTION(:)
   SF%SHAPE_FACTOR            = SHAPE_FACTOR
   SF%DRAG_COEFFICIENT        = DRAG_COEFFICIENT

   IF (ANY(MOISTURE_FRACTION>TWO_EPSILON_EB) .OR. &
       ANY(MASS_PER_VOLUME>TWO_EPSILON_EB)   .OR. &
       ANY(SURFACE_VOLUME_RATIO>TWO_EPSILON_EB)) THEN

      ! Determine convective heat transfer coefficient based on element, not surface geometry

      IF (ANY(MASS_PER_VOLUME>0._EB) .AND. SURFACE_VOLUME_RATIO(1)>TWO_EPSILON_EB) &
         CONVECTION_LENGTH_SCALE = 4._EB/SURFACE_VOLUME_RATIO(1)

      ! Loop over layers and make adjustments to specified densities and moisture content

      LAYER_LOOP_2: DO NL=1,MAX_LAYERS

         ! Convert SURFACE_VOLUME_RATIO into a THICKNESS

         IF (SURFACE_VOLUME_RATIO(NL)>TWO_EPSILON_EB .AND. THICKNESS(NL)<0._EB .AND. .NOT.HT3D .AND. .NOT.VARIABLE_THICKNESS) THEN
            SELECT CASE(GEOMETRY)
               CASE('CARTESIAN')   ; THICKNESS(NL) = 1._EB/SURFACE_VOLUME_RATIO(NL)
               CASE('CYLINDRICAL') ; THICKNESS(NL) = 2._EB/SURFACE_VOLUME_RATIO(NL)
               CASE('SPHERICAL')   ; THICKNESS(NL) = 3._EB/SURFACE_VOLUME_RATIO(NL)
            END SELECT
         ENDIF

         IF (THICKNESS(NL) < 0._EB) EXIT LAYER_LOOP_2

         ! If MOISTURE is added, create adjustment to density of dry fuel component

         IF (MOISTURE_FRACTION(NL)>TWO_EPSILON_EB) THEN
            DO NN=1,MAX_MATERIALS
               IF (MATL_ID(NL,NN) == 'null') EXIT
               IF (MATL_MASS_FRACTION(NL,NN)<TWO_EPSILON_EB) EXIT
               DO NNN=1,N_MATL
                  IF (MATL_ID(NL,NN)==MATERIAL(NNN)%ID) EXIT
               ENDDO
               IF (MATERIAL(NNN)%RHO_S*MOISTURE_FRACTION(NL)/MATERIAL(MOISTURE_INDEX)%RHO_S < 1._EB) THEN
                  SF%DENSITY_ADJUST_FACTOR(NL,NN) = 1._EB / &
                     (1._EB-MATERIAL(NNN)%RHO_S*MOISTURE_FRACTION(NL)/MATERIAL(MOISTURE_INDEX)%RHO_S)
               ELSE
                  WRITE(MESSAGE,'(3A)') 'ERROR(303): MOISTURE_FRACTION on SURF ',TRIM(SF%ID),' exceeds theoretical limit.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
            ENDDO
         ENDIF

         ! If the user has specified a MASS_PER_VOLUME of this layer, invoke the Boundary Fuel Model

         IF (MASS_PER_VOLUME(NL)>TWO_EPSILON_EB) THEN
            SF%BOUNDARY_FUEL_MODEL = .TRUE.
            SUM_D = 0._EB
            DO NN=1,MAX_MATERIALS
               IF (MATL_ID(NL,NN) == 'null') EXIT
               DO NNN=1,N_MATL
                  IF (MATL_ID(NL,NN)==MATERIAL(NNN)%ID) EXIT
               ENDDO
               SUM_D = SUM_D + MATL_MASS_FRACTION(NL,NN)/MATERIAL(NNN)%RHO_S
            ENDDO
            SF%PACKING_RATIO(NL) = MASS_PER_VOLUME(NL)*SUM_D
            SF%KAPPA_S(NL) = SHAPE_FACTOR*SF%PACKING_RATIO(NL)*SURFACE_VOLUME_RATIO(NL)
            SF%DENSITY_ADJUST_FACTOR(NL,:) = SF%PACKING_RATIO(NL)*SF%DENSITY_ADJUST_FACTOR(NL,:)
            EMISSIVITY      = 1._EB
            EMISSIVITY_BACK = 1._EB
         ENDIF

         ! If the user has specified a MOISTURE_FRACTION for this layer, add a new material component and adjust other MFs

         IF (MOISTURE_FRACTION(NL)>0._EB) THEN
            MATL_ID(NL,NN) = 'MOISTURE'
            MATL_MASS_FRACTION(NL,NN) = MOISTURE_FRACTION(NL)/(1._EB+MOISTURE_FRACTION(NL))
            MATL_MASS_FRACTION(NL,1:NN-1) = MATL_MASS_FRACTION(NL,1:NN-1)*(1._EB-MATL_MASS_FRACTION(NL,NN))
         ENDIF

      ENDDO LAYER_LOOP_2

   ENDIF

   ! Look for special TGA_ANALYSIS=.TRUE. to indicate that only a TGA analysis is to be done

   IF (TGA_ANALYSIS) THEN
      IF (N_REACTIONS > 0) THEN
         IF (ANY(DUPLICATE_FUEL)) THEN
            WRITE(MESSAGE,'(A)') 'WARNING: TGA_ANALYSIS selected and one or more reactions use the same FUEL.'// &
                                 'The TGA output for HRR may not be correct.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         ENDIF
      ENDIF
      GEOMETRY = 'CARTESIAN'
      LENGTH   = 0.1
      WIDTH    = 0.1
      BACKING  = 'INSULATED'
      IF (THICKNESS(2)>0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(304): SURF ',TRIM(SF%ID),' One layer only for TGA_ANALYSIS=T.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      THICKNESS(1) = 1.E-6_EB
      HEAT_TRANSFER_COEFFICIENT = 1000._EB
      MINIMUM_LAYER_THICKNESS = 1.E-12_EB
      TGA_SURF_INDEX = N
      INITIAL_RADIATION_ITERATIONS = 0
      RADIATION = .FALSE.
   ENDIF

   ! Level set vegetation fire spread specific

   VEG_LSET_SPREAD = .FALSE.
   IF (VEG_LSET_IGNITE_TIME < 1.E6_EB .OR. VEG_LSET_FUEL_INDEX>0 .OR. &
       VEG_LSET_ROS_00>0._EB .OR. VEG_LSET_ROS_HEAD>0._EB) VEG_LSET_SPREAD = .TRUE.
   IF (VEG_LSET_SPREAD .AND. LEVEL_SET_MODE==0) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(305): SURF ',TRIM(ID),' indicates a level set simulation, but LEVEL_SET_MODE not set on MISC.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (VEG_LSET_FUEL_INDEX>0 .AND. LEVEL_SET_COUPLED_FIRE) HRRPUA = 1._EB  ! HRRPUA to be set properly later
   IF (VEG_LSET_ROS_00    >0 .AND. LEVEL_SET_COUPLED_FIRE) HRRPUA = 1._EB
   IF (VEG_LSET_ROS_HEAD  >0 .AND. LEVEL_SET_COUPLED_FIRE) HRRPUA = 1._EB

   SF%VEG_LSET_SPREAD       = VEG_LSET_SPREAD
   SF%VEG_LSET_ROS_00       = VEG_LSET_ROS_00       ! no-wind, no-slope RoS (m/s), Rothermel model
   SF%VEG_LSET_ROS_HEAD     = VEG_LSET_ROS_HEAD     ! head fire rate of spread (m/s), McArthur model
   SF%VEG_LSET_ROS_FLANK    = VEG_LSET_ROS_FLANK    ! flank fire rate of spread, McArthur model
   SF%VEG_LSET_ROS_BACK     = VEG_LSET_ROS_BACK     ! back fire rate of spread, McArthur model
   SF%VEG_LSET_WIND_EXP     = VEG_LSET_WIND_EXP     ! exponent on wind cosine in ROS formula
   SF%VEG_LSET_SIGMA        = VEG_LSET_SIGMA * 0.01 ! SAV for Farsite emulation in LSET converted to 1/cm
   SF%VEG_LSET_HT           = VEG_LSET_HT
   SF%VEG_LSET_BETA         = VEG_LSET_BETA
   SF%VEG_LSET_TAN2         = VEG_LSET_TAN2
   SF%VEG_LSET_IGNITE_T     = VEG_LSET_IGNITE_TIME
   SF%VEG_LSET_QCON         =-VEG_LSET_QCON*1000._EB ! convert from kW/m^2 to W/m^2
   SF%VEG_LSET_M1           = VEG_LSET_M1
   SF%VEG_LSET_M10          = VEG_LSET_M10
   SF%VEG_LSET_M100         = VEG_LSET_M100
   SF%VEG_LSET_MLW          = VEG_LSET_MLW
   SF%VEG_LSET_MLH          = VEG_LSET_MLH
   SF%VEG_LSET_FUEL_INDEX   = VEG_LSET_FUEL_INDEX
   SF%VEG_LSET_SURF_LOAD    = VEG_LSET_SURF_LOAD
   SF%VEG_LSET_CHAR_FRACTION= VEG_LSET_CHAR_FRACTION
   SF%VEG_LSET_FIREBASE_TIME= VEG_LSET_FIREBASE_TIME
   IF (VEG_LSET_FIREBASE_TIME<0._EB) SF%VEG_LSET_FIREBASE_TIME = 75600._EB/VEG_LSET_SIGMA

   IF (SF%VEG_LSET_FUEL_INDEX>0 .AND. COLOR=='null' .AND. ANY(RGB<0)) THEN
      SELECT CASE(SF%VEG_LSET_FUEL_INDEX)
         CASE(1)  ; RGB=(/255,254,212/)
         CASE(2)  ; RGB=(/255,253,102/)
         CASE(3)  ; RGB=(/236,212, 99/)
         CASE(4)  ; RGB=(/254,193,119/)
         CASE(5)  ; RGB=(/249,197, 92/)
         CASE(6)  ; RGB=(/217,196,152/)
         CASE(7)  ; RGB=(/170,155,127/)
         CASE(8)  ; RGB=(/229,253,214/)
         CASE(9)  ; RGB=(/162,191, 90/)
         CASE(10) ; RGB=(/114,154, 85/)
         CASE(11) ; RGB=(/235,212,253/)
         CASE(12) ; RGB=(/163,177,243/)
         CASE(13) ; RGB=(/  0,  0,  0/)
      END SELECT
   ENDIF

   ! Ember variables

   SF%EMBER_GENERATION_HEIGHT = (/MINVAL(EMBER_GENERATION_HEIGHT),MAXVAL(EMBER_GENERATION_HEIGHT)/)
   IF (SF%EMBER_GENERATION_HEIGHT(1)<0._EB) SF%EMBER_GENERATION_HEIGHT(1) = SF%EMBER_GENERATION_HEIGHT(2)
   SF%EMBER_IGNITION_POWER_MEAN = EMBER_IGNITION_POWER_MEAN
   SF%EMBER_IGNITION_POWER_SIGMA = EMBER_IGNITION_POWER_SIGMA
   SF%EMBER_TRACKING_RATIO = EMBER_TRACKING_RATIO
   SF%EMBER_YIELD = EMBER_YIELD

   ! Minimum and maximum time required to consume all the fuel

   SF%BURN_DURATION = BURN_DURATION
   SF%MINIMUM_BURNOUT_TIME = MINIMUM_BURNOUT_TIME

   ! If a RADIUS is specified, consider it the same as THICKNESS(1)

   IF (THICKNESS(1)<0._EB .AND. RADIUS>0._EB) THICKNESS(1) = RADIUS

   ! Check SURF parameters for potential problems

   LAYER_LOOP: DO IL=1,MAX_LAYERS
      IF (TMP_FRONT>-TMPM .AND. (MATL_ID(IL,1)/='null' .OR. VARIABLE_THICKNESS .OR. HT3D)) DIRICHLET_FRONT = .TRUE.
      IF (TMP_BACK >-TMPM .AND. (MATL_ID(IL,1)/='null' .OR. VARIABLE_THICKNESS .OR. HT3D)) DIRICHLET_BACK  = .TRUE.
      IF ((ADIABATIC.OR.NET_HEAT_FLUX<1.E12_EB.OR.ABS(CONVECTIVE_HEAT_FLUX)>TWO_EPSILON_EB) &
         .AND. MATL_ID(IL,1)/='null') THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(306): SURF ',TRIM(SF%ID),' cannot have a specified flux and a MATL_ID.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (THICKNESS(IL)<=0._EB .AND. .NOT.VARIABLE_THICKNESS .AND. .NOT.HT3D .AND. MATL_ID(IL,1)/='null') THEN
         WRITE(MESSAGE,'(A,A,A,I0)') 'ERROR(307): SURF ',TRIM(SF%ID),' must have a specified THICKNESS for Layer ',IL
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      ! If the user specifies a uniform CELL_SIZE, set the STRETCH_FACTOR to 1.
      IF (CELL_SIZE(IL)>0._EB) STRETCH_FACTOR(IL) = 1._EB
   ENDDO LAYER_LOOP

   IF ((GEOMETRY=='CYLINDRICAL' .OR. GEOMETRY=='SPHERICAL') .AND. RADIUS<0._EB .AND. THICKNESS(1)<0._EB) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(308): SURF ',TRIM(SF%ID),' needs a RADIUS or THICKNESS.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Identify the default SURF

   IF (ID==SURF_DEFAULT) DEFAULT_SURF_INDEX = N

   ! Pack SURF parameters into the SURFACE derived type

   SF                      => SURFACE(N)
   SF%VARIABLE_THICKNESS   = VARIABLE_THICKNESS
   SF%ADIABATIC            = ADIABATIC
   SF%AREA_MULTIPLIER      = AREA_MULTIPLIER
   SELECT CASE(BACKING)
      CASE('VOID')
         SF%BACKING        = VOID
      CASE('INSULATED')
         SF%BACKING        = INSULATED
      CASE('EXPOSED')
         SF%BACKING        = EXPOSED
      CASE DEFAULT
         WRITE(MESSAGE,'(5A)') 'ERROR(309): SURF ',TRIM(SF%ID),' BACKING ',TRIM(BACKING),' not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT
   IF (HT3D) THEN
      SF%HT_DIM = 3
      SF%INCLUDE_BOUNDARY_THR_D_TYPE = .TRUE.
      SOLID_HEAT_TRANSFER_3D         = .TRUE.
   ENDIF
   SF%BLOWING = BLOWING
   SF%DIRICHLET_FRONT        = DIRICHLET_FRONT
   SF%DIRICHLET_BACK         = DIRICHLET_BACK
   SF%BURN_AWAY            = BURN_AWAY

   COUNT_QDOTPP: DO NN=1,MAX_QDOTPP_REF
      IF (NN>1 .AND. RAMP_Q(NN)/='null' .AND. REFERENCE_HEAT_FLUX(NN) < 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(442): SURF ',TRIM(SF%ID),' has more RAMP_Q than REFERENCE_HEAT_FLUX.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (RAMP_Q(NN)=='null' .AND. REFERENCE_HEAT_FLUX(NN) > 0._EB) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(443): SURF ',TRIM(SF%ID),' has more REFERENCE_HEAT_FLUX than RAMP_Q.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (RAMP_Q(NN)/='null' .AND. REFERENCE_HEAT_FLUX(NN) > 0._EB) THEN
         IF (NN>1) THEN
            IF (REFERENCE_THICKNESS(NN) > 0._EB .NEQV. REFERENCE_THICKNESS(NN-1) > 0._EB) THEN
               WRITE(MESSAGE,'(A,A,A)') 'ERROR(444): SURF ',TRIM(SF%ID), ' If one REFERENCE_THICKNESS is set, all must be set.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (REFERENCE_THICKNESS(NN) > 0._EB .AND. REFERENCE_THICKNESS(NN-1) > 0._EB .AND. &
               ABS(1._EB-REFERENCE_THICKNESS(NN-1)/REFERENCE_THICKNESS(NN)) <= 0.1_EB .AND. &
               ABS(1._EB-REFERENCE_THICKNESS(NN-1)/REFERENCE_THICKNESS(NN)) > TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A,A,A,A)') 'WARNING: SURF ',TRIM(SF%ID),' REFERENCE_THICKNESS inputs are close in value.',&
                                        ' Use of nominal thicknesses are recommended.'
               IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
            ENDIF
            IF (REFERENCE_HEAT_FLUX(NN) < REFERENCE_HEAT_FLUX(NN-1)) THEN
               IF (REFERENCE_THICKNESS(NN) > 0._EB .AND. &
                   (REFERENCE_THICKNESS(NN) - REFERENCE_THICKNESS(NN-1)) <=  TWO_EPSILON_EB) THEN
                  WRITE(MESSAGE,'(A,A,A)') 'ERROR(445): SURF ',TRIM(SF%ID),&
                                           ' REFERENCE_HEAT_FLUX values must increase for each thickness.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IF (REFERENCE_THICKNESS(NN) > 0._EB .AND. REFERENCE_THICKNESS(NN) < REFERENCE_THICKNESS(NN-1)) THEN
                  WRITE(MESSAGE,'(A,A,A)') 'ERROR(446): SURF ',TRIM(SF%ID),&
                                           ' REFERENCE_THICKNESS values must increase for each new group of REFERENCE_HEAT_FLUX.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IF (SF%N_THICK_REF==0) SF%N_THICK_REF = 1
               SF%N_THICK_REF = SF%N_THICK_REF + 1
            ENDIF
         ENDIF
         SF%N_QDOTPP_REF = SF%N_QDOTPP_REF + 1
      ENDIF
   ENDDO COUNT_QDOTPP

   IF (SF%N_QDOTPP_REF > 0) THEN
      SF%N_THICK_REF = MAX(1,SF%N_THICK_REF)
      WHERE (REFERENCE_THICKNESS < 0._EB) REFERENCE_THICKNESS = THICKNESS(1)
      ALLOCATE(SF%REFERENCE_HEAT_FLUX(SF%N_QDOTPP_REF))
      SF%REFERENCE_HEAT_FLUX(1:SF%N_QDOTPP_REF) = REFERENCE_HEAT_FLUX(1:SF%N_QDOTPP_REF)*1000._EB
      SF%REFERENCE_HEAT_FLUX_TIME_INTERVAL = REFERENCE_HEAT_FLUX_TIME_INTERVAL

      ALLOCATE(SF%SPYRO_TH_FACTOR(SF%N_THICK_REF))
      ALLOCATE(SF%THICK2QREF(SF%N_THICK_REF,0:SF%N_QDOTPP_REF))
      NNN = 1
      NNNN = 0
      DO NN = 1,SF%N_QDOTPP_REF
         IF (NN > 1) THEN
            IF (REFERENCE_THICKNESS(NN) - REFERENCE_THICKNESS(NN-1) > TWO_EPSILON_EB) THEN
               NNN = NNN + 1
               NNNN = 0
            ENDIF
         ENDIF
         NNNN = NNNN + 1
         SF%THICK2QREF(NNN,0) = NNNN
         SF%THICK2QREF(NNN,NNNN) = NN
         SF%SPYRO_TH_FACTOR(NNN) = REFERENCE_THICKNESS(NN)/THICKNESS(1)
      ENDDO
   ENDIF
   SF%MINIMUM_SCALING_HEAT_FLUX         = MINIMUM_SCALING_HEAT_FLUX*1000._EB
   SF%MAXIMUM_SCALING_HEAT_FLUX         = MAXIMUM_SCALING_HEAT_FLUX*1000._EB
   SF%INERT_Q_REF = INERT_Q_REF
   SF%CONVECTIVE_HEAT_FLUX = 1000._EB*CONVECTIVE_HEAT_FLUX
   SF%CONV_LENGTH          = CONVECTION_LENGTH_SCALE
   SF%CONVERT_VOLUME_TO_MASS = CONVERT_VOLUME_TO_MASS
   IF (SF%CONVERT_VOLUME_TO_MASS .AND. TMP_FRONT<-TMPM) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(310): SURF ',TRIM(SF%ID),' must specify TMP_FRONT for CONVERT_VOLUME_TO_MASS.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SF%CONVERT_VOLUME_TO_MASS .AND. (SF%RAMP(TIME_VELO)%ID/='null' .OR. &
      SF%RAMP(VELO_PROF_X)%ID/='null' .OR. SF%RAMP(VELO_PROF_Y)%ID/='null' .OR. SF%RAMP(VELO_PROF_Z)%ID/='null')) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(311): SURF ',TRIM(SF%ID),' cannot use velocity RAMP with CONVERT_VOLUME_TO_MASS.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   SF%NET_HEAT_FLUX        = 1000._EB*NET_HEAT_FLUX
   SF%DELTA_TMP_MAX        = DELTA_TMP_MAX
   SF%DUCT_PATH            = DUCT_PATH
   SF%DT_INSERT            = DT_INSERT
   SF%E_COEFFICIENT        = E_COEFFICIENT
   IF (SF%HT_DIM>1 .AND. EMISSIVITY>=0._EB .AND. EMISSIVITY_BACK<0._EB) EMISSIVITY_BACK = EMISSIVITY
   SF%EMISSIVITY           = EMISSIVITY
   SF%EMISSIVITY_BACK      = EMISSIVITY_BACK
   IF (EMISSIVITY>=0._EB) SF%EMISSIVITY_SPECIFIED = .TRUE.
   IF (EMISSIVITY_BACK>=0._EB) SF%EMISSIVITY_BACK_SPECIFIED = .TRUE.
   SF%FIRE_SPREAD_RATE     = SPREAD_RATE / TIME_SHRINK_FACTOR
   SF%FREE_SLIP            = FREE_SLIP
   SF%NO_SLIP              = NO_SLIP
   SF%FYI                  = FYI
   SF%EXTERNAL_FLUX        = 1000._EB*EXTERNAL_FLUX
   SF%HORIZONTAL           = HORIZONTAL
   SF%INIT_IDS             = INIT_IDS
   IF (INIT_IDS(1)/='null') INIT_INVOKED_BY_SURF = .TRUE.
   SF%INIT_PER_AREA        = INIT_PER_AREA
   SF%INNER_RADIUS         = INNER_RADIUS
   SELECT CASE(GEOMETRY)
      CASE('CARTESIAN')
         SF%GEOMETRY       = SURF_CARTESIAN
         IF (SF%WIDTH>0._EB)                 SF%BACKING = INSULATED
      CASE('CYLINDRICAL')
         SF%GEOMETRY       = SURF_CYLINDRICAL
         IF (SF%INNER_RADIUS<TWO_EPSILON_EB) SF%BACKING = INSULATED
      CASE('SPHERICAL')
         SF%GEOMETRY       = SURF_SPHERICAL
         IF (SF%INNER_RADIUS<TWO_EPSILON_EB) SF%BACKING = INSULATED
      CASE DEFAULT
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(312): SURF ',TRIM(SF%ID),' GEOMETRY not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT

   SF%H_V = 1000._EB*HEAT_OF_VAPORIZATION
   SELECT CASE(HEAT_TRANSFER_MODEL)
      CASE DEFAULT
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(313): SURF ',TRIM(SF%ID),' HEAT_TRANSFER_MODEL not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      CASE('null')
         SF%HEAT_TRANSFER_MODEL = DEFAULT_HTC_MODEL
      CASE('LOGLAW','LOG LAW')
         SF%HEAT_TRANSFER_MODEL = LOGLAW_HTC_MODEL
      CASE('RAYLEIGH')
         SF%HEAT_TRANSFER_MODEL = RAYLEIGH_HTC_MODEL
      CASE('IMPINGING JET')
         SF%HEAT_TRANSFER_MODEL = IMPINGING_JET_HTC_MODEL
      CASE('FM')
         SF%HEAT_TRANSFER_MODEL = FM_HTC_MODEL
      CASE('UGENT')
         SF%HEAT_TRANSFER_MODEL = UGENT_HTC_MODEL
   END SELECT

   SF%HRRPUA               = 1000._EB*HRRPUA
   SF%MLRPUA               = MLRPUA
   IF ((SF%HRRPUA > 0._EB .OR. SF%MLRPUA > 0) .AND. N_REACTIONS == 0) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(314): SURF ',TRIM(SF%ID),' Must have a REAC line when using HRRPUA or MLRPUA.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   SF%LAYER_DIVIDE         = LAYER_DIVIDE

   IF (ANY(LEAK_PATH>=0) .AND. ANY(LEAK_PATH_ID/='null')) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(315): SURF ',TRIM(SF%ID),' should have only one LEAK_PATH and LEAK_PATH_ID.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   SF%LEAK_PATH            = LEAK_PATH
   SF%LEAK_PATH_ID         = LEAK_PATH_ID
   SF%LENGTH               = LENGTH
   SF%MASS_FLUX            = 0._EB
   SF%MASS_FLUX_VAR        = MASS_FLUX_VAR
   SF%MASS_FRACTION        = 0._EB
   SF%MINIMUM_LAYER_THICKNESS = MINIMUM_LAYER_THICKNESS
   SF%NPPC                 = NPPC
   SF%SUBSTEP_POWER        = SUBSTEP_POWER
   SF%PARTICLE_MASS_FLUX   = PARTICLE_MASS_FLUX
   SF%PARTICLE_SURFACE_DENSITY = PARTICLE_SURFACE_DENSITY
   ALLOCATE(SF%PARTICLE_INSERT_CLOCK(NMESHES),STAT=IZERO)
   CALL ChkMemErr('READ','PARTICLE_INSERT_CLOCK',IZERO)
   IF (SF%PARTICLE_MASS_FLUX>0._EB) THEN
      SF%PARTICLE_INSERT_CLOCK = SF%DT_INSERT
   ELSE
      SF%PARTICLE_INSERT_CLOCK = T_BEGIN
   ENDIF
   SF%PART_ID              = PART_ID
   SF%PLE                  = PLE
   SELECT CASE (PROFILE)
      CASE('null')
         SF%PROFILE        = 0
      CASE('ATMOSPHERIC')
         SF%PROFILE        = ATMOSPHERIC_PROFILE
      CASE('PARABOLIC')
         SF%PROFILE        = PARABOLIC_PROFILE
      CASE('BOUNDARY LAYER')
         SF%PROFILE        = BOUNDARY_LAYER_PROFILE
      CASE('RAMP')
         SF%PROFILE        = RAMP_PROFILE
   END SELECT

   IF (COLOR/='null') THEN
      IF (COLOR=='INVISIBLE') THEN
         TRANSPARENCY = 0._EB
      ELSE
         CALL COLOR2RGB(RGB,COLOR)
      ENDIF
   ENDIF
   IF (ANY(RGB< 0)) THEN
      RGB(1) = 255
      RGB(2) = 204
      RGB(3) = 102
   ENDIF
   IF (SF%ID=="OPEN") THEN
      RGB(1) = 255
      RGB(2) = 0
      RGB(3) = 255
   ENDIF
   SF%RENODE_DELTA_T       = RENODE_DELTA_T
   SF%RGB                  = RGB
   SF%ROUGHNESS            = ROUGHNESS
   SF%TRANSPARENCY         = TRANSPARENCY
   SF%TEXTURE_MAP          = TEXTURE_MAP
   SF%TEXTURE_WIDTH        = TEXTURE_WIDTH
   SF%TEXTURE_HEIGHT       = TEXTURE_HEIGHT
   SF%TMP_EXT              = EXTINCTION_TEMPERATURE + TMPM
   SF%TMP_IGN              = IGNITION_TEMPERATURE + TMPM
   SF%VEL                  = VEL
   SF%VEL_BULK             = VEL_BULK
   SF%VEL_GRAD             = VEL_GRAD
   SF%VEL_PART             = VEL_PART
   SF%VEL_T                = VEL_T
   SF%VOLUME_FLOW          = VOLUME_FLOW
   SF%WIDTH                = WIDTH
   SF%Z0                   = Z0
   IF (HEAT_TRANSFER_COEFFICIENT_BACK < 0._EB) HEAT_TRANSFER_COEFFICIENT_BACK=HEAT_TRANSFER_COEFFICIENT
   SF%H_FIXED              = HEAT_TRANSFER_COEFFICIENT
   SF%H_FIXED_B            = HEAT_TRANSFER_COEFFICIENT_BACK
   IF (RAMP_HEAT_TRANSFER_COEFFICIENT/='null') &
      CALL GET_RAMP_INDEX(RAMP_HEAT_TRANSFER_COEFFICIENT,'TIME',SF%RAMP_H_FIXED_INDEX)
   IF (RAMP_HEAT_TRANSFER_COEFFICIENT_BACK/='null') &
      CALL GET_RAMP_INDEX(RAMP_HEAT_TRANSFER_COEFFICIENT,'TIME',SF%RAMP_H_FIXED_B_INDEX)
   SF%NUSSELT_C0           = NUSSELT_C0
   SF%NUSSELT_C1           = NUSSELT_C1
   SF%NUSSELT_C2           = NUSSELT_C2
   SF%NUSSELT_M            = NUSSELT_M
   SF%HM_FIXED             = MASS_TRANSFER_COEFFICIENT
   SF%XYZ                  = XYZ
   SF%Z_0                  = Z_0
   SF%PARTICLE_EXTRACTION_VELOCITY  = PARTICLE_EXTRACTION_VELOCITY

   ! Roughness conversion

   IF (SF%ROUGHNESS>=0._EB .AND. SF%Z_0>=0._EB) THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(317): SURF ',TRIM(SF%ID),' Specify either ROUGHNESS or Z_0, not both'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SF%ROUGHNESS>=0._EB) THEN
      SF%Z_0 = SF%ROUGHNESS * EXP(-BTILDE_ROUGH*VON_KARMAN_CONSTANT) ! Z_0 \approx ROUGHNESS * 0.0306
   ENDIF
   IF (SF%Z_0>=0._EB) THEN
      SF%ROUGHNESS = SF%Z_0 * EXP(BTILDE_ROUGH*VON_KARMAN_CONSTANT)  ! ROUGHNESS \approx Z_0 * 32.6
   ENDIF
   IF (SF%ROUGHNESS<0._EB .AND. SF%Z_0<0._EB) THEN
      SF%ROUGHNESS = 0._EB
      SF%Z_0 = 0._EB
   ENDIF

   ! Near wall eddy viscosity model

   SELECT CASE (TRIM(NEAR_WALL_TURBULENCE_MODEL))
      CASE DEFAULT
         SF%NEAR_WALL_TURB_MODEL=WALE
      CASE ('WALE')
         SF%NEAR_WALL_TURB_MODEL=WALE
      CASE ('CONSMAG','CONSTANT SMAGORINSKY','VAN DRIEST')
         SF%NEAR_WALL_TURB_MODEL=CONSMAG
   END SELECT

   IF (NEAR_WALL_EDDY_VISCOSITY>0._EB) THEN
      SF%NEAR_WALL_TURB_MODEL = CONSTANT_EDDY_VISCOSITY
      SF%NEAR_WALL_EDDY_VISCOSITY = NEAR_WALL_EDDY_VISCOSITY
   ENDIF

   ! Convert inflowing MASS_FLUX_TOTAL to MASS_FLUX

   IF (MASS_FLUX_TOTAL >= 0._EB) THEN
      SF%MASS_FLUX_TOTAL = MASS_FLUX_TOTAL
   ELSE
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(318): SURF: ',TRIM(SF%ID),' MASS_FLUX_TOTAL is only for outflow. Use MASS_FLUX for inflow.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Error checking

   IF (ANY(MASS_FLUX>0._EB) .AND. ANY(MASS_FRACTION>0._EB))  THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(320): SURF ',TRIM(SF%ID),' cannot use both MASS_FLUX and MASS_FRACTION.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (ANY(MASS_FLUX<0._EB) .OR. PARTICLE_MASS_FLUX<0._EB)  THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(321): SURF ',TRIM(SF%ID),' MASS_FLUX cannot be less than zero.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (ANY(MASS_FLUX>0._EB) .AND. ABS(VEL)>TWO_EPSILON_EB)  THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(322): SURF ',TRIM(SF%ID),' cannot use both MASS_FLUX and VEL.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (ANY(MASS_FLUX>0._EB) .AND. ABS(MASS_FLUX_TOTAL)>TWO_EPSILON_EB)  THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(323): SURF ',TRIM(SF%ID),' cannot use both MASS_FLUX and MASS_FLUX_TOTAL.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (ABS(MASS_FLUX_TOTAL)>TWO_EPSILON_EB .AND. ABS(VEL)>TWO_EPSILON_EB)  THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(324): SURF ',TRIM(SF%ID),' cannot use both MASS_FLUX_TOTAL and VEL.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (ANY(MASS_FRACTION<0._EB))  THEN
      WRITE (MESSAGE,'(A,A,A)') 'ERROR(325): SURF ',TRIM(SF%ID),' cannot use a negative MASS_FRACTION.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (ANY(SF%RAMP(1:N_TRACKED_SPECIES)%ID/='null') .AND. (HRRPUA > 0._EB .OR. MLRPUA > 0._EB)) THEN
         WRITE (MESSAGE,'(A,A,A)') 'ERROR(326): SURF ',TRIM(SF%ID),' cannot use RAMP_MF with MLRPUA or HRRPUA.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SPEC_ID(1)=='null' .AND. (HRRPUA > 0._EB .OR. MLRPUA > 0._EB)) SPEC_ID(1)=REACTION(1)%FUEL
   IF (SPEC_ID(1)/='null' .AND. SPEC_ID(2)=='null' .AND. MASS_FRACTION(1)<TWO_EPSILON_EB .AND. &
      (HRRPUA > 0._EB .OR. MLRPUA > 0._EB)) MASS_FRACTION(1) = 1._EB
   IF (ANY(MASS_FLUX/=0._EB) .OR. ANY(MASS_FRACTION>0._EB)) THEN
      IF (SPEC_ID(1)=='null') THEN
         WRITE (MESSAGE,'(A,A,A)') 'ERROR(327): SURF ',TRIM(SF%ID),' must define SPEC_ID when using MASS_FLUX or MASS_FRACTION.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ELSE
         DO NN=1,MAX_SPECIES
            IF (TRIM(SPEC_ID(NN))=='null') EXIT
            DO NNN=1,N_TRACKED_SPECIES
               IF (TRIM(SPECIES_MIXTURE(NNN)%ID)==TRIM(SPEC_ID(NN))) THEN
                  SF%MASS_FLUX(NNN)    = MASS_FLUX(NN)
                  SF%MASS_FRACTION(NNN)= MASS_FRACTION(NN)
                  SF%RAMP(NNN)%TAU     = TAU_MF(NN)/TIME_SHRINK_FACTOR
                  SF%RAMP(NNN)%ID      = RAMP_MF(NN)
                  EXIT
               ENDIF
               IF (NNN==N_TRACKED_SPECIES) THEN
                  WRITE(MESSAGE,'(A,A,A,A,A)') 'ERROR(328): SURF ',TRIM(SF%ID),' SPEC ',TRIM(SPEC_ID(NN)),' not found.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
            ENDDO
         ENDDO
      ENDIF
      IF (SUM(SF%MASS_FRACTION) > TWO_EPSILON_EB) THEN
         IF (SUM(SF%MASS_FRACTION) > 1._EB) THEN
            WRITE (MESSAGE,'(A,A,A)') 'ERROR(329): SURF ',TRIM(SF%ID),' sum of mass fractions greater than 1.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (SF%MASS_FRACTION(1) > 0._EB) THEN
            WRITE (MESSAGE,'(A,A,A)') 'ERROR(330): SURF ',TRIM(SF%ID),' cannot use background species for MASS_FRACTION.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         SF%MASS_FRACTION(1) = 1._EB - SUM(SF%MASS_FRACTION(2:N_TRACKED_SPECIES))
      ENDIF
   ENDIF
   IF (SF%N_QDOTPP_REF > 0) THEN
      IF (SF%TMP_IGN>=50000._EB .OR. SF%RAMP(TIME_HEAT)%ID=='null' .OR. SF%HRRPUA <=0._EB) THEN
         WRITE (MESSAGE,'(A,A,A)') 'ERROR(332): SURF ',TRIM(SF%ID),&
                                    ' REFERENCE_HEAT_FLUX requires HRRPUA, IGNITION_TEMPERATURE, and RAMP_Q'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF

   ! Set various logical parameters

   IF (ABS(SF%VEL_T(1))>TWO_EPSILON_EB .OR. ABS(SF%VEL_T(2))>TWO_EPSILON_EB) SF%SPECIFIED_TANGENTIAL_VELOCITY = .TRUE.

   ! Count the number of layers for the surface, and compile a LIST of all material names and indices

   COMPUTE_EMISSIVITY      = .FALSE.
   COMPUTE_EMISSIVITY_BACK = .FALSE.
   IF (SF%EMISSIVITY     <0._EB) COMPUTE_EMISSIVITY      = .TRUE.
   IF (SF%EMISSIVITY_BACK<0._EB) COMPUTE_EMISSIVITY_BACK = .TRUE.

   SF%N_LAYERS = 0
   N_LIST = 0
   NAME_LIST = 'null'
   SF%THICKNESS  = 0._EB
   SF%LAYER_MATL_INDEX = 0
   SF%LAYER_DENSITY    = 0._EB
   INDEX_LIST = -1
   ALLOCATE(SF%LAYER_THICKNESS(MAX_LAYERS))    ; SF%LAYER_THICKNESS = 0._EB
   ALLOCATE(SF%HT3D_LAYER(MAX_LAYERS))         ; SF%HT3D_LAYER = .FALSE.
   ALLOCATE(SF%MIN_DIFFUSIVITY(MAX_LAYERS))
   ALLOCATE(SF%DDSUM(MAX_LAYERS))              ; SF%DDSUM = 0._EB
   ALLOCATE(SF%STRETCH_FACTOR(MAX_LAYERS))     ; SF%STRETCH_FACTOR = STRETCH_FACTOR
   ALLOCATE(SF%CELL_SIZE(MAX_LAYERS))          ; SF%CELL_SIZE = CELL_SIZE
   ALLOCATE(SF%CELL_SIZE_FACTOR(MAX_LAYERS))   ; SF%CELL_SIZE_FACTOR = CELL_SIZE_FACTOR
   ALLOCATE(SF%N_LAYER_CELLS_MAX(MAX_LAYERS))  ; SF%N_LAYER_CELLS_MAX = N_LAYER_CELLS_MAX
   ALLOCATE(SF%SMALLEST_CELL_SIZE(MAX_LAYERS)) ; SF%SMALLEST_CELL_SIZE = 0._EB
   ALLOCATE(SF%HEAT_SOURCE(MAX_LAYERS))        ; SF%HEAT_SOURCE = 0._EB

   COUNT_LAYERS: DO NL=1,MAX_LAYERS
      IF (THICKNESS(NL) < 0._EB) EXIT COUNT_LAYERS
      SF%N_LAYERS = SF%N_LAYERS + 1
      SF%LAYER_THICKNESS(NL) = THICKNESS(NL)
      SF%N_LAYER_MATL(NL) = 0
      EMISSIVITY = 0._EB
      COUNT_LAYER_MATL: DO NN=1,MAX_MATERIALS
         IF (MATL_ID(NL,NN) == 'null') EXIT COUNT_LAYER_MATL
         N_LIST = N_LIST + 1
         NAME_LIST(N_LIST) = MATL_ID(NL,NN)
         SF%N_LAYER_MATL(NL) = SF%N_LAYER_MATL(NL) + 1
         SF%MATL_ID(NL,NN) = MATL_ID(NL,NN)
         SF%MATL_MASS_FRACTION(NL,NN) = MATL_MASS_FRACTION(NL,NN)
         DO NNN=1,N_MATL
            IF (MATL_NAME(NNN)==NAME_LIST(N_LIST)) THEN
               INDEX_LIST(N_LIST) = NNN
               SF%LAYER_MATL_INDEX(NL,NN) = NNN
               SF%LAYER_DENSITY(NL) = SF%LAYER_DENSITY(NL) + SF%MATL_MASS_FRACTION(NL,NN) / &
                                      (SF%DENSITY_ADJUST_FACTOR(NL,NN)*MATERIAL(NNN)%RHO_S)
               EMISSIVITY = EMISSIVITY + MATERIAL(NNN)%EMISSIVITY*SF%MATL_MASS_FRACTION(NL,NN) / &
                                         (SF%DENSITY_ADJUST_FACTOR(NL,NN)*MATERIAL(NNN)%RHO_S)
            ENDIF
         ENDDO
         IF (INDEX_LIST(N_LIST)<0) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(333): SURF ',TRIM(SF%ID),' MATL_ID ',TRIM(NAME_LIST(N_LIST)),' not found.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDDO COUNT_LAYER_MATL
      IF (SF%LAYER_DENSITY(NL) > 0._EB) SF%LAYER_DENSITY(NL) = 1./SF%LAYER_DENSITY(NL)
      IF (COMPUTE_EMISSIVITY_BACK)        SF%EMISSIVITY_BACK = EMISSIVITY*SF%LAYER_DENSITY(NL)
      IF (NL==1 .AND. COMPUTE_EMISSIVITY) SF%EMISSIVITY      = EMISSIVITY*SF%LAYER_DENSITY(NL)
      SF%THICKNESS = SF%THICKNESS + SF%LAYER_THICKNESS(NL)
   ENDDO COUNT_LAYERS

   ! Set emissivity to default value if no other method applies.

   IF (SF%EMISSIVITY      < 0._EB) SF%EMISSIVITY      = EMISSIVITY_DEFAULT
   IF (SF%EMISSIVITY_BACK < 0._EB) SF%EMISSIVITY_BACK = EMISSIVITY_DEFAULT

   ! Define mass flux division point if the user does not specify. For all but
   ! surfaces with exposed backing, all pyrolyzed mass migrates to the front surface.

   IF (SF%LAYER_DIVIDE<0._EB .AND. .NOT.SF%BACKING==EXPOSED) SF%LAYER_DIVIDE = REAL(SF%N_LAYERS+1)

   ! Add residue materials

   DO I = 1,MAX_STEPS    ! repeat the residue loop to find chained reactions - allows MAX_STEPS steps
      N_LIST2 = N_LIST
      DO NN = 1,N_LIST2
         ML=>MATERIAL(INDEX_LIST(NN))
         DO NR=1,ML%N_REACTIONS
            DO NNN=1,ML%N_RESIDUE(NR)
               IF (ML%RESIDUE_MATL_NAME(NNN,NR) == 'null') CYCLE
               IF (ANY(NAME_LIST==ML%RESIDUE_MATL_NAME(NNN,NR))) CYCLE
               N_LIST = N_LIST + 1
               IF (N_LIST>MAX_MATERIALS_TOTAL) THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(334): SURF ',TRIM(SF%ID),' has too many materials.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               NAME_LIST (N_LIST) = ML%RESIDUE_MATL_NAME(NNN,NR)
               INDEX_LIST(N_LIST) = ML%RESIDUE_MATL_INDEX(NNN,NR)
            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ! Eliminate multiply counted materials from the list

   N_LIST2 = N_LIST
   WEED_MATL_LIST: DO NN=1,N_LIST
      DO NNN=1,NN-1
         IF (NAME_LIST(NNN)==NAME_LIST(NN)) THEN
            NAME_LIST(NN)  = 'null'
            INDEX_LIST(NN) = 0
            N_LIST2 = N_LIST2-1
            CYCLE WEED_MATL_LIST
         ENDIF
      ENDDO
   ENDDO WEED_MATL_LIST

   ! Allocate parameters indexed by layer

   IF (TMP_FRONT >= -TMPM) TMPMIN = MIN(TMPMIN,TMP_FRONT+TMPM)
   IF (TMP_BACK  >= -TMPM) TMPMIN = MIN(TMPMIN,TMP_BACK +TMPM)

   SF%N_MATL = N_LIST2
   THERM_THICK = .FALSE.

   SF%TMP_GAS_FRONT = TMP_GAS_FRONT + TMPM
   SF%TMP_GAS_BACK  = TMP_GAS_BACK  + TMPM

   IF (SF%LAYER_DENSITY(1) > 0._EB) THEN
      THERM_THICK = .TRUE.
      SF%TMP_INNER = TMP_INNER + TMPM
      IF (SF%TMP_INNER>=0._EB) THEN
         SF%TMP_FRONT = SF%TMP_INNER
         SF%TMP_BACK  = SF%TMP_INNER
         SF%RAMP(TIME_TEMP)%TAU = 0._EB
      ELSE
         SF%TMP_FRONT = TMP_FRONT + TMPM
         SF%TMP_BACK  = TMP_BACK  + TMPM
         SF%TMP_INNER = TMPA
      ENDIF
      ALLOCATE(SF%N_LAYER_CELLS(SF%N_LAYERS))            ! The number of cells in each layer
      ALLOCATE(SF%MATL_NAME(SF%N_MATL))                  ! The list of all material names associated with the surface
      ALLOCATE(SF%MATL_INDEX(SF%N_MATL))                 ! The list of all material indices associated with the surface
      SF%MATL_INDEX = 0
   ELSE
      SF%TMP_FRONT = TMP_FRONT + TMPM
      SF%TMP_INNER = SF%TMP_FRONT
      SF%TMP_BACK  = SF%TMP_FRONT
   ENDIF

   SF%TMP_FRONT_INITIAL = TMP_FRONT_INITIAL + TMPM

   ! Store the names and indices of all materials associated with the surface

   NNN = 0
   DO NN=1,N_LIST
      IF (NAME_LIST(NN)/='null') THEN
         NNN = NNN + 1
         SF%MATL_NAME(NNN)  = NAME_LIST(NN)
         SF%MATL_INDEX(NNN) = INDEX_LIST(NN)
      ENDIF
   ENDDO

   ! Check for contradictory inputs

   DO NN=1,SF%N_MATL
      ML => MATERIAL(SF%MATL_INDEX(NN))
      IF (ML%N_REACTIONS>0 .AND. SF%TMP_IGN<50000._EB) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(335): SURF ',TRIM(SF%ID),' cannot have a reacting MATL and IGNITION_TEMPERATURE.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO

   ! Specified source term

   SF%HEAT_SOURCE(1:SF%N_LAYERS) = 1000._EB*INTERNAL_HEAT_SOURCE(1:SF%N_LAYERS)

   ! Thermal boundary conditions

   IF (SF%ADIABATIC .AND. (SF%NET_HEAT_FLUX < 1.E12_EB .OR. ABS(SF%CONVECTIVE_HEAT_FLUX)>TWO_EPSILON_EB)) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(336): SURF ',TRIM(SF%ID),' cannot use both ADIABATIC and NET or CONVECTIVE_HEAT_FLUX.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SF%NET_HEAT_FLUX < 1.E12_EB .AND. ABS(SF%CONVECTIVE_HEAT_FLUX)>TWO_EPSILON_EB) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(337): SURF ',TRIM(SF%ID),' cannot use both NET and CONVECTIVE_HEAT_FLUX.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (SF%NET_HEAT_FLUX < 1.E12_EB .AND. TMP_FRONT >= -TMPM) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(338): SURF ',TRIM(SF%ID),' cannot use TMP_FRONT and NET_HEAT_FLUX.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (ABS(SF%CONVECTIVE_HEAT_FLUX)>TWO_EPSILON_EB .AND. TMP_FRONT >= -TMPM) SF%SET_H =.TRUE.

   SF%THERMAL_BC_INDEX = SPECIFIED_TEMPERATURE  ! Default thermal boundary condition

   IF (SF%ADIABATIC) THEN
      SF%THERMAL_BC_INDEX = NET_FLUX_BC
      SF%NET_HEAT_FLUX = 0._EB
      SF%EMISSIVITY = 1._EB
   ENDIF
   IF (SF%NET_HEAT_FLUX < 1.E12_EB)                 SF%THERMAL_BC_INDEX = NET_FLUX_BC
   IF (ABS(SF%CONVECTIVE_HEAT_FLUX)>TWO_EPSILON_EB) SF%THERMAL_BC_INDEX = CONVECTIVE_FLUX_BC
   IF (THERM_THICK)                                 SF%THERMAL_BC_INDEX = THERMALLY_THICK
   IF (SF%PROFILE==ATMOSPHERIC_PROFILE)             SF%THERMAL_BC_INDEX = INFLOW_OUTFLOW
   IF (RAMP_T_I /= 'null') THEN
      IF (HT3D) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(339): SURF ',TRIM(SF%ID),' RAMP_T_I cannot be used with HT3D.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (.NOT. SF%THERMAL_BC_INDEX == THERMALLY_THICK) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(340): SURF ',TRIM(SF%ID),' RAMP_T_I requires a thermally thick surface.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (TMP_FRONT_INITIAL > -TMPM-1._EB) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(341): SURF ',TRIM(SF%ID),' RAMP_T_I cannot be used with TMP_FRONT_INITIAL.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (TMP_FRONT > -TMPM-1._EB) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(342): SURF ',TRIM(SF%ID),' RAMP_T_I cannot be used with TMP_FRONT.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (TMP_BACK > -TMPM-1._EB) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(343): SURF ',TRIM(SF%ID),' RAMP_T_I cannot be used with TMP_BACK.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (TMP_INNER > -TMPM-1._EB) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(344): SURF ',TRIM(SF%ID),' RAMP_T_I cannot be used with TMP_INNER.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      CALL GET_RAMP_INDEX(RAMP_T_I,'T_I PROFILE',SF%RAMP_T_I_INDEX)
   ENDIF
   ! Boundary layer profile

   IF (SF%PROFILE==BOUNDARY_LAYER_PROFILE) THEN
      IF ( ABS(VEL_BULK)>ABS(VEL) ) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(345): SURF ',TRIM(SF%ID),' VEL_BULK must be less than or equal to VEL.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF

   ! Set convection length scale automatically for spheres. Set to 1 m for everything else.

   IF (SF%CONV_LENGTH<0._EB) THEN
      SELECT CASE(SF%GEOMETRY)
         CASE(SURF_SPHERICAL)
            SF%CONV_LENGTH = 2._EB*(SF%INNER_RADIUS+SF%THICKNESS)
         CASE(SURF_CYLINDRICAL)
            SF%CONV_LENGTH = 2._EB*SF%THICKNESS
         CASE DEFAULT
            SF%CONV_LENGTH = 1._EB
      END SELECT
   ENDIF

ENDDO READ_SURF_LOOP

! Get indices for all the RAMPs on the SURF line
ALLOCATE(DUPLICATE(N_RAMP+N_SURF*(N_TRACKED_SPECIES+N_SURF_RAMPS)))
DUPLICATE = .FALSE.
DO N=0,N_SURF
   SF=>SURFACE(N)
   DO NR=-N_SURF_RAMPS,N_TRACKED_SPECIES
      IF (NR==0) CYCLE
      IF (SF%RAMP(NR)%ID/='null') THEN
         CALL GET_RAMP_INDEX(SF%RAMP(NR)%ID,SF%RAMP(NR)%TYPE,SF%RAMP(NR)%INDEX)
         IF (SF%N_QDOTPP_REF > 0._EB .AND. NR <= TIME_HEAT .AND. .NOT. DUPLICATE(SF%RAMP(NR)%INDEX)) THEN
            N_CONE_RAMP = N_CONE_RAMP + 3
            DUPLICATE(SF%RAMP(NR)%INDEX)=.TRUE.
         ENDIF
      ELSE
         IF (SF%RAMP(NR)%TAU > 0._EB) SF%RAMP(NR)%INDEX = TANH_RAMP
         IF (SF%RAMP(NR)%TAU < 0._EB) SF%RAMP(NR)%INDEX = TSQR_RAMP
      ENDIF
   ENDDO
ENDDO
DEALLOCATE(DUPLICATE)

! Check for specified flux surfaces

IF ((ANY(SURFACE%THERMAL_BC_INDEX==CONVECTIVE_FLUX_BC) .OR. ANY(SURFACE%THERMAL_BC_INDEX==NET_FLUX_BC)) .AND. &
    ANY(LAGRANGIAN_PARTICLE_CLASS%LIQUID_DROPLET)) THEN
   WRITE(MESSAGE,'(A,A,A)') 'WARNING: Droplet heat transfer is not predicted when a droplet is on a SURF with a specified ',&
                            'ADIABATIC, NET_HEAT_FLUX, or CONVECTIVE_HEAT_FLUX.'
   IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
ENDIF

CONTAINS

SUBROUTINE SET_SURF_DEFAULTS

ADIABATIC               = .FALSE.
AREA_MULTIPLIER         = 1._EB
BACKING                 = 'EXPOSED'
BLOWING                 = .FALSE.
BURN_AWAY               = .FALSE.
BURN_DURATION           = 1.E6_EB
CELL_SIZE               = -1.0_EB
CELL_SIZE_FACTOR        = 1.0_EB
COLOR                   = 'null'
DIRICHLET_FRONT         = .FALSE.
DIRICHLET_BACK          = .FALSE.
CONVECTIVE_HEAT_FLUX    = 0._EB
CONVECTION_LENGTH_SCALE = -1._EB
CONVERT_VOLUME_TO_MASS  = .FALSE.
DELTA_TMP_MAX           = 10._EB
NET_HEAT_FLUX           = 1.E12_EB
DEFAULT                 = .FALSE.
DRAG_COEFFICIENT        = 2.8_EB
DT_INSERT               = 0.01_EB
DUCT_PATH               = 0
E_COEFFICIENT           = 0._EB
EMBER_GENERATION_HEIGHT = -1._EB
EMBER_IGNITION_POWER_MEAN = -1._EB
EMBER_IGNITION_POWER_SIGMA = 0.001_EB
EMBER_TRACKING_RATIO    = 100._EB
EMBER_YIELD             = -1._EB
EMISSIVITY              = -1._EB
EMISSIVITY_DEFAULT      = 0.9_EB
EMISSIVITY_BACK         = -1._EB
EXTERNAL_FLUX           = 0._EB
EXTINCTION_TEMPERATURE  = -273._EB
INERT_Q_REF             = .FALSE.
FREE_SLIP               = .FALSE.
FYI                     = 'null'
GEOMETRY                = 'CARTESIAN'
HEAT_OF_VAPORIZATION    = 0._EB
HEAT_TRANSFER_MODEL     = 'null'
HEAT_TRANSFER_COEFFICIENT = -1._EB
HEAT_TRANSFER_COEFFICIENT_BACK = -1._EB
RAMP_HEAT_TRANSFER_COEFFICIENT = 'null'
RAMP_HEAT_TRANSFER_COEFFICIENT = 'null'
MASS_TRANSFER_COEFFICIENT = -1._EB
HORIZONTAL              = .FALSE.
HRRPUA                  = 0._EB
ID                      = 'null'
IGNITION_TEMPERATURE    = 50000._EB
INIT_IDS                = 'null'
INIT_PER_AREA           = 0._EB
INNER_RADIUS            =  0._EB
INTERNAL_HEAT_SOURCE    = 0._EB
LAYER_DIVIDE            = -1._EB
LEAK_PATH               = -1
LEAK_PATH_ID            = 'null'
LENGTH                  = -1._EB
MASS_FLUX               = 0._EB
MASS_FLUX_TOTAL         = 0._EB
MASS_FLUX_VAR           = -1._EB
MASS_FRACTION           = 0._EB
MASS_PER_VOLUME         = 0._EB
MATL_ID                 = 'null'
MATL_MASS_FRACTION      = 0._EB
MATL_MASS_FRACTION(:,1) = 1._EB
MINIMUM_SCALING_HEAT_FLUX = 0._EB
MAXIMUM_SCALING_HEAT_FLUX = 1.E9_EB
MINIMUM_BURNOUT_TIME    = 1.E6_EB
MINIMUM_LAYER_THICKNESS = 1.E-6_EB
MLRPUA                  = 0._EB
MOISTURE_FRACTION       = 0._EB
N_LAYER_CELLS_MAX       = 999
NEAR_WALL_TURBULENCE_MODEL = 'null'
NEAR_WALL_EDDY_VISCOSITY = -1._EB
NO_SLIP                 = .FALSE.
NPPC                    = 1
NUSSELT_C0              = -1._EB
NUSSELT_C1              = -1._EB
NUSSELT_C2              = -1._EB
NUSSELT_M               = -1._EB
PARTICLE_EXTRACTION_VELOCITY = 1.E6_EB
PARTICLE_MASS_FLUX      = 0._EB
PARTICLE_SURFACE_DENSITY= -1._EB
PART_ID                 = 'null'
PLE                     = 0.3_EB
PROFILE                 = 'null'
RADIUS                  = -1._EB
RAMP_EF                 = 'null'
RAMP_HEAT_TRANSFER_COEFFICIENT = 'null'
RAMP_HEAT_TRANSFER_COEFFICIENT = 'null'
RAMP_MF                 = 'null'
RAMP_Q                  = 'null'
RAMP_V                  = 'null'
RAMP_T                  = 'null'
RAMP_T_I                = 'null'
RAMP_TMP_GAS_FRONT      = 'null'
RAMP_TMP_GAS_BACK       = 'null'
RAMP_TMP_BACK           = 'null'
RAMP_PART               = 'null'
RAMP_V_X                = 'null'
RAMP_V_Y                = 'null'
RAMP_V_Z                = 'null'
REFERENCE_HEAT_FLUX     = -1._EB
REFERENCE_HEAT_FLUX_TIME_INTERVAL = 1.0_EB
REFERENCE_THICKNESS     = -1._EB
RENODE_DELTA_T          = 2._EB
RGB                     = -1
IF (SIM_MODE==DNS_MODE) THEN
   ROUGHNESS = 0._EB
ELSE
   ROUGHNESS = -1._EB !4.5E-5_EB  ! meters, commercial steel
ENDIF
SHAPE_FACTOR            = 0.25_EB
SPEC_ID                 = 'null'
SPREAD_RATE             = -1._EB
STRETCH_FACTOR          = 2._EB
SUBSTEP_POWER           = 2
SURFACE_VOLUME_RATIO    = -1._EB
TAU_MF                  = TAU_DEFAULT
TAU_Q                   = TAU_DEFAULT
TAU_V                   = TAU_DEFAULT
TAU_T                   = TAU_DEFAULT
TAU_PART                = TAU_DEFAULT
TAU_EF                  = 0.001_EB
TEXTURE_MAP             = 'null'
TEXTURE_WIDTH           = 1._EB
TEXTURE_HEIGHT          = 1._EB
TGA_ANALYSIS            = .FALSE.
THICKNESS               = -1._EB
VARIABLE_THICKNESS      = .FALSE.
HT3D                    = .FALSE.
TMP_BACK                = -TMPM-1._EB
TMP_GAS_FRONT           = -TMPM-1._EB
TMP_GAS_BACK            = -TMPM-1._EB
TMP_FRONT               = -TMPM-1._EB
TMP_FRONT_INITIAL       = -TMPM-1._EB
TMP_INNER               = -TMPM-1._EB
TRANSPARENCY            = 1._EB
VEL                     = 0._EB
VEL_BULK                = 0._EB
VEL_GRAD                = -999999._EB
VEL_PART                = -999999._EB
VEL_T                   = 0._EB
VOLUME_FLOW             = 0._EB
WIDTH                   = -1._EB
XYZ                     = -1.E6_EB
Z0                      = 10._EB ! reference height (m)
Z_0                     = -1._EB ! aerodynamic roughness (m)

VEG_LSET_IGNITE_TIME    = 1.E9_EB
VEG_LSET_ROS_00         = 0.0_EB
VEG_LSET_ROS_HEAD       = 0.0_EB
VEG_LSET_ROS_FLANK      = 0.0_EB
VEG_LSET_ROS_BACK       = 0.0_EB
VEG_LSET_WIND_EXP       = 1.0_EB
VEG_LSET_TAN2           = .FALSE.
VEG_LSET_HT             = 1.0_EB
VEG_LSET_BETA           = 0.01_EB
VEG_LSET_SIGMA          = 5000.0_EB
VEG_LSET_QCON           = 0.0_EB
VEG_LSET_M1             = 0.03_EB
VEG_LSET_M10            = 0.04_EB
VEG_LSET_M100           = 0.05_EB
VEG_LSET_MLW            = 0.70_EB
VEG_LSET_MLH            = 0.70_EB
VEG_LSET_FUEL_INDEX     = 0
VEG_LSET_SURF_LOAD      = 0.3_EB !kg/m^2
VEG_LSET_FIREBASE_TIME  = -1.0_EB
VEG_LSET_CHAR_FRACTION  = 0.20_EB

END SUBROUTINE SET_SURF_DEFAULTS

END SUBROUTINE READ_SURF


!> \brief Process the SURF parameters

SUBROUTINE PROC_SURF_1

USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX
INTEGER :: N,NR,ILPC
TYPE (LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()

PROCESS_SURF_LOOP: DO N=0,N_SURF

   SF => SURFACE(N)

   IF (ANY(SF%LEAK_PATH_ID/='null'))  THEN

      IF(SF%LEAK_PATH_ID(1)=='AMBIENT') SF%LEAK_PATH(1)=0
      IF(SF%LEAK_PATH_ID(2)=='AMBIENT') SF%LEAK_PATH(2)=0
      ZONE_LOOP: DO NR = 1, N_ZONE
         IF(TRIM(SF%LEAK_PATH_ID(1))==TRIM(P_ZONE(NR)%ID)) SF%LEAK_PATH(1) = NR
         IF(TRIM(SF%LEAK_PATH_ID(2))==TRIM(P_ZONE(NR)%ID)) SF%LEAK_PATH(2) = NR
         IF(SF%LEAK_PATH(1) >=0 .AND. SF%LEAK_PATH(2)>=0) EXIT ZONE_LOOP
      ENDDO ZONE_LOOP

      IF (SF%LEAK_PATH(1)==-1) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(346): SURF ',TRIM(SF%ID),' ZONE ID for LEAK_PATH_ID(1) not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (SF%LEAK_PATH(2)==-1) THEN
         WRITE(MESSAGE,'(A,A,A)') 'ERROR(347): SURF ',TRIM(SF%ID),' ZONE ID for LEAK_PATH_ID(2) not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

   ENDIF

   IF (SF%LEAK_PATH(2) < SF%LEAK_PATH(1)) THEN
      ILPC = SF%LEAK_PATH(2)
      SF%LEAK_PATH(2)      = SF%LEAK_PATH(1)
      SF%LEAK_PATH(1)      = ILPC
   ENDIF

   IF (SF%LEAK_PATH(1)==SF%LEAK_PATH(2) .AND. SF%LEAK_PATH(1)>=0) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(348): SURF ',TRIM(SF%ID),' cannot set the same ZONE for each leakage path.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   IF (SF%LEAK_PATH(1)>N_ZONE .OR. SF%LEAK_PATH(2)>N_ZONE) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(349): SURF ',TRIM(SF%ID),' LEAK_PATH greater than number of ZONEs.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Look for particle classes that use SURF for property info

   DO ILPC=1,N_LAGRANGIAN_CLASSES

      LPC=>LAGRANGIAN_PARTICLE_CLASS(ILPC)

      IF (LPC%SURF_ID==SF%ID) THEN
         LPC%SURF_INDEX = N

         IF (.NOT.LPC%SOLID_PARTICLE) CYCLE
         IF (LPC%DRAG_LAW==SCREEN_DRAG) CYCLE

         SELECT CASE (SF%GEOMETRY)
            CASE(SURF_CARTESIAN)
               IF (SF%THICKNESS<=0._EB) THEN
                  WRITE(MESSAGE,'(A,A,A)') 'ERROR(350): SURF ',TRIM(SF%ID),' needs a THICKNESS.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IF (.NOT. LPC%DRAG_LAW==POROUS_DRAG) THEN
                  IF (SF%LENGTH<=0._EB) THEN
                     WRITE(MESSAGE,'(A,A,A)') 'ERROR(351): SURF ',TRIM(SF%ID),' needs a LENGTH.'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ENDIF
                  IF (SF%WIDTH<=0._EB) THEN
                     WRITE(MESSAGE,'(A,A,A)') 'ERROR(352): SURF ',TRIM(SF%ID),' needs a WIDTH.'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ENDIF
               ENDIF
            CASE(SURF_CYLINDRICAL)
               IF (.NOT. LPC%DRAG_LAW==POROUS_DRAG) THEN
                  IF (SF%LENGTH <0._EB) THEN
                     WRITE(MESSAGE,'(A,A,A)') 'ERROR(351): SURF ',TRIM(SF%ID),' needs a LENGTH'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ENDIF
               ENDIF
         END SELECT
      ENDIF
   ENDDO

ENDDO PROCESS_SURF_LOOP

! If a particle class uses a SURF line, make sure the SURF ID exists

DO ILPC=1,N_LAGRANGIAN_CLASSES
   LPC=>LAGRANGIAN_PARTICLE_CLASS(ILPC)
   IF (LPC%SURF_INDEX<0) THEN
      WRITE(MESSAGE,'(5A)') 'ERROR(353): PART ',TRIM(LPC%ID),' SURF_ID ',TRIM(LPC%SURF_ID),' not found.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO

END SUBROUTINE PROC_SURF_1


!> \brief Process the SURF parameters

SUBROUTINE PROC_SURF_2
USE MATH_FUNCTIONS, ONLY: INTERPOLATE1D_UNIFORM
USE PHYSICAL_FUNCTIONS, ONLY: Q_REF_FIT
INTEGER :: ILPC,N,NN,NNN,NL,N_LIST,NLPC,NR,NS
LOGICAL :: INDEX_LIST(MAX_LPC)
REAL(EB) :: R_L(0:MAX_LAYERS), FUEL_MF,HRRPUA,E
REAL(EB), ALLOCATABLE, DIMENSION(:) :: Q_REF_INT
INTEGER  :: I_GRAD,I_CONE_RAMP,E_PTR
INTEGER  :: PROCESSED(N_RAMP)
LOGICAL :: BURNING,BLOWING,SUCKING
TYPE(RAMPS_TYPE),POINTER :: RP=>NULL(),RP_E2T=>NULL(),RP_INT=>NULL(),RP_QREF=>NULL()
TYPE(LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()

PROCESSED = 0
I_CONE_RAMP = N_RAMP

PROCESS_SURF_LOOP: DO N=0,N_SURF

   SF => SURFACE(N)
   IF (SF%THERMAL_BC_INDEX==THERMALLY_THICK) SF%INCLUDE_BOUNDARY_ONE_D_TYPE = .TRUE.

   SELECT CASE(SF%GEOMETRY)
      CASE(SURF_CARTESIAN)    ; I_GRAD = 1
      CASE(SURF_CYLINDRICAL)  ; I_GRAD = 2
      CASE(SURF_SPHERICAL)    ; I_GRAD = 3
   END SELECT

   ! Particle Information

   SF%PART_INDEX = 0
   IF (SF%PART_ID/='null') THEN
      DO ILPC=1,N_LAGRANGIAN_CLASSES
         LPC=>LAGRANGIAN_PARTICLE_CLASS(ILPC)
         IF (LPC%ID==SF%PART_ID)  SF%PART_INDEX = ILPC
      ENDDO
      IF (SF%PART_INDEX==0) THEN
         WRITE(MESSAGE,'(5A)') 'ERROR(354): SURF ',TRIM(SF%ID),' PART_ID ',TRIM(SF%PART_ID),' not found.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      PARTICLE_FILE=.TRUE.
   ENDIF

   ! Determine if surface has internal radiation

   SF%INTERNAL_RADIATION = .FALSE.
   IF (RADIATION) THEN
      DO NL=1,SF%N_LAYERS
         IF (SF%KAPPA_S(NL)>0._EB) SF%INTERNAL_RADIATION = .TRUE.
         DO NN =1,SF%N_LAYER_MATL(NL)
            ML => MATERIAL(SF%LAYER_MATL_INDEX(NL,NN))
            IF (ML%KAPPA_S<5.0E4_EB) SF%INTERNAL_RADIATION = .TRUE.
         ENDDO
      ENDDO
   ENDIF

   ! Internal radiation only allowed for Cartesian geometry

   IF (SF%INTERNAL_RADIATION .AND. .NOT.SF%GEOMETRY==SURF_CARTESIAN) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(355): SURF ',TRIM(SF%ID),' not Cartesian and cannot have a MATL with an ABSORPTION_COEFFICIENT.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! In case of internal radiation, do not allow zero-emissivity

   IF (SF%INTERNAL_RADIATION) THEN
      DO NL=1,SF%N_LAYERS
         DO NN =1,SF%N_LAYER_MATL(NL)
            ML => MATERIAL(SF%LAYER_MATL_INDEX(NL,NN))
            IF (ML%EMISSIVITY == 0._EB) THEN
               WRITE(MESSAGE,'(5A)') 'ERROR(356): SURF ',TRIM(SF%ID),' zero emissivity of MATL ',&
                                     TRIM(MATL_NAME(SF%LAYER_MATL_INDEX(NL,NN))),' inconsistent with internal radiation.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDDO
      ENDDO
   ENDIF

   ! Determine if the surface is combustible/burning.

   SF%PYROLYSIS_MODEL = PYROLYSIS_NONE
   BURNING  = .FALSE.
   DO NL=1,SF%N_LAYERS
      DO NN=1,SF%N_LAYER_MATL(NL)
         NNN = SF%LAYER_MATL_INDEX(NL,NN)
         ML => MATERIAL(NNN)
         IF (ML%PYROLYSIS_MODEL/=PYROLYSIS_NONE) THEN
            SF%PYROLYSIS_MODEL = PYROLYSIS_PREDICTED
            IF (N_REACTIONS>0) THEN
               DO NR = 1,N_REACTIONS
                  IF (ANY(ML%NU_SPEC(REACTION(NR)%FUEL_SMIX_INDEX,:)>0._EB)) THEN
                     BURNING = .TRUE.
                     SF%RAMP(TIME_HEAT)%TAU = 0._EB
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ENDDO

   IF (SF%HRRPUA>0._EB .OR. SF%MLRPUA>0._EB) THEN
      IF (SF%PYROLYSIS_MODEL==PYROLYSIS_PREDICTED) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(357): SURF ',TRIM(SF%ID),' has a specified HRRPUA or MLRPUA plus another pyrolysis model.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      BURNING = .TRUE.
      SF%PYROLYSIS_MODEL = PYROLYSIS_SPECIFIED
   ENDIF

   IF (BURNING .AND. N_REACTIONS==0) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(314): SURF ',TRIM(SF%ID),' indicates burning, but there is no REAC line.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Make decisions based on whether there is forced ventilation at the surface

   BLOWING  = .FALSE.
   SUCKING  = .FALSE.
   IF (SF%VEL<0._EB .OR. SF%VOLUME_FLOW<0._EB .OR. SF%MASS_FLUX_TOTAL < 0._EB) BLOWING = .TRUE.
   IF (SF%VEL>0._EB .OR. SF%VOLUME_FLOW>0._EB .OR. SF%MASS_FLUX_TOTAL > 0._EB) SUCKING = .TRUE.
   IF (BLOWING .OR. SUCKING) SF%SPECIFIED_NORMAL_VELOCITY = .TRUE.
   IF (SUCKING) SF%FREE_SLIP = .TRUE.

   IF (BURNING .AND. (BLOWING .OR. SUCKING)) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(359): SURF ',TRIM(SF%ID),' cannot have a specified velocity or volume flux.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Neumann for normal component of velocity

   IF (SF%VEL_GRAD > -999998._EB) THEN
       SF%SPECIFIED_NORMAL_GRADIENT = .TRUE.
       SF%SPECIFIED_NORMAL_VELOCITY = .FALSE.
   ENDIF

   ! Set predefined HRRPUA or MLRPUA

   BURNING_IF: IF (BURNING) THEN
      HRRPUA_MLRPUA_IF: IF (SF%HRRPUA > 0._EB .OR. SF%MLRPUA > 0._EB) THEN
         H_OR_M_IF: IF (SF%HRRPUA>0._EB) THEN
            IF (ANY(SF%MASS_FRACTION > 0._EB)) THEN
               FUEL_MF = 0._EB
               DO NS=1,N_TRACKED_SPECIES
                  IF (SF%MASS_FRACTION(NS) > 0._EB) THEN
                     IF (ANY(REAC_FUEL==SPECIES_MIXTURE(NS)%ID)) THEN
                        NR = FINDLOC(REAC_FUEL,SPECIES_MIXTURE(NS)%ID,1)
                        RN => REACTION(NR)
                        IF (DUPLICATE_FUEL(NR)) THEN
                           WRITE(MESSAGE,'(5A)') 'ERROR(360): SURF ',TRIM(SF%ID),' uses HRRPUA but SPEC ', &
                                                TRIM(SPECIES_MIXTURE(NS)%ID),' is the FUEL for more than one REACtion.'
                           CALL SHUTDOWN(MESSAGE) ; RETURN
                        ENDIF
                        SF%MASS_FLUX(NS) = SF%MASS_FRACTION(NS)*RN%HOC_COMPLETE
                        FUEL_MF = FUEL_MF + SF%MASS_FRACTION(NS)
                     ENDIF
                  ENDIF
               ENDDO
               IF (SUM(SF%MASS_FLUX) < TWO_EPSILON_EB) THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(361): SURF ',TRIM(SF%ID),&
                                       ' uses HRRPUA and MASS_FRACTION but no REACtion FUEL species are specified.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IF (SF%N_LAYERS > 0 .AND. SF%THERMAL_BC_INDEX==THERMALLY_THICK) THEN
                  ! HRRPUA defines the mass of gas into the domiain. The SURF mass loss (M_DOT_G_PP_ACTUAL)
                  ! requires adjustment. the factor is the ratio of the average fuel H_o_C to the MATL H_o_C.
                  IF (MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1) > 0._EB) &
                     SF%M_DOT_G_PP_ACTUAL_FAC = (SUM(SF%MASS_FLUX)/FUEL_MF) / MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1)
               ENDIF
               SF%MASS_FLUX = SF%MASS_FRACTION * SF%HRRPUA / SUM(SF%MASS_FLUX)
               DO NS=1,N_TRACKED_SPECIES
                  IF (SF%MASS_FLUX(NS) > TWO_EPSILON_EB) SF%RAMP(NS) = SF%RAMP(TIME_HEAT)
               ENDDO
               SF%MASS_FRACTION = 0._EB ! Set to zero for error checking later
            ELSE
               RN => REACTION(1)
               SF%MASS_FLUX(RN%FUEL_SMIX_INDEX) = SF%HRRPUA/RN%HOC_COMPLETE
               IF (SF%N_LAYERS > 0 .AND. SF%THERMAL_BC_INDEX==THERMALLY_THICK) THEN
                  IF (MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1) > 0._EB) &
                     SF%M_DOT_G_PP_ACTUAL_FAC = RN%HOC_COMPLETE / MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1)
               ENDIF
               SF%RAMP(RN%FUEL_SMIX_INDEX)    = SF%RAMP(TIME_HEAT)
            ENDIF
         ELSEIF (SF%MLRPUA>0._EB) THEN H_OR_M_IF
            IF (ANY(SF%MASS_FRACTION > 0._EB)) THEN
               FUEL_MF = 0._EB
               DO NS=1,N_TRACKED_SPECIES
                  IF (SF%MASS_FRACTION(NS) > 0._EB .AND. ANY(REAC_FUEL==SPECIES_MIXTURE(NS)%ID)) THEN
                     NR = FINDLOC(REAC_FUEL,SPECIES_MIXTURE(NS)%ID,1)
                     RN => REACTION(NR)
                     IF (DUPLICATE_FUEL(NR)) THEN
                        WRITE(MESSAGE,'(5A)') 'ERROR(362): SURF ',TRIM(SF%ID),' uses MLRPUA and species ', &
                                               TRIM(SPECIES_MIXTURE(NS)%ID),' is the FUEL for more than one REACtion.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                     FUEL_MF = FUEL_MF + RN%HOC_COMPLETE * SF%MASS_FRACTION(NS)
                  ENDIF
               ENDDO
               IF (FUEL_MF < TWO_EPSILON_EB) THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(363): SURF ',TRIM(SF%ID), &
                                       ' uses MLRPUA and MASS_FRACTION but no REACtion FUEL species are specified.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IF (SF%N_LAYERS > 0 .AND. SF%THERMAL_BC_INDEX==THERMALLY_THICK) THEN
                  ! MLRPUA defines the solid mass loss. The gas phase gas addition (M_DOT_G_PP_ADJUST)
                  ! requires adjustment. the factor is the ratio of the MATL H_o_C to the average fuel H_o_C.
                  IF (MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1) > 0._EB) &
                     SF%M_DOT_G_PP_ADJUST_FAC = MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1) / FUEL_MF
               ENDIF
               SF%MASS_FLUX = SF%MASS_FRACTION * SF%MLRPUA
               DO NS=1,N_TRACKED_SPECIES
                  IF (SF%MASS_FLUX(NS) > TWO_EPSILON_EB) SF%RAMP(NS) = SF%RAMP(TIME_HEAT)
               ENDDO
               SF%MASS_FRACTION = 0._EB ! Set to zero for error checking later
            ELSE
               RN => REACTION(1)
               IF (SF%N_LAYERS > 0 .AND. SF%THERMAL_BC_INDEX==THERMALLY_THICK) THEN
                  IF (MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1) > 0._EB) &
                     SF%M_DOT_G_PP_ADJUST_FAC = MATERIAL(SF%MATL_INDEX(1))%HEAT_OF_COMBUSTION(1,1) / RN%HOC_COMPLETE
               ENDIF
               SF%MASS_FLUX(RN%FUEL_SMIX_INDEX) = SF%MLRPUA
               SF%RAMP(RN%FUEL_SMIX_INDEX) = SF%RAMP(TIME_HEAT)
            ENDIF
         ENDIF H_OR_M_IF
      ENDIF HRRPUA_MLRPUA_IF
   ENDIF BURNING_IF

   ! Create QDOTPP_REF and E2T Ramps for Spyro

   IF_QDOTPP_REF: IF (SF%N_QDOTPP_REF > 0) THEN
      ALLOCATE(SF%HRRPUA_INT_INDEX(SF%N_QDOTPP_REF))
      ALLOCATE(SF%QREF_INDEX(SF%N_QDOTPP_REF))
      ALLOCATE(SF%E2T_INDEX(SF%N_QDOTPP_REF))
      SF%HOC_EFF = SF%HRRPUA / SUM(SF%MASS_FLUX)
      SF%Y_S_EFF = 0._EB
      IF (SOOT_INDEX > 0) THEN
         DO NR=1,N_REACTIONS
            RN => REACTION(NR)
            IF (SF%MASS_FLUX(RN%FUEL_SMIX_INDEX) <= 0._EB) CYCLE
            SF%Y_S_EFF = SF%Y_S_EFF + RN%NU_SPECIES(SOOT_INDEX) * MW_SOOT / RN%MW_FUEL * SF%MASS_FLUX(RN%FUEL_SMIX_INDEX)
         ENDDO
         SF%Y_S_EFF = SF%Y_S_EFF/SUM(SF%MASS_FLUX)
      ENDIF
      N_QDOTPP_DO: DO NN=1,SF%N_QDOTPP_REF
         IF (PROCESSED(SF%RAMP(TIME_HEAT-NN+1)%INDEX)>0) THEN
            SF%HRRPUA_INT_INDEX(NN) = PROCESSED(SF%RAMP(TIME_HEAT-NN+1)%INDEX)
            SF%QREF_INDEX(NN) = PROCESSED(SF%RAMP(TIME_HEAT-NN+1)%INDEX) + 1
            SF%E2T_INDEX(NN)  = PROCESSED(SF%RAMP(TIME_HEAT-NN+1)%INDEX) + 2
            CYCLE
         ENDIF
         RP => RAMPS(SF%RAMP(TIME_HEAT-NN+1)%INDEX)
         IF (ABS(RP%T_MIN) > 0._EB) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(447): RAMP ',TRIM(RP%ID),' used with REFERENCE_HEAT_FLUX must start at T = 0.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         I_CONE_RAMP = I_CONE_RAMP + 1
         PROCESSED(SF%RAMP(TIME_HEAT-NN+1)%INDEX) = I_CONE_RAMP
         RP_INT => RAMPS(I_CONE_RAMP)
         RP_INT = RP
         RP_INT%INTERPOLATED_DATA(0) = 0._EB
         SF%HRRPUA_INT_INDEX(NN) = I_CONE_RAMP

         I_CONE_RAMP = I_CONE_RAMP + 1
         RP_QREF => RAMPS(I_CONE_RAMP)
         RP_QREF = RP
         SF%QREF_INDEX(NN) = I_CONE_RAMP

         ALLOCATE(Q_REF_INT(0:RP%NUMBER_INTERPOLATION_POINTS))
         Q_REF_INT(0) = 0._EB
         DO NNN=0,RP%NUMBER_INTERPOLATION_POINTS
            IF (.NOT. SF%INERT_Q_REF) THEN
               HRRPUA = SF%HRRPUA*RP%INTERPOLATED_DATA(NNN)
               RP_QREF%INTERPOLATED_DATA(NNN) = Q_REF_FIT(HRRPUA,SF%HOC_EFF,SF%Y_S_EFF,SF%REFERENCE_HEAT_FLUX(NN))
            ELSE
               RP_QREF%INTERPOLATED_DATA(NNN) = SF%REFERENCE_HEAT_FLUX(NN)
            ENDIF
            IF (NNN>0) THEN
               RP_INT%INTERPOLATED_DATA(NNN) = RP_INT%INTERPOLATED_DATA(NNN-1) + 0.5_EB / RP%RDT * &
                                               (RP%INTERPOLATED_DATA(NNN)+RP%INTERPOLATED_DATA(NNN-1))
               Q_REF_INT(NNN) = Q_REF_INT(NNN-1) + 0.5_EB / RP%RDT * &
                                (RP_QREF%INTERPOLATED_DATA(NNN)+RP_QREF%INTERPOLATED_DATA(NNN-1))
            ENDIF
         ENDDO

         I_CONE_RAMP = I_CONE_RAMP + 1
         RP_E2T  => RAMPS(I_CONE_RAMP)
         SF%E2T_INDEX(NN) = I_CONE_RAMP
         RP_E2T%NUMBER_INTERPOLATION_POINTS = 2 * RP_INT%NUMBER_INTERPOLATION_POINTS
         RP_E2T%T_MIN = 0._EB
         RP_E2T%T_MAX = Q_REF_INT(RP_INT%NUMBER_INTERPOLATION_POINTS)
         RP_E2T%SPAN = RP_E2T%T_MAX
         RP_E2T%RDT = REAL(RP_E2T%NUMBER_INTERPOLATION_POINTS,EB) / RP_E2T%T_MAX
         ALLOCATE(RP_E2T%INTERPOLATED_DATA(0:RP_E2T%NUMBER_INTERPOLATION_POINTS))
         RP_E2T%INTERPOLATED_DATA(0) = 0._EB
         E_PTR = 1
         DO NNN=1,RP_E2T%NUMBER_INTERPOLATION_POINTS
            E = REAL(NNN,EB) / RP_E2T%RDT
            IF (E > Q_REF_INT(E_PTR) .AND. E_PTR < RP%NUMBER_INTERPOLATION_POINTS) THEN
               DO
                  E_PTR = E_PTR + 1
                  IF (E <= Q_REF_INT(E_PTR)) EXIT
               ENDDO
            ENDIF
            RP_E2T%INTERPOLATED_DATA(NNN) = (REAL(E_PTR-1,EB) + (E - Q_REF_INT(E_PTR-1)) / &
                                            (Q_REF_INT(E_PTR)-Q_REF_INT(E_PTR-1))) / RP_INT%RDT
         ENDDO
         DEALLOCATE(Q_REF_INT)

      ENDDO N_QDOTPP_DO
   ENDIF IF_QDOTPP_REF

   ! Compute surface density

   SF%SURFACE_DENSITY = 0._EB
   R_L(0) = SF%THICKNESS
   DO NL=1,SF%N_LAYERS
      R_L(NL) = R_L(NL-1)-SF%LAYER_THICKNESS(NL)
      SF%SURFACE_DENSITY = SF%SURFACE_DENSITY + SF%LAYER_DENSITY(NL) * &
         (R_L(NL-1)**I_GRAD-R_L(NL)**I_GRAD)/(REAL(I_GRAD,EB)*SF%THICKNESS**(I_GRAD-1))
   ENDDO

   IF ((ABS(SF%SURFACE_DENSITY) <= TWO_EPSILON_EB) .AND. SF%BURN_AWAY) THEN
      WRITE(MESSAGE,'(A,A,A)') 'WARNING: SURF ',TRIM(SF%ID),' has BURN_AWAY set but zero combustible density'
      IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
   ENDIF

   ! Ignition Time

   SF%T_IGN = T_BEGIN
   IF (SF%TMP_IGN<50000._EB)                    SF%T_IGN = HUGE(T_END)
   IF (SF%PYROLYSIS_MODEL==PYROLYSIS_PREDICTED) SF%T_IGN = HUGE(T_END)
   IF (SF%VEG_LSET_SPREAD)                      SF%T_IGN = HUGE(T_END)

   ! Species Arrays and Method of Mass Transfer (SPECIES_BC_INDEX)

   SF%SPECIES_BC_INDEX = NO_MASS_FLUX

   IF (ANY(SF%MASS_FRACTION>0._EB) .AND. (ANY(ABS(SF%MASS_FLUX)>TWO_EPSILON_EB) .OR. SF%PYROLYSIS_MODEL/= PYROLYSIS_NONE)) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(364): SURF ',TRIM(SF%ID),' cannot specify mass fraction with mass flux and/or pyrolysis.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   IF (ANY(SF%MASS_FRACTION>0._EB) .AND. SUCKING) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(365): SURF ',TRIM(SF%ID),' cannot specify both mass fraction and outflow velocity.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   IF (ANY(SF%LEAK_PATH>=0) .AND. (BLOWING .OR. SUCKING .OR. SF%PYROLYSIS_MODEL/= PYROLYSIS_NONE)) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(366): SURF ',TRIM(SF%ID),' cannot leak and specify flow or pyrolysis at the same time.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   IF (ANY(ABS(SF%MASS_FLUX)>TWO_EPSILON_EB) .AND. (BLOWING .OR. SUCKING)) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR(367): SURF ',TRIM(SF%ID),' cannot have both a mass flux and specified velocity.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

   IF (BLOWING .OR. SUCKING)        SF%SPECIES_BC_INDEX = SPECIFIED_MASS_FRACTION
   IF (ANY(SF%MASS_FRACTION>0._EB)) SF%SPECIES_BC_INDEX = SPECIFIED_MASS_FRACTION
   IF (ANY(ABS(SF%MASS_FLUX)>TWO_EPSILON_EB) .OR. &
       SF%PYROLYSIS_MODEL==PYROLYSIS_PREDICTED) SF%SPECIES_BC_INDEX = SPECIFIED_MASS_FLUX

   IF (SF%SPECIES_BC_INDEX==SPECIFIED_MASS_FRACTION) THEN
      IF (ALL(ABS(SF%MASS_FRACTION)< TWO_EPSILON_EB)) &
         SF%MASS_FRACTION(1:N_TRACKED_SPECIES) = SPECIES_MIXTURE(1:N_TRACKED_SPECIES)%ZZ0
   ENDIF

   ! Texture map info

   SF%SURF_TYPE = SMV_REGULAR
   IF (SF%TEXTURE_MAP/='null') SF%SURF_TYPE = SMV_TEXTURE

   ! Set BCs for various boundary types

   SF%VELOCITY_BC_INDEX = WALL_MODEL_BC
   IF (SIM_MODE==DNS_MODE)     SF%VELOCITY_BC_INDEX = NO_SLIP_BC
   IF (SF%FREE_SLIP)           SF%VELOCITY_BC_INDEX = FREE_SLIP_BC
   IF (SF%NO_SLIP)             SF%VELOCITY_BC_INDEX = NO_SLIP_BC
   IF (SF%BOUNDARY_FUEL_MODEL) SF%VELOCITY_BC_INDEX = BOUNDARY_FUEL_MODEL_BC

   IF (N==OPEN_SURF_INDEX) THEN
      SF%THERMAL_BC_INDEX = INFLOW_OUTFLOW
      SF%SPECIES_BC_INDEX = INFLOW_OUTFLOW_MASS_FLUX
      SF%VELOCITY_BC_INDEX = FREE_SLIP_BC
      SF%SURF_TYPE = SMV_OUTLINE
      SF%EMISSIVITY = 1._EB
   ENDIF
   IF (N==MIRROR_SURF_INDEX) THEN
      SF%THERMAL_BC_INDEX = NO_CONVECTION
      SF%SPECIES_BC_INDEX = NO_MASS_FLUX
      SF%VELOCITY_BC_INDEX = FREE_SLIP_BC
      SF%SURF_TYPE = SMV_HIDDEN
      SF%EMISSIVITY = 0._EB
   ENDIF
   IF (N==INTERPOLATED_SURF_INDEX) THEN
      SF%THERMAL_BC_INDEX = INTERPOLATED_BC
      SF%SPECIES_BC_INDEX = INTERPOLATED_BC
      SF%VELOCITY_BC_INDEX = INTERPOLATED_VELOCITY_BC
   ENDIF
   IF (N==PERIODIC_SURF_INDEX) THEN
      SF%THERMAL_BC_INDEX = INTERPOLATED_BC
      SF%SPECIES_BC_INDEX = INTERPOLATED_BC
      SF%VELOCITY_BC_INDEX = INTERPOLATED_VELOCITY_BC
   ENDIF
   IF (N==PERIODIC_FLOW_ONLY_SURF_INDEX) THEN
      SF%THERMAL_BC_INDEX = INFLOW_OUTFLOW
      SF%SPECIES_BC_INDEX = INFLOW_OUTFLOW_MASS_FLUX
      SF%VELOCITY_BC_INDEX = INTERPOLATED_VELOCITY_BC
   ENDIF
   IF (N==HVAC_SURF_INDEX) THEN
      SF%THERMAL_BC_INDEX = HVAC_BOUNDARY
      SF%SPECIES_BC_INDEX = HVAC_BOUNDARY
   ENDIF
   IF (N==MASSLESS_TARGET_SURF_INDEX) THEN
      SF%EMISSIVITY = 1._EB
   ENDIF

   ! Do not allow N_LAYERS or N_CELLS_INI to be zero

   IF (.NOT.ALLOCATED(SF%N_LAYER_CELLS)) THEN
      SF%N_LAYERS     = 1
      SF%N_CELLS_MAX  = 1
      SF%N_CELLS_INI  = 1
      SF%N_MATL   = 1
      ALLOCATE(SF%N_LAYER_CELLS(SF%N_LAYERS))
      ALLOCATE(SF%X_S(0:SF%N_CELLS_MAX))
      SF%X_S(0) = 0._EB
      SF%X_S(1) = SF%THICKNESS
      ALLOCATE(SF%RHO_0(0:SF%N_CELLS_MAX+1,SF%N_MATL))
      SF%RHO_0 = 0._EB
      SF%TMP_INNER = TMPA
   ENDIF

   ! Build particle list

   IF (SF%LAYER_DENSITY(1)>0._EB .AND. N_LAGRANGIAN_CLASSES >0) THEN
      N_LIST = 0
      INDEX_LIST = .FALSE.
      DO NN=1,SF%N_MATL
         ML=>MATERIAL(SF%MATL_INDEX(NN))
         DO NR=1,ML%N_REACTIONS
            DO NLPC=1,ML%N_LPC(NR)
               IF (INDEX_LIST(ML%LPC_INDEX(NLPC,NR))) EXIT
                  N_LIST = N_LIST + 1
                  INDEX_LIST(ML%LPC_INDEX(NLPC,NR)) = .TRUE.
            ENDDO
         ENDDO
      ENDDO

      SF%N_LPC = N_LIST
      ALLOCATE(SF%MATL_PART_INDEX(SF%N_LPC))

      NNN = 0
      DO NN=1,MAX_LPC
         IF (INDEX_LIST(NN)) THEN
            NNN = NNN + 1
            SF%MATL_PART_INDEX(NNN) = NN
         ENDIF
         IF (NNN == N_LIST) EXIT
      ENDDO

   ENDIF

ENDDO PROCESS_SURF_LOOP

END SUBROUTINE PROC_SURF_2


! \brief Set up 1-D grids and arrays for thermally-thick solids

SUBROUTINE PROC_WALL

USE GEOMETRY_FUNCTIONS
USE PHYSICAL_FUNCTIONS, ONLY: GET_SPECIFIC_GAS_CONSTANT
INTEGER :: SURF_INDEX,N,NL,II,IL,NN,N_CELLS_MAX,N_LAYER_CELLS_OLD(1:MAX_LAYERS)=0
REAL(EB) :: K_S_0,C_S_0,DENSITY_MAX,DENSITY_MIN
LOGICAL :: REMESH_LAYER(1:MAX_LAYERS)
REAL(EB), ALLOCATABLE, DIMENSION(:) :: X_S_OLD

REMESH_LAYER = .TRUE.

! Calculate ambient temperature thermal DIFFUSIVITY for each MATERIAL, to be used in determining number of solid cells

DO N=1,N_MATL
   ML => MATERIAL(N)
   K_S_0 = ML%K_S(NINT(TMPA))
   C_S_0 = ML%C_S(NINT(TMPA))
   ML%THERMAL_DIFFUSIVITY = K_S_0/(C_S_0*ML%RHO_S)
ENDDO

NWP_MAX = 0  ! For some utility arrays, need to know the greatest number of points of all surface types

! Loop through all surfaces, looking for those that are thermally-thick (have layers).
! Compute smallest cell size for each layer such that internal cells double in size.
! Each layer should have an odd number of cells.

SURF_GRID_LOOP: DO SURF_INDEX=0,N_SURF

   SF => SURFACE(SURF_INDEX)
   IF (SF%THERMAL_BC_INDEX /= THERMALLY_THICK) CYCLE SURF_GRID_LOOP

   ! Compute number of points per layer, and then sum up to get total points for the surface

   SF%N_CELLS_INI = 0
   N_CELLS_MAX    = 0

   LAYER_LOOP: DO NL=1,SF%N_LAYERS

      SF%MIN_DIFFUSIVITY(NL) = 1000000._EB
      DO N = 1,SF%N_LAYER_MATL(NL)
         ML => MATERIAL(SF%LAYER_MATL_INDEX(NL,N))
         SF%MIN_DIFFUSIVITY(NL) = MIN(SF%MIN_DIFFUSIVITY(NL),ML%THERMAL_DIFFUSIVITY)
      ENDDO

      DENSITY_MAX = 0._EB
      DENSITY_MIN = 10000000._EB
      DO N = 1,SF%N_MATL
         ML => MATERIAL(SF%MATL_INDEX(N))
         DO NN = 1,SF%N_LAYER_MATL(NL)
            IF ((ML%PYROLYSIS_MODEL==PYROLYSIS_SOLID.OR.ML%PYROLYSIS_MODEL==PYROLYSIS_SURFACE_OXIDATION) .AND. &
                 SF%LAYER_MATL_INDEX(NL,NN)==SF%MATL_INDEX(N)) THEN
               DENSITY_MAX = MAX(DENSITY_MAX,SF%MATL_MASS_FRACTION(NL,NN)*SF%LAYER_DENSITY(NL))
            ENDIF
         ENDDO
         DENSITY_MIN = MIN(DENSITY_MIN,ML%RHO_S)
      ENDDO

      SF%SWELL_RATIO = 1._EB
      IF (SF%PYROLYSIS_MODEL==PYROLYSIS_PREDICTED .AND. DENSITY_MIN>TWO_EPSILON_EB) &
         SF%SWELL_RATIO = MAX(1._EB,DENSITY_MAX/DENSITY_MIN)

      ! Get highest possible number of cells for this layer

      CALL GET_N_LAYER_CELLS(SF%MIN_DIFFUSIVITY(NL),SF%SWELL_RATIO*SF%LAYER_THICKNESS(NL),SF%STRETCH_FACTOR(NL), &
                             SF%CELL_SIZE_FACTOR(NL),SF%CELL_SIZE(NL),SF%N_LAYER_CELLS_MAX(NL),SF%N_LAYER_CELLS(NL),&
                             SF%SMALLEST_CELL_SIZE(NL),SF%DDSUM(NL))
      N_CELLS_MAX = N_CELLS_MAX + SF%N_LAYER_CELLS(NL)

      ! Get initial number of cells for this layer

      CALL GET_N_LAYER_CELLS(SF%MIN_DIFFUSIVITY(NL),SF%LAYER_THICKNESS(NL),SF%STRETCH_FACTOR(NL), &
                             SF%CELL_SIZE_FACTOR(NL),SF%CELL_SIZE(NL),SF%N_LAYER_CELLS_MAX(NL),SF%N_LAYER_CELLS(NL),&
                             SF%SMALLEST_CELL_SIZE(NL),SF%DDSUM(NL))
      SF%N_CELLS_INI= SF%N_CELLS_INI + SF%N_LAYER_CELLS(NL)

   ENDDO LAYER_LOOP

   IF (SF%N_CELLS_MAX==0) SF%N_CELLS_MAX = N_CELLS_MAX

   ! Allocate arrays to hold x_s, 1/dx_s (center to center, RDXN), 1/dx_s (edge to edge, RDX)

   NWP_MAX = MAX(NWP_MAX,SF%N_CELLS_MAX)
   ALLOCATE(SF%DX(1:SF%N_CELLS_MAX))
   ALLOCATE(SF%RDX(0:SF%N_CELLS_MAX+1))
   ALLOCATE(SF%RDXN(0:SF%N_CELLS_MAX))
   ALLOCATE(SF%DX_WGT(0:SF%N_CELLS_MAX))
   ALLOCATE(SF%X_S(0:SF%N_CELLS_MAX))
   ALLOCATE(SF%LAYER_INDEX(0:SF%N_CELLS_MAX+1))
   ALLOCATE(SF%MF_FRAC(1:SF%N_CELLS_MAX))
   ALLOCATE(SF%RHO_0(0:SF%N_CELLS_MAX+1,SF%N_MATL))

   ! Compute node coordinates
   ! X_S_OLD provides the right size array into GET_WALL_NODE_COORDINATES. REMESH_LAYER defined as .TRUE.
   ALLOCATE(X_S_OLD(0:SF%N_CELLS_MAX)); X_S_OLD=0._EB
   CALL GET_WALL_NODE_COORDINATES(SF%N_CELLS_INI,SF%N_CELLS_MAX+1,SF%N_LAYERS,SF%N_LAYER_CELLS, N_LAYER_CELLS_OLD(1:SF%N_LAYERS), &
        SF%SMALLEST_CELL_SIZE(1:SF%N_LAYERS),SF%STRETCH_FACTOR(1:SF%N_LAYERS),REMESH_LAYER(1:SF%N_LAYERS),&
        SF%X_S,X_S_OLD(0:SF%N_CELLS_MAX),SF%LAYER_THICKNESS(1:SF%N_LAYERS))
   DEALLOCATE(X_S_OLD)

   CALL GET_WALL_NODE_WEIGHTS(SF%N_CELLS_INI,SF%N_LAYERS,SF%N_LAYER_CELLS,SF%LAYER_THICKNESS,SF%GEOMETRY, &
         SF%X_S,SF%LAYER_DIVIDE,SF%DX,SF%RDX,SF%RDXN,SF%DX_WGT,SF%DXF,SF%DXB,SF%LAYER_INDEX,SF%MF_FRAC,SF%INNER_RADIUS)

   ! Initialize the material component densities of the solid, SF%RHO_0(II,N),
   ! where II is the interior cell index and N is the surface material index.
   ! The surface material indices are a subset of the full list of materials.

   SF%RHO_0 = 0._EB

   DO II=0,SF%N_CELLS_INI+1
      IL = SF%LAYER_INDEX(II)
      DO NN=1,SF%N_LAYER_MATL(IL)
         DO N=1,SF%N_MATL
            IF (SF%LAYER_MATL_INDEX(IL,NN)==SF%MATL_INDEX(N)) &
               SF%RHO_0(II,N) = SF%MATL_MASS_FRACTION(IL,NN)*SF%LAYER_DENSITY(IL)
         ENDDO
      ENDDO
   ENDDO

   ! Create an array of material densities, SF%RHO_S(IL,N), where IL is the
   ! layer index and N is the surface material index. Notice that this array
   ! is similar the actual density of the material, ML%RHO_S, but it is
   ! sometimes adjusted to account for the fact that moisture might be added to
   ! a material's void space, changing its density but not the volume of the
   ! layer.

   DO IL=1,SF%N_LAYERS
      DO N=1,SF%N_MATL
         SF%RHO_S(IL,N) = MATERIAL(SF%MATL_INDEX(N))%RHO_S*SF%DENSITY_ADJUST_FACTOR(IL,N)
      ENDDO
   ENDDO

ENDDO SURF_GRID_LOOP

END SUBROUTINE PROC_WALL


!> \brief Read the PRESsure namelist line

SUBROUTINE READ_PRES
CHARACTER(LABEL_LENGTH) :: SOLVER='FFT'
INTEGER :: NM

NAMELIST /PRES/ BAROCLINIC,CHECK_POISSON,FISHPAK_BC,ITERATION_SUSPEND_FACTOR, &
                MAX_PRESSURE_ITERATIONS,PRESSURE_RELAX_TIME,PRESSURE_TOLERANCE, &
                RELAXATION_FACTOR,SOLVER,SUSPEND_PRESSURE_ITERATIONS,TUNNEL_PRECONDITIONER,VELOCITY_TOLERANCE

! Read the single PRES line

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
READ_LOOP: DO
   CALL CHECKREAD('PRES',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_LOOP
   READ(LU_INPUT,PRES,END=23,ERR=24,IOSTAT=IOS)
   24 IF (IOS>0) THEN
      CALL SHUTDOWN('ERROR(101): Problem with PRES line') ; RETURN
   ENDIF
ENDDO READ_LOOP
23 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Given the chosen SOLVER, define internal variable PRES_METHOD:

SELECT CASE(TRIM(SOLVER))

   CASE('UGLMAT')
      PRES_METHOD = 'UGLMAT'
      PRES_FLAG   = UGLMAT_FLAG
      PRES_ON_WHOLE_DOMAIN = .FALSE.
      IF (CHECK_POISSON) GLMAT_VERBOSE=.TRUE.

   CASE('GLMAT')
      PRES_METHOD = 'GLMAT'
      PRES_FLAG   = GLMAT_FLAG
      PRES_ON_WHOLE_DOMAIN = .TRUE.
      IF (CHECK_POISSON) GLMAT_VERBOSE=.TRUE.

   CASE('ULMAT')
      PRES_METHOD = 'ULMAT'
      PRES_FLAG   = ULMAT_FLAG
      PRES_ON_WHOLE_DOMAIN = .FALSE.
      IF (CHECK_POISSON) GLMAT_VERBOSE=.TRUE.

   CASE('FFT')
      ! Nothing to do. By default PRES_FLAG is set to FFT_FLAG in cons.f90
   CASE DEFAULT
      ! Here the user added an unknown name to SOLVER, stop:
      CALL SHUTDOWN('ERROR(371): Pressure solver '//TRIM(SOLVER)//' not known.') ; RETURN
END SELECT

! Determine how many pressure iterations to perform per half time step.

IF (VELOCITY_TOLERANCE>100._EB) THEN
   ITERATE_PRESSURE = .FALSE.
ELSE
   ITERATE_PRESSURE = .TRUE.
   IF (VELOCITY_TOLERANCE<TWO_EPSILON_EB) VELOCITY_TOLERANCE =  0.5_EB*CHARACTERISTIC_CELL_SIZE
   IF (PRESSURE_TOLERANCE<TWO_EPSILON_EB) PRESSURE_TOLERANCE = 20.0_EB/MIN(1._EB,CHARACTERISTIC_CELL_SIZE)**2
ENDIF

IF (NMESHES>1 .AND. ANY(FISHPAK_BC==FISHPAK_BC_PERIODIC)) THEN
   CALL SHUTDOWN('ERROR(372): Cannot use FISHPAK_BC for multiple mesh simulations.') ; RETURN
ENDIF

IF (ANY(FISHPAK_BC>0)) THEN
   CALL SHUTDOWN('ERROR(373): Cannot have FISHPAK_BC>0') ; RETURN
ENDIF

! Create arrays to be used in the special pressure solver for tunnels

IF (TUNNEL_PRECONDITIONER) THEN
   IF (MAX_PRESSURE_ITERATIONS<20) MAX_PRESSURE_ITERATIONS = 20
   TUNNEL_NXP = 0
   DO NM=1,NMESHES
      TUNNEL_NXP = TUNNEL_NXP + MESHES(NM)%IBAR
   ENDDO
   ALLOCATE(TP_AA(TUNNEL_NXP))
   ALLOCATE(TP_BB(TUNNEL_NXP))
   ALLOCATE(TP_CC(TUNNEL_NXP))
   ALLOCATE(TP_DD(TUNNEL_NXP))
   ALLOCATE(TP_RDXN(0:TUNNEL_NXP))
   ALLOCATE(H_BAR(0:TUNNEL_NXP+1))    ; H_BAR      = 0._EB
   ALLOCATE(H_BAR_S(0:TUNNEL_NXP+1))  ; H_BAR_S    = 0._EB
   ALLOCATE(DUDT_BAR(0:TUNNEL_NXP))   ; DUDT_BAR   = 0._EB
   ALLOCATE(DUDT_BAR_S(0:TUNNEL_NXP)) ; DUDT_BAR_S = 0._EB
ENDIF

END SUBROUTINE READ_PRES


!> \brief Read the RADIation nameline line

SUBROUTINE READ_RADI

USE RADCONS
REAL(EB) :: BAND_LIMITS(MAX_NUMBER_SPECTRAL_BANDS+1)
LOGICAL :: OPTICALLY_THIN
NAMELIST /RADI/ ANGLE_INCREMENT,BAND_LIMITS,C_MAX,C_MIN,INITIAL_RADIATION_ITERATIONS,KAPPA0,&
                MIE_MINIMUM_DIAMETER,MIE_MAXIMUM_DIAMETER,MIE_NDG,NMIEANG,NUMBER_RADIATION_ANGLES,&
                OPTICALLY_THIN,PATH_LENGTH,QR_CLIP,RADIATION,RADIATION_ITERATIONS,RADTMP,&
                TIME_STEP_INCREMENT,WIDE_BAND_MODEL,WSGG_MODEL

REAL(EB) THETALOW,THETAUP
INTEGER  NRA,N

! Set default values

BAND_LIMITS(:) = -1._EB

INITIAL_RADIATION_ITERATIONS = 3
NUMBER_RADIATION_ANGLES = 100
TIME_STEP_INCREMENT     = 3
IF (TWO_D) THEN
   NUMBER_RADIATION_ANGLES = 60
   TIME_STEP_INCREMENT     = 2
ENDIF

KAPPA0               = -10._EB
RADTMP               = 900._EB
WIDE_BAND_MODEL      = .FALSE.
WSGG_MODEL           = .FALSE.
NMIEANG              = 15
PATH_LENGTH          = -1.0_EB ! calculate path based on the geometry
ANGLE_INCREMENT      = -1
MIE_MAXIMUM_DIAMETER = 0._EB
MIE_MINIMUM_DIAMETER = 0._EB
MIE_NDG              = 50
QR_CLIP              = 10._EB  ! kW/m3, lower bound for radiation source correction
OPTICALLY_THIN       = .FALSE.

! Read radiation parameters

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
READ_LOOP: DO
   CALL CHECKREAD('RADI',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_LOOP
   READ(LU_INPUT,RADI,END=23,ERR=24,IOSTAT=IOS)
   24 IF (IOS>0) THEN
      CALL SHUTDOWN('ERROR(101): Problem with RADI line') ; RETURN
   ENDIF
ENDDO READ_LOOP
23 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! A few conversions

RADTMP = RADTMP + TMPM
QR_CLIP = QR_CLIP*1000._EB  ! kW/m3 to W/m3

! Optically-thin limit

IF (OPTICALLY_THIN .OR. KAPPA0==0._EB) THEN
   KAPPA0 = 0._EB
   RTE_SOURCE_CORRECTION = .FALSE.
ENDIF

! Define band parameters

IF (WIDE_BAND_MODEL) THEN
   WSGG_MODEL = .FALSE.
   NUMBER_SPECTRAL_BANDS = 6
   TIME_STEP_INCREMENT=MAX(1,TIME_STEP_INCREMENT)
   ANGLE_INCREMENT = 1
   UIIDIM=NUMBER_SPECTRAL_BANDS
ELSEIF (WSGG_MODEL) THEN
   WIDE_BAND_MODEL = .FALSE.
   NUMBER_SPECTRAL_BANDS = 5
   TIME_STEP_INCREMENT=MAX(1,TIME_STEP_INCREMENT)
   ANGLE_INCREMENT = 1
   UIIDIM=NUMBER_SPECTRAL_BANDS
ELSE
   NUMBER_SPECTRAL_BANDS = 1
   IF (ANGLE_INCREMENT < 0) ANGLE_INCREMENT = MAX(1,MIN(5,NUMBER_RADIATION_ANGLES/15))
   UIIDIM = ANGLE_INCREMENT
ENDIF

! Define custom wavelength band limits

IF (ANY(BAND_LIMITS>0._EB)) THEN
   NUMBER_SPECTRAL_BANDS = COUNT(BAND_LIMITS>0._EB) - 1
   IF (NUMBER_SPECTRAL_BANDS<2) THEN ; CALL SHUTDOWN('ERROR(381): Need more spectral band limits.') ; RETURN ; ENDIF
   IF (ANY((BAND_LIMITS(2:NUMBER_SPECTRAL_BANDS+1)-BAND_LIMITS(1:NUMBER_SPECTRAL_BANDS))<0._EB)) THEN
      CALL SHUTDOWN('ERROR(382): Spectral band limits should be given in ascending order.')
      RETURN
   ENDIF
   ALLOCATE(WL_HIGH(1:NUMBER_SPECTRAL_BANDS))
   ALLOCATE(WL_LOW(1:NUMBER_SPECTRAL_BANDS))
   DO I=1,NUMBER_SPECTRAL_BANDS
      WL_LOW(I) = BAND_LIMITS(I)
      WL_HIGH(I)= BAND_LIMITS(I+1)
   ENDDO

   TIME_STEP_INCREMENT=MAX(1,TIME_STEP_INCREMENT)
   ANGLE_INCREMENT = 1
   UIIDIM=NUMBER_SPECTRAL_BANDS
ENDIF

! Calculate actual number of radiation angles and determine the angular discretization

IF (.NOT.RADIATION) THEN

   NUMBER_RADIATION_ANGLES = 1
   INITIAL_RADIATION_ITERATIONS = 1

ELSE

   NRA = NUMBER_RADIATION_ANGLES

   ! Determine the number of polar angles (theta)

   IF (CYLINDRICAL) THEN
      NRT = NINT(SQRT(REAL(NRA)))
   ELSEIF (TWO_D) THEN
      NRT = 1
   ELSE
      NRT = 2*NINT(0.5_EB*1.17*REAL(NRA)**(1._EB/2.26))
   ENDIF

   ! Determine number of azimuthal angles (phi)

   ALLOCATE(NRP(1:NRT),STAT=IZERO)
   CALL ChkMemErr('INIT','NRP',IZERO)

   N = 0
   DO I=1,NRT
      IF (CYLINDRICAL) THEN
         NRP(I) = NINT(REAL(NRA)/(REAL(NRT)))
      ELSEIF (TWO_D) THEN
         NRP(I) = 4*NINT(0.25_EB*REAL(NRA))
      ELSE
         THETALOW = PI*REAL(I-1)/REAL(NRT)
         THETAUP  = PI*REAL(I)/REAL(NRT)
         NRP(I) = NINT(0.5_EB*REAL(NRA)*(COS(THETALOW)-COS(THETAUP)))
         NRP(I) = MAX(4,NRP(I))
         NRP(I) = 4*NINT(0.25_EB*REAL(NRP(I)))
      ENDIF
      N = N + NRP(I)
   ENDDO
   NUMBER_RADIATION_ANGLES = N

ENDIF

END SUBROUTINE READ_RADI


!> \brief Read the CLIP namelist line for user-defined mins and maxes.

SUBROUTINE READ_CLIP

REAL(EB) :: MAXIMUM_DENSITY,MINIMUM_DENSITY,MINIMUM_TEMPERATURE,MAXIMUM_TEMPERATURE
NAMELIST /CLIP/ CLIP_DT_RESTRICTIONS_MAX,FYI,MAXIMUM_DENSITY,MAXIMUM_TEMPERATURE,MINIMUM_DENSITY,MINIMUM_TEMPERATURE

MINIMUM_DENSITY       = -999._EB
MAXIMUM_DENSITY       = -999._EB
MINIMUM_TEMPERATURE   = -999._EB
MAXIMUM_TEMPERATURE   = -999._EB

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
CLIP_LOOP: DO
   CALL CHECKREAD('CLIP',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT CLIP_LOOP
   READ(LU_INPUT,CLIP,END=431,ERR=432,IOSTAT=IOS)
   432 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with CLIP line') ; RETURN ; ENDIF
ENDDO CLIP_LOOP
431 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (MINIMUM_TEMPERATURE>-TMPM) TMPMIN = MINIMUM_TEMPERATURE + TMPM
IF (MAXIMUM_TEMPERATURE>-TMPM) TMPMAX = MAXIMUM_TEMPERATURE + TMPM

IF (TMPMAX > 5000._EB) THEN
   WRITE(MESSAGE,'(A)') 'WARNING: Thermal properties are tabulated between 0 K and 5000 K'
   IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
ENDIF

IF (MINIMUM_DENSITY>0._EB) THEN
   RHOMIN = MINIMUM_DENSITY
ELSE
   RHOMIN = MIN(0.1_EB*RHOA,2._EB*P_INF*MW_MIN/(R0*TMPMAX))
ENDIF

IF (MAXIMUM_DENSITY>0._EB) THEN
   RHOMAX = MAXIMUM_DENSITY
ELSE
   RHOMAX = 2._EB*P_INF*MW_MAX/(R0*MAX(TMPMIN,1._EB))
ENDIF

END SUBROUTINE READ_CLIP


!> \brief Read the RAMP namelist lines

SUBROUTINE READ_RAMP

REAL(EB) :: X,Z,T,F,TM,INITIAL_VALUE
INTEGER  :: I,II,NN,N,NUMBER_INTERPOLATION_POINTS,N_RES_RAMP
CHARACTER(LABEL_LENGTH) :: DEVC_ID,CTRL_ID,CTRL_ID_DEP,DEVC_ID_DEP
LOGICAL :: EXTERNAL_FILE
TYPE(RAMPS_TYPE), POINTER :: RP
TYPE(RESERVED_RAMPS_TYPE), POINTER :: RRP
NAMELIST /RAMP/ CTRL_ID,CTRL_ID_DEP,DEVC_ID,DEVC_ID_DEP,EXTERNAL_FILE,F,FYI,ID,INITIAL_VALUE,NUMBER_INTERPOLATION_POINTS,T,X,Z

IF (N_RAMP==0) RETURN

ALLOCATE(RAMPS(N_RAMP+N_CONE_RAMP),STAT=IZERO)
CALL ChkMemErr('READ','RAMPS',IZERO)
RAMPS(1:N_RAMP)%ID = RAMP_ID(1:N_RAMP)

! Count the number of points in each ramp

COUNT_RAMP_POINTS: DO N=1,N_RAMP

   RP => RAMPS(N)

   ! Count the user-specified or reserved ramps

   IF (RAMP_ID(N)(1:5)=='RSRVD') THEN
      DO I=1,N_RESERVED_RAMPS
         IF (RAMP_ID(N)==RESERVED_RAMPS(I)%ID) THEN
            N_RES_RAMP = I
         ENDIF
      ENDDO
      RRP => RESERVED_RAMPS(N_RES_RAMP)
      RP%RESERVED_RAMP_INDEX = N_RES_RAMP
      RP%NUMBER_DATA_POINTS = RRP%NUMBER_DATA_POINTS
   ELSE
      REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
      RP%NUMBER_DATA_POINTS = 0
      SEARCH_LOOP: DO
         DEVC_ID_DEP = 'null'
         CTRL_ID_DEP = 'null'
         EXTERNAL_FILE = .FALSE.
         INITIAL_VALUE = -2.E20_EB
         CALL CHECKREAD('RAMP',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
         IF (IOS==1) EXIT SEARCH_LOOP
         READ(LU_INPUT,NML=RAMP,ERR=56,IOSTAT=IOS)
         IF (ID/=RAMP_ID(N)) CYCLE SEARCH_LOOP
         RP%NUMBER_DATA_POINTS = RP%NUMBER_DATA_POINTS + 1
         IF (RP%DEVC_ID_DEP =='null') RP%DEVC_ID_DEP = DEVC_ID_DEP
         IF (RP%DEVC_ID_DEP /='null') RAMP_TYPE(N) = 'CTRL DEVC DEP'
         IF (RP%CTRL_ID_DEP =='null') RP%CTRL_ID_DEP = CTRL_ID_DEP
         IF (RP%CTRL_ID_DEP /='null') RAMP_TYPE(N) = 'CTRL DEVC DEP'
         IF (EXTERNAL_FILE) THEN
            READ_EXTERNAL = .TRUE.
            RP%EXTERNAL_FILE = EXTERNAL_FILE
            IF (INITIAL_VALUE <-1.E20_EB) THEN
               WRITE(MESSAGE,'(A,A,A)') 'ERROR(390): RAMP ',TRIM(ID),' is externally controlled and requires an INITIAL_VALUE.'
               CALL SHUTDOWN(MESSAGE)
            ENDIF
            RP%LAST = INITIAL_VALUE
            RAMP_TYPE(N) = 'EXTERNAL'
         ENDIF
      56 IF (IOS>0) THEN
            WRITE(MESSAGE,'(A,I5)') 'ERROR(101): Problem with RAMP, line number ',INPUT_FILE_LINE_NUMBER
            CALL SHUTDOWN(MESSAGE)
            RETURN
         ENDIF
      ENDDO SEARCH_LOOP
   ENDIF

   IF (RP%NUMBER_DATA_POINTS<2 .AND. RP%DEVC_ID_DEP=='null' .AND. RP%CTRL_ID_DEP=='null' .AND. .NOT. RP%EXTERNAL_FILE) THEN
      IF (RP%NUMBER_DATA_POINTS==0) WRITE(MESSAGE,'(A,A,A)') 'ERROR(391): RAMP ',TRIM(RAMP_ID(N)),' not found.'
      IF (RP%NUMBER_DATA_POINTS==1) WRITE(MESSAGE,'(A,A,A)') 'ERROR(392): RAMP ',TRIM(RAMP_ID(N)),' has only one point.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

ENDDO COUNT_RAMP_POINTS

! Read and process the ramp functions

N_RES_RAMP = 0

READ_RAMP_LOOP: DO N=1,N_RAMP

   RP => RAMPS(N)

   ! Allocate arrays to hold the ramp functions

   RP%DEVC_ID = 'null'
   RP%CTRL_ID = 'null'
   ALLOCATE(RP%INDEPENDENT_DATA(1:RP%NUMBER_DATA_POINTS))
   ALLOCATE(RP%DEPENDENT_DATA(1:RP%NUMBER_DATA_POINTS))
   NUMBER_INTERPOLATION_POINTS=5000

   ! Either read in the RAMP lines or transfer the RESERVED_RAMP into the array of RAMPs

   IF (RP%RESERVED_RAMP_INDEX>0) THEN

      RRP => RESERVED_RAMPS(RP%RESERVED_RAMP_INDEX)
      RP%INDEPENDENT_DATA(1:RP%NUMBER_DATA_POINTS) = RRP%INDEPENDENT_DATA(1:RRP%NUMBER_DATA_POINTS)
      RP%DEPENDENT_DATA(1:RP%NUMBER_DATA_POINTS) = RRP%DEPENDENT_DATA(1:RRP%NUMBER_DATA_POINTS)
      RP%NUMBER_INTERPOLATION_POINTS = NUMBER_INTERPOLATION_POINTS

   ELSE

      REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
      NN = 0
      SEARCH_LOOP2: DO
         DEVC_ID = 'null'
         CTRL_ID = 'null'
         X       = -1.E6_EB
         Z       = -1.E6_EB
         CALL CHECKREAD('RAMP',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
         IF (IOS==1) EXIT SEARCH_LOOP2
         READ(LU_INPUT,RAMP)
         IF (ID/=RAMP_ID(N)) CYCLE SEARCH_LOOP2
         IF (RP%DEVC_ID =='null') RP%DEVC_ID = DEVC_ID
         IF (RP%CTRL_ID =='null') RP%CTRL_ID = CTRL_ID
         IF ((RP%DEVC_ID/='null' .OR. RP%CTRL_ID/='null') .AND. (RP%DEVC_ID_DEP/='null' .OR. RP%CTRL_ID_DEP/='null')) THEN
            WRITE(MESSAGE,'(A,A,A)') 'ERROR(393): RAMP: ',TRIM(ID),' cannot specify both CTRL_ID or DEVC_ID.'
            CALL SHUTDOWN(MESSAGE)
            RETURN
         ENDIF
         IF (X>-1.E5_EB) THEN
            RAMP_TYPE(N) = 'X COORDINATE'
            SPATIAL_GRAVITY_VARIATION = .TRUE.
            STRATIFICATION = .FALSE.
            T = X
         ENDIF
         IF (Z>-1.E5_EB) THEN
            RAMP_TYPE(N) = 'Z COORDINATE'
            T = Z
         ENDIF

         IF (RAMP_TYPE(N)=='T_I PROFILE') F = F + TMPM
         IF (RAMP_TYPE(N)=='TEMPERATURE') T = T + TMPM
         IF (RAMP_TYPE(N)=='TIME')        T = T_BEGIN + (T-T_BEGIN)/TIME_SHRINK_FACTOR
         NN = NN+1
         RP%INDEPENDENT_DATA(NN) = T
         IF (NN>1) THEN
            IF (T<=RP%INDEPENDENT_DATA(NN-1)) THEN
               WRITE(MESSAGE,'(A,A,A)') 'ERROR(394): RAMP ',TRIM(RAMP_ID(N)),' variable T must be monotonically increasing.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDIF
         RP%DEPENDENT_DATA(NN) = F
         RP%NUMBER_INTERPOLATION_POINTS = NUMBER_INTERPOLATION_POINTS
      ENDDO SEARCH_LOOP2

   ENDIF

   ! Get Device or Control Index for DEP ramp

   CALL SEARCH_CONTROLLER('RAMP',RP%CTRL_ID_DEP,RP%DEVC_ID_DEP,RP%DEVC_DEP_INDEX,RP%CTRL_DEP_INDEX,N)
   IF (RP%DEVC_DEP_INDEX > 0 .OR. RP%CTRL_DEP_INDEX > 0 .OR. RP%EXTERNAL_FILE) CYCLE

   RP%T_MIN = MINVAL(RP%INDEPENDENT_DATA)
   RP%T_MAX = MAXVAL(RP%INDEPENDENT_DATA)
   RP%SPAN = RP%T_MAX - RP%T_MIN

ENDDO READ_RAMP_LOOP

   ! Set up interpolated ramp values in INTERPOLATED_DATA and get control or device index

DO N=1,N_RAMP
   RP => RAMPS(N)

   IF (RP%CTRL_DEP_INDEX > 0 .OR. RP%DEVC_DEP_INDEX >0 .OR. RP%EXTERNAL_FILE) THEN
      RP%NUMBER_DATA_POINTS = 0
      CYCLE
   ENDIF

   ! Get Device or Control Index

   CALL SEARCH_CONTROLLER('RAMP',RP%CTRL_ID,RP%DEVC_ID,RP%DEVC_INDEX,RP%CTRL_INDEX,N)

   RP%RDT = REAL(RP%NUMBER_INTERPOLATION_POINTS,EB)/RP%SPAN
   ALLOCATE(RAMPS(N)%INTERPOLATED_DATA(0:RP%NUMBER_INTERPOLATION_POINTS+1))
   RAMPS(N)%INTERPOLATED_DATA(0) = RP%DEPENDENT_DATA(1)
   DO I=1,RP%NUMBER_INTERPOLATION_POINTS-1
      TM = RP%INDEPENDENT_DATA(1) + REAL(I,EB)/RP%RDT
      TLOOP: DO II=1,RP%NUMBER_DATA_POINTS-1
         IF (TM>=RP%INDEPENDENT_DATA(II) .AND. TM<RP%INDEPENDENT_DATA(II+1)) THEN
            RP%INTERPOLATED_DATA(I) = RP%DEPENDENT_DATA(II) +  (TM-RP%INDEPENDENT_DATA(II)) * &
                          (RP%DEPENDENT_DATA(II+1)-RP%DEPENDENT_DATA(II))/(RP%INDEPENDENT_DATA(II+1)-RP%INDEPENDENT_DATA(II))
            EXIT TLOOP
         ENDIF
      ENDDO TLOOP
   ENDDO
   RP%INTERPOLATED_DATA(RP%NUMBER_INTERPOLATION_POINTS)   = RP%DEPENDENT_DATA(RP%NUMBER_DATA_POINTS)
   RP%INTERPOLATED_DATA(RP%NUMBER_INTERPOLATION_POINTS+1) = RP%DEPENDENT_DATA(RP%NUMBER_DATA_POINTS)
ENDDO

IF (READ_EXTERNAL .AND. EXTERNAL_FILENAME=='null') THEN
   WRITE(MESSAGE,'(A)') 'ERROR(395): A RAMP with EXTERNAL_FILE is present but no EXTERNAL_FILENAME is defined on MISC.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

END SUBROUTINE READ_RAMP


!> \brief Read the TABLe namelist lines

SUBROUTINE READ_TABL

REAL(EB) :: TABLE_DATA(9)
INTEGER  :: NN,N
TYPE(TABLES_TYPE), POINTER :: TA=>NULL()
NAMELIST /TABL/ FYI,ID,TABLE_DATA

IF (N_TABLE==0) RETURN

ALLOCATE(TABLES(N_TABLE),STAT=IZERO)
CALL ChkMemErr('READ','TABLES',IZERO)

! Count the number of points in each table

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_TABLE_POINTS: DO N=1,N_TABLE
   TA => TABLES(N)
   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   TA%NUMBER_ROWS = 0
   SELECT CASE (TABLE_TYPE(N))
      CASE (SPRAY_PATTERN)
         TA%NUMBER_COLUMNS = 6
      CASE (PART_RADIATIVE_PROPERTY)
         TA%NUMBER_COLUMNS = 3
   END SELECT
   SEARCH_LOOP: DO
      CALL CHECKREAD('TABL',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT SEARCH_LOOP
      TABLE_DATA = -999._EB
      READ(LU_INPUT,NML=TABL,ERR=56,IOSTAT=IOS)
      IF (ID/=TABLE_ID(N)) CYCLE SEARCH_LOOP
      TA%NUMBER_ROWS = TA%NUMBER_ROWS + 1
      MESSAGE='null'
      SELECT CASE(TABLE_TYPE(N))
         CASE (SPRAY_PATTERN)
            IF (TABLE_DATA(1)<0._EB .OR.           TABLE_DATA(1)>180._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(396): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad 1st latitude.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(2)<TABLE_DATA(1).OR. TABLE_DATA(2)>180._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(397): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad 2nd latitude.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(3)<-180._EB .OR.        TABLE_DATA(3)>360._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(398): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad 1st longitude.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(4)<TABLE_DATA(3).OR. TABLE_DATA(4)>360._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(399): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad 2nd longitude.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(5)<0._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(400): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad velocity.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(6)<0._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(401): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad mass flow.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         CASE (PART_RADIATIVE_PROPERTY)
            IF (TABLE_DATA(1)<0._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(402): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad wavelength.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(2)<=0._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(403): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad real index.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (TABLE_DATA(3)< 0._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,A,A)') 'ERROR(404): Row ',TA%NUMBER_ROWS,' of ',TRIM(TABLE_ID(N)),' has a bad complex index.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
      END SELECT

      56 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR(101): Problem with TABLE '//TRIM(TABLE_ID(N)) ) ; RETURN ; ENDIF
   ENDDO SEARCH_LOOP
   IF (TA%NUMBER_ROWS<=0) THEN
      WRITE(MESSAGE,'(A,A,A)') 'ERROR(407): TABLE ',TRIM(TABLE_ID(N)), ' not found.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO COUNT_TABLE_POINTS

READ_TABL_LOOP: DO N=1,N_TABLE
   TA => TABLES(N)
   ALLOCATE(TA%TABLE_DATA(TA%NUMBER_ROWS,TA%NUMBER_COLUMNS),STAT=IZERO)
   CALL ChkMemErr('READ','TA%TABLE_DATA',IZERO)
   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   NN = 0
   SEARCH_LOOP2: DO
      CALL CHECKREAD('TABL',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT SEARCH_LOOP2
      READ(LU_INPUT,TABL)
      IF (ID/=TABLE_ID(N)) CYCLE SEARCH_LOOP2
      NN = NN+1
      TA%TABLE_DATA(NN,:) = TABLE_DATA(1:TA%NUMBER_COLUMNS)
   ENDDO SEARCH_LOOP2
ENDDO READ_TABL_LOOP

END SUBROUTINE READ_TABL


!> \brief Read the OBSTruction namelist lines

SUBROUTINE READ_OBST(QUICK_READ)

USE MEMORY_FUNCTIONS, ONLY: REALLOCATE_CELL
USE GEOMETRY_FUNCTIONS, ONLY: BLOCK_CELL,CIRCLE_CELL_INTERSECTION_AREA,SEARCH_OTHER_MESHES
USE COMPLEX_GEOMETRY, ONLY: INTERSECT_CONE_AABB,INTERSECT_CYLINDER_AABB,INTERSECT_SPHERE_AABB,INTERSECT_OBB_AABB,ROTATION_MATRIX
USE MISC_FUNCTIONS, ONLY: PROCESS_MESH_NEIGHBORHOOD
LOGICAL, INTENT(IN), OPTIONAL :: QUICK_READ
TYPE(OBSTRUCTION_TYPE), POINTER :: OB2,OBT
TYPE(MULTIPLIER_TYPE), POINTER :: MR
TYPE(OBSTRUCTION_TYPE), DIMENSION(:), ALLOCATABLE, TARGET :: TEMP_OBSTRUCTION
INTEGER :: I,NM,NOM,NOM2,N_OBST_O,IC,N,NN,NNN,NNNN,N_NEW_OBST,RGB(3),N_OBST_DIM,II,JJ,KK,MULT_INDEX,SHAPE_TYPE,IIO,JJO,KKO,IOR,&
           N_LAYER_CELLS_MAX
CHARACTER(LABEL_LENGTH) :: ID,DEVC_ID,SHAPE,SURF_ID,SURF_ID_INTERIOR,SURF_IDS(3),SURF_ID6(6),CTRL_ID,MULT_ID,MATL_ID(MAX_MATERIALS)
CHARACTER(25) :: COLOR
LOGICAL :: OVERLAY,IS_INTERSECT
REAL(EB) :: TRANSPARENCY,XB1,XB2,XB3,XB4,XB5,XB6,BULK_DENSITY,VOL_ADJUSTED,VOL_SPECIFIED,UNDIVIDED_INPUT_AREA(3),&
            UNDIVIDED_INPUT_LENGTH(3),HEIGHT,RADIUS,XYZ(3),ORIENTATION(3),AABB(6),ROTMAT(3,3),THETA,LENGTH,WIDTH,SHAPE_AREA(3),&
            XXI,YYJ,ZZK,DX_GHOST,DY_GHOST,DZ_GHOST,MATL_MASS_FRACTION(MAX_MATERIALS),INTERNAL_HEAT_SOURCE,STRETCH_FACTOR,&
            CELL_SIZE,CELL_SIZE_FACTOR
LOGICAL :: EMBEDDED,THICKEN,THICKEN_LOC,PERMIT_HOLE,ALLOW_VENT,REMOVABLE,BNDF_FACE(-3:3),BNDF_OBST,OUTLINE,REJECT_OBST,FOUND
NAMELIST /OBST/ ALLOW_VENT,BNDF_FACE,BNDF_OBST,BULK_DENSITY,CELL_SIZE,CELL_SIZE_FACTOR,COLOR,CTRL_ID,DEVC_ID,FYI,HEIGHT,ID,&
                INTERNAL_HEAT_SOURCE,LENGTH,MATL_ID,MATL_MASS_FRACTION,MULT_ID,N_LAYER_CELLS_MAX,ORIENTATION,OUTLINE,OVERLAY,&
                PERMIT_HOLE,RADIUS,REMOVABLE,RGB,SHAPE,STRETCH_FACTOR,SURF_ID,SURF_ID_INTERIOR,SURF_ID6,SURF_IDS,&
                TEXTURE_ORIGIN,THETA,THICKEN,TRANSPARENCY,WIDTH,XB,XYZ

MESH_LOOP: DO NM=1,NMESHES

   IF (.NOT.PRESENT(QUICK_READ)) THEN
      M => MESHES(NM)
      IF (.NOT.PROCESS_MESH_NEIGHBORHOOD(NM)) CYCLE MESH_LOOP
      CALL POINT_TO_MESH(NM)
   ENDIF

   ! Count OBST lines

   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   N_OBST_DIM = 0  ! Dimension of MESHES(NM)%OBSTRUCTION
   N_OBST_O   = 0  ! Number of "Original" obstructions; that is, obstructions in the input file
   COUNT_OBST_LOOP: DO
      CALL CHECKREAD('OBST',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT COUNT_OBST_LOOP
      MULT_ID = 'null'
      SURF_ID = 'null'
      SURF_IDS = 'null'
      SURF_ID6 = 'null'
      READ(LU_INPUT,NML=OBST,END=1,ERR=2,IOSTAT=IOS)
      CALL CHECK_XB(XB)
      MULT_INDEX = -1
      IF (MULT_ID=='null') THEN
         MULT_INDEX = 0
      ELSE
         DO N=1,N_MULT
            MR => MULTIPLIER(N)
            IF (MULT_ID==MR%ID) THEN
               MULT_INDEX = N
            ENDIF
         ENDDO
      ENDIF
      IF (MULT_INDEX==-1) THEN
         WRITE(MESSAGE,'(A,I0,3A)') 'ERROR(601): OBST ',N_OBST_O+1,' MULT_ID ',TRIM(MULT_ID),' not found.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF
      MR => MULTIPLIER(MULT_INDEX)
      K_MULT_LOOP2: DO KK=MR%K_LOWER,MR%K_UPPER
         J_MULT_LOOP2: DO JJ=MR%J_LOWER,MR%J_UPPER
            I_MULT_LOOP2: DO II=MR%I_LOWER,MR%I_UPPER
               IF (.NOT.MR%SEQUENTIAL) THEN
                  XB1 = XB(1) + MR%DX0 + II*MR%DXB(1)
                  XB2 = XB(2) + MR%DX0 + II*MR%DXB(2)
                  XB3 = XB(3) + MR%DY0 + JJ*MR%DXB(3)
                  XB4 = XB(4) + MR%DY0 + JJ*MR%DXB(4)
                  XB5 = XB(5) + MR%DZ0 + KK*MR%DXB(5)
                  XB6 = XB(6) + MR%DZ0 + KK*MR%DXB(6)
               ELSE
                  XB1 = XB(1) + MR%DX0 + II*MR%DXB(1)
                  XB2 = XB(2) + MR%DX0 + II*MR%DXB(2)
                  XB3 = XB(3) + MR%DY0 + II*MR%DXB(3)
                  XB4 = XB(4) + MR%DY0 + II*MR%DXB(4)
                  XB5 = XB(5) + MR%DZ0 + II*MR%DXB(5)
                  XB6 = XB(6) + MR%DZ0 + II*MR%DXB(6)
               ENDIF
               N_OBST_O = N_OBST_O + 1
               IF (PRESENT(QUICK_READ)) THEN
                  DO I=1,N_HT3D_SURF_LINES
                     IF (HT3D_SURF_LIST(I)==SURF_ID .OR. ANY(HT3D_SURF_LIST(I)==SURF_IDS) .OR. &
                                                         ANY(HT3D_SURF_LIST(I)==SURF_ID6)) THEN
                        IF (SIZE(HT3D_OBST)<=N_HT3D_OBST) CALL REALLOCATE_HT3D_OBST(SIZE(HT3D_OBST),SIZE(HT3D_OBST)+100)
                        N_HT3D_OBST = N_HT3D_OBST + 1
                        HT3D_OBST(N_HT3D_OBST)%XS = XB1
                        HT3D_OBST(N_HT3D_OBST)%XF = XB2
                        HT3D_OBST(N_HT3D_OBST)%YS = XB3
                        HT3D_OBST(N_HT3D_OBST)%YF = XB4
                        HT3D_OBST(N_HT3D_OBST)%ZS = XB5
                        HT3D_OBST(N_HT3D_OBST)%ZF = XB6
                     ENDIF
                  ENDDO
                  CYCLE I_MULT_LOOP2
               ENDIF
               IF (XB1>M%XF+M%DX(M%IBAR) .OR. XB2<M%XS-M%DX(1) .OR. &
                   XB3>M%YF+M%DY(M%JBAR) .OR. XB4<M%YS-M%DY(1) .OR. &
                   XB5>M%ZF+M%DZ(M%KBAR) .OR. XB6<M%ZS-M%DZ(1)) CYCLE I_MULT_LOOP2
               N_OBST_DIM = N_OBST_DIM + 1
            ENDDO I_MULT_LOOP2
         ENDDO J_MULT_LOOP2
      ENDDO K_MULT_LOOP2
      2 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with OBST number ',N_OBST_O+1,', line number ',INPUT_FILE_LINE_NUMBER
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF
   ENDDO COUNT_OBST_LOOP
   1 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

   IF (PRESENT(QUICK_READ)) RETURN

   ! Allocate OBSTRUCTION array

   ALLOCATE(M%OBSTRUCTION(0:N_OBST_DIM),STAT=IZERO)
   CALL ChkMemErr('READ','OBSTRUCTION',IZERO)
   OBSTRUCTION=>M%OBSTRUCTION

   N        = 0
   N_OBST   = N_OBST_O

   READ_OBST_LOOP: DO NN=1,N_OBST_O

      WRITE(ID,'(A,I0)') 'OBST-',NN
      MULT_ID  = 'null'
      SURF_ID  = 'null'
      SURF_ID_INTERIOR  = 'null'
      SURF_IDS = 'null'
      SURF_ID6 = 'null'
      COLOR    = 'null'
      MATL_ID  = 'null'
      MATL_MASS_FRACTION = 0._EB
      MATL_MASS_FRACTION(1) = 1._EB
      RGB         = -1
      INTERNAL_HEAT_SOURCE = 0._EB
      STRETCH_FACTOR = -1._EB
      CELL_SIZE   = -1._EB
      CELL_SIZE_FACTOR = -1._EB
      N_LAYER_CELLS_MAX = -1
      BULK_DENSITY= -1._EB
      TRANSPARENCY= 1._EB
      BNDF_FACE   = BNDF_DEFAULT
      BNDF_OBST   = BNDF_DEFAULT
      THICKEN     = THICKEN_OBSTRUCTIONS
      OUTLINE     = .FALSE.
      OVERLAY     = .TRUE.
      TEXTURE_ORIGIN = -999._EB
      DEVC_ID     = 'null'
      CTRL_ID     = 'null'
      PERMIT_HOLE = .TRUE.
      ALLOW_VENT  = .TRUE.
      REMOVABLE   = .TRUE.
      XB          = -9.E30_EB
      SHAPE       = 'null'
      XYZ         = 0._EB
      RADIUS      = -1._EB
      LENGTH      = -1._EB
      WIDTH       = -1._EB
      HEIGHT      = -1._EB
      ORIENTATION = (/0._EB,0._EB,1._EB/)
      THETA       = 0._EB
      SHAPE_TYPE  = -1
      SHAPE_AREA  = 0._EB

      ! Read the OBST line

      CALL CHECKREAD('OBST',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT READ_OBST_LOOP
      READ(LU_INPUT,OBST,END=35)

      ! Reorder OBST coordinates if necessary

      CALL CHECK_XB(XB)

      ! Special shapes

      IF (TRIM(SHAPE)/='null') THEN

         ! specify shape type
         SELECT CASE(TRIM(SHAPE))
            CASE('SPHERE');    SHAPE_TYPE = OBST_SPHERE_TYPE
            CASE('CYLINDER');  SHAPE_TYPE = OBST_CYLINDER_TYPE
            CASE('CONE');      SHAPE_TYPE = OBST_CONE_TYPE
            CASE('BOX');       SHAPE_TYPE = OBST_BOX_TYPE
         END SELECT

         ! detect input errors
         IF ((SHAPE_TYPE==OBST_SPHERE_TYPE .OR. SHAPE_TYPE==OBST_CYLINDER_TYPE .OR. SHAPE_TYPE==OBST_CONE_TYPE) &
            .AND. RADIUS<0._EB) THEN
            WRITE(MESSAGE,'(A,I0,A)')  'ERROR(602): OBST ',NN,' SHAPE requires RADIUS.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         IF ((SHAPE_TYPE==OBST_CYLINDER_TYPE .OR. SHAPE_TYPE==OBST_CONE_TYPE) .AND. HEIGHT<0._EB) THEN
            WRITE(MESSAGE,'(A,I0,A)')  'ERROR(603): OBST ',NN,' SHAPE requires HEIGHT.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         IF (SHAPE_TYPE==OBST_BOX_TYPE .AND. (LENGTH<0._EB .OR. WIDTH<0._EB .OR. HEIGHT<0._EB)) THEN
            WRITE(MESSAGE,'(A,I0,A)')  'ERROR(604): OBST ',NN,' BOX SHAPE requires LENGTH, WIDTH, HEIGHT.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         IF (ORIENTATION(1)==0._EB .AND. &
             ORIENTATION(2)==0._EB .AND. &
             ORIENTATION(3)==1._EB .AND. &
             (NMESHES==1 .OR. SHAPE_TYPE==OBST_CYLINDER_TYPE)) THEN
            OBST_SHAPE_AREA_ADJUST = .TRUE.
         ELSE
            OBST_SHAPE_AREA_ADJUST = .FALSE.
         ENDIF

         ! compute shape areas
         SELECT CASE(SHAPE_TYPE)
            CASE(OBST_SPHERE_TYPE)
               SHAPE_AREA(1) = 4._EB*PI*RADIUS**2
            CASE(OBST_CYLINDER_TYPE)
               IF (OBST_SHAPE_AREA_ADJUST) THEN
                  SHAPE_AREA(1) = CIRCLE_CELL_INTERSECTION_AREA(XYZ(1),XYZ(2),RADIUS,M%XS,M%XF,M%YS,M%YF)
               ELSE
                  SHAPE_AREA(1) = PI*RADIUS**2
               ENDIF
               SHAPE_AREA(2) = 2._EB*PI*RADIUS*HEIGHT
               SHAPE_AREA(3) = SHAPE_AREA(1)
               CALL ROTATION_MATRIX(ROTMAT,ORIENTATION,THETA)
            CASE(OBST_CONE_TYPE)
               SHAPE_AREA(1) = PI*RADIUS*( RADIUS + SQRT(HEIGHT**2 + RADIUS**2) ) - PI*RADIUS**2
               SHAPE_AREA(2) = PI*RADIUS**2
               CALL ROTATION_MATRIX(ROTMAT,ORIENTATION,THETA)
            CASE(OBST_BOX_TYPE)
               SHAPE_AREA(1) = LENGTH*WIDTH
               SHAPE_AREA(2) = WIDTH*HEIGHT
               SHAPE_AREA(3) = HEIGHT*LENGTH
               CALL ROTATION_MATRIX(ROTMAT,ORIENTATION,THETA)
         END SELECT
      ENDIF

      ! Loop over all possible multiples of the OBST

      MR => MULTIPLIER(0)
      DO NNN=1,N_MULT
         IF (MULT_ID==MULTIPLIER(NNN)%ID) THEN
            MULT_INDEX=NNN
            MR => MULTIPLIER(NNN)
         ENDIF
      ENDDO

      K_MULT_LOOP: DO KK=MR%K_LOWER,MR%K_UPPER
         J_MULT_LOOP: DO JJ=MR%J_LOWER,MR%J_UPPER
            I_MULT_LOOP: DO II=MR%I_LOWER,MR%I_UPPER

               IF (MR%SKIP(II,JJ,KK)) CYCLE I_MULT_LOOP

               IF (.NOT.MR%SEQUENTIAL) THEN
                  XB1 = XB(1) + MR%DX0 + II*MR%DXB(1)
                  XB2 = XB(2) + MR%DX0 + II*MR%DXB(2)
                  XB3 = XB(3) + MR%DY0 + JJ*MR%DXB(3)
                  XB4 = XB(4) + MR%DY0 + JJ*MR%DXB(4)
                  XB5 = XB(5) + MR%DZ0 + KK*MR%DXB(5)
                  XB6 = XB(6) + MR%DZ0 + KK*MR%DXB(6)
               ELSE
                  XB1 = XB(1) + MR%DX0 + II*MR%DXB(1)
                  XB2 = XB(2) + MR%DX0 + II*MR%DXB(2)
                  XB3 = XB(3) + MR%DY0 + II*MR%DXB(3)
                  XB4 = XB(4) + MR%DY0 + II*MR%DXB(4)
                  XB5 = XB(5) + MR%DZ0 + II*MR%DXB(5)
                  XB6 = XB(6) + MR%DZ0 + II*MR%DXB(6)
               ENDIF

               ! Increase the OBST counter

               N = N + 1

               ! Look for obstructions that are within a half grid cell of the current mesh.
               ! If the obstruction is thin and has the THICKEN attribute, look for it within an entire grid cell.
               ! If there OBST is in another mesh with a gap in between, reject it.

               THICKEN_LOC = THICKEN
               REJECT_OBST = .FALSE.

               IF ( (XB2>=XS-0.5_EB*DX(0)   .AND. XB2<XS) .OR. (THICKEN .AND. 0.5_EB*(XB1+XB2)>=XS-DX(0)    .AND. XB2<XS) ) THEN
                  DX_GHOST = DX(0)
                  CALL SEARCH_OTHER_MESHES(XS-0.01_EB*DX(0),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  CALL SEARCH_OTHER_MESHES(XS-0.51_EB*DX(0),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM2,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  IF (NOM==0 .AND. NOM2>0) REJECT_OBST = .TRUE.
                  IF (NOM>0) THEN
                     IF (ALLOCATED(MESHES(NOM)%DX))DX_GHOST = MESHES(NOM)%DX(IIO)
                  ENDIF
                  IF (XB2>=XS-0.5_EB*DX_GHOST) THEN
                     XB1 = XS
                     XB2 = XS
                     THICKEN_LOC = .FALSE.
                  ENDIF
               ENDIF
               IF ( (XB1<XF+0.5_EB*DX(IBP1) .AND. XB1>XF) .OR. (THICKEN .AND. 0.5_EB*(XB1+XB2)< XF+DX(IBP1) .AND. XB1>XF) ) THEN
                  DX_GHOST = DX(IBP1)
                  CALL SEARCH_OTHER_MESHES(XF+0.01_EB*DX(IBP1),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  CALL SEARCH_OTHER_MESHES(XF+0.51_EB*DX(IBP1),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM2,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  IF (NOM==0 .AND. NOM2>0) REJECT_OBST = .TRUE.
                  IF (NOM>0) THEN
                     IF (ALLOCATED(MESHES(NOM)%DX)) DX_GHOST = MESHES(NOM)%DX(IIO)
                  ENDIF
                  IF (XB1<XF+0.5_EB*DX_GHOST) THEN
                     XB1 = XF
                     XB2 = XF
                     THICKEN_LOC = .FALSE.
                  ENDIF
               ENDIF
               IF ( (XB4>=YS-0.5_EB*DY(0)   .AND. XB4<YS) .OR. (THICKEN .AND. 0.5_EB*(XB3+XB4)>=YS-DY(0)    .AND. XB4<YS) ) THEN
                  DY_GHOST = DY(0)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),YS-0.01_EB*DY(0),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),YS-0.51_EB*DY(0),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM2,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  IF (NOM==0 .AND. NOM2>0) REJECT_OBST = .TRUE.
                  IF (NOM>0) THEN
                     IF (ALLOCATED(MESHES(NOM)%DY)) DY_GHOST = MESHES(NOM)%DY(JJO)
                  ENDIF
                  IF (XB4>=YS-0.5_EB*DY_GHOST) THEN
                     XB3 = YS
                     XB4 = YS
                     THICKEN_LOC = .FALSE.
                  ENDIF
               ENDIF
               IF ( (XB3<YF+0.5_EB*DY(JBP1) .AND. XB3>YF) .OR. (THICKEN .AND. 0.5_EB*(XB3+XB4)< YF+DY(JBP1) .AND. XB3>YF) ) THEN
                  DY_GHOST = DY(JBP1)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),YF+0.01_EB*DY(JBP1),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),YF+0.51_EB*DY(JBP1),0.5_EB*(MAX(ZS,XB5)+MIN(ZF,XB6)),&
                                           NOM2,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  IF (NOM==0 .AND. NOM2>0) REJECT_OBST = .TRUE.
                  IF (NOM>0) THEN
                     IF (ALLOCATED(MESHES(NOM)%DY)) DY_GHOST = MESHES(NOM)%DY(JJO)
                  ENDIF
                  IF (XB3<YS+0.5_EB*DY_GHOST) THEN
                     XB3 = YF
                     XB4 = YF
                     THICKEN_LOC = .FALSE.
                  ENDIF
               ENDIF
               IF ( (XB6>=ZS-0.5_EB*DZ(0)   .AND. XB6<ZS) .OR. (THICKEN .AND. 0.5_EB*(XB5+XB6)>=ZS-DZ(0)    .AND. XB6<ZS) ) THEN
                  DZ_GHOST = DZ(0)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),ZS-0.01_EB*DZ(0),&
                                           NOM,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),ZS-0.51_EB*DZ(0),&
                                           NOM2,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  IF (NOM==0 .AND. NOM2>0) REJECT_OBST = .TRUE.
                  IF (NOM>0) THEN
                     IF (ALLOCATED(MESHES(NOM)%DZ)) DZ_GHOST = MESHES(NOM)%DZ(KKO)
                  ENDIF
                  IF (XB6>=ZS-0.5_EB*DZ_GHOST) THEN
                     XB5 = ZS
                     XB6 = ZS
                     THICKEN_LOC = .FALSE.
                  ENDIF
               ENDIF
               IF ( (XB5<ZF+0.5_EB*DZ(KBP1) .AND. XB5>ZF) .OR. (THICKEN .AND. 0.5_EB*(XB5+XB6)< ZF+DZ(KBP1) .AND. XB5>ZF) ) THEN
                  DZ_GHOST = DZ(KBP1)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),ZF+0.01_EB*DZ(KBP1),&
                                           NOM,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  CALL SEARCH_OTHER_MESHES(0.5_EB*(MAX(XS,XB1)+MIN(XF,XB2)),0.5_EB*(MAX(YS,XB3)+MIN(YF,XB4)),ZF+0.51_EB*DZ(KBP1),&
                                           NOM2,IIO,JJO,KKO,XXI,YYJ,ZZK)
                  IF (NOM==0 .AND. NOM2>0) REJECT_OBST = .TRUE.
                  IF (NOM>0) THEN
                     IF (ALLOCATED(MESHES(NOM)%DZ)) DZ_GHOST = MESHES(NOM)%DZ(KKO)
                  ENDIF
                  IF (XB5<ZF+0.5_EB*DZ_GHOST) THEN
                     XB5 = ZF
                     XB6 = ZF
                     THICKEN_LOC = .FALSE.
                  ENDIF
               ENDIF

               ! Save the original, undivided obstruction lengths and face areas.

               UNDIVIDED_INPUT_LENGTH(1) = ABS(XB2-XB1)
               UNDIVIDED_INPUT_LENGTH(2) = ABS(XB4-XB3)
               UNDIVIDED_INPUT_LENGTH(3) = ABS(XB6-XB5)

               UNDIVIDED_INPUT_AREA(1) = (XB4-XB3)*(XB6-XB5)
               UNDIVIDED_INPUT_AREA(2) = (XB2-XB1)*(XB6-XB5)
               UNDIVIDED_INPUT_AREA(3) = (XB2-XB1)*(XB4-XB3)

               ! Throw out obstructions that are not within computational domain

               XB1 = MAX(XB1,XS)
               XB2 = MIN(XB2,XF)
               XB3 = MAX(XB3,YS)
               XB4 = MIN(XB4,YF)
               XB5 = MAX(XB5,ZS)
               XB6 = MIN(XB6,ZF)
               IF (XB1>XF .OR. XB2<XS .OR. XB3>YF .OR. XB4<YS .OR. XB5>ZF .OR. XB6<ZS .OR. REJECT_OBST) THEN
                  N = N-1
                  N_OBST = N_OBST-1
                  CYCLE I_MULT_LOOP
               ENDIF

               ! Begin processing of OBSTruction

               OB=>OBSTRUCTION(N)

               OB%UNDIVIDED_INPUT_LENGTH(1:3) = UNDIVIDED_INPUT_LENGTH(1:3)
               OB%UNDIVIDED_INPUT_AREA(1:3)   = UNDIVIDED_INPUT_AREA(1:3)

               OB%X1 = XB1
               OB%X2 = XB2
               OB%Y1 = XB3
               OB%Y2 = XB4
               OB%Z1 = XB5
               OB%Z2 = XB6

               ! Determine the indices of the obstruction according to cell edges, not centers.

               OB%I1 = NINT( GINV(XB1-XS,1,NM)*RDXI   )
               OB%I2 = NINT( GINV(XB2-XS,1,NM)*RDXI   )
               OB%J1 = NINT( GINV(XB3-YS,2,NM)*RDETA  )
               OB%J2 = NINT( GINV(XB4-YS,2,NM)*RDETA  )
               OB%K1 = NINT( GINV(XB5-ZS,3,NM)*RDZETA )
               OB%K2 = NINT( GINV(XB6-ZS,3,NM)*RDZETA )

               ! If desired, thicken small obstructions

               IF (THICKEN_LOC) THEN
                  IF(OB%I1==OB%I2) THEN
                     OB%I1 = INT(GINV(.5_EB*(XB1+XB2)-XS,1,NM)*RDXI)
                     OB%I2 = MIN(OB%I1+1,IBAR)
                  ENDIF
                  IF (OB%J1==OB%J2) THEN
                     OB%J1 = INT(GINV(.5_EB*(XB3+XB4)-YS,2,NM)*RDETA)
                     OB%J2 = MIN(OB%J1+1,JBAR)
                  ENDIF
                  IF (OB%K1==OB%K2) THEN
                     OB%K1 = INT(GINV(.5_EB*(XB5+XB6)-ZS,3,NM)*RDZETA)
                     OB%K2 = MIN(OB%K1+1,KBAR)
                  ENDIF
               ELSE
                  !Don't allow thickening if an OBST straddles the midpoint and is small compared to grid cell
                  IF (GINV(XB2-XS,1,NM)-GINV(XB1-XS,1,NM)<0.25_EB/RDXI .AND. OB%I1 /= OB%I2) THEN
                     IF(GINV(XB1-XS,1,NM)-REAL(OB%I1,EB) < REAL(OB%I2,EB) - GINV(XB2-XS,1,NM)) THEN
                        OB%I2=OB%I1
                     ELSE
                        OB%I1=OB%I2
                     ENDIF
                  ENDIF
                  IF (GINV(XB4-YS,2,NM)-GINV(XB3-YS,2,NM)<0.25_EB/RDETA .AND. OB%J1 /= OB%J2) THEN
                     IF(GINV(XB3-XS,2,NM)-REAL(OB%J1,EB) < REAL(OB%J2,EB) - GINV(XB4-YS,2,NM)) THEN
                        OB%J2=OB%J1
                     ELSE
                        OB%J1=OB%J2
                     ENDIF
                  ENDIF
                  IF (GINV(XB6-ZS,3,NM)-GINV(XB5-ZS,3,NM)<0.25_EB/RDZETA .AND. OB%K1 /= OB%K2) THEN
                     IF(GINV(XB5-ZS,3,NM)-REAL(OB%I1,EB) < REAL(OB%I2,EB) - GINV(XB6-ZS,3,NM)) THEN
                        OB%K2=OB%K1
                     ELSE
                        OB%K1=OB%K2
                     ENDIF
                  ENDIF
               ENDIF

               ! Throw out obstructions that are too small

               IF ((OB%I1==OB%I2.AND.OB%J1==OB%J2) .OR. (OB%I1==OB%I2.AND.OB%K1==OB%K2) .OR. (OB%J1==OB%J2.AND.OB%K1==OB%K2)) THEN
                  N = N-1
                  N_OBST= N_OBST-1
                  CYCLE I_MULT_LOOP
               ENDIF

               ! Throw out obstructions that are outside shape hull

               IF (SHAPE_TYPE>0) THEN
                  !AABB = (/XB1,XB2,XB3,XB4,XB5,XB6/)
                  AABB = (/X(OB%I1),X(OB%I2),Y(OB%J1),Y(OB%J2),Z(OB%K1),Z(OB%K2)/) ! possibly THICKENed OBST
                  SELECT CASE (SHAPE_TYPE)
                     CASE (OBST_SPHERE_TYPE);   IS_INTERSECT = INTERSECT_SPHERE_AABB(XYZ,RADIUS,AABB)
                     CASE (OBST_CYLINDER_TYPE); IS_INTERSECT = INTERSECT_CYLINDER_AABB(XYZ,HEIGHT,RADIUS,ROTMAT,AABB)
                     CASE (OBST_CONE_TYPE);     IS_INTERSECT = INTERSECT_CONE_AABB(XYZ,HEIGHT,RADIUS,ROTMAT,AABB)
                     CASE (OBST_BOX_TYPE);      IS_INTERSECT = INTERSECT_OBB_AABB(XYZ,LENGTH,WIDTH,HEIGHT,ROTMAT,AABB)
                  END SELECT
                  IF (.NOT.IS_INTERSECT) THEN
                     N = N-1
                     N_OBST = N_OBST-1
                     CYCLE I_MULT_LOOP
                  ENDIF
                  ! Snap OBST to mesh, special AREA_ADJUST required for SHAPE (see init.90)
                  OB%X1 = X(OB%I1)
                  OB%X2 = X(OB%I2)
                  OB%Y1 = Y(OB%J1)
                  OB%Y2 = Y(OB%J2)
                  OB%Z1 = Z(OB%K1)
                  OB%Z2 = Z(OB%K2)
                  ! Store SHAPE_AREA for area adjustment
                  OB%SHAPE_TYPE = SHAPE_TYPE
                  OB%SHAPE_AREA = SHAPE_AREA
                  OB%MULT_INDEX = MULT_INDEX
               ENDIF

               ! Check to see if obstruction is completely embedded in another

               EMBEDDED = .FALSE.
               EMBED_LOOP: DO NNN=1,N-1
                  OB2=>OBSTRUCTION(NNN)
                  IF (OB%I1>=OB2%I1 .AND. OB%I2<=OB2%I2 .AND. &
                      OB%J1>=OB2%J1 .AND. OB%J2<=OB2%J2 .AND. &
                      OB%K1>=OB2%K1 .AND. OB%K2<=OB2%K2) THEN
                     EMBEDDED = .TRUE.
                     EXIT EMBED_LOOP
                  ENDIF
               ENDDO EMBED_LOOP

               ! Remove obstructions that are within another and have no controls or devices

               IF (EMBEDDED .AND. DEVC_ID=='null' .AND. CTRL_ID=='null' .AND. PERMIT_HOLE .AND. REMOVABLE) THEN
                  N = N-1
                  N_OBST= N_OBST-1
                  CYCLE I_MULT_LOOP
               ENDIF

               ! Check if the SURF IDs exist

               IF (SURF_ID/='null') THEN
                  CALL CHECK_SURF_NAME(SURF_ID,EX)
                  IF (.NOT.EX) THEN
                     WRITE(MESSAGE,'(A,A,A)')  'ERROR(605): SURF_ID ',TRIM(SURF_ID),' does not exist.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
               ENDIF

               IF (SURF_ID_INTERIOR/='null') THEN
                  CALL CHECK_SURF_NAME(SURF_ID_INTERIOR,EX)
                  IF (.NOT.EX) THEN
                     WRITE(MESSAGE,'(A,A,A)')  'ERROR(606): SURF_ID_INTERIOR ',TRIM(SURF_ID_INTERIOR),' does not exist.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
               ENDIF

               DO NNNN=1,3
                  IF (SURF_IDS(NNNN)/='null') THEN
                     CALL CHECK_SURF_NAME(SURF_IDS(NNNN),EX)
                     IF (.NOT.EX) THEN
                        WRITE(MESSAGE,'(A,A,A)')  'ERROR(605): SURF_ID ',TRIM(SURF_IDS(NNNN)),' does not exist.'
                        CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                     ENDIF
                  ENDIF
               ENDDO

               DO NNNN=1,6
                  IF (SURF_ID6(NNNN)/='null') THEN
                     CALL CHECK_SURF_NAME(SURF_ID6(NNNN),EX)
                     IF (.NOT.EX) THEN
                        WRITE(MESSAGE,'(A,A,A)')  'ERROR(605): SURF_ID ',TRIM(SURF_ID6(NNNN)),' does not exist.'
                        CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                     ENDIF
                  ENDIF
               ENDDO

               ! Check for thin obstruction

               IF (OB%I1==OB%I2 .AND. OB%I1>0 .AND. OB%I2<IBAR) OB%THIN = .TRUE.
               IF (OB%J1==OB%J2 .AND. OB%J1>0 .AND. OB%J2<JBAR) OB%THIN = .TRUE.
               IF (OB%K1==OB%K2 .AND. OB%K1>0 .AND. OB%K2<KBAR) OB%THIN = .TRUE.

               ! Save boundary condition info for obstacles

               OB%SURF_INDEX(:) = DEFAULT_SURF_INDEX

               NNNN = 0
               DO NNN=0,N_SURF
                  IF (SURF_ID          ==SURFACE(NNN)%ID) OB%SURF_INDEX(:)    = NNN
                  IF (SURF_ID_INTERIOR ==SURFACE(NNN)%ID) OB%SURF_INDEX_INTERIOR = NNN
                  IF (SURF_IDS(1)      ==SURFACE(NNN)%ID) OB%SURF_INDEX(3)    = NNN
                  IF (SURF_IDS(2)      ==SURFACE(NNN)%ID) OB%SURF_INDEX(-2:2) = NNN
                  IF (SURF_IDS(3)      ==SURFACE(NNN)%ID) OB%SURF_INDEX(-3)   = NNN
                  IF (SURF_ID6(1)      ==SURFACE(NNN)%ID) OB%SURF_INDEX(-1)   = NNN
                  IF (SURF_ID6(2)      ==SURFACE(NNN)%ID) OB%SURF_INDEX( 1)   = NNN
                  IF (SURF_ID6(3)      ==SURFACE(NNN)%ID) OB%SURF_INDEX(-2)   = NNN
                  IF (SURF_ID6(4)      ==SURFACE(NNN)%ID) OB%SURF_INDEX( 2)   = NNN
                  IF (SURF_ID6(5)      ==SURFACE(NNN)%ID) OB%SURF_INDEX(-3)   = NNN
                  IF (SURF_ID6(6)      ==SURFACE(NNN)%ID) OB%SURF_INDEX( 3)   = NNN
               ENDDO

               ! If the obstruction is assigned a SURF with 3-D heat transfer (HT3D), adjust the nominal layer thickness for this
               ! surface. Storage arrays for WALL and THIN_WALL are based on the maximum thickness.
               ! Also, look for thin obstructions that could off-gas so that certain arrays can be allocated.

               DO IOR=-3,3
                  IF (IOR==0) CYCLE
                  SF => SURFACE(OB%SURF_INDEX(IOR))
                  DO NNN=1,SF%N_MATL
                     ML => MATERIAL(SF%MATL_INDEX(NNN))
                     IF (ML%N_REACTIONS>0 .AND. OB%THIN) REACTING_THIN_OBSTRUCTIONS = .TRUE.
                  ENDDO
               ENDDO

               ! Determine if the OBST is CONSUMABLE

               FACE_LOOP: DO NNN=-3,3
                  IF (NNN==0) CYCLE FACE_LOOP
                  IF (SURFACE(OB%SURF_INDEX(NNN))%BURN_AWAY) OB%CONSUMABLE = .TRUE.
               ENDDO FACE_LOOP

               ! Calculate the increase or decrease in the obstruction volume over the user-specified

               VOL_SPECIFIED = (OB%X2-OB%X1)*(OB%Y2-OB%Y1)*(OB%Z2-OB%Z1)
               VOL_ADJUSTED  = (X(OB%I2)-X(OB%I1))*(Y(OB%J2)-Y(OB%J1))*(Z(OB%K2)-Z(OB%K1))
               IF (VOL_SPECIFIED>0._EB) THEN
                  OB%VOLUME_ADJUST = VOL_ADJUSTED/VOL_SPECIFIED
               ELSE
                  OB%VOLUME_ADJUST = 0._EB
               ENDIF

               ! Creation and removal logic

               OB%ID      = ID
               OB%DEVC_ID = DEVC_ID
               OB%CTRL_ID = CTRL_ID
               OB%HIDDEN = .FALSE.

               ! Property ID

               CALL SEARCH_CONTROLLER('OBST',CTRL_ID,DEVC_ID,OB%DEVC_INDEX,OB%CTRL_INDEX,N)
               IF (DEVC_ID /='null' .OR. CTRL_ID /='null') OB%REMOVABLE = .TRUE.

               IF (OB%CONSUMABLE) OB%REMOVABLE = .TRUE.

               ! Choose obstruction color index

               SELECT CASE (COLOR)
                  CASE ('INVISIBLE')
                     OB%COLOR_INDICATOR = -3
                     RGB(1) = 255
                     RGB(2) = 204
                     RGB(3) = 102
                     TRANSPARENCY = 0._EB
                  CASE ('null')
                     IF (ANY (RGB<0)) THEN
                        OB%COLOR_INDICATOR = -1
                     ELSE
                        OB%COLOR_INDICATOR = -3
                     ENDIF
                  CASE DEFAULT
                     CALL COLOR2RGB(RGB,COLOR)
                     OB%COLOR_INDICATOR = -3
               END SELECT
               OB%RGB  = RGB
               OB%TRANSPARENCY = TRANSPARENCY

               ! Miscellaneous assignments

               OB%TEXTURE(:) = TEXTURE_ORIGIN(:)  ! Origin of texture map
               OB%ORDINAL = NN  ! Order of OBST in original input file
               OB%PERMIT_HOLE = PERMIT_HOLE
               OB%ALLOW_VENT  = ALLOW_VENT
               OB%OVERLAY     = OVERLAY

               ! Only allow the use of BULK_DENSITY if the obstruction has a non-zero volume

               OB%BULK_DENSITY = BULK_DENSITY
               IF (BULK_DENSITY > 0._EB) OB%MASS = OB%BULK_DENSITY*(OB%X2-OB%X1)*(OB%Y2-OB%Y1)*(OB%Z2-OB%Z1)

               ! Check for inconsistencies in specification of BUL_DENSITY

               IF (OB%CONSUMABLE .AND. OB%BULK_DENSITY <= 0._EB) THEN
                  DO IOR=-2,3
                     IF (IOR==0) CYCLE
                     IF (OB%SURF_INDEX(IOR)/=OB%SURF_INDEX(-3)) THEN
                        WRITE(MESSAGE,'(A,A,A)')  'ERROR(607): OBST ',TRIM(OB%ID),' needs a BULK_DENSITY if it is to BURN_AWAY.'
                        CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                     ENDIF
                  ENDDO
               ENDIF

               ! Make obstruction invisible if it's within a finer mesh

               DO NOM=1,NM-1
                  IF (XB1>MESHES(NOM)%XS .AND. XB2<MESHES(NOM)%XF .AND. &
                      XB3>MESHES(NOM)%YS .AND. XB4<MESHES(NOM)%YF .AND. &
                      XB5>MESHES(NOM)%ZS .AND. XB6<MESHES(NOM)%ZF) OB%COLOR_INDICATOR=-2
               ENDDO

               ! Prevent drawing of boundary info if desired

               IF (BNDF_DEFAULT) THEN
                  OB%SHOW_BNDF(:) = BNDF_FACE(:)
                  IF (.NOT.BNDF_OBST) OB%SHOW_BNDF(:) = .FALSE.
               ELSE
                  OB%SHOW_BNDF(:) = BNDF_FACE(:)
                  IF (BNDF_OBST) OB%SHOW_BNDF(:) = .TRUE.
               ENDIF

               ! In Smokeview, draw the outline of the obstruction

               IF (OUTLINE) OB%TYPE_INDICATOR = 2

               ! Read in optional material components

               DO NNN=1,MAX_MATERIALS
                  IF (MATL_ID(NNN) == 'null') EXIT
                  IF (MATL_MASS_FRACTION(NNN)<TWO_EPSILON_EB) EXIT
                  FOUND = .FALSE.
                  DO NNNN=1,N_MATL
                     IF (MATL_ID(NNN)==MATERIAL(NNNN)%ID) THEN
                        FOUND = .TRUE.
                        EXIT
                     ENDIF
                  ENDDO
                  IF (.NOT.FOUND) THEN
                     WRITE(MESSAGE,'(A,A,A)') 'ERROR(608): MATL_ID ', TRIM(MATL_ID(NNN)),' not found.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
                  OB%MATL_INDEX(NNN) = NNNN
                  OB%MATL_MASS_FRACTION(NNN) = MATL_MASS_FRACTION(NNN)
               ENDDO

               ! Internal heat source

               IF (CELL_SIZE>0._EB) STRETCH_FACTOR = 1._EB
               OB%HEAT_SOURCE = INTERNAL_HEAT_SOURCE*1000._EB
               OB%STRETCH_FACTOR = STRETCH_FACTOR
               OB%CELL_SIZE = CELL_SIZE
               OB%CELL_SIZE_FACTOR = CELL_SIZE_FACTOR
               OB%N_LAYER_CELLS_MAX = N_LAYER_CELLS_MAX

            ENDDO I_MULT_LOOP
         ENDDO J_MULT_LOOP
      ENDDO K_MULT_LOOP

   ENDDO READ_OBST_LOOP
35 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ENDDO MESH_LOOP

! Read HOLEs and cut out blocks

CALL READ_HOLE

! Look for OBSTructions that are meant to BURN_AWAY and break them up into single cell blocks

MESH_LOOP_2: DO NM=1,NMESHES

   M => MESHES(NM)

   IF (.NOT.PROCESS_MESH_NEIGHBORHOOD(NM)) CYCLE MESH_LOOP_2

   CALL POINT_TO_MESH(NM)

   N_OBST_O = N_OBST
   DO N=1,N_OBST_O
      OB => OBSTRUCTION(N)
      IF (OB%CONSUMABLE) THEN

         N_NEW_OBST = MAX(1,OB%I2-OB%I1)*MAX(1,OB%J2-OB%J1)*MAX(1,OB%K2-OB%K1)
         IF (N_NEW_OBST > 1) THEN

            ! Create a temporary array of obstructions with the same properties as the one being replaced, except coordinates

            ALLOCATE(TEMP_OBSTRUCTION(N_NEW_OBST))
            DO NN=1,N_NEW_OBST
               TEMP_OBSTRUCTION(NN) = OBSTRUCTION(N)
            ENDDO
            NN = 0
            DO K=OB%K1,MAX(OB%K1,OB%K2-1)
               DO J=OB%J1,MAX(OB%J1,OB%J2-1)
                  DO I=OB%I1,MAX(OB%I1,OB%I2-1)
                     NN = NN + 1
                     OBT=>TEMP_OBSTRUCTION(NN)
                     OBT%I1 = I
                     OBT%I2 = MIN(I+1,OB%I2)
                     OBT%J1 = J
                     OBT%J2 = MIN(J+1,OB%J2)
                     OBT%K1 = K
                     OBT%K2 = MIN(K+1,OB%K2)
                     OBT%X1 = M%X(OBT%I1)
                     OBT%X2 = M%X(OBT%I2)
                     OBT%Y1 = M%Y(OBT%J1)
                     OBT%Y2 = M%Y(OBT%J2)
                     OBT%Z1 = M%Z(OBT%K1)
                     OBT%Z2 = M%Z(OBT%K2)
                     ! For thin obstructions, maintain original thickness
                     IF (OB%I1==OB%I2) THEN ; OBT%X1=OB%X1 ; OBT%X2=OB%X2 ; ENDIF
                     IF (OB%J1==OB%J2) THEN ; OBT%Y1=OB%Y1 ; OBT%Y2=OB%Y2 ; ENDIF
                     IF (OB%K1==OB%K2) THEN ; OBT%Z1=OB%Z1 ; OBT%Z2=OB%Z2 ; ENDIF
                     IF (OB%BULK_DENSITY > 0._EB) OBT%MASS = OB%MASS/REAL(N_NEW_OBST,EB)
                  ENDDO
                ENDDO
            ENDDO

            CALL RE_ALLOCATE_OBST(NM,N_OBST,N_NEW_OBST-1)
            OBSTRUCTION=>M%OBSTRUCTION
            OBSTRUCTION(N) = TEMP_OBSTRUCTION(1)
            OBSTRUCTION(N_OBST+1:N_OBST+N_NEW_OBST-1) = TEMP_OBSTRUCTION(2:N_NEW_OBST)
            N_OBST = N_OBST + N_NEW_OBST-1
            DEALLOCATE(TEMP_OBSTRUCTION)

         ENDIF
      ENDIF
   ENDDO

ENDDO MESH_LOOP_2

! Allocate the number of cells for each mesh that are SOLID or border a boundary

ALLOCATE(CELL_COUNT(NMESHES)) ; CELL_COUNT = 0
ALLOCATE(CELL_COUNT_INTEGERS(NMESHES)) ; CELL_COUNT_INTEGERS = 0
ALLOCATE(CELL_COUNT_LOGICALS(NMESHES)) ; CELL_COUNT_LOGICALS = 0

ALLOCATE(EDGE_COUNT(NMESHES)) ; EDGE_COUNT = 0

! Go through all meshes, recording which cells are solid

MESH_LOOP_3: DO NM=1,NMESHES

   M => MESHES(NM)

   IF (.NOT.PROCESS_MESH_NEIGHBORHOOD(NM)) CYCLE MESH_LOOP_3

   CALL POINT_TO_MESH(NM)

   ! Compute areas of obstruction faces, both actual (AB0) and FDS approximated (AB)

   DO N=1,N_OBST
      OB=>OBSTRUCTION(N)
      OB%INPUT_AREA(1) = (OB%Y2-OB%Y1)*(OB%Z2-OB%Z1)
      OB%INPUT_AREA(2) = (OB%X2-OB%X1)*(OB%Z2-OB%Z1)
      OB%INPUT_AREA(3) = (OB%X2-OB%X1)*(OB%Y2-OB%Y1)
      OB%FDS_AREA(1)   = (Y(OB%J2)-Y(OB%J1))*(Z(OB%K2)-Z(OB%K1))
      OB%FDS_AREA(2)   = (X(OB%I2)-X(OB%I1))*(Z(OB%K2)-Z(OB%K1))
      OB%FDS_AREA(3)   = (X(OB%I2)-X(OB%I1))*(Y(OB%J2)-Y(OB%J1))
   ENDDO

   ! Create main blockage index array (ICA)

   ALLOCATE(M%CELL_INDEX(0:IBP1,0:JBP1,0:KBP1),STAT=IZERO)
   CALL ChkMemErr('READ','CELL_INDEX',IZERO) ; CELL_INDEX=>M%CELL_INDEX ; CELL_INDEX = 0

   DO K=0,KBP1
      DO J=0,JBP1
         DO I=0,1
            IF (CELL_INDEX(I,J,K)==0) THEN
               CELL_COUNT(NM) = CELL_COUNT(NM) + 1
               CELL_INDEX(I,J,K) = CELL_COUNT(NM)
            ENDIF
         ENDDO
         DO I=IBAR,IBP1
            IF (CELL_INDEX(I,J,K)==0) THEN
               CELL_COUNT(NM) = CELL_COUNT(NM) + 1
               CELL_INDEX(I,J,K) = CELL_COUNT(NM)
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   DO K=0,KBP1
      DO I=0,IBP1
         DO J=0,1
            IF (CELL_INDEX(I,J,K)==0) THEN
               CELL_COUNT(NM) = CELL_COUNT(NM) + 1
               CELL_INDEX(I,J,K) = CELL_COUNT(NM)
            ENDIF
         ENDDO
         DO J=JBAR,JBP1
            IF (CELL_INDEX(I,J,K)==0) THEN
               CELL_COUNT(NM) = CELL_COUNT(NM) + 1
               CELL_INDEX(I,J,K) = CELL_COUNT(NM)
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   DO J=0,JBP1
      DO I=0,IBP1
         DO K=0,1
            IF (CELL_INDEX(I,J,K)==0) THEN
               CELL_COUNT(NM) = CELL_COUNT(NM) + 1
               CELL_INDEX(I,J,K) = CELL_COUNT(NM)
            ENDIF
         ENDDO
         DO K=KBAR,KBP1
            IF (CELL_INDEX(I,J,K)==0) THEN
               CELL_COUNT(NM) = CELL_COUNT(NM) + 1
               CELL_INDEX(I,J,K) = CELL_COUNT(NM)
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   DO N=1,N_OBST
      OB=>OBSTRUCTION(N)
      DO K=OB%K1,OB%K2+1
         DO J=OB%J1,OB%J2+1
            DO I=OB%I1,OB%I2+1
               IF (CELL_INDEX(I,J,K)==0) THEN
                  CELL_COUNT(NM) = CELL_COUNT(NM) + 1
                  CELL_INDEX(I,J,K) = CELL_COUNT(NM)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ! Store in SOLID which cells are solid and which are not

   CALL REALLOCATE_CELL(NM,CELL_COUNT(NM),CELL_COUNT(NM))
   CELL => M%CELL

   ! Make all exterior cells solid

   CALL BLOCK_CELL(NM,   0,   0,   0,JBP1,   0,KBP1,1,0)
   CALL BLOCK_CELL(NM,IBP1,IBP1,   0,JBP1,   0,KBP1,1,0)
   CALL BLOCK_CELL(NM,   0,IBP1,   0,   0,   0,KBP1,1,0)
   CALL BLOCK_CELL(NM,   0,IBP1,JBP1,JBP1,   0,KBP1,1,0)
   CALL BLOCK_CELL(NM,   0,IBP1,   0,JBP1,   0,   0,1,0)
   CALL BLOCK_CELL(NM,   0,IBP1,   0,JBP1,KBP1,KBP1,1,0)

   ! Block off cells filled by obstructions

   DO N=1,N_OBST
      OB=>OBSTRUCTION(N)
      CALL BLOCK_CELL(NM,OB%I1+1,OB%I2,OB%J1+1,OB%J2,OB%K1+1,OB%K2,1,N)
   ENDDO

   DO K=0,KBP1
      DO J=0,JBP1
         DO I=0,IBP1
            IC = CELL_INDEX(I,J,K)
            IF (IC>0) THEN
               CELL(IC)%I = I
               CELL(IC)%J = J
               CELL(IC)%K = K
            ENDIF
         ENDDO
      ENDDO
   ENDDO

ENDDO MESH_LOOP_3

END SUBROUTINE READ_OBST


!> \brief Read the HOLE namelist lines

SUBROUTINE READ_HOLE

USE MISC_FUNCTIONS, ONLY: PROCESS_MESH_NEIGHBORHOOD
CHARACTER(LABEL_LENGTH) :: DEVC_ID,CTRL_ID,MULT_ID
CHARACTER(25) :: COLOR
INTEGER :: NM,N_HOLE,NN,NDO,N,I1,I2,J1,J2,K1,K2,RGB(3),N_HOLE_NEW,N_HOLE_O,II,JJ,KK,NNN,DEVC_INDEX_O,CTRL_INDEX_O
REAL(EB) :: X1,X2,Y1,Y2,Z1,Z2,TRANSPARENCY
NAMELIST /HOLE/ COLOR,CTRL_ID,DEVC_ID,FYI,ID,MULT_ID,RGB,TRANSPARENCY,XB
REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: TEMP_XB
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CONTROLLED
TYPE(OBSTRUCTION_TYPE), ALLOCATABLE, DIMENSION(:) :: TEMP_OBST
TYPE(MULTIPLIER_TYPE), POINTER :: MR=>NULL()

ALLOCATE(TEMP_OBST(0:6))

N_HOLE    = 0
N_HOLE_O  = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

COUNT_LOOP: DO
   CALL CHECKREAD('HOLE',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_LOOP
   MULT_ID = 'null'
   READ(LU_INPUT,NML=HOLE,END=1,ERR=2,IOSTAT=IOS)
   N_HOLE_O = N_HOLE_O + 1
   N_HOLE_NEW = 0
   IF (MULT_ID=='null') THEN
      N_HOLE_NEW = 1
   ELSE
      DO N=1,N_MULT
         MR => MULTIPLIER(N)
         IF (MULT_ID==MR%ID) N_HOLE_NEW = MR%N_COPIES
      ENDDO
      IF (N_HOLE_NEW==0) THEN
         WRITE(MESSAGE,'(A,A,A,I0)') 'ERROR(609): MULT_ID ',TRIM(MULT_ID),' not found on HOLE line ',N_HOLE_O
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF
   ENDIF
   N_HOLE   = N_HOLE   + N_HOLE_NEW
   2 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0,A,I0)')  'ERROR(101): Problem with HOLE number ',N_HOLE_O+1,', line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
   ENDIF
ENDDO COUNT_LOOP
1 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ALLOCATE (TEMP_XB(N_HOLE_O,6))
TEMP_XB = 0._EB
ALLOCATE (CONTROLLED(N_HOLE_O))
CONTROLLED = .FALSE.

READ_HOLE_LOOP: DO N=1,N_HOLE_O

   ! Set default values for the HOLE namelist parameters

   DEVC_ID  = 'null'
   CTRL_ID  = 'null'
   ID       = 'null'
   MULT_ID  = 'null'
   COLOR    = 'null'
   RGB      = -1
   TRANSPARENCY = 1._EB
   XB       = -9.E30_EB

   ! Read the HOLE line

   CALL CHECKREAD('HOLE',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_HOLE_LOOP
   READ(LU_INPUT,HOLE)

   ! Re-order coordinates, if necessary

   CALL CHECK_XB(XB)
   TEMP_XB(N,:) = XB
   ! Check for overlap if controlled
   IF (DEVC_ID/='null' .OR. CTRL_ID/='null') CONTROLLED(N) = .TRUE.
   IF (N>1) THEN
      DO NN = 1,N-1
         IF (TEMP_XB(NN,1) >= XB(2) .OR. TEMP_XB(NN,3) >= XB(4) .OR. TEMP_XB(NN,5) >= XB(6) .OR. &
             TEMP_XB(NN,2) <= XB(1) .OR. TEMP_XB(NN,4) <= XB(3) .OR. TEMP_XB(NN,6) <= XB(5)) CYCLE
         IF ((TEMP_XB(NN,1) <= XB(2) .AND. TEMP_XB(NN,2) >= XB(1)) .AND. &
             (TEMP_XB(NN,3) <= XB(4) .AND. TEMP_XB(NN,4) >= XB(3)) .AND. &
             (TEMP_XB(NN,5) <= XB(6) .AND. TEMP_XB(NN,6) >= XB(5))) THEN
            IF (CONTROLLED(N) .OR. CONTROLLED(NN)) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(610): HOLE number ',N_HOLE_O+1,' Cannot overlap HOLEs with a DEVC or CTRL_ID.'
               CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
            ENDIF
         ENDIF
      ENDDO
   ENDIF

   ! Loop over all the meshes to determine where the HOLE is

   MESH_LOOP: DO NM=1,NMESHES

      IF (.NOT.PROCESS_MESH_NEIGHBORHOOD(NM)) CYCLE MESH_LOOP

      M=>MESHES(NM)
      CALL POINT_TO_MESH(NM)

      ! Loop over all possible multiples of the HOLE

      MR => MULTIPLIER(0)
      DO NNN=1,N_MULT
         IF (MULT_ID==MULTIPLIER(NNN)%ID) MR => MULTIPLIER(NNN)
      ENDDO

      K_MULT_LOOP: DO KK=MR%K_LOWER,MR%K_UPPER
         J_MULT_LOOP: DO JJ=MR%J_LOWER,MR%J_UPPER
            I_MULT_LOOP: DO II=MR%I_LOWER,MR%I_UPPER

               IF (MR%SKIP(II,JJ,KK)) CYCLE I_MULT_LOOP

               IF (.NOT.MR%SEQUENTIAL) THEN
                  X1 = XB(1) + MR%DX0 + II*MR%DXB(1)
                  X2 = XB(2) + MR%DX0 + II*MR%DXB(2)
                  Y1 = XB(3) + MR%DY0 + JJ*MR%DXB(3)
                  Y2 = XB(4) + MR%DY0 + JJ*MR%DXB(4)
                  Z1 = XB(5) + MR%DZ0 + KK*MR%DXB(5)
                  Z2 = XB(6) + MR%DZ0 + KK*MR%DXB(6)
               ELSE
                  X1 = XB(1) + MR%DX0 + II*MR%DXB(1)
                  X2 = XB(2) + MR%DX0 + II*MR%DXB(2)
                  Y1 = XB(3) + MR%DY0 + II*MR%DXB(3)
                  Y2 = XB(4) + MR%DY0 + II*MR%DXB(4)
                  Z1 = XB(5) + MR%DZ0 + II*MR%DXB(5)
                  Z2 = XB(6) + MR%DZ0 + II*MR%DXB(6)
               ENDIF

               ! Check if hole is contained within the current mesh

               IF (X1>=XF .OR. X2<=XS .OR. Y1>YF .OR. Y2<=YS .OR. Z1>ZF .OR. Z2<=ZS) CYCLE I_MULT_LOOP

               ! Assign mesh-limited bounds

               X1 = MAX(X1,XS-0.001_EB*DX(0))
               X2 = MIN(X2,XF+0.001_EB*DX(IBP1))
               Y1 = MAX(Y1,YS-0.001_EB*DY(0))
               Y2 = MIN(Y2,YF+0.001_EB*DY(JBP1))
               Z1 = MAX(Z1,ZS-0.001_EB*DZ(0))
               Z2 = MIN(Z2,ZF+0.001_EB*DZ(KBP1))

               I1 = NINT( GINV(X1-XS,1,NM)*RDXI   )
               I2 = NINT( GINV(X2-XS,1,NM)*RDXI   )
               J1 = NINT( GINV(Y1-YS,2,NM)*RDETA  )
               J2 = NINT( GINV(Y2-YS,2,NM)*RDETA  )
               K1 = NINT( GINV(Z1-ZS,3,NM)*RDZETA )
               K2 = NINT( GINV(Z2-ZS,3,NM)*RDZETA )

               NN=0
               OBST_LOOP: DO
                  NN=NN+1
                  IF (NN>N_OBST) EXIT OBST_LOOP
                  OB=>OBSTRUCTION(NN)
                  DEVC_INDEX_O = OB%DEVC_INDEX
                  CTRL_INDEX_O = OB%CTRL_INDEX
                  IF (.NOT.OB%PERMIT_HOLE) CYCLE OBST_LOOP

                  ! TEMP_OBST(0) is the intersection of HOLE and OBST

                  TEMP_OBST(0)    = OBSTRUCTION(NN)

                  TEMP_OBST(0)%I1 = MAX(I1,OB%I1)
                  TEMP_OBST(0)%I2 = MIN(I2,OB%I2)
                  TEMP_OBST(0)%J1 = MAX(J1,OB%J1)
                  TEMP_OBST(0)%J2 = MIN(J2,OB%J2)
                  TEMP_OBST(0)%K1 = MAX(K1,OB%K1)
                  TEMP_OBST(0)%K2 = MIN(K2,OB%K2)

                  TEMP_OBST(0)%X1 = MAX(X1,OB%X1)
                  TEMP_OBST(0)%X2 = MIN(X2,OB%X2)
                  TEMP_OBST(0)%Y1 = MAX(Y1,OB%Y1)
                  TEMP_OBST(0)%Y2 = MIN(Y2,OB%Y2)
                  TEMP_OBST(0)%Z1 = MAX(Z1,OB%Z1)
                  TEMP_OBST(0)%Z2 = MIN(Z2,OB%Z2)

                  ! Ignore OBSTs that do not intersect with HOLE or are merely sliced by the hole.

                  IF (TEMP_OBST(0)%I2-TEMP_OBST(0)%I1<0 .OR. TEMP_OBST(0)%J2-TEMP_OBST(0)%J1<0 .OR. &
                      TEMP_OBST(0)%K2-TEMP_OBST(0)%K1<0) CYCLE OBST_LOOP
                  IF (TEMP_OBST(0)%I2-TEMP_OBST(0)%I1==0) THEN
                     IF (OB%I1<TEMP_OBST(0)%I1 .OR.  OB%I2>TEMP_OBST(0)%I2) CYCLE OBST_LOOP
                  ENDIF
                  IF (TEMP_OBST(0)%J2-TEMP_OBST(0)%J1==0) THEN
                     IF (OB%J1<TEMP_OBST(0)%J1 .OR.  OB%J2>TEMP_OBST(0)%J2) CYCLE OBST_LOOP
                  ENDIF
                  IF (TEMP_OBST(0)%K2-TEMP_OBST(0)%K1==0) THEN
                     IF (OB%K1<TEMP_OBST(0)%K1 .OR.  OB%K2>TEMP_OBST(0)%K2) CYCLE OBST_LOOP
                  ENDIF

                  IF (TEMP_OBST(0)%X2<=X1 .OR. TEMP_OBST(0)%X1>=X2 .OR. TEMP_OBST(0)%Y2<=Y1 .OR. TEMP_OBST(0)%Y1>=Y2 .OR. &
                     TEMP_OBST(0)%Z2<=Z1 .OR. TEMP_OBST(0)%Z1>=Z2)  CYCLE OBST_LOOP

                  ! Start counting new OBSTs that need to be created

                  NDO=0

                  IF ((OB%I1<I1.AND.I1<OB%I2) .OR. (XB(1)>=XS.AND.I1==0.AND.OB%I1==0)) THEN
                     NDO=NDO+1
                     TEMP_OBST(NDO)=OBSTRUCTION(NN)
                     TEMP_OBST(NDO)%I1 = OB%I1
                     TEMP_OBST(NDO)%I2 = I1
                     TEMP_OBST(NDO)%X1 = OB%X1
                     TEMP_OBST(NDO)%X2 = X1
                  ENDIF

                  IF ((OB%I1<I2.AND.I2<OB%I2) .OR. (XB(2)<=XF.AND.I2==IBAR.AND.OB%I2==IBAR)) THEN
                     NDO=NDO+1
                     TEMP_OBST(NDO)=OBSTRUCTION(NN)
                     TEMP_OBST(NDO)%I1 = I2
                     TEMP_OBST(NDO)%I2 = OB%I2
                     TEMP_OBST(NDO)%X1 = X2
                     TEMP_OBST(NDO)%X2 = OB%X2
                  ENDIF

                  IF ((OB%J1<J1.AND.J1<OB%J2) .OR. (XB(3)>=YS.AND.J1==0.AND.OB%J1==0)) THEN
                     NDO=NDO+1
                     TEMP_OBST(NDO)=OBSTRUCTION(NN)
                     TEMP_OBST(NDO)%I1 = MAX(I1,OB%I1)
                     TEMP_OBST(NDO)%I2 = MIN(I2,OB%I2)
                     TEMP_OBST(NDO)%X1 = MAX(X1,OB%X1)
                     TEMP_OBST(NDO)%X2 = MIN(X2,OB%X2)
                     TEMP_OBST(NDO)%J1 = OB%J1
                     TEMP_OBST(NDO)%J2 = J1
                     TEMP_OBST(NDO)%Y1 = OB%Y1
                     TEMP_OBST(NDO)%Y2 = Y1
                  ENDIF

                  IF ((OB%J1<J2.AND.J2<OB%J2) .OR. (XB(4)<=YF.AND.J2==JBAR.AND.OB%J2==JBAR)) THEN
                     NDO=NDO+1
                     TEMP_OBST(NDO)=OBSTRUCTION(NN)
                     TEMP_OBST(NDO)%I1 = MAX(I1,OB%I1)
                     TEMP_OBST(NDO)%I2 = MIN(I2,OB%I2)
                     TEMP_OBST(NDO)%X1 = MAX(X1,OB%X1)
                     TEMP_OBST(NDO)%X2 = MIN(X2,OB%X2)
                     TEMP_OBST(NDO)%J1 = J2
                     TEMP_OBST(NDO)%J2 = OB%J2
                     TEMP_OBST(NDO)%Y1 = Y2
                     TEMP_OBST(NDO)%Y2 = OB%Y2
                  ENDIF

                  IF ((OB%K1<K1.AND.K1<OB%K2) .OR. (XB(5)>=ZS.AND.K1==0.AND.OB%K1==0)) THEN
                     NDO=NDO+1
                     TEMP_OBST(NDO)=OBSTRUCTION(NN)
                     TEMP_OBST(NDO)%I1 = MAX(I1,OB%I1)
                     TEMP_OBST(NDO)%I2 = MIN(I2,OB%I2)
                     TEMP_OBST(NDO)%X1 = MAX(X1,OB%X1)
                     TEMP_OBST(NDO)%X2 = MIN(X2,OB%X2)
                     TEMP_OBST(NDO)%J1 = MAX(J1,OB%J1)
                     TEMP_OBST(NDO)%J2 = MIN(J2,OB%J2)
                     TEMP_OBST(NDO)%Y1 = MAX(Y1,OB%Y1)
                     TEMP_OBST(NDO)%Y2 = MIN(Y2,OB%Y2)
                     TEMP_OBST(NDO)%K1 = OB%K1
                     TEMP_OBST(NDO)%K2 = K1
                     TEMP_OBST(NDO)%Z1 = OB%Z1
                     TEMP_OBST(NDO)%Z2 = Z1
                  ENDIF

                  IF ((OB%K1<K2.AND.K2<OB%K2) .OR. (XB(6)<=ZF.AND.K2==KBAR.AND.OB%K2==KBAR)) THEN
                     NDO=NDO+1
                     TEMP_OBST(NDO)=OBSTRUCTION(NN)
                     TEMP_OBST(NDO)%I1 = MAX(I1,OB%I1)
                     TEMP_OBST(NDO)%I2 = MIN(I2,OB%I2)
                     TEMP_OBST(NDO)%X1 = MAX(X1,OB%X1)
                     TEMP_OBST(NDO)%X2 = MIN(X2,OB%X2)
                     TEMP_OBST(NDO)%J1 = MAX(J1,OB%J1)
                     TEMP_OBST(NDO)%J2 = MIN(J2,OB%J2)
                     TEMP_OBST(NDO)%Y1 = MAX(Y1,OB%Y1)
                     TEMP_OBST(NDO)%Y2 = MIN(Y2,OB%Y2)
                     TEMP_OBST(NDO)%K1 = K2
                     TEMP_OBST(NDO)%K2 = OB%K2
                     TEMP_OBST(NDO)%Z1 = Z2
                     TEMP_OBST(NDO)%Z2 = OB%Z2
                  ENDIF

                  ! Maintain ordinal rank of original obstruction, but negate it. This will be a code for Smokeview.

                  TEMP_OBST(:)%ORDINAL = -OB%ORDINAL

                  ! Re-allocate space of new OBSTs, or remove entry for dead OBST

                  NEW_OBST_IF: IF (NDO>0) THEN
                        CALL RE_ALLOCATE_OBST(NM,N_OBST,NDO)
                        OBSTRUCTION=>M%OBSTRUCTION
                        OBSTRUCTION(N_OBST+1:N_OBST+NDO) = TEMP_OBST(1:NDO)
                        N_OBST = N_OBST + NDO
                  ENDIF NEW_OBST_IF

                  ! If the HOLE is to be created or removed, save it in OBSTRUCTION(NN), the original OBST that was broken up

                  DEVC_OR_CTRL: IF (DEVC_ID/='null' .OR. CTRL_ID/='null') THEN

                     OBSTRUCTION(NN) = TEMP_OBST(0)
                     OB => OBSTRUCTION(NN)
                     OB%DEVC_INDEX_O = DEVC_INDEX_O
                     OB%CTRL_INDEX_O = CTRL_INDEX_O
                     OB%DEVC_ID = DEVC_ID
                     OB%CTRL_ID = CTRL_ID
                     CALL SEARCH_CONTROLLER('HOLE',CTRL_ID,DEVC_ID,OB%DEVC_INDEX,OB%CTRL_INDEX,N)
                     IF (DEVC_ID/='null' .OR. CTRL_ID /='null') THEN
                        OB%REMOVABLE   = .TRUE.
                        OB%HOLE_FILLER = .TRUE.
                        IF (DEVC_ID/='null') OB%CTRL_INDEX = -1
                        IF (CTRL_ID/='null') OB%DEVC_INDEX = -1
                     ENDIF
                     IF (OB%CONSUMABLE)    OB%REMOVABLE = .TRUE.

                     SELECT CASE (COLOR)
                        CASE ('INVISIBLE')
                           OB%COLOR_INDICATOR = -3
                           OB%RGB(1) = 255
                           OB%RGB(2) = 204
                           OB%RGB(3) = 102
                           OB%TRANSPARENCY = 0._EB
                        CASE ('null')
                           IF (ANY(RGB>0)) THEN
                              OB%COLOR_INDICATOR = -3
                              OB%RGB  = RGB
                              OB%TRANSPARENCY = TRANSPARENCY
                           ENDIF
                        CASE DEFAULT
                           CALL COLOR2RGB(RGB,COLOR)
                           OB%COLOR_INDICATOR = -3
                           OB%RGB  = RGB
                           OB%TRANSPARENCY = TRANSPARENCY
                     END SELECT

                  ELSE DEVC_OR_CTRL

                     OBSTRUCTION(NN) = OBSTRUCTION(N_OBST)
                     N_OBST = N_OBST-1
                     NN = NN-1

                  ENDIF DEVC_OR_CTRL

               ENDDO OBST_LOOP
            ENDDO I_MULT_LOOP
         ENDDO J_MULT_LOOP
      ENDDO K_MULT_LOOP
   ENDDO MESH_LOOP
ENDDO READ_HOLE_LOOP

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

DEALLOCATE(TEMP_OBST)
DEALLOCATE (CONTROLLED)
DEALLOCATE (TEMP_XB)

END SUBROUTINE READ_HOLE


SUBROUTINE RE_ALLOCATE_OBST(NM,N_OBST,NDO)

TYPE (OBSTRUCTION_TYPE), ALLOCATABLE, DIMENSION(:) :: DUMMY
INTEGER, INTENT(IN) :: NM,NDO,N_OBST
TYPE (MESH_TYPE), POINTER :: M=>NULL()
M=>MESHES(NM)
ALLOCATE(DUMMY(0:N_OBST))
DUMMY(0:N_OBST) = M%OBSTRUCTION(0:N_OBST)
DEALLOCATE(M%OBSTRUCTION)
ALLOCATE(M%OBSTRUCTION(0:N_OBST+NDO))
M%OBSTRUCTION(0:N_OBST) = DUMMY(0:N_OBST)
DEALLOCATE(DUMMY)
END SUBROUTINE RE_ALLOCATE_OBST


!> \brief Read the VENT namelist lines

SUBROUTINE READ_VENT

USE GEOMETRY_FUNCTIONS, ONLY : BLOCK_CELL,CIRCLE_CELL_INTERSECTION_AREA
USE DEVICE_VARIABLES, ONLY : DEVICE
USE CONTROL_VARIABLES, ONLY : CONTROL
USE MATH_FUNCTIONS, ONLY: GET_RAMP_INDEX
USE MISC_FUNCTIONS, ONLY: PROCESS_MESH_NEIGHBORHOOD

INTEGER :: N,N_TOTAL,N_EXPLICIT,NM,NNN,IOR,I1,I2,J1,J2,K1,K2,RGB(3),N_EDDY,II,JJ,KK,OBST_INDEX,N_IMPLICIT_VENTS,I_MODE,&
           N_ORIGINAL_VENTS,IC0,IC1
           
INTEGER :: GI1,GI2,GJ1,GJ2,GK1,GK2           
REAL(EB) :: SPREAD_RATE,TRANSPARENCY,XYZ(3),TMP_EXTERIOR,DYNAMIC_PRESSURE,XB_USER(6),XB_MESH(6), &
            REYNOLDS_STRESS(3,3),L_EDDY,VEL,VEL_RMS,L_EDDY_IJ(3,3),UVW(3),RADIUS
CHARACTER(LABEL_LENGTH) :: ID,DEVC_ID,CTRL_ID,SURF_ID,PRESSURE_RAMP,TMP_EXTERIOR_RAMP,MULT_ID,OBST_ID
CHARACTER(25) :: COLOR
TYPE(MULTIPLIER_TYPE), POINTER :: MR
LOGICAL :: REJECT_VENT,OUTLINE,GEOM,SOLID_FOUND
TYPE IMPLICIT_VENT_TYPE
   REAL(EB) :: XB(6)
   INTEGER, DIMENSION(3) :: RGB=-1
   CHARACTER(LABEL_LENGTH) :: MB='null',SURF_ID='null',ID='null'
END TYPE
TYPE(IMPLICIT_VENT_TYPE), ALLOCATABLE, DIMENSION(:) :: IMPLICIT_VENT
NAMELIST /VENT/ COLOR,CTRL_ID,DB,DEVC_ID,DYNAMIC_PRESSURE,FYI,GEOM,ID,IOR,L_EDDY,L_EDDY_IJ, &
                MB,MULT_ID,N_EDDY,OBST_ID,OUTLINE,PBX,PBY,PBZ,PRESSURE_RAMP,RADIUS,REYNOLDS_STRESS, &
                RGB,SPREAD_RATE,SURF_ID,TEXTURE_ORIGIN,TMP_EXTERIOR,TMP_EXTERIOR_RAMP,TRANSPARENCY, &
                UVW,VEL_RMS,XB,XYZ

! For a given MPI process, only read and process VENTs in the MESHes it controls or the MESH's immediate neighbors

MESH_LOOP_1: DO NM=1,NMESHES

   IF (.NOT.PROCESS_MESH_NEIGHBORHOOD(NM)) CYCLE MESH_LOOP_1

   CALL POINT_TO_MESH(NM)

   ! Special circumstances where VENTs are implied, not explicitly included in input file

   CALL DEFINE_IMPLICIT_VENTS

   ! Read the input file twice, first to count the VENTs, then to store the info in MESHES(NM)%VENTS(N)

   COUNT_OR_READ_LOOP: DO I_MODE=1,2

   ! Allocate the derived type variable VENTS that holds all vent info

   IF (I_MODE==2) THEN
      ALLOCATE(MESHES(NM)%VENTS(N_VENT),STAT=IZERO) ; CALL ChkMemErr('READ','VENTS',IZERO) ; VENTS=>MESHES(NM)%VENTS
      IF (MY_RANK==0 .AND. .NOT.ALLOCATED(ORIGINAL_VENTS)) ALLOCATE(ORIGINAL_VENTS(N_ORIGINAL_VENTS))
   ENDIF

   ! Rewind the input file and read all possible vents

   N_VENT           = 0  ! Number of VENTs stored by each mesh
   N_TOTAL          = 0  ! Counter of all VENTs, both explicit and implicit
   N_EXPLICIT       = 0  ! Counter of explicitly declared VENTs
   N_VENT_TOTAL     = 0  ! Purely for Smokeview drawing of VENTs
   N_ORIGINAL_VENTS = 0  ! Number of specified vents for use with Smokeview and HVAC drawing

   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

   READ_VENT_LOOP: DO

      CALL SET_VENT_DEFAULTS

      N_TOTAL = N_TOTAL + 1

      ! Read the VENT lines that are explicitly listed in the input file, not the implicit MIRROR VENTs

      IF (N_TOTAL<=N_IMPLICIT_VENTS) THEN
         XB(1)      = IMPLICIT_VENT(N_TOTAL)%XB(1)
         XB(2)      = IMPLICIT_VENT(N_TOTAL)%XB(2)
         XB(3)      = IMPLICIT_VENT(N_TOTAL)%XB(3)
         XB(4)      = IMPLICIT_VENT(N_TOTAL)%XB(4)
         XB(5)      = IMPLICIT_VENT(N_TOTAL)%XB(5)
         XB(6)      = IMPLICIT_VENT(N_TOTAL)%XB(6)
         RGB(1)     = IMPLICIT_VENT(N_TOTAL)%RGB(1)
         RGB(2)     = IMPLICIT_VENT(N_TOTAL)%RGB(1)
         RGB(3)     = IMPLICIT_VENT(N_TOTAL)%RGB(1)
         MB         = IMPLICIT_VENT(N_TOTAL)%MB
         SURF_ID    = IMPLICIT_VENT(N_TOTAL)%SURF_ID
         ID         = IMPLICIT_VENT(N_TOTAL)%ID
      ELSE
         CALL CHECKREAD('VENT',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
         IF (IOS==1) EXIT READ_VENT_LOOP
         N_EXPLICIT = N_EXPLICIT + 1
         READ(LU_INPUT,VENT,END=37,ERR=36,IOSTAT=IOS)    ! Read in info for VENT N
      36 IF (IOS>0) THEN
            WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with VENT number ',N_EXPLICIT,', line number ',INPUT_FILE_LINE_NUMBER
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
      ENDIF

      ! Assign VENT an ID if not given by user

      IF (ID=='null') WRITE(ID,'(I0)') N_EXPLICIT

      ! Simple error flagging

      IF (SURF_ID=='HVAC' .AND. MULT_ID/='null') THEN
         WRITE(MESSAGE,'(2A)') 'ERROR(801): VENT ',TRIM(ID),' cannot use MULT_ID because it involves HVAC.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF
      IF (SURF_ID=='HVAC' .AND. ID=='null') THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(802): VENT ',TRIM(ID), ' needs an explicit ID because it involves HVAC.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF

      ! Special cases where VENT is specified with PBX, PBY, PBZ, MB, or DB

      IF (PBX>-1.E5_EB .OR. PBY>-1.E5_EB .OR. PBZ>-1.E5_EB) THEN
         IF (MULT_ID/='null') THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(803): VENT ',TRIM(ID),' cannot use MULT_ID because it uses PBX, PBY or PBZ.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         XB = (/XS,XF,YS,YF,ZS,ZF/)
         IF (PBX>-1.E5_EB) XB(1:2) = PBX
         IF (PBY>-1.E5_EB) XB(3:4) = PBY
         IF (PBZ>-1.E5_EB) XB(5:6) = PBZ
      ELSEIF (MB/='null') THEN
         IF (NMESHES>1 .AND. SURF_ID=='PERIODIC') THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(804): VENT ',TRIM(ID),' should use PBX, PBY, PBZ or XB if it is PERIODIC.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         IF (MULT_ID/='null') THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(805): VENT ',TRIM(ID),' cannot use MULT_ID because it uses MB.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         XB = (/XS,XF,YS,YF,ZS,ZF/)
         SELECT CASE (MB)
            CASE('XMIN') ; XB(2) = XS
            CASE('XMAX') ; XB(1) = XF
            CASE('YMIN') ; XB(4) = YS
            CASE('YMAX') ; XB(3) = YF
            CASE('ZMIN') ; XB(6) = ZS
            CASE('ZMAX') ; XB(5) = ZF
            CASE DEFAULT
               WRITE(MESSAGE,'(3A)') 'ERROR(806): VENT ',TRIM(ID),' must set MB to XMIN, XMAX, YMIN, YMAX, ZMIN, or ZMAX.'
               CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         END SELECT
      ELSEIF (DB/='null') THEN
         IF (MULT_ID/='null') THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(807): VENT ',TRIM(ID),' cannot use MULT_ID because it uses DB.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         XB = (/XS,XF,YS,YF,ZS,ZF/)
         SELECT CASE (DB)
            CASE('XMIN') ; XB(1:2) = XS_MIN+TWO_EPSILON_EB
            CASE('XMAX') ; XB(1:2) = XF_MAX-TWO_EPSILON_EB
            CASE('YMIN') ; XB(3:4) = YS_MIN+TWO_EPSILON_EB
            CASE('YMAX') ; XB(3:4) = YF_MAX-TWO_EPSILON_EB
            CASE('ZMIN') ; XB(5:6) = ZS_MIN+TWO_EPSILON_EB
            CASE('ZMAX') ; XB(5:6) = ZF_MAX-TWO_EPSILON_EB
            CASE DEFAULT
               WRITE(MESSAGE,'(3A)') 'ERROR(808): VENT ',TRIM(ID),' must set DB to XMIN, XMAX, YMIN, YMAX, ZMIN, or ZMAX.'
               CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         END SELECT
      ENDIF

      ! Check that the vent is properly specified

      IF (ABS(XB(3)-XB(4))<=SPACING(XB(4))  .AND. TWO_D .AND. N_TOTAL>N_IMPLICIT_VENTS) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(809): VENT ',TRIM(ID),' cannot be specified on a y boundary in a 2D calc.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF

      IF (ABS(XB(1)-XB(2))>SPACING(XB(2))  .AND. ABS(XB(3)-XB(4))>SPACING(XB(4)).AND.ABS(XB(5)-XB(6))>SPACING(XB(6)) ) THEN
         WRITE(MESSAGE,'(3A)')  'ERROR(810): VENT ',TRIM(ID),' must be a plane.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF

      ! Check if the XB coords are in the proper order

      CALL CHECK_XB(XB)

      ! Loop over all possible multiples of the VENT and save the user-specified coords, XB_USER

      MR => MULTIPLIER(0)
      DO NNN=1,N_MULT
         IF (MULT_ID==MULTIPLIER(NNN)%ID) THEN
            MR => MULTIPLIER(NNN)
            EXIT
         ENDIF
         IF (MULT_ID/='null' .AND. NNN==N_MULT) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(811): VENT ',TRIM(ID),' MULT_ID ',TRIM(MULT_ID),' not found.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
      ENDDO

      K_MULT_LOOP: DO KK=MR%K_LOWER,MR%K_UPPER
         J_MULT_LOOP: DO JJ=MR%J_LOWER,MR%J_UPPER
            I_MULT_LOOP: DO II=MR%I_LOWER,MR%I_UPPER

               IF (MR%SKIP(II,JJ,KK)) CYCLE I_MULT_LOOP

               REJECT_VENT = .FALSE.

               N_VENT_TOTAL = N_VENT_TOTAL + 1  ! Count all possible VENTs for use in Smokeview

               IF (.NOT.MR%SEQUENTIAL) THEN
                  XB_USER(1) = XB(1) + MR%DX0 + II*MR%DXB(1)
                  XB_USER(2) = XB(2) + MR%DX0 + II*MR%DXB(2)
                  XB_USER(3) = XB(3) + MR%DY0 + JJ*MR%DXB(3)
                  XB_USER(4) = XB(4) + MR%DY0 + JJ*MR%DXB(4)
                  XB_USER(5) = XB(5) + MR%DZ0 + KK*MR%DXB(5)
                  XB_USER(6) = XB(6) + MR%DZ0 + KK*MR%DXB(6)
               ELSE
                  XB_USER(1) = XB(1) + MR%DX0 + II*MR%DXB(1)
                  XB_USER(2) = XB(2) + MR%DX0 + II*MR%DXB(2)
                  XB_USER(3) = XB(3) + MR%DY0 + II*MR%DXB(3)
                  XB_USER(4) = XB(4) + MR%DY0 + II*MR%DXB(4)
                  XB_USER(5) = XB(5) + MR%DZ0 + II*MR%DXB(5)
                  XB_USER(6) = XB(6) + MR%DZ0 + II*MR%DXB(6)
               ENDIF

               ! Save the original VENT coordinates in a special array on MPI process 0

               N_ORIGINAL_VENTS = N_ORIGINAL_VENTS + 1

               IF (NM==1 .AND. MY_RANK==0 .AND. I_MODE==2) THEN
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%ID = ID
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%X1 = XB_USER(1)
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%X2 = XB_USER(2)
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%Y1 = XB_USER(3)
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%Y2 = XB_USER(4)
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%Z1 = XB_USER(5)
                  ORIGINAL_VENTS(N_ORIGINAL_VENTS)%Z2 = XB_USER(6)
               ENDIF

               ! Save the VENT coordinates for the given MESH

               XB_MESH(1) = MAX(XB_USER(1),XS)
               XB_MESH(2) = MIN(XB_USER(2),XF)
               XB_MESH(3) = MAX(XB_USER(3),YS)
               XB_MESH(4) = MIN(XB_USER(4),YF)
               XB_MESH(5) = MAX(XB_USER(5),ZS)
               XB_MESH(6) = MIN(XB_USER(6),ZF)

               I1 = MAX(0,   NINT(GINV(XB_MESH(1)-XS,1,NM)*RDXI   ))
               I2 = MIN(IBAR,NINT(GINV(XB_MESH(2)-XS,1,NM)*RDXI   ))
               J1 = MAX(0,   NINT(GINV(XB_MESH(3)-YS,2,NM)*RDETA  ))
               J2 = MIN(JBAR,NINT(GINV(XB_MESH(4)-YS,2,NM)*RDETA  ))
               K1 = MAX(0,   NINT(GINV(XB_MESH(5)-ZS,3,NM)*RDZETA ))
               K2 = MIN(KBAR,NINT(GINV(XB_MESH(6)-ZS,3,NM)*RDZETA ))

               ! Decide if the VENT is inside or at the boundary of the current MESH.
               ! The factor of 10 is to ensure that a vent that is fairly close to the boundary is counted.

               IF ((XB_MESH(1)-XF)>10._EB*SPACING(XF) .OR. (XS-XB_MESH(2))>10._EB*SPACING(XS) .OR. &
                   (XB_MESH(3)-YF)>10._EB*SPACING(YF) .OR. (YS-XB_MESH(4))>10._EB*SPACING(YS) .OR. &
                   (XB_MESH(5)-ZF)>10._EB*SPACING(ZF) .OR. (ZS-XB_MESH(6))>10._EB*SPACING(ZS)) REJECT_VENT = .TRUE.

               IF (ABS(XB_MESH(1)-XB_MESH(2))<=SPACING(XB_MESH(2))) THEN
                  IF (J1==J2  .OR. K1==K2) REJECT_VENT=.TRUE.
                  IF (I1>IBAR .OR. I2<0)   REJECT_VENT=.TRUE.
               ENDIF
               IF (ABS(XB_MESH(3)-XB_MESH(4))<=SPACING(XB_MESH(4))) THEN
                  IF (I1==I2  .OR. K1==K2) REJECT_VENT=.TRUE.
                  IF (J1>JBAR .OR. J2<0)   REJECT_VENT=.TRUE.
               ENDIF
               IF (ABS(XB_MESH(5)-XB_MESH(6))<=SPACING(XB_MESH(6))) THEN
                  IF (I1==I2  .OR. J1==J2) REJECT_VENT=.TRUE.
                  IF (K1>KBAR .OR. K2<0)   REJECT_VENT=.TRUE.
               ENDIF

               ! If the VENT is rejected, cycle

               IF (REJECT_VENT) THEN
                  CYCLE I_MULT_LOOP
               ELSE
                  N_VENT = N_VENT + 1
                  IF (I_MODE==1) CYCLE I_MULT_LOOP
               ENDIF

               ! The VENT is accepted, add an entry to MESHES(NM)%VENTS

               VT=>VENTS(N_VENT)

               ! Set basic VENT coordinates

               VT%I1 = I1
               VT%I2 = I2
               VT%J1 = J1
               VT%J2 = J2
               VT%K1 = K1
               VT%K2 = K2

               VT%X1 = XB_MESH(1)
               VT%X2 = XB_MESH(2)
               VT%Y1 = XB_MESH(3)
               VT%Y2 = XB_MESH(4)
               VT%Z1 = XB_MESH(5)
               VT%Z2 = XB_MESH(6)

               VT%X1_ORIG = XB_USER(1)
               VT%X2_ORIG = XB_USER(2)
               VT%Y1_ORIG = XB_USER(3)
               VT%Y2_ORIG = XB_USER(4)
               VT%Z1_ORIG = XB_USER(5)
               VT%Z2_ORIG = XB_USER(6)

#if defined global_mesh
               VT%GI1 = I1 + MESHES(NM)%GI1
               VT%GI2 = I2 + MESHES(NM)%GI1
               VT%GJ1 = J1 + MESHES(NM)%GJ1
               VT%GJ2 = J2 + MESHES(NM)%GJ1
               VT%GK1 = K1 + MESHES(NM)%GK1
               VT%GK2 = K2 + MESHES(NM)%GK1
#endif

               ! Vent area

               IF (ABS(XB_USER(1)-XB_USER(2))<=SPACING(XB_USER(2))) &
                  VT%UNDIVIDED_INPUT_AREA = (XB_USER(4)-XB_USER(3))*(XB_USER(6)-XB_USER(5))
               IF (ABS(XB_USER(3)-XB_USER(4))<=SPACING(XB_USER(4))) &
                  VT%UNDIVIDED_INPUT_AREA = (XB_USER(2)-XB_USER(1))*(XB_USER(6)-XB_USER(5))
               IF (ABS(XB_USER(5)-XB_USER(6))<=SPACING(XB_USER(6))) &
                  VT%UNDIVIDED_INPUT_AREA = (XB_USER(2)-XB_USER(1))*(XB_USER(4)-XB_USER(3))

               IF (RADIUS>0._EB) VT%UNDIVIDED_INPUT_AREA = PI*RADIUS**2

               IF (ABS(VT%X2-VT%X1)<=SPACING(VT%X2) ) VT%INPUT_AREA = (VT%Y2-VT%Y1)*(VT%Z2-VT%Z1)
               IF (ABS(VT%Y2-VT%Y1)<=SPACING(VT%Y2) ) VT%INPUT_AREA = (VT%X2-VT%X1)*(VT%Z2-VT%Z1)
               IF (ABS(VT%Z2-VT%Z1)<=SPACING(VT%Z2) ) VT%INPUT_AREA = (VT%X2-VT%X1)*(VT%Y2-VT%Y1)

               ! Check the SURF_ID against the list of SURF's

               CALL CHECK_SURF_NAME(SURF_ID,EX)
               IF (.NOT.EX) THEN
                  WRITE(MESSAGE,'(5A)') 'ERROR(812): VENT ',TRIM(ID),' SURF_ID ',TRIM(SURF_ID),' not found.'
                  CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
               ENDIF

               ! Assign SURF_INDEX, Index of the Boundary Condition

               VT%SURF_INDEX = DEFAULT_SURF_INDEX
               DO NNN=0,N_SURF
                  IF (SURF_ID==SURFACE(NNN)%ID) VT%SURF_INDEX = NNN
               ENDDO

               IF (SURF_ID=='OPEN')                            VT%TYPE_INDICATOR =  2
               IF (SURF_ID=='MIRROR' .OR. SURF_ID=='PERIODIC') VT%TYPE_INDICATOR = -2
               IF ((MB/='null' .OR.  PBX>-1.E5_EB .OR. PBY>-1.E5_EB .OR. PBZ>-1.E5_EB) .AND. SURF_ID=='OPEN') VT%TYPE_INDICATOR=-2
               IF (SURF_ID=='PERIODIC FLOW ONLY') VT%SURF_INDEX = PERIODIC_FLOW_ONLY_SURF_INDEX

               VT%BOUNDARY_TYPE = SOLID_BOUNDARY
               IF (VT%SURF_INDEX==OPEN_SURF_INDEX)               VT%BOUNDARY_TYPE = OPEN_BOUNDARY
               IF (VT%SURF_INDEX==MIRROR_SURF_INDEX)             VT%BOUNDARY_TYPE = MIRROR_BOUNDARY
               IF (VT%SURF_INDEX==PERIODIC_SURF_INDEX)           VT%BOUNDARY_TYPE = PERIODIC_BOUNDARY
               IF (VT%SURF_INDEX==PERIODIC_FLOW_ONLY_SURF_INDEX) VT%BOUNDARY_TYPE = PERIODIC_BOUNDARY
               IF (VT%SURF_INDEX==HVAC_SURF_INDEX)               VT%BOUNDARY_TYPE = HVAC_BOUNDARY

               VT%IOR = IOR
               VT%ORDINAL = N_EXPLICIT

               ! Activate and Deactivate logic

               VT%ACTIVATED = .TRUE.
               VT%DEVC_ID   = DEVC_ID
               VT%CTRL_ID   = CTRL_ID
               VT%ID        = ID
               CALL SEARCH_CONTROLLER('VENT',CTRL_ID,DEVC_ID,VT%DEVC_INDEX,VT%CTRL_INDEX,N_VENT)
               IF (DEVC_ID /= 'null') THEN
                  IF (.NOT.DEVICE(VT%DEVC_INDEX)%INITIAL_STATE) VT%ACTIVATED = .FALSE.
               ENDIF
               IF (CTRL_ID /= 'null') THEN
                  IF (.NOT.CONTROL(VT%CTRL_INDEX)%INITIAL_STATE) VT%ACTIVATED = .FALSE.
               ENDIF

               IF ( (VT%BOUNDARY_TYPE==OPEN_BOUNDARY .OR. VT%BOUNDARY_TYPE==MIRROR_BOUNDARY .OR. &
                     VT%BOUNDARY_TYPE==PERIODIC_BOUNDARY) .AND. &
                     (VT%DEVC_ID /= 'null' .OR. VT%CTRL_ID /= 'null') ) THEN
                  WRITE(MESSAGE,'(3A)')  'ERROR(813): VENT ',TRIM(ID),' cannot be controlled by a device.'
                  CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
               ENDIF

               ! Set the VENT color index

               SELECT CASE(COLOR)
                  CASE('INVISIBLE')
                     VT%COLOR_INDICATOR = 8
                     TRANSPARENCY = 0._EB
                  CASE('null')
                     VT%COLOR_INDICATOR = 99
                  CASE DEFAULT
                     VT%COLOR_INDICATOR = 99
                     CALL COLOR2RGB(RGB,COLOR)
               END SELECT
               IF (VT%COLOR_INDICATOR==8) VT%TYPE_INDICATOR = -2
               IF (OUTLINE)               VT%TYPE_INDICATOR =  2
               VT%RGB = RGB
               VT%TRANSPARENCY = TRANSPARENCY

               ! Parameters for specified spread of a fire over a VENT

               IF (ALL(XYZ<-1.E5_EB) .AND. SPREAD_RATE>0._EB) THEN
                  XYZ(1)=0.5_EB*(VT%X1+VT%X2)
                  XYZ(2)=0.5_EB*(VT%Y1+VT%Y2)
                  XYZ(3)=0.5_EB*(VT%Z1+VT%Z2)
               ENDIF
               VT%X0 = XYZ(1)
               VT%Y0 = XYZ(2)
               VT%Z0 = XYZ(3)
               VT%FIRE_SPREAD_RATE = SPREAD_RATE / TIME_SHRINK_FACTOR

               ! Circular VENT

               IF (RADIUS>0._EB) THEN
                  IF (ANY(XYZ<-1.E5_EB)) THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(814): VENT ',TRIM(ID),' requires center point XYZ.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
                  VT%RADIUS = RADIUS
               ENDIF

               ! Dynamic Pressure

               VT%DYNAMIC_PRESSURE = DYNAMIC_PRESSURE
               IF (PRESSURE_RAMP/='null') CALL GET_RAMP_INDEX(PRESSURE_RAMP,'TIME',VT%PRESSURE_RAMP_INDEX)

               ! Synthetic Eddy Method

               VT%N_EDDY = N_EDDY
               IF (L_EDDY>TWO_EPSILON_EB) THEN
                  VT%SIGMA_IJ = L_EDDY
               ELSE
                  VT%SIGMA_IJ = L_EDDY_IJ ! Modified SEM (Jarrin, Ch. 7)
                  VT%SIGMA_IJ = MAX(VT%SIGMA_IJ,1.E-10_EB)
               ENDIF
               IF (VEL_RMS>0._EB) THEN
                  VT%R_IJ=0._EB
                  VT%R_IJ(1,1)=VEL_RMS**2
                  VT%R_IJ(2,2)=VEL_RMS**2
                  VT%R_IJ(3,3)=VEL_RMS**2
               ELSE
                  VT%R_IJ = REYNOLDS_STRESS
                  VT%R_IJ = MAX(VT%R_IJ,1.E-10_EB)
               ENDIF

               ! Check SEM parameters

               IF (N_EDDY>0) THEN
                  SYNTHETIC_EDDY_METHOD = .TRUE.
                  IF (ANY(VT%SIGMA_IJ<TWO_EPSILON_EB)) THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(815): VENT ',TRIM(ID),' L_EDDY = 0 in Synthetic Eddy Method.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
                  IF (ALL(ABS(VT%R_IJ)<TWO_EPSILON_EB)) THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(816): VENT ',TRIM(ID),' VEL_RMS = 0 in Synthetic Eddy Method.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
                  IF (TRIM(SURF_ID)=='HVAC') THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(817): VENT ',TRIM(ID),' Synthetic Eddy Method not permitted with HVAC.'
                     CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
                  ENDIF
               ENDIF

#if defined atm_variables
               IF (N_EDDY<0) THEN
                COUPLED_ATM_BOUNDARY = .TRUE.              
               ENDIF
#endif         

#if defined vent_debug
							 print*, 'reading coupled vents in mesh', NM, 'N_VENT ', N_VENT,  VT%I1,  VT%I2, VT%GI1, VT%GI2
							 !VT%J1,  VT%J2,  VT%K1,  VT%K2    
#endif      


               ! Check if the VENT is attached to a specific OBST

               IF (OBST_ID/='null') THEN
                  DO OBST_INDEX=1,N_OBST
                     IF (OBST_ID==OBSTRUCTION(OBST_INDEX)%ID) VT%OBST_INDEX = OBST_INDEX
                  ENDDO
               ENDIF

               ! Miscellaneous

               VT%TMP_EXTERIOR = TMP_EXTERIOR + TMPM
               IF (VT%TMP_EXTERIOR>0._EB) TMPMIN = MIN(TMPMIN,VT%TMP_EXTERIOR)
               IF (TMP_EXTERIOR_RAMP/='null') CALL GET_RAMP_INDEX(TMP_EXTERIOR_RAMP,'TIME',VT%TMP_EXTERIOR_RAMP_INDEX)

               VT%TEXTURE(:) = TEXTURE_ORIGIN(:)

               VT%UVW = UVW
               IF (ALL(VT%UVW > -1.E12_EB)) THEN
                  VT%UVW = VT%UVW/SQRT(VT%UVW(1)**2+VT%UVW(2)**2+VT%UVW(3)**2)
               ENDIF

               VT%GEOM = GEOM

            ENDDO I_MULT_LOOP
         ENDDO J_MULT_LOOP
      ENDDO K_MULT_LOOP

   ENDDO READ_VENT_LOOP

   ENDDO COUNT_OR_READ_LOOP

37 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ENDDO MESH_LOOP_1

! Go through all the meshes again, but this time only if PROCESS(NM)==MY_RANK

MESH_LOOP_2: DO NM=1,NMESHES

   IF (PROCESS(NM)/=MY_RANK) CYCLE MESH_LOOP_2

   CALL POINT_TO_MESH(NM)

   ! Check vents and assign orientations

   VENT_LOOP_2: DO N=1,N_VENT

      VT => VENTS(N)

      I1 = MAX(0,VT%I1)
      I2 = MIN(IBAR,VT%I2)
      J1 = MAX(0,VT%J1)
      J2 = MIN(JBAR,VT%J2)
      K1 = MAX(0,VT%K1)
      K2 = MIN(KBAR,VT%K2)

      IF (VT%IOR==0) THEN
         IF (I1==      0 .AND. I2==0) VT%IOR =  1
         IF (I1==IBAR .AND. I2==IBAR) VT%IOR = -1
         IF (J1==      0 .AND. J2==0) VT%IOR =  2
         IF (J1==JBAR .AND. J2==JBAR) VT%IOR = -2
         IF (K1==      0 .AND. K2==0) VT%IOR =  3
         IF (K1==KBAR .AND. K2==KBAR) VT%IOR = -3
      ENDIF

      ! Assign orientation and determine if the VENT has a solid backing

      SOLID_FOUND = .FALSE.

      IF (VT%IOR/=0) SOLID_FOUND = .TRUE.

      IF (I1==I2) THEN
         DO K=K1+1,K2
            DO J=J1+1,J2
               IC0 = CELL_INDEX(I2  ,J,K)
               IC1 = CELL_INDEX(I2+1,J,K)
               IF (VT%IOR==0 .AND. .NOT.CELL(IC1)%SOLID) VT%IOR =  1
               IF (VT%IOR==0 .AND. .NOT.CELL(IC0)%SOLID) VT%IOR = -1
               IF (.NOT.CELL(IC1)%EXTERIOR .AND. CELL(IC1)%SOLID) SOLID_FOUND = .TRUE.
               IF (.NOT.CELL(IC0)%EXTERIOR .AND. CELL(IC0)%SOLID) SOLID_FOUND = .TRUE.
            ENDDO
         ENDDO
      ENDIF
      IF (J1==J2) THEN
         DO K=K1+1,K2
            DO I=I1+1,I2
               IC0 = CELL_INDEX(I,J2  ,K)
               IC1 = CELL_INDEX(I,J2+1,K)
               IF (VT%IOR==0 .AND. .NOT.CELL(IC1)%SOLID) VT%IOR =  2
               IF (VT%IOR==0 .AND. .NOT.CELL(IC0)%SOLID) VT%IOR = -2
               IF (.NOT.CELL(IC1)%EXTERIOR .AND. CELL(IC1)%SOLID) SOLID_FOUND = .TRUE.
               IF (.NOT.CELL(IC0)%EXTERIOR .AND. CELL(IC0)%SOLID) SOLID_FOUND = .TRUE.
            ENDDO
         ENDDO
      ENDIF
      IF (K1==K2) THEN
         DO J=J1+1,J2
            DO I=I1+1,I2
               IC0 = CELL_INDEX(I,J,K2)
               IC1 = CELL_INDEX(I,J,K2+1)
               IF (VT%IOR==0 .AND. .NOT.CELL(IC1)%SOLID) VT%IOR =  3
               IF (VT%IOR==0 .AND. .NOT.CELL(IC0)%SOLID) VT%IOR = -3
               IF (.NOT.CELL(IC1)%EXTERIOR .AND. CELL(IC1)%SOLID) SOLID_FOUND = .TRUE.
               IF (.NOT.CELL(IC0)%EXTERIOR .AND. CELL(IC0)%SOLID) SOLID_FOUND = .TRUE.
            ENDDO
         ENDDO
      ENDIF

      IF (VT%IOR==0) THEN
         WRITE(MESSAGE,'(3A)')  'ERROR(818): VENT ',TRIM(VT%ID),' requires an orientation index, IOR.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF

      ! Assign global periodicity

      IF (ABS(VT%IOR)==1 .AND. VT%SURF_INDEX==PERIODIC_SURF_INDEX) PERIODIC_DOMAIN_X = .TRUE.
      IF (ABS(VT%IOR)==2 .AND. VT%SURF_INDEX==PERIODIC_SURF_INDEX) PERIODIC_DOMAIN_Y = .TRUE.
      IF (ABS(VT%IOR)==3 .AND. VT%SURF_INDEX==PERIODIC_SURF_INDEX) PERIODIC_DOMAIN_Z = .TRUE.

      ! If the VENT is in the interior of the mesh, check for certain things

      IF ((ABS(VT%IOR)==1 .AND. I1>=1 .AND. I1<=IBM1) .OR. &
          (ABS(VT%IOR)==2 .AND. J1>=1 .AND. J1<=JBM1) .OR. &
          (ABS(VT%IOR)==3 .AND. K1>=1 .AND. K1<=KBM1)) THEN
         IF (VT%BOUNDARY_TYPE==OPEN_BOUNDARY .OR. VT%BOUNDARY_TYPE==MIRROR_BOUNDARY .OR. VT%BOUNDARY_TYPE==PERIODIC_BOUNDARY) THEN
            WRITE(MESSAGE,'(3A)')  'ERROR(819): VENT ',TRIM(ID),' is OPEN, MIRROR OR PERIODIC and must be on an exterior boundary.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         IF (.NOT.SOLID_FOUND) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(820): VENT ',TRIM(VT%ID),' has no solid backing or an orientation index (IOR) is needed.'
            CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
         ENDIF
         IF (VT%BOUNDARY_TYPE/=HVAC_BOUNDARY) VT%BOUNDARY_TYPE = SOLID_BOUNDARY
      ENDIF

      ! Open up boundary cells if it is an open vent

      IF (VT%BOUNDARY_TYPE==OPEN_BOUNDARY) THEN
         SELECT CASE(VT%IOR)
            CASE( 1)
               CALL BLOCK_CELL(NM,   0,   0,J1+1,  J2,K1+1,  K2,0,0)
            CASE(-1)
               CALL BLOCK_CELL(NM,IBP1,IBP1,J1+1,  J2,K1+1,  K2,0,0)
            CASE( 2)
               CALL BLOCK_CELL(NM,I1+1,  I2,   0,   0,K1+1,  K2,0,0)
            CASE(-2)
               CALL BLOCK_CELL(NM,I1+1,  I2,JBP1,JBP1,K1+1,  K2,0,0)
            CASE( 3)
               CALL BLOCK_CELL(NM,I1+1,  I2,J1+1,  J2,   0,   0,0,0)
            CASE(-3)
               CALL BLOCK_CELL(NM,I1+1,  I2,J1+1,  J2,KBP1,KBP1,0,0)
         END SELECT
      ENDIF

      ! Check UVW

      IF (ABS(VT%UVW(ABS(VT%IOR))) < TWO_EPSILON_EB) THEN
         WRITE(MESSAGE,'(3A)')  'ERROR(821): VENT ',TRIM(VT%ID),' cannot have normal component of UVW equal to 0.'
         CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
      ENDIF

   ENDDO VENT_LOOP_2

   ! Compute vent areas and check for passive openings

   VENT_LOOP_3: DO N=1,N_VENT

      VT => VENTS(N)

      IF (VT%SURF_INDEX==HVAC_SURF_INDEX .AND. N>1) THEN
         DO NNN=1,N-1
            IF (TRIM(VT%ID)==TRIM(VENTS(NNN)%ID) .AND. VENTS(NNN)%SURF_INDEX==HVAC_SURF_INDEX) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR(822): VENT ',TRIM(VT%ID),' has the same ID as another VENT.'
               CALL SHUTDOWN(MESSAGE,PROCESS_0_ONLY=.FALSE.) ; RETURN
            ENDIF
         ENDDO
      ENDIF

      VT%FDS_AREA = 0._EB
      IF (VT%RADIUS>0._EB) VT%INPUT_AREA = 0._EB

      I1 = VT%I1
      I2 = VT%I2
      J1 = VT%J1
      J2 = VT%J2
      K1 = VT%K1
      K2 = VT%K2

      VT%GHOST_CELLS_ONLY = .TRUE.

      SELECT CASE(ABS(VT%IOR))
         CASE(1)
            DO K=K1+1,K2
               DO J=J1+1,J2
                  IF (J>=1 .AND. J<=JBAR .AND. K>=1 .AND. K<=KBAR) VT%GHOST_CELLS_ONLY = .FALSE.
                  IF ( VT%RADIUS>0._EB) THEN
                     VT%INPUT_AREA = VT%INPUT_AREA + CIRCLE_CELL_INTERSECTION_AREA(VT%Y0,VT%Z0,VT%RADIUS,Y(J-1),Y(J),Z(K-1),Z(K))
                     IF (((YC(J)-VT%Y0)**2+(ZC(K)-VT%Z0)**2)>VT%RADIUS**2) CYCLE
                  ENDIF
                  VT%FDS_AREA = VT%FDS_AREA + DY(J)*DZ(K)
               ENDDO
            ENDDO
         CASE(2)
            DO K=K1+1,K2
               DO I=I1+1,I2
                  IF (I>=1 .AND. I<=IBAR .AND. K>=1 .AND. K<=KBAR) VT%GHOST_CELLS_ONLY = .FALSE.
                  IF ( VT%RADIUS>0._EB) THEN
                     VT%INPUT_AREA = VT%INPUT_AREA + CIRCLE_CELL_INTERSECTION_AREA(VT%X0,VT%Z0,VT%RADIUS,X(I-1),X(I),Z(K-1),Z(K))
                     IF (((XC(I)-VT%X0)**2+(ZC(K)-VT%Z0)**2)>VT%RADIUS**2) CYCLE
                  ENDIF
                  VT%FDS_AREA = VT%FDS_AREA + DX(I)*DZ(K)
               ENDDO
            ENDDO
         CASE(3)
            DO J=J1+1,J2
               DO I=I1+1,I2
                  IF (I>=1 .AND. I<=IBAR .AND. J>=1 .AND. J<=JBAR) VT%GHOST_CELLS_ONLY = .FALSE.
                  IF ( VT%RADIUS>0._EB) THEN
                     VT%INPUT_AREA = VT%INPUT_AREA + CIRCLE_CELL_INTERSECTION_AREA(VT%X0,VT%Y0,VT%RADIUS,X(I-1),X(I),Y(J-1),Y(J))
                     IF (((XC(I)-VT%X0)**2+(YC(J)-VT%Y0)**2)>VT%RADIUS**2) CYCLE
                  ENDIF
                  VT%FDS_AREA = VT%FDS_AREA + DX(I)*DY(J)
               ENDDO
            ENDDO 
      END SELECT

   ENDDO  VENT_LOOP_3


!Print*,'Mesh',NM, 'Vents setup finished',N_vent,n_total,n_explicit, N_vent_total
ENDDO MESH_LOOP_2




CONTAINS


SUBROUTINE SET_VENT_DEFAULTS

COLOR             = 'null'
CTRL_ID           = 'null'
DB                = 'null'
DEVC_ID           = 'null'
DYNAMIC_PRESSURE  = 0._EB
GEOM              = .FALSE.
ID                = 'null'
IOR               = 0
L_EDDY            = 0._EB
L_EDDY_IJ         = 0._EB
MB                = 'null'
MULT_ID           = 'null'
N_EDDY            = 0
OBST_ID           = 'null'
OUTLINE           = .FALSE.
PBX               = -1.E6_EB
PBY               = -1.E6_EB
PBZ               = -1.E6_EB
PRESSURE_RAMP     = 'null'
RADIUS            = -1._EB
REYNOLDS_STRESS   = 0._EB
RGB               = -1
SPREAD_RATE       = -1._EB
SURF_ID           = 'null'
TEXTURE_ORIGIN    = -999._EB
TMP_EXTERIOR      = -1000.
TMP_EXTERIOR_RAMP = 'null'
TRANSPARENCY      = 1._EB
UVW               = -1.E12_EB
VEL               = -1.E12_EB ! experimental (rjm)
VEL_RMS           = 0._EB
XYZ               = -1.E6_EB

END SUBROUTINE SET_VENT_DEFAULTS


!> \brief Define VENTs that the user has not explicitly defined

SUBROUTINE DEFINE_IMPLICIT_VENTS

IF (.NOT.ALLOCATED(IMPLICIT_VENT)) ALLOCATE(IMPLICIT_VENT(3))

N_IMPLICIT_VENTS = 0

! For a 2-D simulation, add MIRROR VENTs to lower and upper y boundary

IF (TWO_D) THEN
   IMPLICIT_VENT(N_IMPLICIT_VENTS+1)%MB      = 'YMIN'
   IMPLICIT_VENT(N_IMPLICIT_VENTS+2)%MB      = 'YMAX'
   IMPLICIT_VENT(N_IMPLICIT_VENTS+1)%SURF_ID = 'MIRROR'
   IMPLICIT_VENT(N_IMPLICIT_VENTS+2)%SURF_ID = 'MIRROR'
   N_IMPLICIT_VENTS = N_IMPLICIT_VENTS + 2
ENDIF

! For a cylindrical geometry where r_min=0, set a MIRROR BC

IF (CYLINDRICAL .AND. XS<=TWO_EPSILON_EB) THEN
   IMPLICIT_VENT(N_IMPLICIT_VENTS+1)%MB      = 'XMIN'
   IMPLICIT_VENT(N_IMPLICIT_VENTS+1)%SURF_ID = 'MIRROR'
   N_IMPLICIT_VENTS = N_IMPLICIT_VENTS + 1
ENDIF

END SUBROUTINE DEFINE_IMPLICIT_VENTS

END SUBROUTINE READ_VENT


!> \brief Read the INIT namelist lines

SUBROUTINE READ_INIT

USE PHYSICAL_FUNCTIONS, ONLY: GET_SPECIFIC_GAS_CONSTANT
USE COMP_FUNCTIONS, ONLY: GET_FILE_NUMBER
USE MATH_FUNCTIONS, ONLY: GET_RAMP_INDEX
USE GEOMETRY_FUNCTIONS, ONLY: BLOCK_MESH_INTERSECTION_VOLUME
USE DEVICE_VARIABLES, ONLY: DEVICE_TYPE,DEVICE,N_DEVC
REAL(EB) :: DIAMETER,TEMPERATURE,DENSITY,RR_SUM,ZZ_GET(1:N_TRACKED_SPECIES),MASS_PER_VOLUME,BULK_DENSITY_FACTOR, &
            MASS_PER_TIME,DT_INSERT,UVW(3),HRRPUV,XYZ(3),DX,DY,DZ,HEIGHT,RADIUS,INNER_RADIUS,MASS_FRACTION(MAX_SPECIES), &
            PARTICLE_WEIGHT_FACTOR,VOLUME_FRACTION(MAX_SPECIES),CROWN_BASE_HEIGHT,CROWN_BASE_WIDTH,TREE_HEIGHT
INTEGER  :: NM,N,NN,NNN,II,JJ,KK,NS,NS2,NS3,N_PARTICLES,N_INIT_NEW,N_INIT_READ,N_PARTICLES_PER_CELL,SURF_INDEX,III
LOGICAL  :: CELL_CENTERED,UNIFORM,DRY
CHARACTER(LABEL_LENGTH) :: ID,CTRL_ID,DEVC_ID,PART_ID,SHAPE,MULT_ID,SPEC_ID(1:MAX_SPECIES),ORIENTATION_RAMP(3),&
                           PATH_RAMP(3),RAMP_Q,RAMP_PART,NODE_ID
CHARACTER(MESSAGE_LENGTH) :: BULK_DENSITY_FILE
TYPE(INITIALIZATION_TYPE), POINTER :: IN=>NULL()
TYPE(MULTIPLIER_TYPE), POINTER :: MR=>NULL()
TYPE(LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()
TYPE(DEVICE_TYPE), POINTER :: DV,DV2
NAMELIST /INIT/ BULK_DENSITY_FACTOR,BULK_DENSITY_FILE,CELL_CENTERED,CROWN_BASE_HEIGHT,CROWN_BASE_WIDTH,CTRL_ID,DB,DENSITY,DEVC_ID,&
                DIAMETER,DRY,DT_INSERT,DX,DY,DZ,HEIGHT,HRRPUV,ID,INNER_RADIUS,MASS_FRACTION,MASS_PER_TIME,MASS_PER_VOLUME,MULT_ID,&
                NODE_ID,N_PARTICLES,N_PARTICLES_PER_CELL,ORIENTATION_RAMP,PART_ID,PARTICLE_WEIGHT_FACTOR,PATH_RAMP,&
                RADIATIVE_FRACTION,RADIUS,RAMP_PART,RAMP_Q,SHAPE,SPEC_ID,TEMPERATURE,TREE_HEIGHT,UNIFORM,UVW,VOLUME_FRACTION,XB,&
                XYZ
N_INIT = 0
N_INIT_READ = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

COUNT_LOOP: DO
   CALL CHECKREAD('INIT',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_LOOP
   MULT_ID = 'null'
   N_INIT_READ = N_INIT_READ + 1
   READ(LU_INPUT,NML=INIT,END=11,ERR=12,IOSTAT=IOS)
   12 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with INIT number ',N_INIT_READ,', line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   N_INIT_NEW = 0
   IF (MULT_ID=='null') THEN
      N_INIT_NEW = 1
   ELSE
      DO N=1,N_MULT
         MR => MULTIPLIER(N)
         IF (MULT_ID==MR%ID) N_INIT_NEW = MR%N_COPIES
      ENDDO
      IF (N_INIT_NEW==0) THEN
         WRITE(MESSAGE,'(A,I0,2A)') 'ERROR(841): INIT number ',N_INIT_READ,' has an unknown MULT_ID ',TRIM(MULT_ID)
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
   N_INIT = N_INIT + N_INIT_NEW
ENDDO COUNT_LOOP
11 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Add reserved INIT lines

N_INIT = N_INIT + N_INIT_RESERVED

! If there are no INIT lines, return

IF (N_INIT==0) RETURN

ALLOCATE(INITIALIZATION(N_INIT),STAT=IZERO)
CALL ChkMemErr('READ','INITIALIZATION',IZERO)

DO NN=1,N_INIT
   ALLOCATE(INITIALIZATION(NN)%MASS_FRACTION(N_TRACKED_SPECIES),STAT=IZERO)
   CALL ChkMemErr('READ','INITIALIZATION',IZERO)
   INITIALIZATION(NN)%MASS_FRACTION=0._EB
ENDDO

NN = 0

INIT_LOOP: DO N=1,N_INIT_READ+N_INIT_RESERVED

   IF (N<=N_INIT_READ) THEN

      ! Read in the INIT lines

      CALL CHECKREAD('INIT',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT INIT_LOOP
      CALL SET_INIT_DEFAULTS
      READ(LU_INPUT,INIT)
      IF (ID=='null') WRITE(ID,'(I0)') N
      IF (ANY(MASS_FRACTION>=0._EB) .AND. ANY(VOLUME_FRACTION>=0._EB)) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(842): INIT ',TRIM(ID),' cannot have both MASS_FRACTION and VOLUME_FRACTION.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

   ELSE

      ! Use information from DEVC line to create an INIT line for 'RADIATIVE HEAT FLUX GAS' or 'ADIABATIC SURFACE TEMPERATURE GAS'

      CALL SET_INIT_DEFAULTS
      DV => DEVICE(INIT_RESERVED(N-N_INIT_READ)%DEVC_INDEX)  ! First device in the line of POINTS
      WRITE(PART_ID,'(A)') 'RESERVED TARGET PARTICLE'
      ID = DV%ID
      XYZ(1) = DV%X
      XYZ(2) = DV%Y
      XYZ(3) = DV%Z
      IF (DV%LINE==0) THEN
         N_PARTICLES = 1
      ELSE
         N_PARTICLES = INIT_RESERVED(N-N_INIT_READ)%N_PARTICLES
         DV2 => DEVICE(INIT_RESERVED(N-N_INIT_READ)%DEVC_INDEX+N_PARTICLES-1)  ! Last device in the line of POINTS
         DX = (DV2%X-DV%X)/REAL(N_PARTICLES-1,EB)
         DY = (DV2%Y-DV%Y)/REAL(N_PARTICLES-1,EB)
         DZ = (DV2%Z-DV%Z)/REAL(N_PARTICLES-1,EB)
      ENDIF
   ENDIF

   ! Check if domain information is provided for particle INIT

   IF (PART_ID/='null') THEN
      IF (ALL(ABS(XB)>1.E5_EB) .AND. ALL(XYZ<-1.E5_EB) .AND. DB=='null' .AND. BULK_DENSITY_FILE=='null' .AND. &
         (TRIM(PATH_RAMP(1))=='null' .OR. TRIM(PATH_RAMP(2))=='null' .OR. TRIM(PATH_RAMP(3))=='null')) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(843): INIT ',TRIM(ID),' requires PART_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF

   ! Check if domain boundary has been set as the INIT volume

   IF (DB/='null') THEN
      ! any string will work, but suggest DB='WHOLE DOMAIN'
      XB(1) = XS_MIN
      XB(2) = XF_MAX
      XB(3) = YS_MIN
      XB(4) = YF_MAX
      XB(5) = ZS_MIN
      XB(6) = ZF_MAX
   ENDIF

   ! Transform XYZ into XB if necessary, and move XYZ points off of mesh boundaries.

   IF (ANY(XYZ>-100000._EB)) THEN

      MESH_LOOP: DO NM=1,NMESHES
         M=>MESHES(NM)
         IF (XYZ(1)>=M%XS .AND. XYZ(1)<=M%XF .AND. XYZ(2)>=M%YS .AND.  XYZ(2)<=M%YF .AND. XYZ(3)>=M%ZS .AND. XYZ(3)<=M%ZF) THEN
            IF (ABS(XYZ(1)-M%XS)<TWO_EPSILON_EB) XYZ(1) = XYZ(1) + 0.01_EB*M%DXI
            IF (ABS(XYZ(1)-M%XF)<TWO_EPSILON_EB) XYZ(1) = XYZ(1) - 0.01_EB*M%DXI
            IF (ABS(XYZ(2)-M%YS)<TWO_EPSILON_EB) XYZ(2) = XYZ(2) + 0.01_EB*M%DETA
            IF (ABS(XYZ(2)-M%YF)<TWO_EPSILON_EB) XYZ(2) = XYZ(2) - 0.01_EB*M%DETA
            IF (ABS(XYZ(3)-M%ZS)<TWO_EPSILON_EB) XYZ(3) = XYZ(3) + 0.01_EB*M%DZETA
            IF (ABS(XYZ(3)-M%ZF)<TWO_EPSILON_EB) XYZ(3) = XYZ(3) - 0.01_EB*M%DZETA
            EXIT MESH_LOOP
         ENDIF
      ENDDO MESH_LOOP

      XB(1:2) = XYZ(1)
      XB(3:4) = XYZ(2)
      XB(5:6) = XYZ(3)
   ENDIF

   ! If an offset has been specified, set the SHAPE to LINE.

   IF (DX/=0._EB .OR. DY/=0._EB .OR. DZ/=0._EB) SHAPE = 'LINE'

   IF (N_PARTICLES>0 .AND. SHAPE=='LINE') THEN
      XB(2) = XB(1) + DX*(N_PARTICLES-1)
      XB(4) = XB(3) + DY*(N_PARTICLES-1)
      XB(6) = XB(5) + DZ*(N_PARTICLES-1)
   ENDIF

   ! Create a box around a CONE

   IF (SHAPE=='CONE' .OR. SHAPE=='RING' .OR. SHAPE=='CYLINDER') THEN
      XB(1) = XYZ(1) - RADIUS
      XB(2) = XYZ(1) + RADIUS
      XB(3) = XYZ(2) - RADIUS
      XB(4) = XYZ(2) + RADIUS
      XB(5) = XYZ(3)
      XB(6) = XYZ(3) + HEIGHT
      IF (SHAPE=='RING') XB(6) = XB(5)

      IF (CROWN_BASE_WIDTH > 0._EB) THEN !WFDS compatablitiy
        HEIGHT = XYZ(3) + TREE_HEIGHT
        RADIUS = 0.5_EB*CROWN_BASE_WIDTH
        XB(1) = XYZ(1) - RADIUS
        XB(2) = XYZ(1) + RADIUS
        XB(3) = XYZ(2) - RADIUS
        XB(4) = XYZ(2) + RADIUS
        XB(5) = XYZ(3) + CROWN_BASE_HEIGHT
        XB(6) = HEIGHT
      ENDIF

   ENDIF

   ! Reorder XB coordinates if necessary

   CALL CHECK_XB(XB)

   ! Loop over all possible multiples of the INIT

   MR => MULTIPLIER(0)
   DO NNN=1,N_MULT
      IF (MULT_ID==MULTIPLIER(NNN)%ID) MR => MULTIPLIER(NNN)
   ENDDO

   NNN = 0
   K_MULT_LOOP: DO KK=MR%K_LOWER,MR%K_UPPER
      J_MULT_LOOP: DO JJ=MR%J_LOWER,MR%J_UPPER
         I_MULT_LOOP: DO II=MR%I_LOWER,MR%I_UPPER

            IF (MR%SKIP(II,JJ,KK)) CYCLE I_MULT_LOOP

            NNN = NNN + 1  ! Counter for MULT INIT lines

            NN = NN + 1
            IN => INITIALIZATION(NN)

            ! Store the input parameters

            IF (.NOT.MR%SEQUENTIAL) THEN
               IN%X1 = XB(1) + MR%DX0 + II*MR%DXB(1)
               IN%X2 = XB(2) + MR%DX0 + II*MR%DXB(2)
               IN%Y1 = XB(3) + MR%DY0 + JJ*MR%DXB(3)
               IN%Y2 = XB(4) + MR%DY0 + JJ*MR%DXB(4)
               IN%Z1 = XB(5) + MR%DZ0 + KK*MR%DXB(5)
               IN%Z2 = XB(6) + MR%DZ0 + KK*MR%DXB(6)
            ELSE
               IN%X1 = XB(1) + MR%DX0 + II*MR%DXB(1)
               IN%X2 = XB(2) + MR%DX0 + II*MR%DXB(2)
               IN%Y1 = XB(3) + MR%DY0 + II*MR%DXB(3)
               IN%Y2 = XB(4) + MR%DY0 + II*MR%DXB(4)
               IN%Z1 = XB(5) + MR%DZ0 + II*MR%DXB(5)
               IN%Z2 = XB(6) + MR%DZ0 + II*MR%DXB(6)
            ENDIF

            IF (MR%N_COPIES>1) THEN
               WRITE(IN%ID,'(A,A,I5.5)') TRIM(ID),'-',NNN
            ELSE
               IN%ID = ID
            ENDIF

            IN%CELL_CENTERED = CELL_CENTERED
            IN%DIAMETER      = DIAMETER*1.E-6_EB
            IN%DX            = DX
            IN%DY            = DY
            IN%DZ            = DZ
            IN%X0            = XYZ(1)
            IN%Y0            = XYZ(2)
            IN%Z0            = XYZ(3)
            IN%NODE_ID       = NODE_ID
            IN%CTRL_ID       = CTRL_ID
            IN%DEVC_ID       = DEVC_ID
            CALL SEARCH_CONTROLLER('INIT',IN%CTRL_ID,IN%DEVC_ID,IN%DEVC_INDEX,IN%CTRL_INDEX,N)
            IN%VOLUME        = (IN%X2-IN%X1)*(IN%Y2-IN%Y1)*(IN%Z2-IN%Z1)
            ALLOCATE(IN%VOLUME_ADJUST(NMESHES))
            MESH_LOOP_2: DO NM=1,NMESHES
               M=>MESHES(NM)
               CALL BLOCK_MESH_INTERSECTION_VOLUME(NM,IN%X1,IN%X2,IN%Y1,IN%Y2,IN%Z1,IN%Z2,IN%VOLUME_ADJUST(NM))
            ENDDO MESH_LOOP_2
            IN%TEMPERATURE   = TEMPERATURE + TMPM
            IN%DENSITY       = DENSITY
            IN%SHAPE         = SHAPE
            IN%HEIGHT        = HEIGHT
            IN%RADIUS        = RADIUS
            IN%INNER_RADIUS  = INNER_RADIUS
            IN%HRRPUV        = HRRPUV*1000._EB
            IN%CHI_R         = RADIATIVE_FRACTION
            IN%DRY           = DRY
            IF (HRRPUV > TWO_EPSILON_EB) INIT_HRRPUV = .TRUE.
            IF (RAMP_PART/='null') CALL GET_RAMP_INDEX(RAMP_PART,'TIME',IN%RAMP_PART_INDEX)
            IF (RAMP_Q/='null') CALL GET_RAMP_INDEX(RAMP_Q,'TIME',IN%RAMP_Q_INDEX)
            IF (DENSITY > 0._EB) RHOMAX = MAX(RHOMAX,IN%DENSITY)

            SPEC_INIT_IF: IF (ANY(MASS_FRACTION>=0._EB)) THEN

               IF (SPEC_ID(1)=='null') THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(844): INIT ',TRIM(ID),' requires a SPEC_ID.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               DO NS=1,MAX_SPECIES
                  IF (SPEC_ID(NS)=='null') EXIT
                  DO NS2=1,N_TRACKED_SPECIES
                     IF (TRIM(SPEC_ID(NS))==TRIM(SPECIES_MIXTURE(NS2)%ID)) THEN
                        IN%MASS_FRACTION(NS2) = MASS_FRACTION(NS)
                        EXIT
                     ENDIF
                     IF (NS2==N_TRACKED_SPECIES)  THEN
                        WRITE(MESSAGE,'(4A)') 'ERROR(845): INIT ',TRIM(ID),' cannot find SPEC_ID ',TRIM(SPEC_ID(NS))
                           CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDDO
               ENDDO

               IF (SUM(IN%MASS_FRACTION) > 1._EB) THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(846): INIT ',TRIM(ID),' cannot have sum of specified mass fractions > 1.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IF (IN%MASS_FRACTION(1)<=TWO_EPSILON_EB) THEN
                  IN%MASS_FRACTION(1) = 1._EB - SUM(IN%MASS_FRACTION(2:N_TRACKED_SPECIES))
               ELSE
                  WRITE(MESSAGE,'(3A)') 'ERROR(847): INIT ',TRIM(ID),' cannot use background species for MASS_FRACTION.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               ZZ_GET(1:N_TRACKED_SPECIES) = IN%MASS_FRACTION(1:N_TRACKED_SPECIES)
               CALL GET_SPECIFIC_GAS_CONSTANT(ZZ_GET,RR_SUM)
               IN%ADJUST_SPECIES_CONCENTRATION = .TRUE.

            ELSEIF (ANY(VOLUME_FRACTION>=0._EB)) THEN SPEC_INIT_IF
               MASS_FRACTION = 0._EB
               IF (SPEC_ID(1)=='null') THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(844): INIT ',TRIM(ID),' requires a SPEC_ID.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               NS3 = MAX_SPECIES
               DO NS=1,MAX_SPECIES
                  IF (SPEC_ID(NS)=='null') THEN
                     VOLUME_FRACTION(NS) = 0._EB
                     NS3 = MIN(MAX_SPECIES,NS-1)
                     EXIT
                  ENDIF
                  DO NS2=1,N_TRACKED_SPECIES
                     IF (TRIM(SPEC_ID(NS))==TRIM(SPECIES_MIXTURE(NS2)%ID)) THEN
                        MASS_FRACTION(NS2) = VOLUME_FRACTION(NS)*SPECIES_MIXTURE(NS2)%MW
                        EXIT
                     ENDIF
                     IF (NS2==N_TRACKED_SPECIES)  THEN
                        WRITE(MESSAGE,'(4A)') 'ERROR(845): INIT ',TRIM(ID),' cannot find SPEC_ID ',TRIM(SPEC_ID(NS))
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDDO
               ENDDO

               IF (MASS_FRACTION(1)<=TWO_EPSILON_EB) THEN
                  MASS_FRACTION(1) = (1._EB - SUM(VOLUME_FRACTION(1:NS3)))*SPECIES_MIXTURE(1)%MW
               ELSE
                  WRITE(MESSAGE,'(3A)') 'ERROR(847): INIT ',TRIM(ID),' cannot use background species for VOLUME_FRACTION.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               IN%MASS_FRACTION(1:N_TRACKED_SPECIES) = MASS_FRACTION(1:N_TRACKED_SPECIES)/SUM(MASS_FRACTION(1:N_TRACKED_SPECIES))
               ZZ_GET(1:N_TRACKED_SPECIES) = IN%MASS_FRACTION(1:N_TRACKED_SPECIES)
               CALL GET_SPECIFIC_GAS_CONSTANT(ZZ_GET,RR_SUM)
               IN%ADJUST_SPECIES_CONCENTRATION = .TRUE.

            ELSE SPEC_INIT_IF

               IN%MASS_FRACTION(1:N_TRACKED_SPECIES) = SPECIES_MIXTURE(1:N_TRACKED_SPECIES)%ZZ0
               RR_SUM = RSUM0

            ENDIF SPEC_INIT_IF

            IF (TEMPERATURE > 0._EB) TMPMIN = MIN(TMPMIN,IN%TEMPERATURE)

            IF (IN%TEMPERATURE > 0._EB .AND. IN%DENSITY < 0._EB) THEN
               IN%DENSITY        = P_INF/(IN%TEMPERATURE*RR_SUM)
               IN%ADJUST_DENSITY = .TRUE.
            ELSEIF (IN%TEMPERATURE < 0._EB .AND. IN%DENSITY > 0._EB) THEN
               IN%TEMPERATURE        = P_INF/(IN%DENSITY*RR_SUM)
               IN%ADJUST_TEMPERATURE = .TRUE.
            ENDIF

            ! Special case where INIT is used to introduce a block of particles

            IN%MASS_PER_TIME   = MASS_PER_TIME
            IN%MASS_PER_VOLUME = MASS_PER_VOLUME

            IF(N_PARTICLES_PER_CELL>0 .AND. N_PARTICLES>0) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(848): INIT ',N,' cannot use both N_PARTICLES and N_PARTICLES_PER_CELL.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF

            IN%N_PARTICLES     = N_PARTICLES
            IN%N_PARTICLES_PER_CELL = N_PARTICLES_PER_CELL
            IN%PARTICLE_WEIGHT_FACTOR = PARTICLE_WEIGHT_FACTOR

            IF (IN%MASS_PER_VOLUME>TWO_EPSILON_EB .AND. IN%VOLUME<TWO_EPSILON_EB) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(849): INIT ',N,' XB has no volume.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF

            IN%BULK_DENSITY_FACTOR = BULK_DENSITY_FACTOR
            IN%BULK_DENSITY_FILE = BULK_DENSITY_FILE

            ! Warnings for over-specified vegetation inputs
            IF (IN%BULK_DENSITY_FILE/='null') THEN
               ! BULK_DENSITY_FILES provide MASS_PER_VOLUME on a dry basis
               IN%DRY = .TRUE.
               N_PARTICLES_PER_CELL = MAX(N_PARTICLES_PER_CELL,1)
               IF (IN%MASS_PER_VOLUME>0._EB) THEN
                  WRITE(MESSAGE,'(A,I0,A)') 'WARNING: INIT ',N,' MASS_PER_VOLUME ignored in favor of BULK_DENSITY_FILE'
                  IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
               ENDIF
               IF(IN%N_PARTICLES>0) THEN
                  WRITE(MESSAGE,'(A,I0,A)') 'WARNING: INIT ',N,' N_PARTICLES ignored with BULK_DENSITY_FILE'
                  IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
                  IN%N_PARTICLES = 0
               ENDIF
            ENDIF

            IN%DT_INSERT = DT_INSERT
            IF (DT_INSERT>0._EB) IN%SINGLE_INSERTION = .FALSE.

            DO SURF_INDEX=0,N_SURF
               DO III=1,10
                  IF (IN%ID==SURFACE(SURF_INDEX)%INIT_IDS(III) .AND. SURFACE(SURF_INDEX)%INIT_IDS(III)/='null') THEN
                     IN%INVOKED_BY_SURF = .TRUE.
                     SURFACE(SURF_INDEX)%INIT_INDICES(III) = NN
                  ENDIF
               ENDDO
            ENDDO

            ! Set up a clock to keep track of particle insertions

            ALLOCATE(IN%PARTICLE_INSERT_CLOCK(NMESHES),STAT=IZERO)
            CALL ChkMemErr('READ','PARTICLE_INSERT_CLOCK',IZERO)
            IN%PARTICLE_INSERT_CLOCK = T_BEGIN

            ALLOCATE(IN%ALREADY_INSERTED(NMESHES),STAT=IZERO)
            CALL ChkMemErr('READ','ALREADY_INSERTED',IZERO)
            IN%ALREADY_INSERTED = .FALSE.

            ! Assign an index to identify the particle class

            PART_ID_IF: IF (PART_ID/='null') THEN

               DO NS=1,N_LAGRANGIAN_CLASSES
                  IF (PART_ID==LAGRANGIAN_PARTICLE_CLASS(NS)%ID) THEN
                     IN%PART_INDEX = NS
                     PARTICLE_FILE = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               IF (IN%PART_INDEX<1) THEN
                  WRITE(MESSAGE,'(4A)') 'ERROR(850): INIT ',TRIM(ID),' cannot find PART_ID ',TRIM(PART_ID)
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               LPC => LAGRANGIAN_PARTICLE_CLASS(IN%PART_INDEX)
               IN%N_PARTICLES = N_PARTICLES*MAX(1,LPC%N_ORIENTATION)
               IN%N_PARTICLES_PER_CELL = N_PARTICLES_PER_CELL*MAX(1,LPC%N_ORIENTATION)

               ! Check for a particle path

               IF (TRIM(PATH_RAMP(1))/='null' .OR. TRIM(PATH_RAMP(2))/='null' .OR. TRIM(PATH_RAMP(3))/='null') THEN
                  IF (.NOT. N_PARTICLES == 1) THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(851): INIT ',TRIM(ID),' requires N_PARTICLES=1 for a PATH_RAMP.'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ENDIF
                  IF (TRIM(PATH_RAMP(1))/='null') THEN
                     CALL GET_RAMP_INDEX(PATH_RAMP(1),'TIME',IN%PATH_RAMP_INDEX(1))
                     IF (IN%PATH_RAMP_INDEX(1)==0) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(852): INIT ',TRIM(ID),' PATH_RAMP(1) not found.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDIF
                  IF (TRIM(PATH_RAMP(2))/='null') THEN
                     CALL GET_RAMP_INDEX(PATH_RAMP(2),'TIME',IN%PATH_RAMP_INDEX(2))
                     IF (IN%PATH_RAMP_INDEX(2)==0) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(852): INIT ',TRIM(ID),' PATH_RAMP(2) not found.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDIF
                  IF (TRIM(PATH_RAMP(3))/='null') THEN
                     CALL GET_RAMP_INDEX(PATH_RAMP(3),'TIME',IN%PATH_RAMP_INDEX(3))
                     IF (IN%PATH_RAMP_INDEX(3)==0) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(852): INIT ',TRIM(ID),' PATH_RAMP(3) not found.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDIF
               ENDIF

               ! Check for a particle orientation

               IF (TRIM(ORIENTATION_RAMP(1))/='null' .OR. TRIM(ORIENTATION_RAMP(2))/='null' .OR. &
                   TRIM(ORIENTATION_RAMP(3))/='null') THEN
                  IF (TRIM(ORIENTATION_RAMP(1))/='null') THEN
                     CALL GET_RAMP_INDEX(ORIENTATION_RAMP(1),'TIME',IN%ORIENTATION_RAMP_INDEX(1))
                     IF (IN%ORIENTATION_RAMP_INDEX(1)==0) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(853): INIT ',TRIM(ID),' ORIENTATION_RAMP(1) not found.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDIF
                  IF (TRIM(ORIENTATION_RAMP(2))/='null') THEN
                     CALL GET_RAMP_INDEX(ORIENTATION_RAMP(2),'TIME',IN%ORIENTATION_RAMP_INDEX(2))
                     IF (IN%ORIENTATION_RAMP_INDEX(2)==0) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(853): INIT ',TRIM(ID),' ORIENTATION_RAMP(2) not found.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDIF
                  IF (TRIM(ORIENTATION_RAMP(3))/='null') THEN
                     CALL GET_RAMP_INDEX(ORIENTATION_RAMP(3),'TIME',IN%ORIENTATION_RAMP_INDEX(3))
                     IF (IN%ORIENTATION_RAMP_INDEX(3)==0) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(853): INIT ',TRIM(ID),' ORIENTATION_RAMP(3) not found.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ENDIF
                  ENDIF
                  IF (ANY(IN%ORIENTATION_RAMP_INDEX <= 0)) THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(854): INIT ',TRIM(ID),' ORIENTATION_RAMP components must all be defined.'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ENDIF
               ENDIF

            ENDIF PART_ID_IF

            ! Random position is default, set to UNIFORM if desired

            IN%UNIFORM = UNIFORM

            ! Initial velocity components

            IN%U0 = UVW(1)
            IN%V0 = UVW(2)
            IN%W0 = UVW(3)

         ENDDO I_MULT_LOOP
      ENDDO J_MULT_LOOP
   ENDDO K_MULT_LOOP

ENDDO INIT_LOOP

! Check if there are any devices that refer to INIT lines

DEVICE_LOOP: DO NN=1,N_DEVC
   DV => DEVICE(NN)
   IF (DV%INIT_ID=='null') CYCLE
   DO I=1,N_INIT
      IN => INITIALIZATION(I)
      IF (IN%ID==DV%INIT_ID) THEN
         IF (ANY(IN%PATH_RAMP_INDEX > 0)) THEN
            IF (DV%SPATIAL_STATISTIC/='null') THEN
               WRITE(MESSAGE,'(5A)') 'ERROR(855): INIT ',TRIM(IN%ID),' DEVC_ID ',TRIM(DV%ID),' cannot use a SPATIAL_STATISTIC.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDIF
         CYCLE DEVICE_LOOP
      ENDIF
   ENDDO
   WRITE(MESSAGE,'(3A)') 'ERROR(856): The INIT_ID for DEVC ',TRIM(DV%ID),' cannot be found.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDDO DEVICE_LOOP

! Rewind the input file and return

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

CONTAINS


!> \brief Set default values for the INIT namelist group

SUBROUTINE SET_INIT_DEFAULTS

BULK_DENSITY_FACTOR       = 1.0_EB
BULK_DENSITY_FILE         = 'null'
CELL_CENTERED             = .FALSE.
CROWN_BASE_WIDTH          = -1._EB
CROWN_BASE_HEIGHT         = -1._EB
CTRL_ID                   = 'null'
DENSITY                   = -1000._EB
DEVC_ID                   = 'null'
DIAMETER                  = -1._EB
DRY                       = .FALSE.
DT_INSERT                 = -1._EB
DX                        =  0._EB
DY                        =  0._EB
DZ                        =  0._EB
HEIGHT                    = -1._EB
HRRPUV                    =  0._EB
ID                        = 'null'
INNER_RADIUS              = 0._EB
MASS_FRACTION             = -1._EB
MASS_PER_TIME             = -1._EB
MASS_PER_VOLUME           = -1._EB
MULT_ID                   = 'null'
N_PARTICLES               = 0
N_PARTICLES_PER_CELL      = 0
PARTICLE_WEIGHT_FACTOR    = 1.0_EB
NODE_ID                   = 'null'
ORIENTATION_RAMP          = 'null'
PART_ID                   = 'null'
PATH_RAMP                 = 'null'
RADIATIVE_FRACTION        = 0.0_EB
RADIUS                    = -1._EB
RAMP_PART                 = 'null'
RAMP_Q                    = 'null'
SHAPE                     = 'BLOCK'
SPEC_ID                   = 'null'
TEMPERATURE               = -1000._EB
TREE_HEIGHT               = -1._EB
UNIFORM                   = .FALSE.
UVW                       = 0._EB
VOLUME_FRACTION           = -1._EB
DB                        = 'null'
XB(1)                     = -1000000._EB
XB(2)                     =  1000000._EB
XB(3)                     = -1000000._EB
XB(4)                     =  1000000._EB
XB(5)                     = -1000000._EB
XB(6)                     =  1000000._EB
XYZ                       = -1000000._EB

END SUBROUTINE SET_INIT_DEFAULTS

END SUBROUTINE READ_INIT


SUBROUTINE PROC_INIT

INTEGER :: NN
REAL(EB) :: MOIST_FRAC
TYPE (LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC => NULL()
TYPE (INITIALIZATION_TYPE), POINTER :: IN => NULL()

DO NN=1,N_INIT
   IN=>INITIALIZATION(NN)
   IF (IN%MASS_PER_TIME>0._EB .OR. IN%MASS_PER_VOLUME>0._EB .OR. IN%BULK_DENSITY_FILE/='null') THEN
      LPC => LAGRANGIAN_PARTICLE_CLASS(IN%PART_INDEX)
      IF (LPC%DENSITY < 0._EB) THEN
         WRITE(MESSAGE,'(5A)') 'ERROR(857): INIT ',TRIM(IN%ID),' PARTicle class ',TRIM(LPC%ID),' requires a density.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ELSE
         IF (LPC%DRAG_LAW == POROUS_DRAG .AND. IN%MASS_PER_VOLUME>0._EB) &
            LPC%POROUS_VOLUME_FRACTION = IN%MASS_PER_VOLUME / LPC%DENSITY
      ENDIF
      IF (LPC%SURF_INDEX>0) THEN
         MOIST_FRAC = SURFACE(LPC%SURF_INDEX)%MOISTURE_FRACTION(1)
         IF (MOIST_FRAC>TWO_EPSILON_EB .AND. IN%DRY) THEN
            IN%MASS_PER_TIME   = IN%MASS_PER_TIME  *(1._EB+MOIST_FRAC)
            IN%MASS_PER_VOLUME = IN%MASS_PER_VOLUME*(1._EB+MOIST_FRAC)
         ENDIF
      ENDIF
   ENDIF
   IF (IN%PART_INDEX >0) THEN
      LPC => LAGRANGIAN_PARTICLE_CLASS(IN%PART_INDEX)
      IF (LPC%DRAG_LAW == POROUS_DRAG .AND. LPC%POROUS_VOLUME_FRACTION <= 0._EB) THEN
         WRITE(MESSAGE,'(5A)') 'ERROR (858): INIT ',TRIM(IN%ID),' PARTicle class ',TRIM(LPC%ID),&
                               ' requires a PART POROUS_VOLUME_FRACTION or INIT MASS_PER_VOLUME'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
ENDDO

END SUBROUTINE PROC_INIT


!> \brief Read the ZONE namelist lines

SUBROUTINE READ_ZONE

REAL(EB), ALLOCATABLE, DIMENSION(:) :: LEAK_AREA,LEAK_REFERENCE_PRESSURE,LEAK_PRESSURE_EXPONENT,DISCHARGE_COEFFICIENT
REAL(EB) :: XYZ(3,N_ZONE_POINTS)
INTEGER  :: N,NM,NN
CHARACTER(LABEL_LENGTH) :: ID
INTEGER, ALLOCATABLE, DIMENSION(:) :: COUNTS,DISPLS
NAMELIST /ZONE/ DISCHARGE_COEFFICIENT,ID,LEAK_AREA,LEAK_PRESSURE_EXPONENT,LEAK_REFERENCE_PRESSURE,XYZ

ALLOCATE (LEAK_AREA(0:MAX_LEAK_PATHS))
ALLOCATE (LEAK_REFERENCE_PRESSURE(0:MAX_LEAK_PATHS))
ALLOCATE (LEAK_PRESSURE_EXPONENT(0:MAX_LEAK_PATHS))
ALLOCATE (DISCHARGE_COEFFICIENT(0:MAX_LEAK_PATHS))

! Count the ZONE lines

N_ZONE = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_ZONE_LOOP: DO
   CALL CHECKREAD('ZONE',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_ZONE_LOOP
   READ(LU_INPUT,NML=ZONE,END=11,ERR=12,IOSTAT=IOS)
   N_ZONE = N_ZONE + 1
   12 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with ZONE number ',N_ZONE+1
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_ZONE_LOOP
11 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! If all of the meshes are sealed and no ZONEs are declared, stop with an ERROR

ALLOCATE(COUNTS(0:N_MPI_PROCESSES-1)) ; COUNTS = 0
ALLOCATE(DISPLS(0:N_MPI_PROCESSES-1)) ; DISPLS = 0
DO N=0,N_MPI_PROCESSES-1
   DO NM=1,NMESHES
      IF (PROCESS(NM)==N) COUNTS(N) = COUNTS(N) + 1
   ENDDO
   IF (N>0) DISPLS(N) = COUNTS(N-1) + DISPLS(N-1)
ENDDO

! If there are no ZONE lines, return

IF (N_ZONE==0) RETURN

! Allocate ZONE arrays

ALLOCATE(P_ZONE(N_ZONE),STAT=IZERO)
CALL ChkMemErr('READ','P_ZONE',IZERO)

! Read in and process ZONE lines

READ_ZONE_LOOP: DO N=1,N_ZONE

   ALLOCATE(P_ZONE(N)%LEAK_AREA(0:N_ZONE),STAT=IZERO)
   CALL ChkMemErr('READ','LEAK_AREA',IZERO)
   ALLOCATE(P_ZONE(N)%DISCHARGE_COEFFICIENT(0:N_ZONE),STAT=IZERO)
   CALL ChkMemErr('READ','DISCHARGE_COEFFICIENT',IZERO)
   ALLOCATE(P_ZONE(N)%LEAK_PRESSURE_EXPONENT(0:N_ZONE),STAT=IZERO)
   CALL ChkMemErr('READ','LEAK_PRESSURE_EXPONENT',IZERO)
   ALLOCATE(P_ZONE(N)%LEAK_REFERENCE_PRESSURE(0:N_ZONE),STAT=IZERO)
   CALL ChkMemErr('READ','LEAK_REFERENCE_PRESSURE',IZERO)

   ! Default ZONE parameters

   WRITE(ID,'(A,I0)') 'ZONE_',N
   DISCHARGE_COEFFICIENT = 1._EB
   LEAK_AREA     = 0._EB
   LEAK_REFERENCE_PRESSURE = 4._EB
   LEAK_PRESSURE_EXPONENT = 0.5_EB
   XYZ           = -1000000._EB

   ! Read the ZONE line(s)

   CALL CHECKREAD('ZONE',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_ZONE_LOOP
   READ(LU_INPUT,ZONE)

   ! Assign parameters to P_ZONE dervied type variable

   P_ZONE(N)%ID = ID

   IF (XYZ(1,1)>-999999._EB) THEN  ! Process the first XYZ point. The rest are ignored.
      P_ZONE(N)%X = XYZ(1,1)
      P_ZONE(N)%Y = XYZ(2,1)
      P_ZONE(N)%Z = XYZ(3,1)
   ELSE  ! Find the first non-solid cell in the first mesh controlled by this MPI process
      M => MESHES(LOWER_MESH_INDEX)
      K_LOOP: DO K=1,M%KBAR
         J_LOOP: DO J=1,M%JBAR
            I_LOOP: DO I=1,M%IBAR
               IF (M%CELL(M%CELL_INDEX(I,J,K))%SOLID) CYCLE I_LOOP
               P_ZONE(N)%X = M%XC(I)
               P_ZONE(N)%Y = M%YC(J)
               P_ZONE(N)%Z = M%ZC(K)
               EXIT K_LOOP
            ENDDO I_LOOP
         ENDDO J_LOOP
      ENDDO K_LOOP
   ENDIF

   ! Check the leakage paths and logic

   DO NN=0,N_ZONE
      P_ZONE(N)%LEAK_AREA(NN) = LEAK_AREA(NN)
      P_ZONE(N)%LEAK_REFERENCE_PRESSURE(NN) = LEAK_REFERENCE_PRESSURE(NN)
      P_ZONE(N)%LEAK_PRESSURE_EXPONENT(NN) = LEAK_PRESSURE_EXPONENT(NN)
      P_ZONE(N)%DISCHARGE_COEFFICIENT(NN) = DISCHARGE_COEFFICIENT(NN)
   ENDDO

   IF (N > 1) THEN
      DO NN=1,N-1
         IF (P_ZONE(NN)%LEAK_AREA(N)>0._EB) THEN
            IF (P_ZONE(N)%LEAK_AREA(NN) > 0._EB) THEN
               WRITE(MESSAGE,'(A,I0,A,I0)')  'ERROR(871): LEAK_AREA specified twice for ZONE ',N,' and ',NN
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ELSE
               P_ZONE(N)%LEAK_AREA(NN)               = P_ZONE(NN)%LEAK_AREA(N)
               P_ZONE(N)%LEAK_REFERENCE_PRESSURE(NN) = P_ZONE(NN)%LEAK_REFERENCE_PRESSURE(N)
               P_ZONE(N)%LEAK_PRESSURE_EXPONENT(NN)  = P_ZONE(NN)%LEAK_PRESSURE_EXPONENT(N)
               P_ZONE(N)%DISCHARGE_COEFFICIENT(NN)   = P_ZONE(NN)%DISCHARGE_COEFFICIENT(N)
            ENDIF
         ENDIF
      ENDDO
   ENDIF

ENDDO READ_ZONE_LOOP

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

DEALLOCATE (LEAK_AREA)

END SUBROUTINE READ_ZONE


!> \brief Read the DEViCe namelist lines and the store the info in DEVICE()

SUBROUTINE READ_DEVC

USE DEVICE_VARIABLES, ONLY: DEVICE_TYPE,SUBDEVICE_TYPE,DEVICE,N_DEVC,N_DEVC_TIME,N_DEVC_LINE,MAX_DEVC_LINE_POINTS,&
                            DEVC_PIPE_OPERATING
USE GEOMETRY_FUNCTIONS, ONLY: TRANSFORM_COORDINATES,SEARCH_OTHER_MESHES
INTEGER  :: N,NN,NM,MESH_NUMBER=0,N_DEVC_READ,IOR,TRIP_DIRECTION,VELO_INDEX,POINTS,I_POINT,PIPE_INDEX,ORIENTATION_INDEX, &
            N_INTERVALS,MOVE_INDEX,IIG,JJG,KKG,NOM,ERROR_CODE
REAL(EB) :: DEPTH,ORIENTATION(3),ROTATION,SETPOINT,FLOWRATE,BYPASS_FLOWRATE,DELAY,XYZ(3),CONVERSION_FACTOR,CONVERSION_ADDEND, &
            SMOOTHING_FACTOR,QUANTITY_RANGE(2),STATISTICS_START,STATISTICS_END,COORD_FACTOR,CELL_L,&
            TIME_PERIOD,FORCE_DIRECTION(3),XI,YJ,ZK,XBP(6),DX,DY,DZ,&
            POINTS_ARRAY_X(POINTS_ARRAY_DIM),POINTS_ARRAY_Y(POINTS_ARRAY_DIM),POINTS_ARRAY_Z(POINTS_ARRAY_DIM)
CHARACTER(LABEL_LENGTH) :: QUANTITY,QUANTITY2,PROP_ID,CTRL_ID,DEVC_ID,INIT_ID,SURF_ID,SPATIAL_STATISTIC,TEMPORAL_STATISTIC,&
                 MOVE_ID,STATISTICS,PART_ID,MATL_ID,SPEC_ID,UNITS, &
                 DUCT_ID,NODE_ID(2),D_ID,R_ID,X_ID,Y_ID,Z_ID,NO_UPDATE_DEVC_ID,NO_UPDATE_CTRL_ID,REAC_ID,XYZ_UNITS
LOGICAL :: INITIAL_STATE,LATCH,DRY,TIME_AVERAGED,HIDE_COORDINATES,RELATIVE,OUTPUT,NEW_ORIENTATION_VECTOR,TIME_HISTORY,&
           LINE_DEVICE,ABSOLUTE_VALUE,OVERLAPPING_X,OVERLAPPING_Y,OVERLAPPING_Z
TYPE (DEVICE_TYPE), POINTER :: DV
TYPE (SUBDEVICE_TYPE), POINTER :: SDV
NAMELIST /DEVC/ ABSOLUTE_VALUE,BYPASS_FLOWRATE,CELL_L,CONVERSION_ADDEND,CONVERSION_FACTOR,COORD_FACTOR,CTRL_ID,DB,DELAY,DEPTH,&
                DEVC_ID,D_ID,DRY,DUCT_ID,DX,DY,DZ,FLOWRATE,FORCE_DIRECTION,FYI,HIDE_COORDINATES,ID,&
                INITIAL_STATE,INIT_ID,IOR,LATCH,MATL_ID,MOVE_ID,N_INTERVALS,NODE_ID,NO_UPDATE_CTRL_ID,NO_UPDATE_DEVC_ID,&
                ORIENTATION,OUTPUT,PART_ID,PIPE_INDEX,&
                POINTS,POINTS_ARRAY_X,POINTS_ARRAY_Y,POINTS_ARRAY_Z,&
                PROP_ID,QUANTITY,QUANTITY2,QUANTITY_RANGE,REAC_ID,RELATIVE,R_ID,ROTATION,SETPOINT,SMOOTHING_FACTOR,&
                SPATIAL_STATISTIC,SPEC_ID,STATISTICS,STATISTICS_END,STATISTICS_START,SURF_ID,TEMPORAL_STATISTIC,&
                TIME_AVERAGED,TIME_HISTORY,TIME_PERIOD,TRIP_DIRECTION,UNITS,VELO_INDEX,XB,XBP,XYZ,X_ID,Y_ID,Z_ID,XYZ_UNITS
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: MESH_DEVICE_ARRAY
INTEGER, ALLOCATABLE, DIMENSION(:) :: MESH_DEVICE

! Read the input file and count the number of DEVC lines

N_DEVC = 0
N_DEVC_READ = 0
N_DEVC_TIME = 0
N_DEVC_LINE = 0

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_DEVC_LOOP: DO
   CALL CHECKREAD('DEVC',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_DEVC_LOOP
   POINTS = 1
   TIME_HISTORY = .FALSE.
   READ(LU_INPUT,NML=DEVC,END=11,ERR=12,IOSTAT=IOS)
   N_DEVC      = N_DEVC      + POINTS
   N_DEVC_READ = N_DEVC_READ + 1
   IF (POINTS>1 .AND. .NOT.TIME_HISTORY) MAX_DEVC_LINE_POINTS = MAX(MAX_DEVC_LINE_POINTS,POINTS)
   12 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with DEVC number ',N_DEVC_READ+1
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO COUNT_DEVC_LOOP
11 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_DEVC==0) RETURN

! Allocate DEVICE array to hold all information for each device

ALLOCATE(DEVICE(N_DEVC),STAT=IZERO) ; CALL ChkMemErr('READ','DEVICE',IZERO)

! Speceial case for QUANTITY='RADIATIVE HEAT FLUX GAS' or 'ADIABATIC SURFACE TEMPERATURE GAS'

ALLOCATE(INIT_RESERVED(N_DEVC),STAT=IZERO) ; CALL ChkMemErr('READ','INIT_RESERVED',IZERO)
N_INIT_RESERVED = 0

! Allocate temporary logical arrays to indicate if a given mesh holds a given device

ALLOCATE(MESH_DEVICE_ARRAY(NMESHES,N_DEVC)) ; MESH_DEVICE_ARRAY = 0
ALLOCATE(MESH_DEVICE(NMESHES))

! Read in the DEVC lines, keeping track of TIME-history devices, and LINE array devices

N_DEVC = 0

READ_DEVC_LOOP: DO NN=1,N_DEVC_READ

   CALL CHECKREAD('DEVC',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_DEVC_LOOP
   CALL SET_DEVC_DEFAULTS
   READ(LU_INPUT,DEVC)

   ! Backward compatibility

   IF (QUANTITY(1:11)=='VOLUME FLOW') THEN
      IF (QUANTITY(13:13)=='+') QUANTITY_RANGE(1) = 0._EB
      IF (QUANTITY(13:13)=='-') QUANTITY_RANGE(2) = 0._EB
      SPATIAL_STATISTIC = 'AREA INTEGRAL'
      IF (XB(1)==XB(2)) QUANTITY = 'U-VELOCITY'
      IF (XB(3)==XB(4)) QUANTITY = 'V-VELOCITY'
      IF (XB(5)==XB(6)) QUANTITY = 'W-VELOCITY'
   ENDIF

   IF (QUANTITY(1:9)=='MASS FLOW') THEN
      IF (QUANTITY(11:11)=='+') QUANTITY_RANGE(1) = 0._EB
      IF (QUANTITY(11:11)=='-') QUANTITY_RANGE(2) = 0._EB
      SPATIAL_STATISTIC = 'AREA INTEGRAL'
      IF (XB(1)==XB(2)) QUANTITY = 'MASS FLUX X'
      IF (XB(3)==XB(4)) QUANTITY = 'MASS FLUX Y'
      IF (XB(5)==XB(6)) QUANTITY = 'MASS FLUX Z'
   ENDIF

   IF (QUANTITY(1:9)=='HEAT FLOW') THEN
      IF (QUANTITY(11:11)=='+') QUANTITY_RANGE(1) = 0._EB
      IF (QUANTITY(11:11)=='-') QUANTITY_RANGE(2) = 0._EB
      SPATIAL_STATISTIC = 'AREA INTEGRAL'
      IF (XB(1)==XB(2)) QUANTITY = 'ENTHALPY FLUX X'
      IF (XB(3)==XB(4)) QUANTITY = 'ENTHALPY FLUX Y'
      IF (XB(5)==XB(6)) QUANTITY = 'ENTHALPY FLUX Z'
   ENDIF

   IF (QUANTITY=='HRR') THEN
      SPATIAL_STATISTIC = 'VOLUME INTEGRAL'
      QUANTITY = 'HRRPUV'
   ENDIF

   ! Special cases

   IF (QUANTITY=='TRANSMISSION') QUANTITY2 = 'PATHLENGTH'
   IF (QUANTITY=='FIRE DEPTH'.AND.(SETPOINT-1.E20_EB)<TWO_EPSILON_EB) SETPOINT = MIN(200._EB,20._EB/CHARACTERISTIC_CELL_SIZE)

   ! Check to see if a domain boundary has been set

   IF (DB/='null') THEN
      ! any string will work, but suggest DB='WHOLE DOMAIN'
      XB(1) = XS_MIN
      XB(2) = XF_MAX
      XB(3) = YS_MIN
      XB(4) = YF_MAX
      XB(5) = ZS_MIN
      XB(6) = ZF_MAX
   ENDIF

   ! Check the QUANTITY_RANGE

   IF (QUANTITY_RANGE(2) <= QUANTITY_RANGE(1)) THEN
      WRITE(MESSAGE,'(3A)')  'ERROR(881): DEVC ',TRIM(ID),' has QUANTITY_RANGE(2) <= QUANTITY_RANGE(1).'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Transform coordinates if necessary

   MOVE_INDEX = 0
   IF (MOVE_ID/='null') THEN
      DO I=1,N_MOVE
         IF (MOVE_ID==MOVEMENT(I)%ID) THEN
            MOVE_INDEX = MOVEMENT(I)%INDEX
            EXIT
         ENDIF
      ENDDO
      IF (MOVE_INDEX==0) THEN
         WRITE(MESSAGE,'(3A)')  'ERROR(882): DEVC ',TRIM(ID),' MOVE_ID is not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (XYZ(1)>-1.E5_EB) CALL TRANSFORM_COORDINATES(XYZ(1),XYZ(2),XYZ(3),MOVE_INDEX,1)
      IF (XB(1) >-1.E5_EB) CALL TRANSFORM_COORDINATES(XB(1),XB(3),XB(5),MOVE_INDEX,1)
      IF (XB(1) >-1.E5_EB) CALL TRANSFORM_COORDINATES(XB(2),XB(4),XB(6),MOVE_INDEX,1)
      IF (NORM2(ORIENTATION)<1.E5) CALL TRANSFORM_COORDINATES(ORIENTATION(1),ORIENTATION(2),ORIENTATION(3),MOVE_INDEX,2)
   ENDIF

   ! Save POINTS line coordinates in XBP

   IF (POINTS>1 .AND. XBP(1)<-1.E5_EB) THEN
      XBP = XB
      XB  = -1.E6_EB
   ENDIF

   ! Reassign the old parameter STATISTICS to either SPATIAL_STATISTIC or TEMPORAL_STATISTIC.
   ! Also, look for names that do not exist.

   CALL CHECK_STATISTIC_NAME(STATISTICS,1,ERROR_CODE)         ; IF (ERROR_CODE>0) THEN ; CALL SHUTDOWN(MESSAGE) ; RETURN ; ENDIF
   CALL CHECK_STATISTIC_NAME(TEMPORAL_STATISTIC,2,ERROR_CODE) ; IF (ERROR_CODE>0) THEN ; CALL SHUTDOWN(MESSAGE) ; RETURN ; ENDIF
   CALL CHECK_STATISTIC_NAME(SPATIAL_STATISTIC,3,ERROR_CODE)  ; IF (ERROR_CODE>0) THEN ; CALL SHUTDOWN(MESSAGE) ; RETURN ; ENDIF

   ! Backward compartibility

   IF (.NOT.TIME_AVERAGED) TEMPORAL_STATISTIC = 'INSTANT VALUE'

   ! Determine if the device is a steady-state "line" device or the usual time-history device.

   LINE_DEVICE = .FALSE.
   IF (POINTS>1 .AND. .NOT.TIME_HISTORY) THEN
      LINE_DEVICE = .TRUE.
      IF (STATISTICS_START<-1.E6_EB) THEN
         STATISTICS_START = T_BEGIN + 0.5_EB*(T_END-T_BEGIN)
      ELSE
         STATISTICS_START = T_BEGIN + (STATISTICS_START-T_BEGIN)/TIME_SHRINK_FACTOR
      ENDIF
      IF (TEMPORAL_STATISTIC=='null') TEMPORAL_STATISTIC='RUNNING AVERAGE'
   ELSE
      IF (STATISTICS_START<-1.E6_EB) STATISTICS_START = T_BEGIN
   ENDIF

   IF (STATISTICS_END>1.E6_EB) THEN
      STATISTICS_END = T_END + TWO_EPSILON_EB
   ELSE
      STATISTICS_END = T_BEGIN + (STATISTICS_END-T_BEGIN)/TIME_SHRINK_FACTOR
   ENDIF

   ! Error statement involving POINTS

   IF (POINTS>1 .AND. ANY(XBP<-1.E5_EB) .AND. INIT_ID=='null') THEN
      WRITE(MESSAGE,'(3A)')  'ERROR(883): DEVC ',TRIM(ID),' must have coordinates given in terms of XBP.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Make ORIENTATION consistent with IOR

   SELECT CASE(IOR)
      CASE( 1) ; ORIENTATION=(/ 1._EB, 0._EB, 0._EB/)
      CASE(-1) ; ORIENTATION=(/-1._EB, 0._EB, 0._EB/)
      CASE( 2) ; ORIENTATION=(/ 0._EB, 1._EB, 0._EB/)
      CASE(-2) ; ORIENTATION=(/ 0._EB,-1._EB, 0._EB/)
      CASE( 3) ; ORIENTATION=(/ 0._EB, 0._EB, 1._EB/)
      CASE(-3) ; ORIENTATION=(/ 0._EB, 0._EB,-1._EB/)
   END SELECT

   ! Add ORIENTATION to global list

   ORIENTATION_INDEX = 0

   IF (NORM2(ORIENTATION)<1.E5) THEN
      NEW_ORIENTATION_VECTOR = .TRUE.
      DO I=1,N_ORIENTATION_VECTOR
         IF (ABS(ORIENTATION(1)-ORIENTATION_VECTOR(1,I))<TWO_EPSILON_EB .AND. &
             ABS(ORIENTATION(2)-ORIENTATION_VECTOR(2,I))<TWO_EPSILON_EB .AND. &
             ABS(ORIENTATION(3)-ORIENTATION_VECTOR(3,I))<TWO_EPSILON_EB) THEN
            NEW_ORIENTATION_VECTOR = .FALSE.
            ORIENTATION_INDEX = I
            EXIT
         ENDIF
      ENDDO
   ELSE
      NEW_ORIENTATION_VECTOR = .FALSE.
   ENDIF

   IF (NEW_ORIENTATION_VECTOR) THEN
      N_ORIENTATION_VECTOR = N_ORIENTATION_VECTOR + 1
      IF (N_ORIENTATION_VECTOR>UBOUND(ORIENTATION_VECTOR,DIM=2)) THEN
         ORIENTATION_VECTOR => REALLOCATE2D(ORIENTATION_VECTOR,1,3,0,N_ORIENTATION_VECTOR+10)
      ENDIF
      IF (ALL(ABS(ORIENTATION(1:3))<TWO_EPSILON_EB)) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(884): DEVC ',TRIM(ID),' components of ORIENTATION are all zero.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      ORIENTATION_VECTOR(1:3,N_ORIENTATION_VECTOR) = ORIENTATION(1:3) / NORM2(ORIENTATION)
      ORIENTATION_INDEX = N_ORIENTATION_VECTOR
   ENDIF

   ! Process the point devices along a line, if necessary

   POINTS_LOOP: DO I_POINT=1,POINTS

      MESH_DEVICE = 0               ! MESH_DEVICE(NM)=1 means that the device involves mesh NM
      IF (I_POINT>1) XB = -1.E6_EB  ! Reset XB for new point along a linear array of POINTS

      ! Create a straight line of point devices

      IF (POINTS>1) THEN
         XYZ(1) = XBP(1) + (XBP(2)-XBP(1))*REAL(I_POINT-1,EB)/REAL(POINTS-1,EB)
         XYZ(2) = XBP(3) + (XBP(4)-XBP(3))*REAL(I_POINT-1,EB)/REAL(POINTS-1,EB)
         XYZ(3) = XBP(5) + (XBP(6)-XBP(5))*REAL(I_POINT-1,EB)/REAL(POINTS-1,EB)
         IF (I_POINT<=POINTS_ARRAY_DIM) THEN
            IF (POINTS_ARRAY_X(I_POINT)>-1.E6) XYZ(1) = POINTS_ARRAY_X(I_POINT)
            IF (POINTS_ARRAY_Y(I_POINT)>-1.E6) XYZ(2) = POINTS_ARRAY_Y(I_POINT)
            IF (POINTS_ARRAY_Z(I_POINT)>-1.E6) XYZ(3) = POINTS_ARRAY_Z(I_POINT)
         ENDIF
         IF (DX>0._EB .OR. DY>0._EB .OR. DZ>0._EB) THEN
            XB(1) = XYZ(1) - DX
            XB(2) = XYZ(1) + DX
            XB(3) = XYZ(2) - DY
            XB(4) = XYZ(2) + DY
            XB(5) = XYZ(3) - DZ
            XB(6) = XYZ(3) + DZ
         ENDIF
      ENDIF

      ! Assign a dummy XYZ triplet for devices that use a SPATIAL_STATISTIC

      IF (XB(1)>-1.E5_EB) THEN
         IF (TRIM(QUANTITY)=='VELOCITY PATCH') THEN
            IF (ANY(XYZ<-1.E5_EB)) THEN
               XYZ(1) = XB(1) + (XB(2)-XB(1))/2._EB
               XYZ(2) = XB(3) + (XB(4)-XB(3))/2._EB
               XYZ(3) = XB(5) + (XB(6)-XB(5))/2._EB
            ENDIF
         ELSE
            IF (POINTS==1 .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               XYZ(1) = XB(1) + (XB(2)-XB(1))/2._EB
               XYZ(2) = XB(3) + (XB(4)-XB(3))/2._EB
               XYZ(3) = XB(5) + (XB(6)-XB(5))/2._EB
            ENDIF
         ENDIF
      ELSE
         IF (XYZ(1) < -1.E5_EB .AND. CTRL_ID=='null' .AND. DUCT_ID=='null' .AND. NODE_ID(1)=='null' .AND. INIT_ID=='null' &
            .AND. CELL_L < 0._EB) THEN
            WRITE(MESSAGE,'(3A)')  'ERROR(885): DEVC ',TRIM(ID),' must have coordinates, even if it is not a point quantity.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF

      ! Check if there are any devices with specified XB that do not fall within a mesh.

      IF (XB(1)>-1.E5_EB) THEN
         IF (QUANTITY/='PATH OBSCURATION' .AND. QUANTITY/='TRANSMISSION' .AND. QUANTITY/='FIRE DEPTH') CALL CHECK_XB(XB)
         BAD = .TRUE.
         CHECK_MESH_LOOP: DO NM=1,NMESHES
            M=>MESHES(NM)
            OVERLAPPING_X = .TRUE.
            OVERLAPPING_Y = .TRUE.
            OVERLAPPING_Z = .TRUE.
            IF (XB(1)==XB(2) .AND. (XB(1)> M%XF .OR. XB(2)< M%XS)) OVERLAPPING_X = .FALSE.
            IF (XB(1)/=XB(2) .AND. ((XB(1)>=M%XF.AND.XB(2)>=M%XF) .OR. (XB(1)<=M%XS.AND.XB(2)<=M%XS))) OVERLAPPING_X = .FALSE.
            IF (XB(3)==XB(4) .AND. (XB(3)> M%YF .OR. XB(4)< M%YS)) OVERLAPPING_Y = .FALSE.
            IF (XB(3)/=XB(4) .AND. ((XB(3)>=M%YF.AND.XB(4)>=M%YF) .OR. (XB(3)<=M%YS.AND.XB(4)<=M%YS))) OVERLAPPING_Y = .FALSE.
            IF (XB(5)==XB(6) .AND. (XB(5)> M%ZF .OR. XB(6)< M%ZS)) OVERLAPPING_Z = .FALSE.
            IF (XB(5)/=XB(6) .AND. ((XB(5)>=M%ZF.AND.XB(6)>=M%ZF) .OR. (XB(5)<=M%ZS.AND.XB(6)<=M%ZS))) OVERLAPPING_Z = .FALSE.

            ! Handle the case of XB plane on interpolated mesh boundary
            ! This block is necessary so that XB statistics are not double counted at mesh interfaces.
            ! The strategy is to assign the DV to the high-side mesh boundary.  For example, if the device plane is normal to X,
            ! then the DV will be assigned to the mesh where XB(1)=XB(2)=M%XF.
            NOM=0
            IF (XB(1)==XB(2) .AND. OVERLAPPING_X) THEN
               IF (XYZ(1)==M%XS) CALL SEARCH_OTHER_MESHES(XYZ(1)-TWO_EPSILON_EB,XYZ(2),XYZ(3),NOM,IIG,JJG,KKG,XI,YJ,ZK)
               IF (NOM>0) OVERLAPPING_X = .FALSE. ! Device will be assigned to NOM
            ENDIF
            IF (XB(3)==XB(4) .AND. OVERLAPPING_Y) THEN
               IF (XYZ(2)==M%YS) CALL SEARCH_OTHER_MESHES(XYZ(1),XYZ(2)-TWO_EPSILON_EB,XYZ(3),NOM,IIG,JJG,KKG,XI,YJ,ZK)
               IF (NOM>0) OVERLAPPING_Y = .FALSE.
            ENDIF
            IF (XB(5)==XB(6) .AND. OVERLAPPING_Z) THEN
               IF (XYZ(3)==M%ZS) CALL SEARCH_OTHER_MESHES(XYZ(1),XYZ(2),XYZ(3)-TWO_EPSILON_EB,NOM,IIG,JJG,KKG,XI,YJ,ZK)
               IF (NOM>0) OVERLAPPING_Z = .FALSE.
            ENDIF

            IF (OVERLAPPING_X .AND. OVERLAPPING_Y .AND. OVERLAPPING_Z) THEN
               BAD = .FALSE.
               IF (PROCESS(NM)==MY_RANK) MESH_DEVICE(NM) = 1
               MESH_NUMBER = NM
            ENDIF
         ENDDO CHECK_MESH_LOOP
      ENDIF

      ! Determine the bounds, XB, for an interpolated gas device

      IF (SPATIAL_STATISTIC=='INTERPOLATION') THEN
         CALL SEARCH_OTHER_MESHES(XYZ(1),XYZ(2),XYZ(3),NM,IIG,JJG,KKG,XI,YJ,ZK)
         IF (NM>0 .AND. IIG>0 .AND. JJG>0 .AND. KKG>0) THEN
            M => MESHES(NM)
            XB(1) = M%X(NINT(XI)) - 0.5_EB*M%DX(IIG)
            XB(2) = M%X(NINT(XI)) + 0.5_EB*M%DX(IIG)
            XB(3) = M%Y(NINT(YJ)) - 0.5_EB*M%DY(JJG)
            XB(4) = M%Y(NINT(YJ)) + 0.5_EB*M%DY(JJG)
            XB(5) = M%Z(NINT(ZK)) - 0.5_EB*M%DZ(KKG)
            XB(6) = M%Z(NINT(ZK)) + 0.5_EB*M%DZ(KKG)
         ELSE
            XB = 0._EB
         ENDIF
      ENDIF

      ! Force MASS MEAN spatial statistic for FAVRE average

      IF ( (TEMPORAL_STATISTIC=='FAVRE AVERAGE' .OR. TEMPORAL_STATISTIC=='FAVRE RMS') &
         .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
         SPATIAL_STATISTIC='MASS MEAN'
         CALL SEARCH_OTHER_MESHES(XYZ(1),XYZ(2),XYZ(3),NM,IIG,JJG,KKG,XI,YJ,ZK)
         IF (IIG>0 .AND. JJG>0 .AND. KKG>0) THEN
            M => MESHES(NM)
            XB(1) = M%X(IIG-1)
            XB(2) = M%X(IIG)
            XB(3) = M%Y(JJG-1)
            XB(4) = M%Y(JJG)
            XB(5) = M%Z(KKG-1)
            XB(6) = M%Z(KKG)
         ELSE
            XB = 0._EB
         ENDIF
      ENDIF

      ! Determine which mesh the device is in

      BAD = .TRUE.
      MESH_LOOP: DO NM=1,NMESHES
         M=>MESHES(NM)
         IF (DB/='null') THEN
            BAD = .FALSE.
            EXIT MESH_LOOP
         ENDIF
         IF (XYZ(1)>=M%XS .AND. XYZ(1)<=M%XF .AND. XYZ(2)>=M%YS .AND. XYZ(2)<=M%YF .AND. XYZ(3)>=M%ZS .AND. XYZ(3)<=M%ZF) THEN
            IF (ABS(XYZ(1)-M%XS)<TWO_EPSILON_EB .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               IF (IOR==-1) THEN
                  CYCLE MESH_LOOP
               ELSE
                  XYZ(1) = XYZ(1) + 0.01_EB*M%DXI
               ENDIF
            ENDIF
            IF (ABS(XYZ(1)-M%XF)<TWO_EPSILON_EB .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               IF (IOR==1) THEN
                  CYCLE MESH_LOOP
               ELSE
                  XYZ(1) = XYZ(1) - 0.01_EB*M%DXI
               ENDIF
            ENDIF
            IF (ABS(XYZ(2)-M%YS)<TWO_EPSILON_EB .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               IF (IOR==-2) THEN
                  CYCLE MESH_LOOP
               ELSE
                  XYZ(2) = XYZ(2) + 0.01_EB*M%DETA
               ENDIF
            ENDIF
            IF (ABS(XYZ(2)-M%YF)<TWO_EPSILON_EB .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               IF (IOR==2) THEN
                  CYCLE MESH_LOOP
               ELSE
                  XYZ(2) = XYZ(2) - 0.01_EB*M%DETA
               ENDIF
            ENDIF
            IF (ABS(XYZ(3)-M%ZS)<TWO_EPSILON_EB .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               IF (IOR==-3) THEN
                  CYCLE MESH_LOOP
               ELSE
                  XYZ(3) = XYZ(3) + 0.01_EB*M%DZETA
               ENDIF
            ENDIF
            IF (ABS(XYZ(3)-M%ZF)<TWO_EPSILON_EB .AND. SPATIAL_STATISTIC/='INTERPOLATION') THEN
               IF (IOR==3) THEN
                  CYCLE MESH_LOOP
               ELSE
                  XYZ(3) = XYZ(3) - 0.01_EB*M%DZETA
               ENDIF
            ENDIF
            MESH_NUMBER = NM
            BAD = .FALSE.
            IF (PROCESS(NM)==MY_RANK) MESH_DEVICE(NM) = 1
            EXIT MESH_LOOP
         ENDIF
      ENDDO MESH_LOOP

      ! Make sure there is either a QUANTITY or PROP_ID for the DEVICE

      IF (QUANTITY=='null' .AND. PROP_ID=='null') THEN
         WRITE(MESSAGE,'(3A)')  'ERROR(886): DEVC ',TRIM(ID),' must have either an output QUANTITY or PROP_ID.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (BAD) THEN
         IF (CTRL_ID/='null' .OR. DUCT_ID/='null' .OR. NODE_ID(1)/='null' .OR. INIT_ID/='null' .OR. CELL_L>-1._EB) THEN
            XYZ(1) = MESHES(1)%XS
            XYZ(2) = MESHES(1)%YS
            XYZ(3) = MESHES(1)%ZS
            MESH_NUMBER = 1
            IF (INIT_ID/='null') THEN
               MESH_DEVICE = 1  ! This is the case where a DEVC is assigned to particles specified on an INIT line
            ELSE
               IF (PROCESS(1)==MY_RANK) MESH_DEVICE(1) = 1  ! This refers to HVAC or control logic
            ENDIF
         ELSE
            WRITE(MESSAGE,'(A,A,A)') 'WARNING: DEVC ',TRIM(ID),' is not within any mesh.'
            IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
            CYCLE READ_DEVC_LOOP
         ENDIF
      ENDIF

      ! Don't print out clocks

      IF (QUANTITY=='TIME' .AND. NO_UPDATE_DEVC_ID=='null' .AND. NO_UPDATE_CTRL_ID=='null' ) OUTPUT = .FALSE.

      ! Determine if the DEVC is a TIME or LINE device

      IF (.NOT.LINE_DEVICE .AND. OUTPUT)  N_DEVC_TIME = N_DEVC_TIME + 1
      IF (LINE_DEVICE .AND. I_POINT==1)   N_DEVC_LINE = N_DEVC_LINE + 1

      ! Assign properties to the DEVICE array

      N_DEVC = N_DEVC + 1

      DV => DEVICE(N_DEVC)

      MESH_DEVICE_ARRAY(1:NMESHES,N_DEVC) = MESH_DEVICE(1:NMESHES)

      IF (QUANTITY2/='null') DV%N_QUANTITY = 2

      ALLOCATE(DV%QUANTITY(DV%N_QUANTITY))       ; DV%QUANTITY = 'null'
      ALLOCATE(DV%QUANTITY_INDEX(DV%N_QUANTITY)) ; DV%QUANTITY_INDEX = 0
      ALLOCATE(DV%I(DV%N_QUANTITY))
      ALLOCATE(DV%J(DV%N_QUANTITY))
      ALLOCATE(DV%K(DV%N_QUANTITY))

      DV%RELATIVE          = RELATIVE
      DV%CONVERSION_ADDEND = CONVERSION_ADDEND
      DV%CONVERSION_FACTOR = CONVERSION_FACTOR
      DV%COORD_FACTOR      = COORD_FACTOR
      DV%DEPTH             = DEPTH
      DV%IOR               = IOR
      IF (POINTS>1 .AND. .NOT.LINE_DEVICE) THEN
         WRITE(DV%ID,'(A,A,I0)') TRIM(ID),'-',I_POINT
      ELSE
         DV%ID             = ID
      ENDIF
      IF (LINE_DEVICE) DV%LINE = N_DEVC_LINE
      DV%POINT             = I_POINT
      DV%N_POINTS          = POINTS
      DV%MESH              = MESH_NUMBER
      DV%ORDINAL           = NN
      DV%ORIENTATION_INDEX = ORIENTATION_INDEX
      DV%PROP_ID           = PROP_ID
      DV%DEVC_ID           = DEVC_ID
      DV%CTRL_ID           = CTRL_ID
      DV%SURF_ID           = SURF_ID
      DV%PART_ID           = PART_ID
      DV%MATL_ID           = MATL_ID
      DV%SPEC_ID           = SPEC_ID
      DV%DUCT_ID           = DUCT_ID
      DV%INIT_ID           = INIT_ID
      DV%MOVE_ID           = MOVE_ID
      DV%NODE_ID           = NODE_ID
      DV%REAC_ID           = REAC_ID
      DV%CELL_L            = CELL_L
      DV%TIME_PERIOD       = TIME_PERIOD
      DV%N_INTERVALS       = N_INTERVALS
      DV%QUANTITY(1)       = QUANTITY
      IF (QUANTITY2/='null') DV%QUANTITY(2) = QUANTITY2
      DV%ROTATION          = ROTATION*TWOPI/360._EB
      DV%SETPOINT          = SETPOINT
      DV%LATCH             = LATCH
      DV%OUTPUT            = OUTPUT
      DV%ABSOLUTE_VALUE    = ABSOLUTE_VALUE
      DV%TRIP_DIRECTION    = TRIP_DIRECTION
      DV%INITIAL_STATE     = INITIAL_STATE
      DV%CURRENT_STATE     = INITIAL_STATE
      DV%PRIOR_STATE       = INITIAL_STATE
      DV%FLOWRATE          = FLOWRATE
      DV%BYPASS_FLOWRATE   = BYPASS_FLOWRATE
      DV%SMOOTHING_FACTOR  = SMOOTHING_FACTOR
      DV%SPATIAL_STATISTIC = SPATIAL_STATISTIC
      DV%TEMPORAL_STATISTIC= TEMPORAL_STATISTIC
      DV%STATISTICS_END    = STATISTICS_END
      DV%STATISTICS_START  = STATISTICS_START
      DV%SURF_INDEX        = 0
      DV%UNITS             = UNITS
      DV%XYZ_UNITS         = XYZ_UNITS
      DV%DELAY             = DELAY / TIME_SHRINK_FACTOR
      DV%X1                = XB(1)
      DV%X2                = XB(2)
      DV%Y1                = XB(3)
      DV%Y2                = XB(4)
      DV%Z1                = XB(5)
      DV%Z2                = XB(6)
      DV%X0                = XBP(1)
      DV%Y0                = XBP(3)
      DV%Z0                = XBP(5)
      IF (ABS(DV%X1-DV%X2)<=SPACING(DV%X2) .AND. DV%IOR==0) DV%IOR_ASSUMED = 1
      IF (ABS(DV%Y1-DV%Y2)<=SPACING(DV%Y2) .AND. DV%IOR==0) DV%IOR_ASSUMED = 2
      IF (ABS(DV%Z1-DV%Z2)<=SPACING(DV%Z2) .AND. DV%IOR==0) DV%IOR_ASSUMED = 3
      DV%X                 = XYZ(1)
      DV%Y                 = XYZ(2)
      DV%Z                 = XYZ(3)
      IF (X_ID=='null') X_ID = TRIM(ID)//'-x'
      IF (Y_ID=='null') Y_ID = TRIM(ID)//'-y'
      IF (Z_ID=='null') Z_ID = TRIM(ID)//'-z'
      DV%D_ID              = D_ID
      DV%R_ID              = R_ID
      DV%X_ID              = X_ID
      DV%Y_ID              = Y_ID
      DV%Z_ID              = Z_ID
      DV%DRY               = DRY
      DV%VELO_INDEX        = VELO_INDEX
      DV%PIPE_INDEX        = PIPE_INDEX
      DV%NO_UPDATE_DEVC_ID = NO_UPDATE_DEVC_ID
      DV%NO_UPDATE_CTRL_ID = NO_UPDATE_CTRL_ID
      DV%QUANTITY_RANGE    = QUANTITY_RANGE
      DV%HIDE_COORDINATES  = HIDE_COORDINATES
      IF (NORM2(ORIENTATION)<1.E5_EB) THEN
         DV%OVEC = ORIENTATION/NORM2(ORIENTATION)
      ELSE
         DV%OVEC = (/0._EB,0._EB,0._EB/)
      ENDIF
      IF (NORM2(FORCE_DIRECTION)<1.E5_EB) THEN
         DV%DFVEC = FORCE_DIRECTION/NORM2(FORCE_DIRECTION)
      ELSE
         DV%DFVEC = (/0._EB,0._EB,0._EB/)
      ENDIF

      IF (LINE_DEVICE) THEN
         IF (.NOT.DV%HIDE_COORDINATES) THEN
            IF (ABS(XBP(1)-XBP(2))> SPACING(XBP(2)) .AND. ABS(XBP(3)-XBP(4))<=SPACING(XBP(4)) .AND. &
               ABS(XBP(5)-XBP(6))<=SPACING(XBP(6))) DV%LINE_COORD_CODE = 1
            IF (ABS(XBP(1)-XBP(2))<=SPACING(XBP(2)) .AND. ABS(XBP(3)-XBP(4))> SPACING(XBP(4)) .AND. &
               ABS(XBP(5)-XBP(6))<=SPACING(XBP(6))) DV%LINE_COORD_CODE = 2
            IF (ABS(XBP(1)-XBP(2))<=SPACING(XBP(2)) .AND. ABS(XBP(3)-XBP(4))<=SPACING(XBP(4)) .AND. &
               ABS(XBP(5)-XBP(6))> SPACING(XBP(6))) DV%LINE_COORD_CODE = 3
            IF (ABS(XBP(1)-XBP(2))> SPACING(XBP(2)) .AND. ABS(XBP(3)-XBP(4))> SPACING(XBP(4)) .AND. &
               ABS(XBP(5)-XBP(6))<=SPACING(XBP(6))) DV%LINE_COORD_CODE = 12
            IF (ABS(XBP(1)-XBP(2))> SPACING(XBP(2)) .AND. ABS(XBP(3)-XBP(4))<=SPACING(XBP(4)) .AND. &
               ABS(XBP(5)-XBP(6))> SPACING(XBP(6))) DV%LINE_COORD_CODE = 13
            IF (ABS(XBP(1)-XBP(2))<=SPACING(XBP(2)) .AND. ABS(XBP(3)-XBP(4))> SPACING(XBP(4)) .AND. &
               ABS(XBP(5)-XBP(6))> SPACING(XBP(6))) DV%LINE_COORD_CODE = 23
            IF (DV%R_ID/='null') DV%LINE_COORD_CODE = 4  ! Special case where radial coordinates are requested
            IF (DV%D_ID/='null') DV%LINE_COORD_CODE = 5  ! Special case where distance from origin is requested
         ELSE
            DV%LINE_COORD_CODE = 0
         ENDIF
      ENDIF

      ! Special case for QUANTITY='RADIATIVE HEAT FLUX GAS' or 'ADIABATIC SURFACE TEMPERATURE GAS'.
      ! Save information to create INIT line.

      IF (DV%QUANTITY(1)=='RADIATIVE HEAT FLUX GAS' .OR. &
          DV%QUANTITY(1)=='GAUGE HEAT FLUX GAS' .OR. &
          DV%QUANTITY(1)=='RADIANCE' .OR. &
          DV%QUANTITY(1)=='ADIABATIC SURFACE TEMPERATURE GAS') THEN
         IF (DV%ORIENTATION_INDEX==0) THEN
            WRITE(MESSAGE,'(3A)')  'ERROR(887): DEVC ',TRIM(ID),' must have an ORIENTATION.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         DV%INIT_ID = DV%ID
         TARGET_PARTICLES_INCLUDED = .TRUE.
         IF (DV%POINT==1 .OR. TIME_HISTORY) THEN
            DV%POINT = 1
            N_INIT_RESERVED = N_INIT_RESERVED + 1
            INIT_RESERVED(N_INIT_RESERVED)%DEVC_INDEX = N_DEVC
            INIT_RESERVED(N_INIT_RESERVED)%N_PARTICLES = POINTS
         ENDIF
      ENDIF

      ! Miscellaneous actions taken based on specific device attributes

      IF (TRIM(DV%QUANTITY(1)) == 'CHEMISTRY SUBITERATIONS') OUTPUT_CHEM_IT = .TRUE.
      IF (TRIM(DV%QUANTITY(1)) == 'REAC SOURCE TERM' .OR. TRIM(DV%QUANTITY(1)) == 'HRRPUV REAC') REAC_SOURCE_CHECK=.TRUE.

      IF (TRIM(QUANTITY)=='CUTCELL VELOCITY DIVERGENCE') THEN
         STORE_CUTCELL_DIVERGENCE   = .TRUE.
         STORE_CARTESIAN_DIVERGENCE = .TRUE.
      ENDIF
      IF (TRIM(QUANTITY)=='CARTESIAN VELOCITY DIVERGENCE') STORE_CARTESIAN_DIVERGENCE = .TRUE.
      IF (TRIM(QUANTITY)=='PRESSURE POISSON RESIDUAL') STORE_PRESSURE_POISSON_RESIDUAL = .TRUE.

      IF (DV%SPATIAL_STATISTIC(1:3)=='MIN') MIN_DEVICES_EXIST = .TRUE.
      IF (DV%SPATIAL_STATISTIC(1:3)=='MAX') MAX_DEVICES_EXIST = .TRUE.

      IF (DV%TEMPORAL_STATISTIC(1:3)=='MIN' .OR. DV%TEMPORAL_STATISTIC(1:3)=='MAX') THEN
         IF (DV%TIME_PERIOD>0._EB) THEN
            IF (DV%N_INTERVALS<0) DV%N_INTERVALS = 10
         ELSE
            DV%N_INTERVALS = 1
         ENDIF
         IF (DV%TEMPORAL_STATISTIC(1:3)=='MIN') THEN
            ALLOCATE(DV%TIME_MIN_VALUE(DV%N_INTERVALS))
            DV%TIME_MIN_VALUE = 1.E20_EB
         ELSE
            ALLOCATE(DV%TIME_MAX_VALUE(DV%N_INTERVALS))
            DV%TIME_MAX_VALUE = -1.E20_EB
         ENDIF
      ENDIF

   ENDDO POINTS_LOOP

ENDDO READ_DEVC_LOOP

ALLOCATE (DEVC_PIPE_OPERATING(MAXVAL(DEVICE%PIPE_INDEX)))
DEVC_PIPE_OPERATING = 0

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

! Loop through the meshes and record info for only those devices in the mesh

DEVICE_LOOP: DO N=1,N_DEVC

   DV => DEVICE(N)
   ALLOCATE(DV%SUBDEVICE_INDEX(NMESHES),STAT=IZERO) ;   CALL ChkMemErr('READ','SUBDEVICE_INDEX',IZERO)
   DV%SUBDEVICE_INDEX = 0
   DV%N_SUBDEVICES = SUM(MESH_DEVICE_ARRAY(:,N))
   IF (DV%N_SUBDEVICES==0) CYCLE DEVICE_LOOP

   ALLOCATE(DV%SUBDEVICE(DV%N_SUBDEVICES),STAT=IZERO) ; CALL ChkMemErr('READ','SUBDEVICE',IZERO)

   NN = 0

   MESH_LOOP_2: DO NM=1,NMESHES

      IF (MESH_DEVICE_ARRAY(NM,N)==0) CYCLE MESH_LOOP_2

      NN = NN + 1
      DV%SUBDEVICE_INDEX(NM) = NN
      SDV => DV%SUBDEVICE(NN)
      SDV%MESH = NM

      M=>MESHES(NM)

      IF (DV%X1>-1.E5_EB) THEN
         SDV%X1 = MAX(DV%X1,M%XS)
         SDV%X2 = MIN(DV%X2,M%XF)
         SDV%Y1 = MAX(DV%Y1,M%YS)
         SDV%Y2 = MIN(DV%Y2,M%YF)
         SDV%Z1 = MAX(DV%Z1,M%ZS)
         SDV%Z2 = MIN(DV%Z2,M%ZF)
         SDV%I1 =   FLOOR( GINV(SDV%X1-M%XS,1,NM)*M%RDXI * ONE_P_EPS ) + 1
         SDV%I2 = CEILING( GINV(SDV%X2-M%XS,1,NM)*M%RDXI * ONE_M_EPS )
         SDV%J1 =   FLOOR( GINV(SDV%Y1-M%YS,2,NM)*M%RDETA * ONE_P_EPS ) + 1
         SDV%J2 = CEILING( GINV(SDV%Y2-M%YS,2,NM)*M%RDETA * ONE_M_EPS )
         SDV%K1 =   FLOOR( GINV(SDV%Z1-M%ZS,3,NM)*M%RDZETA * ONE_P_EPS ) + 1
         SDV%K2 = CEILING( GINV(SDV%Z2-M%ZS,3,NM)*M%RDZETA * ONE_M_EPS )
         IF (SDV%I1>SDV%I2) SDV%I1 = SDV%I2
         IF (SDV%J1>SDV%J2) SDV%J1 = SDV%J2
         IF (SDV%K1>SDV%K2) SDV%K1 = SDV%K2
      ENDIF

   ENDDO MESH_LOOP_2
ENDDO DEVICE_LOOP

DEALLOCATE(MESH_DEVICE)
DEALLOCATE(MESH_DEVICE_ARRAY)

CONTAINS

SUBROUTINE SET_DEVC_DEFAULTS

ABSOLUTE_VALUE   = .FALSE.
RELATIVE         = .FALSE.
CONVERSION_ADDEND = 0._EB
CONVERSION_FACTOR = 1._EB
COORD_FACTOR     = 1._EB
DEPTH            = 0._EB
IOR              = 0
ID               = 'null'
ORIENTATION(1:3) = (/0._EB,0._EB,-1.E6_EB/)
FORCE_DIRECTION(1:3) = (/0._EB,0._EB,-1.E6_EB/)
PROP_ID          = 'null'
CTRL_ID          = 'null'
DEVC_ID          = 'null'
SURF_ID          = 'null'
PART_ID          = 'null'
MATL_ID          = 'null'
SPEC_ID          = 'null'
DUCT_ID          = 'null'
INIT_ID          = 'null'
MOVE_ID          = 'null'
NODE_ID          = 'null'
REAC_ID          = 'null'
CELL_L           = -1._EB
FLOWRATE         = 0._EB
DELAY            = 0._EB
BYPASS_FLOWRATE  = 0._EB
QUANTITY         = 'null'
QUANTITY2        = 'null'
ROTATION         = 0._EB
XB(1)            = -1.E6_EB
XB(2)            =  1.E6_EB
XB(3)            = -1.E6_EB
XB(4)            =  1.E6_EB
XB(5)            = -1.E6_EB
XB(6)            =  1.E6_EB
XBP              = XB
DB               = 'null'
DX               = 0._EB
DY               = 0._EB
DZ               = 0._EB
INITIAL_STATE    = .FALSE.
LATCH            = .TRUE.
OUTPUT           = .TRUE.
POINTS           = 1
POINTS_ARRAY_X   = -1.E7
POINTS_ARRAY_Y   = -1.E7
POINTS_ARRAY_Z   = -1.E7
SETPOINT         = 1.E20_EB
SMOOTHING_FACTOR = 0._EB
STATISTICS       = 'null'
SPATIAL_STATISTIC= 'null'
TEMPORAL_STATISTIC= 'null'
STATISTICS_START = -1.E20_EB
STATISTICS_END   =  1.E20_EB
TRIP_DIRECTION   = 1
TIME_AVERAGED    = .TRUE.
TIME_HISTORY     = .FALSE.
UNITS            = 'null'
XYZ_UNITS        = 'm'
VELO_INDEX       = 0
XYZ              = -1.E6_EB
D_ID             = 'null'
R_ID             = 'null'
X_ID             = 'null'
Y_ID             = 'null'
Z_ID             = 'null'
HIDE_COORDINATES = .FALSE.
DRY              = .FALSE.
PIPE_INDEX       = 1
NO_UPDATE_DEVC_ID= 'null'
NO_UPDATE_CTRL_ID= 'null'
QUANTITY_RANGE(1)= -1.E50_EB
QUANTITY_RANGE(2)=  1.E50_EB
N_INTERVALS      = -1
TIME_PERIOD      = -1._EB

END SUBROUTINE SET_DEVC_DEFAULTS


!> \brief Check if the STATISTIC name is on the list
!> \param STAT_NAME Name of the statistic
!> \param TYPE_CODE 1 means STATISTICS, 2 means TEMPORAL_STATISTIC, 3 means SPATIAL_STATISTIC
!> \param ERROR_CODE Returns 1 if the name is not found

SUBROUTINE CHECK_STATISTIC_NAME(STAT_NAME,TYPE_CODE,ERROR_CODE)

CHARACTER(LABEL_LENGTH), INTENT(IN) :: STAT_NAME
INTEGER, INTENT(IN)  :: TYPE_CODE
INTEGER, INTENT(OUT) :: ERROR_CODE
LOGICAL :: NO_TEMPORAL_STATISTIC_FOUND,NO_SPATIAL_STATISTIC_FOUND

ERROR_CODE = 0
NO_TEMPORAL_STATISTIC_FOUND = .FALSE.
NO_SPATIAL_STATISTIC_FOUND  = .FALSE.

IF (TYPE_CODE==1 .OR. TYPE_CODE==2) THEN
   SELECT CASE(STAT_NAME)
      CASE('null')
      CASE('INSTANT VALUE')   ; TEMPORAL_STATISTIC = 'INSTANT VALUE'
      CASE('TIME AVERAGE')    ; TEMPORAL_STATISTIC = 'TIME AVERAGE'
      CASE('FAVRE AVERAGE')   ; TEMPORAL_STATISTIC = 'FAVRE AVERAGE'
      CASE('RUNNING AVERAGE') ; TEMPORAL_STATISTIC = 'RUNNING AVERAGE'
      CASE('TIME INTEGRAL')   ; TEMPORAL_STATISTIC = 'TIME INTEGRAL'
      CASE('MAX')             ; TEMPORAL_STATISTIC = 'MAX'
      CASE('MIN')             ; TEMPORAL_STATISTIC = 'MIN'
      CASE('MAX TIME')        ; TEMPORAL_STATISTIC = 'MAX TIME'
      CASE('MIN TIME')        ; TEMPORAL_STATISTIC = 'MIN TIME'
      CASE('RMS')             ; TEMPORAL_STATISTIC = 'RMS'
      CASE('FAVRE RMS')       ; TEMPORAL_STATISTIC = 'FAVRE RMS'
      CASE('COV')             ; TEMPORAL_STATISTIC = 'COV'
      CASE('CORRCOEF')        ; TEMPORAL_STATISTIC = 'CORRCOEF'
      CASE('SMOOTHED')        ; TEMPORAL_STATISTIC = 'SMOOTHED'
      CASE DEFAULT
         IF (TYPE_CODE==2) THEN
            WRITE(MESSAGE,'(3A)')  'ERROR(888): DEVC ',TRIM(ID),' TEMPORAL_STATISTIC is not recognized.'
            ERROR_CODE = 1
            RETURN
         ELSE
            NO_TEMPORAL_STATISTIC_FOUND = .TRUE.
         ENDIF
   END SELECT
ENDIF

IF (TYPE_CODE==1 .OR. TYPE_CODE==3) THEN
   SELECT CASE(STAT_NAME)
      CASE('null')
      CASE('AREA')             ; SPATIAL_STATISTIC = 'AREA'
      CASE('AREA INTEGRAL')    ; SPATIAL_STATISTIC = 'AREA INTEGRAL'
      CASE('VOLUME INTEGRAL')  ; SPATIAL_STATISTIC = 'VOLUME INTEGRAL'
      CASE('VOLUME MEAN')      ; SPATIAL_STATISTIC = 'VOLUME MEAN'
      CASE('VOLUME')           ; SPATIAL_STATISTIC = 'VOLUME'
      CASE('SURFACE INTEGRAL') ; SPATIAL_STATISTIC = 'SURFACE INTEGRAL'
      CASE('SURFACE AREA')     ; SPATIAL_STATISTIC = 'SURFACE AREA'
      CASE('SUM')              ; SPATIAL_STATISTIC = 'SUM'
      CASE('MASS INTEGRAL')    ; SPATIAL_STATISTIC = 'MASS INTEGRAL'
      CASE('MASS MEAN')        ; SPATIAL_STATISTIC = 'MASS MEAN'
      CASE('CENTROID X')       ; SPATIAL_STATISTIC = 'CENTROID X'
      CASE('CENTROID Y')       ; SPATIAL_STATISTIC = 'CENTROID Y'
      CASE('CENTROID Z')       ; SPATIAL_STATISTIC = 'CENTROID Z'
      CASE('MASS')             ; SPATIAL_STATISTIC = 'MASS'
      CASE('MAX')              ; SPATIAL_STATISTIC = 'MAX'
      CASE('MAXLOC X')         ; SPATIAL_STATISTIC = 'MAXLOC X'
      CASE('MAXLOC Y')         ; SPATIAL_STATISTIC = 'MAXLOC Y'
      CASE('MAXLOC Z')         ; SPATIAL_STATISTIC = 'MAXLOC Z'
      CASE('MIN')              ; SPATIAL_STATISTIC = 'MIN'
      CASE('MINLOC X')         ; SPATIAL_STATISTIC = 'MINLOC X'
      CASE('MINLOC Y')         ; SPATIAL_STATISTIC = 'MINLOC Y'
      CASE('MINLOC Z')         ; SPATIAL_STATISTIC = 'MINLOC Z'
      CASE('MEAN')             ; SPATIAL_STATISTIC = 'MEAN'
      CASE('INTERPOLATION')    ; SPATIAL_STATISTIC = 'INTERPOLATION'
      CASE DEFAULT
         IF (TYPE_CODE==3) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(889): DEVC ',TRIM(ID),' SPATIAL_STATISTIC is not recognized.'
            ERROR_CODE = 1
            RETURN
         ELSE
            NO_SPATIAL_STATISTIC_FOUND = .TRUE.
         ENDIF
   END SELECT
ENDIF

IF (NO_TEMPORAL_STATISTIC_FOUND .AND. NO_SPATIAL_STATISTIC_FOUND) THEN
   WRITE(MESSAGE,'(3A)')  'ERROR(890): DEVC ',TRIM(ID),' STATISTICS is not recognized.'
   ERROR_CODE = 1
ENDIF

END SUBROUTINE CHECK_STATISTIC_NAME

END SUBROUTINE READ_DEVC


!> \brief Read the ConTRoL namelist lines

SUBROUTINE READ_CTRL

USE CONTROL_VARIABLES
USE MATH_FUNCTIONS, ONLY : GET_RAMP_INDEX

LOGICAL :: INITIAL_STATE,LATCH,CONTROL_FORCE(3)
INTEGER :: N,NC,TRIP_DIRECTION
REAL(EB) :: SETPOINT(2), DELAY,CONSTANT,PROPORTIONAL_GAIN,INTEGRAL_GAIN,DIFFERENTIAL_GAIN,TARGET_VALUE,PERCENTILE
CHARACTER(LABEL_LENGTH) :: ID,FUNCTION_TYPE,INPUT_ID(MAX_INPUT_ID),RAMP_ID,ON_BOUND
TYPE (CONTROL_TYPE), POINTER :: CF=>NULL()
NAMELIST /CTRL/  CONSTANT,CONTROL_FORCE,DELAY,DIFFERENTIAL_GAIN,FUNCTION_TYPE,ID,INITIAL_STATE,&
                 INPUT_ID,INTEGRAL_GAIN,LATCH,N,ON_BOUND,PERCENTILE,PROPORTIONAL_GAIN,RAMP_ID,&
                 SETPOINT,TARGET_VALUE,TRIP_DIRECTION

N_CTRL = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_CTRL_LOOP: DO
   CALL CHECKREAD('CTRL',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_CTRL_LOOP
   READ(LU_INPUT,NML=CTRL,END=11,ERR=12,IOSTAT=IOS)
   N_CTRL = N_CTRL + 1
   12 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with CTRL number ',N_CTRL+1
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO COUNT_CTRL_LOOP
11 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_CTRL==0) RETURN

! Allocate CONTROL array and set initial values of all to 0

ALLOCATE(CONTROL(N_CTRL),STAT=IZERO)
CALL ChkMemErr('READ','CONTROL',IZERO)

! Read in the CTRL lines

READ_CTRL_LOOP: DO NC=1,N_CTRL

   CALL CHECKREAD('CTRL',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_CTRL_LOOP
   CALL SET_CTRL_DEFAULTS
   READ(LU_INPUT,CTRL)

   ! Make sure there is either a FUNCTION_TYPE type for the CTRL

   IF (FUNCTION_TYPE=='null') THEN
      WRITE(MESSAGE,'(A,I0,A)')  'ERROR(901): CTRL ',NC,' must have a FUNCTION_TYPE.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! Assign properties to the CONTROL array

   CF => CONTROL(NC)
   CF%CONSTANT        = CONSTANT
   CF%ID              = ID
   CF%LATCH           = LATCH
   CF%INITIAL_STATE   = INITIAL_STATE
   CF%CURRENT_STATE   = INITIAL_STATE
   CF%PRIOR_STATE     = INITIAL_STATE
   CF%SETPOINT        = SETPOINT
   CF%DELAY           = DELAY / TIME_SHRINK_FACTOR
   CF%RAMP_ID         = RAMP_ID
   CF%N               = N
   CF%INPUT_ID        = INPUT_ID
   CF%TRIP_DIRECTION  = TRIP_DIRECTION
   CF%PROPORTIONAL_GAIN =PROPORTIONAL_GAIN
   CF%INTEGRAL_GAIN = INTEGRAL_GAIN
   CF%DIFFERENTIAL_GAIN = DIFFERENTIAL_GAIN
   CF%TARGET_VALUE = TARGET_VALUE
   CF%PERCENTILE   = PERCENTILE
   CF%CONTROL_FORCE = CONTROL_FORCE
   IF (ANY(CF%CONTROL_FORCE)) CTRL_DIRECT_FORCE=.TRUE.
   IF (ON_BOUND=='UPPER') THEN
      CF%ON_BOUND = 1
   ELSE
      CF%ON_BOUND = -1
   ENDIF
   !Assign control index
   SELECT CASE(FUNCTION_TYPE)
      CASE('ALL')
         CF%CONTROL_INDEX = AND_GATE
      CASE('ANY')
         CF%CONTROL_INDEX = OR_GATE
      CASE('ONLY')
         CF%CONTROL_INDEX = XOR_GATE
         CF%N = 1
      CASE('AT_LEAST')
         CF%CONTROL_INDEX = X_OF_N_GATE
      CASE('EXTERNAL')
         CF%CONTROL_INDEX = CF_EXTERNAL
         CF%LATCH = .FALSE.
         READ_EXTERNAL = .TRUE.
      CASE('TIME_DELAY')
         CF%CONTROL_INDEX = TIME_DELAY
      CASE('DEADBAND')
         CF%CONTROL_INDEX = DEADBAND
      CASE('CUSTOM')
         CF%CONTROL_INDEX = CUSTOM
         CALL GET_RAMP_INDEX(RAMP_ID,'CONTROL',CF%RAMP_INDEX)
         CF%LATCH = .FALSE.
      CASE('KILL')
         CF%CONTROL_INDEX = KILL
      CASE('RESTART')
         CF%CONTROL_INDEX = CORE_DUMP
      CASE('SUM')
         CF%CONTROL_INDEX = CF_SUM
      CASE('SUBTRACT')
         CF%CONTROL_INDEX = CF_SUBTRACT
      CASE('MULTIPLY')
         CF%CONTROL_INDEX = CF_MULTIPLY
      CASE('DIVIDE')
         CF%CONTROL_INDEX = CF_DIVIDE
      CASE('POWER')
         CF%CONTROL_INDEX = CF_POWER
      CASE('EXP')
         CF%CONTROL_INDEX = CF_EXP
      CASE('LOG')
         CF%CONTROL_INDEX = CF_LOG
      CASE('SIN')
         CF%CONTROL_INDEX = CF_SIN
      CASE('COS')
         CF%CONTROL_INDEX = CF_COS
      CASE('ASIN')
         CF%CONTROL_INDEX = CF_ASIN
      CASE('ACOS')
         CF%CONTROL_INDEX = CF_ACOS
      CASE('ATAN')
         CF%CONTROL_INDEX = CF_ATAN
      CASE('MIN')
         CF%CONTROL_INDEX = CF_MIN
      CASE('MAX')
         CF%CONTROL_INDEX = CF_MAX
      CASE('ABS')
         CF%CONTROL_INDEX = CF_ABS
      CASE('PERCENTILE')
         CF%CONTROL_INDEX = CF_PERCENTILE
      CASE('PID')
         CF%CONTROL_INDEX = CF_PID
         IF (CF%TARGET_VALUE<-1.E30_EB) THEN
            WRITE(MESSAGE,'(A,I0,A)') 'ERROR(902): CTRL ',NC,', PID controller must be given a TARGET_VALUE.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      CASE DEFAULT
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(903): CTRL ',NC,' FUNCTION_TYPE not recognized.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
   END SELECT

   IF (CF%CONTROL_INDEX < 100) THEN
      WRITE_DEVC_CTRL = .TRUE.
   ELSE
      IF (SETPOINT(1) <1.E30_EB) WRITE_DEVC_CTRL = .TRUE.
   ENDIF

ENDDO READ_CTRL_LOOP
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (READ_EXTERNAL .AND. EXTERNAL_FILENAME=='null') THEN
   WRITE(MESSAGE,'(A,I0,A)') 'ERROR(904): CTRL ',NC,' FUNCTION_TYPE=EXTERNAL but no EXTERNAL_FILENAME given on MISC.'
   CALL SHUTDOWN(MESSAGE) ; RETURN
ENDIF

CONTAINS

SUBROUTINE SET_CTRL_DEFAULTS
   CONSTANT      = -9.E30_EB
   ID            = 'null'
   LATCH         = .TRUE.
   INITIAL_STATE = .FALSE.
   CONTROL_FORCE = .FALSE.
   SETPOINT      = 1.E30_EB
   DELAY         = 0._EB
   FUNCTION_TYPE = 'null'
   RAMP_ID       = 'null'
   INPUT_ID      = 'null'
   ON_BOUND      = 'LOWER'
   N             = 1
   TRIP_DIRECTION = 1
   PROPORTIONAL_GAIN = 1._EB
   INTEGRAL_GAIN     = 0._EB
   DIFFERENTIAL_GAIN = 0._EB
   TARGET_VALUE      = 0._EB
   PERCENTILE        = 0.95

END SUBROUTINE SET_CTRL_DEFAULTS

END SUBROUTINE READ_CTRL


!> \brief Process the CONTROL function parameters

SUBROUTINE PROC_CTRL

USE CONTROL_VARIABLES
USE DEVICE_VARIABLES, ONLY: DEVICE_TYPE,N_DEVC,DEVICE
LOGICAL :: CONSTANT_SPECIFIED,TSF_WARNING=.FALSE.
INTEGER :: NC,NN,NNN,DEVC_INDEX
REAL(EB) :: DX,DY,DZ
TYPE (CONTROL_TYPE), POINTER :: CF
TYPE (DEVICE_TYPE), POINTER :: DV

PROC_CTRL_LOOP: DO NC = 1, N_CTRL

   CF => CONTROL(NC)
   CONSTANT_SPECIFIED = .FALSE.
   IF (CF%CONTROL_INDEX== TIME_DELAY) TSF_WARNING=.TRUE.

   ! setup input array

   CF%N_INPUTS = 0
   INPUT_COUNT: DO NN=1,MAX_INPUT_ID
      IF (CF%INPUT_ID(NN)=='null') EXIT INPUT_COUNT
   END DO INPUT_COUNT
   CF%N_INPUTS=NN-1
   IF (CF%N_INPUTS==0 .AND. CF%CONTROL_INDEX /= CF_EXTERNAL) THEN
      WRITE(MESSAGE,'(3A)')  'ERROR(905): CTRL ',TRIM(CF%ID),' must have at least one input.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   SELECT CASE (CF%CONTROL_INDEX)
      CASE (1:50,101:199)
         IF (CF%N_INPUTS /= 1) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(906): CTRL ',TRIM(CF%ID),' must have only one input.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (CF%CONTROL_INDEX/=CUSTOM .AND. ANY(CF%INPUT_ID=='CONSTANT')) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(907): CTRL ',TRIM(CF%ID),' cannot use a CONSTANT INPUT_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      CASE (51:54,56:100)
         IF (CF%N_INPUTS < 1) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(905): CTRL ',TRIM(CF%ID),' must have at least one input.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      CASE (301:399)
         IF (CF%N_INPUTS < 2) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(908): CTRL ',TRIM(CF%ID),' must have at least two inputs.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      CASE (201:299)
         IF (CF%N_INPUTS /= 2) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(909): CTRL ',TRIM(CF%ID),' must have only two inputs.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      CASE (401:499)
         IF (ANY(CF%INPUT_ID=='CONSTANT')) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(907): CTRL ',TRIM(CF%ID),' cannot use a CONSTANT INPUT_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (CF%N_INPUTS /= 1) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(906): CTRL ',TRIM(CF%ID),' must have only one input.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
   END SELECT
   ALLOCATE (CF%INPUT(CF%N_INPUTS),STAT=IZERO)
   CALL ChkMemErr('READ','CF%INPUT',IZERO)
   ALLOCATE (CF%INPUT_TYPE(CF%N_INPUTS),STAT=IZERO)
   CALL ChkMemErr('READ','CF%INPUT_TYPE',IZERO)
   CF%INPUT_TYPE = -1

   BUILD_INPUT: DO NN = 1, CF%N_INPUTS
      IF (TRIM(CF%INPUT_ID(NN))==TRIM(CF%ID)) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(910): CTRL ',TRIM(CF%ID),' cannot use a control function as an input to itself.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
      IF (CF%INPUT_ID(NN)=='CONSTANT') THEN
         IF (CONSTANT_SPECIFIED) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(911): CTRL ',TRIM(CF%ID),' can only specify one input as a constant value.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (CF%CONSTANT < -8.E30_EB) THEN
            WRITE(MESSAGE,'(3A)')  'ERROR(912): CTRL ',TRIM(CF%ID),' has no CONSTANT specified.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         CF%INPUT_TYPE(NN) = CONSTANT_INPUT
         CONSTANT_SPECIFIED = .TRUE.
         CYCLE BUILD_INPUT
      ENDIF
      CTRL_LOOP: DO NNN = 1, N_CTRL
         IF (CONTROL(NNN)%ID == CF%INPUT_ID(NN)) THEN
            CF%INPUT(NN) = NNN
            CF%INPUT_TYPE(NN) = CONTROL_INPUT
            IF (CF%CONTROL_INDEX == CF_PERCENTILE) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR(913): CTRL ',TRIM(CF%ID),' uses PERCENTILE and must have a DEVC as input.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (CF%CONTROL_INDEX == CUSTOM .AND. CONTROL(NNN)%CONTROL_INDEX < 101) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR(914): CTRL ',TRIM(CF%ID),' is CUSTOM and must have a DEVC or math CTRL as input.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            EXIT CTRL_LOOP
         ENDIF
      END DO CTRL_LOOP
      DEVC_LOOP: DO NNN = 1, N_DEVC
         IF (DEVICE(NNN)%ID == CF%INPUT_ID(NN)) THEN
            IF (ANY(DEVICE(NNN)%QUANTITY_INDEX==41)) TSF_WARNING=.TRUE.
            IF (CF%INPUT_TYPE(NN) > 0) THEN
               WRITE(MESSAGE,'(3A,I0,A)') 'ERROR(915): CTRL ',TRIM(CF%ID),' input ',NN,' is the ID for both a DEVC and a CTRL.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            CF%INPUT(NN) = NNN
            CF%INPUT_TYPE(NN) = DEVICE_INPUT
            EXIT DEVC_LOOP
         ENDIF
      END DO DEVC_LOOP
      IF (CF%INPUT_TYPE(NN) > 0) CYCLE BUILD_INPUT
      WRITE(MESSAGE,'(4A)') 'ERROR(916): CTRL ',TRIM(CF%ID),' cannot locate INPUT_ID ',TRIM(CF%INPUT_ID(NN))
      CALL SHUTDOWN(MESSAGE) ; RETURN
   END DO BUILD_INPUT

   ! Special setup for a PERCENTILE control device

   IF (CF%CONTROL_INDEX==CF_PERCENTILE) THEN
      DEVC_INDEX = CF%INPUT(1)
      DV => DEVICE(DEVC_INDEX)
      ALLOCATE(CF%PSUM(0:DV%N_POINTS))
      ALLOCATE(CF%LSUM(0:DV%N_POINTS))
      ALLOCATE(CF%DL(1:DV%N_POINTS))
      CF%LSUM(0) = 0._EB
      DO NN=2,DV%N_POINTS-1
         DX = 0.5_EB*(DEVICE(DEVC_INDEX+NN)%X - DEVICE(DEVC_INDEX+NN-2)%X)
         DY = 0.5_EB*(DEVICE(DEVC_INDEX+NN)%Y - DEVICE(DEVC_INDEX+NN-2)%Y)
         DZ = 0.5_EB*(DEVICE(DEVC_INDEX+NN)%Z - DEVICE(DEVC_INDEX+NN-2)%Z)
         CF%DL(NN) = SQRT(DX**2+DY**2+DZ**2)
      ENDDO
      CF%DL(1)           = DEVICE(DEVC_INDEX+1)%X - DEVICE(DEVC_INDEX)%X
      CF%DL(DV%N_POINTS) = DEVICE(DEVC_INDEX+DV%N_POINTS-1)%X - DEVICE(DEVC_INDEX+DV%N_POINTS-2)%X
      CF%LSUM(0) = 0._EB
      DO NN=1,DV%N_POINTS
         CF%LSUM(NN) = CF%LSUM(NN-1) + CF%DL(NN)
      ENDDO
   ENDIF

END DO PROC_CTRL_LOOP

IF (ABS(TIME_SHRINK_FACTOR-1._EB)>TWO_EPSILON_EB .AND. TSF_WARNING) THEN
    IF (MY_RANK==0)  WRITE(LU_ERR,'(A)') 'WARNING: One or more time based CTRL functions are being used with TIME_SHRINK_FACTOR'
ENDIF

END SUBROUTINE PROC_CTRL


!> \brief Process the OBSTruction parameters

SUBROUTINE PROC_OBST

USE GEOMETRY_FUNCTIONS, ONLY: BLOCK_CELL
INTEGER :: NM,I,J,K,IS,JS,KS,IC1,IC2

MESH_LOOP: DO NM=1,NMESHES

   IF (PROCESS(NM)/=MY_RANK)   CYCLE MESH_LOOP

   M=>MESHES(NM)
   CALL POINT_TO_MESH(NM)

   ! Make mesh edge cells not solid if cells on either side are not solid

   DO K=0,KBP1,KBP1
      IF (K==0) THEN ; KS=1 ; ELSE ; KS=-1 ; ENDIF
      DO J=0,JBP1,JBP1
         IF (J==0) THEN ; JS=1 ; ELSE ; JS=-1 ; ENDIF
         DO I=1,IBAR
            IC1 = CELL_INDEX(I,J+JS,K) ; IC2 = CELL_INDEX(I,J,K+KS)
            IF (.NOT.CELL(IC1)%SOLID .AND. .NOT.CELL(IC2)%SOLID) CALL BLOCK_CELL(NM,I,I,J,J,K,K,0,0)
            CELL(CELL_INDEX(I,J,K))%EXTERIOR_EDGE = .TRUE.
         ENDDO
      ENDDO
   ENDDO

   DO K=0,KBP1,KBP1
      IF (K==0) THEN ; KS=1 ; ELSE ; KS=-1 ; ENDIF
      DO I=0,IBP1,IBP1
         IF (I==0) THEN ; IS=1 ; ELSE ; IS=-1 ; ENDIF
         DO J=1,JBAR
            IC1 = CELL_INDEX(I+IS,J,K) ; IC2 = CELL_INDEX(I,J,K+KS)
            IF (.NOT.CELL(IC1)%SOLID .AND. .NOT.CELL(IC2)%SOLID) CALL BLOCK_CELL(NM,I,I,J,J,K,K,0,0)
            CELL(CELL_INDEX(I,J,K))%EXTERIOR_EDGE = .TRUE.
         ENDDO
      ENDDO
   ENDDO

   DO J=0,JBP1,JBP1
      IF (J==0) THEN ; JS=1 ; ELSE ; JS=-1 ; ENDIF
      DO I=0,IBP1,IBP1
         IF (I==0) THEN ; IS=1 ; ELSE ; IS=-1 ; ENDIF
         DO K=1,KBAR
            IC1 = CELL_INDEX(I+IS,J,K) ; IC2 = CELL_INDEX(I,J+JS,K)
            IF (.NOT.CELL(IC1)%SOLID .AND. .NOT.CELL(IC2)%SOLID) CALL BLOCK_CELL(NM,I,I,J,J,K,K,0,0)
            CELL(CELL_INDEX(I,J,K))%EXTERIOR_EDGE = .TRUE.
         ENDDO
      ENDDO
   ENDDO

ENDDO MESH_LOOP


END SUBROUTINE PROC_OBST


!> \brief Process the DEViCes

SUBROUTINE PROC_DEVC

USE COMP_FUNCTIONS, ONLY: CHANGE_UNITS
USE GEOMETRY_FUNCTIONS, ONLY: SEARCH_OTHER_MESHES
USE PHYSICAL_FUNCTIONS, ONLY: GET_VISCOSITY,GET_CONDUCTIVITY
USE CONTROL_VARIABLES
USE DEVICE_VARIABLES, ONLY: DEVICE_TYPE,SUBDEVICE_TYPE,DEVICE,N_DEVC,PROPERTY,PROPERTY_TYPE,MAX_HISTOGRAM_NBINS,&
                            N_HISTOGRAM

INTEGER  :: N,NN,NNN,NS,MAXCELLS,I,J,K,NOM,NR,II,JJ,KK
REAL(EB) :: XX,YY,ZZ,DISTANCE,SCANDISTANCE,DX,DY,DZ,XI,YJ,ZK,F,DFDD,NU,UU,TOL,RE,ZZ_G(1:N_TRACKED_SPECIES),MU_G,K_G
TYPE (DEVICE_TYPE),  POINTER :: DV
TYPE (SUBDEVICE_TYPE),  POINTER :: SDV
TYPE (PROPERTY_TYPE),  POINTER :: PY

IF (N_DEVC==0) RETURN

! Loop through all devices and set initial values

PROC_DEVC_LOOP: DO N=1,N_DEVC

   DV => DEVICE(N)

   ! Check for HVAC outputs with no HVAC inputs

   IF ((DV%DUCT_ID/='null' .OR. DV%NODE_ID(1)/='null') .AND. .NOT. HVAC_SOLVE) THEN
      WRITE(MESSAGE,'(3A)')  'ERROR(891): DEVC ',TRIM(DV%ID),' HVAC outputs specified with no HVAC inputs.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   ! If the Device has a SURF_ID, get the SURF_INDEX

   IF (DV%SURF_ID/='null') THEN
      DO NN=1,N_SURF
         IF (SURFACE(NN)%ID==DV%SURF_ID) DV%SURF_INDEX = NN
      ENDDO
   ENDIF

   ! Check if the device PROPERTY exists and is appropriate

   DV%PROP_INDEX = 0

   IF (DV%PROP_ID /='null') THEN

      CALL GET_PROPERTY_INDEX(DV%PROP_INDEX,'DEVC',DV%PROP_ID)
      IF (DV%QUANTITY(1)=='null' .AND. PROPERTY(DV%PROP_INDEX)%QUANTITY=='null') THEN
         WRITE(MESSAGE,'(5A)') 'ERROR(892): DEVC ',TRIM(DV%ID),' or PROP ',TRIM(DV%PROP_ID),' must have a QUANTITY.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (DV%QUANTITY(1)=='null' .AND. PROPERTY(DV%PROP_INDEX)%QUANTITY/='null') DV%QUANTITY(1) = PROPERTY(DV%PROP_INDEX)%QUANTITY

      ! Create an auto-ignition exclusion zone (AIT) in the cell containing a SPARK

      IF (PROPERTY(DV%PROP_INDEX)%SPARK .AND. PROCESS(DV%MESH)==MY_RANK) THEN
         M => MESHES(DV%MESH)
         DO NR=1,N_REACTIONS
            RN => REACTION(NR)
            RN%N_AIT_EXCLUSION_ZONES = RN%N_AIT_EXCLUSION_ZONES + 1
            CALL GET_IJK(DV%X,DV%Y,DV%Z,DV%MESH,XI,YJ,ZK,II,JJ,KK)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%X1 = M%X(II-1)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%X2 = M%X(II)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%Y1 = M%Y(JJ-1)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%Y2 = M%Y(JJ)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%Z1 = M%Z(KK-1)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%Z2 = M%Z(KK)
            RN%AIT_EXCLUSION_ZONE(RN%N_AIT_EXCLUSION_ZONES)%DEVC_INDEX = N
         ENDDO
      ENDIF

   ENDIF

   ! Check if the output QUANTITY exists and is appropriate

   QUANTITY_IF: IF (DV%QUANTITY(1) /= 'null') THEN
      CALL GET_QUANTITY_INDEX(DV%SMOKEVIEW_LABEL,DV%SMOKEVIEW_BAR_LABEL,DV%QUANTITY_INDEX(1),I_DUM(1), &
                              DV%Y_INDEX,DV%Z_INDEX,DV%PART_CLASS_INDEX,DV%DUCT_INDEX,DV%NODE_INDEX(1),&
                              DV%REAC_INDEX,DV%MATL_INDEX,'DEVC', &
                              DV%QUANTITY(1),'null',DV%SPEC_ID,DV%PART_ID,DV%DUCT_ID,DV%NODE_ID(1),DV%REAC_ID,DV%MATL_ID,&
                              DV%CELL_L,DV%DUCT_CELL_INDEX)

      IF (DV%QUANTITY(1)=='CONTROL' .OR. DV%QUANTITY(1)=='CONTROL VALUE') UPDATE_DEVICES_AGAIN = .TRUE.

      IF (DV%TEMPORAL_STATISTIC=='null') THEN
         IF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%TIME_AVERAGED) THEN
            DV%TEMPORAL_STATISTIC = 'TIME AVERAGE'
         ELSE
            DV%TEMPORAL_STATISTIC = 'INSTANT VALUE'
         ENDIF
      ENDIF

      IF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%INTEGRATED .AND. DV%X1<=-1.E6_EB) THEN
         WRITE(MESSAGE,'(5A)') 'ERROR(893): DEVC ',TRIM(DV%ID),' QUANTITY ',TRIM(DV%QUANTITY(1)),' requires XB coordinates.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (DV%QUANTITY_INDEX(1) < 0 .AND. (DV%SPATIAL_STATISTIC=='MASS MEAN' .OR. DV%SPATIAL_STATISTIC=='VOLUME MEAN' .OR. &
                                          DV%SPATIAL_STATISTIC=='VOLUME INTEGRAL' .OR. DV%SPATIAL_STATISTIC=='MASS INTEGRAL' .OR. &
                                          DV%SPATIAL_STATISTIC=='MASS' .OR. DV%SPATIAL_STATISTIC=='VOLUME')) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(894): DEVC ',TRIM(DV%ID),' uses invalid solid phase SPATIAL_STATISTIC.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (DV%QUANTITY_INDEX(1) > 0 .AND. (DV%SPATIAL_STATISTIC=='SURFACE INTEGRAL' .OR. DV%SPATIAL_STATISTIC=='SURFACE AREA')) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(895): DEVC ',TRIM(DV%ID),' uses invalid gas phase SPATIAL_STATISTIC.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (DV%QUANTITY_INDEX(1)>0 .AND. DV%SPATIAL_STATISTIC/='null' .AND. DV%X1<-1.E5_EB) THEN
         WRITE(MESSAGE,'(3A)') 'ERROR(896): DEVC ',TRIM(DV%ID),' requires XB for SPATIAL_STATISTIC.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (TRIM(DV%QUANTITY(1))=='NODE PRESSURE DIFFERENCE') THEN
         CALL GET_QUANTITY_INDEX(DV%SMOKEVIEW_LABEL,DV%SMOKEVIEW_BAR_LABEL,DV%QUANTITY_INDEX(1),I_DUM(1),DV%Y_INDEX,DV%Z_INDEX,&
                                 DV%PART_CLASS_INDEX,DV%DUCT_INDEX,DV%NODE_INDEX(2),I_DUM(2),I_DUM(3),'DEVC', &
                                 DV%QUANTITY(1),'null',DV%SPEC_ID,DV%PART_ID,DV%DUCT_ID,DV%NODE_ID(2),'null','null',-1._EB,&
                                 I_DUM(4))
         IF (DV%NODE_INDEX(1)==DV%NODE_INDEX(2)) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(897): DEVC ',TRIM(DV%ID),' NODE 1 = NODE 2.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF

       IF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%HVAC .AND. DV%MESH /= 1) THEN
           WRITE(MESSAGE,'(3A)') 'ERROR(898): DEVC ',TRIM(DV%ID),' should not use XYZ or XB for an HVAC output QUANTITY.'
           CALL SHUTDOWN(MESSAGE) ; RETURN
       ENDIF

      IF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%INTEGRATED_PARTICLES) DEVC_PARTICLE_FLUX = .TRUE.

   ENDIF QUANTITY_IF

   ! Even if the device is not in a mesh that is handled by the current MPI process, assign its unit.

   DV%QUANTITY(1) = OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%NAME
   IF (DV%UNITS=='null') THEN
      DV%UNITS = OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%UNITS
   ELSE
      DV%UNITS_SPECIFIED=.TRUE.
   ENDIF

   ! Assign properties to the DEVICE array

   DV%T_CHANGE   = 1.E7_EB
   DV%CTRL_INDEX = 0
   DV%T          = T_BEGIN
   DV%TMP_L      = TMPA

   ! COVariance and CORRelation COEFficient STATISTICS requiring QUANTITY2

   QUANTITY2_IF: IF (DV%N_QUANTITY==2) THEN
      IF (DV%RELATIVE) WRITE(MESSAGE,'(A,A)') 'WARNING: RELATIVE not applicable for multi-QUANTITY DEViCe ',TRIM(DV%ID)
      DV%RELATIVE=.FALSE.
      CALL GET_QUANTITY_INDEX(DV%SMOKEVIEW_LABEL,DV%SMOKEVIEW_BAR_LABEL,DV%QUANTITY_INDEX(2),I_DUM(1), &
                              DV%Y_INDEX,DV%Z_INDEX,DV%PART_CLASS_INDEX,DV%DUCT_INDEX,DV%NODE_INDEX(1),DV%REAC_INDEX,I_DUM(2),&
                             'DEVC',DV%QUANTITY(2),'null',DV%SPEC_ID,DV%PART_ID,DV%DUCT_ID,DV%NODE_ID(1),DV%REAC_ID,'null',&
                             -1._EB,I_DUM(3))
      DV%QUANTITY(2)   = OUTPUT_QUANTITY(DV%QUANTITY_INDEX(2))%NAME
      DV%SMOKEVIEW_LABEL = TRIM(DV%QUANTITY(1))//' '//TRIM(DV%QUANTITY(2))//' '//TRIM(DV%TEMPORAL_STATISTIC)
      DV%SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%SHORT_NAME)//'_'// &
                               TRIM(OUTPUT_QUANTITY(DV%QUANTITY_INDEX(2))%SHORT_NAME)//'_'//TRIM(DV%TEMPORAL_STATISTIC)
      SELECT CASE(DV%TEMPORAL_STATISTIC)
         CASE('COV')
            DV%UNITS = TRIM(OUTPUT_QUANTITY(DV%QUANTITY_INDEX(1))%UNITS)//'*'//TRIM(OUTPUT_QUANTITY(DV%QUANTITY_INDEX(2))%UNITS)
         CASE('CORRCOEF')
            DV%UNITS = ''
      END SELECT
   ENDIF QUANTITY2_IF

   ! Determine the cell indices of the device

   IF (DV%MESH>0) THEN
      IF (PROCESS(DV%MESH)==MY_RANK) THEN
         M => MESHES(DV%MESH)
         DO NNN=1,DV%N_QUANTITY
            CALL GET_IJK(DV%X,DV%Y,DV%Z,DV%MESH,XI,YJ,ZK,DV%I(NNN),DV%J(NNN),DV%K(NNN))
            IF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(NNN))%CELL_POSITION==CELL_CENTER) THEN
               DV%I(NNN) = MAX(1,MIN(DV%I(NNN),M%IBAR))
               DV%J(NNN) = MAX(1,MIN(DV%J(NNN),M%JBAR))
               DV%K(NNN) = MAX(1,MIN(DV%K(NNN),M%KBAR))
            ELSEIF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(NNN))%CELL_POSITION==CELL_FACE) THEN
               SELECT CASE(OUTPUT_QUANTITY(DV%QUANTITY_INDEX(NNN))%IOR)
                  CASE(1) ; DV%I(NNN) = NINT(XI)
                  CASE(2) ; DV%J(NNN) = NINT(YJ)
                  CASE(3) ; DV%K(NNN) = NINT(ZK)
               END SELECT
            ELSEIF (OUTPUT_QUANTITY(DV%QUANTITY_INDEX(NNN))%CELL_POSITION==CELL_EDGE) THEN
               SELECT CASE(OUTPUT_QUANTITY(DV%QUANTITY_INDEX(NNN))%IOR)
                  CASE(1) ; DV%J(NNN) = NINT(YJ) ; DV%K(NNN) = NINT(ZK)
                  CASE(2) ; DV%I(NNN) = NINT(XI) ; DV%K(NNN) = NINT(ZK)
                  CASE(3) ; DV%I(NNN) = NINT(XI) ; DV%J(NNN) = NINT(YJ)
               END SELECT
            ENDIF
         ENDDO
      ELSE
         DV%I=1 ; DV%J=1 ; DV%K=1
      ENDIF
   ENDIF

   ! Initialize histogram

   IF(PROPERTY(DV%PROP_INDEX)%HISTOGRAM) THEN
      ALLOCATE(DV%HISTOGRAM_COUNTS(PROPERTY(DV%PROP_INDEX)%HISTOGRAM_NBINS))
      DV%HISTOGRAM_COUNTS(:)=0._EB
      N_HISTOGRAM=N_HISTOGRAM+1
      MAX_HISTOGRAM_NBINS =MAX(MAX_HISTOGRAM_NBINS,PROPERTY(DV%PROP_INDEX)%HISTOGRAM_NBINS)
   ENDIF

   ! Do initialization of special models

   SPECIAL_QUANTITIES: SELECT CASE (DV%QUANTITY(1))

      CASE ('CHAMBER OBSCURATION')

         IF (DV%PROP_INDEX<1) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(931): DEVC ',TRIM(DV%ID),' is a smoke detector and must have a PROP_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (PROPERTY(DV%PROP_INDEX)%Y_INDEX<0 .AND. PROPERTY(DV%PROP_INDEX)%Z_INDEX<0) THEN
            IF (SOOT_INDEX<1) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR(932): DEVC ',TRIM(DV%ID),' is a smoke detector and requires a smoke source.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ELSE
               PROPERTY(DV%PROP_INDEX)%Y_INDEX = SOOT_INDEX
            ENDIF
         ENDIF
         ALLOCATE(DV%T_E(0:1000))
         ALLOCATE(DV%Y_E(0:1000))
         DV%T_E      = T_BEGIN
         DV%Y_E      = 0._EB
         DV%N_T_E    = -1
         DV%Y_C      = 0._EB
         DV%SETPOINT = PROPERTY(DV%PROP_INDEX)%ACTIVATION_OBSCURATION
         IF (PROPERTY(DV%PROP_INDEX)%Y_INDEX>0) DV%Y_INDEX = PROPERTY(DV%PROP_INDEX)%Y_INDEX
         IF (PROPERTY(DV%PROP_INDEX)%Z_INDEX>0) DV%Z_INDEX = PROPERTY(DV%PROP_INDEX)%Z_INDEX

      CASE ('LINK TEMPERATURE','SPRINKLER LINK TEMPERATURE')

         IF (DV%PROP_INDEX<1) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(933): DEVC ',TRIM(DV%ID),' must have a PROP_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (PROPERTY(DV%PROP_INDEX)%ACTIVATION_TEMPERATURE <= -273.15_EB) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(934): DEVC ',TRIM(DV%ID),' PROP ',TRIM(DV%PROP_ID),' needs an ACTIVATION_TEMPERATURE.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

         DV%SETPOINT = PROPERTY(DV%PROP_INDEX)%ACTIVATION_TEMPERATURE
         DV%TMP_L    = PROPERTY(DV%PROP_INDEX)%INITIAL_TEMPERATURE

      CASE ('THERMOCOUPLE')

         IF (DV%SPATIAL_STATISTIC/='null') THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(935): DEVC ',TRIM(DV%ID),' is a THERMOCOUPLE and cannot use SPATIAL_STATISTIC.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

         DV%TMP_L = PROPERTY(DV%PROP_INDEX)%INITIAL_TEMPERATURE  ! Initial TC temperature

         IF (DV%PROP_INDEX>0) THEN
            PY => PROPERTY(DV%PROP_INDEX)
            IF (PY%TIME_CONSTANT>0._EB) THEN  ! Convert a specified TIME_CONSTANT into an equivalent bead DIAMETER
               IF (PY%HEAT_TRANSFER_COEFFICIENT>0._EB) THEN  ! Calculate effective diameter directly
                  PY%DIAMETER = 6._EB*PY%HEAT_TRANSFER_COEFFICIENT*PY%TIME_CONSTANT/(PY%DENSITY*PY%SPECIFIC_HEAT)
               ELSE  ! Calculate effective diameter implicitly
                  ZZ_G = 0._EB
                  ZZ_G(1) = 1._EB
                  CALL GET_VISCOSITY(ZZ_G,MU_G,TMPA)
                  CALL GET_CONDUCTIVITY(ZZ_G,K_G,TMPA)
                  UU = 20._EB
                  TOL = 1._EB
                  RE = RHOA*UU*PY%DIAMETER/MU_G  ! Make initial estimate of Re and Nu based on default bead diameter
                  NU = 2._EB + 0.6_EB*SQRT(RE)*PR_AIR**ONTH
                  PY%DIAMETER = SQRT(6._EB*K_G*NU*PY%TIME_CONSTANT/(PY%DENSITY*PY%SPECIFIC_HEAT))
                  DO WHILE(TOL>1.E-5_EB)
                     RE = RHOA*UU*PY%DIAMETER/MU_G
                     NU = 2._EB + 0.6_EB*SQRT(RE)*PR_AIR**ONTH
                     F = 6._EB*K_G*NU*PY%TIME_CONSTANT - PY%DENSITY*PY%SPECIFIC_HEAT*PY%DIAMETER**2
                     DFDD = 1.8_EB*K_G*PY%TIME_CONSTANT*PR_AIR**ONTH*RE**(-0.5)*RHOA*UU/MU_G - &
                            2._EB*PY%DENSITY*PY%SPECIFIC_HEAT*PY%DIAMETER
                     PY%DIAMETER = PY%DIAMETER - F/DFDD
                     TOL = ABS(F/DFDD)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF

      CASE ('SOLID DENSITY')

         IF (DV%MATL_ID=='null') THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(936): DEVC ',TRIM(DV%ID),' must have a MATL_ID.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

      CASE ('LAYER HEIGHT','UPPER TEMPERATURE','LOWER TEMPERATURE')

         DV%TMP_LOW = TMPA
         DV%TMP_UP  = TMPA
         DV%Z_INT   = 0._EB
         CALL SEARCH_OTHER_MESHES(DV%X1,DV%Y1,DV%Z1,NOM,I,J,K)
         IF (NOM==0) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(937): DEVC ',TRIM(DV%ID),' is partially outside of the domain.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         DV%LOWEST_MESH = NOM

      CASE ('TRANSMISSION','PATH OBSCURATION','FIRE DEPTH')

         IF (DV%MESH>0) THEN
            M  => MESHES(DV%MESH)
            IF (DV%QUANTITY(1) .NE. 'FIRE DEPTH') THEN
               IF (DV%PROP_INDEX>0) THEN
                  IF (PROPERTY(DV%PROP_INDEX)%Y_INDEX<1 .AND. PROPERTY(DV%PROP_INDEX)%Z_INDEX<1) THEN
                     IF (SOOT_INDEX<1) THEN
                        WRITE(MESSAGE,'(3A)') 'ERROR(932): DEVC ',TRIM(DV%ID),' is a smoke detector and requires a smoke source.'
                        CALL SHUTDOWN(MESSAGE) ; RETURN
                     ELSE
                        PROPERTY(DV%PROP_INDEX)%Y_INDEX = SOOT_INDEX
                     ENDIF
                  ENDIF
               ELSE
                  IF (SOOT_INDEX <=0) THEN
                     WRITE(MESSAGE,'(3A)') 'ERROR(932): DEVC ',TRIM(DV%ID),' is a smoke detector and requires a smoke source.'
                     CALL SHUTDOWN(MESSAGE) ; RETURN
                  ENDIF
               ENDIF
            ENDIF
            IF (PROPERTY(DV%PROP_INDEX)%Y_INDEX>0) DV%Y_INDEX = PROPERTY(DV%PROP_INDEX)%Y_INDEX
            IF (PROPERTY(DV%PROP_INDEX)%Z_INDEX>0) DV%Z_INDEX = PROPERTY(DV%PROP_INDEX)%Z_INDEX
            DISTANCE = SQRT((DV%X1-DV%X2)**2 + (DV%Y1-DV%Y2)**2 + (DV%Z1-DV%Z2)**2)
            SCANDISTANCE = 0.0001_EB * DISTANCE
            DX = (DV%X2-DV%X1) * 0.0001_EB
            DY = (DV%Y2-DV%Y1) * 0.0001_EB
            DZ = (DV%Z2-DV%Z1) * 0.0001_EB

            DO NS=1,DV%N_SUBDEVICES
               SDV => DV%SUBDEVICE(NS)
               M => MESHES(SDV%MESH)
               MAXCELLS = 2*MAX(M%IBAR,M%JBAR,M%KBAR)
               ALLOCATE(SDV%I_PATH(0:MAXCELLS))
               ALLOCATE(SDV%J_PATH(0:MAXCELLS))
               ALLOCATE(SDV%K_PATH(0:MAXCELLS))
               ALLOCATE(SDV%D_PATH(MAXCELLS))
               SDV%D_PATH    = 0._EB
               SDV%I_PATH(0) = -1
               SDV%J_PATH(0) = -1
               SDV%K_PATH(0) = -1
               NN = 0
               DO NNN=1,10000
                  XX = DV%X1 + (NNN-0.5_EB)*DX
                  YY = DV%Y1 + (NNN-0.5_EB)*DY
                  ZZ = DV%Z1 + (NNN-0.5_EB)*DZ
                  CALL SEARCH_OTHER_MESHES(XX,YY,ZZ,NOM,I,J,K)
                  IF (NOM/=SDV%MESH) CYCLE
                  IF (NN>0) THEN
                     IF (I/=SDV%I_PATH(NN) .OR. J/=SDV%J_PATH(NN) .OR. K/=SDV%K_PATH(NN)) NN = NN + 1
                  ELSE
                     NN = 1
                  ENDIF
                  SDV%D_PATH(NN) = SDV%D_PATH(NN) + SCANDISTANCE
                  SDV%I_PATH(NN) = I
                  SDV%J_PATH(NN) = J
                  SDV%K_PATH(NN) = K
               ENDDO
               SDV%N_PATH = NN
            ENDDO
         ENDIF

      CASE ('CONTROL')

         C1: DO NN=1,N_CTRL
            IF (CONTROL(NN)%ID==DV%CTRL_ID) THEN
               DV%CTRL_INDEX = NN
               EXIT C1
            ENDIF
         ENDDO C1
         IF (DV%CTRL_ID/='null' .AND. DV%CTRL_INDEX<=0) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(938): DEVC ',TRIM(DV%ID),' CTRL_ID ',TRIM(DV%CTRL_ID),' does not exist.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

         DV%SETPOINT = 0.5
         DV%TRIP_DIRECTION = 1

      CASE ('CONTROL VALUE')

         C2: DO NN=1,N_CTRL
            IF (CONTROL(NN)%ID==DV%CTRL_ID) THEN
               DV%CTRL_INDEX = NN
               EXIT C2
            ENDIF
         ENDDO C2
         IF (DV%CTRL_ID/='null' .AND. DV%CTRL_INDEX<=0) THEN
            WRITE(MESSAGE,'(5A)') 'ERROR(938): DEVC ',TRIM(DV%ID),' CTRL_ID ',TRIM(DV%CTRL_ID),' does not exist.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (CONTROL(DV%CTRL_INDEX)%CONTROL_INDEX>2 .AND. CONTROL(DV%CTRL_INDEX)%CONTROL_INDEX<101) THEN
            WRITE(MESSAGE,'(5A)')  'ERROR(939): DEVC ',TRIM(DV%ID),' CTRL_ID ',TRIM(DV%CTRL_ID), &
                                   ' is a logic only function and can only use the QUANTITY of CONTROL.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

      CASE ('ASPIRATION')

         ! Check either for a specified SMOKE SPECies, or if simple chemistry model is being used
         IF (DV%PROP_INDEX>0) THEN
            IF (PROPERTY(DV%PROP_INDEX)%Y_INDEX<1 .AND. PROPERTY(DV%PROP_INDEX)%Z_INDEX<1 .AND. SOOT_INDEX<1) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR(932): DEVC ',TRIM(DV%ID),' is a smoke detector and requires a smoke source.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDIF
         ! Count number of inputs for detector and verify that input is DENSITY with a specified SPEC_ID for smoke
         NNN = 0
         DO NN=1,N_DEVC
            IF (DEVICE(NN)%DEVC_ID==DV%ID) THEN
               IF (DEVICE(NN)%QUANTITY(1)/='DENSITY' .OR. DEVICE(NN)%SPEC_ID=='null') THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(940): DEVC ',TRIM(DEVICE(NN)%ID)," must use QUANTITY='DENSITY' and a SPEC_ID."
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               NNN = NNN + 1
            ENDIF
         ENDDO
         ALLOCATE(DV%DEVC_INDEX(NNN),STAT=IZERO)
         CALL ChkMemErr('READ','DV%DEVC_INDEX',IZERO)
         DV%DEVC_INDEX = -1
         ALLOCATE(DV%YY_SOOT(NNN,0:100))
         CALL ChkMemErr('READ','DV%YY_SOOT',IZERO)
         DV%YY_SOOT = 0._EB
         ALLOCATE(DV%TIME_ARRAY(0:100))
         CALL ChkMemErr('READ','DV%TIME_ARRAY',IZERO)
         DV%TIME_ARRAY = 0._EB
         DV%TOTAL_FLOWRATE = DV%BYPASS_FLOWRATE
         DV%DT             = -1._EB
         DV%N_INPUTS = NNN
         NNN = 1
         DO NN=1,N_DEVC
            IF (DEVICE(NN)%DEVC_ID==DV%ID) THEN
               DV%TOTAL_FLOWRATE  = DV%TOTAL_FLOWRATE + DEVICE(NN)%FLOWRATE
               DV%DT = MAX(DV%DT,DEVICE(NN)%DELAY)
               IF (NN > N) THEN
                  WRITE(MESSAGE,'(3A)') 'ERROR(941): DEVC ',TRIM(DV%ID),' must be listed after all of its inputs.'
                  CALL SHUTDOWN(MESSAGE) ; RETURN
               ENDIF
               DV%DEVC_INDEX(NNN)     = NN
               NNN = NNN + 1
            ENDIF
         ENDDO
         DV%DT = DV%DT * 0.01_EB

      CASE ('FED')
         DV%SPATIAL_STATISTIC  = 'null'
         DV%TEMPORAL_STATISTIC = 'TIME INTEGRAL'
         IF (DV%PROP_INDEX>0) THEN
            IF (PROPERTY(DV%PROP_INDEX)%FED_ACTIVITY<1 .AND. PROPERTY(DV%PROP_INDEX)%FED_ACTIVITY>3) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR(942): DEVC ',TRIM(DV%ID),' is a FED detector and requires an activity.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            DV%FED_ACTIVITY = PROPERTY(DV%PROP_INDEX)%FED_ACTIVITY
         ENDIF

      CASE ('VELOCITY PATCH')
         PATCH_VELOCITY = .TRUE.
         ALLOCATE(DV%DEVC_INDEX(1),STAT=IZERO)
         DV%DEVC_INDEX(1) = 0
         DO NN=1,N_DEVC
            IF (DEVICE(NN)%ID==DV%DEVC_ID) DV%DEVC_INDEX(1) = NN
         ENDDO
         IF (DV%DEVC_ID=='null' .OR. DV%DEVC_INDEX(1)==0) THEN
            WRITE(MESSAGE,'(3A)') 'ERROR(943): DEVC ',TRIM(DV%ID),' is a VELOCITY PATCH and needs a DEVC_ID to control it.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF

   END SELECT SPECIAL_QUANTITIES

   IF ((DV%SPATIAL_STATISTIC/='null' .OR. DV%TEMPORAL_STATISTIC/='null') .AND. .NOT.DV%UNITS_SPECIFIED) THEN
      CALL CHANGE_UNITS(DV%QUANTITY(1),DV%UNITS,DV%SPATIAL_STATISTIC,DV%TEMPORAL_STATISTIC,LU_ERR)
   ENDIF

   IF (DV%NO_UPDATE_DEVC_ID/='null' .OR. DV%NO_UPDATE_CTRL_ID/='null') &
      CALL SEARCH_CONTROLLER('DEVC',DV%NO_UPDATE_CTRL_ID,DV%NO_UPDATE_DEVC_ID,DV%NO_UPDATE_DEVC_INDEX,DV%NO_UPDATE_CTRL_INDEX,N)

   IF (DV%SETPOINT <1.E20_EB) WRITE_DEVC_CTRL = .TRUE.

ENDDO PROC_DEVC_LOOP

END SUBROUTINE PROC_DEVC


!> \brief Read in the PROFile namelist lines

SUBROUTINE READ_PROF

INTEGER :: N,NM,MESH_NUMBER,NN,N_PROFO,IOR,FORMAT_INDEX
REAL(EB) :: XYZ(3)
CHARACTER(LABEL_LENGTH) :: QUANTITY,INIT_ID,MATL_ID,LABEL_DUM(2)
LOGICAL :: CELL_CENTERED
TYPE (PROFILE_TYPE), POINTER :: PF
NAMELIST /PROF/ CELL_CENTERED,FORMAT_INDEX,FYI,ID,INIT_ID,IOR,MATL_ID,QUANTITY,XYZ

N_PROF = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_PROF_LOOP: DO
   CALL CHECKREAD('PROF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_PROF_LOOP
   READ(LU_INPUT,NML=PROF,END=11,ERR=12,IOSTAT=IOS)
   N_PROF = N_PROF + 1
   12 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with PROF number ',N_PROF+1
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDDO COUNT_PROF_LOOP
11 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_PROF==0) RETURN

ALLOCATE(PROFILE(N_PROF),STAT=IZERO)
CALL ChkMemErr('READ','PROFILE',IZERO)

N_PROFO = N_PROF
N       = 0

PROF_LOOP: DO NN=1,N_PROFO

   N = N+1

   CELL_CENTERED = .FALSE.
   FORMAT_INDEX = 1
   FYI = 'null'
   ID = 'null'
   INIT_ID = 'null'
   IOR  = 0
   MATL_ID = 'null'
   QUANTITY = 'null'
   XYZ = 1.E10_EB
   WRITE(ID,'(A,I0)') 'PROFILE ',N

   CALL CHECKREAD('PROF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT PROF_LOOP
   READ(LU_INPUT,PROF)

   ! Check for bad PROF quantities or coordinates

   IF (IOR==0 .AND. INIT_ID=='null') THEN
      WRITE(MESSAGE,'(A,I0,A)') 'ERROR(951): PROF ',NN,' requires an orientation index, IOR.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF

   MESH_NUMBER = 0

   ! If the PROFile has been assigned an orientation (IOR/=0), determine what mesh its XYZ is in. If not in any mesh, reject it.

   IF (IOR/=0) THEN
      MESH_LOOP: DO NM=1,NMESHES
         M=>MESHES(NM)
         IF (XYZ(1)>=M%XS .AND. XYZ(1)<=M%XF .AND. XYZ(2)>=M%YS .AND. XYZ(2)<=M%YF .AND. XYZ(3)>=M%ZS .AND. XYZ(3)<=M%ZF) THEN
            IF (ABS(XYZ(1)-M%XS)<TWO_EPSILON_EB) THEN
               IF (IOR==-1) THEN ; CYCLE MESH_LOOP ; ELSE ; XYZ(1)=XYZ(1)+0.001_EB*(M%XF-M%XS)/REAL(M%IBAR,EB) ; ENDIF
            ENDIF
            IF (ABS(XYZ(1)-M%XF)<TWO_EPSILON_EB) THEN
               IF (IOR== 1) THEN ; CYCLE MESH_LOOP ; ELSE ; XYZ(1)=XYZ(1)-0.001_EB*(M%XF-M%XS)/REAL(M%IBAR,EB) ; ENDIF
            ENDIF
            IF (ABS(XYZ(2)-M%YS)<TWO_EPSILON_EB) THEN
               IF (IOR==-2) THEN ; CYCLE MESH_LOOP ; ELSE ; XYZ(2)=XYZ(2)+0.001_EB*(M%YF-M%YS)/REAL(M%JBAR,EB) ; ENDIF
            ENDIF
            IF (ABS(XYZ(2)-M%YF)<TWO_EPSILON_EB) THEN
               IF (IOR== 2) THEN ; CYCLE MESH_LOOP ; ELSE ; XYZ(2)=XYZ(2)-0.001_EB*(M%YF-M%YS)/REAL(M%JBAR,EB) ; ENDIF
            ENDIF
            IF (ABS(XYZ(3)-M%ZS)<TWO_EPSILON_EB) THEN
               IF (IOR==-3) THEN ; CYCLE MESH_LOOP ; ELSE ; XYZ(3)=XYZ(3)+0.001_EB*(M%ZF-M%ZS)/REAL(M%KBAR,EB) ; ENDIF
            ENDIF
            IF (ABS(XYZ(3)-M%ZF)<TWO_EPSILON_EB) THEN
               IF (IOR== 3) THEN ; CYCLE MESH_LOOP ; ELSE ; XYZ(3)=XYZ(3)-0.001_EB*(M%ZF-M%ZS)/REAL(M%KBAR,EB) ; ENDIF
            ENDIF
            MESH_NUMBER = NM
            EXIT MESH_LOOP
         ENDIF
      ENDDO MESH_LOOP
      IF (MESH_NUMBER==0) THEN  ! No meshes have been found. Throw out this PROFile.
         N      = N-1
         N_PROF = N_PROF-1
         WRITE(MESSAGE,'(A,I0,A)') 'WARNING: PROF ',NN,' is not located in any MESH.'
         IF (MY_RANK==0) WRITE(LU_ERR,'(A)') TRIM(MESSAGE)
         CYCLE PROF_LOOP
      ENDIF
   ENDIF

   ! Assign parameters to the PROFILE array

   PF => PROFILE(N)
   PF%CELL_CENTERED= CELL_CENTERED
   PF%FORMAT_INDEX = FORMAT_INDEX
   PF%ORDINAL      = NN
   PF%INIT_ID      = INIT_ID
   PF%MESH         = MESH_NUMBER
   PF%ID           = ID
   PF%MATL_ID      = MATL_ID
   IF (QUANTITY=='TEMPERATURE' .OR. QUANTITY=='WALL TEMPERATURE') QUANTITY = 'INSIDE WALL TEMPERATURE'
   IF (QUANTITY=='DENSITY') QUANTITY = 'SOLID DENSITY'
   PF%QUANTITY     = QUANTITY
   CALL GET_QUANTITY_INDEX(LABEL_DUM(1),LABEL_DUM(2),PF%QUANTITY_INDEX,I_DUM(1), &
                           I_DUM(2),I_DUM(3),I_DUM(4),I_DUM(5),I_DUM(6),I_DUM(7),&
                           I_DUM(8),'PROF', &
                           PF%QUANTITY,'null','null','null','null','null','null',PF%MATL_ID,&
                           -1._EB,I_DUM(9))
   IF (.NOT. OUTPUT_QUANTITY(PF%QUANTITY_INDEX)%PROF_APPROPRIATE) THEN
      WRITE(MESSAGE,'(A,I0,A3)') 'ERROR(952): PROF ',NN,' QUANTITY ',TRIM(PF%QUANTITY),' is not valid.'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   PF%X            = XYZ(1)
   PF%Y            = XYZ(2)
   PF%Z            = XYZ(3)
   PF%IOR          = IOR

ENDDO PROF_LOOP
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

END SUBROUTINE READ_PROF


!> \brief Read in the ISOsurFace namelist lines

SUBROUTINE READ_ISOF

REAL(EB) :: VALUE(10)
CHARACTER(LABEL_LENGTH) :: QUANTITY,QUANTITY2,SPEC_ID,SPEC_ID2
INTEGER :: N,VELO_INDEX,VELO_INDEX2,SKIP
TYPE(ISOSURFACE_FILE_TYPE), POINTER :: IS=>NULL()
REAL(FB) :: DELTA
NAMELIST /ISOF/ DELTA,FYI,QUANTITY,QUANTITY2,SKIP,SPEC_ID,SPEC_ID2,VALUE,VELO_INDEX,VELO_INDEX2

N_ISOF = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_ISOF_LOOP: DO
   CALL CHECKREAD('ISOF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_ISOF_LOOP
   READ(LU_INPUT,NML=ISOF,END=9,ERR=10,IOSTAT=IOS)
   N_ISOF = N_ISOF + 1
   10 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with ISOF number ',N_ISOF
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_ISOF_LOOP
9 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ALLOCATE(ISOSURFACE_FILE(N_ISOF),STAT=IZERO)
CALL ChkMemErr('READ','ISOSURFACE_FILE',IZERO)

READ_ISOF_LOOP: DO N=1,N_ISOF
   IS => ISOSURFACE_FILE(N)
   DELTA            = -1.0_FB
   QUANTITY         = 'null'
   QUANTITY2        = 'null'
   SKIP             = 1
   SPEC_ID          = 'null'
   SPEC_ID2         = 'null'
   VALUE            = -999._EB
   VELO_INDEX       = 0
   VELO_INDEX2      = 0

   CALL CHECKREAD('ISOF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_ISOF_LOOP
   READ(LU_INPUT,ISOF)

   IS%DELTA            = DELTA
   IS%VELO_INDEX       = VELO_INDEX
   IS%VELO_INDEX2      = VELO_INDEX2
   IS%SKIP             = SKIP

   CALL GET_QUANTITY_INDEX(IS%SMOKEVIEW_LABEL,IS%SMOKEVIEW_BAR_LABEL,IS%INDEX,I_DUM(1), &
                           IS%Y_INDEX,IS%Z_INDEX,I_DUM(2),I_DUM(3),I_DUM(4),I_DUM(5),I_DUM(6),'ISOF', &
                           QUANTITY,'null',SPEC_ID,'null','null','null','null','null',-1._EB,I_DUM(7))

   IF ( QUANTITY2 /= 'null' ) THEN
      CALL GET_QUANTITY_INDEX(IS%SMOKEVIEW_LABEL2,IS%SMOKEVIEW_BAR_LABEL2,IS%INDEX2,I_DUM(1), &
                           IS%Y_INDEX2,IS%Z_INDEX2,I_DUM(2),I_DUM(3),I_DUM(4),I_DUM(5),I_DUM(6),'ISOF', &
                           QUANTITY2,'null',SPEC_ID2,'null','null','null','null','null',-1._EB,I_DUM(7))
   ENDIF

   VALUE_LOOP: DO I=1,10
      IF (VALUE(I)<=-998._EB) EXIT VALUE_LOOP
      IS%N_VALUES = I
      IS%VALUE(I) = REAL(VALUE(I),FB)
   ENDDO VALUE_LOOP

ENDDO READ_ISOF_LOOP

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

END SUBROUTINE READ_ISOF

!> \brief Read the SLiCe File namelist lines

SUBROUTINE READ_SLCF

REAL(EB) :: MAXIMUM_VALUE,MINIMUM_VALUE
REAL(EB) :: AGL_SLICE
INTEGER :: N,NN,NM,MESH_NUMBER,N_SLCF_O,NITER,ITER,VELO_INDEX,GEOM_INDEX
LOGICAL :: VECTOR,CELL_CENTERED,DEBUG
CHARACTER(LABEL_LENGTH) :: QUANTITY,SPEC_ID,PART_ID,QUANTITY2,PROP_ID,REAC_ID,SLICETYPE
REAL(EB), PARAMETER :: TOL=1.E-10_EB
REAL(FB) :: RLE_MIN, RLE_MAX
TYPE (SLICE_TYPE), POINTER :: SL
NAMELIST /SLCF/ AGL_SLICE,CELL_CENTERED,DB,FYI,DEBUG,ID,MAXIMUM_VALUE,MESH_NUMBER,&
                MINIMUM_VALUE,PART_ID,PBX,PBY,PBZ,PROP_ID,QUANTITY,QUANTITY2,REAC_ID,RLE_MIN,RLE_MAX,SLICETYPE,&
                SPEC_ID,VECTOR,VELO_INDEX,XB

MESH_LOOP: DO NM=1,NMESHES

   IF (MY_RANK/=PROCESS(NM)) CYCLE MESH_LOOP

   M=>MESHES(NM)
   CALL POINT_TO_MESH(NM)

   N_SLCF   = 0
   N_SLCF_O = 0
   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   COUNT_SLCF_LOOP: DO
      VECTOR  = .FALSE.
      MESH_NUMBER=NM
      CALL CHECKREAD('SLCF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT COUNT_SLCF_LOOP
      READ(LU_INPUT,NML=SLCF,END=9,ERR=10,IOSTAT=IOS)
      N_SLCF_O = N_SLCF_O + 1
      IF (MESH_NUMBER/=NM) CYCLE COUNT_SLCF_LOOP
      N_SLCF  = N_SLCF + 1
      IF (VECTOR .AND. TWO_D) N_SLCF = N_SLCF + 2
      IF (VECTOR .AND. .NOT. TWO_D) N_SLCF = N_SLCF + 3
      10 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with SLCF number ',N_SLCF_O+1
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO COUNT_SLCF_LOOP
   9 CONTINUE

   ALLOCATE(M%SLICE(N_SLCF),STAT=IZERO)
   CALL ChkMemErr('READ','ISP1',IZERO)
   CALL POINT_TO_MESH(NM)  ! Reset the pointers after the allocation

   N = 0
   N_TERRAIN_SLCF = 0

   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   SLCF_LOOP: DO NN=1,N_SLCF_O
      QUANTITY  = 'null'
      QUANTITY2 = 'null'
      PBX      = -1.E6_EB
      PBY      = -1.E6_EB
      PBZ      = -1.E6_EB
      DB       = 'null'
      VECTOR   = .FALSE.
      ID       = 'null'
      MESH_NUMBER=NM
      MINIMUM_VALUE = 0._EB
      MAXIMUM_VALUE = 0._EB
      AGL_SLICE = -1._EB
      REAC_ID  = 'null'
      SPEC_ID  = 'null'
      PART_ID  = 'null'
      PROP_ID  = 'null'
      GEOM_INDEX = -1
      SLICETYPE = 'STRUCTURED'
      CELL_CENTERED = .FALSE.
      DEBUG = .FALSE.
      VELO_INDEX = 0
      RLE_MIN = 1.0_FB
      RLE_MAX = 0.0_FB

      CALL CHECKREAD('SLCF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT SLCF_LOOP
      READ(LU_INPUT,SLCF)
      IF (MESH_NUMBER/=NM) CYCLE SLCF_LOOP

      IF (AGL_SLICE>=0._EB .AND. CELL_CENTERED) THEN
         WRITE(MESSAGE,'(A)') 'ERROR(961): CELL_CENTERED not allowed with AGL_SLICE.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (PBX>-1.E5_EB .OR. PBY>-1.E5_EB .OR. PBZ>-1.E5_EB) THEN
         XB(1) = XS
         XB(2) = XF
         XB(3) = YS
         XB(4) = YF
         XB(5) = ZS
         XB(6) = ZF
         IF (PBX>-1.E5_EB) XB(1:2) = PBX
         IF (PBY>-1.E5_EB) XB(3:4) = PBY
         IF (PBZ>-1.E5_EB) XB(5:6) = PBZ
      ENDIF

      IF (DB/='null' .OR. AGL_SLICE>=0._EB) THEN
         XB(1) = XS_MIN
         XB(2) = XF_MAX
         XB(3) = YS_MIN
         XB(4) = YF_MAX
         XB(5) = ZS_MIN
         XB(6) = ZF_MAX
         SELECT CASE(DB)
            CASE('XMIN')
               XB(1:2) = XS_MIN
            CASE('XMID')
               XB(1:2) = 0.5_EB*(XS_MIN+XF_MAX)
            CASE('XMAX')
               XB(1:2) = XF_MAX
            CASE('YMIN')
               XB(3:4) = YS_MIN
            CASE('YMID')
               XB(3:4) = 0.5_EB*(YS_MIN+YF_MAX)
            CASE('YMAX')
               XB(3:4) = YF_MAX
            CASE('ZMIN')
               XB(5:6) = ZS_MIN
            CASE('ZMID')
               XB(5:6) = 0.5_EB*(ZS_MIN+ZF_MAX)
            CASE('ZMAX')
               XB(5:6) = ZF_MAX
         END SELECT
      ENDIF

      CALL CHECK_XB(XB)

      XB(1) = MAX(XB(1),XS)
      XB(2) = MIN(XB(2),XF)
      XB(3) = MAX(XB(3),YS)
      XB(4) = MIN(XB(4),YF)
      XB(5) = MAX(XB(5),ZS)
      XB(6) = MIN(XB(6),ZF)

      ! Reject a slice if it is beyond the bounds of the current mesh

      IF (XB(1)>XF .OR. XB(2)<XS .OR. XB(3)>YF .OR. XB(4)<YS .OR. XB(5)>ZF .OR. XB(6)<ZS) THEN
         N_SLCF = N_SLCF - 1
         IF (VECTOR .AND. TWO_D) N_SLCF = N_SLCF - 2
         IF (VECTOR .AND. .NOT. TWO_D) N_SLCF = N_SLCF - 3
         CYCLE SLCF_LOOP
      ENDIF

      ! Process vector quantities

      NITER = 1
      IF (VECTOR .AND. TWO_D) NITER = 3
      IF (VECTOR .AND. .NOT. TWO_D) NITER = 4

      VECTORLOOP: DO ITER=1,NITER
         N = N + 1
         SL=>SLICE(N)
         SL%SLCF_INDEX=NN
         SL%ID = ID
         SL%SLICETYPE = TRIM(SLICETYPE)
         SL%GEOM_INDEX = GEOM_INDEX
         IF (CELL_CENTERED) THEN
            DO I=1,IBAR
               IF ( ABS(XB(1)-XC(I)) < 0.5_EB*DX(I) + TOL ) SL%I1 = I
               IF ( ABS(XB(2)-XC(I)) < 0.5_EB*DX(I) + TOL ) SL%I2 = I
            ENDDO
            DO J=1,JBAR
               IF ( ABS(XB(3)-YC(J)) < 0.5_EB*DY(J) + TOL ) SL%J1 = J
               IF ( ABS(XB(4)-YC(J)) < 0.5_EB*DY(J) + TOL ) SL%J2 = J
            ENDDO
            DO K=1,KBAR
               IF ( ABS(XB(5)-ZC(K)) < 0.5_EB*DZ(K) + TOL ) SL%K1 = K
               IF ( ABS(XB(6)-ZC(K)) < 0.5_EB*DZ(K) + TOL ) SL%K2 = K
            ENDDO
            IF (SL%I1<SL%I2) SL%I1=SL%I1-1
            IF (SL%J1<SL%J2) SL%J1=SL%J1-1
            IF (SL%K1<SL%K2) SL%K1=SL%K1-1
         ELSEIF (AGL_SLICE>=0._EB) THEN
            SL%I1 = 0
            SL%I2 = IBAR
            SL%J1 = 0
            SL%J2 = JBAR
            SL%K1 = 1
            SL%K2 = 1
         ELSE
            SL%I1 = NINT( GINV(XB(1)-XS,1,NM)*RDXI )
            SL%I2 = NINT( GINV(XB(2)-XS,1,NM)*RDXI )
            SL%J1 = NINT( GINV(XB(3)-YS,2,NM)*RDETA )
            SL%J2 = NINT( GINV(XB(4)-YS,2,NM)*RDETA )
            SL%K1 = NINT( GINV(XB(5)-ZS,3,NM)*RDZETA )
            SL%K2 = NINT( GINV(XB(6)-ZS,3,NM)*RDZETA )
         ENDIF
         SL%MINMAX(1) = REAL(MINIMUM_VALUE,FB)
         SL%MINMAX(2) = REAL(MAXIMUM_VALUE,FB)
         IF (ITER==2)                    QUANTITY = 'U-VELOCITY'
         IF (ITER==3 .AND. .NOT. TWO_D)  QUANTITY = 'V-VELOCITY'
         IF (ITER==3 .AND. TWO_D)        QUANTITY = 'W-VELOCITY'
         IF (ITER==4)                    QUANTITY = 'W-VELOCITY'
         SL%RLE_MIN = RLE_MIN
         SL%RLE_MAX = RLE_MAX
         IF (RLE_MAX>RLE_MIN) THEN
            SL%RLE = .TRUE.
         ELSE
            SL%RLE = .FALSE.
         ENDIF
         SL%VELO_INDEX = VELO_INDEX
         CALL GET_QUANTITY_INDEX(SL%SMOKEVIEW_LABEL,SL%SMOKEVIEW_BAR_LABEL,SL%INDEX,SL%INDEX2, &
                                 SL%Y_INDEX,SL%Z_INDEX,SL%PART_INDEX,I_DUM(1),I_DUM(2),SL%REAC_INDEX,SL%MATL_INDEX,'SLCF', &
                                 QUANTITY,QUANTITY2,SPEC_ID,PART_ID,'null','null',REAC_ID,'null',-1._EB,I_DUM(3),&
                                 SLICETYPE=SLICETYPE)

         ! If the user needs to do a particle flux calculation, detect that here.

         IF (OUTPUT_QUANTITY(SL%INDEX)%INTEGRATED_PARTICLES) SLCF_PARTICLE_FLUX = .TRUE.

         ! Determine if this is a 3D slice

         IF (SL%I1/=SL%I2 .AND. SL%J1/=SL%J2 .AND. SL%K1/=SL%K2) SL%THREE_D = .TRUE.

         ! For terrain slices, AGL=above ground level

         IF (ITER == 1 .AND. AGL_SLICE > -1._EB) THEN
            SL%TERRAIN_SLICE = .TRUE.
            SL%AGL_SLICE     = AGL_SLICE
            N_TERRAIN_SLCF   = N_TERRAIN_SLCF + 1
         ENDIF
         IF (ITER==2 .OR. ITER==3 .OR. ITER ==4) THEN
            IF (SLICE(N-1)%TERRAIN_SLICE) THEN
               SL%TERRAIN_SLICE =  .TRUE.
               SL%AGL_SLICE     = SLICE(N-1)%AGL_SLICE
               N_TERRAIN_SLCF   = N_TERRAIN_SLCF + 1
            ENDIF
         ENDIF

         SL%CELL_CENTERED = CELL_CENTERED
         SL%DEBUG         = DEBUG

         ! Check if the slcf PROPERTY exists (for FED_ACTIVITY input)

         SL%PROP_INDEX = 0
         IF (PROP_ID /='null') THEN
            CALL GET_PROPERTY_INDEX(SL%PROP_INDEX,'SLCF',PROP_ID)
         ENDIF

      ENDDO VECTORLOOP

      IF (TRIM(QUANTITY)=='CHEMISTRY SUBITERATIONS') OUTPUT_CHEM_IT = .TRUE.

      IF (TRIM(QUANTITY)=='REAC SOURCE TERM' .OR. TRIM(QUANTITY)=='HRRPUV REAC') REAC_SOURCE_CHECK = .TRUE.

      IF (TRIM(SLICETYPE)=='CUT_CELLS') THEN
         SL%SMOKEVIEW_LABEL     = 'Cut Cells'
         SL%SMOKEVIEW_BAR_LABEL = 'ccells'
      ENDIF

      IF (TRIM(QUANTITY)=='CUTCELL VELOCITY DIVERGENCE') THEN
         STORE_CUTCELL_DIVERGENCE   = .TRUE.
         STORE_CARTESIAN_DIVERGENCE = .TRUE.
      ENDIF
      IF (TRIM(QUANTITY)=='CARTESIAN VELOCITY DIVERGENCE') STORE_CARTESIAN_DIVERGENCE = .TRUE.

      IF (TRIM(QUANTITY)=='PRESSURE POISSON RESIDUAL') STORE_PRESSURE_POISSON_RESIDUAL = .TRUE.

   ENDDO SLCF_LOOP

   ALLOCATE(M%K_AGL_SLICE(0:IBP1,0:JBP1,N_TERRAIN_SLCF),STAT=IZERO)
   CALL ChkMemErr('READ','K_AGL_SLICE',IZERO)
   M%K_AGL_SLICE = 0
   N = 0
   DO NN = 1,N_SLCF
      SL=>SLICE(NN)
      IF (SL%TERRAIN_SLICE) THEN
        TERRAIN_CASE = .TRUE.
        N = N + 1
        M%K_AGL_SLICE(0:IBP1,0:JBP1,N) =  INT(SL%AGL_SLICE*M%RDZ(1))
        ! Subtract one because bottom of domain will be accounted for when cycling through walls cells
        M%K_AGL_SLICE(0:IBP1,0:JBP1,N) =  MAX(0,M%K_AGL_SLICE(0:IBP1,0:JBP1,N)-1)
      ENDIF
   ENDDO

   N_SLCF_MAX = MAX(N_SLCF_MAX,N_SLCF)

ENDDO MESH_LOOP

END SUBROUTINE READ_SLCF


!> \brief Read the RADF (RADiation File) namelist lines and create special radiation output files

SUBROUTINE READ_RADF

INTEGER :: N,NN,NM,I_STEP,J_STEP,K_STEP
TYPE (RAD_FILE_TYPE), POINTER :: RF
NAMELIST /RADF/ FYI,I_STEP,J_STEP,K_STEP,XB

MESH_LOOP: DO NM=1,NMESHES

   IF (MY_RANK/=PROCESS(NM)) CYCLE MESH_LOOP

   M=>MESHES(NM)
   CALL POINT_TO_MESH(NM)

   N_RADF = 0
   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   COUNT_RADF_LOOP: DO
      CALL CHECKREAD('RADF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT COUNT_RADF_LOOP
      READ(LU_INPUT,NML=RADF,END=9,ERR=10,IOSTAT=IOS)
      N_RADF = N_RADF + 1
      10 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with RADF number ',N_RADF+1
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO COUNT_RADF_LOOP
   9 CONTINUE

   IF (N_RADF==0) CYCLE MESH_LOOP

   ALLOCATE(M%RAD_FILE(N_RADF),STAT=IZERO) ; CALL ChkMemErr('READ','RAD_FILE',IZERO)
   CALL POINT_TO_MESH(NM)  ! Reset the pointers after the allocation

   N = 0

   REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
   RADF_LOOP: DO NN=1,N_RADF
      XB=-1.E9_EB
      I_STEP = 1
      J_STEP = 1
      K_STEP = 1
      CALL CHECKREAD('RADF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT RADF_LOOP
      READ(LU_INPUT,RADF)

      CALL CHECK_XB(XB)

      XB(1) = MAX(XB(1),XS)
      XB(2) = MIN(XB(2),XF)
      XB(3) = MAX(XB(3),YS)
      XB(4) = MIN(XB(4),YF)
      XB(5) = MAX(XB(5),ZS)
      XB(6) = MIN(XB(6),ZF)

      ! Reject a block if it is beyond the bounds of the current mesh

      IF (XB(1)>XF .OR. XB(2)<XS .OR. XB(3)>YF .OR. XB(4)<YS .OR. XB(5)>ZF .OR. XB(6)<ZS) THEN
         N_RADF = N_RADF - 1
         CYCLE RADF_LOOP
      ENDIF

      N = N+1

      RF=>RAD_FILE(N)

      RF%I_STEP = I_STEP
      RF%J_STEP = J_STEP
      RF%K_STEP = K_STEP

      DO I=0,IBP1    ; IF (XC(I)<XB(1)) RF%I1 = I+1 ; ENDDO
      DO I=IBP1,0,-1 ; IF (XB(2)<XC(I)) RF%I2 = I-1 ; ENDDO
      DO J=0,JBP1    ; IF (YC(J)<XB(3)) RF%J1 = J+1 ; ENDDO
      DO J=JBP1,0,-1 ; IF (XB(4)<YC(J)) RF%J2 = J-1 ; ENDDO
      DO K=0,KBP1    ; IF (ZC(K)<XB(5)) RF%K1 = K+1 ; ENDDO
      DO K=KBP1,0,-1 ; IF (XB(6)<ZC(K)) RF%K2 = K-1 ; ENDDO

      RF%N_POINTS = 0
      DO K=RF%K1,RF%K2,RF%K_STEP
         DO J=RF%J1,RF%J2,RF%J_STEP
            DO I=RF%I1,RF%I2,RF%I_STEP
               RF%N_POINTS = RF%N_POINTS + 1  ! Just count the total number of points to evaluate
            ENDDO
         ENDDO
      ENDDO

   ENDDO RADF_LOOP

ENDDO MESH_LOOP

END SUBROUTINE READ_RADF

!> \brief Read the BNDF (BouNDary File) namelist lines

SUBROUTINE READ_BNDF

USE DEVICE_VARIABLES
USE COMP_FUNCTIONS, ONLY : CHANGE_UNITS
INTEGER :: N, DEBUG
LOGICAL :: CELL_CENTERED
CHARACTER(LABEL_LENGTH) :: QUANTITY,PROP_ID,SPEC_ID,PART_ID,TEMPORAL_STATISTIC,DUMMY='null',MATL_ID
NAMELIST /BNDF/ CELL_CENTERED,DEBUG,FYI,MATL_ID,PART_ID,PROP_ID,QUANTITY,SPEC_ID,TEMPORAL_STATISTIC
TYPE(BOUNDARY_FILE_TYPE), POINTER :: BF=>NULL()

N_BNDF = 0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_BNDF_LOOP: DO
   CALL CHECKREAD('BNDF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_BNDF_LOOP
   READ(LU_INPUT,NML=BNDF,END=209,ERR=210,IOSTAT=IOS)
   N_BNDF = N_BNDF + 1
   210 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with BNDF number ',N_BNDF+1
         CALL SHUTDOWN(MESSAGE) ; RETURN
       ENDIF
ENDDO COUNT_BNDF_LOOP
209 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

ALLOCATE(BOUNDARY_FILE(N_BNDF),STAT=IZERO)
CALL ChkMemErr('READ','BOUNDARY_FILE',IZERO)

BNDF_TIME_INTEGRALS = 0

READ_BNDF_LOOP: DO N=1,N_BNDF
   BF => BOUNDARY_FILE(N)
   DEBUG=0
   CELL_CENTERED = .FALSE.
   MATL_ID  = 'null'
   PART_ID  = 'null'
   PROP_ID  = 'null'
   SPEC_ID  = 'null'
   TEMPORAL_STATISTIC = 'null'
   QUANTITY = 'WALL_TEMPERATURE'
   CALL CHECKREAD('BNDF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT READ_BNDF_LOOP
   READ(LU_INPUT,BNDF)

   IF (TRIM(QUANTITY)=='AMPUA_Z' .OR. TRIM(QUANTITY)=='CPUA_Z' .OR. TRIM(QUANTITY)=='MPUA_Z') THEN
      IF (N_LP_ARRAY_INDICES == 0) THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR(962): BNDF ',N,' CPUA_Z, MPUA_Z, and AMPUA_Z require liquid droplets.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ELSE
         IF (.NOT. ALL(LAGRANGIAN_PARTICLE_CLASS%LIQUID_DROPLET)) THEN
               WRITE(MESSAGE,'(A,I0,A)') 'ERROR(962): BNDF ',N,' CPUA_Z, MPUA_Z, and AMPUA_Z require liquid droplets.'
               CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF
   ENDIF

   ! Look to see if output QUANTITY exists

   CALL GET_QUANTITY_INDEX(BF%SMOKEVIEW_LABEL,BF%SMOKEVIEW_BAR_LABEL,BF%INDEX,I_DUM(1), &
                           BF%Y_INDEX,BF%Z_INDEX,BF%PART_INDEX,I_DUM(2),I_DUM(3),I_DUM(4),I_DUM(5),'BNDF', &
                           QUANTITY,'null',SPEC_ID,PART_ID,'null','null','null',MATL_ID,-1._EB,I_DUM(6))
   BF%MATL_ID = MATL_ID

   BF%UNITS = OUTPUT_QUANTITY(BF%INDEX)%UNITS

   ! Assign miscellaneous attributes to the boundary file

   BF%DEBUG = DEBUG
   BF%CELL_CENTERED = CELL_CENTERED

   ! Check to see if PROP_ID exists

   BF%PROP_INDEX = 0
   IF (PROP_ID/='null')  CALL GET_PROPERTY_INDEX(BF%PROP_INDEX,'BNDF',PROP_ID)

   ! Check to see if the QUANTITY is to be time integrated

   IF (TEMPORAL_STATISTIC=='TIME INTEGRAL') THEN
      BNDF_TIME_INTEGRALS = BNDF_TIME_INTEGRALS + 1
      BF%TIME_INTEGRAL_INDEX = BNDF_TIME_INTEGRALS
      CALL CHANGE_UNITS(QUANTITY,BF%UNITS,DUMMY,TEMPORAL_STATISTIC,LU_ERR)
   ENDIF

ENDDO READ_BNDF_LOOP
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

END SUBROUTINE READ_BNDF


!> \brief Read the SM3D (Smoke3D File) namelist lines

SUBROUTINE READ_SM3D

CHARACTER(LABEL_LENGTH) :: QUANTITY,SPEC_ID
INTEGER ::  N,N_SMOKE3D_USER_SPECIFIED,N_SMOKE3D_RESERVED
NAMELIST /SM3D/ FYI,QUANTITY,SPEC_ID
TYPE(SMOKE3D_TYPE), POINTER :: S3

IF (.NOT.SMOKE3D) RETURN

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
N_SMOKE3D_USER_SPECIFIED = 0
COUNT_SM3D_LOOP: DO
   CALL CHECKREAD('SM3D',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_SM3D_LOOP
   READ(LU_INPUT,NML=SM3D,END=209,ERR=210,IOSTAT=IOS)
   N_SMOKE3D_USER_SPECIFIED = N_SMOKE3D_USER_SPECIFIED + 1
   210 IF (IOS>0) THEN
         WRITE(MESSAGE,'(A,I0)') 'ERROR(101): Problem with SM3D number ',N_SMOKE3D+1
         CALL SHUTDOWN(MESSAGE) ; RETURN
       ENDIF
ENDDO COUNT_SM3D_LOOP
209 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_REACTIONS>0) THEN
   N_SMOKE3D_RESERVED = 3
ELSE
   N_SMOKE3D_RESERVED = 0
ENDIF

N_SMOKE3D = N_SMOKE3D_USER_SPECIFIED + N_SMOKE3D_RESERVED

IF (N_SMOKE3D==0) RETURN

ALLOCATE(SMOKE3D_FILE(N_SMOKE3D),STAT=IZERO)
CALL ChkMemErr('READ','SMOKE3D_FILE',IZERO)

READ_SM3D_LOOP: DO N=1,N_SMOKE3D

   S3 => SMOKE3D_FILE(N)
   SPEC_ID  = 'null'
   IF (N<=N_SMOKE3D_RESERVED) THEN
      IF (N==1) THEN ; QUANTITY='DENSITY' ; SPEC_ID='SOOT' ; ENDIF
      IF (N==2) THEN ; QUANTITY='HRRPUV' ; ENDIF
      IF (N==3) THEN ; QUANTITY='TEMPERATURE' ; ENDIF
   ELSE
      CALL CHECKREAD('SM3D',LU_INPUT,IOS) ; IF (STOP_STATUS==SETUP_STOP) RETURN
      IF (IOS==1) EXIT READ_SM3D_LOOP
      READ(LU_INPUT,SM3D)
   ENDIF

   ! Look to see if output QUANTITY exists

   CALL GET_QUANTITY_INDEX(S3%SMOKEVIEW_LABEL,S3%SMOKEVIEW_BAR_LABEL,S3%QUANTITY_INDEX,I_DUM(1), &
                           S3%Y_INDEX,S3%Z_INDEX,I_DUM(2),I_DUM(3),I_DUM(4),I_DUM(5),I_DUM(6),'SMOKE3D', &
                           QUANTITY,'null',SPEC_ID,'null','null','null','null','null',-1._EB,I_DUM(7))

   IF (S3%Y_INDEX>0) THEN
      S3%MASS_EXTINCTION_COEFFICIENT = SPECIES(S3%Y_INDEX)%MASS_EXTINCTION_COEFFICIENT
   ELSEIF (S3%Z_INDEX >= 0) THEN
      S3%MASS_EXTINCTION_COEFFICIENT = SPECIES_MIXTURE(S3%Z_INDEX)%MASS_EXTINCTION_COEFFICIENT
   ELSE
      S3%MASS_EXTINCTION_COEFFICIENT = 0._EB
   ENDIF

   S3%UNITS = OUTPUT_QUANTITY(S3%QUANTITY_INDEX)%UNITS

   IF (SPEC_ID/='null') THEN
      S3%DISPLAY_TYPE = 'GAS'
   ELSEIF (QUANTITY=='HRRPUV') THEN
      S3%DISPLAY_TYPE = 'FIRE'
   ELSE
      S3%DISPLAY_TYPE = 'TEMPERATURE'
   ENDIF

ENDDO READ_SM3D_LOOP

END SUBROUTINE READ_SM3D


!> \brief Check for existing SURF names

SUBROUTINE CHECK_SURF_NAME(NAME,EXISTS)

LOGICAL, INTENT(OUT) :: EXISTS
CHARACTER(*), INTENT(IN) :: NAME
INTEGER :: NS

EXISTS = .FALSE.
DO NS=0,N_SURF
   IF (NAME==SURFACE(NS)%ID) EXISTS = .TRUE.
ENDDO

END SUBROUTINE CHECK_SURF_NAME


!> \brief Define the index and other properties of output quantities

SUBROUTINE GET_QUANTITY_INDEX(SMOKEVIEW_LABEL,SMOKEVIEW_BAR_LABEL,OUTPUT_INDEX,OUTPUT2_INDEX, &
                              Y_INDEX,Z_INDEX,PART_INDEX,DUCT_INDEX,NODE_INDEX,REAC_INDEX,MATL_INDEX,OUTTYPE, &
                              QUANTITY,QUANTITY2,SPEC_ID_IN,PART_ID,DUCT_ID,NODE_ID,REAC_ID,MATL_ID,CELL_L,&
                              DUCT_CELL_INDEX,SLICETYPE)
USE MISC_FUNCTIONS, ONLY: GET_SPEC_OR_SMIX_INDEX
CHARACTER(*), INTENT(INOUT) :: QUANTITY
CHARACTER(*), INTENT(OUT) :: SMOKEVIEW_LABEL,SMOKEVIEW_BAR_LABEL
CHARACTER(*) :: SPEC_ID_IN,PART_ID,DUCT_ID,NODE_ID
REAL(EB) :: CELL_L
CHARACTER(LABEL_LENGTH) :: SPEC_ID
CHARACTER(*), INTENT(IN) :: OUTTYPE,QUANTITY2,REAC_ID,MATL_ID
CHARACTER(*), OPTIONAL, INTENT(IN) :: SLICETYPE
INTEGER, INTENT(OUT) :: OUTPUT_INDEX,Y_INDEX,Z_INDEX,PART_INDEX,DUCT_INDEX,NODE_INDEX,REAC_INDEX,OUTPUT2_INDEX,MATL_INDEX,&
                        DUCT_CELL_INDEX
INTEGER :: ND,NS,NN,NR,N_PLUS,N_MINUS

! Backward compatibility

IF (QUANTITY=='oxygen') THEN
   QUANTITY    = 'VOLUME FRACTION'
   SPEC_ID_IN  = 'OXYGEN'
ENDIF
IF (QUANTITY=='carbon monoxide') THEN
   QUANTITY    = 'VOLUME FRACTION'
   SPEC_ID_IN  = 'CARBON MONOXIDE'
ENDIF
IF (QUANTITY=='carbon dioxide') THEN
   QUANTITY    = 'VOLUME FRACTION'
   SPEC_ID_IN  = 'CARBON DIOXIDE'
ENDIF
IF (QUANTITY=='soot') THEN
   QUANTITY    = 'VOLUME FRACTION'
   SPEC_ID_IN  = 'SOOT'
ENDIF
IF (QUANTITY=='soot density') THEN
   QUANTITY    = 'DENSITY'
   SPEC_ID_IN  = 'SOOT'
ENDIF
IF (QUANTITY=='fuel') THEN
   QUANTITY    = 'VOLUME FRACTION'
   WRITE(SPEC_ID_IN,'(A)') REACTION(1)%FUEL
ENDIF

IF (TRIM(QUANTITY)/='null') THEN ! If QUANTITY happens to be undefined with the following loop it ends up being
                                 ! erroneously set to the largest negative ND with value different than 'null'.
   DO ND=-N_OUTPUT_QUANTITIES,N_OUTPUT_QUANTITIES
      IF (QUANTITY==OUTPUT_QUANTITY(ND)%OLD_NAME) QUANTITY = OUTPUT_QUANTITY(ND)%NAME
   ENDDO
ENDIF

! Initialize indices

Y_INDEX = -1
Z_INDEX = -1

SPEC_ID = SPEC_ID_IN

IF (QUANTITY=='OPTICAL DENSITY'         .AND. SPEC_ID=='null') SPEC_ID='SOOT'
IF (QUANTITY=='EXTINCTION COEFFICIENT'  .AND. SPEC_ID=='null') SPEC_ID='SOOT'
IF (QUANTITY=='AEROSOL VOLUME FRACTION' .AND. SPEC_ID=='null') SPEC_ID='SOOT'
IF (QUANTITY=='VISIBILITY'              .AND. SPEC_ID=='null') SPEC_ID='SOOT'

PART_INDEX = 0
DUCT_INDEX = 0
DUCT_CELL_INDEX = 0
NODE_INDEX = 0
OUTPUT2_INDEX = 0
REAC_INDEX = 0
MATL_INDEX = 0

! Look for the appropriate SPEC or SMIX index

IF (SPEC_ID/='null') THEN
   CALL GET_SPEC_OR_SMIX_INDEX(SPEC_ID,Y_INDEX,Z_INDEX)
   IF (QUANTITY=='AEROSOL VOLUME FRACTION' .AND. SPEC_ID/='SOOT') THEN
      IF (Z_INDEX<0) THEN
            WRITE(MESSAGE,'(A,A,A)')  'ERROR: SPEC_ID ',TRIM(SPEC_ID),' for AEROSOL VOLUME FRACTION must be a tracked species'
            CALL SHUTDOWN(MESSAGE) ; RETURN
      ELSE
         IF (SPECIES_MIXTURE(Z_INDEX)%SINGLE_SPEC_INDEX<0) THEN
            WRITE(MESSAGE,'(A,A,A)')  'ERROR: SPEC_ID ',TRIM(SPEC_ID),' for AEROSOL VOLUME FRACTION cannot be a lumped species'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ELSE
            IF (.NOT. SPECIES_MIXTURE(Z_INDEX)%DEPOSITING) THEN
               WRITE(MESSAGE,'(A,A,A)')  'ERROR: SPEC_ID ',TRIM(SPEC_ID),' for AEROSOL VOLUME FRACTION is not an AEROSOL'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   IF (Z_INDEX>=0  .AND. Y_INDEX>=1) THEN
      IF(TRIM(QUANTITY)=='DIFFUSIVITY') THEN
         Y_INDEX=-999
      ELSE
         Z_INDEX=-999
      ENDIF
   ENDIF
   IF (Z_INDEX<0 .AND. Y_INDEX<1) THEN
      IF (OUTTYPE=='SMOKE3D') THEN
         OUTPUT_INDEX = 0
         RETURN
      ELSE
         WRITE(MESSAGE,'(A,A,A,A)')  'ERROR: SPEC_ID ',TRIM(SPEC_ID),' is not explicitly specified for QUANTITY ',TRIM(QUANTITY)
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDIF
ENDIF

IF (PRESENT(SLICETYPE)) THEN
   IF( (TRIM(SLICETYPE)=='CUT_CELLS' .OR. TRIM(SLICETYPE)=='EXIMBND_FACES') .AND. TRIM(QUANTITY)=='null') THEN
      OUTPUT_INDEX = 0
      RETURN
   ENDIF
ENDIF

! Assign HVAC indexes

IF (DUCT_ID/='null') THEN
   DO ND = 1, N_DUCTS
      IF (DUCT_ID==DUCT(ND)%ID) THEN
         DUCT_INDEX = ND
         EXIT
      ENDIF
   ENDDO
ENDIF

IF (CELL_L > 0._EB) THEN
   IF (DUCT_ID == 'null') THEN
      WRITE(MESSAGE,'(A,A,A,A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a DUCT_ID'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ELSEIF (CELL_L > DUCT(DUCT_INDEX)%LENGTH) THEN
      WRITE(MESSAGE,'(A,A,A,A)')  'ERROR: CELL_L used for output QUANTITY ',TRIM(QUANTITY),' is outside of DUCT_ID ',&
      TRIM(DUCT_ID)
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   DUCT_CELL_INDEX = MAX(1,NINT(CELL_L/DUCT(DUCT_INDEX)%LENGTH*DUCT(DUCT_INDEX)%N_CELLS))
ENDIF

IF (NODE_ID/='null') THEN
   DO NN = 1, N_DUCTNODES
      IF (NODE_ID==DUCTNODE(NN)%ID) THEN
         NODE_INDEX = NN
         EXIT
      ENDIF
   ENDDO
ENDIF

IF (TRIM(QUANTITY)=='FILTER LOADING') THEN
   Y_INDEX = -999
   DO NS = 1,N_TRACKED_SPECIES
      IF (TRIM(SPECIES_MIXTURE(NS)%ID)==TRIM(SPEC_ID)) THEN
         Z_INDEX = NS
         EXIT
      ENDIF
   ENDDO
   IF (Z_INDEX<0) THEN
      WRITE(MESSAGE,'(A,A,A)')  'ERROR: FILTER LOADING. ',TRIM(SPEC_ID),' is not a tracked species'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDIF


IF (TRIM(QUANTITY)=='EQUILIBRIUM VAPOR FRACTION' .OR. TRIM(QUANTITY)=='EQUILIBRIUM TEMPERATURE') THEN
   Y_INDEX = -999
   DO NS = 1,N_TRACKED_SPECIES
      IF (TRIM(SPECIES_MIXTURE(NS)%ID)==TRIM(SPEC_ID)) THEN
         Z_INDEX = NS
         EXIT
      ENDIF
   ENDDO
   IF (Z_INDEX<0) THEN
      WRITE(MESSAGE,'(A,A,A)')  'ERROR: EQUILIBRIUM VAPOR FRACTION. ',TRIM(SPEC_ID),' is not a tracked species'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   IF (.NOT. SPECIES_MIXTURE(Z_INDEX)%EVAPORATING) THEN
      WRITE(MESSAGE,'(A,A,A)')  'ERROR: EQUILIBRIUM VAPOR FRACTION. ',TRIM(SPEC_ID),' is not an evaporating species'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDIF

IF (TRIM(QUANTITY)=='MIXTURE FRACTION') THEN
   DO NR = 1, N_REACTIONS
      IF (REACTION(NR)%SIMPLE_CHEMISTRY) CYCLE
      N_PLUS = 0
      N_MINUS = 0
      DO NN = 1,N_TRACKED_SPECIES
         IF (REACTION(NR)%NU(NN) > 0)  THEN
            N_PLUS = N_PLUS + 1
            Z_INDEX = NN
         ELSEIF (REACTION(NR)%NU(NN) < 0)  THEN
            N_MINUS = N_MINUS + 1
         ENDIF
      ENDDO
      IF (N_PLUS/=1 .AND. N_MINUS/=2) THEN
         WRITE(MESSAGE,'(A,I0,A)') 'ERROR: REAC ',NR,' MIXTURE FRACTION requires reaction of the form A + B -> C'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
   ENDDO
ENDIF

IF (QUANTITY=='ADVECTIVE MASS FLUX X' .OR. QUANTITY=='ADVECTIVE MASS FLUX Y' .OR.  QUANTITY=='ADVECTIVE MASS FLUX Z' .OR. &
    QUANTITY=='DIFFUSIVE MASS FLUX X' .OR. QUANTITY=='DIFFUSIVE MASS FLUX Y' .OR.  QUANTITY=='DIFFUSIVE MASS FLUX Z' .OR. &
    QUANTITY=='TOTAL MASS FLUX X'     .OR. QUANTITY=='TOTAL MASS FLUX Y'     .OR.  QUANTITY=='TOTAL MASS FLUX Z'     .OR. &
    QUANTITY=='TOTAL MASS FLUX WALL') STORE_SPECIES_FLUX = .TRUE.

IF (TRIM(QUANTITY)=='HRRPUV REAC') THEN
   IF (TRIM(REAC_ID)=='null') THEN
      WRITE(MESSAGE,'(A)') 'ERROR: HRRPUV REAC requires a REAC_ID'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
   DO NR = 1,N_REACTIONS
      IF (TRIM(REAC_ID)==TRIM(REACTION(NR)%ID)) REAC_INDEX = NR
   ENDDO
   IF (REAC_INDEX==0) THEN
      WRITE(MESSAGE,'(3A)') 'ERROR: REAC_ID ',TRIM(REAC_ID),' not found for HRRPUV REAC'
      CALL SHUTDOWN(MESSAGE) ; RETURN
   ENDIF
ENDIF

! Assigne MATL_INDEX when MATL_ID is specified

IF (MATL_ID/='null') THEN
   DO NN = 1,N_MATL
      IF (TRIM(MATL_ID)==TRIM(MATERIAL(NN)%ID)) MATL_INDEX = NN
   ENDDO
ENDIF

! Assign PART_INDEX when PART_ID is specified

IF (PART_ID/='null') THEN
   DO NS=1,N_LAGRANGIAN_CLASSES
      IF (PART_ID==LAGRANGIAN_PARTICLE_CLASS(NS)%ID) THEN
         PART_INDEX = NS
         EXIT
      ENDIF
   ENDDO
ENDIF

! Loop over all possible output quantities and assign an index number to match the desired QUANTITY
DO ND=-N_OUTPUT_QUANTITIES,N_OUTPUT_QUANTITIES
   IF (OUTPUT_QUANTITY(ND)%NAME=='null') CYCLE
   IF (QUANTITY2==OUTPUT_QUANTITY(ND)%NAME) THEN

      OUTPUT2_INDEX=ND

      IF (OUTPUT_QUANTITY(ND)%SPEC_ID_REQUIRED .AND. (Y_INDEX<1 .AND. Z_INDEX<0)) THEN
         WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY2 ',TRIM(QUANTITY2),' requires a SPEC_ID'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      ! QUANTITY2 only works with SLCF at the moment
      IF (.NOT.OUTPUT_QUANTITY(ND)%SLCF_APPROPRIATE) THEN
          WRITE(MESSAGE,'(3A)')  'ERROR: The QUANTITY2 ',TRIM(QUANTITY2),' is not appropriate for SLCF'
          CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

   ENDIF
ENDDO

QUANTITY_INDEX_LOOP: DO ND=-N_OUTPUT_QUANTITIES,N_OUTPUT_QUANTITIES

   QUANTITY_IF:IF (QUANTITY==OUTPUT_QUANTITY(ND)%NAME) THEN

      OUTPUT_INDEX = ND

      IF (OUTPUT_QUANTITY(ND)%QUANTITY2_REQUIRED .AND. OUTPUT2_INDEX==0) THEN
         WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a QUANTITY2'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%SPEC_ID_REQUIRED .AND. (Y_INDEX<1 .AND. Z_INDEX<0)) THEN
         IF (SPEC_ID=='null') THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a SPEC_ID'
         ELSE
            WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),'. SPEC_ID ',TRIM(SPEC_ID),' not found.'
         ENDIF
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%MATL_ID_REQUIRED .AND. MATL_INDEX < 1) THEN
         IF (MATL_ID=='null') THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a MATL_ID'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ELSE
            WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' MATL_ID ', TRIM(MATL_ID),' not found.'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%PART_ID_REQUIRED .AND. PART_INDEX<1) THEN
         IF (PART_ID=='null') THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a PART_ID'
         ELSE
            WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),'. PART_ID ',TRIM(PART_ID),' not found.'
         ENDIF
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%DUCT_ID_REQUIRED .AND. DUCT_INDEX<1) THEN
         IF (DUCT_ID=='null') THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a DUCT_ID'
         ELSE
            WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),'. DUCT_ID ',TRIM(DUCT_ID),' not found.'
         ENDIF
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%CELL_L_REQUIRED .AND. DUCT_CELL_INDEX<1) THEN
         IF (DUCT_ID=='null') THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a positive CELL_L'
         ELSE
            WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' in DUCT_ID ',TRIM(DUCT_ID),&
            ' requires a positive CELL_L'
         ENDIF
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%CELL_L_REQUIRED .AND. (HVAC_MASS_TRANSPORT .NEQV. .TRUE.)) THEN
         WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' in DUCT_ID ',TRIM(DUCT_ID),&
         ' requires HVAC_MASS_TRANSPORT to be active.'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (OUTPUT_QUANTITY(ND)%NODE_ID_REQUIRED .AND. NODE_INDEX<1) THEN
         IF (NODE_ID=='null') THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),' requires a NODE_ID'
         ELSE
            WRITE(MESSAGE,'(5A)')  'ERROR: Output QUANTITY ',TRIM(QUANTITY),'. NODE_ID ',TRIM(NODE_ID),' not found.'
         ENDIF
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (( QUANTITY=='RELATIVE HUMIDITY' .OR. QUANTITY=='HUMIDITY').AND. H2O_INDEX==0) THEN
         WRITE(MESSAGE,'(A)')  'ERROR: RELATIVE HUMIDITY and HUMIDITY require SPEC=WATER VAPOR'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      END IF

      IF (TRIM(QUANTITY)=='DIFFUSIVITY' .AND. SIM_MODE==DNS_MODE .AND. Z_INDEX < 0) THEN
         WRITE(MESSAGE,'(A)')  'ERROR: DIFFUSIVITY requires a tracked species SPEC_ID when using DNS'
         CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF

      IF (TRIM(QUANTITY)=='SURFACE DEPOSITION') THEN
         IF (Z_INDEX==0) THEN
            WRITE(MESSAGE,'(A)')  'ERROR: Cannot select background species for deposition'
            CALL SHUTDOWN(MESSAGE) ; RETURN
         ENDIF
         IF (Y_INDEX > 0) THEN
            IF (SPECIES(Y_INDEX)%MODE /= AEROSOL_SPECIES) THEN
               WRITE(MESSAGE,'(A,A,A)')'ERROR: SURFACE DEPOSITION for ',TRIM(SPEC_ID),' is not an aerosol species'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (SPECIES(Y_INDEX)%AWM_INDEX < 0) THEN
               N_SURFACE_DENSITY_SPECIES = N_SURFACE_DENSITY_SPECIES + 1
               SPECIES(Y_INDEX)%AWM_INDEX = N_SURFACE_DENSITY_SPECIES
            ENDIF
         ELSE
            IF(.NOT. SPECIES_MIXTURE(Z_INDEX)%DEPOSITING) THEN
               WRITE(MESSAGE,'(A,A,A)')'ERROR: SURFACE DEPOSITION for ',TRIM(SPEC_ID),' is not an aerosol tracked species'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (SPECIES_MIXTURE(Z_INDEX)%AWM_INDEX < 0) THEN
               N_SURFACE_DENSITY_SPECIES = N_SURFACE_DENSITY_SPECIES + 1
               SPECIES_MIXTURE(Z_INDEX)%AWM_INDEX = N_SURFACE_DENSITY_SPECIES
            ENDIF
         ENDIF
      ENDIF

      IF (TRIM(QUANTITY)=='MPUV_Z' .OR. TRIM(QUANTITY)=='ADD_Z' .OR. TRIM(QUANTITY)=='ADT_Z' .OR. TRIM(QUANTITY)=='ADA_Z' .OR. &
          TRIM(QUANTITY)=='QABS_Z' .OR. TRIM(QUANTITY)=='QSCA_Z' .OR. TRIM(QUANTITY)=='MPUA_Z' .OR. TRIM(QUANTITY)=='CPUA_Z' .OR. &
          TRIM(QUANTITY)=='AMPUA_Z') THEN
         IF (N_LAGRANGIAN_CLASSES==0) THEN
            WRITE(MESSAGE,'(3A)')  'ERROR: The QUANTITY ',TRIM(QUANTITY),' requires liquid droplets'
            CALL SHUTDOWN(MESSAGE)             ; RETURN
         ELSE
            IF (.NOT. ALL(LAGRANGIAN_PARTICLE_CLASS%LIQUID_DROPLET)) THEN
               WRITE(MESSAGE,'(3A)')  'ERROR: The QUANTITY ',TRIM(QUANTITY),' requires liquid droplets'
               CALL SHUTDOWN(MESSAGE)             ; RETURN
            ENDIF
         ENDIF
      ENDIF

      SELECT CASE (TRIM(OUTTYPE))
         CASE ('SLCF')
            ! Throw out bad slices
            IF (.NOT. OUTPUT_QUANTITY(ND)%SLCF_APPROPRIATE) THEN
               WRITE(MESSAGE,'(3A)')  'ERROR: The QUANTITY ',TRIM(QUANTITY),' is not appropriate for SLCF'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         CASE ('DEVC')
            IF (.NOT.OUTPUT_QUANTITY(ND)%DEVC_APPROPRIATE) THEN
               WRITE(MESSAGE,'(3A)')  'ERROR: The QUANTITY ',TRIM(QUANTITY),' is not appropriate for DEVC'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (QUANTITY=='AMPUA' .OR. QUANTITY=='AMPUA_Z') ACCUMULATE_WATER = .TRUE.
         CASE ('PART')
            IF (.NOT. OUTPUT_QUANTITY(ND)%PART_APPROPRIATE) THEN
               WRITE(MESSAGE,'(3A)') 'ERROR: ',TRIM(QUANTITY),' is not a particle output QUANTITY'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         CASE ('BNDF')
            IF (.NOT. OUTPUT_QUANTITY(ND)%BNDF_APPROPRIATE) THEN
               WRITE(MESSAGE,'(3A)')  'ERROR: The QUANTITY ',TRIM(QUANTITY),' is not appropriate for BNDF'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
            IF (QUANTITY=='AMPUA' .OR. QUANTITY=='AMPUA_Z') ACCUMULATE_WATER = .TRUE.
         CASE('ISOF')
            IF (.NOT.OUTPUT_QUANTITY(ND)%ISOF_APPROPRIATE) THEN
               WRITE(MESSAGE,'(3A)')  'ERROR: ISOF quantity ',TRIM(QUANTITY),' not appropriate for isosurface'
               CALL SHUTDOWN(MESSAGE) ; RETURN
            ENDIF
         CASE ('PLOT3D')
             IF (OUTPUT_QUANTITY(ND)%SOLID_PHASE) THEN
                WRITE(MESSAGE,'(5A)') 'ERROR: ',TRIM(OUTTYPE),'_QUANTITY ',TRIM(QUANTITY), ' not appropriate for gas phase'
                CALL SHUTDOWN(MESSAGE) ; RETURN
             ENDIF
             IF (.NOT.OUTPUT_QUANTITY(ND)%SLCF_APPROPRIATE) THEN
                WRITE(MESSAGE,'(5A)') 'ERROR: ',TRIM(OUTTYPE),'_QUANTITY ',TRIM(QUANTITY), ' not appropriate for Plot3D'
                CALL SHUTDOWN(MESSAGE) ; RETURN
             ENDIF
         CASE DEFAULT
      END SELECT

      ! Assign Smokeview Label

      IF (Z_INDEX>=0) THEN
         IF (TRIM(QUANTITY)=='MIXTURE FRACTION') THEN
            SMOKEVIEW_LABEL = TRIM(QUANTITY)
            SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)
         ELSE
            SMOKEVIEW_LABEL = TRIM(SPECIES_MIXTURE(Z_INDEX)%ID)//' '//TRIM(QUANTITY)
            SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)//'_'//TRIM(SPECIES_MIXTURE(Z_INDEX)%ID)
         ENDIF
      ELSEIF (Y_INDEX>0) THEN
         SMOKEVIEW_LABEL = TRIM(SPECIES(Y_INDEX)%ID)//' '//TRIM(QUANTITY)
         SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)//'_'//TRIM(SPECIES(Y_INDEX)%FORMULA)
      ELSEIF (PART_INDEX>0) THEN
         SMOKEVIEW_LABEL = TRIM(LAGRANGIAN_PARTICLE_CLASS(PART_INDEX)%ID)//' '//TRIM(QUANTITY)
         SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)
      ELSEIF (OUTPUT2_INDEX/=0) THEN
         SMOKEVIEW_LABEL = TRIM(QUANTITY)//' '//TRIM(QUANTITY2)
         SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)//'_'//TRIM(OUTPUT_QUANTITY(OUTPUT2_INDEX)%SHORT_NAME)
      ELSEIF (REAC_INDEX/=0) THEN
         SMOKEVIEW_LABEL = TRIM(QUANTITY)//' '//TRIM(REACTION(REAC_INDEX)%ID)
         SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)//'_'//TRIM(REACTION(REAC_INDEX)%ID)
      ELSEIF (MATL_INDEX/=0) THEN
         SMOKEVIEW_LABEL = TRIM(QUANTITY)//' '//TRIM(MATERIAL(MATL_INDEX)%ID)
         SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)//'_'//TRIM(MATERIAL(MATL_INDEX)%ID)
      ELSE
         SMOKEVIEW_LABEL = TRIM(QUANTITY)
         SMOKEVIEW_BAR_LABEL = TRIM(OUTPUT_QUANTITY(ND)%SHORT_NAME)
      ENDIF

      RETURN
   ENDIF QUANTITY_IF

ENDDO QUANTITY_INDEX_LOOP

! If no match for desired QUANTITY is found, stop the job

WRITE(MESSAGE,'(5A)') 'ERROR: ',TRIM(OUTTYPE),' QUANTITY ',TRIM(QUANTITY), ' not found'
CALL SHUTDOWN(MESSAGE) ; RETURN

END SUBROUTINE GET_QUANTITY_INDEX


SUBROUTINE GET_PROPERTY_INDEX(P_INDEX,OUTTYPE,PROP_ID)

USE DEVICE_VARIABLES
CHARACTER(*), INTENT(IN) :: PROP_ID
CHARACTER(*), INTENT(IN) :: OUTTYPE
INTEGER, INTENT(INOUT) :: P_INDEX
INTEGER :: NN

DO NN=1,N_PROP
  IF (PROP_ID==PROPERTY(NN)%ID) THEN
     P_INDEX = NN
     SELECT CASE (TRIM(OUTTYPE))
        CASE ('SLCF')
        CASE ('DEVC')
        CASE ('PART')
        CASE ('OBST')
        CASE ('BNDF')
        CASE ('PLOT3D')
        CASE DEFAULT
     END SELECT
     RETURN
  ENDIF
ENDDO

WRITE(MESSAGE,'(5A)')  'ERROR: ',TRIM(OUTTYPE),' PROP_ID ',TRIM(PROP_ID),' not found'
CALL SHUTDOWN(MESSAGE) ; RETURN

END SUBROUTINE GET_PROPERTY_INDEX


!> \brief Read the CSVF namelist lines

SUBROUTINE READ_CSVF

USE OUTPUT_DATA
CHARACTER(FORMULA_LENGTH) :: SPECFILE,SPECFILE_BASE,SPECFILE_NM,TMPFILE,TMPFILE_BASE,TMPFILE_NM,UVWFILE,UVWFILE_BASE,UVWFILE_NM
INTEGER :: N_CSVF,NM,STRLEN
NAMELIST /CSVF/ SPECFILE,TMPFILE,UVWFILE

! Set defaults

UVWFILE  = 'null'
TMPFILE  = 'null'
SPECFILE = 'null'

N_CSVF=0
REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_CSVF_LOOP: DO
   CALL CHECKREAD('CSVF',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_CSVF_LOOP
   READ(LU_INPUT,NML=CSVF,END=16,ERR=17,IOSTAT=IOS)
   N_CSVF=N_CSVF+1
   16 IF (IOS>0) THEN ; CALL SHUTDOWN('ERROR: problem with CSVF line') ; RETURN ; ENDIF
ENDDO COUNT_CSVF_LOOP
17 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0

IF (N_CSVF==0) RETURN

! Allocate CSVFINFO array

ALLOCATE(CSVFINFO(NMESHES),STAT=IZERO)
CALL ChkMemErr('READ','CSVF',IZERO)

NMESHES_IF: IF (NMESHES==1) THEN

   IF (TRIM(UVWFILE)/='null') THEN
      CSVFINFO(1)%UVWFILE = UVWFILE
      UVW_RESTART = .TRUE.
   ENDIF

   IF (TRIM(TMPFILE)/='null') THEN
      CSVFINFO(1)%TMPFILE = TMPFILE
      TMP_RESTART = .TRUE.
   ENDIF

   IF (TRIM(SPECFILE)/='null') THEN
      CSVFINFO(1)%SPECFILE = SPECFILE
      SPEC_RESTART = .TRUE.
   ENDIF

ELSE NMESHES_IF

   ! Read UVWFILE per MESH

   IF (TRIM(UVWFILE)/='null') THEN
      STRLEN = LEN(TRIM(UVWFILE))
      WRITE(UVWFILE_BASE,'(A)') UVWFILE(1:STRLEN-5) ! this subtracts '1.csv' from the end of the UVWFILE input string
      DO NM=1,NMESHES
         IF (PROCESS(NM)/=MY_RANK) CYCLE
         WRITE(UVWFILE_NM,'(A,I0,A)') TRIM(UVWFILE_BASE),NM,'.csv'
         CSVFINFO(NM)%UVWFILE = UVWFILE_NM
      ENDDO
      UVW_RESTART = .TRUE.
   ENDIF

   ! Read TMPFILE per MESH

   IF (TRIM(TMPFILE)/='null') THEN
      STRLEN = LEN(TRIM(TMPFILE))
      WRITE(TMPFILE_BASE,'(A)') TMPFILE(1:STRLEN-5)
      DO NM=1,NMESHES
         IF (PROCESS(NM)/=MY_RANK) CYCLE
         WRITE(TMPFILE_NM,'(A,I0,A)') TRIM(TMPFILE_BASE),NM,'.csv'
         CSVFINFO(NM)%TMPFILE = TMPFILE_NM
      ENDDO
      TMP_RESTART = .TRUE.
   ENDIF

   ! Read SPECFILE per MESH

   IF (TRIM(SPECFILE)/='null') THEN
      STRLEN = LEN(TRIM(SPECFILE))
      WRITE(SPECFILE_BASE,'(A)') SPECFILE(1:STRLEN-5)
      DO NM=1,NMESHES
         IF (PROCESS(NM)/=MY_RANK) CYCLE
         WRITE(SPECFILE_NM,'(A,I0,A)') TRIM(SPECFILE_BASE),NM,'.csv'
         CSVFINFO(NM)%SPECFILE = SPECFILE_NM
      ENDDO
      SPEC_RESTART = .TRUE.
   ENDIF

ENDIF NMESHES_IF

END SUBROUTINE READ_CSVF


!> \brief Calculate the heat of vaporization of water

SUBROUTINE CALC_H2O_HV

USE PROPERTY_DATA, ONLY: THERMO_TABLE_GAS,THERMO_TABLE_LIQUID,GAS_PROPS
CHARACTER(LABEL_LENGTH) :: RADCAL_NAME
CHARACTER(FORMULA_LENGTH) :: FORMULA='null'
INTEGER :: I
REAL(EB) :: CP_G,CP_G_O,CP_L,CP_L_O,H_G,H_L,G_F,RCON,H_V,T_R,T_M,T_B,DENSITY,MU_LIQUID,K_LIQUID,BETA_LIQUID
REAL(EB) :: SIGMA=1._EB,EPSOK=1._EB,PR_GAS=1._EB,MW=1._EB,ATOM_COUNTS(118),H_F=-1.E30_EB
REAL(EB) :: H_V_1
LOGICAL :: LISTED,FUEL

RCON = R0/MW_H2O

CALL GAS_PROPS(WATER_INDEX,SIGMA,EPSOK,PR_GAS,MW,FORMULA,LISTED,ATOM_COUNTS,H_F,RADCAL_NAME)

H_G = 0
H_L = 0

DO I=1,I_MAX_TEMP
   CALL THERMO_TABLE_GAS (I,CP_G,WATER_INDEX,RCON,G_F)
   CALL THERMO_TABLE_LIQUID (I,CP_L,H_V,T_R,T_M,T_B,WATER_INDEX,FUEL,DENSITY,MU_LIQUID,K_LIQUID,BETA_LIQUID)
   IF (I==1) THEN
      CP_G_O = CP_G
      CP_L_O = CP_L
   ENDIF
   H_G = H_G + 0.5_EB*(CP_G+CP_G_O)
   H_L = H_L + 0.5_EB*(CP_L+CP_L_O)
   H_V_H2O(I) = H_G-H_L
   CP_G_O=CP_G
   CP_L_O=CP_L
END DO

I = INT(T_R)
H_V_1 = H_V_H2O(I) + (T_R-REAL(I,EB))*(H_V_H2O(I+1)-H_V_H2O(I))
H_V_H2O = H_V_H2O + H_V - H_V_1

END SUBROUTINE CALC_H2O_HV


SUBROUTINE REALLOCATE_HT3D_OBST(N1,N2)

TYPE(HT3D_OBST_TYPE), ALLOCATABLE, DIMENSION(:) :: DUMMY
INTEGER, INTENT(IN) :: N1,N2

ALLOCATE(DUMMY(1:N2))
DUMMY(1:N1) = HT3D_OBST(1:N1)
CALL MOVE_ALLOC(DUMMY,HT3D_OBST)

END SUBROUTINE REALLOCATE_HT3D_OBST

#if defined init_file_in
SUBROUTINE READ_IC
use coupled_files

integer:: N_iccd

NAMELIST /ICCD/ OBFile, ICFile
                

REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
COUNT_IC_LOOP: DO
   CALL CHECKREAD('ICCD',LU_INPUT,IOS)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
   IF (IOS==1) EXIT COUNT_IC_LOOP
   READ(LU_INPUT,NML=ICCD,END=9,ERR=10,IOSTAT=IOS)
   N_iccd = N_iccd + 1
   10 IF (IOS>0) THEN
      WRITE(MESSAGE,'(A,I0,A,I0)') 'ERROR(101): Problem with ICCD number ',N_iccd,', line number ',INPUT_FILE_LINE_NUMBER
      CALL SHUTDOWN(MESSAGE) ; RETURN
      ENDIF
ENDDO COUNT_IC_LOOP
9 REWIND(LU_INPUT) ; INPUT_FILE_LINE_NUMBER = 0
end subroutine read_ic
#endif

END MODULE READ_INPUT
