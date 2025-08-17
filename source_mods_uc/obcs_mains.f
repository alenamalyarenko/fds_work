 #read in BC - obcs_fields_load.F
       CALL GET_PERIODIC_INTERVAL(
     O                  iRecP, iRec0, iRec1, bWght, aWght,
     I                  externForcingCycle, externForcingPeriod,
     I                  deltaTClock, myTime, myThid )
 
 
 IF ( OBSuFile .NE. ' '  ) THEN
         CALL READ_REC_XZ_RL( OBSuFile,fp,Nr,OBSu0,iRec0,myIter,myThid )
        ENDIF
   
# interpolate BC

       IF ( OBSuFile .NE. ' '  ) CALL OBCS_TIME_INTERP_XZ(
     &      OBSu, OBSu0, OBSu1, aWght, bWght, myThid )        
        
        
        
        
        
CBOP
C     !ROUTINE: OBCS_TIME_INTERP_XZ
C     !INTERFACE:
      SUBROUTINE OBCS_TIME_INTERP_XZ(
     O     fld,
     I     fld0, fld1, aWght, bWght, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_TIME_INTERP_XZ
C     | o Interpolate between to records
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     aWght, bWght :: Interpolation weights
C     myThid       :: my Thread Id. number
      _RL fld (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL fld0(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL fld1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL aWght,bWght
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     bi,bj,i,j :: loop counters
      INTEGER bi,bj,i,k
CEOP
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k = 1, Nr
          DO i=1-OLx,sNx+OLx
           fld(i,k,bi,bj)   = bWght*fld0(i,k,bi,bj)
     &                       +aWght*fld1(i,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      RETURN
      END        
      
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XZ_RL(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_XZ_RL is a "front-end" interface to the low-level I/O
C routines.
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      INTEGER fPrec
      INTEGER nNz
      _RL field(1-OLx:sNx+OLx,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
      CHARACTER*(2) fType
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP

c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      useCurrentDir = .FALSE.
      fType='RL'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_SEC_XZ(
     I                      fName, fPrec, useCurrentDir,
     I                      fType, nNz, 1, nNz,
     O                      field, dummyRS,
     I                      iRec, myThid )
#endif
      RETURN
      END
      
      
C--  File mdsio_read_section.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o MDS_READ_SEC_XZ
C--   o MDS_READ_SEC_YZ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MDS_READ_SEC_XZ
C !INTERFACE:
      SUBROUTINE MDS_READ_SEC_XZ(
     I   fName,
     I   filePrec,
     I   useCurrentDir,
     I   arrType,
     I   kSize, kLo, kHi,
     O   fldRL, fldRS,
     I   irecord,
     I   myThid )

C !DESCRIPTION
C Arguments:
C
C fName       string  :: base name for file to read
C filePrec    integer :: number of bits per word in file (32 or 64)
C useCurrentDir(logic):: always read from the current directory (even if
C                        "mdsioLocalDir" is set)
C arrType     char(2) :: which array (fldRL/RS) to read into, either "RL" or "RS"
C kSize       integer :: size of second dimension, normally either 1 or Nr
C kLo         integer :: 1rst vertical level (of array fldRL/RS) to read-in
C kHi         integer :: last vertical level (of array fldRL/RS) to read-in
C fldRL         RL    :: array to read into if arrType="RL", fldRL(:,kSize,:,:)
C fldRS         RS    :: array to read into if arrType="RS", fldRS(:,kSize,:,:)
C irecord     integer :: record number to read
C myThid      integer :: thread identifier
C
C MDS_READ_SEC_XZ first checks to see IF the file "fName" exists, then
C if the file "fName.data" exists and finally the tiled files of the
C form "fName.xxx.yyy.data" exist.
C The precision of the file is decsribed by filePrec, set either
C  to floatPrec32 or floatPrec64. The char*(2) string arrType, either "RL"
C  or "RS", selects which array is filled in, either fldRL or fldRS.
C This routine reads vertical slices (X-Z) including the overlap region.
C irecord is the record number to be read and must be >= 1.
C The file data is stored in fldRL/RS *but* the overlaps are *not* updated.
C
C Created: 06/03/00 spk@ocean.mit.edu
CEOP

C !USES:
      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER kSize, kLo, kHi
      INTEGER irecord
      INTEGER myThid
C !OUTPUT PARAMETERS:
      _RL  fldRL(*)
      _RS  fldRS(*)

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK, MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName, pfName
      INTEGER IL, pIL, dUnit, nLev, irec
      INTEGER iG, jG, bi, bj, k, kL
      LOGICAL exst
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      LOGICAL globalFile, fileIsOpen
      INTEGER length_of_rec
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
       WRITE(msgBuf,'(A,I9.8)')
     &   ' MDS_READ_SEC_XZ: argument irecord = ',irecord
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDS_READ_SEC_XZ: Invalid value for irecord'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
      ENDIF

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
       pfName= fName
      ELSE
       WRITE(pfName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      ENDIF
      pIL=ILNBLNK( pfName )

C Set number of levels to read:
      nLev = kHi - kLo + 1

C Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      INQUIRE( file=dataFName, exist=exst )
      IF (exst) THEN
       globalFile = .TRUE.
      ENDIF


C If we are reading from a global file then we open it here
      IF (globalFile) THEN
       length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
       OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &       access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      ENDIF

C Loop over all tiles
      DO bj=1,nSy
       DO bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx
         jG=bj+(myYGlobalLo-1)/sNy
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         INQUIRE( file=dataFName, exist=exst )
C Of course, we only open the file IF the tile is "active"
C (This is a place-holder for the active/passive mechanism
         IF (exst) THEN
          
          length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
          OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          fileIsOpen=.FALSE.
         
         Print*,  ' MDS_READ_SEC_XZ: Files DO not exist'
          
         ENDIF
        ENDIF

        IF (fileIsOpen) THEN
         DO k=1,nLev
           kL = k + kLo - 1
           IF (globalFile) THEN

            iG = myXGlobalLo-1 + (bi-1)*sNx
            jG = (myYGlobalLo-1)/sNy + (bj-1)
            irec = 1 + INT(iG/sNx) + nSx*nPx*(k-1)
     &           + nSx*nPx*nLev*(irecord-1)

           ELSE
            iG = 0
            jG = 0
            irec = k + nLev*(irecord-1)
           ENDIF
           IF (filePrec .EQ. precFloat32) THEN
            READ(dUnit,rec=irec) r4seg
            IF (arrType .EQ. 'RS') THEN
             CALL MDS_SEG4toRS_2D( sNx,OLx,kSize,bi,bj,kL,.TRUE.,
     &                             r4seg,fldRS )
            ELSEIF (arrType .EQ. 'RL') THEN
             CALL MDS_SEG4toRL_2D( sNx,OLx,kSize,bi,bj,kL,.TRUE.,
     &                             r4seg,fldRL )
            ELSE
             WRITE(msgBuf,'(A)')
     &         ' MDS_READ_SEC_XZ: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
            ENDIF
           ELSEIF (filePrec .EQ. precFloat64) THEN
            READ(dUnit,rec=irec) r8seg
            IF (arrType .EQ. 'RS') THEN
             CALL MDS_SEG8toRS_2D(sNx,OLx,kSize,bi,bj,kL,.TRUE.,
     &                             r8seg,fldRS )
            ELSEIF (arrType .EQ. 'RL') THEN
             CALL MDS_SEG8toRL_2D(sNx,OLx,kSize,bi,bj,kL,.TRUE.,
     &                             r8seg,fldRL )
            ELSE
             WRITE(msgBuf,'(A)')
     &         ' MDS_READ_SEC_XZ: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
            ENDIF
           ELSE
            WRITE(msgBuf,'(A)')
     &        ' MDS_READ_SEC_XZ: illegal value for filePrec'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
           ENDIF
C End of k loop
         ENDDO
         IF (.NOT. globalFile) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
         ENDIF
        ENDIF
C End of bi,bj loops
       ENDDO
      ENDDO

C If global file was opened then close it
      IF (fileIsOpen .AND. globalFile) THEN
       CLOSE( dUnit )
       fileIsOpen = .FALSE.
      ENDIF

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      RETURN
      END
      
      
      
      
      
      
      
      
      
      SUBROUTINE GET_PERIODIC_INTERVAL(
     O               tRec0, tRec1, tRec2, wght1, wght2,
     I               cycleLength, recSpacing, deltaT,
     I               currentTime, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GET\_PERIODIC\_INTERVAL
C     | o Provide time-record indices arround current time
C     |   from a periodic, regularly spaced, time sequence
C     | o Extended to non-periodic, regularly spaced, time
C     |   sequence (case cycleLength=0) as in pkg/rbcs
C     *==========================================================*
C     | From a regularly-spaced sequence of time records
C     | this routine returns the index of the two records
C     | surrounding the current time and the record index of
C     | corresponding to the previous time-step ; also provides
C     | the weighting factor for a linear interpolation
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     cycleLength :: length of the periodic cycle (in s), zero if non-periodic
C     recSpacing  :: time record spacing
C     deltaT      :: time-step
C     currentTime :: current time
C     myThid      :: my Thread Id number
      _RL      cycleLength, recSpacing, deltaT, currentTime
      INTEGER  myThid
C     !OUTPUT PARAMETERS:
C     tRec0       :: time-record intex corresponding to the previous time-step
C     tRec1       :: 1rst time-record intex just before current time
C     tRec2       ::  2nd time-record intex just after current time
C     wght1       :: linear interpolation weight (applies to 1rst record)
C     wght2       :: linear interpolation weight (applies to 2nd  record)
      INTEGER  tRec0, tRec1, tRec2
      _RL      wght1, wght2

C     !LOCAL VARIABLES:
C     == Local variables ==
C     nbRec       :: number of time-records
C     msgBuf      :: Informational/error message buffer
      INTEGER  nbRec
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL      locTime, modTime, tmpTime
CEOP

C     Implicit function:
      _RL F90MODULO, arg1, arg2
C statement function to emulate Fortran 90 MODULO
C this modulo has the same sign as arg2 (and absolute value < |arg2|)
      F90MODULO(arg1,arg2) = MOD(MOD(arg1,arg2)+arg2,arg2)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      tRec0 = 0
      tRec1 = 0
      tRec2 = 0
      wght1 = 0.
      wght2 = 0.

      IF ( cycleLength.EQ.0. _d 0 ) THEN
C--   Non-periodic time-record sequence:

        locTime = currentTime - recSpacing*0.5
        modTime = F90MODULO(locTime,recSpacing)

C-    time-record before (tRec1) and after (tRec2) current time:
        tRec1 = 1 + NINT( (locTime-modTime)/recSpacing )
        tRec2 = 1 + tRec1

C-    linear interpolation weights:
        wght2 = modTime / recSpacing
        wght1 = 1. _d 0 - wght2

C-    previous time-step record:
        locTime = locTime-deltaT
        modTime = F90MODULO( locTime, recSpacing )
        tRec0 = 1 + NINT( (locTime-modTime)/recSpacing )


      ENDIF

      RETURN
      END
      