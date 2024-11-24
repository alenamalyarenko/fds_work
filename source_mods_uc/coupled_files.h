#if defined coupled
MODULE COUPLED_FILES
PUBLIC 
#if defined init_file_in 
CHARACTER(13) :: ICFile
#endif

#if defined coupled_bc_file
CHARACTER(13) :: OBFile  
#endif  
!OBNuFile,  OBSuFile,  OBEuFile,  OBWuFile,  
!OBNvFile,  OBSvFile,  OBEvFile,  OBWvFile,   
!OBNwFile,  OBSwFile,  OBEwFile,  OBWwFile,   
!OBNtFile,  OBStFile,  OBEtFile,  OBWtFile,   
                                     
!this is how they are written:
!REAL, DIMENSION(IBAR,KBAR,NT):: TN,TS
!REAL, DIMENSION(IBP1,KBP1,NT):: UN, US,VN,VS,WN,WS
!REAL, DIMENSION(JBAR,KBAR,NT):: TE,TW
!REAL, DIMENSION(JBP1,KBP1,NT):: UE, UW,VE,VW,WE,WW
	
REAL, DIMENSION(60,20,10):: TN,TS
REAL, DIMENSION(61,21,10):: UN, USS,VN,VSS,WN,WSS
REAL, DIMENSION(60,20,10):: TE,TW
REAL, DIMENSION(61,21,10):: UE, UW,VE,VW,WE,WWW


END MODULE COUPLED_FILES
#endif
