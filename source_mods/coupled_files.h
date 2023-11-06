#if defined coupled
MODULE COUPLED_FILES
PUBLIC 
#if defined init_file_in
CHARACTER(10) :: ICFile
#endif

#if defined coupled_bc
CHARACTER(10) :: OBFile  
#endif  
!OBNuFile,  OBSuFile,  OBEuFile,  OBWuFile,  
!OBNvFile,  OBSvFile,  OBEvFile,  OBWvFile,   
!OBNwFile,  OBSwFile,  OBEwFile,  OBWwFile,   
!OBNtFile,  OBStFile,  OBEtFile,  OBWtFile,   
                                     

END MODULE COUPLED_FILES
#endif