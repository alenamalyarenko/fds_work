#if defined coupled
MODULE COUPLED_FILES
PUBLIC 
#if defined init_file_in 
CHARACTER(13) :: ICFile
#endif

#if defined coupled_bc_file
!bc_palm_7a.nc - 13 characters
CHARACTER(13) :: OBFile 
integer:: ncid	 
real:: recSpacing	
#endif  



END MODULE COUPLED_FILES
#endif
