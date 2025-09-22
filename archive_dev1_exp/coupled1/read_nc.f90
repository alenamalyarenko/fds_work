program read_nc

use netcdf



INTEGER  :: I,J,K,II,JJ,KK,IW,IOR,LU_UVW,IERROR,IMIN,IMAX,JMIN,JMAX,KMIN,KMAX
!INTEGER, INTENT(IN) :: NM
REAL, DIMENSION(60,60,20):: TMP0



REAL :: MI,MJ,MK,GI1,GI2,GJ1,GJ2,GK1,GK2
  integer, parameter :: NDIMS = 3
  integer :: status, ncid, varid1
  integer,dimension(3)::start,end
  integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
  
character(10):: ICFile
ICFile='ic_palm.nc'


GI1=0.0
GJ1=0.0
GK1=0.0

GI2=20.0
GJ2=20.0
GK2=20.0

start(1)=GI1
start(2)=GJ1
start(3)=GK1

end(1)=GI2
end(2)=GJ2
end(3)=GK2

status=nf90_open(ICFile, nf90_nowrite, ncid)

status=nf90_inquire(ncid, ndims_in, nvars_in, ngatts_in, unlimdimid_in)
 Print*, "Number of dimensions:", ndims_in
 Print*, "Number of variables:", nvars_in
 Print*, "Number of global attributes:", ngatts_in


 
! 
 status=nf90_inq_varid(ncid, 'theta', varid1)
!status=nf90_get_att()

Print*,NM, GI1,IBP1
print*, start, end

 !status=nf90_get_var(ncid, varid1, TMP0) 



status=nf90_get_var(ncid, varid1, TMP0,start,end) 


!status=nf90_get_var(ncid, varid1, TMP0,start = (/ GI1, GJ1, GK1 /),  count = (/ IBP1, JBP1, KBP1 /) ) 
! if(status /= nf90_NoErr) call handle_err(status)

 status=nf90_close(ncid)


end program 