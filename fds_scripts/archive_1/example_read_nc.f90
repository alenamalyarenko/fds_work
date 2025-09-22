! This is the name of the data file we will read.
  character (len = *), parameter :: FILE_NAME = "sfc_pres_temp.nc"
  integer :: ncid

  ! We are reading 2D data, a 6 x 12 lat-lon grid.
  integer, parameter :: NDIMS = 2
  integer, parameter :: NLATS = 6, NLONS = 12
  character (len = *), parameter :: LAT_NAME = "latitude"
  character (len = *), parameter :: LON_NAME = "longitude"
  integer :: lat_dimid, lon_dimid

  ! For the lat lon coordinate netCDF variables.
  real :: lats(NLATS), lons(NLONS)
  integer :: lat_varid, lon_varid

  ! We will read surface temperature and pressure fields. 
  character (len = *), parameter :: PRES_NAME = "pressure"
  character (len = *), parameter :: TEMP_NAME = "temperature"
  integer :: pres_varid, temp_varid
  integer :: dimids(NDIMS)


  ! Read the data into these arrays.
  real :: pres_in(NLONS, NLATS), temp_in(NLONS, NLATS)



  ! We will learn about the data file and store results in these
  ! program variables.
  integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in

  ! Loop indices
  integer :: lat, lon

 
   nf90_open(FILE_NAME, nf90_nowrite, ncid)


  ! Get the varids of the latitude and longitude coordinate variables.
  nf90_inq_varid(ncid, LAT_NAME, lat_varid) )
  nf90_inq_varid(ncid, LON_NAME, lon_varid) )
  nf90_inq_varid(ncid, PRES_NAME, pres_varid)
  nf90_inq_varid(ncid, TEMP_NAME, temp_varid)

  nf90_get_var(ncid, lat_varid, lats)
  nf90_get_var(ncid, lon_varid, lons)
  nf90_get_var(ncid, pres_varid, pres_in) 
  nf90_get_var(ncid, temp_varid, temp_in)



   nf90_close(ncid) )

