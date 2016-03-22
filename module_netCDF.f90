! This is part of the netCDF package.
! Copyright 2006 University Corporation for Atmospheric Research/Unidata.
! See COPYRIGHT file for conditions of use.

! This example writes some surface pressure and temperatures. It is
! intended to illustrate the use of the netCDF fortran 90 API. The
! companion program sfc_pres_temp_rd.f90 shows how to read the netCDF
! data file created by this program.

! This program is part of the netCDF tutorial:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

! Full documentation of the netCDF Fortran 90 API can be found at:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90

! $Id: sfc_pres_temp_wr.f90,v 1.12 2010/04/06 19:32:09 ed Exp $

MODULE routine4netCDF

  USE netcdf
! USE main_data
  IMPLICIT NONE

  type descrDIM
    character (len = 50) :: name
    integer*4  :: nPt     ! Number points in each dimension
    integer*4  :: id      ! id Dimension
  endtype descrDIM

  type descrVAR
    character (len = 50) :: name
    integer(kind = 4)    :: id        ! id variable
    character (len = 20) :: units
    integer(kind = 4)    :: iFillValue = -999999
    real(kind = 4)       :: fFillValue = -999999.0
    character(len = 9)   :: cFillValue = "."
    integer(kind = 4)    :: nDims=0  ! number of dimentions
  endtype descrVAR

  integer(kind = 4) :: nvar_i=60, nvar_f=25, nvar_c9, nvar_c8, nvar_c20
  real(kind = 8), allocatable      ::  array_f(:,:,:)
  integer(kind = 4), allocatable   ::  array_i(:,:,:)
  character(len = 9), allocatable  ::  array_c9(:)
  character(len = 8), allocatable  ::  array_c8(:)
  character(len =20), allocatable  ::  array_c20(:)



  type (descrDIM) dim(5)
  type (descrVAR) var_i(60), var_f(25), var_c(3)
  
CONTAINS

SUBROUTINE write_netCDF4COSMO( FILE_NAME, dbl )

  ! This is the name of the data file we will create.
  character (len = *), INTENT (IN) :: FILE_NAME
  integer(kind = 4) :: dbl !  print debug level
  integer :: ncid
  integer :: i 

! integer, parameter :: NDIMS = 1

! integer :: dimids(NDIMS)

 ! It's good practice for each variable to carry a "units" attribute.
  character (len = *), parameter :: UNITS = "units"
  
  nvar_c8  = var_c(3)%nDims
  nvar_c9  = var_c(1)%nDims
  nvar_c20 = var_c(2)%nDims

  if( dbl >= 4 ) write (*,*) 'c8,c9,c20 = ', nvar_c8,nvar_c9,nvar_c20
  ! Create the file. 
  call check( nf90_create(trim(FILE_NAME), nf90_clobber, ncid) )
  ! Define the dimensions.
  if( dbl >= 3 ) write (*,*) '--------Define the dimensions----start-'
  call check( nf90_def_dim(ncid, trim(dim(1)%name), NF90_UNLIMITED, dim(1)%id) ) ! dim(1)%nPt
  if( dbl >= 4 ) write (*,*) '--------Define the dimensions------after 1D-'
  call check( nf90_def_dim(ncid, trim(dim(2)%name), dim(2)%nPt,     dim(2)%id) )
  if( dbl >= 4 ) write (*,*) '--------Define the dimensions------after 2D-'
  call check( nf90_def_dim(ncid, trim(dim(3)%name),          9,     dim(3)%id) )
  if( dbl >= 4 ) write (*,*) '--------Define the dimension-------after char9-'
  call check( nf90_def_dim(ncid, trim(dim(4)%name),         20,     dim(4)%id) )
  if( dbl >= 4 ) write (*,*) '--------Define the dimension-------after char20-'
  call check( nf90_def_dim(ncid, trim(dim(5)%name),          8,     dim(5)%id) )
  if( dbl >= 4 ) write (*,*) '--------Define the dimension-------after char8-'
  if( dbl >= 3 ) write (*,*) '--------Define the dimensions------end-'
  ! Define the coordinate variables. They will hold the coordinate
  ! information, that is, the latitudes and longitudes. A varid is
  ! returned for each.

  ! Assign units attributes to coordinate var data. This attaches a
  ! text attribute to each of the coordinate variables, containing the
  ! units.

  ! Define the netCDF variables. The dimids array is used to pass the
  ! dimids of the dimensions of the netCDF variables.
! dimids = (/ lon_dimid, lat_dimid /)

  do i=1,nvar_i
    if    ( var_i(i)%nDims==1 ) then
      if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'int'
      call check( nf90_def_var(ncid, trim(var_i(i)%name), NF90_INT, (/ dim(1)%id /), var_i(i)%id ) )
    elseif( var_i(i)%nDims==2 ) then
      if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'int'
      call check( nf90_def_var(ncid, trim(var_i(i)%name), NF90_INT, (/ dim(2)%id, dim(1)%id /), var_i(i)%id ) )
    endif
  enddo
  do i=1,nvar_f
    if    ( var_f(i)%nDims==1 ) then
      if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'float'  !,'double'
      call check( nf90_def_var(ncid, trim(var_f(i)%name), NF90_FLOAT , (/ dim(1)%id /), var_f(i)%id ) )
!     call check( nf90_def_var(ncid, trim(var_f(i)%name), NF90_DOUBLE, dimids, var_f(i)%id ) )
    elseif( var_f(i)%nDims==2 ) then
      if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'float'  !,'double'
      call check( nf90_def_var(ncid, trim(var_f(i)%name), NF90_FLOAT , (/ dim(2)%id, dim(1)%id /), var_f(i)%id ) )
    endif
  enddo
  do i=1,nvar_c9
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'character9'
    call check( nf90_def_var(ncid, trim(var_c(i)%name), NF90_CHAR , (/ dim(3)%id, dim(1)%id /), var_c(i)%id ) )
  enddo
  do i=2,nvar_c20+1
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'character20'
    call check( nf90_def_var(ncid, trim(var_c(i)%name), NF90_CHAR , (/ dim(4)%id, dim(1)%id /), var_c(i)%id ) )
  enddo
  do i=3,nvar_c8+2
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl >= 4 ) write (*,*) 'Define the netCDF variables', i, 'character8'
    call check( nf90_def_var(ncid, trim(var_c(i)%name), NF90_CHAR , (/ dim(5)%id, dim(1)%id /), var_c(i)%id ) )
  enddo

! Assign units attributes to different netCDF variables.
  do i=1,nvar_i
    if  ( var_i(i)%nDims==0 ) cycle
    if( dbl >= 4 ) write (*,*) 'Define attributes of the netCDF variables', i, 'int'
    call check( nf90_put_att(ncid, var_i(i)%id, "_FillValue", var_i(i)%iFillValue ) )
  enddo
  do i=1,nvar_f
    if  ( var_f(i)%nDims==0 ) cycle
    if( dbl >= 4 ) write (*,*) 'Define attributes of the netCDF variables', i, 'float' !,'double'
    call check( nf90_put_att(ncid, var_f(i)%id, "_FillValue" , var_f(i)%fFillValue ) )
  enddo
!   do i=MAX(MAX(1,nvar_c8*2),nvar_c20*3),(nvar_c9+nvar_c8+nvar_c20)
  do i=1,3
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl >= 4 ) write (*,*) 'Define attributes of the netCDF variables', i, 'character'
    call check( nf90_put_att(ncid, var_c(i)%id, "_FillValue" , trim(var_c(i)%cFillValue) ) )
  enddo

  ! End define mode.
  call check( nf90_enddef(ncid) )

  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  ! call check( nf90_put_var(ncid, lon_varid, lons) )

  IF( dbl >= 4 ) WRITE (*,*) 'bound i-array:', ubound(array_i(:,:,:) )
  IF( dbl >= 4 ) WRITE (*,*) 'bound f-array:', ubound(array_f(:,:,:) )
  IF( dbl >= 4 ) WRITE (*,*) 'bound c-array:', ubound(array_c9(:) ), len(array_c9(1))
  IF( dbl >= 4 ) WRITE (*,*) 'bound c-array:', ubound(array_c8(:) ), len(array_c8(1))
  IF( dbl >= 4 ) WRITE (*,*) 'bound c-array:', ubound(array_c20(:) ),len(array_c20(1))

  ! Write the pretend data. This will write our surface pressure and
  ! surface temperature data. The arrays of data are the same size as
  ! the netCDF variables we have defined.
  do i=1,nvar_i
    if    ( var_i(i)%nDims==1 ) then
      if( dbl >= 3 )write (*,*) 'Write the netCDF variables', i, 'int';
      call check( nf90_put_var(ncid, var_i(i)%id, array_i(i,1,:) ) )
    elseif( var_i(i)%nDims==2 ) then
      if( dbl >= 3 ) write (*,*) 'Write the netCDF variables', i, 'int'
      call check( nf90_put_var(ncid, var_i(i)%id, array_i(i, 1:dim(2)%nPt, :) ) )
    endif
  enddo
  do i=1,nvar_f
    if    ( var_f(i)%nDims==1 ) then
      if( dbl >= 3 ) write (*,*) 'Write the netCDF variables', i, 'float'
      call check( nf90_put_var(ncid, var_f(i)%id, real(array_f(i,1,:),4) ) )
    elseif( var_f(i)%nDims==2 ) then
      if( dbl >= 3 ) write (*,*) 'Write the netCDF variables', i, 'float'
      call check( nf90_put_var(ncid, var_f(i)%id, real(array_f(i,:,:),4) ) )
    endif
  enddo
  do i=1,nvar_c9
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl > 2 ) WRITE (*,*) 'Write the netCDF variables', i, 'character9'
    call check( nf90_put_var(ncid, var_c(i)%id, array_c9(1:dim(1)%nPt) ) )
    if( dbl > 5 ) WRITE(*,*) array_c9(1:dim(1)%nPt)
  enddo
  do i=2,nvar_c20+1
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl > 2 ) WRITE (*,*) 'Write the netCDF variables', i, 'character20'
    call check( nf90_put_var(ncid, var_c(i)%id, array_c20(1:dim(1)%nPt) ) )
    if( dbl > 5 ) WRITE(*,*) array_c20(1:dim(1)%nPt)
  enddo
  do i=3,nvar_c8+2
    if  ( var_c(i)%nDims==0 ) cycle
    if( dbl > 2 ) WRITE (*,*) 'Write the netCDF variables', i, 'character8'
    call check( nf90_put_var(ncid, var_c(i)%id, array_c8(1:dim(1)%nPt) ) )
    if( dbl > 5 ) WRITE(*,*) array_c8(1:dim(1)%nPt)
  enddo

  ! Close the file.
  call check( nf90_close(ncid) )
  ! If we got this far, everything worked as expected. Yipee! 
  if( dbl >= 2 ) print *,"*** SUCCESS writing example file ",trim(FILE_NAME),"!"

ENDSUBROUTINE write_netCDF4COSMO

SUBROUTINE init_name_vars_netCDF()

    dim(1)%name = "BUFR_records"
    dim(2)%name = "Loop_000_maxlen"
    dim(3)%name = "YDDDD_strlen"
    dim(4)%name = "YSUPL_strlen"
    dim(5)%name = "YAIRN_strlen"

    var_i(1:8)%nDims=1
!  ----------first section - header for all type obs. - mandatory --------------
    var_i( 1)%name='edition_number'                 ! BUFR edition number (usually = 4)
    var_i( 2)%name='section1_centre'                ! originating / generating data centre e.g. processing centre for GPS reports
    var_i( 3)%name='section1_subcentre'             ! 
    var_i( 4)%name='section1_update_sequence_nr'    ! upd. seq. number (station correction)
    var_i( 5)%name='section1_data_category'         ! WMO data category
    var_i( 6)%name='section1_int_data_sub_category' ! internatl. (WMO) data sub category
    var_i( 7)%name='section1_date'
    var_i( 8)%name='section1_time'

!  ---------third section - individual for each type obs.-----------------------
    var_i( 9)%name='MII'      ! WMO block number
    var_i(10)%name='NIII'     ! WMO station number
    var_i(11)%name='NIX'      ! Type of station  ! stati type (0:auto,1:manned,2:hybrid,3:miss)
    var_i(12)%name='MJJJ'     ! Year
    var_i(13)%name='MMM'      ! Month
    var_i(14)%name='MYY'      ! Day
    var_i(15)%name='MGG'      ! Hour
    var_i(16)%name='NGG'      ! Minute
    var_i(17)%name='NDNDN'    ! Wind direction [degree true]
    var_i(18)%name='MUUU'     ! relative humidity 
    var_i(19)%name='NGGTP'    ! TIME PERIOD OR DISPLACEMENT [min] (temporary) for average wind 10m (usialy 10min)
    var_i(20)%name='NGGTP0'   ! TIME PERIOD OR DISPLACEMENT [min] (temporary) ! 2 times for wind gusts
    var_i(21)%name='MTISI'    ! time significance (BUFR Table 008021)    (dito) (marker average=1)
    var_i(22)%name='MN'       ! Cloud cover (total) [%]
    var_i(23)%name='MVTSU'    ! Vertical significance
    var_i(24)%name='MNH'      ! Cloud amount(of low or middle clouds)
    var_i(25)%name='MCC'      ! Cloud type (low clouds Cl)
    var_i(26)%name='MCC0'     ! Cloud type (medium clouds Cm)
    var_i(27)%name='MCC1'     ! Cloud type (high clouds Ch)
    var_i(28)%name='MDREP'    ! Delayed descriptor replication
    var_i(29)%name='NWW'      ! Present weather
    var_i(30)%name='MGGTP'    ! Time period in hours for PW(SYNOP)/ precip(BUOY)
    var_i(31)%name='MW1'      ! Past weather 1
    var_i(32)%name='MW2'      ! Past weather 2
    var_i(33)%name='MGGTP1'   ! Start of time period in hours ! 2 times
    var_i(34)%name='MGGTP2'   ! Start of time period in hours
    var_i(35)%name='MGGTP3'   ! End of time period in hours
    var_i(36)%name='MGGTP4'   ! Start of time period in hours
    var_i(37)%name='MGGTP5'   ! End of time period in hours
    var_i(38)%name='ME'       ! State of ground (w. or w/o snow)

    var_i(40)%name='NHHHN'    ! Geopotential height [gpm] of the standard level

    var_i(41)%name='NRARA'    ! WMO Common Table C2: radiosonde type/system(91 or 92 var(j))src_obs_proc_cdf.f90:4021
    var_i(42)%name='NSASA'    ! Tracking technique / status of system       src_obs_proc_cdf.f90:4029
    var_i(43)%name='NSR'      ! Solar+infrared radiation correc. - BUFR Table B 002013
    var_i(44)%name='NA4'      ! BUFR Table B 002003 : type of measur. equipment
    var_i(45)%name='MH'       ! Height of release of sonde above MSL/ Height obs via profile[m]
    var_i(46)%name='MSEQM'    ! Station elevation quality mark
    var_i(47)%name='MEDRE'    ! Extended delayed descriptor replicat. fac.
    var_i(48)%name='NLTPD'    ! Time displacement since launch time [s]
    var_i(49)%name='MEVSS'    ! Extended vertical sounding significance

    var_i(50)%name='MABNN'    ! Buoy/platform identifier
    var_i(51)%name='NFLEV'    ! Flight level
    var_i(52)%name='MPHAI'    ! Phase of flight
    var_i(53)%name='MQARA'    ! Wind quality/roll angle

    var_i(54)%name='MHP'      ! Height of station [m]

!! Prepare check of time of last known position for buoys (section 6) 
    var_i(55)%name='MJJJ0'    ! Year
    var_i(56)%name='MMM0'     ! Month
    var_i(57)%name='MYY0'     ! Day
    var_i(58)%name='MGG0'     ! Hour
    var_i(59)%name='NGG0'     ! Minute

    var_f( 1)%name='MLAH'     ! Latitude  (high accuracy) [degree]
    var_f( 2)%name='MLOH'     ! Longitude (high accuracy) [degree]
    var_f( 3)%name='MHOSNN'   ! Height of station above MSL 
    var_f( 4)%name='MHOBNN'   ! Height of barometer a. MSL  ???
    var_f( 5)%name='MPPP'     ! Pressure
    var_f( 6)%name='MPPPP'    ! Pressure reduced to MSL
    var_f( 7)%name='NPPP'     ! 3-hour pressure change
    var_f( 8)%name='MTDBT'    ! Temperature / dry-bulb temperat.
    var_f( 9)%name='MTDNH'    ! Dew-point temperature 
    var_f(10)%name='NFNFN'    ! Wind speed 
    var_f(11)%name='MVV'      ! Horizontal visibility 
    var_f(12)%name='NFXGU'    ! maximum wind speed of gusts               ! 2 times
    var_f(13)%name='MRRR'     ! total precipitation (first period)        ! 2 times
    var_f(14)%name='MHOSEN'   ! HEIGHT OF SENSOR ABOVE LOCAL GROUND [m]
    var_f(15)%name='MRR24'    ! Total precipitation past 24 hours
    var_f(16)%name='NH'       ! Cloud base height above surface
    var_f(17)%name='MTXTXH'   ! Maximum temperature over period specified
    var_f(18)%name='MTNTNH'   ! Minimum temperature over period specified
    var_f(19)%name='MPN'      ! Pressure (standard level)

    var_f(20)%name='MLADH'    ! Latitude displacement since launch site
    var_f(21)%name='MLODH'    ! Longitude displacement since launch site
    var_f(22)%name='NSSS'     ! Total snow depth

    var_f(23)%name='MLALA'    ! Latitude  (coarce accuracy) [degree]
    var_f(24)%name='MLOLO'    ! Longitude (coarce accuracy) [degree]

    var_f(25)%name='MTVIR'    ! Virtual temperature [K]

    var_c( 1)%name='YDDDD'    ! Ship or mobile land sta. identifier
    var_c( 2)%name='YSOSN'    ! Station or site name
    var_c( 3)%name='YAIRN'    ! Aircraft identification


ENDSUBROUTINE init_name_vars_netCDF

#ifdef  SIMPLENETCDF
SUBROUTINE write_netCDF(NLONS,NLATS,lons,lats,var)

  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "sfc_pres_temp.nc"
  integer :: ncid

  ! We are writing 2D data, a 12 x 6 lon-lat grid. We will need two
  ! netCDF dimensions.
  integer, parameter :: NDIMS = 2
! integer, parameter :: NLATS = 10000, NLONS = 10000
  integer,   INTENT (IN)            :: NLATS , NLONS 
  character (len = *), parameter :: LAT_NAME = "latitude"
  character (len = *), parameter :: LON_NAME = "longitude"
  integer :: lat_dimid, lon_dimid

  ! In addition to the latitude and longitude dimensions, we will also
  ! create latitude and longitude netCDF variables which will hold the
  ! actual latitudes and longitudes. Since they hold data about the
  ! coordinate system, the netCDF term for these is: "coordinate
  ! variables."
  real,   INTENT (IN) :: lats(NLATS), lons(NLONS)
  integer :: lat_varid, lon_varid
  real, parameter :: START_LAT = 25.0, START_LON = -125.0

  ! We will write surface temperature and pressure fields. 
  character (len = *), parameter :: PRES_NAME="pressure"
  character (len = *), parameter :: TEMP_NAME="temperature"
  integer :: pres_varid, temp_varid
  integer :: dimids(NDIMS)

  ! It's good practice for each variable to carry a "units" attribute.
  character (len = *), parameter :: UNITS = "units"
  character (len = *), parameter :: PRES_UNITS = "hPa"
  character (len = *), parameter :: TEMP_UNITS = "celsius"
  character (len = *), parameter :: LAT_UNITS = "degrees_north"
  character (len = *), parameter :: LON_UNITS = "degrees_east"

  ! We will create some pressure and temperature data to write out.
  real, dimension(:,:), allocatable :: temp_out
  real, dimension(:,:), allocatable :: pres_out
  real,   INTENT (IN)  :: var(NLATS)
  real, parameter :: SAMPLE_PRESSURE = 900.0
  real, parameter :: SAMPLE_TEMP = 9.0

  ! Loop indices
  integer :: lat, lon

  ! Allocate memory.
! allocate(pres_out(NLONS, NLATS))
! allocate(temp_out(NLONS, NLATS))
!write (*,*)  lats(1:20)
  ! Create pretend data. If this were not an example program, we would
  ! have some real data to write, for example, model output.
!  do lat = 1, NLATS
!     lats(lat) = START_LAT + (lat - 1) * 5.0
!  end do
!  do lon = 1, NLONS
!     lons(lon) = START_LON + (lon - 1) * 5.0
! end do
!  do lon = 1, NLONS
!     do lat = 1, NLATS
!        pres_out(lon, lat) = SAMPLE_PRESSURE + (lon - 1) * NLATS + (lat - 1)
!        temp_out(lon, lat) = SAMPLE_TEMP + .25 * ((lon - 1) * NLATS + (lat - 1))
!     end do
!  end do
!write (*,*)  lats(1:20)
  ! Create the file. 
  call check( nf90_create(FILE_NAME, nf90_clobber, ncid) )

  ! Define the dimensions.
  call check( nf90_def_dim(ncid, LAT_NAME, NLATS, lat_dimid) )
  call check( nf90_def_dim(ncid, LON_NAME, NLONS, lon_dimid) )

  ! Define the coordinate variables. They will hold the coordinate
  ! information, that is, the latitudes and longitudes. A varid is
  ! returned for each.
  call check( nf90_def_var(ncid, LAT_NAME, NF90_REAL, lat_dimid, lat_varid) )
  call check( nf90_def_var(ncid, LON_NAME, NF90_REAL, lon_dimid, lon_varid) )

  ! Assign units attributes to coordinate var data. This attaches a
  ! text attribute to each of the coordinate variables, containing the
  ! units.
  call check( nf90_put_att(ncid, lat_varid, UNITS, LAT_UNITS) )
  call check( nf90_put_att(ncid, lon_varid, UNITS, LON_UNITS) )

  ! Define the netCDF variables. The dimids array is used to pass the
  ! dimids of the dimensions of the netCDF variables.
  dimids = (/ lon_dimid, lat_dimid /)
! call check( nf90_def_var(ncid, PRES_NAME, NF90_REAL, dimids, pres_varid) )
! call check( nf90_def_var(ncid, TEMP_NAME, NF90_REAL, dimids, temp_varid) )
  call check( nf90_def_var(ncid, PRES_NAME, NF90_REAL, dimids, pres_varid) )


  ! Assign units attributes to the pressure and temperature netCDF
  ! variables.
!  call check( nf90_put_att(ncid, pres_varid, UNITS, PRES_UNITS) )
!  call check( nf90_put_att(ncid, temp_varid, UNITS, TEMP_UNITS) )

  ! End define mode.
  call check( nf90_enddef(ncid) )

  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.

  call check( nf90_put_var(ncid, lat_varid, lats) )
  call check( nf90_put_var(ncid, lon_varid, lons) )

  write (*,*)  NLONS, NLATS, ubound(var)

  ! Write the pretend data. This will write our surface pressure and
  ! surface temperature data. The arrays of data are the same size as
  ! the netCDF variables we have defined.
  call check( nf90_put_var(ncid, pres_varid, var) )
! call check( nf90_put_var(ncid, temp_varid, temp_out) )
   write (*,*)  'cool3'
  ! Close the file.
  call check( nf90_close(ncid) )
   write (*,*)  'cool4'
  ! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS writing example file sfc_pres_temp.nc!"

ENDSUBROUTINE write_netCDF
#endif

SUBROUTINE check(status)
  USE netcdf
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop 2
    end if
ENDSUBROUTINE check  

ENDMODULE routine4netCDF

