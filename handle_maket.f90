SUBROUTINE handle()
! TODO:
! - check coordinate of TEMP station in SLOVPURx - bring double string
! - check handle 96-142 for TEMPMAKT
  
  INTEGER :: mZ, mT, mW  ! number level on surface{0-1}, tropopause {0-6}, windmax {0-6} for TEMPMAKT
  INTEGER :: ilat, ilon
  REAL*4  :: unitTemp, unitPressure, unitCloud
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: filterSt
  ! LOGICAL, DIMENSION(:) :: filterSt
  ! LOGICAL, DIMENSION(:) :: filterSt

!------------SET units for variable P and T------------------------
  SELECT CASE( TRIM(typefile) )
  CASE('maket','csv','station')   !  [C] and [gPa]
    unitTemp     = 0.0
    unitPressure = 0.1
  CASE DEFAULT                     !  [K] and [Pa]
    unitTemp     = 273.15
    unitPressure = 10.0
  ENDSELECT
  unitCloud = 10    ! [%]
!------------------------------------------------------------------

  set_id: SELECT CASE ( RECNAME(el) )
  CASE( 'SYNOPMAK','SYNOPDOP','TEMPMAKT','TEBDTMAK','TEBDWMAK','TEMPALLC' )
    ListSt(1:NY) = INT(value_field(1,1:NY,t))*1000+INT(value_field(2,:,t))

    DO j=1,NY
      WRITE(idST(j),'(i5.5,a3)')    ListSt(j),'   '
      df(el,t)%idST(j)(1:8)=idST(j)(1:8)
    ENDDO
    !WRITE(*,*)  '==',idST(200)(1:8),'=='
    !WRITE(string1,'(10000i8.8)')  idST

  ENDSELECT set_id

  handle_maket: SELECT CASE( RECNAME(el) )
  CASE( 'SYNOPMAK','SYNOPDOP')
    ilat = 3;    ilon = 4
    FORALL( j=1:NY )
     !value_field2(1:2,j,t)=value_field(1:2,j,t)                          ! index
      value_field2(3:4,j,t) = value_field(3:4,j,t)*0.01                     ! lat, lon [degree]
     !value_field2(5:7,j,el,t)=value_field(5:7,j,t)                       ! altitude [m], sigh, geopot
      value_field2(8:9,j,t)=value_field(8:9,j,t)*unitPressure             ! ps, pmsl
      value_field2(10:11,j,t)=(value_field(10:11,j,t)-1000)*0.1+unitTemp  ! t_2m, td_2m
     !value_field2(12:14,j,t)=value_field(12:14,j,t)                      ! direct, speed, kind baric tendency
      value_field2(15,j,t)=(value_field(15,j,t)-1000)*unitPressure        ! pressure tendency [Pa/3hour]
      value_field2(16:17,j,t)=value_field(16:17,j,t)*0.1                  ! horisontal visiable [m], sunlight [hour]
      value_field2(18:19,j,t)=value_field(18:19,j,t)*unitCloud            ! total cloud in [ball->%], Hh[ball->%]
     !value_field2(20:23,j,t)=value_field(20:23,j,t)                      ! Cl[code], Cm[code], Ch[code], HBASE_CL [m]
      value_field2(24:25,j,t)=value_field(24:25,j,t)                      ! ww (current weather)[code 4677], ww1(past weather)[code 4561]
      value_field2(26:28,j,t)=(value_field(26:28,j,t)-1000)*0.1+unitTemp  ! tmin, tmax, tgmin
      value_field2(29:31,j,t)=value_field(29:31,j,t)*0.1                  ! R6,R12,R24 - precipitation for 6,12,24 hours [mm] 
      value_field2(32,j,t)=(value_field(32,j,t)-1000)*0.1 +unitTemp       ! TG (temp soil)
     !value_field2(33,j,t) = value_field(33,j,t)                          ! Hsnow [cm]
    END FORALL

    IF( Date(t) > 20021231 )THEN
      FORALL( j=1:NY )
       !value_field2(34:40,j,t) = value_field(34:40,j,t)                    ! Rshift, E4, E1, E3, Sp1, Sp2, Sp3
        value_field2(41:43,j,t) = value_field(41:43,j,t)*0.1                ! PBagrCorrect, PHCorrect, PExetter [gpa]
        value_field2(44:46,j,t) = value_field(44:46,j,t)*0.1                ! THCorrect, TPExetter, Tmsl [C]
      END FORALL
    ! ELSE
       !value_field2(34,j,t)    = value_field(34,j,t)                       ! marker storm and hail
       !value_field2(35:40,j,t) = value_field(35:40,j,t)                    ! reserved
    END IF

    ! Negative values is marker bad values from ASOI
    WHERE ( value_field(8:13,:,t) < 0 )  value_field2(8:13,:,t)=oundef
    ! В базе встречаются значения осадков, равные +9990. 
    ! приходится их приравнивать к константе отсутствия.
    WHERE( value_field(29:31,:,t) == 9990.0 ) value_field2(29:31,:,t)=oundef
    ! В базе встречаются значения облачности, равное 99 
    ! приходится её приравнивать к константе отсутствия.
    WHERE( value_field(18,:,t) == 99.0 ) value_field2(18,:,t)=oundef
    ! Задать знак тенденции согласно КН-01
    WHERE( value_field(14,:,t) > 4.0 ) value_field2(15,:,t)=-value_field2(15,:,t)

    ! Цикл для подстановки координат из словаря
    DO j=1,NY
      DO jj=1,17000
        IF( ListSt(j) == dictionarySt(1,jj) )THEN
          value_field2(3,j,t) = dictionarySt(2,jj)*0.001_8           ! lat from dictionary
          value_field2(4,j,t) = dictionarySt(3,jj)*0.001_8           ! lon from dictionary
          EXIT  ! other station no used
        ENDIF
      ENDDO
    ENDDO
! Закомментировал, так как это исправили.
!!     Вручную установить высоту станции Шереметьево
       WHERE( ListSt(:)==27514 ) value_field2(5,:,t)=187
!!     Вручную устанвлены высоты 4 станций в Шведции, так как там незадана высота
       WHERE( ListSt(:)==2287 ) value_field2(5,:,t)=   7.
       WHERE( ListSt(:)==2561 ) value_field2(5,:,t)=  73.
       WHERE( ListSt(:)==2629 ) value_field2(5,:,t)= 104.
       WHERE( ListSt(:)==2667 ) value_field2(5,:,t)=   4.

      df(el,:)%header(1)  = 'index'
      df(el,:)%header(2)  = 'index'
      df(el,:)%header(3)  = 'lat'
      df(el,:)%header(4)  = 'lon'
      df(el,:)%header(5)  = 'height'
      df(el,:)%header(6)  = 'mPS'  ! 'PS_corr'
      df(el,:)%header(7)  = 'gps'
      df(el,:)%header(8)  = 'ps'
      df(el,:)%header(9)  = 'pmsl'
      df(el,:)%header(10) = 't2m'
      df(el,:)%header(11) = 'td2m'
      df(el,:)%header(12) = 'dd10m'
      df(el,:)%header(13) = 'ff10m'
      df(el,:)%header(14) = 'typTPS'
      df(el,:)%header(15) = 'tps'
      df(el,:)%header(16) = 'hVV'
      df(el,:)%header(17) = 'tSunl'
      df(el,:)%header(18) = 'clct'
      df(el,:)%header(19) = 'clcl_clcm'
      df(el,:)%header(20) = 'typLC'
      df(el,:)%header(21) = 'typMC'
      df(el,:)%header(22) = 'typHC'
      df(el,:)%header(23) = 'hBasLC'
      df(el,:)%header(24) = 'WW'
      df(el,:)%header(25) = 'pWW'
      df(el,:)%header(26) = 'tMin2m'
      df(el,:)%header(27) = 'tMax2m'
      df(el,:)%header(29) = 'R6'
      df(el,:)%header(30) = 'R12'
      df(el,:)%header(31) = 'R24'
      df(el,:)%header(32) = 't_g'
      df(el,:)%header(33) = 'hSnow'

      IF( Date(t) > 20021231 )THEN
        df(el,:)%header(28) = 'tMinG'
        df(el,:)%header(34) = 'shiftR'
        df(el,:)%header(35) = 'E4'
        df(el,:)%header(36) = 'E1'
        df(el,:)%header(37) = 'E3'
        df(el,:)%header(38) = 'sp1'
        df(el,:)%header(39) = 'sp2'
        df(el,:)%header(40) = 'sp3'
        df(el,:)%header(41) = 'pmsl-B'
        df(el,:)%header(42) = 'pmsl-hc'
        df(el,:)%header(43) = 'pmsl-MO'
        df(el,:)%header(44) = 't2m-hc'
        df(el,:)%header(45) = 't2m-MO'
        df(el,:)%header(46) = 'tmsl'
     ELSE
        df(el,:)%header(28) = 'tAveDay'
        df(el,:)%header(34) = 'stm&hail'
        df(el,:)%header(35) = '-'
        df(el,:)%header(36) = '-'
        df(el,:)%header(37) = '-'
        df(el,:)%header(38) = '-'
        df(el,:)%header(39) = '-'
        df(el,:)%header(40) = '-'
     ENDIF

    CASE( 'TEMPMAKT' ) handle_maket
      ilat = 5;    ilon = 6
      FORALL( j=1:NY )
       !value_field2(1:4,j,t)=value_field(1:4,j,t)                    ! indexRegion, indexSt
       !value_field2(3,j,t)=value_field(3,j,t)                        ! marker: 2(TEMP, TEMP-SHIP) or 8(PILOT)
       !value_field2(4,j,t)=value_field(4,j,t)                        ! altitude
        value_field2(5:6,j,t)=value_field(5:6,j,t)*0.01               ! lat, lon
       !value_field2(7:10,j,el,t)=value_field(7:10,j,t)               ! hour, day, reserv1, reserv2
        value_field2(11,j,t)=value_field(11,j,t)-1000                 ! H1000
       !value_field2(12,j,t)=(value_field(12,j,t)-1000.)*0.1+unitTemp ! T on 1000gPa
       !value_field2(13,j,t)=value_field(13,j,t)*0.1                  ! Td (rcld-saturation deficit) on level 1000gPa
       !value_field2(14,j,t)=value_field(14,j,t)                      ! direct of wind on level [degree]
       !value_field2(15,j,t)=value_field(15,j,t)                      ! speed of wind on level [m/s]
       !value_field2(101:124,j,t)=value_field(101:124,j,t)            ! data on level tropopause P t ff ddd
       !value_field2(125:142,j,t)=value_field(125:142,j,t)            ! data on level maxWind P(H) t ff ddd. P>0 .or. H<0
       !value_field2(143:146,j,t)=value_field(143:146,j,t)            ! reserv
       !value_field2(147:150,j,t)=value_field(147:150,j,t)            ! 'TEMP' or name ship for 'TEMP-SHIP'
      END FORALL
      FORALL( j=1:NY, i=11:86:5 )
        !value_field2(i,j,t)=value_field(i,j,t)  ! 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10 [gpm]
        value_field2(i+1,j,t)=(value_field(i+1,j,t)-1000.)*0.1+unitTemp ! T on level
        value_field2(i+2,j,t)=value_field(i+2,j,t)*0.1                  ! Td (rcld-saturation deficit) on level
        !value_field2(i+3,j,t)=value_field(i+3,j,t)                     ! direct of wind on level [degree]
        !value_field2(i+4,j,t)=value_field(i+4,j,t)                     ! speed  of wind on level [m/s]
      END FORALL

      value_field2(96:142,:,t) = oundef ! need change

      DO j=1,NY
        mz = value_field(93,j,t) ! 0 or 1
        mt = value_field(94,j,t) ! 0-6
        mw = value_field(95,j,t) ! 0-6

        DO k=1,mz
          value_field2(96,j,t)=value_field(96,j,t)*unitPressure        ! PMSL
          value_field2(97,j,t)=(value_field(97,j,t)-1000)*0.1+unitTemp ! T_2M
          value_field2(98,j,t)=value_field(98,j,t)*0.1                 ! TdSurf (rcld-saturation deficit)
          value_field2(99:100,j,t)=value_field(99:100,j,t)             ! direct, speed of wind on Surface
        ENDDO
        DO k=1,mt
          value_field2( 97+k*4,j,t) = value_field(92+mz*5+k*4,j,t)*unitPressure       ! P
          value_field2( 98+k*4,j,t) =(value_field(93+mz*5+k*4,j,t)-1000)*0.1+unitTemp ! T
          value_field2( 99+k*4,j,t) = value_field(94+mz*5+k*4,j,t)                    ! direct of wind
          value_field2(100+k*4,j,t) = value_field(95+mz*5+k*4,j,t)                    ! speed of wind
        ENDDO
        DO k=1,mw
          value_field2(122+k*3,j,t) = value_field(93+mz*5+mt*4+k*3,j,t)*unitPressure  ! P
          value_field2(123+k*3,j,t) = value_field(94+mz*5+mt*4+k*3,j,t)               ! direct of wind
          value_field2(124+k*3,j,t) = value_field(95+mz*5+mt*4+k*3,j,t)               ! speed of wind
        ENDDO
      ENDDO

      WHERE( value_field( 11: 90,:,t) < 0 )  value_field2( 11: 90,:,t)=oundef
      WHERE( value_field( 96:142,:,t) < 0 )  value_field2( 96:142,:,t)=oundef

      df(el,:)%clevel(11)='1000'
      df(el,:)%clevel(16)='925'
      df(el,:)%clevel(21)='850'
      df(el,:)%clevel(26)='700'
      df(el,:)%clevel(31)='500'
      df(el,:)%clevel(36)='400'
      df(el,:)%clevel(41)='300'
      df(el,:)%clevel(46)='250'
      df(el,:)%clevel(51)='200'
      df(el,:)%clevel(56)='150'
      df(el,:)%clevel(61)='100'
      df(el,:)%clevel(66)='70'
      df(el,:)%clevel(71)='50'
      df(el,:)%clevel(76)='30'
      df(el,:)%clevel(81)='20'
      df(el,:)%clevel(86)='10'
      df(el,:)%clevel(96)='surf'
      df(el,:)%clevel(101)='ltrop'
      df(el,:)%clevel(105)='ltrop'
      df(el,:)%clevel(109)='ltrop'
      df(el,:)%clevel(113)='ltrop'
      df(el,:)%clevel(117)='ltrop'
      df(el,:)%clevel(121)='ltrop'
      df(el,:)%clevel(125)='vmax'
      df(el,:)%clevel(128)='vmax'
      df(el,:)%clevel(131)='vmax'
      df(el,:)%clevel(134)='vmax'
      df(el,:)%clevel(137)='vmax'
      df(el,:)%clevel(140)='vmax'

      df(el,:)%header(1)='index'
      df(el,:)%header(2)='index'
      df(el,:)%header(3)='ToP'
      df(el,:)%header(4)='height'
      df(el,:)%header(5)='lat'
      df(el,:)%header(6)='lon'
      df(el,:)%header(7)='hh:mm'
      df(el,:)%header(8)='MM:dd'
      df(el,:)%header(11)='H1000'
      df(el,:)%header(12)='T1000'
      df(el,:)%header(13)='dD1000'
      df(el,:)%header(14)='WD1000'
      df(el,:)%header(15)='WS1000'

      FORALL( k = 16:86:5 )
        df(el,:)%header(k)   = 'H'//df(el,1)%clevel(k)
        df(el,:)%header(k+1) = 'T'//df(el,1)%clevel(k)
        df(el,:)%header(k+2) = 'dD'//df(el,1)%clevel(k)
        df(el,:)%header(k+3) = 'WD'//df(el,1)%clevel(k)  !DD
        df(el,:)%header(k+4) = 'WS'//df(el,1)%clevel(k)  !FF
      ENDFORALL

      df(el,:)%header( 96) = 'pSurf'
      df(el,:)%header( 97) = 'tSurf'
      df(el,:)%header( 98) = 'dDSurf'
      df(el,:)%header( 99) = 'wDSurf'
      df(el,:)%header(100) = 'wSSurf'

      df(el,t)%header(101:124:4) = 'PTrop'
      df(el,t)%header(102:124:4) = 'TTrop'
      df(el,t)%header(103:124:4) = 'WDTrop'
      df(el,t)%header(104:124:4) = 'WSTrop'

      df(el,t)%header(125:142:3) = 'PTrop'
      df(el,t)%header(126:142:3) = 'WDTrop'
      df(el,t)%header(127:142:3) = 'WSTrop'

!     string Type(Sr) RaRaSaSa OSurf OTropop OmaxW" 

    CASE( 'TEBDTMAK' ) handle_maket
      ilat = 5;    ilon = 6
      FORALL( j=1:NY )
        !value_field2(1:2,j,t)=value_field(1:2,j,t)                  ! indexRegion, indexSt
        !value_field2(3,j,t)=value_field(3,j,t)                      ! marker observation (6)
        !value_field2(4,j,t)=value_field(4,j,t)                      ! altitude
        value_field2(5:6,j,t)=value_field(5:6,j,t)*0.01              ! lat, lon
        !value_field2(7:10,j,el,t)=value_field(7:10,j,t)             ! N point in sec B, NT point in B+D, jk=10+NT*3, reserv
        !value_field2(307:310,j,t)=value_field(307:310,j,t)          ! 'TEMP' or name ship for 'TEMP-SHIP'
      END FORALL
      FORALL( j=1:NY, i=11:304:3 )
        value_field2(i,j,t)=value_field(i,j,t)*unitPressure          ! pressure [Pa]
        value_field2(i+1,j,t)=(value_field(i+1,j,t)-1000.)*0.1+unitTemp ! temperature
        value_field2(i+2,j,t)= value_field(i+2,j,t)*0.1                 ! dew point
      END FORALL

    CASE( 'TEBDWMAK' ) handle_maket
      ilat = 5;    ilon = 6
      FORALL( j=1:NY )
        !value_field2(1:2,j,t)=value_field(1:2,j,t)                  ! indexRegion, indexSt
        !value_field2(3,j,t)=value_field(3,j,t)                      ! marker observation (7)
        !value_field2(4,j,t)=value_field(4,j,t)                      ! altitude
        value_field2(5:6,j,t)=value_field(5:6,j,t)*0.01              ! lat, lon
        !value_field2(7:10,j,el,t)=value_field(7:10,j,t)             ! N point in sec B, NT point in B+D, jk=10+NT*3, reserv
        !value_field2(307:310,j,t)=value_field(307:310,j,t)          ! 'TEMP' or name ship for 'TEMP-SHIP'
      END FORALL
      FORALL( j=1:NY, i=11:304:3 )
        value_field2(i,j,t) = value_field(i,j,t)*unitPressure        ! pressure
        !value_field2(i+1,j,t) = value_field(i+1,j,t)                ! wind direction
        !value_field2(i+2,j,t) = value_field(i+2,j,t)                ! wind speed
      ENDFORALL

    CASE( 'TEMPALLC' ) handle_maket
      ilat = 6;    ilon = 7
      FORALL( j=1:NY )
        value_field2(3,j,t)   = value_field(1,j,t)                     ! marker start
        value_field2(1:2,j,t) = value_field(2:3,j,t)                   ! indexRegion, indexSt
        value_field2(2,j,t)   = value_field(3,j,t)                     ! indexSt
       !value_field2(4,j,t)   = value_field(4,j,t)                     ! marker observation (7)
       !value_field2(5,j,t)   = value_field(5,j,t)                     ! altitude
        value_field2(6:7,j,t) = value_field(6:7,j,t)*0.01              ! lat, lon
       !value_field2(11,j,t)  = value_field(11,j,t)                    ! nlevels
      ENDFORALL

      FORALL( j=1:NY, i=15:2600:13 )
        value_field2(i  , j,t) = value_field(i,j,t)*unitPressure    ! P
        value_field2(i+2, j,t) = value_field(i+2,j,t)               ! H
        value_field2(i+4, j,t) = value_field(i+4,j,t)*0.1+unitTemp  ! T
        value_field2(i+6, j,t) = value_field(i+6,j,t)*0.1           ! Td (rcld-saturation deficit) on level 1000gPa
       !value_field2(i+8, j,t) = value_field(i+8,j,t)               ! direct of wind on level [degree]
       !value_field2(i+10,j,t) = value_field(i+10,j,t)              ! speed  of wind on level [m/s]
      ENDFORALL
      
  CASE( 'SHIPBMAK' )
    ilat = 3;    ilon = 4
    FORALL( j=1:NY )
     !value_field2(1:2,j,t)   = value_field(1:2,j,t)                        ! 99, number report
      value_field2(3:4,j,t)   = value_field(3:4,j,t)*0.01                   ! lat, lon [degree]
     !value_field2(5:8,j,t)   = value_field(5:8,j,t)                        ! name of ship or number buoy
      value_field2(9,j,t)     = value_field(9,j,t)*unitPressure             ! pmsl
      value_field2(10:11,j,t) = (value_field(10:11,j,t)-1000)*0.1+unitTemp  ! t_2m, td_2m
     !value_field2(12:14,j,t) = value_field(12:14,j,t)                      ! direct, speed, kind baric tendency
      value_field2(15,j,t)    = (value_field(15,j,t)-1000)*unitPressure     ! pressure tendency [Pa/3hour]
      value_field2(16,j,t)    = value_field(16,j,t)*10                      ! horisontal visiable [m]
      value_field2(17,j,t)    = value_field(17,j,t)*0.1                     ! sunlight [hour]
      value_field2(18:19,j,t) = value_field(18:19,j,t)*unitCloud            ! total cloud in [ball->%], Hh[ball->%]
     !value_field2(20:23,j,t) = value_field(20:23,j,t)                      ! Cl[code], Cm[code], Ch[code], HBASE_CL [m]
     !value_field2(24:25,j,t) = value_field(24:25,j,t)                      ! ww (current weather)[code 4677], ww1(past weather)[code 4561]
      value_field2(26,j,t)    = (value_field(26,j,t)-1000)*0.1+unitTemp     ! t_surface_water
     !value_field2(27:41,j,t) = value_field(27:41,j,t)                      ! reserv
      value_field2(42:43,j,t) = value_field(42:43,j,t)*unitPressure         ! PHCorrect, PExetter
      value_field2(44:45,j,t) = value_field(44:45,j,t)*0.1+unitTemp         ! THCorrect, TPExetter
     !value_field2(46,j,t)    = value_field(46,j,t)                         ! marker: 21 - SHIP, 152 - BUOY
    END FORALL

    ! отрицательные значения - это забракованные данные задачей OASYN
    WHERE( value_field(8:13,:,t) < 0 )  value_field2(8:13,:,t) = oundef
    ! В базе встречаются значения облачности, равное 99
    ! приходится её приравнивать к константе отсутствия.
    WHERE( value_field(18,:,t) == 99.0 ) value_field2(18,:,t) = oundef
    ! Задать знак тенденции согласно КН-01
    WHERE( value_field(14,:,t) > 4.0 ) value_field2(15,:,t) = -value_field2(15,:,t)

    df(el,:)%header(1)  = 'index'
    df(el,:)%header(2)  = 'NShip'
    df(el,:)%header(3)  = 'lat'
    df(el,:)%header(4)  = 'lon'
    df(el,:)%header(5)  = 'name'
    df(el,:)%header(9)  = 'PMSL'
    df(el,:)%header(10) = 't_2m'
    df(el,:)%header(11) = 'td_2m'
    df(el,:)%header(12) = 'wDir'
    df(el,:)%header(13) = 'wSp'
    df(el,:)%header(14) = 'typeBT'
    df(el,:)%header(15) = 'barTen'
    df(el,:)%header(16) = 'hVV'
    df(el,:)%header(17) = 'tsunl'
    df(el,:)%header(18) = 'clct'
    df(el,:)%header(19) = 'clcl/m'
    df(el,:)%header(20) = 'clcl'
    df(el,:)%header(21) = 'clcm'
    df(el,:)%header(22) = 'clch'
    df(el,:)%header(23) = 'hbas_cl'
    df(el,:)%header(24) = 'WW'
    df(el,:)%header(25) = 'pWW'
    df(el,:)%header(26) = 't_s'
    df(el,:)%header(42) = 'pmsl_hc'
    df(el,:)%header(43) = 'pmsl_uk'
    df(el,:)%header(44) = 't2m_hc'
    df(el,:)%header(45) = 't2m_uk'
    df(el,:)%header(46) = 'S|B'

  CASE( 'AIREPMAK')
    ilat = 5;    ilon = 6
    FORALL( j=1:NY ) ! 1:20000
     !value_field2(1:3,j,t)   = value_field(1:3,j,t)                       ! 500, number report, 5
     !value_field2(4,j,t)     = value_field(4,j,t)                         ! marker: 43 - AMDAR, 45 - AIREP
      value_field2(5:6,j,t)   = value_field(5:6,j,t)*0.01                  ! lat, lon
     !value_field2(7:8,j,el,t)= value_field(7:8,j,t)                       ! time in hhmm, day in MMdd
     !value_field2(9,j,el,t)  = value_field(9,j,t)                         ! height of flight [m]
      value_field2(10,j,t)    = value_field(10,j,t)*unitPressure           ! pressure 
     !value_field2(11:12,j,t) = value_field(11:12,j,t)                     ! direct wind [degree], speed wind [m/s]
      value_field2(13,j,t)    = (value_field(13,j,t)-1000)*0.1+unitTemp    ! temperature
     !value_field2(14:16,j,t) = value_field(14:16,j,t)                     ! reserv
     !value_field2(17:20,j,t) = value_field(17:20,j,t)                     ! name of aircraft

    END FORALL
    ! отрицательные значения - это забракованные данные задачей CONTREL
    WHERE (  value_field(11:12,:,t) < 0  )  value_field2(11:12,:,t) = oundef

    df(el,:)%header(1)='index'
    df(el,:)%header(2)='number'
    df(el,:)%header(3)='index'
    df(el,:)%header(4)='marker'
    df(el,:)%header(5)='lat'
    df(el,:)%header(6)='lon'
    df(el,:)%header(7)='time'
    df(el,:)%header(8)='date'
    df(el,:)%header(9)='height'
    df(el,:)%header(10)='pressure'
    df(el,:)%header(11)='directW'
    df(el,:)%header(12)='speedW'
    df(el,:)%header(13)='temperature'
    df(el,:)%header(17)='name'

  CASE( 'APXITPS8')
    ilat = 3;    ilon = 4
    FORALL( j=1:NY )
      !value_field2(1:2,j,t) = value_field(1:2,j,t)                         ! index
      value_field2(3:4,j,t)  =  value_field(3:4,j,t)*0.01                   ! lat, lon [degree]
      value_field2(5:12,j,t) = (value_field(5:12,j,t)-1000)*0.1+unitTemp    ! t2m
      value_field2(16,j,t) = value_field(16,j,t)*unitPressure               ! daily average pressure
      ! value_field2(17,j,t) = value_field(17,j,t)*0.1                      ! maxWind
      !value_field2(12:14,j,t)=value_field(12:14,j,t)                       ! direct, speed, kind baric tendency
      ! value_field2(15,j,t)=(value_field(15,j,t)-1000)*unitPressure        ! pressure tendency [Pa/3hour]
      ! value_field2(16:17,j,t)=value_field(16:17,j,t)*0.1                  ! horisontal visiable [m], sunlight [hour]
      ! value_field2(18:19,j,t)=value_field(18:19,j,t)*unitCloud            ! total cloud in [ball->%], Hh[ball->%]
     !value_field2(20:23,j,t)=value_field(20:23,j,t)                      ! Cl[code], Cm[code], Ch[code], HBASE_CL [m]
      ! value_field2(24:25,j,t)=value_field(24:25,j,t)                      ! ww (current weather)[code 4677], ww1(past weather)[code 4561]
      ! value_field2(26:28,j,t)=(value_field(26:28,j,t)-1000)*0.1+unitTemp  ! tmin, tmax, tgmin
      ! value_field2(29:31,j,t)=value_field(29:31,j,t)*0.1                  ! R6,R12,R24 - precipitation for 6,12,24 hours [mm] 
      ! value_field2(32,j,t)=(value_field(32,j,t)-1000)*0.1 +unitTemp       ! TG (temp soil)
     !value_field2(33:40,j,t)=value_field(33:40,j,t)                      ! Hsnow [cm]
      ! value_field2(41:43,j,t)=value_field(41:43,j,t)*0.1                  ! PBagrCorrect, PHCorrect, PExetter [gpa]
      ! value_field2(44:46,j,t)=value_field(44:46,j,t)*0.1                  ! THCorrect, TPExetter, Tmsl [C]

      value_field2(30:31,j,t) = value_field(30:31,j,t)*0.1 + unitTemp

    ENDFORALL

    ! df(el,:)%header(1:36)  = '-'
    df(el,:)%header(1)  = 'index'
    df(el,:)%header(2)  = 'index'
    df(el,:)%header(3)  = 'lat'
    df(el,:)%header(4)  = 'lon'

    df(el,:)%header(5)  = 't2m: 0h'
    df(el,:)%header(6)  = '3h'
    df(el,:)%header(7)  = '6h'
    df(el,:)%header(8)  = '9h'
    df(el,:)%header(9)  = '12h'
    df(el,:)%header(10) = '15h'
    df(el,:)%header(11) = '18h'
    df(el,:)%header(12) = '21h'
    df(el,:)%header(13) = 'tmin'
    df(el,:)%header(14) = 'tmax'
    df(el,:)%header(15) = 'hsnow'

    df(el,:)%header(16) = 'davePS'
    df(el,:)%header(17) = 'maxWind'
    df(el,:)%header(18) = 'daveT'
    df(el,:)%header(19) = 'davePMSL'
    df(el,:)%header(20) = 'daveTd'

    df(el,:)%header(21) = 'R6'
    df(el,:)%header(22) = 'R6'
    df(el,:)%header(23) = 'R6'
    df(el,:)%header(24) = 'R6'
    df(el,:)%header(25) = 'R12'
    df(el,:)%header(26) = 'R12'
    df(el,:)%header(27) = 'R24'
    df(el,:)%header(28) = 'R24'

    df(el,:)%header(29) = 'dursun'

    df(el,:)%header(30) = 'Tnorm'
    df(el,:)%header(31) = 'dTnorm'

    df(el,:)%header(32) = 'cTMin'
    df(el,:)%header(33) = 'cTMax'
    df(el,:)%header(34) = 'minHoriz'

    df(el,:)%header(35) = 'dirMaxW'
    df(el,:)%header(36) = 'ths|hail'

  ENDSELECT handle_maket

  ! Замена входной константы отсутствия данных на выходную
  WHERE( value_field == iundef )  value_field2 = oundef
  ALLOCATE( filterSt(NY) )
  filterSt = .FALSE.

  filter: SELECT CASE( RECNAME(el) )
  CASE( 'SYNOPMAK','SYNOPDOP','TEMPMAKT','TEBDTMAK','TEBDWMAK','TEMPALLC','APXITPS8' )

    IF(LP > 1) WRITE(*,*) 'filterArea = ', filterArea

    IF    ( filterArea == 'RU40'  ) THEN
      WHERE( value_field(1,:,t) > 40 .AND. value_field(1,:,t) < 100 )  &
        filterSt = .TRUE.  ! only for near Russia station 1-40

    ELSEIF( filterArea == 'cm_ena' .OR. filterArea == 'cm_ENA' )THEN
      CALL filterAreaCmENA( value_field2(ilat,:,t), value_field2(ilon,:,t), NY, filterSt )

    ELSEIF( COUNT(filterCoord>0) >2 )THEN
      CALL filterCoordinates( value_field2(ilat,:,t), value_field2(ilon,:,t), NY, filterSt )

    ENDIF

    WHERE( filterSt ) value_field(2,:,t) = iundef

    IF( RECNAME(el)=='TEMPMAKT' )THEN
      CALL remove_sort_voidSt()
    ELSE
      CALL remove_voidSt()
    ENDIF

  CASE( 'SHIPBMAK' )

    IF( filterArea=='RU40' )THEN
      WHERE( value_field(3,:,t) > 30 .AND.  value_field(4,:,t) >  0 )  &
        value_field(2,:,t) = iundef   ! area lat[30-90], lon[0-180]

    ELSEIF( filterArea == 'cm_ena' .OR. filterArea == 'cm_ENA' )THEN
      CALL filterAreaCmENA( value_field2(ilat,:,t), value_field2(ilon,:,t), NY, filterSt )

    ELSEIF( COUNT(filterCoord>0) >2 )THEN
      CALL filterCoordinates( value_field2(ilat,:,t), value_field2(ilon,:,t), NY, filterSt )

    ENDIF

    WHERE( filterSt ) value_field(46,:,t) = iundef
    CALL remove_sort_voidSt2()

  CASE( 'AIREPMAK' )

    CALL remove_sort_voidSt2()
    IF( filterAREA=='RU40' )THEN
      WHERE( value_field(5,:,t) > 30   .AND.   &
             value_field(6,:,t) >  0  )  value_field(2,:,t)=iundef   ! area lat[30-90], lon[0-180]

    ELSEIF( filterArea == 'cm_ena' .OR. filterArea == 'cm_ENA' )THEN
      CALL filterAreaCmENA( value_field2(5,:,t),value_field2(6,:,t), NY, filterSt )

    ELSEIF( COUNT(filterCoord>0) >2 )THEN
      CALL filterCoordinates( value_field2(ilat,:,t), value_field2(ilon,:,t), NY, filterSt )

    ENDIF

    WHERE( filterSt ) value_field(2,:,t) = iundef

    CALL remove_voidSt()

  CASE DEFAULT 
    value_field3(1:NX,1:NY,t) = value_field2(1:NX,1:NY,t)

  ENDSELECT filter
  DEALLOCATE( filterSt )
  
ENDSUBROUTINE HANDLE

SUBROUTINE filterAreaCmENA( gLat, gLon, NY, filterSt )

  INTEGER, INTENT(IN) :: NY
  REAL*4, PARAMETER ::  pollat     =  25 ! latitude of the rotated north pole
  REAL*4, PARAMETER ::  pollon     = -90 ! longitude of the rotated north pole
  REAL*4, PARAMETER ::  rlon_start = -60 ! first longitude of the grid
  REAL*4, PARAMETER ::  rlat_start = -30 ! first latitude of the grid
  REAL*4, PARAMETER ::  rlon_end   =  60 ! last longitude of the grid
  REAL*4, PARAMETER ::  rlat_end   =  30 ! last latitude of the grid
  REAL*4, PARAMETER ::  polgam     =   0 ! angle between the north poles of the systems
  REAL*4 :: phi2phirot, rla2rlarot
  ! INTEGER :: inSt, outSt

  REAL*8,DIMENSION(NY), INTENT(IN) :: glon, glat
  REAL*4,DIMENSION(NY) :: rlon, rlat
  LOGICAL, INTENT(OUT) :: filterSt(NY)

  DO i=1,NY
    rlat(i) = phi2phirot ( REAL(glat(i),4), REAL(glon(i),4), pollat, pollon )
    rlon(i) = rla2rlarot ( REAL(glat(i),4), REAL(glon(i),4), pollat, pollon, polgam )
  ENDDO
  ! CALL STATS(rlat,NY,iundef,'rlat',1)
  ! CALL STATS(rlon,NY,iundef,'rlon',0)
  
  ! IF( LP>4) WRITE(*,*)  rlon(1:10)
  ! IF( LP>4) WRITE(*,*)  glon(1:10)
  ! IF( LP>4) WRITE(*,*)  rlat(1:10)
  ! IF( LP>4) WRITE(*,*)  glat(1:10)
 
  filterSt = .TRUE.
  WHERE( (rlat > rlat_start .AND. rlat < rlat_end) .AND. &
         (rlon > rlon_start .AND. rlon < rlon_end) ) filterSt = .FALSE.
  WHERE( rlat == oundef ) filterSt = .TRUE.
  IF( LP>2) WRITE(*,*) COUNT(filterSt), 'stations will be rejected filterAreaCmENA'

ENDSUBROUTINE filterAreaCmENA

SUBROUTINE filterCoordinates( gLat, gLon, NY, filterSt )

  INTEGER, INTENT(IN) :: NY

  REAL*8,DIMENSION(NY), INTENT(IN) :: glon, glat
  LOGICAL, INTENT(OUT) :: filterSt(NY)

  filterSt = .TRUE.
  WHERE( (glat >= filterCoord(3) .AND. glat <= filterCoord(4)  ) .AND. &
         (glon >= filterCoord(1) .AND. glon <= filterCoord(2)) ) filterSt = .FALSE.
  WHERE( glat == oundef ) filterSt = .TRUE.
  IF( LP>2) WRITE(*,*) COUNT(filterSt), 'stations will be rejected filterCoordinates'

ENDSUBROUTINE filterCoordinates

SUBROUTINE remove_voidSt()

! turn off empty station
    jj=0
    DO j=1,NY
      IF ( value_field(2,j,t) /= iundef .and. &
           value_field(1,j,t) /= 22222  .and. &
           value_field(1,j,t) /= iundef .and. &
           value_field(1,j,t) /= oundef       ) THEN
           ! WRITE(*,*) value_field(2,j,t), value_field(1,j,t)
        jj=jj+1
        value_field3(:,jj,t) = value_field2(:,j,t)
      ENDIF
    ENDDO

    NY = jj
    ! IF ( value_field(1,NY,t)==22222 ) THEN
      ! NY=jj-1        ! last station 22222
      ! IF (LP > 4 ) WRITE (*,*) 'last station was remove'
    ! ENDIF

    df(el,t)%NY = NY
    IF (LP > 1) WRITE(*,'("valid station=",i5," from ", i5)') NY, j-1

ENDSUBROUTINE remove_voidSt

SUBROUTINE remove_sort_voidSt()

  INTEGER*4  jj1, jj2, jj3

  IF ( value_field(1,NY,t)==22222 ) THEN
    NY=NY-1        ! last station 22222
    IF(LP > 4) WRITE (*,*) 'last station was remove'
  ENDIF

! remove empty station
  jj1=0; jj2=0; jj3=0
  DO j=1,NY
    IF( value_field(2,j,t) /= iundef .and.  &
        value_field(1,j,t) /= 999.   .and.  &
        value_field(3,j,t) == 2         )THEN
      jj1 = jj1+1
      value_field3(:,jj1,t) = value_field2(:,j,t)
    ENDIF
  ENDDO
  jj2 = jj1
  DO j=1,NY
    IF( value_field(2,j,t) /= iundef .and. value_field(1,j,t) == 999. )THEN
      jj2 = jj2+1
      value_field3(:,jj2,t) = value_field2(:,j,t)
    ENDIF
  ENDDO
  jj3 = jj2
  DO j=1,NY
    IF( value_field(2,j,t) /= iundef .and. value_field(3,j,t) == 8 )THEN
      jj3 = jj3+1
      value_field3(:,jj3,t) = value_field2(:,j,t)
    ENDIF
  ENDDO

  df(el,t)%NYY(1) = jj1
  df(el,t)%NYY(2) = jj2 - jj1
  df(el,t)%NYY(3) = jj3 - jj2
  ! WRITE(*,*) jj1, jj2, jj3

  NY = df(el,t)%NYY(1) + df(el,t)%NYY(2) + df(el,t)%NYY(3)
  df(el,t)%NY = NY

  WRITE(*,*)'valid station=', df(el,t)%NYY(1:3),' from ', NY

ENDSUBROUTINE remove_sort_voidSt

SUBROUTINE remove_sort_voidSt2()
  INTEGER*4  jj1, jj2
  INTEGER*4  icf(2), vcf(2), lab ! index and value checking field, position for char=name

  SELECT CASE( RECNAME(el) )
  CASE( 'SHIPBMAK' )
    icf = (/46,46/)
    vcf = (/21,152/) ! ship, buoy
    lab = 5
  CASE( 'AIREPMAK' )
    icf = (/ 4, 4/)
    vcf = (/43,45/)  ! AMDAR, AIREP
    lab = 17
  ENDSELECT

  IF (LP > 3 )  WRITE (*,*) '  sort and remove 2 value_field'

  IF( value_field(1,NY,t)==22222 )THEN
    NY = NY - 1        ! last station 22222
    IF( LP > 4 ) WRITE (*,*) 'last station was remove'
  ENDIF

! remove empty station
  jj1 = 0; jj2 = 0
  DO j=1,NY
    IF( value_field(icf(1),j,t) == vcf(1) )THEN
      jj1 = jj1 + 1
      value_field3(:,jj1,t) = value_field2(:,j,t)
      WRITE(idST(jj1),'(4a2)') INT(value_field2(lab:lab+3,j,t))
      df(el,t)%idST(jj1)(1:8) = TRIM(idST(jj1)(1:8))
    ENDIF
  ENDDO
  jj2 = jj1
  DO j=1,NY
    IF( value_field(icf(2),j,t) == vcf(2) )THEN
      jj2 = jj2+1
      value_field3(:,jj2,t) = value_field2(:,j,t)
      WRITE(idST(jj2),'(4a2)') INT(value_field2(lab:lab+3,j,t))
      df(el,t)%idST(jj2)(1:8) = TRIM(idST(jj2)(1:8))
    ENDIF
  ENDDO

  df(el,t)%NYY(1) = jj1
  df(el,t)%NYY(2) = jj2-jj1

  NY = df(el,t)%NYY(1) + df(el,t)%NYY(2)
  df(el,t)%NY = NY

  WRITE(*,*) 'valid station=', df(el,t)%NYY(1), df(el,t)%NYY(2),' from ',j-1

ENDSUBROUTINE remove_sort_voidSt2
