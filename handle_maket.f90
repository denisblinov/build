SUBROUTINE handle()
  
  INTEGER*4 mZ, mT, mW  ! number level on surface{0-1}, tropopause {0-6}, windmax {0-6} for TEMPMAKT
  REAL*4       unitTemp, unitPressure, unitCloud

!------------SET units for variable P and T------------------------
  SELECT CASE( trim(typefile) )
  CASE('maket','csv','stantion')   !  [C] and [gPa]
    unitTemp=0.0
    unitPressure=0.1
  CASE DEFAULT                     !  [K] and [Pa]
    unitTemp=273.15
    unitPressure=10.0
  ENDSELECT
  unitCloud=10    ! [%]
!------------------------------------------------------------------

  set_id: SELECT CASE ( RECNAME(el) )
  CASE( 'SYNOPMAK','SYNOPDOP','TEMPMAKT','TEBDTMAK','TEBDWMAK','TEMPALLC' )
    ListSt(1:NY)=INT(value_field(1,1:NY,t))*1000+INT(value_field(2,:,t))

    DO j=1,NY
      WRITE(idST(j),'(i5.5,a3)')    ListSt(j),'   '
      df(el,t)%idST(j)(1:8)=idST(j)(1:8)
    ENDDO
    !WRITE(*,*)  '==',idST(200)(1:8),'=='
    !WRITE(string1,'(10000i8.8)')  idST

  ENDSELECT set_id

  handle_maket: SELECT CASE( RECNAME(el) )
  CASE( 'SYNOPMAK','SYNOPDOP')
    FORALL( j=1:NY )
     !value_field2(1:2,j,t)=value_field(1:2,j,t)                          ! index
      value_field2(3:4,j,t)=value_field(3:4,j,t)*0.01                     ! lat, lon [degree]
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
     !value_field2(33:40,j,t)=value_field(33:40,j,t)                      ! Hsnow [cm]
      value_field2(41:43,j,t)=value_field(41:43,j,t)*0.1                  ! PBagrCorrect, PHCorrect, PExetter [gpa]
      value_field2(44:46,j,t)=value_field(44:46,j,t)*0.1                  ! THCorrect, TPExetter, Tmsl [C]
    END FORALL
    ! отрицательные значени€ - это забракованные ASOI данные
    WHERE (  value_field(8:13,:,t) < 0  )  value_field2(8:13,:,t)=oundef
    ! ¬ базе встречаютс€ значени€ осадков, равные +9990. 
    ! приходитс€ их приравнивать к константе отсутстви€.
    WHERE( value_field(29:31,:,t) == 9990.0 ) value_field2(29:31,:,t)=oundef
    ! ¬ базе встречаютс€ значени€ облачности, равное 99 
    ! приходитс€ еЄ приравнивать к константе отсутстви€.
    WHERE( value_field(18,:,t) == 99.0 ) value_field2(18,:,t)=oundef
    ! «адать знак тенденции согласно  Ќ-01
    WHERE( value_field(14,:,t) > 4.0 ) value_field2(15,:,t)=-value_field2(15,:,t)

    ! ÷икл для подстановки координат из словар€
    DO j=1,NY
      DO jj=1,17000
        IF( ListSt(j) == dictionarySt(1,jj) )THEN
          value_field2(3,j,t) = dictionarySt(2,jj)*0.001_8           ! lat from dictionary
          value_field2(4,j,t) = dictionarySt(3,jj)*0.001_8           ! lon from dictionary
          EXIT  ! other station no used
        ENDIF
      ENDDO
    ENDDO
! «акомментировал, так как это исправили.
!!     ¬ручную установить высоту станции Ўереметьево
       WHERE( ListSt(:)==27514 ) value_field2(5,:,t)=187
!!     ¬ручную устанвлены высоты 4 станций в Ўведции, так как там незадана высота
       WHERE( ListSt(:)==2287 ) value_field2(5,:,t)=   7.
       WHERE( ListSt(:)==2561 ) value_field2(5,:,t)=  73.
       WHERE( ListSt(:)==2629 ) value_field2(5,:,t)= 104.
       WHERE( ListSt(:)==2667 ) value_field2(5,:,t)=   4.

      df(el,:)%header(1)='index'
      df(el,:)%header(2)='index'
      df(el,:)%header(3)='lat'
      df(el,:)%header(4)='lon'
      df(el,:)%header(5)='height'
      df(el,:)%header(6)='cP'  ! 'PS_corr'
      df(el,:)%header(7)='gps'
      df(el,:)%header(8)='ps'
      df(el,:)%header(9)='pmsl'
      df(el,:)%header(10)='t_2m'
      df(el,:)%header(11)='td_2m'
      df(el,:)%header(12)='dd_10m'
      df(el,:)%header(13)='ff_10m'
      df(el,:)%header(14)='typTPS'
      df(el,:)%header(15)='tps'
      df(el,:)%header(16)='hVV'
      df(el,:)%header(17)='tSunl'
      df(el,:)%header(18)='clct'
      df(el,:)%header(19)='clcl_clcm'
      df(el,:)%header(20)='typLC'
      df(el,:)%header(21)='typMC'
      df(el,:)%header(22)='typHC'
      df(el,:)%header(23)='hBasLC'
      df(el,:)%header(24)='WW'
      df(el,:)%header(25)='pWW'
      df(el,:)%header(26)='tMin2m'
      df(el,:)%header(27)='tMax2m'
      df(el,:)%header(28)='tMinG'
      df(el,:)%header(29)='R6'
      df(el,:)%header(30)='R12'
      df(el,:)%header(31)='R24'
      df(el,:)%header(32)='t_g'
      df(el,:)%header(33)='hSnow'
      df(el,:)%header(34)='shiftR'
      df(el,:)%header(35)='E4'
      df(el,:)%header(36)='E1'
      df(el,:)%header(37)='E3'
      df(el,:)%header(38)='sp1'
      df(el,:)%header(39)='sp2'
      df(el,:)%header(40)='sp3'
      df(el,:)%header(41)='pmsl-B'
      df(el,:)%header(42)='pmsl-hc'
      df(el,:)%header(43)='pmsl-MO'
      df(el,:)%header(44)='t_2m-hc'
      df(el,:)%header(45)='t_2m-MO'
      df(el,:)%header(46)='t_msl'

    CASE( 'TEMPMAKT' ) handle_maket
      FORALL( j=1:NY )
       !value_field2(1:4,j,t)=value_field(1:4,j,t)                    ! indexRegion, indexSt
       !value_field2(3,j,t)=value_field(3,j,t)                        ! marker: 2(TEMP, TEMP-SHIP) or 8(PILOT)
       !value_field2(4,j,t)=value_field(4,j,t)                        ! altitude
        value_field2(5:6,j,t)=value_field(5:6,j,t)*0.01               ! lat, lon
       !value_field2(7:10,j,el,t)=value_field(7:10,j,t)               ! hour, day, reserv1, reserv2
        value_field2(11,j,t)=value_field(11,j,t)-1000                 ! H1000
        value_field2(12,j,t)=(value_field(12,j,t)-1000.)*0.1+unitTemp ! T on 1000gPa
        value_field2(13,j,t)=value_field(13,j,t)*0.1                  ! Td (rcld-saturation deficit) on level 1000gPa
       !value_field2(14,j,t)=value_field(14,j,t)                      ! direct of wind on level [degree]
       !value_field2(15,j,t)=value_field(15,j,t)                      ! speed  of wind on level [m/s]
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

      value_field2(96:142,:,t)=oundef

      DO j=1,NY
        mz=value_field(93,j,t)
        mt=value_field(94,j,t)
        mw=value_field(95,j,t)

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
      df(el,:)%header(13)='Td1000'
      df(el,:)%header(14)='WD1000'
      df(el,:)%header(15)='WS1000'
      df(el,:)%header(16)='H925'
      df(el,:)%header(17)='T925'
      df(el,:)%header(18)='Td925'
      df(el,:)%header(19)='WD925'
      df(el,:)%header(20)='WS925'
      df(el,:)%header(21)='H850'
      df(el,:)%header(22)='T850'
      df(el,:)%header(23)='Td850'
      df(el,:)%header(24)='WD850'
      df(el,:)%header(25)='WS850'
      df(el,:)%header(26)='H700'
      df(el,:)%header(27)='T700'
      df(el,:)%header(28)='Td700'
      df(el,:)%header(29)='WD700'
      df(el,:)%header(30)='WS700'
      df(el,:)%header(31)='H500'
      df(el,:)%header(32)='T500'
      df(el,:)%header(33)='Td500'
      df(el,:)%header(34)='WD500'
      df(el,:)%header(35)='WS500'
      df(el,:)%header(36)='H400'
      df(el,:)%header(37)='T400'
      df(el,:)%header(38)='Td400'
      df(el,:)%header(39)='WD400'
      df(el,:)%header(40)='WS400'
      df(el,:)%header(41)='H300'
      df(el,:)%header(42)='T300'
      df(el,:)%header(43)='Td300'
      df(el,:)%header(44)='WD300'
      df(el,:)%header(45)='WS300'
      df(el,:)%header(46)='H250'
      df(el,:)%header(47)='T250'
      df(el,:)%header(48)='Td250'
      df(el,:)%header(49)='WD250'
      df(el,:)%header(50)='WS250'
      
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

!     string Type(Sr) RaRaSaSa OSurf OTropop OmaxW" 

    CASE( 'TEBDTMAK' ) handle_maket
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
      FORALL( j=1:NY )
        value_field2(3,j,t) = value_field(1,j,t)                     ! marker start
        value_field2(1:2,j,t)=value_field(2:3,j,t)                   ! indexRegion, indexSt
        value_field2(2,j,t)=value_field(3,j,t)                       ! indexSt
       !value_field2(4,j,t)=value_field(4,j,t)                       ! marker observation (7)
       !value_field2(5,j,t)=value_field(5,j,t)                       ! altitude
        value_field2(6:7,j,t)=value_field(6:7,j,t)*0.01              ! lat, lon
       !value_field2(11,j,t)=value_field(11,j,t)                     ! nlevels
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

    FORALL( j=1:NY )
     !value_field2(1:2,j,t)=value_field(1:2,j,t)                          ! 99, number report
      value_field2(3:4,j,t)=value_field(3:4,j,t)*0.01                     ! lat, lon [degree]
     !value_field2(5:8,j,el,t)=value_field(5:8,j,t)                       ! name of ship or number buoy
      value_field2(9,j,t)=value_field(9,j,t)*unitPressure                 ! pmsl
      value_field2(10:11,j,t)=(value_field(10:11,j,t)-1000)*0.1+unitTemp  ! t_2m, td_2m
     !value_field2(12:14,j,t)=value_field(12:14,j,t)                      ! direct, speed, kind baric tendency
      value_field2(15,j,t)=(value_field(15,j,t)-1000)*unitPressure        ! pressure tendency [Pa/3hour]
      value_field2(16,j,t)=value_field(16,j,t)*10                         ! horisontal visiable [m]
      value_field2(17,j,t)=value_field(17,j,t)*0.1                        ! sunlight [hour]
      value_field2(18:19,j,t)=value_field(18:19,j,t)*unitCloud            ! total cloud in [ball->%], Hh[ball->%]
     !value_field2(20:23,j,t)=value_field(20:23,j,t)                      ! Cl[code], Cm[code], Ch[code], HBASE_CL [m]
     !value_field2(24:25,j,t)=value_field(24:25,j,t)                      ! ww (current weather)[code 4677], ww1(past weather)[code 4561]
      value_field2(26,j,t)=(value_field(26,j,t)-1000)*0.1+unitTemp        ! t_surface_water
     !value_field2(27:41,j,t)=value_field(27:41,j,t)                      ! reserv
      value_field2(42:43,j,t)=value_field(42:43,j,t)*unitPressure         ! PHCorrect, PExetter
      value_field2(44:45,j,t)=value_field(44:45,j,t)*0.1+unitTemp         ! THCorrect, TPExetter
     !value_field2(46,j,t)=value_field(46,j,t)                            ! marker: 21 - SHIP, 152 - BUOY
    END FORALL

    ! отрицательные значени€ - это забракованные данные задачей OASYN
    WHERE (  value_field(8:13,:,t)  < 0  )  value_field2(8:13,:,t) =oundef
    ! ¬ базе встречаютс€ значени€ облачности, равное 99 
    ! приходитс€ еЄ приравнивать к константе отсутстви€.
    WHERE( value_field(18,:,t) == 99.0 ) value_field2(18,:,t)=oundef
    ! «адать знак тенденции согласно  Ќ-01
    WHERE( value_field(14,:,t) > 4.0 ) value_field2(15,:,t)=-value_field2(15,:,t)

    df(el,:)%header(1)='index'
    df(el,:)%header(2)='number'
    df(el,:)%header(3)='lat'
    df(el,:)%header(4)='lon'
    df(el,:)%header(5)='name'
    df(el,:)%header(9)='PMSL'
    df(el,:)%header(10)='t_2m'
    df(el,:)%header(11)='td_2m'
    df(el,:)%header(12)='directW'
    df(el,:)%header(13)='speedW'
    df(el,:)%header(14)='type_dpst'
    df(el,:)%header(15)='dpst'
    df(el,:)%header(16)='hVV'
    df(el,:)%header(17)='tsunl'
    df(el,:)%header(18)='clct'
    df(el,:)%header(19)='clcl/clcm'
    df(el,:)%header(20)='clcl'
    df(el,:)%header(21)='clcm'
    df(el,:)%header(22)='clch'
    df(el,:)%header(23)='hbas_cl'
    df(el,:)%header(24)='WW'
    df(el,:)%header(25)='pWW'
    df(el,:)%header(26)='t_s'
    df(el,:)%header(42)='pmsl_hc'
    df(el,:)%header(43)='pmsl_uk'
    df(el,:)%header(44)='t2m_hc'
    df(el,:)%header(45)='t2m_uk'
    df(el,:)%header(46)='marker'

  CASE( 'AIREPMAK')
    FORALL( j=1:NY ) ! 1:20000
     !value_field2(1:3,j,t)=value_field(1:3,j,t)                          ! 500, number report, 5
     !value_field2(4,j,t)=value_field(4,j,t)                              ! marker: 43 - AMDAR, 45 - AIREP
      value_field2(5:6,j,t)=value_field(5:6,j,t)*0.01                     ! lat, lon
     !value_field2(7:8,j,el,t)=value_field(7:8,j,t)                       ! time in hhmm, day in MMdd
     !value_field2(9,j,el,t)=value_field(9,j,t)                           ! height of flight [m]
      value_field2(10,j,t)=value_field(10,j,t)*unitPressure               ! pressure 
     !value_field2(11:12,j,t)=value_field(11:12,j,t)                      ! direct wind [degree], speed wind [m/s]
      value_field2(13,j,t)=(value_field(13,j,t)-1000)*0.1+unitTemp        ! temperature
     !value_field2(14:16,j,t)=value_field(14:16,j,t)                      ! reserv
     !value_field2(17:20,j,t)=value_field(17:20,j,t)                      ! name of aircraft

    END FORALL
    ! отрицательные значени€ - это забракованные данные задачей CONTREL
    WHERE (  value_field(11:12,:,t) < 0  )  value_field2(11:12,:,t)=oundef

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
    
  ENDSELECT handle_maket

  ! »зменить входную константу отсутстви€ данных на выходную
  WHERE( value_field == iundef  )   value_field2=oundef
  
  SELECT CASE( RECNAME(el) )
  CASE( 'SYNOPMAK','SYNOPDOP','TEMPMAKT','TEBDTMAK','TEBDWMAK','TEMPALLC' )

    IF(filterAREA=='RU40') THEN
      WHERE( value_field(1,:,t) > 40 .and. value_field(1,:,t) < 100 )  value_field(2,:,t)=iundef   ! only for near Russia station 1-40
    ENDIF

    IF( RECNAME(el)=='TEMPMAKT' )THEN
      CALL remove_sort_voidSt()
    ELSE
      CALL remove_voidSt()
    ENDIF

  CASE( 'SHIPBMAK' )

    IF(filterAREA=='RU40') THEN
      WHERE( value_field(3,:,t) > 30     &
       .and. value_field(4,:,t) >  0  )  value_field(2,:,t)=iundef   ! area lat[30-90], lon[0-180]
    ENDIF

    CALL remove_sort_voidSt2()

  CASE( 'AIREPMAK' )
    CALL remove_sort_voidSt2()
    IF(filterAREA=='RU40') THEN
      WHERE( value_field(5,:,t) > 30   .and.   &
             value_field(6,:,t) >  0  )  value_field(2,:,t)=iundef   ! area lat[30-90], lon[0-180]
    ENDIF

  CASE DEFAULT 
    value_field3(1:NX,1:NY,t)=value_field2(1:NX,1:NY,t)

  ENDSELECT
  
ENDSUBROUTINE HANDLE

SUBROUTINE remove_voidSt()

! turn off empty station
    jj=0
    DO j=1,NY
      IF ( value_field(2,j,t) /= iundef .and. &
           value_field(1,j,t) /= 22222        ) THEN
        jj=jj+1
        value_field3(:,jj,t)=value_field2(:,j,t)
      ENDIF
    ENDDO

    NY=jj
    ! IF ( value_field(1,NY,t)==22222 ) THEN
      ! NY=jj-1        ! last stantion 22222
      ! IF (LP > 4 ) WRITE (*,*) 'last stantion was remove'
    ! ENDIF

    df(el,t)%NY = NY
    IF (LP > 1) WRITE(*,'("valid stantion=",i5," from ", i5)') NY, j-1

ENDSUBROUTINE remove_voidSt

SUBROUTINE remove_sort_voidSt()

  INTEGER*4  jj1, jj2, jj3

  IF ( value_field(1,NY,t)==22222 ) THEN
    NY=NY-1        ! last stantion 22222
    IF(LP > 4) WRITE (*,*) 'last stantion was remove'
  ENDIF

! remove empty station
  jj1=0; jj2=0; jj3=0
  DO j=1,NY
    IF( value_field(2,j,t) /= iundef .and.  &
        value_field(1,j,t) /= 999.   .and.  &
        value_field(3,j,t) == 2         )THEN
      jj1 = jj1+1
      value_field3(:,jj1,t)=value_field2(:,j,t)
    ENDIF
  ENDDO
  jj2 = jj1
  DO j=1,NY
    IF( value_field(2,j,t) /= iundef .and. value_field(1,j,t) == 999. )THEN
      jj2 = jj2+1
      value_field3(:,jj2,t)=value_field2(:,j,t)
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
  df(el,t)%NYY(3) = jj3 - jj2 - jj1

  NY = df(el,t)%NYY(1) + df(el,t)%NYY(2) + df(el,t)%NYY(3)
  df(el,t)%NY = NY

  WRITE(*,*) 'valid stantion=', df(el,t)%NYY(1:3),' from ', j-1, NY

ENDSUBROUTINE remove_sort_voidSt

SUBROUTINE remove_sort_voidSt2()
  INTEGER*4  jj1, jj2
  INTEGER*4  icf(2), vcf(2), lab ! index and value checking field, position for char=name

  SELECT CASE( RECNAME(el) )
  CASE( 'SHIPBMAK' )
    icf=(/46,46/)
    vcf=(/21,152/) ! ship, buoy
    lab=5
  CASE( 'AIREPMAK' )
    icf=(/ 4, 4/)
    vcf=(/43,45/)  ! AMDAR, AIREP
    lab=17
  ENDSELECT

  IF (LP > 3 )  WRITE (*,*) '  sort and remove 2 value_field'

  IF ( value_field(1,NY,t)==22222 ) THEN
    NY=NY-1        ! last stantion 22222
    IF(LP > 4) WRITE (*,*) 'last stantion was remove'
  ENDIF

! remove empty station
  jj1=0;jj2=0
  DO j=1,NY
    IF( value_field(icf(1),j,t) == vcf(1) )THEN
      jj1=jj1+1
      value_field3(:,jj1,t)=value_field2(:,j,t)
      WRITE(idST(jj1),'(4a2)') INT(value_field2(lab:lab+3,j,t))
      df(el,t)%idST(jj1)(1:8) = TRIM(idST(jj1)(1:8))
    ENDIF
  ENDDO
  jj2=jj1
  DO j=1,NY
    IF( value_field(icf(2),j,t) == vcf(2) )THEN
      jj2=jj2+1
      value_field3(:,jj2,t)=value_field2(:,j,t)
      WRITE(idST(jj2),'(4a2)') INT(value_field2(lab:lab+3,j,t))
      df(el,t)%idST(jj2)(1:8) = TRIM(idST(jj2)(1:8))
    ENDIF
  ENDDO

  df(el,t)%NYY(1)=jj1
  df(el,t)%NYY(2)=jj2-jj1

  NY=df(el,t)%NYY(1) + df(el,t)%NYY(2)
  df(el,t)%NY=NY

  WRITE(*,*) 'valid stantion=', df(el,t)%NYY(1), df(el,t)%NYY(2),' from ',j-1

ENDSUBROUTINE remove_sort_voidSt2
