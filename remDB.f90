!*********************************************************************************
!            THE PROGRAM FOR READING THE FIELDS FROM Remote DataBase
!*********************************************************************************
!
! Description: reading RHM-database
!
! Author: Denis Blinov, denisblinov@ya.ru 2008-2015
! Resourse: 
! - library LibServHMC.a (mandatory) for reading data from remote-base
! - library bankload.a (optional) for reading data from local-base
! - library netcdf(optional) for writing data in netcdf-format
! - library bufr(optional) for writing data in bufr-format
!
! TODO:
! - Внедрить подпрорамму проверки ошибок/аварийного завершения HANDLE_ERROR
! - format printing and OUT message 
! - optimize section OUTPUT
! - describe DefGrid
! - add print legend of variables for maket and csv: long name and unit
! - delete SYNOPDOP from input - only for internal calling
! - good format output for TEMPXXXX 
! - check field at surface, tropopause, vmax etc in TEMPMAKT
! - right format for csv-output
! - filter for COSMO-ENA13: add rotate convertion

PROGRAM ReadBASE     
  USE  RemDB
  USE  main_data
  USE  routine4netCDF
  IMPLICIT NONE

!--------------  Переменные для базы данных -----------------------------
  CHARACTER*15 HOST/'192.168.97.72'/        ! ip-adress or name host with base
  CHARACTER*4  nameBASE           ! name base
  INTEGER*4    codeOPEN/560701/, &    ! code access for OPEN base(one for one user/host)   
               codeBASE               ! code(number) base /420511/
  LOGICAL   :: statBASE=.false. , cycleBASE=.true.
!------------------------------------------------------------------------
!>>------------  Переменные для типа запроса ----------------------------
  LOGICAL      infoGrid/0/, infoListFields/0/, infoRecord/0/
  LOGICAL      readFIELD/1/, readFIELD2/0/, rMaket/0/
  INTEGER*4    typeREQUEST
!<<----------------------------------------------------------------------
!--------------  Переменные для сетки и размерностей --------------------
  INTEGER, PARAMETER ::   NELmax=200       ! максимальное количество 2-мерных полей
  INTEGER*4  NX/0/, &   ! Количество точек по оси X
             NY/0/, &   ! Количество точек по оси Y
             NT/1/, &   ! Количество точек по оси T
             NZ/1/, &   ! Количество точек по оси Z
!            NF/1/, &   ! Количество точек по заблаговременностям (T2) или по членам анcамбля
             NEL/0/, NEL0, &  ! Количество полей (метеоэлементов)
             NumbSt/40000/  ! Количество станций - надо уменьшить до 17тыс.- в случае станций, но не хватает для 
              ! координаты отдельного экземпляра записи в ряду данных.

  CHARACTER*8, DIMENSION(NELmax) ::  listfields
!------------------------------------------------------------------------
!--------------  Переменные для вывода / печати --------------------
  CHARACTER*20  &
    typefile,   &  ! { text maket csv list_station netCDF4COSMO bufr4versus stGrads binary binary4GRADS }
    filterArea, filterObs, strDATE
  CHARACTER*10   char10, typeINPUT
  CHARACTER(LEN=40000) ListName
  CHARACTER*30  ::  FileName = 'fields.dat' , fmOU= '(g)', oFileName='fields.dat'
  CHARACTER*256 ::  oDirName ='./', inFile
  LOGICAL::      appendfile=.false.

!------------------------------------------------------------------------

!->>------------  Внутренние переменные (для работы программы)------------
  INTEGER*4    i, j, jj, k, l, ll, z, ncount/1/, nzab, amountPt   ! переменные для циклов
  INTEGER*4    iunit/1001/, ounit/2001/    ! номера для входного и выходного потока
  INTEGER*4    codeRETURN, incrYdir/1/ ! , increment in Y-Direction
  INTEGER*4    DefRec(60), DefGrid(60), LP
  INTEGER*4    oNXstart/1/, oNXend/1/, oNYstart/1/, oNYend/1/
  INTEGER*4    Nrecord, NLEV/1/, NFLAG/1/, nterm
  CHARACTER*8, DIMENSION(NELmax)  :: RECNAME        ! full name field for in base
  INTEGER*4, DIMENSION(100) :: listPoints
  CHARACTER*8, ALLOCATABLE ::  idST(:)    !
  INTEGER*4, ALLOCATABLE ::  iArray(:,:,:), ListSt(:) ! for Maket
  INTEGER*8   ::  dictionarySt(6,17000), nST ! dictionary of station for rdslov
  CHARACTER(LEN=8)  ::  dictionaryStName(2,17000) ! name dictionary of station for rdslov
  REAL*4   ::  dictionaryStation(8,17000)    ! dictionary of station - all 8 fields

  REAL*4, ALLOCATABLE ::  value_field(:,:,:)
  REAL*8, ALLOCATABLE ::  value_field2(:,:,:), value_field3(:,:,:)
  CHARACTER*3  termForecast
!!CHARACTER(LEN=500) :: HEADSYNOP

!-<<-------------------------------------------------------------------<<-

  NAMELIST /BASE/    HOST, nameBASE, statBASE, codeBASE, codeOPEN, LevelPrint
  NAMELIST /REQUEST/ infoGrid, infoListFields, infoRecord,                    & ! ACTION
                     readFIELD, readFIELD2, rMaket
  NAMELIST /FIELD/   listfields, DATEHH, date_end,                            &
                     HFstep, startTerm, endTerm
  NAMELIST /OUTPUT/  typefile, FileName, oDirName, fmOU, appendfile,          &
                     listPoints, filterArea

!------------------------------------------------------------------

!  1.  read namelist and initial
!  2.  connection with base
!  2.1   OPEN host and base
!  2.2   cycle on term
!  2.3   cycle on meteoelement
!  2.4   info
!  2.5   receive data
!  3.    handle array
!  4.    OUTPUT

!-----------------------------
!     initial default value
  DATEHH(:)=oundef
  listPoints=iundef
  listfields(:)='--------'
!-----------------------------

  CALL GETARG(1,typeINPUT)
  CALL GETARG(2,inFile)

  IF (trim(typeINPUT)=='tty') THEN
    iunit=5
    WRITE(*,*) ' input variable from namelist &BASE'
    READ(iunit,base)
    WRITE(*,*) ' input variable from namelist &REQUEST'
    READ(iunit,request)
    WRITE(*,*) ' input variable from namelist &FIELD'
    READ(iunit,field) 
    WRITE(*,*) ' input variable from namelist &OUTPUT'
    READ(iunit,output) 
  ELSEIF(trim(typeINPUT)=='-f') THEN
    iunit=1001
    OPEN (iunit, file=trim(inFile), status='old')
    READ(iunit,base)
    READ(iunit,request)
    READ(iunit,field) 
    READ(iunit,output) 
    CLOSE(iunit)
  ELSE
    iunit=1001
    OPEN (iunit, file="remDB.nl", status='old')
    READ(iunit,base)
    READ(iunit,request)
    READ(iunit,field) 
    READ(iunit,output) 
    CLOSE(iunit)
  ENDIF
  
!----------------------------------------
  LP = LevelPrint
  IF (LP>4)  WRITE(6,base)
  IF (LP>4)  WRITE(6,request)
  IF (LP>4)  WRITE(6,field)
  IF (LP>4)  WRITE(6,output)
!----------------------------------------

  IF ( startTerm>endTerm )  endTerm=startTerm

  NT =COUNT( DATEHH > 1900010100, DIM=1 ) 
  SELECT CASE (listfields(1))
  CASE( 'SYNOPMAK', 'SYNOPDOP','SHIPBMAK', 'TEMPMAKT', 'TEBDTMAK', 'TEBDWMAK', 'AIREPMAK' )
    hstep=6
  CASE( 'TEMPALLC' )
    hstep=12
  CASE DEFAULT
    hstep=6
  ENDSELECT

  DO WHILE( date_end > DATEHH(nt))
    DATEHH(nt+1)=newDATE(DATEHH(nt),hstep)
    nt=nt+1
    IF(LP>4) WRITE(*,*) 'new date =', DATEHH(nt),' (', DATEHH(nt-1),' )'
  ENDDO

  NEL0=count( listfields(:)/='--------' , DIM=1 )
  
  ! set undef value for typefile
  SELECT CASE (trim(typefile))
  CASE( 'bufr4versus' )
    oundef=1.7D38
  CASE( 'binary4GRADS' )
    oundef=-9999.
  ENDSELECT
  
  IF(LP>4) WRITE(*,*) HUGE(oundef),MAXEXPONENT(oundef),RADIX(oundef)
  
!------------------------------------------------------
  WRITE(*,*)  ! start output on terminal
!---------estimate number RECORD-----------------------

  j=0;i=0
  loopListFields:  DO WHILE  ( i<NELmax .and. listfields(i+1)/='--------' )  ! cycle listfields
    !if( listfields(i)=='--------' )  exit loopListFields        ! exit when void value exist
    j=j+1;i=i+1

    IF( scan(listfields(i),'*_?.')==5 ) THEN
      DO nterm = startTerm, endTerm, HFstep
        WRITE (termForecast,'(i3.3)') nterm
        RECNAME(j) = listfields(i)(1:4)//termForecast//listfields(i)(8:8)
        WRITE(*,*) 'compute template value ','nterm = ', nterm, 'field = ', RECNAME(j)
        j=j+1
      ENDDO
      j=j-1
    ELSE
      RECNAME(j) = listfields(i)
    ENDIF

  ENDDO loopListFields

  NEL=j
  IF (LP>4) WRITE(*,*)'NT=',NT, 'nel=', NEL, ' - after globbing'

!--------------------------------------------

  IF( statBASE ) cycleBASE=.false.

  Date(1:NT)=DATEHH(1:NT)/100          ! YMD422с  YYYYMMDD  Y4M2D2_c
  year(1:NT)=Date(1:NT)/10000          ! YYYYc  cYYYY  iYYYY year
  month=mod(Date(1:NT), 10000)/100     ! month iMM
  Day(1:NT)=mod(Date(1:NT), 100)       ! Day DD iDD
  hour(1:NT)=mod(DATEHH(1:NT), 100)    ! iHH hour

  ALLOCATE( df(1:nel,1:NT), var(1:nel,1:NT) )


!====================================================================
!--------------------------------------------------------------------
!                      OPENING BASES
!                    -------------
  IF(HOST=='local' ) THEN
    CALL OPENb(codeOPEN, nameBASE, 1_4, codeRETURN)
  ELSE
    CALL OPENRemHost(HOST, codeRETURN)
    CALL HANDLE_ERROR(codeRETURN,' Error OPENing Host ','STOP')

    CALL OPENRemDB(codeOPEN, nameBASE, 1_4, codeRETURN)
  ENDIF
  CALL HANDLE_ERROR(codeRETURN,' Error OPENing data Bases ','STOP')

!==========================================================================
!              GETTING info about grid, record, listfields
!              -------------------------------------------

!    ----------ListRecord---------------
  IF ( infoListFields ) THEN
    CALL GnameRemDB(nameBASE, codeBASE, Nrecord, ListName, codeRETURN)    ! list variable in field
    WRITE (*,*) 'number field', Nrecord
    WRITE (*,'(9(a8,3x))') (ListName(i:i+8), i=1,Nrecord*8, 8)
  ENDIF

  LoopRecord:  do el=1, NEL        ! record cycle

    df(el,:)%RECNAME = RECNAME(el)
    df(el,:)%RECNAME = RECNAME(el)

    !    ----------infoGrid---------------
    IF (infoGrid .and. cycleBASE )  THEN
      CALL GetcGrRemDB(nameBASE, codeBASE, RECNAME(el), DefGrid, codeRETURN)
    ELSEIF (infoGrid .and.  statBASE ) THEN
      CALL GetqGrRemDB(nameBASE, codeBASE, RECNAME(el), DefGrid, codeRETURN)
    ENDIF
    IF (infoGrid) CALL array_def(DefGrid )

    !    ----------infoRecord---------------
    !if (infoRecord .and. .NOT. statBASE )THEN 
    IF ( cycleBASE )  CALL GetcRemDB(nameBASE, codeBASE, RECNAME(el), DefRec, codeRETURN)
    !ELSEIF (infoRecord .and. .NOT. statBASE )THEN
    !IF (LP > 5 )  WRITE (6,*) 'attempt read info from statBASE base'
    IF ( statBASE )  CALL GetqRemDB(nameBASE, codeBASE, RECNAME(el), DefRec, codeRETURN)
    IF (LP > 5 )  WRITE (6,*) 'codeRETURN from infoRecord = ', codeRETURN, RECNAME(el)
    IF (infoRecord)  CALL array_def(DefRec(1:60) )
    !WRITE (6,'(60i8)') DefRec(1:60)

    df(el,:)%NX = DefRec(17)
    IF(     cycleBASE )THEN
        df(el,:)%NY = DefRec(18)
    ELSEIF ( statBASE )THEN
        df(el,:)%NY = DefRec(36)
    ENDIF

  ENDDO  LoopRecord                                ! cycle RECORD

! ----------------Reading dictionary of station-------------------

  SELECT CASE (RECNAME(1))
  CASE( 'SYNOPMAK','SYNOPDOP','TEMPMAKT','TEBDTMAK','TEBDWMAK','TEMPALLC' )
    IF( HOST=='local' ) THEN
      CALL RDSLOV('SHOT',260601_8,'SLOVPUR3',dictionarySt,dictionaryStName,nST,codeRETURN)
      IF ( LP >2 ) WRITE(*,*) ' codeRETURN from RDSLOV =',codeRETURN, nST
    ELSE
      IF( nameBASE /= 'SHOT' ) CALL OPENRemDB(codeOPEN, 'SHOT', 1_4, codeRETURN)
      CALL RDFQremdb('SHOT',260601_4,'SLOVPUR3',dictionaryStation,codeRETURN)
      IF (LP >3 ) WRITE(*,*) ' codeRETURN from RDFQ =',codeRETURN
      nST = COUNT(dictionaryStation(1,:)>0)
      IF (LP >3 ) WRITE(*,*) 'number stations in dictionary SLOVPUR3',nST
      dictionarySt(1:6,:) = dictionaryStation(1:6,:)
!     dictionaryStName(1:2,:) = dictionaryStation(7:8,:)
    ENDIF

    CALL HANDLE_ERROR(codeRETURN,'error calling RDSLOV/RDFQ ','message')

  ENDSELECT

  LoopRecord2:  DO el=1,NEL        ! record cycle

    LoopTime:  DO t=1,NT       ! time cycle

      NX = df(el,t)%NX
      NY = df(el,t)%NY

      df(el,t)%year     = year(t)
      df(el,t)%month    = month(t)
      df(el,t)%day      = day(t)
      df(el,t)%minute   = minute(t)
      df(el,t)%second   = 0
      IF( RECNAME(el) == 'SYNOPDOP' )THEN
        df(el,t)%hour = hour(t)+3
      ELSE
        df(el,t)%hour = hour(t)
      ENDIF

      WRITE(char10,'(i4.4, 3(i2.2))') df(el,t)%year, df(el,t)%month, df(el,t)%day, df(el,t)%hour
      df(el,t)%charDATEHH = char10

      NumbSt=NY
      ALLOCATE( value_field(NX,NY,NT), value_field2(NX,NY,NT), value_field3(NX,NY,NT) )
      value_field = oundef

      ALLOCATE( iArray(NX,NY,NEL), ListSt(NumbSt), idST(NumbSt) )
      IF(LP > 2)WRITE(*,"('read record',a10,i10,i3.2,' (',i5,',',i4,')')")RECNAME(el), Date(t), hour(t), NY, NX

      !--------------------------------------------------------------------
      !                      READING SYNOP-DATA
      !                    --------------------------

      IF( rMaket .and. cycleBASE ) THEN
        CALL ReadMaket(nameBASE, codeBASE, Date(t), RECNAME(el), hour(t), NumbSt, ListSt, iArray, codeRETURN)
        !CALL ReadSynop(nameBASE, codeBASE, Date(t), RECNAME(el), hour, NumbSt, ListSt, iArray, codeRETURN)
      ELSEIF( rMaket .and. statBASE ) THEN
        CALL ReadMaketRows(nameBASE, codeBASE, RECNAME(el), year(t), month(t), Day(t), hour(t), minute(t), NumbSt, ListSt, iArray,  codeRETURN)
        !CALL RdSynopRows(nameBASE, codeBASE, RECNAME(el), Year, Month, Day, hour, minute, NumbSt, ListSt, iArray,  codeRETURN)
      ENDIF

      !--------------------------------------------------------------------
      !                      READING 2D-FIELDS
      !                    --------------------------

      IF( HOST=='local' )THEN
        CALL rdfc(nameBASE, codeBASE, Date(t), RECNAME(el), hour(t), value_field(1:NX,1:NY,t), codeRETURN)
      ELSE
        IF     (readFIELD .and. cycleBASE)  THEN
          CALL rdfcRemDB(nameBASE, codeBASE, Date(t), RECNAME(el), hour(t), value_field(1:NX,1:NY,t), codeRETURN)
        ELSEIF (readFIELD2 .and. statBASE)  THEN
          CALL rdfqRemDB(nameBASE, codeBASE, Date(t), RECNAME(el), hour(t), value_field(1:NX,1:NY,t), codeRETURN)
        ELSEIF (readFIELD2 .and. cycleBASE) THEN
          CALL RdfcrRemDB(nameBASE, codeBASE, RECNAME(el), year(t), month(t), Day(t), hour(t), minute(t), value_field(1:NX,1:NY,t), codeRETURN)
        ELSEIF (readFIELD .and. statBASE) THEN
          CALL RdfqrRemDB(nameBASE, codeBASE, RECNAME(el), year(t), month(t), Day(t), hour(t), minute(t), value_field(1:NX,1:NY,t), codeRETURN)
        ENDIF
      ENDIF

      IF( codeRETURN/=0 )THEN
        IF (LP >5 ) WRITE(*,*) RECNAME(el),Date(t),hour(t),' codeRETURN= ',codeRETURN
      ENDIF
      !WRITE(FileName,'(i8,i2.2,a4)') Date, hour,RECNAME;     WRITE(*,*) 'numberElements = ',NEL
      IF (LP >9 )  WRITE(*,*) RECNAME(el),Date(t),hour(t),'MINvalue1,2= ',minval(value_field(:,:,t)),minval(value_field(:,:,t), MASK=value_field(:,:,t)>iundef),'MAXvalue= ',maxval(value_field(:,:,t))

      value_field2 = value_field

      IF (LP > 3)  WRITE (*,*) '  before handle  value_field ', t, df(el,t)%NY
      ! handle reading array from base
      CALL HANDLE()

      IF (LP > 3)  WRITE (*,*) '  after handle  value_field ', t, df(el,t)%NY
      !var(el,t)=outfield(value_field2(1:NX,1:NY,t))
      var(el,t)=outfield(value_field3(1:NX,1:NY,t))

      DEALLOCATE(value_field, value_field2, value_field3, idST, ListSt, iArray )
      IF (LP > 3)  WRITE (*,*) '  after deallocation  value_field '

    ENDDO  LoopTime     ! cycle  time

    !WRITE (*,*) idST(1:NY)
    IF (LP > 5 )  WRITE (*,*) '==',df(el,1)%idST(-1:7),'=='
    !WRITE (*,*) value_field(3,1,1)

  ENDDO  LoopRecord2  ! cycle  RECORD

  
! CLOSE connection with host
  IF( HOST=='local' )THEN
    CALL closb       (HOST, codeRETURN)
  ELSE
    CALL CLOSERemHost(HOST, codeRETURN)
  ENDIF


  IF (LP > 5) WRITE(10001,'(46f8.1)')  ((var(1,1)%p2(i,j),i=1,46),j=1,df(1,1)%NY) ! for debug synop maket
  IF (LP > 4) WRITE(*, * ) 'ubound(var(1,1)%p2) = ', ubound(var(1,1)%p2)  ! bounds array


!====================================================================================================
!->>--------------------------  OUTPUT  ----------------------------

!!IF(.NOT.readFIELD)

  DO el=1,NEL
    IF( RECNAME(el)=='SYNOPDOP' ) RECNAME(el) = 'SYNOPMAK'
  ENDDO

  SELECT CASE (trim(typefile))
  CASE( 'text' )  ! TERMINAL  and  TEXT   ! simple text field
    DO t=1,NT; DO el=1,NEL

      IF( incrYdir == -1 )THEN
        oNXstart=1
        oNXend=df(el,t)%NX
        oNYstart=df(el,t)%NY
        oNYend=1
      ELSE
        oNXstart=1
        oNXend=df(el,t)%NX
        oNYstart=1
        oNYend=df(el,t)%NY
      ENDIF

      OPEN (ounit,file=trim(filename), position='append')
      WRITE(ounit,*) 'DATE=',Date(t), hour(t), 'zab=',startTerm,'format=',fmOU

      WRITE(ounit,'(10E15.6)') (( var(el,t)%p2(i,j),i=oNXstart,oNXend),      &
                                                    j=oNYstart,oNYend,incrYdir)
      CLOSE(ounit)

    ENDDO;ENDDO  ! cycle  RECORD ; cycle  time

  CASE( 'maket' )   ! list off all station

    DO t=1,NT; DO el=1,NEL
      NY = df(el,t)%NY

      IF (FileName=='DATE_RECNAME')THEN
        oFileName = df(el,t)%charDATEHH//'_'//RECNAME(el)
      ENDIF
      OPEN (ounit,file=trim(ofilename), position='append')
      !WRITE(ounit,*) 'DATE=',Date(t), hour(t), 'zab=',startTerm;

      IF (RECNAME(el)=='SYNOPDOP' .or. RECNAME(el)=='SYNOPMAK') THEN
       !WRITE(ounit,'(45(a,"    "))') (trim(df(el,t)%header(i)),i=2,45)
        WRITE(ounit,2210) (trim(df(el,t)%header(i)),i=2,46)
        WRITE(ounit,2211)               ( &
         ( INT(var(el,t)%p2(1,j)), INT(var(el,t)%p2(2,j)),      & ! index
           (var(el,t)%p2(3:4,j)), INT(var(el,t)%p2(5,j)),       & ! lon,lat,h
           INT(var(el,t)%p2(6:9,j)),                            & !
           var(el,t)%p2(10:11,j),                               & ! 
           INT(var(el,t)%p2(12,j)), INT(var(el,t)%p2(13,j)),    &
           INT(var(el,t)%p2(14,j)),  var(el,t)%p2(15,j),        &
           (var(el,t)%p2(16:17,j)),      &
           INT(var(el,t)%p2(18:25,j)),  &
           (var(el,t)%p2(26:32,j)),     &
           INT(var(el,t)%p2(33:40,j)),  &
           (var(el,t)%p2(41:46,j)) ), j=1,NY )
        !WRITE(ounit,'(i2)') ( (int(var(el,t)%p2(1,j))),j=1,NY)

      2210 FORMAT(( a5,a8,a9,a7,a4,3a6,2a6,35(x,a6) ))
      2211 FORMAT(( i2,i3.3, xf7.3,xf8.3, xi6, xi3,xi5,xi5,xi5, xf5.1,xf5.1, &
                    2(xi6), xi6, 3(xf6.1), 8(xi6), 7(xf6.1), 8(xi6), 6(xf6.1) ))

      ELSEIF( RECNAME(el)=='TEMPMAKT' ) THEN
        ! WRITE(ounit,'(100(a,"    "))') df(el,t)%header(2:96)
        WRITE(ounit,'(a6, a4, a7,a7,a8, 2(a6), 5(xa6),75(xa5), 5(xa6), 42(xa6), 8(xa5))')  & 
                     (trim(df(el,t)%header(i)),i=2,8),   &
                     (trim(df(el,t)%header(i)),i=11,90),  &
                     (trim(df(el,t)%header(i)),i=96,142)
        WRITE(ounit, 2003 ) &
             (( INT(var(el,t)%p2(1,j)), INT(var(el,t)%p2(2,j)),              &
                INT(var(el,t)%p2(3,j)),    &
                INT(var(el,t)%p2(4,j)), (var(el,t)%p2(5:6,j)),              &
                INT(var(el,t)%p2(7:8,j)),        &  ! var(el,t)%p2(9:10,j), - reserved fields
                ! var(el,t)%p2(11:90,j),           &
                ((INT(var(el,t)%p2(jj,j)),(var(el,t)%p2(jj+1,j)),(var(el,t)%p2(jj+2,j)),INT(var(el,t)%p2(jj+3,j)),INT(var(el,t)%p2(jj+4,j)) ),jj=11,90,5),           &
                var(el,t)%p2(96:100,j),          &
                (var(el,t)%p2(101:142,j)),       &  ! var(el,t)%p2(143:146,j), - reserved fields
                INT((var(el,t)%p2(147:150,j))),  &
                INT(var(el,t)%p2(91:95,j))      ),j=1,NY )

       2003 FORMAT( (i3,i3.3, xi3, xi6,xf6.2,xf7.2, 2(xi5.4), &
                     1(xi6,2(xf6.1),2(xi6)), 15(xi5,2(xf5.1),2(xi5)),  &
                     1(xi6,2(xf6.1),2(xi6)), &
                     6(xi6,xf6.1,2(xi6)), &
                     6(xi6,2(xi6)), &
                     ! 5(xf8.1), 42(xf8.1), 4a2, 5i6) )
                     x4a2, 5(xi4)) )
                     ! 80(xf8.1), 5(xf8.1), 42(xf8.1), 4a2, 5i6) )

      ELSEIF( RECNAME(el)=='TEBDTMAK' ) THEN
        WRITE(ounit,'("  index sign    height  lat      lon      n         NT     jk    reserv    ")')
        WRITE(ounit,'(i7, i3, 7f8.0,/,"Pres  ",97f7.0:,/,"TEMP  ",97f6.2,/,"T_DP  ",97f6.2)')  &
         ( ( int(var(el,t)%p2(1,j)*1000+var(el,t)%p2(2,j)), int(var(el,t)%p2(3,j)),  var(el,t)%p2(4:10,j), &
            (var(el,t)%p2(i,j),i=11,301,3),                   &
            (var(el,t)%p2(i,j),i=12,301,3),                   &
            (var(el,t)%p2(i,j),i=13,301,3)),  j=1,NY )

      ELSEIF( RECNAME(el)=='TEBDWMAK' ) THEN
        WRITE(ounit,'("  index sign    height  lat      lon      n         NT     jk    reserv    ")')
        WRITE(ounit,'(i7, i3, 7f8.0,/,"Pres  ",97f7.0,/,"Wdir  ",97f5.0,/,"Wspeed",97f5.0)')  &
         ( ( int(var(el,t)%p2(1,j)*1000+var(el,t)%p2(2,j)), int(var(el,t)%p2(3,j)),  var(el,t)%p2(4:10,j), &
            (var(el,t)%p2(i,j),i=11,301,3),                   &
            (var(el,t)%p2(i,j),i=12,301,3),                   &
            (var(el,t)%p2(i,j),i=13,301,3)),  j=1,NY )

      ELSEIF( RECNAME(el)=='AIREPMAK' ) THEN
        WRITE(ounit,'(12(a,"  "))') df(el,t)%header(2), df(el,t)%header(4:13),  df(el,t)%header(17)
        WRITE(ounit,'(i6, i3, 2f10.2, 2i6.4, 4i8, f10.2, " ",4a2)')   &
          (( int(var(el,t)%p2(2,j)), int(var(el,t)%p2(4,j)),          &
             var(el,t)%p2(5:6,j),    int(var(el,t)%p2(7:8,j)),        &
             int(var(el,t)%p2(9:12,j)),   var(el,t)%p2(13,j),         &
             int(var(el,t)%p2(17:20,j)) ),j=1,NY )  

      ELSEIF (RECNAME(el)=='SHIPBMAK') THEN
        WRITE(ounit,'(27(a,"    "))')  df(el,t)%header(2:5), df(el,t)%header(9:26), df(el,t)%header(42:46)
        WRITE(ounit,'(i5, 2f8.2, " ", 4a2, 3f8.2, 2i5, 13f8.1, 4f8.1, i4 )')   &
        (( int(var(el,t)%p2(2,j)), (var(el,t)%p2(3:4,j)),    & 
           int(var(el,t)%p2(5:8,j)),                         &
           var(el,t)%p2(9:11,j), int(var(el,t)%p2(12:25,j)), &
           var(el,t)%p2(26,j), (var(el,t)%p2(42:45,j)),      &
           int(var(el,t)%p2(46,j)) ) ,j=1,NY )

      ENDIF

      CLOSE(ounit)

    ENDDO;ENDDO  ! cycle  RECORD ; cycle  time

  CASE( 'csv' )
    WRITE(*,"('Output in ',a12,'-file: ',a)") trim(typefile), trim(FileName)

    IF (LP > 5 ) WRITE (*,*) 'typefile == csv', listPoints
    DO t=1,NT; DO el=1,NEL
      WRITE(*,"('Writing record ',a)") df(el,t)%RECNAME
      NY=df(el,t)%NY

      WRITE(strDATE,"(i4.4,'-',i2.2,'-',i2.2,x,i2.2,':00')") df(el,t)%year, df(el,t)%month, df(el,t)%day, df(el,t)%hour

      j=1;jj=1
      DO WHILE ( listPoints(jj)>iundef .and. j<=NY  )
        IF (LP > 5 ) WRITE (*,*) 'seach stantion',jj,j, listPoints(jj)
        IF(  int(var(el,t)%p2(1,j)*1000+var(el,t)%p2(2,j),4)==int(listPoints(jj),4)  )THEN
          IF (LP > 5 ) WRITE (*,*) 'FIND stantion !!!',jj,j
          IF (FileName=='ID')  WRITE(oFileName,'(i5.5)') listPoints(jj)
          OPEN (ounit,file=trim(ofilename)//'.csv', position='append')

          SELECT CASE( df(el,t)%RECNAME )
          CASE( 'SYNOPMAK', 'SYNOPDOP' )
            IF(t==1 .and. el==1) WRITE(ounit,'(a16,45(a,";"))')'DATE;', df(el,t)%header(2:46)
            WRITE(ounit,6401)  &
              strDATE, int(var(el,t)%p2(1,j)), int(var(el,t)%p2(2,j)), &
              var(el,t)%p2(3:4,j), ( int(var(el,t)%p2(i,j)) ,i=5,7),   &
              var(el,t)%p2(8:9,j), (var(el,t)%p2(10:46,j))

          CASE('TEMPMAKT')
            WRITE(ounit,6002) strDATE, int(var(el,t)%p2(1,j)),int(var(el,t)%p2(2,j)), var(el,t)%p2(5:6,j), int(var(el,t)%p2(4,j),4)
            WRITE(ounit,*)'Pressure;','H;','T;','D_TD;','WDir;','WSpeed;'
            WRITE(ounit,6001) trim(df(el,t)%clevel(96)), int(var(el,t)%p2(96,j)), var(el,t)%p2(97,j), var(el,t)%p2(98,j),int(var(el,t)%p2(99,j)),var(el,t)%p2(100,j)
            DO l=11,86,5
              WRITE(ounit,6001) trim(df(el,t)%clevel(l)), int(var(el,t)%p2(l,j)), var(el,t)%p2(l+1,j), var(el,t)%p2(l+2,j),int(var(el,t)%p2(l+3,j)),var(el,t)%p2(l+4,j)
            ENDDO
            WRITE(ounit,*) int(var(el,t)%p2(93,j)), int(var(el,t)%p2(94,j)), int(var(el,t)%p2(95,j))
            DO l=101,121,4
              WRITE(ounit,5001) trim(df(el,t)%clevel(l)), var(el,t)%p2(l,j), var(el,t)%p2(l+1,j), int(var(el,t)%p2(l+2,j)), var(el,t)%p2(l+3,j)
            ENDDO
            DO l=125,140,3
              WRITE(ounit,4001) trim(df(el,t)%clevel(l)), var(el,t)%p2(l,j), int(var(el,t)%p2(l+1,j)), var(el,t)%p2(l+2,j)
            ENDDO

          CASE('TEMPALLC')
            WRITE(ounit,6002) strDATE, int(var(el,t)%p2(1,j)),int(var(el,t)%p2(2,j)), var(el,t)%p2(6:7,j), int(var(el,t)%p2(5,j),4)
            WRITE(ounit,*) 'Pressure;','H;','T;','D_TD;','WDir;','WSpeed;','lev;','cP;','cH;','cT;','cTd;','cDD;','cFF;'
            DO l=14,(var(el,t)%p2(11,j)+1)*13,13  !
              WRITE(ounit,3101)     &
                var(el,t)%p2(l+1,j), int(var(el,t)%p2(l+3,j)), var(el,t)%p2(l+5,j), var(el,t)%p2(l+7,j), int(var(el,t)%p2(l+9,j)), int(var(el,t)%p2(l+11,j),4), int(var(el,t)%p2(l,j),4),        &
                ( int(var(el,t)%p2(ll,j),4), ll=l+2,l+12,2)    !int(var(el,t)%p2(l+4,j),4), int(var(el,t)%p2(l+6,j),4), int(var(el,t)%p2(l+8,j),4), int(var(el,t)%p2(l+10,j),4), int(var(el,t)%p2(l+12,j),4)
            ENDDO
            WRITE(ounit,*)

          ENDSELECT
            IF (LP > 5 ) WRITE (*,*) 'bingooooooo', listPoints(jj)
          jj=jj+1;j=0;
          CLOSE(ounit)
        ELSEIF( j==NY )THEN
          j=0
          jj=jj+1
          !if (LP > 5 ) WRITE (*,*) 'booo', listPoints(jj),jj,j
        ENDIF
        j=j+1
      ENDDO

    ENDDO;ENDDO  ! cycle  RECORD ; cycle  time

 4001 FORMAT(a10, ';', f10.2, ';;;;;', i6, ';', f10.2, ';')
 5001 FORMAT(a10, ';', f10.2, ';', f10.2, ';;;', i6, ';', f10.2, ';')
 6001 FORMAT(a10, ';', i6, ';', f10.2, ';', f10.2, ';', i6, ';', f10.2, ';')
 6002 FORMAT(a16, ';', i4, i3.3, ';', 2(f8.2,';'), i6, ';')
 3101 FORMAT(f10.2, ';', i8, ';', f10.2, ';', f10.2, ';', i6, ';', i6, ';', i3, ';', 6(i3, ';') )
 7001 FORMAT(a16, ';', i4, i3.3, ';', 2(f8.2,';'), f8.2, ';', i4, ';')
 6401 FORMAT(a16:, ';', i4, i3.3, ';', 2(f8.2,';'), i6, ';', i3, ';', i6, ';', 2(f8.2,';'), 37(f8.2,';'))
   
  CASE( 'stGrads' )

    DO t=1,NT; DO el=1,NEL
      IF (FileName=='DATE_RECNAME')  oFileName=df(el,t)%charDATEHH//'_'//RECNAME(el)
      OPEN (11,file=trim(oDirName)//trim(oFileName),FORM='UNFORMATTED',  RECORDTYPE='STREAM')
      NY=df(el,t)%NY
      DO j=1,NY          ! Cycle for each station
        IF( j<10  .and. LP > 5 ) WRITE (*,*)'==',df(el,t)%idST(j),'==', var(el,t)%p2(3,j),var(el,t)%p2(4,j)
        WRITE (11) df(el,t)%idST(j), var(el,t)%p2(3,j), var(el,t)%p2(4,j), 0.0_4, NLEV, NFLAG
        WRITE (11) ( var(el,t)%p2(1:df(el,t)%NX,j) )
      ENDDO

      WRITE (11) df(el,t)%idST(NY), var(el,t)%p2(3,NY), var(el,t)%p2(4,NY), 0.0_4, int(0,4), NFLAG  ! final marker
      CLOSE (11)    ! CLOSE file with list stations
!     if (LP >14 ) WRITE (*,* )idST(3), value_field(1,3,el,t), value_field(2,3,el,t)
    ENDDO; ENDDO  ! cycle  RECORD ; cycle  time

#ifdef NETCDF
  CASE( 'netCDF4COSMO' )
!   amountPt=0

    CALL init_name_vars_netCDF()

    loopT: DO t=1,NT; loopEL: DO el=1,NEL
        WRITE(*,*) '== el =',nel,'t =',nt

        IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
          CALL init_CDF_array( 1, df(el,t)%NY, 'synop'  )
!         CALL init_CDF_array( 1+amountPt,  df(el,t)%NY + amountPt, 'synop'  )
!         amountPt=amountPt+df(el,t)%NY

        ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' )THEN

          CALL init_CDF_array( 1,  df(el,t)%NYY(1), 'pilot_p' )
          CALL init_CDF_array( 1+df(el,t)%NYY(1),  df(el,t)%NYY(2), 'tempship' )
!         if( df(el+1,t)%RECNAME=='TEBDTMAK'  .and. el<NEL ) wt=1
          CALL init_CDF_array( 1+df(el,t)%NYY(1)+df(el,t)%NYY(2),  df(el,t)%NYY(3), 'temp' ) ! or NY

        ELSEIF( df(el,t)%RECNAME=='TEBDTMAK' )THEN
          CALL init_CDF_array( 1,  df(el,t)%NY, 'temp' )

        ELSEIF( df(el,t)%RECNAME=='TEBDWMAK' )THEN
          CALL init_CDF_array( 1,  df(el,t)%NY, 'temp' )

        ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
          CALL init_CDF_array( 1,  df(el,t)%NYY(1), 'ship' )
          CALL init_CDF_array( 1+df(el,t)%NYY(1),  df(el,t)%NYY(2), 'buoy' )

        ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
          CALL init_CDF_array( 1,  df(el,t)%NY, 'amdar' )

        ENDIF

      ENDDO loopEL
    ENDDO loopT 
#endif

  CASE( 'binary4GRADS' )
    IF (LP > 1 ) WRITE (*,*) 'WRITE FILE in ',trim(typefile)

    ncount=1
    NX=df(1,1)%NX
    NY=df(1,1)%NY
    IF( incrYdir == -1 )THEN
      oNXstart=1
      oNXend=NX
      oNYstart=NY
      oNYend=1
    ELSE
      oNXstart=1
      oNXend=NX
      oNYstart=1
      oNYend=NY
    ENDIF

    OPEN (ounit,file=trim(filename), FORM='UNFORMATTED', ACCESS='DIRECT', RECL=NX*NY)

    nzab=NEL/NEL0

    DO t=1,NT
      DO z=0,nzab-1
        DO el=1,NEL,nzab
          IF (LP > 2 ) WRITE(*,"('WRITE record',5i4,3x,a)"),ncount, t, el, z, NEL0, df(el+z,t)%RECNAME
          WRITE(ounit,rec=ncount) (( var(el+z,t)%p2(i,j),i=oNXstart,oNXend),j=oNYstart, oNYend, incrYdir )
    !!!!!!WRITE(ounit,rec=ncount) (( var(el,t)%p2(i,j),i=oNXstart,oNXend),j=oNYstart, oNYend, incrYdir )
          ncount=ncount+1
        ENDDO             ! cycle  RECORD
      ENDDO               ! cycle  lead-time
    ENDDO                 ! cycle  time

    CLOSE(ounit)

  CASE( 'list_station')
  
    DO t=1,NT; DO el=1,NEL
        NY=df(el,t)%NY

        IF (FileName=='DATE_RECNAME')  oFileName=df(el,t)%charDATEHH//'_'//RECNAME(el)
        OPEN (ounit,file=trim(ofilename), position='append')
        WRITE(ounit,'(a30)') '  index     lat     lon  height'
        IF ( RECNAME(el)=='SYNOPMAK' .or. RECNAME(el)=='SYNOPDOP')THEN
            WRITE(ounit,'(i7, 3f12.2)')  &
              (( int(var(el,t)%p2(1,j)*1000+var(el,t)%p2(2,j)), &
              (var(el,t)%p2(3:5,j))   ),j=1,NY )
        ELSEIF (RECNAME(el)=='TEMPMAKT'  &
               .or. RECNAME(el)=='TEBDTMAK' .or. RECNAME(el)=='TEBDWMAK') THEN
            WRITE(ounit,'(i7, 3f12.2)')  &
              (( int(var(el,t)%p2(1,j)*1000+var(el,t)%p2(2,j)), &
              (var(el,t)%p2(5:6,j)),(var(el,t)%p2(4,j))   ),j=1,NY )
        ENDIF
        CLOSE(ounit)

    ENDDO; ENDDO  ! cycle  RECORD ; cycle  time

    OPEN (ounit,file='SLOVPUR3.txt', status='unknown')
    WRITE(ounit,'(i10, 2f10.3, i10, 4x, 2a8)')           &
         (( dictionarySt(1,j),                     &
            dictionarySt(2:3,j)*0.001,             &
            dictionarySt(4,j),                     &
            dictionaryStName(1:2,j)  ),j=1,nSt )
    CLOSE(ounit)

#ifdef BUFR
  CASE( 'bufr4versus')
    IF(LP > 2) WRITE(*,"('write file in format',a12)") trim(typefile)

    DO t=1,NT
      DO el=1,NEL
        oFileName="synop"//df(el,t)%charDATEHH//".bufr"

        IF(LP > 2)WRITE(*,"('writing record ',a,i4,' in file ',a)") df(el,t)%RECNAME, t, oFileName

        CALL encode_bufr(trim(oFileName))

      ENDDO
    ENDDO
#endif

  CASE( 'kml','KML')
    IF(LP > 2) WRITE(*,"('write file in format',a12)") trim(typefile)

    DO t=1,NT
      DO el=1,NEL
        oFileName=df(el,t)%RECNAME//df(el,t)%charDATEHH//".kml"
        OPEN (ounit,file=trim(oFileName), position='append')

        IF(LP > 2)WRITE(*,"('writing record ',a,i4,' in file ',a)") df(el,t)%RECNAME, t, oFileName

        WRITE(ounit, * ) '<?xml version="1.0" encoding="UTF-8"?>'
        WRITE(ounit, * ) '<kml xmlns="http://www.opengis.net/kml/2.2">'
        WRITE(ounit, * ) '</Document>'
        WRITE(ounit, * ) '</kml>'

        CLOSE(ounit)
      ENDDO
    ENDDO

  ENDSELECT

!<<-------------------------------------------------------------------

  DEALLOCATE( df, var )
  IF (LP > 3 )  WRITE (*,*) '  after deallocation  df, var '

!====================================================================================================

CONTAINS

INCLUDE "handle_maket.f90"

#ifdef NETCDF
INCLUDE "init_netCDF.f90"
#endif

#ifdef BUFR
INCLUDE "init_BUFR.f90"
#endif

!SUBROUTINE read_arguments()
!ENDSUBROUTINE read_arguments

! SUBROUTINE HANDLE_ERROR(ierr,errmess,act)
  ! INTEGER, INTENT(IN)          :: ierr
  ! CHARACTER(LEN=*), INTENT(IN) :: errmess, act
  ! IF( ierr==0 ) RETURN
  ! SELECT CASE( trim(act) )
  ! CASE('attention','message')
    ! WRITE(*,*) errmess, ierr
  ! CASE('stop')
    ! WRITE(*,*) errmess, ierr
    ! STOP
  ! ENDSELECT
! ENDSUBROUTINE HANDLE_ERROR

ENDPROGRAM ReadBASE
!==================================================================================================================


