SUBROUTINE init_CDF_array ( i1, ni, Observation )
!  i1 - start index
!  ni - size array or section - count stations
!  i2 - end index

  INTEGER, INTENT(IN) :: i1, ni
  CHARACTER(len = *), INTENT(IN) ::    Observation
  CHARACTER*30  ::  oFileName
  CHARACTER*256 ::  oDirName
  CHARACTER(len = 2)  ::    suf=''
  INTEGER   i2, i, j, jj, numberfile
  LOGICAL  ::       exist_file

  i2=i1-1+ni
  IF (LP > 3 ) WRITE (*,*)  'obs=', trim(Observation), i1,i2, ni

  ! Инициализировать список переменных для каждого вида наблюдений
  CALL INIT_nDims(Observation)

  ! инициализировать количество вертикальных уровней, точек
  CALL INIT_nPt(ni)

  !-----------------------------------

  ALLOCATE( array_i(nvar_i,dim(2)%nPt, 1:ni) )
  array_i=oundef

  !allocate( array_f(nvar_f,dim(2)%nPt,dim(1)%nPt) )
  ALLOCATE( array_f(nvar_f,dim(2)%nPt, 1:ni) )
  array_f=oundef

  ALLOCATE (array_c9(dim(1)%nPt), array_c8(dim(1)%nPt) )
  !array_c='.'
  
!-----------------------------------

  CALL INIT_code_data(Observation)

  CALL INIT_date_time(i1,i2)

  CALL INIT_wmo_index(i1,i2,Observation)

  CALL INIT_meteo_fields(i1,ni,Observation)

  IF (LP > 4 ) WRITE (*,*) 'dims:', dim(1:3)%name

  oFileName = "cdfin_"//trim(Observation)
!  Проверка на существование записываемых файлов
!  Если они уже сужествют, то программа будет добавлять к ним индексы.
  numberfile=1; exist_file=.TRUE.; suf=''
  DO WHILE ( exist_file .and. numberfile<10 )
    IF(numberfile>1) WRITE(suf,'(a1,i1.1)') '.',numberfile
    INQUIRE( FILE=trim(oFileName)//suf, EXIST = exist_file )
    numberfile=numberfile+1
    IF (LP > 4 ) WRITE (*,*)  'oF=',trim(oFileName),numberfile
  ENDDO
  
  CALL WRITE_netCDF4COSMO( trim(oFileName)//suf, LP )

  IF(LP>3 ) WRITE (*,*) 'before deallocate array_(i,f,c9,c8)'
  DEALLOCATE ( array_i )
  DEALLOCATE ( array_f )
  DEALLOCATE ( array_c9, array_c8 )
  IF(LP>3 ) WRITE (*,*) 'after deallocate array_(i,f,c9,c8)'

ENDSUBROUTINE init_CDF_array

SUBROUTINE INIT_nDims(Observation)
  CHARACTER(len = *), INTENT(IN) ::    Observation

  IF (LP > 4 ) WRITE (*,*) 'start RT INIT_nDims()'

  var_i(:)%nDims=0
  var_f(:)%nDims=0
  var_c(:)%nDims=0

    IF( df(el,t)%RECNAME=='SYNOPMAK' .or.    &
        df(el,t)%RECNAME=='SYNOPDOP'     )THEN
      var_i( 1:8 )%nDims=1
      var_i( 9:32)%nDims=1
      var_i(  33 )%nDims=2
      var_i(34:38)%nDims=1
      var_i(  40 )%nDims=1

      var_f( 1:12)%nDims=1
      var_f(  13 )%nDims=2
      var_f(14:19)%nDims=1
      var_f(  22 )%nDims=1

    ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
      var_i( 1:8 )%nDims=1
      var_i(11:21)%nDims=1

      var_f( 1:10)%nDims=1
      var_f(    7)%nDims=0
!!    var_f(  13 )%nDims=2
      var_f(  14 )%nDims=1
!!    var_f(  22 )%nDims=1
      
      var_c(   1 )%nDims=1

    ELSEIF( Observation == 'ship' )THEN ! SHIP
      var_i( 1:8 )%nDims=1
      var_i(11:32)%nDims=1
     !var_i(34:36)%nDims=1

      var_f( 3:12)%nDims=1
      var_f(  14 )%nDims=1
      var_f(16:19)%nDims=1
      var_f(23:24)%nDims=1

      var_c(   1 )%nDims=1

    ELSEIF( Observation == 'buoy' )THEN ! BUOY
      var_i( 1:8 )%nDims=1
      var_i(11:17)%nDims=1
      var_i(  50 )%nDims=1
      var_i(55:59)%nDims=1

      var_f( 1:4 )%nDims=1
      var_f( 5:10)%nDims=1
      var_f(  14 )%nDims=1

    ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN ! AIREP+AMDAR
      var_i( 1:8 )%nDims=1
      var_i(12:17)%nDims=1
      var_i(51:53)%nDims=1

      var_f( 1:2 )%nDims=1
      var_f( 8:10)%nDims=1

      var_c(   3 )%nDims=1

    ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' .or.    &
            df(el,t)%RECNAME=='TEBDTMAK' .or.    &
            df(el,t)%RECNAME=='TEBDWMAK' )THEN
      var_i( 1:8 )%nDims=1
      var_i( 9:10)%nDims=1
      var_i(12:16)%nDims=1
      var_i(  17 )%nDims=2
      var_i(  21 )%nDims=1
     !var_i(23:27)%nDims=1    ! clouds
      var_i(  40 )%nDims=2    ! Geopotential
      var_i(41:44)%nDims=1
      var_i(  47 )%nDims=1    ! MEDRE
     !var_i(  48 )%nDims=2
      var_i(  49 )%nDims=2    ! MEVSS

      var_f( 1:4 )%nDims=1
      var_f( 8:10)%nDims=2
     !var_f(  16 )%nDims=1    ! clouds
      var_f(  19 )%nDims=2
     !var_f(20:21)%nDims=2

      IF( trim(Observation)=='pilot_p' )THEN
        var_f( 8:9)%nDims=0
      ENDIF

    ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
      var_i( 1:8 )%nDims=1
      var_i( 9:10)%nDims=1
      var_i(12:16)%nDims=1
      var_i(  28 )%nDims=1
      var_i(  45 )%nDims=2
      var_i(  54 )%nDims=1

      var_f(23:24)%nDims=1
      var_f(  25 )%nDims=2

    ENDIF

  IF (LP > 4 ) WRITE (*,*) 'end RT INIT_nDims()'

ENDSUBROUTINE INIT_nDims

SUBROUTINE INIT_nPt(ni)
  INTEGER, INTENT(IN) ::  ni

  IF (LP > 4 ) WRITE (*,*) 'start RT INIT_nPt()'

  dim(1)%nPt=ni ! количество пунктов измерений
  dim(3)%nPt=9  ! количество букв в названии
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or.    &
      df(el,t)%RECNAME=='SYNOPDOP' .or.    &
      df(el,t)%RECNAME=='AMS' )THEN
    dim(2)%nPt=2
  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    dim(2)%nPt=21
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
    dim(2)%nPt=2
  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    dim(2)%nPt=1
    dim(3)%nPt=8
  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    dim(2)%nPt=17  ! 1 surface and 16 in air
  ELSEIF( df(el,t)%RECNAME=='TEBDWMAK' .or. df(el,t)%RECNAME=='TEBDTMAK' )THEN
    dim(2)%nPt=100
  ENDIF

  IF (LP > 4 ) WRITE (*,*) 'end RT INIT_nPt()'

ENDSUBROUTINE INIT_nPt

SUBROUTINE INIT_code_data(Observation)
  CHARACTER(len = *), INTENT(IN) ::    Observation

  IF (LP > 4 ) WRITE (*,*) 'start RT INIT_code_data()'

! header - section 1 - main for all type observation
  array_i( 1,1,:)=4        ! code of table of BUFR
  array_i( 2,1,:)=4        ! Moscow
  array_i( 3,1,:)=0        ! subcenter
  array_i( 4,1,:)=0        ! upd. seq. number (station correction)

! category and subcategory data BUFR 4 section 1
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_i( 5,1,:)=0    ! category of data     src_obs_proc_cdf.f90(4.21):2658
    array_i( 6,1,:)=2    ! subcategory of data  src_obs_proc_cdf.f90(4.21):2660

  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_i( 5,1,:)=0    ! category of data     src_obs_proc_cdf.f90(4.21):2654
    array_i( 6,1,:)=7    ! subcategory of data  src_obs_proc_cdf.f90(4.21):2665

  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    array_i( 5,1,:)=2    ! category of data     src_obs_cdfin_comhead.f90(5.00):1215
    array_i( 6,1,:)=11   ! subcategory of data  src_obs_cdfin_comhead.f90(5.00):1223

  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. trim(Observation)=='ship')THEN
    array_i( 5,1,:)=1    ! category of data     src_obs_proc_cdf.f90(4.21):2668
    array_i( 6,1,:)=0    ! subcategory of data  src_obs_proc_cdf.f90(4.21):2668

  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. trim(Observation)=='buoy')THEN
    array_i( 5,1,:)=1    ! category of data     src_obs_proc_cdf.f90(4.21):2668
    array_i( 6,1,:)=25   ! subcategory of data  src_obs_proc_cdf.f90(4.21):2671

  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT'  )THEN
    array_i( 5,1,:)=2    ! category of data     src_obs_proc_cdf.f90:2675
    array_i( 6,1,:)=4    ! subcategory of data  src_obs_proc_cdf.f90:2675
    IF( trim(Observation)=='tempship' )THEN
      array_i( 6,1,:)=5  ! subcategory of data  src_obs_proc_cdf.f90:2675
    ELSEIF( trim(Observation)=='pilot_p' )THEN
      array_i( 6,1,:)=1  ! subcategory of data  src_obs_proc_cdf.f90:2675
    ENDIF

  ELSEIF( df(el,t)%RECNAME=='TEBDTMAK' .or. df(el,t)%RECNAME=='TEBDWMAK' )THEN
    array_i( 5,1,:)=2    ! category of data     src_obs_proc_cdf.f90:2675
    array_i( 6,1,:)=4    ! subcategory of data  src_obs_proc_cdf.f90:2675

  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    array_i( 5,1,:)=4    ! category of data     src_obs_proc_cdf.f90(4.21):2684
    array_i( 6,1,:)=1    ! subcategory of data  src_obs_proc_cdf.f90(4.21):2685

  ENDIF
  
  IF (LP > 4 ) WRITE (*,*) 'end RT INIT_code_data()'

ENDSUBROUTINE init_code_data

SUBROUTINE INIT_date_time(i1,i2)
  INTEGER, INTENT(IN) :: i1, i2
  INTEGER  ee, j, jj, el_link
  INTEGER*4 :: ntobs
  ntobs=ubound(df(:,:),1)

  IF (LP > 4 ) WRITE (*,*) 'start RT INIT_date_time()'

  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_i( 7,1,:)= DATE(t)
    array_i( 8,1,:)= df(el,t)%Hour*10000 + df(el,t)%minute*100

  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_i( 7,1,:)= DATE(i1:i2)
    array_i( 8,1,:)= HOUR(i1:i2)

  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    array_i( 7,1,:)= DATE(i1:i2)
    array_i( 8,1,:)= HOUR(i1:i2)

  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
    array_i( 7,1,:)= DATE(t)
    array_i( 8,1,:)= df(el,t)%Hour*10000 + df(el,t)%minute*100

  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    WHERE( var(el,t)%p2(7,i1:i2)< 0 )  var(el,t)%p2(7,i1:i2) = Hour(t)*100
    WHERE( var(el,t)%p2(8,i1:i2)< 0 )
      WHERE( var(el,t)%p2(7,i1:i2)> 2200 )
        array_i( 7,1,:)= DATE(t)-1
      ELSE WHERE
        array_i( 7,1,:)= DATE(t)
      ENDWHERE
    ELSE WHERE
      array_i( 7,1,:)= Year(t)*10000 + INT(var(el,t)%p2(8,i1:i2),8)  ! i8 -impotant for correct data
    ENDWHERE
    array_i( 8,1,:)=  var(el,t)%p2(7,i1:i2) * 100 ! возможно, стоит добавить перепроверку

  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    WHERE( var(el,t)%p2(7,i1:i2)< 0 )  var(el,t)%p2(7,i1:i2) = Hour(t)*100
    WHERE( var(el,t)%p2(8,i1:i2)< 0 )
      WHERE( var(el,t)%p2(7,i1:i2)> 2000 )
        array_i( 7,1,:)= DATE(t)-1
      ELSE WHERE
        array_i( 7,1,:)= DATE(t)
      ENDWHERE
    ELSE WHERE
      array_i( 7,1,:)= Year(t)*10000 + INT(var(el,t)%p2(8,i1:i2),8)  ! i8 -impotant for correct data
    ENDWHERE
    array_i( 8,1,:)=  var(el,t)%p2(7,i1:i2) * 100

  ELSEIF( df(el,t)%RECNAME=='TEBDTMAK' .or. df(el,t)%RECNAME=='TEBDWMAK' )THEN
    el_link=0
    DO ee=1,ntobs
      IF (df(ee,t)%RECNAME=='TEMPMAKT') el_link=ee
    ENDDO
    IF( LP>4 ) WRITE(*,*) 'el_link=', el_link 
    IF ( el_link<1 .or. el_link>10 ) STOP 'bad el_link'

    jj=1
    DO j=1, df(el,t)%NY
      DO WHILE ( (var(el_link,t)%p2(1,jj)*1000+var(el_link,t)%p2(2,jj)) /=    & 
                 (var(el,t)%p2(1,j)*1000+var(el,t)%p2(2,j))  .and.  jj <= df(el_link,t)%NY )
        jj=jj+1
      ENDDO
      IF( var(el_link,t)%p2(1,jj)==var(el,t)%p2(1,j) .and.              &
          var(el_link,t)%p2(2,jj)==var(el,t)%p2(2,j) )THEN
        array_i( 7,1,j) = var(el_link,t)%p2(8,jj)  ! day
        array_i( 8,1,j) = var(el_link,t)%p2(7,jj)  ! time
        IF( var(el_link,t)%p2(92,jj) >= 0 )THEN
          array_i(41,1,j) = INT( var(el_link,t)%p2(92,jj)/100 )             ! RaRa
          array_i(42,1,j) = mod( INT(var(el_link,t)%p2(92,jj)), 100 )       ! SaSa
        ENDIF
        array_i(43,1,j) = var(el_link,t)%p2(91,jj)/100    ! NSR

        jj=1
        !if( LP>1 ) WRITE(*,*) 'bingo='
        CYCLE
      ENDIF
    ENDDO
    WHERE( array_i( 8,1,:)< 0 )  array_i( 8,1,:) = Hour(t)*100

    WHERE( array_i( 7,1,:) < 0 )
      WHERE( array_i( 8,1,:) > 2200 )
        array_i( 7,1,:)= DATE(t)-1
      ELSE WHERE
        array_i( 7,1,:)= DATE(t)
      ENDWHERE
    ELSE WHERE
      array_i( 7,1,:)= Year(t)*10000 + INT( array_i( 7,1,:) ,8)  ! i8 -impotant for correct data
    ENDWHERE
    array_i( 8,1,:) = array_i( 8,1,:) * 100
    
  ENDIF

! Year Month Day Hour Minute
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' .or. &
      df(el,t)%RECNAME=='SHIPBMAK' )THEN
    array_i(12,1,:)=df(el,t)%Year
    array_i(13,1,:)=df(el,t)%month
    array_i(14,1,:)=df(el,t)%day
    array_i(15,1,:)=df(el,t)%Hour
    array_i(16,1,:)=df(el,t)%minute
    ! for buoy
    array_i(55,1,:)=df(el,t)%Year
    array_i(56,1,:)=df(el,t)%month
    array_i(57,1,:)=df(el,t)%day
    array_i(58,1,:)=df(el,t)%Hour
    array_i(59,1,:)=df(el,t)%minute

  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_i(12,1,:)= array_i( 7,1,:)/10000
    array_i(13,1,:)= mod( array_i( 7,1,:)/100 , 100 )
    array_i(14,1,:)= mod( array_i( 7,1,:), 100 )
    array_i(15,1,:)= array_i( 8,1,:)/10000
    array_i(16,1,:)= mod( array_i( 8,1,:)/100 , 100 )

  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    array_i(12,1,:)= array_i( 7,1,:)/10000
    array_i(13,1,:)= mod( array_i( 7,1,:)/100 , 100 )
    array_i(14,1,:)= mod( array_i( 7,1,:), 100 )
    array_i(15,1,:)= array_i( 8,1,:)/10000
    array_i(16,1,:)= mod( array_i( 8,1,:)/100 , 100 )

  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    array_i(12,1,:)= array_i( 7,1,:)/10000 
    array_i(13,1,:)= mod( array_i( 7,1,:)/100 , 100 )
    array_i(14,1,:)= mod( array_i( 7,1,:), 100 ) 
    array_i(15,1,:)= var(el,t)%p2(7,i1:i2)/100
    array_i(16,1,:)= mod( INT(var(el,t)%p2(7,i1:i2)), 100 )

  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' .or.   &  ! может быть, стоит переписать эту секцию
          df(el,t)%RECNAME=='TEBDTMAK' .or. df(el,t)%RECNAME=='TEBDWMAK' )THEN
    array_i(12,1,:)= array_i( 7,1,:)/10000 
    array_i(13,1,:)= mod( array_i( 7,1,:)/100 , 100 )
    array_i(14,1,:)= mod( array_i( 7,1,:), 100 ) 
    array_i(15,1,:)= INT( var(el,t)%p2(7,i1:i2)/100 )
    array_i(16,1,:)= mod( INT(var(el,t)%p2(7,i1:i2)), 100 )
    array_i(21,1,:)= 18

  ENDIF

  IF (LP > 4 ) WRITE (*,*) 'end RT INIT_date_time()'

ENDSUBROUTINE INIT_date_time

SUBROUTINE INIT_wmo_index(i1,i2,Observation)
  INTEGER, INTENT(IN) :: i1, i2
  CHARACTER(len = *), INTENT(IN) ::    Observation

  IF (LP > 4 ) WRITE (*,*) 'start RT INIT_wmo_index()'

! WMO index stantion
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' .or.    &
      df(el,t)%RECNAME=='TEMPMAKT' .or. df(el,t)%RECNAME=='TEBDTMAK' .or.    &
      df(el,t)%RECNAME=='TEBDWMAK' )THEN
    array_i( 9,1,:)=var(el,t)%p2(1,i1:i2)
    array_i(10,1,:)=var(el,t)%p2(2,i1:i2)

  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    FORALL(j=i1:i2) array_c9(j) = df(el,t)%idST(j)(1:8)//df(el+1,t)%idST(j)(1:1)

  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    array_i( 9,1,:) = INT( var(el,t)%p2(1,i1:i2) ) / 1000
    array_i(10,1,:) = MOD( INT(var(el,t)%p2(1,i1:i2)), 1000)

  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. Observation == 'ship' )THEN
    array_c9(:) = df(el,t)%idST(i1:i2)

  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. Observation == 'buoy' )THEN
    array_i(50,1,:)=(IBITS(INT(var(el,t)%p2(5,i1:i2)), 0,8 )-48)*10000 +    &
                    (IBITS(INT(var(el,t)%p2(5,i1:i2)), 8,8 )-48)*1000  +    &
                    (IBITS(INT(var(el,t)%p2(6,i1:i2)), 0,8 )-48)*100   +    &
                    (IBITS(INT(var(el,t)%p2(6,i1:i2)), 8,8 )-48)*10    +    &
                    (IBITS(INT(var(el,t)%p2(7,i1:i2)), 0,8 )-48)
! Почему-то только первые 16 битов используются для записи символов.
! Остальные биты забиты нулями.
!write(*,'(4(Z,"-"))') INT(var(el,t)%p2(5,i1)),INT(var(el,t)%p2(6,i1)),INT(var(el,t)%p2(7,i1)),INT(var(el,t)%p2(8,i1))

  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
  !!FORALL (i=i1:i2) array_c8(i)=df(el,t)%idST(i)
    array_c8(:) = df(el,t)%idST(i1:i2)

  ENDIF
  
  IF (LP > 4 ) WRITE (*,*) 'end RT INIT_wmo_index()'

ENDSUBROUTINE INIT_wmo_index

SUBROUTINE INIT_meteo_fields(i1,ni,Observation)
  INTEGER, INTENT(IN) :: i1, ni
  CHARACTER(len = *), INTENT(IN) ::    Observation
  INTEGER*4 i2
  i2=i1-1+ni

  IF(LP>4) WRITE (*,*) 'start RT INIT_meteo_fields()',i1,i2,ni

! type SYNOP station (NIX)
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_i(11,1,:)=1          ! manned station
  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_i(11,1,:)=0          ! auto station
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. Observation == 'ship' )THEN
    array_i(11,1,:)=1          ! manned station
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. Observation == 'buoy' )THEN
    array_i(11,1,:)=0          ! auto station
  ENDIF

! Wind Direction (NDNDN)
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_i(17,1,:)=var(el,t)%p2(12,i1:i2)
    !array_i(21,1,:)=2
  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    IF(LP>4) WRITE (*,*)'start initial wdir', t !, ubound(array_i(17,1,:)),ubound(var(el,t)%p2(4,i1:i2))
!!  WRITE (*,*) var(el,t)%p2(4,i1:i2)
    array_i(17,1,:) = var(el,t)%p2(4,i1:i2)
    IF(LP>4) WRITE (*,*)'end initial wdir'
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
    array_i(17,1,:)=var(el,t)%p2(12,i1:i2)
  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    array_i(17,1,:)=var(el,t)%p2(11,i1:i2)
  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    array_i(17, 1,:)=var(el,t)%p2(99,i1:i2)
    FORALL (i=2:17)  array_i(17, i,:)=var(el,t)%p2( i*5+4,i1:i2)
  ELSEIF( df(el,t)%RECNAME=='TEBDWMAK' )THEN
    FORALL (i=1:98)  array_i(17, i,:) = var(el,t)%p2 (i*3+9,i1:i2)
  ENDIF

! clouds (MN,NH)
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_i(22,1,:)=var(el,t)%p2(18,i1:i2)     ! MN in  %
    ! below check for acceptable value
    WHERE( array_i(22,1,:)>101 ) array_i(22,1,:)=oundef
    WHERE( array_i(22,1,:)<0   ) array_i(22,1,:)=oundef
    array_f(16,1,:)=var(el,t)%p2(23,i1:i2)     ! NH
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
    array_i(22,1,:)=var(el,t)%p2(18,i1:i2)     ! MN in  %
  ENDIF

! snow (NSSS)
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_f(22,1,:)=var(el,t)%p2(33,i1:i2)     ! NSSS
  ENDIF

! geopot(NHHHN)
  IF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    array_i(40, 1,:) = var(el,t)%p2(4,i1:i2)     ! on level ground ! Корректнее ещё добавить 2(метра) там где не пустые занчения
    FORALL (i=2:17)  array_i(40, i,:)=var(el,t)%p2( i*5+1,i1:i2)
  ENDIF

! TEMP var: RaRa SaSa ... ... 
  IF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    WHERE( var(el,t)%p2(92,i1:i2) >= 0 ) array_i(41,1,:) = INT( var(el,t)%p2(92,i1:i2)/100 )
    WHERE( var(el,t)%p2(92,i1:i2) >= 0 ) array_i(42,1,:) = mod( INT(var(el,t)%p2(92,i1:i2)), 100 ) 
    array_i(43,1,:)= var(el,t)%p2(91,i1:i2)
    array_i(44,1,:)= oundef
    array_i(45,1,:)= oundef
    array_i(46,1,:)= oundef
    array_i(48,1,:)= oundef
    array_i(49,1,:)= oundef
  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    FORALL (l=1:dim(2)%nPt )
      array_i(45,l,:)= var(el,t)%p2(4,i1:i2) + 50 * (l-1) ! MH
    ENDFORALL
  ENDIF

! lat lon height presHeightSt
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_f( 1,1,:) = var(el,t)%p2(3,i1:i2)  ! MLAH
    array_f( 2,1,:) = var(el,t)%p2(4,i1:i2)  ! MLON
    array_f( 3,1,:) = var(el,t)%p2(5,i1:i2)  ! MHOSNN
    array_f( 4,1,:) = oundef                 ! MHOBNN
  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_f( 1,1,:) = var(el,t)%p2(23,i1:i2) ! MLAH
    array_f( 2,1,:) = var(el,t)%p2(24,i1:i2) ! MLON
    array_f( 3,1,:) = var(el,t)%p2(22,i1:i2) ! MHOSNN
    array_f( 4,1,:) = oundef                 ! MHOBNN
  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    array_f(23,1,:) = var(el,t)%p2(2,i1:i2)  ! MLALA
    array_f(24,1,:) = var(el,t)%p2(3,i1:i2)  ! MLOLO
    array_i(54,1,:) = var(el,t)%p2(4,i1:i2)  ! MHP
    array_i(28,1,:) = dim(2)%nPt             ! MDREP
!   array_f( 4,1,:) = oundef                 ! MHOBNN
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. Observation=='ship' )THEN
    array_f(23,1,:) = var(el,t)%p2(3,i1:i2)  ! MLALA
    array_f(24,1,:) = var(el,t)%p2(4,i1:i2)  ! MLOLO
    array_f( 3,1,:) = 2                      ! MHOSNN
    array_f( 4,1,:) = oundef                 ! MHOBNN
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' .and. Observation=='buoy' )THEN
    array_f( 1,1,:) = var(el,t)%p2(3,i1:i2)  ! MLAH
    array_f( 2,1,:) = var(el,t)%p2(4,i1:i2)  ! MLON
    array_f( 3,1,:) = 2                      ! MHOSNN
    array_f( 4,1,:) = oundef                 ! MHOBNN
  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' .or.                                  &
          df(el,t)%RECNAME=='TEBDTMAK' .or. df(el,t)%RECNAME=='TEBDWMAK' )THEN
    array_f( 1,1,:)=var(el,t)%p2(5,i1:i2)    ! MLAH
    array_f( 2,1,:)=var(el,t)%p2(6,i1:i2)    ! MLON 
    array_f( 3,1,:)=var(el,t)%p2(4,i1:i2)    ! MHOSNN
    array_f( 4,1,:) = oundef
  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    array_f( 1,1,:) = var(el,t)%p2(5,i1:i2)       ! MLAH
    array_f( 2,1,:) = var(el,t)%p2(6,i1:i2)       ! MLON
    WHERE( var(el,t)%p2(9,i1:i2)>0 )              &
    array_i(51,1,:) = var(el,t)%p2(9,i1:i2)/30.48 ! NFLEV in hectofoot (FL)
    ! если высота равна iundef, и давление равно 1013, тогда высота равна 0-50м
    array_i(52,1,:) = oundef                      ! MPHAI
    array_i(53,1,:) = oundef                      ! MQARA
  ENDIF

! PS PMSL dPS3 T_2m Td_2m hVisib MHOSEN
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_f( 5,1,:) = var(el,t)%p2(8,i1:i2)   ! MPPP
    array_f( 6,1,:) = var(el,t)%p2(9,i1:i2)   ! MPPPP
    array_f( 7,1,:) = var(el,t)%p2(15,i1:i2)  ! NPPP
    array_f( 8,1,:) = var(el,t)%p2(10,i1:i2)  ! MTDBT
    array_f( 9,1,:) = var(el,t)%p2(11,i1:i2)  ! MTDNH
    array_f(11,1,:) = var(el,t)%p2(16,i1:i2)  ! MVV
    array_f(14,1,:) = 2                       ! MHOSEN

  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_f( 5,1,:) = var(el,t)%p2( 2,i1:i2)  ! MPPP
    array_f( 6,1,:) = var(el,t)%p2( 8,i1:i2)  ! MPPPP
    array_f( 8,1,:) = var(el,t)%p2( 1,i1:i2)  ! MTDBT
    array_f( 9,1,:) = var(el,t)%p2( 7,i1:i2)  ! MTDNH
    array_f(14,1,:) = 2                       ! MHOSEN
    array_i(18,1,:) = var(el,t)%p2( 3,i1:i2)  ! MUUU (relhum)

  ELSEIF( df(el,t)%RECNAME=='RASS' )THEN
    FORALL( l=1:dim(2)%nPt )
      array_f(25,l,:) = var(el,t)%p2(l+4,i1:i2)  ! MTVIR
    ENDFORALL

  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
    array_f( 5,1,:) = var(el,t)%p2(9,i1:i2)   ! MPPP
    array_f( 6,1,:) = var(el,t)%p2(9,i1:i2)   ! MPPPP
    array_f( 7,1,:) = var(el,t)%p2(15,i1:i2)  ! NPPP
    array_f( 8,1,:) = var(el,t)%p2(10,i1:i2)  ! MTDBT
    array_f( 9,1,:) = var(el,t)%p2(11,i1:i2)  ! MTDNH
    array_f(11,1,:) = var(el,t)%p2(16,i1:i2)  ! MVV
    array_f(14,1,:) = 2                       ! MHOSEN

  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    array_f( 8,1,:) = var(el,t)%p2(13,i1:i2)  ! MTDBT
    array_f( 9,1,:) = oundef                  ! MTDNH

  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    array_f( 8, 1,:)=var(el,t)%p2( 97,i1:i2)
    WHERE( var(el,t)%p2(98,i1:i2) >=0 ) array_f( 9, 1,:) = var(el,t)%p2( 97,i1:i2) - var(el,t)%p2( 98,i1:i2)
    FORALL (i=2:17)
      array_f( 8, i,:)=var(el,t)%p2( i*5+2,i1:i2)
      WHERE( var(el,t)%p2(i*5+3,i1:i2) >=0 ) array_f(9, i,:) = var(el,t)%p2(i*5+2,i1:i2) - var(el,t)%p2(i*5+3,i1:i2)
    END FORALL

  ELSEIF( df(el,t)%RECNAME=='TEBDTMAK' )THEN ! T Td
    FORALL (i=1:98)
      array_f( 8, i,:)=var(el,t)%p2 (i*3+9 ,i1:i2)
      array_f( 9, i,:)=var(el,t)%p2 (i*3+10,i1:i2)
    END FORALL

  ENDIF

! wind speed
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_f(10,1,:)=var(el,t)%p2(13,i1:i2)    ! NFNFN
  ELSEIF( df(el,t)%RECNAME=='AMS' )THEN
    array_f(10,1,:)=var(el,t)%p2(13,i1:i2)    ! NFNFN
  ELSEIF( df(el,t)%RECNAME=='SHIPBMAK' )THEN
    array_f(10,1,:)=var(el,t)%p2(13,i1:i2)    ! NFNFN
  ELSEIF( df(el,t)%RECNAME=='AIREPMAK' )THEN
    array_f(10,1,:)=var(el,t)%p2(12,i1:i2)    ! NFNFN
  ELSEIF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    array_f(10, 1,:)=var(el,t)%p2(100,i1:i2)  ! NFNFN
    FORALL (i=2:17)  array_f(10, i,:)=var(el,t)%p2( i*5+5,i1:i2)
  ELSEIF( df(el,t)%RECNAME=='TEBDWMAK' )THEN
    FORALL (i=1:98)  array_f(10, i,:) = var(el,t)%p2 (i*3+10,i1:i2) ! NFNFN
  ENDIF

! precipitation
  IF( df(el,t)%RECNAME=='SYNOPMAK' .or. df(el,t)%RECNAME=='SYNOPDOP' )THEN
    array_f(13,1,:)=var(el,t)%p2(29,i1:i2)
    array_f(13,2,:)=var(el,t)%p2(30,i1:i2)
    array_f(15,1,:)=var(el,t)%p2(31,i1:i2)
    array_i(33,1,:)= -6     ! MGGTP1
    array_i(33,2,:)= -12
  ENDIF

! vertical pressure
  IF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    array_f(19, 1,:)= var(el,t)%p2( 96,i1:i2)
    array_f(19, 2,:)= 100000.
    array_f(19, 3,:)=  92500.
    array_f(19, 4,:)=  85000.
    array_f(19, 5,:)=  70000.
    array_f(19, 6,:)=  50000.
    array_f(19, 7,:)=  40000.
    array_f(19, 8,:)=  30000.
    array_f(19, 9,:)=  25000.
    array_f(19,10,:)=  20000.
    array_f(19,11,:)=  15000.
    array_f(19,12,:)=  10000.
    array_f(19,13,:)=   7000.
    array_f(19,14,:)=   5000.
    array_f(19,15,:)=   3000.
    array_f(19,16,:)=   2000.
    array_f(19,17,:)=   1000.
  ELSEIF( df(el,t)%RECNAME=='TEBDTMAK' .or. df(el,t)%RECNAME=='TEBDWMAK' )THEN
    FORALL (i=1:98)  array_f(19, i,:)=var(el,t)%p2 (i*3+8,i1:i2)
  ENDIF

! MEDRE
  IF( df(el,t)%RECNAME=='TEMPMAKT' )THEN
    FORALL(j=1:ni) array_i(47,1,j)=count( array_i(17, 1:dim(2)%nPt,j)>oundef )
  ELSEIF( df(el,t)%RECNAME=='TEBDTMAK' .or. df(el,t)%RECNAME=='TEBDWMAK' )THEN
    FORALL(j=1:ni) array_i(47,1,j)=count( array_f(19, 1:dim(2)%nPt,j)>oundef )
  ENDIF

! level significance MEVSS, BUFR Tab 008042, data_obs_cdfin.f90:368
  SELECT CASE( df(el,t)%RECNAME )
  CASE ('TEMPMAKT')
    WHERE( array_i(17,    1,:)>oundef ) array_i(49,1,:)   = 131072
    WHERE( array_i(17, 2:17,:)>oundef ) array_i(49,2:17,:)= 65536
  CASE ('TEBDTMAK')
    WHERE( array_f(19, :,:)>oundef) array_i(49,:,:)= 8192
!+++ добавить значения по тропопаузе и по максимальному ветру
  CASE ('TEBDWMAK')
    WHERE( array_f(19, :,:)>oundef) array_i(49,:,:)= 2048
!+++ добавить значения по тропопаузе и по максимальному ветру
  ENDSELECT

  IF (LP > 4 ) WRITE (*,*)'end RT INIT_meteo_fields()'

ENDSUBROUTINE INIT_meteo_fields

