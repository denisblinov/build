MODULE RemDB
  COMMON/RemotDB/ sock, LevelPrint
  INTEGER*4 sock
  INTEGER*4 LevelPrint/0/
ENDMODULE RemDB

MODULE  main_data
! This module is main for all type meteodata and for different programm:
! 1)remDB  2) WRITE netCDF4COSMO 3) collect data

  INTEGER, PARAMETER ::     nTmax=300         ! ������������ ���������� ��������� �� �������. 300/8=37.5 3h-term
  INTEGER*4    el, t    ! ���������� ��� ������
  INTEGER*4, DIMENSION(nTmax) :: Year, month, Day, minute, second
  INTEGER*4  hour(20000), &   ! ���� 
             Date(20000), DATEHH(nTmax),   &   ! ����  ! variable for base
             date_end, hstep,          &   ! �������� ����
             startTerm/0/, endTerm/0/, &   ! ������ � ��������� ������������������
             HFstep/6/                     ! ��� � ����� ����� ��������������������
  REAL*4 ::  iundef= -9999.0, oundef= -999999.0

! INTEGER*4    newDATE     ! function date

! this structure for description 2-DIMENSION fields
  TYPE descrRECORD
   !SEQUENCE
    INTEGER*4::              NX      ! 
    INTEGER*4::              NY      !
    INTEGER*4,DIMENSION(5):: nYY     ! number points in row
    INTEGER*4   ::           year
    INTEGER*4   ::           month
    INTEGER*4   ::           day
    INTEGER*4   ::           hour
    INTEGER*4   ::           minute
    INTEGER*4   ::           second
    CHARACTER*8              RECNAME ! name in base RHM
    CHARACTER*10::           charDATEHH
    CHARACTER(len=12),DIMENSION(100)  ::  header  !
    CHARACTER(len=12),DIMENSION(100)  ::  clevel  !
    CHARACTER(len=8),DIMENSION(20000) ::  idST    ! list id
  ENDTYPE descrRECORD

  TYPE outfield
    SEQUENCE
    REAL*4, ALLOCATABLE ::      p2(:,:)
!   REAL*4, ALLOCATABLE ::      p3(:,:,:)
  ENDTYPE outfield

  TYPE (descrRECORD),  ALLOCATABLE ::  df(:,:)   ! description Field
  TYPE (outfield),     ALLOCATABLE ::  var(:,:)  ! array with values

CONTAINS

SUBROUTINE array_def( def )

    INTEGER*4 def(60)

    !WRITE (*,*) 'definition field ',def
    WRITE (*,*) '--begin------definition field----------------'
    WRITE (*,*) 'code of file             ',def(1)
    WRITE (*,*) 'number of center         ',def(2)
    WRITE (*,*) 'code of GRID             ',def(3)
    WRITE (*,*) 'code of unit             ',def(4)
    WRITE (*,*) 'reserved                 ',def(5)
    WRITE (*,*) 'reserved                 ',def(6)
    WRITE (*,'(" time and day delivery for term 00",i4.2,":",i2.2, i4.2)') def(8),  def(9), def(7)
    WRITE (*,'(" time and day delivery for term 12",i4.2,":",i2.2, i4.2)') def(11), def(12),def(10)
    WRITE (*,*) 'reserved                 ',def(13)
    WRITE (*,*) 'reserved                 ',def(14)
    WRITE (*,*) 'logicUserUndef           ',def(15)
    WRITE (*,*) 'lmarkPark                ',def(16)
    WRITE (*,*) 'NX                       ',def(17)
    WRITE (*,*) 'NY                       ',def(18)
    WRITE (*,*) 'processID                ',def(19)
    WRITE (*,*) 'spectralMark             ',def(20)
    WRITE (*,*) 'type handle data         ',def(21)
    WRITE (*,*) 'code keep data           ',def(22)
    WRITE (*,*) 'SUR1                     ',def(23)
    WRITE (*,*) 'SUR2                     ',def(24)
    WRITE (*,*) 'LEV1                     ',def(25)
    WRITE (*,*) 'LEV2                     ',def(26)
    WRITE (*,*) 'LEVTYPE                  ',def(27)
    WRITE (*,*) 'DATATYPE                 ',def(28)
    WRITE (*,*) 'MIN                      ',def(29)
    WRITE (*,*) 'MAX                      ',def(30)
    WRITE (*,*) 'precision data           ',def(31)
    WRITE (*,*) 'QUALITY data             ',def(32)
    WRITE (*,*) 'term gather data         ',def(33)
    WRITE (*,*) 'termKeepGatherdata       ',def(34)
    WRITE (*,*) 'code discret             ',def(35)
    WRITE (*,*) 'NY                       ',def(36)
    WRITE (*,'(a,3x,8i6)') 'list term forecast',def(37:44)
    WRITE (*,*) 'term forecast            ',def(45)
    WRITE (*,*) 'value NX_first           ',def(46)
    WRITE (*,*) 'value NX_last            ',def(47)
    WRITE (*,*) 'step  NX (dX)            ',def(48)
    WRITE (*,*) 'value NY_first           ',def(49)
    WRITE (*,*) 'value NY_last            ',def(50)
    WRITE (*,*) 'step  NY (dY)            ',def(51)
    WRITE (*,*) 'value axe3_first         ',def(52)
    WRITE (*,*) 'value axe3_last          ',def(53)
    WRITE (*,*) 'step  axe3               ',def(54)
    WRITE (*,*) 'value axe4_first         ',def(55)
    WRITE (*,*) 'value axe4_last          ',def(56)
    WRITE (*,*) 'step  axe4               ',def(57)
    WRITE (*,*) 'value axe5_first         ',def(58)
    WRITE (*,*) 'value axe5_last          ',def(59)
    WRITE (*,*) 'step  axe5               ',def(60)
    WRITE (*,*) '--end------definition field------------------';

ENDSUBROUTINE array_def

SUBROUTINE CNAME (Name,Ele,Lev,DOMAIN,Zab,Grid)
    CHARACTER*8  Name
    CHARACTER*4 Ele,DOMAIN,Grid
    INTEGER*4 Lev,Zab,Lv1,Lv2,Zb1,Zb2,Zb3,Lv
    Lv=Lev
    SELECT CASE(Lev)
        CASE(55)
            Lv=Lev
        CASE(96, 97, 98, 99)
            Lv=Lev
        CASE(87, 88, 89)
            Lv=Lev
        CASE(71,72,79)
            Lv=Lev
        CASE(1000)
            Lv=0
        CASE DEFAULT
            Lv=Lev/10
    ENDSELECT
    Lv1=mod(LV,10)
    Lv2=LV/10
    Zb1=MOD(Zab,10)
    Zb3=Zab/100
    Zb2=MOD(Zab,100)/10
    WRITE (Name,'(A1,2I1,A1,3I1,A1)') Ele,Lv2,Lv1,DOMAIN,Zb3,Zb2,Zb1,Grid
RETURN
ENDSUBROUTINE CNAME

!******************************************************************************************************************
! ��������� CNAME ��������� ����� �������, ���������� ���� ����������� �������� � ��������� ���, ����������� � ����������� ������ ������
! CALL CNAME (RECNAME(el),ELE,LEV,DOMAIN,ZAB,GRID)  ! ��������� ��� ������
!
!    ELE        - ��� �������� � ��������� ����. Character*4. 'X---'
!    LEV        - �������� �������� � ��� ��� �������������� ������� ������(98 ������� ����, 99 - ����������� �����)
!    DOMAIN        - �������������� �������. Character*4.'X---': N - �������� ���������, S - ����� ���������, R - ������� ������� 
!    ZAB        - �������� �������� ������������������ ��������(� �����), ��� ����� ������� ZAB=0
!    GRID    - ����������� ����� � ��������� �����. Character*4. 'X---' (G - 145x37)
!    RECNAME    - �������� �������� � ������� ����������� ��� ������. CHARACTER*8
!                    ��������� �����:
!        x1        - �������� � ������ ��������
!        x2x3    - ����������� ��� �������  
!        x4        - �������������� �������
!        x5x6x7    - ������������������ ��������    
!        x8        - �����
! �������� H500N072G
!******************************************************************************************************************
! ��������� TIMEFM(MR) �������� ������� ���� � �����
! Call TIMEFM(MR)    , ���  MR - ������ ������ ���� �� 15 ���������, ��� ������ 6:
!    MR(1)    - ��������� 2 ����� ����
!    MR(2)    - ����� ������ � ����
!    MR(3)    - ����� ��� � ������
!    MR(4)    - ����
!    MR(5)    - ������
!    MR(6)    - �������
!******************************************************************************************************************
! ��������� YESTER ��������� �������������� ���� 
! CALL YESTER(D,M,Y,DY,MY,YY)
!    D,M,Y - ������ ���, ������ � ���� �������� ���� INTEGER
!    DY,MY,YY - ������ ���, ������ � ���� �������������� ����(��������� �� ������ ���� �� ���� �����) INTEGER
! ��� ��������� ������ ����� ��� INTEGER*4
!******************************************************************************************************************
! ��������� OPENB � CLOSB ��������� � ��������� ���� ������
! CALL OPENB (kpp, bases, nbas, kv)
! CALL CLOSB (kpp, bases, nbas, kv)
!    kpp        - ��� ���������, ����������� ���� ������. INTEGER*4. ��������� ����: ������, ��� �� - ����������� ����� �������������, � ������� �������� ������������; �� - ����������� ����� ������������ � �������������; �� - ����������� ����� ��������� ������������.    
!    Bases    - ������ ��� ��� ������, A4(CHARACTER*4) ������������ ����� ���� ������� �� ����� 10 ��� ������
!    nbas    - ���������� ����������� ��� ������. �� ����� ���� ������ 10. INTEGER
!    kv        - ��� ��������, ������������ �� ���������� ���������. ���� kv=0, ��� ������, ��� ������ �� ����������.INTEGER*4
!******************************************************************************************************************
! ��������� OPENRemDB ��������� �������� ���� ������
! CALL OPENRemDB (kpp, bases, nbas, kv)
!    kpp        - ��� ���������, ����������� ���� ������. INTEGER*4. ��������� ����: ������, ��� �� - ����������� ����� �������������, � ������� �������� ������������; �� - ����������� ����� ������������ � �������������; �� - ����������� ����� ��������� ������������.    
!    Bases    - ������ ��� ��� ������, A4(CHARACTER*4) ������������ ����� ���� ������� �� ����� 10 ��� ������
!    nbas    - ���������� ����������� ��� ������. �� ����� ���� ������ 10. INTEGER
!    kv        - ��� ��������, ������������ �� ���������� ���������. ���� kv=0, ��� ������, ��� ������ �� ����������.INTEGER*4
!******************************************************************************************************************
! ��������� RDFC ������ ���������� ���������� ���������� ������ �� ������������ ����� (RdfcRemDB - ����������� ���������, ��� ��������� ���� �����)
! CALL RDFC(basnam,kf,dath,recnam,srn,wa,kv)    
!    basnam(Bases(i)) -  ��� ���� ������, A4(CHARACTER*4)
!    kf        - ��� ����� ���� ������, �� �������� �������� ��� � ������� ������������ ����������. INTEGER*4
!    dath    - ���� ���������� � ������� ��������� �������� ��� ������������ ������. ������. INTEGER*4
!    RECNAME    - ��� �������� ���� ������ ������������� ������ ������� ����. CHARACTER*8
!    srn        - ���� ����������(� �����), � �������� ��������� �������� ��� ������������ ������. INTEGER*4 
!    wa        - ������� ������� ����������� ������, � ������� �������� �� ���� ������ ��� �� ������� ������������ � ���� ������ ���������� �������. ��� �������,���������� � ��� ��������� �������� ������ ��������������� ����������� � �� ������. Character
!    kv        - ��� ��������, ������������ �� ���������� ���������. INTEGER*4
!******************************************************************************************************************
! ��������� OPENRemHost ����������� � ����������� �� ������� ��������� ����
! CALL OPENRemHost(FHost,kv)
!    FHost    - IP-����� ���������� � ��������� �����. CHARACTER*15.
!        FHost=FHost_cray/'192.24.25.2'/
!        FHost=FHost_xeon/'192.168.97.25'/
!    kv        - ��� ��������, ������������ �� ���������� ���������. INTEGER*4
!*******************************************************************************
!    P        - ��������
!    H        - ������������
!    T        - ����������� �������
!    S        - ����������� ����� ����
!    D        - ������� ����� ����
!    R        - ������������� ���������
!    U        - ��������� ������������ �������� ����� 
!    V        - �������������� ������������ �������� �����
!    C        - ����� ����������
!    F        - ���������� �����
!    A        - ����������, ���������� �� ���������
!    B        - ������������ ��������
!    E        - ������������ ������� ������� �������
!    N        - ���������� �������
!    M        - ������� ������ ��������
!    K        - ������������ ������
!    Q        - ���������� ���������
!    W        - ����������� ����
!    Y        - ������������ ��������������
!    NL        - �������� ���������
!    NM        - ���� ���������� ����� �� ���������
!    FL        - ���������� ��������� ������
!    OR        - ���������
!    CD        - ����������� �������������
!    SX SY    - ���������� ����� ������������ ������������ ������� ���������
!    NH        - ������ � ������� ����������
!    PH        - �������� � ������
!    OC        - ������ �������� ��������
!*******************************************************************************


FUNCTION newDATE( iDATEhh, hourstep)
  IMPLICIT NONE
  INTEGER   (KIND=4), INTENT(IN)   ::                           &
    iDATEhh,   & ! DATE in format YYYYMMDDhh
    hourstep     ! step in hour
  INTEGER   (KIND=4)   ::                                       &
    ndaymonth(12), iyear, imonth, iday, ihour
  INTEGER   (KIND=4)   :: newDATE

  DATA  ndaymonth / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

  iyear  = iDATEhh/1000000
  imonth = mod(iDATEhh, 1000000)/10000
  iday   = mod(iDATEhh, 10000)/100
  ihour  = mod(iDATEhh, 100) + hourstep
  IF( mod(iyear,400)/=100 .and. mod(iyear,400)/=200 .and. mod(iyear,400)/=300 & 
      .and. mod(iyear,4)==0 )  ndaymonth(2)=29

  DO WHILE( ihour>23 )
    iday=iday+1
    ihour=ihour-24
    IF( iday>ndaymonth(imonth) )THEN
      iday=iday-ndaymonth(imonth)
      imonth=imonth+1
      IF( imonth>12 )THEN
        imonth=imonth-12
        iyear=iyear+1
      ENDIF
    ENDIF
  ENDDO
  newDATE=iyear*1000000 + imonth*10000 + iday*100 + ihour

ENDFUNCTION newDATE

!!+unix2c Converts Unix system time to date/time INTEGER array.
!subroutine unix2c(utime, idate)
!     implicit none
!     INTEGER utime, idate(6)
!!utime  input  Unix system time, seconds since 1970.0
!!idate  output Array: 1=year, 2=month, 3=date, 4=hour, 5=minute, 6=secs
!!-Author  Clive Page, Leicester University, UK.   1995-MAY-2
!     INTEGER mjday, nsecs
!     REAL day
!!Note the MJD algorithm only works from years 1901 to 2099.
!     mjday    = int(utime/86400 + 40587)
!     idate(1) = 1858 + int( (mjday + 321.51) / 365.25)
!     day      = aint( mod(mjday + 262.25, 365.25) ) + 0.5
!     idate(2) = 1 + int(mod(day / 30.6 + 2.0, 12.0) )
!     idate(3) = 1 + int(mod(day,30.6))
!     nsecs    = mod(utime, 86400)
!     idate(6) = mod(nsecs, 60)
!     nsecs    = nsecs / 60
!     idate(5) = mod(nsecs, 60)
!     idate(4) = nsecs / 60
!endsubroutine unix2c

ENDMODULE main_data
