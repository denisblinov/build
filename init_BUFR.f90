SUBROUTINE encode_bufr(filename)

  USE  routine4BUFR
IMPLICIT  INTEGER(I,K,N) !, REAL(R,V)
!REAL RVIND
!INTEGER IUNIT1

!
!**** *BUFR*
!
!
!     PURPOSE.
!     --------
!         An example of using Bufr packing software.
!         It will create synop data in bufr edition 4 
!
!
!**   INTERFACE.
!     ----------
!
!          NONE.
!
!     METHOD.
!     -------
!
!          NONE.
!
!
!     EXTERNALS.
!     ----------
!
!
!     REFERENCE.
!     ----------
!
!          NONE.
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       05/04/2005.
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!
!
      CHARACTER(len = 250), intent(IN) ::  filename
!
!                                                                       
!     ------------------------------------------------------------------
!*          1. INITIALIZE CONSTANTS AND VARIABLES.
!              -----------------------------------

!     RVIND=1.7D38
      RVIND=oundef
!

      CALL PBOPEN(IUNIT1,trim(filename),'W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON synop.dat'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
!
!     INITIALIZE DELAYED REPLICATION FACTORS OR REFERENCE VALUES ETD.
!
      KDATA(1:KDLEN)=1
      VALUES(1:KDLEN)=RVIND

      KDLENG=3
!
!     SET DATA DECSRIPTORS
!

      ktdlen=19

      ktdlst( 1)=001001     ! WMO BLOCK NUMBER
      ktdlst( 2)=001002     ! WMO STATION NUMBER
      ktdlst( 3)=004001     ! YEAR
      ktdlst( 4)=004002     ! MONTH
      ktdlst( 5)=004003     ! DAY
      ktdlst( 6)=004004     ! HOUR
      ktdlst( 7)=005001     ! LATITUDE (HIGH ACCURACY)
      ktdlst( 8)=006001     ! LONGITUDE (HIGH ACCURACY)
      ktdlst( 9)=007030     ! HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL (SEE NOTE 3)
      ktdlst(10)=010051     ! PRESSURE REDUCED TO MEAN SEA LEVEL
      ktdlst(11)=012004     ! TEMPERATURE AT 2M
      ktdlst(12)=012006     ! DEW POINT TEMPERATURE AT 2M
      ktdlst(13)=013022     ! TOTAL PRECIPITATION PAST 12 HOURS
      ktdlst(14)=013021     ! TOTAL PRECIPITATION PAST  6 HOURS
      ktdlst(15)=020010     ! CLOUD COVER (TOTAL)
      ktdlst(16)=011011     ! WIND DIRECTION AT 10 M
      ktdlst(17)=011012     ! WIND SPEED AT 10 M
      ktdlst(18)=022021     ! HEIGHT OF WAVES  M
      ktdlst(19)=013023     ! TOTAL PRECIPITATION PAST 24 HOURS

      values( 3)=REAL(df(el,t)%year, 8)   ! YEAR
      values( 4)=REAL(df(el,t)%month,8)   ! MONTH
      values( 5)=REAL(df(el,t)%day,  8)   ! DAY
      values( 6)=REAL(df(el,t)%hour, 8)   ! HOUR

!     SET CCITTIA5 STATION OR SITE NAME

      cvals(1)='MOSCOW'
!
!     SECTION 0 CONTENT
!
      KSEC0(1)=0      ! TOTAL LENGTH OF SECTION 0
      KSEC0(2)=0      ! TOTAL LENGTH OF BUFR MESSAGE
      KSEC0(3)=4      ! BUFR EDITION NUMBER
!
!     SECTION 1 CONTENT
!
      KSEC1(1)=22    ! TOTAL LENGTH OF SECTION 1 (  set to 18 for edition <= 3)
      KSEC1(2)=4     ! BUFR EDITION NUMBER
!     KSEC1(3)=89    ! ORIGINATING CENTRE
      KSEC1(3)=5     ! ORIGINATING CENTRE MOSCOW (COMMON CODE TABLE C–1: Identification of originating/generating centre)
      KSEC1(4)=1     ! UPDATE SEQUENCE NUMBER
      KSEC1(5)=0     ! FLAG (PRESENCE OF SECTION 2)
      KSEC1(6)=0     ! DATA CATEGORY
      KSEC1(7)=0     ! LOCAL DATA SUB-CATEGORY
      KSEC1(8)=0     ! VERSION NUMBER OF LOCAL TABLE USED
      KSEC1(9)=nint(values(3))   !! YEAR
      IF(KSEC1(2).le.3) THEN
        IF(ksec1(9).gt.2000) THEN
           ksec1(9)=ksec1(9)-2000
        ELSE
           ksec1(9)=ksec1(9)-1900
        ENDIF
      ENDIF
      KSEC1(10)=nint(values(4))   ! MONTH
      KSEC1(11)=nint(values(5))   ! DAY
      KSEC1(12)=nint(values(6))   ! HOUR
      KSEC1(13)=0                 ! MINUTE
      KSEC1(14)=0                 ! BUFR MASTER TABLE( ZERO) FOR METEOROLOGICAL DATA)
      KSEC1(15)=13                ! VERSION NUMBER OF MASTER TABLE USED
      KSEC1(16)=255               ! ORIGINATING SUB-CENTRE
      KSEC1(17)=2                 ! INTERNATIONAL SUB-CATEGORY
      KSEC1(18)=0                 ! SECOND
!
!     SECTION 2 CONTENT
!
      KSEC2(1)=52

      KSEC2(2:JSEC2)=0
!
!     SECTION 3 CONTENT
!
      KSEC3(1)=0     ! TOTAL LENGTH OF SECTION 3
      KSEC3(2)=0     ! RESERVED
      KSEC3(3)=1
      KSEC3(4)=0     ! 64 FOR COMPRESSION/ 0 MANY SUBSETS
      if(KSEC3(3).GT.1) KSEC3(4)=64
!
      IREP=0
!
!
!*          6. PACK BUFR MESSAGE
!              -----------------
!
!---------------------------------------------------------------
!              This CALL is not needed for packing. It just 
!              prints expanded list corresponding to ktdlst sequence
!              and delayed replications in kdata array. This four
!              lines can be deleted or commented out.
!       K=1
!       CALL BUXDES(K,KSEC1,KTDLEN,KTDLST,KDLENG,KDATA,KELEM,   &
!                  KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
!
!       IF(KERR.NE.0) CALL EXIT(2)
!---------------------------------------------------------------
!
!
!*          6.2 ENCODE DATA INTO BUFR MESSAGE.
!               ------------------------------
!

    DO I=1,df(el,t)%NY ! cycle on stations

      values( 1) = var(el,t)%p2(1,i)     ! WMO BLOCK NUMBER
      values( 2) = var(el,t)%p2(2,i)     ! WMO STATION NUMBER
      values( 7) = var(el,t)%p2(3,i)     ! LATITUDE (HIGH ACCURACY)
      values( 8) = var(el,t)%p2(4,i)     ! LONGITUDE (HIGH ACCURACY)
      values( 9) = var(el,t)%p2(5,i)     ! HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL (SEE NOTE 3)
      values(10) = var(el,t)%p2(9,i)     ! PRESSURE REDUCED TO MEAN SEA LEVEL
      values(11) = var(el,t)%p2(10,i)    ! TEMPERATURE AT 2M
      values(12) = var(el,t)%p2(11,i)    ! DEW POINT TEMPERATURE AT 2M
      ! IF ( KSEC1(12) ==6 .OR. KSEC1(12) ==18 ) THEN
      values(13) = var(el,t)%p2(30,i)  ! TOTAL PRECIPITATION PAST 12 HOURS
      ! ENDIF
      values(14) = var(el,t)%p2(29,i)    ! TOTAL PRECIPITATION PAST  6 HOURS
      values(15) = var(el,t)%p2(18,i)    ! CLOUD COVER (TOTAL)
      values(16) = var(el,t)%p2(12,i)    ! WIND DIRECTION AT 10 M
      values(17) = var(el,t)%p2(13,i)    ! WIND SPEED AT 10 M
      values(18) = rvind                 ! 
      values(19) = var(el,t)%p2(31,i)    ! TOTAL PRECIPITATION PAST 24 HOURS

      KBUFL=3000
      KPMISS=1
      KPRUS=1
      NOKEY=0
      CALL BUPRQ(KPMISS,KPRUS,NOKEY)
!
      KERR=0
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,           &
                   KTDLEN,KTDLST,KDLENG,KDATA,KELEM,        &
                   KVALS,VALUES,CVALS,KBUFL,KBUFR,KERR)


      IF(KERR.GT.0) THEN
         CALL EXIT(2)
      ELSEIF(KERR.lt.0) THEN
         print*,'Encoding return_code=',kerr
      END IF 
!
!     ILEN=KBUFL*JBPW/8
      ILEN=KSEC0(2)
!
      IERR=0
      CALL PBWRITE(IUNIT1,KBUFR,ILEN,IERR)
      IF(IERR.LT.0) THEN
         PRINT*,'ERROR WRITING INTO TARGET FILE.'
         CALL EXIT(2)
      END IF
 
    IF (LP > 10 )CALL unpack_bufr()
 
    ENDDO

ENDSUBROUTINE encode_bufr
