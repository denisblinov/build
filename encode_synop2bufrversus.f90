! Copyright 1981-2012 ECMWF. 
!
! This software is licensed under the terms of the GNU Lesser 
! General Public License Version 3 which can be obtained at 
! http://www.gnu.org/licenses/lgpl.html.  
! 
! In applying this licence, ECMWF does not waive the privileges 
! and immunities granted to it by virtue of its status as an 
! intergovernmental organisation nor does it submit to any
! jurisdiction. 
! 

       PROGRAM BUFR2VERSUS
!
!**** *BUFR*
!
!
!     PURPOSE.
!     --------
!         An example of using Bufr packing/unpacking software.
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
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
!
!
      PARAMETER(JSUP =  9,JSEC0=   3,JSEC1= 40,JSEC2=4096,JSEC3=    4,
     1          JSEC4=2,JELEM=320000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
     2          JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=4096000,JKEY=46)

!
      PARAMETER (KDLEN=200,KELEM=4000)
      parameter (KVALS=4000,KVALS1=4000)
! 
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY)
      DIMENSION ISUP(JSUP)  ,ISEC0(JSEC0),ISEC1(JSEC1)
      DIMENSION ISEC2(JSEC2),ISEC3(JSEC3),ISEC4(JSEC4)
!
      REAL*8    VALUES(KVALS),VALUE(KVALS1)
      REAL*8    RQV(KELEM)
      REAL*8    RVIND
!
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM),KRQ(KELEM)
      DIMENSION ITDLST(KELEM),ITDEXP(KELEM)
      DIMENSION KDATA(KDLEN),IDATA(KDLEN)
!
      CHARACTER*8  CF
      CHARACTER*64 CNAMES(KELEM),CNAME(KELEM)
      CHARACTER*24 CUNITS(KELEM),CUNIT(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 CVAL (KVALS1)
      CHARACTER*80 YENC
!
!                                                                       
!     ------------------------------------------------------------------
!*          1. INITIALIZE CONSTANTS AND VARIABLES.
!              -----------------------------------
 100  CONTINUE
!
!
      RVIND=1.7D38
!

      CALL PBOPEN(IUNIT1,'synop.bufr','W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON synop.dat'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
!

!
!     INITIALIZE DELAYED REPLICATION FACTORS OR REFERENCE VALUES ETD.
!
      DO 101 I=1,KDLEN
      KDATA(I)=1
      VALUES(I)=RVIND
 101  CONTINUE
!
!
      KDLENG=3
!
!
!     SET DATA DECSRIPTORS
!


!K      ktdlst(    1)=  307080
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
      ktdlst(14)=013021     ! TOTAL PRECIPITATION
      ktdlst(15)=020010     ! CLOUD COVER (TOTAL)
      ktdlst(16)=011011     ! WIND DIRECTION AT 10 M
      ktdlst(17)=011012     ! WIND SPEED AT 10 M

      ktdlen=17

      values( 1)=27.        ! WMO BLOCK NUMBER
      values( 2)=625.       ! WMO STATION NUMBER
      values( 3)=2010.      ! YEAR
      values( 4)=3.         ! MONTH
      values( 5)=13.        ! DAY
      values( 6)=12.        ! HOUR
      values( 7)=55.133     ! LATITUDE (HIGH ACCURACY)
      values( 8)=38.733     ! LONGITUDE (HIGH ACCURACY)
      values( 9)=114.       ! HEIGHT OF STATION GROUND ABOVE MEAN SEA LEVEL (SEE NOTE 3)
      values(10)=1000.      ! PRESSURE REDUCED TO MEAN SEA LEVEL
      values(11)=300.       ! TEMPERATURE AT 2M
      values(12)=rvind      ! DEW POINT TEMPERATURE AT 2M
      values(13)=13.        ! TOTAL PRECIPITATION PAST 12 HOURS
      values(14)=5.         ! TOTAL PRECIPITATION
      values(15)=75.        ! CLOUD COVER (TOTAL)
      values(16)=90.        ! WIND DIRECTION AT 10 M
      values(17)=7.         ! WIND SPEED AT 10 M

 
!     SET CCITTIA5 STATION OR SITE NAME

      cvals(1)='MOSCOW'
!
!
!     SECTION 0 CONTENT
!
      KSEC0(1)=0      ! TOTAL LENGTH OF SECTION 0
      KSEC0(2)=0      ! TOTAL LENGTH OF BUFR MESSAGE
      KSEC0(3)=4      ! BUFR EDITION NUMBER
!
!     SECTION 1 CONTENT
!
      KSEC1(1)=22    ! TOTTAL LENGTH OF SECTION 1 (  set to 18 for edition <= 3)
      KSEC1(2)=4     ! BUFR EDITION NUMBER
!K      KSEC1(3)=89    ! ORIGINATING CENTRE
      KSEC1(3)=5    !! ORIGINATING CENTRE MOSCOW (COMMON CODE TABLE C–1: Identification of originating/generating centre)
      KSEC1(4)=1     ! UPDATE SEQUENCE NUMBER
      KSEC1(5)=0     ! FLAG (PRESENCE OF SECTION 2)
      KSEC1(6)=0     ! DATA CATEGORY
      KSEC1(7)=0     ! LOCAL DATA SUB-CATEGORY
      KSEC1(8)=0     ! VERSION NUMBER OF LOCAL TABLE USED
      KSEC1(9)=nint(values(5))   !! YEAR
      if(KSEC1(2).le.3) then
        if(ksec1(9).gt.2000) then
           ksec1(9)=ksec1(9)-2000
        else
           ksec1(9)=ksec1(9)-1900
        end if
      end if
      KSEC1(10)=nint(values(6))  !! MONTH
      KSEC1(11)=nint(values(7))   ! DAY
      KSEC1(12)=nint(values(8))   ! HOUR
      KSEC1(13)=nint(values(9))   ! MINUTE
      KSEC1(14)=0    ! BUFR MASTER TABLE( ZERO) FOR METEOROLOGICAL DATA)
      KSEC1(15)=13   ! VERSION NUMBER OF MASTER TABLE USED
      KSEC1(16)=255  ! ORIGINATING SUB-CENTRE
      KSEC1(17)=2    ! INTERNATIONAL SUB-CATEGORY
      KSEC1(18)=0    ! SECOND
      
!
!     SECTION 2 CONTENT
!
      KSEC2(1)=52
!
      DO 110 I=2,JSEC2
      KSEC2(I)=0
 110  CONTINUE
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
 600  CONTINUE
!
!---------------------------------------------------------------
!              This call is not needed for packing. It just 
!              prints expanded list corresponding to ktdlst sequence
!              and delayed replications in kdata array. This four
!              lines can be deleted or commented out.
!K      K=1
!K      CALL BUXDES(K,KSEC1,KTDLEN,KTDLST,KDLENG,KDATA,KELEM,
!K     1            KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
!
!K      IF(KERR.NE.0) CALL EXIT(2)
!---------------------------------------------------------------
!
!
!*          6.2 ENCODE DATA INTO BUFR MESSAGE.
!               ------------------------------
 620  CONTINUE
!
      KBUFL=3000
      KPMISS=1
      KPRUS=1
      NOKEY=0
      CALL BUPRQ(KPMISS,KPRUS,NOKEY)
!
      KERR=0
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST,KDLENG,KDATA,KELEM,
     2             KVALS,VALUES,CVALS,KBUFL,KBUFR,KERR)
!

      IF(KERR.GT.0) THEN
         CALL EXIT(2)
      ELSEIF(KERR.lt.0) then
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

!
!     -----------------------------------------------------------------
!*          7. UNPACK MESSAGE.
!              -------------
 700  CONTINUE
!
      DO 702 I=1,KVALS1
      VALUE(I)=RVIND
 702  CONTINUE
!
 701  CONTINUE
!
      CALL BUFREX(KBUFL,KBUFR,ISUP,ISEC0 ,ISEC1,ISEC2 ,ISEC3 ,ISEC4,
     1            KELEM,CNAME,CUNIT,KVALS1,VALUE,CVAL,IERR)
!
      IF(IERR.NE.0) CALL EXIT(2)
!
      CALL BUPRS0(ISEC0)                                            ! PRINT SECTION 0 OF BUFR MESSAGE
      CALL BUPRS1(ISEC1)                                            ! PRINT SECTION 1 OF BUFR MESSAGE
      CALL BUUKEY(ISEC1,ISEC2,KEY,ISUP,KERR)                        ! EXPANDS LOCAL ECMWF INFORMATION FROM SECTION 2
      CALL BUPRS2(ISUP ,KEY)                                        ! PRINT SECTION 2 OF BUFR MESSAGE (EXPANDED RDB KEY)
      ISUBSET=1
      CALL BUSEL2(ISUBSET,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,
     1            CUNITS,IERR)                                      ! RETURNS LIST OF DATA DESCRIPTORS AS IN SECTION 3
      CALL BUPRS3(ISEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KELEM,CNAME)    ! PRINT SECTION 3 OF BUFR MESSAGE
!
!K      WRITE(*,'(a,$)') ' STARTING SUBSET TO BE PRINTED : '
!K      READ(*,'(I5)')   IST
!K      WRITE(*,'(a,$)') ' ENDING SUBSET TO BE PRINTED : '
!K      READ(*,'(I6)')   IEND
      IST=1
      IEND=1
!
      ICODE=0
      CALL BUPRT(ICODE,IST,IEND,KELEM,CNAME,CUNIT,CVAL,
     1           KVALS1,VALUE,ISUP,ISEC1,IERR)                      ! PRINT EXPANDED BUFR MESSAGE
!
!      
      IREP=IREP+1
!   
!K      IF(IREP.GT.3) GO TO 900 
!K      GO TO 900
!
 810  CONTINUE
!
      WRITE(*,'(1H ,A)') 'OPEN ERROR ON INPUT FILE'
      GO TO 900
!      
 800  CONTINUE
!
      IF(IERR.EQ.-1) THEN
         print*,'Number of records processed ',IREP
      ELSE
         print*,' BUFR : error= ',ierr
      END IF
!
 900  CONTINUE
!
      STOP
      END
