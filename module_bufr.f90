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
MODULE routine4BUFR

      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y), INTEGER(I,K,N)
      INTEGER, PARAMETER :: JSUP =  9,JSEC0=   3,JSEC1= 40,JSEC2=4096,JSEC3=4, &
                JSEC4=2,JELEM=320000,JSUBS=400,JCVAL=150 ,JBUFL=512000,     &
                JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT= 200,     &
                JWORK=4096000,JKEY=46

!
      INTEGER, PARAMETER :: KDLEN=200,KELEM=4000
      INTEGER, parameter :: KVALS=4000,KVALS1=4000
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

CONTAINS

SUBROUTINE unpack_bufr()
    !     -----------------------------------------------------------------
!*          7. UNPACK MESSAGE.
!              -------------

      VALUE(1:KVALS1)=RVIND

!
      CALL BUFREX(KBUFL,KBUFR,ISUP,ISEC0 ,ISEC1,ISEC2 ,ISEC3 ,ISEC4,     &
                  KELEM,CNAME,CUNIT,KVALS1,VALUE,CVAL,IERR)
!
      IF(IERR.NE.0) CALL EXIT(2)
!
      CALL BUPRS0(ISEC0)                                            ! PRINT SECTION 0 OF BUFR MESSAGE
      CALL BUPRS1(ISEC1)                                            ! PRINT SECTION 1 OF BUFR MESSAGE
      CALL BUUKEY(ISEC1,ISEC2,KEY,ISUP,KERR)                        ! EXPANDS LOCAL ECMWF INFORMATION FROM SECTION 2
      CALL BUPRS2(ISUP ,KEY)                                        ! PRINT SECTION 2 OF BUFR MESSAGE (EXPANDED RDB KEY)
      ISUBSET=1
      CALL BUSEL2(ISUBSET,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,      &
                  CUNITS,IERR)                                      ! RETURNS LIST OF DATA DESCRIPTORS AS IN SECTION 3
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
      CALL BUPRT(ICODE,IST,IEND,KELEM,CNAME,CUNIT,CVAL,               &
                 KVALS1,VALUE,ISUP,ISEC1,IERR)                      ! PRINT EXPANDED BUFR MESSAGE
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
      RETURN
!      
 800  CONTINUE
!
      IF(IERR.EQ.-1) THEN
         print*,'Number of records processed ',IREP
      ELSE
         print*,' BUFR : error= ',ierr
      END IF
      
ENDSUBROUTINE unpack_bufr

ENDMODULE routine4BUFR