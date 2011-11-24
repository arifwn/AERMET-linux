      SUBROUTINE DOCLDS(ASOS1,ASOS2,ASOS3,ICCVR,ICEIL)
C***********************************************************************
C*    DOCLDS Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Decodes the ASOS Cloud Data
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       May 1998
C*
C*    INPUTS:     3 levels of ASOS cloud data of the form XXYYY
C*                where XX = cloud coverage, YYY = ceiling in hundreds
C*                of feet
C*
C*    OUTPUTS:    1 Hour Numeric Value for Ceiling and Cloud Cover
C*
C*    CALLED FROM: FILMET
C***********************************************************************
C*
C*    Variable Declarations

      IMPLICIT NONE
      
      INTEGER  ACC1,ACC2,ACC3,ACHT1,ACHT2,ACHT3,ASOS1,ASOS2,ASOS3
      INTEGER  MAXCLD, ICCVR, ICEIL, UNKNOWN

      MAXCLD  = 99
      ICCVR   = 99
      ICEIL   = 77777
      UNKNOWN = 99999        ! Unknown flag for TD-3280 data is 09999


c     First process the ceiling height

      IF( ASOS1 .NE. UNKNOWN )THEN
         ACC1  = INT(ASOS1/1000.0)
         ACHT1 = ASOS1 - ACC1*1000
         MAXCLD = ACC1
         IF( MAXCLD .GT. 02 )THEN
            ICEIL = ACHT1
         ENDIF
      ENDIF

      IF( ASOS2 .NE. UNKNOWN )THEN
         ACC2  = INT(ASOS2/1000.0)
         ACHT2 = ASOS2 - ACC2*1000
         IF( ACC2 .NE. 99  .AND.  ACC2 .GT. MAXCLD )THEN
            MAXCLD = ACC2
            IF( MAXCLD .GT. 02 .AND. ICEIL .NE. 77777 )THEN
               ICEIL = ACHT2
            ENDIF
         ENDIF

      ENDIF

      IF( ASOS3 .NE. UNKNOWN )THEN
         ACC3  = INT(ASOS3/1000.0)
         ACHT3 = ASOS3 - ACC3*1000
         IF( ACC3 .NE. 99  .AND.  ACC3 .GT. MAXCLD )THEN
            MAXCLD = ACC3
            IF( MAXCLD .GT. 02 .AND. ICEIL .NE. 77777 )THEN
               ICEIL = ACHT3
            ENDIF
         ENDIF
      ENDIF


C---- Convert the ASOS sky condition codes to  fractional cloud cover
C     (tenths).  Note that the VALID codes are 0, 2, 4, 6, 7, 9, and 99.
C     The remaining codes (1, 3, 5, 8, and 10) are not valid according to
C     the HUSWO documentation, but are allowed to pass through as 1/10,
C     3/10, etc. (per conversation with Dennis Atkinson, USEPA, 10/30/98).
C     These codes _may_ be augmented by human observers or errors, but 
C     there is no way to determine this.

      IF( MAXCLD .EQ. 0 )THEN                         ! Clear
C*       A valid ASOS Sky Condition Value
         ICCVR = 0
      ELSEIF( MAXCLD .EQ. 01 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 1
      ELSEIF( MAXCLD .EQ. 02 )THEN
C*       A valid ASOS Sky Condition Value             ! Scattered
         ICCVR = 3
      ELSEIF( MAXCLD .EQ. 03 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 3
      ELSEIF( MAXCLD .EQ. 04 )THEN                    ! Broken
C*       A valid ASOS Sky Condition Value
         ICCVR = 7
      ELSEIF( MAXCLD .EQ. 05 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 5
      ELSEIF( MAXCLD .EQ. 06 )THEN                    ! Overcast
C*       A valid ASOS Sky Condition Value
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 07 )THEN
C*       A valid ASOS Sky Condition Value             ! Obstruction
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 08 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 8
      ELSEIF( MAXCLD .EQ. 09 )THEN                    ! Unknown
C*       A valid ASOS Sky Condition Value
         ICCVR = 99
C        WRITE( IDIAG, 500 ) IY, IM, ID, IH
      ELSEIF( MAXCLD .EQ. 10 )THEN                    ! Overcast
C*       A valid ASOS Sky Condition Value
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 99 )THEN
         ICCVR = 99                                   ! Missing
C        WRITE( IDIAG, 510 ) IY, IM, ID, IH

      ENDIF

C 500 FORMAT( ' DOCLDS: ASOS cloud cover unknown for (yy/mm/dd/hh) ',
C    &         4(I3.2:,'/') )
C 510 FORMAT( ' DOCLDS: ASOS cloud data missing for (yy/mm/dd/hh) ',
C    &         4(I3.2:,'/') )


      RETURN
      END

