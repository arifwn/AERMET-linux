        SUBROUTINE READRL (FIELD,LEN,VAR,ISTAT)
C=====================================================================**
C
C     Purpose:  To read a real value into VAR from CHAR*LEN field
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      CHARACTER*(*) FIELD
      CHARACTER*8 RDFMT
      INTEGER LEN,ISTAT,DCPT,DEC
      REAL VAR
C
      DCPT = INDEX(FIELD,'.')
      IF( DCPT.EQ.0 ) DCPT = LEN
      DEC = LEN - DCPT
      WRITE( RDFMT,10 ) LEN,DEC
 10   FORMAT ('(F', I2, '.', I2, ')')
      READ( FIELD,FMT=RDFMT, IOSTAT=ISTAT) VAR
      RETURN
      END

