      PROGRAM AERMET
C=======================================================================
C            MAIN Module of the AERMET Meteorological Processor
C                           Version Dated 11059
C 
C                            February 28, 2011
C
C               *** SEE AERMET MODEL CHANGE BULLETIN MCB#2 ***
C
C       ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS (SCRAM) WEBSITE
C
C                      http://www.epa.gov/scram001/
C
C=======================================================================
C
C       This revised version of AERMOD (dated 11059) includes the
C       following modifications relative to the previous version
C       (dated 06341); see MCB#2 and updated User's Guide Addendum:
C
C                                                                        
C-- Bug Fixes:                                                           
C
C   1.  Corrected several problems associated with the processing of
C       subhourly ONSITE meteorological data, including:
C       
C       a) Modified the procedure for calculating hourly averages
C          to use the "hour-ending" convention in accordance with
C          Section 7.3 of "Meteorological Monitoring Guidance for 
C          Regulatory Modeling Applications," EPA-454/R-99-005, 
C          February 2000; previous versions used the "hour-beginning"
C          convention.
C          
C       b) Corrected the procedure for calculating hourly averages 
C          from subhourly values for sigma-theta.  Previous versions
C          used an equation in Section 5.2 of the AERMET User's Guide,
C          but this has been replaced with the equation referenced in
C          Table 8-3 of Appendix W. The equation from Section 5.2 of 
C          the AERMET User's Guide was also coded incorrectly in 
C          previous versions of AERMET, which resulted in runtime
C          errors in some cases due to taking a square root of a 
C          negative number.
C          
C       c) Corrected the processing of subhourly wind data to properly
C          code hours as calm when more than half of the samples for 
C          the hour are non-missing, but below the wind threshold.  
C          Previous versions coded these hours as missing rather than 
C          calm.
C          
C       d) Addressed several issues associated with the extraction of 
C          ONSITE data that could result in erroneous values being 
C          assigned to ONSITE variables, and also improved error 
C          handling and reporting for the processing of ONSITE data.
C         
C       e) Corrected problems associated with processing of ONSITE
C          precipitation data, including the fact that subhourly 
C          precipitation values were averaged rather than summed 
C          to determine the hourly value, and that negative values
C          were also included in the summed value (before averaging)
C          if the missing data code was not properly specified.
C   
C   2.  The implementation of the SUBNWS option was corrected to 
C       avoid using the BULKRN option using ONSITE delta-T data when 
C       the ONSITE winds are missing and the reference winds are 
C       based on SURFACE data. Vertical rofiles of ONSITE temperature, 
C       sigma-theta, and sigma-w are also skipped if the reference
C       winds are based on SURFACE data.  These changes are intended
C       to avoid internal inconsistencies in the characterization of
C       the boundary layer that may occur with some combinations of
C       SURFACE and ONSITE data.
C
C   3.  Corrected problems with time zone adjustments for cases with
C       surface and onsite stations in different time zones.
C          
C   4.  Incorporated changes to properly handle ISHD surface files 
C       with the “problem” addressed on an interim basis by FIXISHD 
C       utility, which is no longer required.
C          
C-- Enhancements:                                                        
C
C   1.  Enhancements were made to the heirarchy for handling missing 
C       station pressure to make full use of other available data and 
C       of user-specified station elevations for SURFACE stations 
C       and ONSITE stations, if available.
C       
C   2.  Increased the maximum record length for the runstream input file 
C       from 80 to 132 characters, removed any limit on the record length
C       for ONSITE data records, increased the maximum field length for 
C       filenames from 48 to 96, and allow for use of double quotes (“) 
C       as field delimiters to support filenames with embedded spaces.
C       
C   3.  Several enhancements to provide more flexibility in selecting
C       the most appropriate upper air sounding, including a new option
C       for a user-specified window on upper air observation time, and
C       an option for AERMET to automatically select the most appropriate 
C       sounding based on local sunrise. These enhancements allow AERMET 
C       to better utilize upper air data that occurs more frequently 
C       than twice-daily, including "pseudo" upper air data derived from 
C       gridded meteorological models or reanalysis data, if deemed to 
C       be appropriate in the future (note that use of gridded model 
C       outputs or reanalysis data in AERMET would be considered as 
C       an alternative modeling technique at the present time).  These 
C       enhancements also provide better support for applications of 
C       AERMOD beyond the U.S.
C        
C   4.  Enhancement to allow the use of hourly-averaged winds derived 
C       from 1-minute ASOS wind data (TD-6405), generated by the new
C       AERMINUTE program, which can improve the representativeness 
C       of airport wind data by providing hourly averages as well as
C       filling in for many hours that are missing in the standard
C       airport data archives due to calm or variable winds.
C       
C   5.  Adjustment of ASOS-based wind speeds (including winds derived 
C       from 1-minute ASOS data) by +0.5 knot to account for the bias 
C       in reported ASOS wind speeds due to winds being truncated 
C       (rather than rounded) to whole knots.  This includes changes
C       to several subroutines to allow tracking whether surface data 
C       are ASOS or observer-based across all three stages of data 
C       processing, and the inclusion of an ASOS 'flag' in the surface
C       extraction and surface QA files.  A database of ASOS commission
C       dates has been included to allow AERMET to determine whether 
C       observations for a specific surface station are ASOS or not. 
C       The surface output file format has also been modified to include 
C       a character string for each hour identifying whether the wind 
C       speeds have been adjusted or not to account for the truncation 
C       of the ASOS winds ('ADJ-' or 'NAD-') and whether the reference 
C       wind data is based on ONSITE, SURFACE or 1-MIN ASOS data (OS, 
C       SFC, or A1).
C       
C   6.  Several enhancements to the error handling and reporting related 
C       to processing ONSITE data, including an option to use ‘FREE’ 
C       format to read the data and the option to specify missing data 
C       codes and upper/lower bounds for ONSITE data as REAL variables.
C       
C   7.  A new option/requirement incorporated in Stage 3 to specify a 
C       secondary set of surface characteristics for use when NWS winds 
C       are substituted for missing on-site winds using the SUBNWS option
C       on the METPREP pathway.  The primary set of surface characteristics 
C       is used when the reference wind is based on ONSITE data, and the 
C       secondary surface characteristics are used when SURFACE data are 
C       substituted for missing ONSITE data.  This ensures that an 
C       appropriate surface roughness will be used depending on the 
C       location of the reference wind data.
C       
C   8.  New keywords, AERSURF and AERSURF2, were added to the METPREP 
C       pathway to allow users to specify an external file to read
C       for surface characteristics, such as an AERSURFACE output file,
C       rather than entering the data directly in the Stage 3 input
C       file.
C       
C   9.  Enhancements to utilize on-site precipitation and relative 
C       humidity data, if available, in the surface output file.
C
C-- Miscellaneous:                                                       
C                                                                        
C   1.  The LOCATION keyword is no longer supported on the METPREP 
C       pathway in Stage 3.  The METPREP LOCATION keyword had been used
C       as the location for determining sunrise for convective mixing
C       height calculations.  This could result in somewhat different
C       results using the same raw input data depending on the METPREP
C       LOCATION, which are not meaningful.  A non-fatal warning message
C       is issued if the METPREP LOCATION keyword is found, and the
C       location used to determine sunrise for convective mixing height
C       calculations is based on the primary surface station location,
C       i.e., the ONSITE station if available, or the SURFACE (NWS) 
C       station location.
C       
C   2.  Modified several subroutines to adjust the lower limit on the
C       value of surface roughness length to 0.0001 meters, rather than
C       0.001 meters.  This allows more flexibility for applications of
C       AERMET for overwater or other very low roughness settings.
C       
C   3.  Several subroutines were modified to improve error handling and
C       reporting for all three processing stages, including adjustments 
C       and improvements to the order and formatting of information in 
C       the report file.
C       
C   4.  Implemented some restrictions on the use of surface (NWS) data 
C       outside the valid range of dates based on the surface data format,
C       i.e., data that have been reformatted from another file format.
C       These restrictions affect cloud cover for ASOS data in the SCRAM 
C       or SAMSON format, since the SAMSON CDs and nearly all SCRAM surface 
C       data predated the advent of ASOS in 1992.  The ASOS cloud covers 
C       are set to missing for any SURFACE data input in the SCRAM or 
C       SAMSON format, since the interpretation and formatting of ASOS
C       cloud cover codes cannot be verified in these cases.
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       February 28, 2011
C
C-----  MODIFIED FROM:          AERMOD
C                       (Version Dated 06341)
C
C=======================================================================
C                                 AERMET
C        Meteorological Preprocessor for the AERMOD Dispersion Model
C
C     PURPOSE:  Controls the overall flow of AERMET processing through
C               Stages 1, 2 and 3.
C
C               There are four data types that can be processed
C               through AERMET:
C
C                  UPPERAIR - Upper air soundings
C
C                  SURFACE  - Hourly surface weather observations
C
C                  ASOS1MIN - Hourly-averaged winds derived from 
C                             1-minute ASOS data (TD-6405), processed
C                             by the AERMINUTE processor
C
C                  ONSITE   - User supplied on-site meteorological data
C
C                In Stage 1, the UPPERAIR and SURFACE data are extracted
C                from a standard archive-formatted file and the quality
C                of the data are assessed; also in this stage, the
C                quality of the ONSITE data are assessed, if provided
C                by the user. There is no standard archive format for 
C                on-site data, hence, there is no data extraction.
C
C                In Stage 2, the data input in Stage 1 are combined, 
C                or merged, into 24-hour blocks of data.
C
C                In Stage 3, the merged data from Stage 2 are processed
C                to determine boundary layer scaling parameters, compute
C                hourly mixing heights, and output hourly data formatted
C                for input to the AERMOD dispersion model.  Stage 3 output 
c                consists of a "surface" file, with scaling paramters,
C                reference wind and temperature data, hourly mixing 
C                heights, and other parameters that may be used with 
C                the deposition algorithms in AERMOD; and a "profile"
C                file, with vertical profiles of wind, temperature, and 
C                turbulence data collected from an onsite tower, if
C                available.     
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        10/18/96
C          - added the write statement to display processing setup and
C            processing completed messages
C
C        06/27/99
C          - added capability to process upper air data retrieved from
C            Radiosonde Data of North America compact disc (aka "FSL
C            data")
C
C        10/26/06
C          - A single executable (AERMET) was created, replacing 
C            STAGE1N2 and STAGE3.
C
C        05/29/08: MACTEC Federal Programs
C          - Commented out opening unit 3 (the console or screen) all 
C            references to unit 3 in the code have been changed to 
C            * (default output)
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'


      CHARACTER*96 INPFILE
      INTEGER  :: ISTAT, ISTAGE, DEVICE

c     WRITE(6, *)'Enter the name of the input file:     '               ! ! dtb001 01085
c     WRITE(6, *)                                                       ! ! dtb001 01085
c     READ(5, '(A96)') INPFILE                                          ! ! dtb001 01085
c     WRITE(6, *)'The input file is:  ', INPFILE                        ! ! dtb001 01085

c     OPEN (DEVIN, FILE=INPFILE, STATUS='OLD', ERR=900)                 ! ! dtb001 01085

      INPFILE = 'AERMET.INP'
      
      OPEN (DEVIN, FILE=INPFILE, STATUS='OLD', ERR=900)

C---- Open the console/screen for model output displays
C      OPEN( UNIT=3, FILE='CON', CARRIAGECONTROL='FORTRAN')

C---- Display version of AERMET that is being run
      WRITE( *,1 ) VERSNO
    1 FORMAT('   AERMET Version', A6 )


      oslon = '        '
      oslat = '        '
      ISTAGE = 0
      PGNUM  = 0
      STATUS = 0
      OSDNUM = 0
      OSHTPREV = 0
      L_HgtsVary = .FALSE.
      
C---- Call PRESET to pre-process runstream input file ('aermet.inp')
C     to determine which stage of processing is being performed
C     based on pathways included.
      CALL PRESET( ISTAGE )

C---- Process user supplied set up data.
      CALL SETUP( ISTAGE )

C---- Assign processing stage:
      IF( STABLK(5) .EQ. 2 )THEN
C        MERGE Pathway in Stage 2
         ISTAGE = 2
      ELSEIF( STABLK(6) .EQ. 2 )THEN
C        METPREP Pathway in Stage 3
         ISTAGE = 3
      ELSEIF( STABLK(2) .EQ. 2 .OR. STABLK(3) .EQ. 2 .OR.
     &        STABLK(4) .EQ. 2 )THEN
C        UPPERAIR, SURFACE, or ONSITE Pathway in Stage 1
         ISTAGE = 1
      ENDIF

C---- Call status summary, SUMRY1
C         This routine is called before any data processing because
C         all pathway statuses are set at this point.  Errors in data
C         processing may change the status and, hence, the original
C         intent for data processing is lost
      CALL SUMRY1( ISTAGE )

C---- Test status words (all must be ok, or we skip processing)

      IF( JBSTAT.LT.0 .OR. UASTAT.LT.0 .OR.
     &    SFSTAT.LT.0 .OR. OSSTAT.LT.0 .OR. 
     &    MRSTAT.LT.0 .OR. MPSTAT.LT.0 )THEN
         GO TO 10
      ENDIF

      IF( .NOT. SETERR .AND. STABLK(6) .NE. 2 )THEN
C------- Perform STAGE1 or STAGE2 processing

C------- Rewind the temporary file (DEV70) for later use

         REWIND DEV70

C------  UPPERAIR data pathway - extract and/or QA.

         IF( STABLK(2) .EQ. 2  .AND.  JBSTAT .NE. -1 )THEN
            CALL UAPATH
            ISTAGE = 1
         ENDIF

C------- SURFACE data pathway - extract and/or QA.

         IF( STABLK(3) .EQ. 2  .AND.
     &      JBSTAT .NE. -1    .AND.  UASTAT .NE. -1 )THEN
            CALL SFPATH
            ISTAGE = 1
         ENDIF

C------- ONSITE data pathway - QA.

         IF( STABLK(4) .EQ. 2  .AND.  JBSTAT .NE. -1  .AND.
     &       UASTAT .NE. -1    .AND.  SFSTAT .NE. -1 )THEN
            CALL OSPATH
            ISTAGE = 1
         ENDIF

C------- MERGE data pathway.

         IF( STABLK(5) .EQ. 2 .AND.
     &       JBSTAT .NE. -1   .AND.  UASTAT .NE. -1 .AND.
     &       SFSTAT .NE. -1   .AND.  OSSTAT .NE. -1 )THEN
            CALL MRPATH
            ISTAGE = 2
         ENDIF

C------- Close out run, generating the summary of the run.

         WRITE( *,8 )
    8    FORMAT('   Processing completed; writing summary files'/ )

C----    Call the QA audit routine if there was a QA on any pathway

         ISTAT = 0
         IF( (UASTAT .EQ. 2) .OR. (UASTAT .EQ. 3) .OR.
     &       (SFSTAT .EQ. 2) .OR. (SFSTAT .EQ. 3) .OR.
     &       (OSSTAT .EQ. 2) .OR. (OSSTAT .EQ. 3) )THEN
            CALL AUDIT( ISTAGE,ISTAT )
            IF( ISTAT.EQ.1 )THEN
               RUNERR = .TRUE.
            ENDIF
         ENDIF

         IF( STATUS(1,1).EQ.2 )THEN
            DEVICE = DEV50
         ELSE
            DEVICE = DEVIO
         ENDIF

         IF( ISTAGE .EQ. 1 .AND. OSSTAT .NE. 0 )THEN
C----       Call routine to summarize ONSITE values:
C           OBS/HOUR, calm threshold and heights for tower data
            CALL OSSMRY (DEVICE)
         ENDIF

C----    Call the summary routine (the argument defines the stage)
         CALL SUMRY2( ISTAGE )

      ELSEIF( .NOT. SETERR .AND. STATUS(1,3) .EQ. 0 )THEN
C------- Perform STAGE3 processing
C

C------- METPREP data pathway.

         IF( STABLK(6) .EQ. 2 .AND.
     &       JBSTAT .NE. -1   .AND.  UASTAT .NE. -1 .AND.
     &       SFSTAT .NE. -1   .AND.  OSSTAT .NE. -1 .AND.
     &       MRSTAT .NE. -1 )THEN

            ISTAT = 0
            WRITE( *,* ) ' '
C---------- Call MPMET to proces merged data in Stage 3
            CALL MPMET( ISTAT )

            IF( ISTAT .EQ. 1 )THEN
C              An error condition occurred in Stage 3
               MPSTAT = -1
               RUNERR = .TRUE.
            ENDIF
         ENDIF

C        Close out the run, generating such reports as needed.

         WRITE( *, 18 )
   18    FORMAT('   Processing completed; writing summary files'/ )

         CALL MPFIN( ISTAGE )

      ENDIF

C --- Branch statement to skip processing when SETUP errors are found
   10 CONTINUE
   
      IF( STATUS(1,1) .EQ. 2 )THEN
C ---    Issue message regarding the Summary Report file
         WRITE(DEVIO,5060) DISK50
      ENDIF

      IF( SETERR .AND. ISTAGE .EQ. 3 )THEN
C ---    Call MPFIN to get the summary of inputs if 
C        setup errors occur for Stage 3
         WRITE( *, 18 )
         CALL MPFIN( ISTAGE )

      ELSEIF( SETERR .AND. (ISTAGE.EQ.1 .OR. ISTAGE.EQ.2) )THEN
C ---    Call SUMRY2 to get the summary of inputs if
C        setup errors occur for Stages 1 or 2
         CALL SUMRY2( ISTAGE )

      ENDIF

C --- Close and delete temporary files
      close(unit=dev70,status="delete")
      close(unit=dev75,status="delete")

      GO TO 910
      
  900 WRITE(6, '(A12)')' Error opening:  ', INPFILE
  
  910 CONTINUE

      STOP

5060  FORMAT(/3X,'The Summary Report Generated by AERMET Is In: ',
     &         /5X,A96/)

      END


      BLOCK DATA

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'BLOCK1.INC'
      INCLUDE 'BLOCK2.INC'

      END

