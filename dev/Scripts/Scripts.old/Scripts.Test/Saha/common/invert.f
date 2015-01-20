C
C          *****    INVERT.FOR     *****
C
C          Written by:   Shawn E. Griffith
C	   Modified by:   Charlotte F. Fischer
C	   Date:          February, 1984
C	   Created for:   VAX/11 system 
C

C
C	Variable list
C
C     INT   =  Intergral type
C     K     =  K number
C     I     =  Matrix row index
C     J     =  Matrix column index
C     COE   =  Coefficent
C     NOV1-2= Number of overlaps
C     POW1  =  Power P first overlap integral
C     POW2  =  Power P second overlap integral
C     EL1-4 =  Electrons 1,2,3 and 4
C     TEMP  =  Temporary character variable
C     STR1  =  Array that contains electrons and K number for sorting
C     STR2  =  Contains rest data associated with the integral in STR1
C     HEAD  =  File heading for FGR.LST and INV.LST
C     TIME  =  Real variable used to compute sort time.
C
C     >>>>>>>  STR1 and STR2 are dimensioned to  8000  elements <<<<<<<<
C

C
C   Variable Declaration
C
	REAL TIME,SECS
	INTEGER     L,MINUTS
	CHARACTER*3 EL1,EL2,EL3,EL4,TEMP,I,J,K*2,COE*14,INT*1,HEAD*72,
     :            NAME*26,ANS*1
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR
C
C
C
C...................***   Executions begins here    ***.......................
C
	NAME = 'FGR.LST'
	NINT = 4
	PRINT '(/,A)', ' Is this an FGR.LST (Y/N) ? '
	READ '(A1)', ANS
	IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') THEN
	   NAME = 'BREIT.LST'
	   NINT = 8
	END IF

C    Opens input(FGR.LST) and output(INV.LST) files
C
	OPEN(UNIT=21,FILE=NAME,STATUS='OLD')
	OPEN(UNIT=22,FILE='INV.LST',STATUS='NEW')
	IF (NINT.EQ.8) OPEN(UNIT=23,FILE='LSJPTR.LST',STATUS='NEW')
C
C    Read heading from FGR.LST and write it to INV.LST.
C

	READ(21,5) HEAD
	WRITE(22,5) HEAD
  5	FORMAT(A72)

	NOV1 = 0
	NOV2 = 0
	ITOTAL = 0
	IINT = 0

	DO 10 INTCNT = 1,NINT
	   TIME = 0.0
	   COUNT = 1
	   IF (INTCNT .LE. 2 .OR. INTCNT .EQ. 5) THEN
		CALL INVFGZ
	   ELSE IF (INTCNT .EQ. 3) THEN
		CALL READR
		CALL SORT
		COUNTR = COUNT
		DO 20 II = 1,COUNT
		   STOR1(II) = STR1(II)
		   STOR2(II) = STR2(II)
		   LASTR(II) = LAST(II)
		   ORDERR(II) = ORDER(II)
 20		CONTINUE
	   ELSE IF (INTCNT .EQ. 4) THEN
		CALL READL
		CALL SORT
		CALL OVLAPS
		CALL UNIQUR
		CALL UNIQUE
	   ELSE
		CALL READR
		CALL SORT
		CALL UNIQUE
	   END IF
	   ITOTAL = ITOTAL + COUNT
	   TIME=0.0
	   MINUTS = TIME/60.
	   SECS = TIME - MINUTS*60.
	   WRITE(6,30) COUNT,STR2(1)(15:15),MINUTS,SECS
 30	   FORMAT(1X,'*** ',I5,1X,A2,' integrals completed in ',I2,
     :		' minutes and ',F6.3,' seconds ***')
 10	CONTINUE
	WRITE(6,'(2(A,1X,I5/))') ' The total number of terms is ',
     :		ITOTAL,' The total number of integrals is ',IINT
	END

C..........................................................................
C
C
C    This section reads data from FGR.LST, puts the data in
C   canonical form for sorting then stores it in the arrays
C   STR1 and STR2.  An asterisk '*' read into INT signals the
C   end of an integral list.
C
	SUBROUTINE INVFGZ


	INTEGER L,OPTR
	CHARACTER*3 EL1,EL2,EL3,EL4,TEMP,I,J,K*2,COE*14,INT*1
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR


	K = '  '

 10	FORMAT (A14,A1,A2,2(1X,A3,A3))
 11	FORMAT(A14,A1,2(1X,A3,A3))
100	IF (INTCNT .NE. 5) THEN
	    READ(21,10) COE,INT,K,EL1,I,EL2,J
        ELSE
            READ(21,11) COE,INT,EL1,I,EL2,J
 	END IF

	IF (INT.EQ.'*') THEN
	   COUNT=COUNT-1
	   GOTO 200
	ENDIF

	IF (EL1.GT.EL2) THEN
	   TEMP=EL2
	   EL2=EL1
	   EL1=TEMP
	ENDIF

	IF (I.LT.J) THEN
	    TEMP = I
	    I = J
	    J = TEMP
	ENDIF


	STR1(COUNT)=EL1//EL2//K
	STR2(COUNT)=COE//INT//I//J
	COUNT=COUNT+1
	GOTO 100

C
C     Program branches to here after all data for one integral list is read
C   The data is first passed to a sort routine(SORT) and then to another
C   routine(UNIQUE) to create a list of unique integrals with a pointer to
C   the corresponding data.  The integral list will be output with the
C   data list for that integral following.
C     The two arrays STR1 and STR2 and the integer COUNT are in common for
C   use in the the two subroutines SORT and UNIQUE.
C     After UNIQUE has returned a check is made to see if all integral lists
C   have been read and sorted.  If they have not been, COUNT is reset and
C   the program goes to the read section.
C
200	TIME=0.0
	CALL SORT
	CALL UNIQUE

	END

C..........................................................................
C
C    This subroutine reads R data from FGR.LST, puts it in
C   canonical form for sorting then stores it in the arrays
C   STR1 and STR2.  An asterisk '*' read into INT signals the
C   end of an integral list.
C
	SUBROUTINE READR


	INTEGER L,OPTR
	CHARACTER*3 EL1,EL2,EL3,EL4,TEMP,I,J,K*2,COE*14,INT*1,HEAD*30,
     :            OV1,OV2,POW1*2,STR*8,EL(4),OV3,OV4,POW2*2
	EQUIVALENCE (EL(1),EL1),(EL(2),EL2),(EL(3),EL3),(EL(4),EL4)

      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR


 10	FORMAT (A14,A1,A2,2(1X,3(A3)),1X,2(1X,A3,1X,A3,1X,A2:))
100   	READ(21,10) COE,INT,K,EL1,EL2,I,EL3,EL4,J,OV1,OV2,POW1,
     :		OV3,OV4,POW2

	IF (INT.EQ.'*') THEN
	   COUNT=COUNT-1
	   RETURN
	ENDIF
	IF (COUNT .GT. (5000))
     :	   STOP ' Too many integrals: MAX = (5000)'

	IF (INTCNT .EQ. 3) THEN
	   IF ((EL1.EQ.EL2) .AND. (EL3.GT.EL4))  THEN
		TEMP = EL3
		EL3 = EL4
		EL4 = TEMP
	   ELSE IF ((EL3.EQ.EL4) .AND. (EL1.GT.EL2)) THEN
		TEMP = EL1
		EL1 = EL2
		EL2 = TEMP
	   END IF

*	Find position of the lowest electron.  To preserve phases,
*    we will not interchange the LEFT/RIGHT position of a single
*    electron.

	   II = 1
	   DO 20 JJ = 2,4
		IF (EL(JJ) .LT. EL(II)) II = JJ
 20	   CONTINUE
	   GO TO (21,22,23,24) II
 21	   STR1(COUNT) = EL1//EL2//EL3//EL4//K
	   GO TO 30

 22	   STR1(COUNT) = EL2//EL1//EL4//EL3//K
	   GO TO 30

 23	   STR1(COUNT) = EL3//EL4//EL1//EL2//K
	   GO TO 30

 24	   STR1(COUNT) = EL4//EL3//EL2//EL1//K

	ELSE IF (INTCNT .EQ. 7) THEN
	   IF (EL2.GT.EL4) THEN
		TEMP = EL2
		EL2 = EL4
		EL4 = TEMP
	   END IF
	   STR1(COUNT) = EL1//EL2//EL3//EL4//K
	ELSE
	   IF (EL1.GT.EL3) THEN
		TEMP = EL1
		EL1 = EL3
		EL3 = EL1
	   END IF
	   IF (EL2.GT.EL4) THEN
		TEMP = EL2
		EL2 = EL4
		EL4 = TEMP
	   END IF

	   STR1(COUNT) = EL1//EL2//EL3//EL4//K
	END IF

 30	STR2(COUNT) = COE//INT//I//J

	IF (INTCNT.EQ.3) THEN
C
C          Set pointer value
C
	   CALL FINDOV(OV1,OV2,POW1,OV3,OV4,POW2,OPTR)

	   WRITE(STR2(COUNT)(22:26),'(I5)') OPTR
	END IF

	COUNT=COUNT+1
	GOTO 100

	END
C
C..............................................................................
C
C	This subroutine's purpose is to sort the data input from FGR.LST
C    after it has been put in canonical form.  The method used to sort the
C    data is the diminishing increment or Shell sort.
C
C

	SUBROUTINE SORT

C
C     Variable declartion
C
	INTEGER K,L,M,N,INC
	CHARACTER*26 TEMP
      LOGICAL DONE(5000)
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR

C
C	Initialize arrays for ordered ouput
C
	DO 1 I = 1,COUNT
	   DONE(I) = .FALSE.
	   LAST(I) = 0
	   ORDER(I) = 0
  1     CONTINUE
C
C	Determine LAST for unique integrals along with ORDER
C
	NEXT = 1
	DO 2 I = 1,COUNT
	   IF (.NOT. DONE(I)) THEN
	      ORDER(NEXT) = I
	      NEXT = NEXT + 1
	      DONE(I) = .TRUE.
	      DO 3 J = I+1,COUNT
		 IF (STR1(J) .EQ. STR1(I)) THEN
		    ORDER(NEXT) = J
		    NEXT = NEXT + 1
		    DONE(J) = .TRUE.
		 END IF
  3	      CONTINUE
	      LAST(I) = NEXT-1
	   END IF
  2	CONTINUE

	RETURN
	END

C...............................................................................
C
C    This subroutine is used to create a unique list of integrals and pointers
C  to the data that generated the integral.  The input is from the arrays STR1
C  and STR2.  Output is the disk file  "INV.LST" with the list of integrals and
C  pointers followed by the data that produced the integrals.
C

	SUBROUTINE UNIQUE

C
C  Variable declaration
C
	INTEGER I
	CHARACTER*3 EL1,EL2,EL3,EL4,K*2,PREV*14,INT*1
      CHARACTER*1 INTV(1:8)
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR
C
      DATA (INTV(I), I=1,8) / 'F','G','R','L','Z','N','V','S' /
C
C     Begin Unique operation.  Set INT equal to its appropriate value
C   dependent upon the value of INTCNT.
C

	INT = INTV(INTCNT)

 10	FORMAT(' ',A1,A2,'(',A3,',',A3,')',I5)
 12	FORMAT(' ',A1,A2,'(',2A3,',',2A3,')',I5)

	DO 150 I=1,COUNT
	   IF (LAST(I) .NE. 0) THEN
		IINT = IINT + 1
		EL1=STR1(I)(1:3)
		EL2=STR1(I)(4:6)
		IF (INTCNT.NE.5 .AND. INTCNT.NE.4) THEN
		   IF (INTCNT.LE.2) THEN
			K=STR1(I)(7:8)
		   	WRITE(22,10) INT,K,EL1,EL2,LAST(I)
		   ELSE IF (INTCNT.GT.5) THEN
			EL3=STR1(I)(7:9)
			EL4=STR1(I)(10:12)
			K=STR1(I)(13:14)
			WRITE(22,12) INT,K,EL1,EL2,EL3,EL4,LAST(I)
		   END IF
	   	ELSE
		   WRITE(22,10) INT,'  ',EL1,EL2,LAST(I)
		ENDIF
	   ENDIF
150	CONTINUE

C
C     Writes an asterisk '*' to INV.LST to seperate
C   the data list from the integral list.
C

	WRITE(22,15)
 15	FORMAT(' *')

C
C     This loop writes the data to the file INV.LST.
C
 20	FORMAT(A26)
	IOUNIT = 22
	IF (INTCNT .GE. 5) IOUNIT = 23
	DO 200 I=1,COUNT
	   II = ORDER(I)
	   DO 210 J = 26,1,-1
	      IF (STR2(II)(J:J).NE.' ') GO TO 220
210	   CONTINUE

220	   WRITE(IOUNIT,'(A)')  STR2(II)(1:J)
200	CONTINUE

	WRITE(IOUNIT,30)
 30	FORMAT(14X,'*')

	RETURN
	END


C
C..............................................................................
C
C	Subroutine UNIQUR computes a unique list of integrals for the
C   R integral list.
C

	SUBROUTINE UNIQUR

C
C  Variable declaration
C
	INTEGER I
	CHARACTER*3 EL1,EL2,EL3,EL4,K*2,PREV*14,INT*1
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR
C
C     Begin Unique operation.  Set INT equal to its appropriate value
C   dependent upon the value of INTCNT.
C
	INT='R'

 10	FORMAT(' ',A1,A2,'(',2A3,','2A3,')',I5)

	DO 150 I=1,COUNTR
	   IF(LASTR(I) .NE. 0) THEN
		IINT = IINT + 1
		EL1=STOR1(I)(1:3)
		EL2=STOR1(I)(4:6)
		EL3=STOR1(I)(7:9)
		EL4=STOR1(I)(10:12)
		K=STOR1(I)(13:14)
		WRITE(22,10) INT,K,EL1,EL2,EL3,EL4,LASTR(I)
	   ENDIF
	   PREV=STOR1(I+1)
150	CONTINUE

C
C     Writes an asterisk '*' to INV.LST to seperate
C   the data list from the integral list.
C

	WRITE(22,15)
 15	FORMAT(' *')

C
C     This loop writes the data to the file INV.LST.
C
C

	DO 200 I=1,COUNTR
	   II = ORDERR(I)
	   DO 210 J = 26,1,-1
		IF (STOR2(II)(J:J) .NE. ' ') GO TO 220
210	   CONTINUE
220	   WRITE(22,'(A)')  STOR2(II)(1:J)
200	CONTINUE

	WRITE(22,30)
 30	FORMAT(14X,'*')

	RETURN
	END

C
C.............................................................................
C
C     This subroutine processes the L integrals.  Reads in the data and puts
C   them in canonical form.
C
	SUBROUTINE READL

C
C   Variable declaration
C

	INTEGER OPTR
	CHARACTER*3 EL1,EL2,TEMP,I,J,COE*14,INT*1,OV1,OV2,OV3,OV4,
     :            POW1*2,POW2*2,STR8*8,STR*16
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR
C


 10	FORMAT (A14,A1,2(1X,A3,A3),1X,2(2(1X,A3),1X,A2))
100   READ(21,10) COE,INT,EL1,I,EL2,J,OV1,OV2,POW1,OV3,OV4,POW2

	IF (INT.EQ.'*') THEN
	   COUNT=COUNT-1
	   RETURN
	ENDIF

	IF (EL1.GT.EL2) THEN
	   TEMP=EL2
	   EL2=EL1
	   EL1=TEMP
	ENDIF
	STR1(COUNT)=EL1//EL2
	STR2(COUNT)=COE//INT//I//J

	CALL FINDOV(OV1,OV2,POW1,OV3,OV4,POW2,OPTR)
 22	WRITE(STR2(COUNT)(22:26),'(I5)') OPTR
 
	COUNT=COUNT+1

	GOTO 100

	END
C
C.............................................................................
C
C     This subroutine processes the O integrals.  It maintains the
C   unique set of overlaps (unnsorted) and determines the pointer value.
C   A negative value denotes a double overlap.
C
	SUBROUTINE FINDOV(OV1,OV2,POW1,OV3,OV4,POW2,OPTR)

C
C   Variable declaration
C

	INTEGER OPTR
	CHARACTER*3 EL1,EL2,TEMP,I,J,COE*14,INT*1,OV1,OV2,OV3,OV4,
     :            POW1*2,POW2*2,STR8*8,STR*16
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR
C

	IF (OV1.GT.OV2)THEN
	   TEMP=OV2
	   OV2=OV1
	   OV1=TEMP
	ENDIF
	IF (OV3.GT.OV4)THEN
	   TEMP=OV4
	   OV4=OV3
	   OV3=TEMP
	ENDIF
	IF (OV1//OV2.GT.OV3//OV4 .AND. POW2.NE.'  ') THEN
	   TEMP=OV3
	   OV3=OV1
	   OV1=TEMP
	   TEMP=OV4
	   OV4=OV2
	   OV2=TEMP
	   TEMP = POW2
	   POW2 = POW1
	   POW1 = TEMP
	ENDIF

	OPTR = 0
	IF (POW1.NE.'  ') THEN
	   IF (POW2.NE.'  ') THEN

C
C             Search STR4 for a double overlap
C
	    STR = OV1//OV2//POW1//OV3//OV4//POW2
	    DO 20 II = 1,NOV2
		IF (STR.EQ.STR4(II)) THEN
		    OPTR = II
		    RETURN
		ENDIF
 20	    CONTINUE

C
C     String not found .. add to list
C
	    NOV2 = NOV2 + 1
	    IF (NOV2.GT.(100)) STOP 'TOO MANY OVERLAP INTEGRALS'
	    STR4(NOV2) = STR
	    OPTR = -NOV2
  	ELSE

C	   Search the single overlap list
C
	    STR8 = OV1//OV2//POW1
	    DO 30 II = 1,NOV1
		IF (STR8.EQ.STR3(II)) THEN
		    OPTR = II
		    RETURN
		ENDIF
 30	    CONTINUE

C
C     String not found .. add to list
C
	    NOV1 = NOV1 + 1
	    IF (NOV1.GT.(200)) STOP 'TOO MANY OVERLAP INTEGRALS'
	    STR3(NOV1) = STR8
	    OPTR = NOV1
	  ENDIF
	ENDIF
	END
C.......................................................................
C
C     This subroutine prints out the lists of overlap integrals
C
	SUBROUTINE OVLAPS
      IMPLICIT LOGICAL (A-Z)

      INTEGER   I
	CHARACTER OV1*3,OV2*3,POW1*2,OV3*3,OV4*3,POW2*2
      INTEGER   COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :          LAST, LASTR, ORDER, ORDERR
      CHARACTER STR1*14, STR2*26, STR3*8, STR4*16, STOR1*14, 
     :          STOR2*26
      DIMENSION LAST(5000), LASTR(5000), ORDER(5000), ORDERR(5000),
     :          STR1(1:5000), STR2(1:5000), STR3(1:200), STR4(1:100),
     :          STOR1(1:5000), STOR2(1:5000)
C
C   Common declaration
C
      COMMON STR1, STR2, STR3, STR4, STOR1, STOR2
      COMMON /NUM/ COUNT, COUNTR, INTCNT, NOV1, NOV2, IINT, 
     :             LAST, LASTR, ORDER, ORDERR
C


 10	FORMAT(1X,2('O',A2,'<',A3,'|',A3,'>':))

C    Write the single overlap list
C
	DO 20 I = 1,NOV1
	    OV1=STR3(I)(1:3)
	    OV2=STR3(I)(4:6)
	    POW1 = STR3(I)(7:8)
	    WRITE(22,10) POW1,OV1,OV2
 20	CONTINUE

	WRITE(22,21)
 21	FORMAT(' *')

C
C    Write the double overlap list
C

	DO 30 I = 1,NOV2
	    OV1 = STR4(I)(1:3)
	    OV2 = STR4(I)(4:6)
	    POW1 = STR4(I)(7:8)
	    OV3 = STR4(I)(9:11)
	    OV4 = STR4(I)(12:14)
	    POW2 = STR4(I)(15:16)
	    WRITE(22,10) POW1,OV1,OV2,POW2,OV3,OV4
 30	CONTINUE

	WRITE(22,21)

	RETURN
	END
