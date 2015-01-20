C
C     ------------------------------------------------------------------
C     3          P R O G R A M   L I S T I N G
C     ------------------------------------------------------------------
C
C
C     All comments in the program listing assume the radial function P
C     is the solution of an equation of the form
C
C      P" + ( 2Z/R - Y - L(L+1)/R**2 - E)P = X + T
C
C     where Y is called a potential function
C           X is called an exchange function, and
C           T includes contributions from off-diagonal energy parameter,
C             interactions between configurations, etc.
C
C     The program uses LOG(Z*R) as independent variable and
C                      P/SQRT(R) as dependent variable.
C     As a result all equations must be transformed as described in
C     Sec. 6-2 and 6-4.
C
C     This program is written as a System 360/370 double precision
C     program.  However, on computers with a word length of 48 bits or
C     more it should be run as a single precision program.  Such con-
C     version is facilitated through the use of IMPLICIT type declar-
C     ations and the initialization of virtually all double precision
C     constants in the INIT program.  Conversion to a single precision
C     program requires that:
C     1. All IMPLICIT REAL*8 cards be removed
C     2. Type declarations REAL*8 and REAL*4 be replaced by REAL
C                          INTEGER  be replaced by INTEGER
C                          LOGICAL*1  be replaced by LOGICAL
C     3. DOUBLE PRECISION be removed from FUNCTION definition cards
C     4. Double precision constants be changed
C     5. Function names such as DABS, DSQRT, etc. be changed to ABS,
C        SQRT, etc.
C
C
C     ------------------------------------------------------------------
C    3-1       M A I N   P R O G R A M
C     ------------------------------------------------------------------
C
C       The MAIN program controls the overall calculation and  allows
C   a series of cases to be processed as one run.  Each  case  itself
C   may  consist  of  a  series of atoms or ions in an iso-electronic
C   sequence.  In each case, all but the initial  estimates  for  the
C   first  are  obtained  by  scaling  the previous results using the
C   scaling of Sec.  (7-2).   Mixing coefficients are left unchanged.
C
C
C
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
        COMMON /PARM1/ ICASE, IPTYP(30), IORTHO
	LOGICAL PRINT
	CHARACTER ANS*1
	REAL TIME
      EQUIVALENCE (IUC,IU(1)),(OUC,OU(1))
*
*  ***** Define unit numbers and open files *********************
*								*
*	 UNIT NUMBERS AND FILE NAMES MAY BE MACHINE		*
*	 DEPENDENT. CHECK THE FOLLOWING SECTION. 		*
*								*
*	 IN - Standard input unit, normally the terminal	*
*	 OUT- Standard output unit, normally the terminal	*
*	 PRI- Printer output unit or file.			*
*								*
	IN = 5
	OUT = 6
	PRI = 3
C
C  *****  WRITE OUT HEADER
C
      WRITE(OUT,9)
9     FORMAT(//10X,'============================================'/
     :         10X,'|   M     M    CCCC    HH  HH   FFFFF      |'/
     :         10X,'|   MM   MM   CC  CC   HH  HH   FF         |'/
     :         10X,'|   MMM MMM   CC       HHHHHH   FFFF  .82  |'/
     :         10X,'|   MM M MM   CC  CC   HH  HH   FF         |'/
     :         10X,'|   MM   MM    CCCC    HH  HH   FF         |'/
     :         10X,'============================================')
C
C  *****  WRITE OUT DIMENSION INFORMATION
C
      WRITE(OUT,99) 'NCFG',500,'NWF',30,'NO',880
99    FORMAT(//10X,'THE DIMENSIONS FOR THE CURRENT VERSION ARE:'/
     :       (10X,3(A6,'=',I3,4X)/)/)
C
C  *****  INITIALIZE COMMON DATA ARRAYS
C
      CALL INITA
      CALL INITR
*								*
*  ***** IN THE OPEN STATEMENTS CHECK FOR VALID FILE NAMES ******
*								*
1     PRINT '(//A/A//)', ' START OF CASE',' ============='
      WRITE (*,*) 'Type of scattering - (1) Electron, (2) Positron?'
      READ (*,*) ICASE
      WRITE (*,*) 'Orthogonality (1) OFF, or (2) ON ? '
      READ (*,*) IORTHO
      WRITE (*,*) 
33    PRINT *,' Input data - CFG, FGR, WFN - from'
      PRINT '($,A)', ' (0 -omit; 1-terminal; 2-file: FORMAT(I,I,I)) '
      READ(IN,'(3I)') IU
      IF (IU(1) .EQ. 0 .OR. IU(2) .EQ. 0) THEN
          PRINT *,' Input for CFG and/or FGR cannot be omitted'
          GO TO 33
      END IF
      PRINT '(/A/$,A)', ' Output data - CFG, FGR, WFN, MTRX - to',
     :      ' (0-omit; 1-terminal; 2-file; 3-printer: FORMAT(I,I,I,I)) '
      READ(IN,'(4I)') OU
      DO 3 I = 1,4
	 IF (I .LE. 3) THEN
            IF (IU(I) .EQ. 1) THEN
                IU(I) = IN
            ELSE IF (IU(I) .GE. 2) THEN
*	    ... The input unit numbers are 21, 22, 23 ...
                  IU(I) = 20 + I
                  IF (I .EQ. 1) THEN
                     OPEN(UNIT=IUC,FILE='CFG.INP',STATUS='OLD')
                    ELSE IF (I .EQ. 2) THEN
                     OPEN(UNIT=IUD,FILE='INV.LST',STATUS='OLD')
                    ELSE
                     OPEN(UNIT=IUF,FILE='WFN.INP',STATUS='OLD',
     :                     FORM='UNFORMATTED')
                  END IF
	    END IF
         END IF
         IF (OU(I) .EQ. 1) OU(I) = OUT
         IF (OU(I) .EQ. 2) THEN
*	    ... The output unit numbers are 31, 32, 33, 34 ...
            OU(I) = 30 + I
            IF (I .EQ. 1) THEN
               OPEN(UNIT=OUC,FILE='cfg.out',STATUS='NEW')
              ELSE IF (I .EQ. 2) THEN
               OPEN(UNIT=OUD,FILE='fgr.val',STATUS='NEW')
              ELSE IF (I .EQ. 3) THEN
               OPEN(UNIT=OUF,FILE='wfn.out',STATUS='NEW',
     :               FORM='UNFORMATTED')
              ELSE
               OPEN(UNIT=OUH,FILE='mtx.out',STATUS='NEW')
	    END IF
         END IF
    3 CONTINUE
*
         OPEN(UNIT=70,FILE='phase.out',STATUS='NEW')
*
*  ***** END OF INPUT/OUTPUT INTERFACE **************************
*
*	 The following is a non-standard procedure for timing a
*	calculation.  It may be deleted or replaced.
*
	TIME = 0.0
*
      FAIL = .FALSE.
      DO 4 I=1,(30)
      DPM(I) = D10
      IEPTR(I) = 0

4     CONTINUE
      DO 5 I = 1,(199)
         IJE(I) = 0
5     CONTINUE
C
C  *****  DETERMINE DATA ABOUT THE PROBLEM
C
      CALL DATA(ECORE)
C
C
C  ***** REWIND WAVEFUNCTION FILE FOR NEXT CASE
C
         IF (IU(3) .GT. 10) REWIND(UNIT=IU(3))
C
C  *****  SET PARAMETERS TO THEIR DEFAULT VALUE
C
13    PRINT = .FALSE.
      SCFTOL = 1.D-6
      NSCF = 20
      IC = 0
      ACFG = D0
      TRACE = .FALSE.
      PRINT '(/$,A)',' Default values for other parameters ? (Y/N) '
      READ (IN,'(A)') ANS
      IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') THEN
         PRINT '(/$,A,A)',' Default values for',
     :   ' PRINT, SCFTOL ? (Y/N) '
         READ(IN,'(A)') ANS
         IF ( ANS .NE. 'Y' .AND. ANS .NE. 'y'  ) THEN
            PRINT '(A)',' Input free FORMAT(L, F, F) '
            READ(IN,'(L,F)') PRINT, SCFTOL
         END IF
         PRINT '(/$,A)', ' Default values for NSCF, IC ? (Y/N) '
         READ(IN,'(A)') ANS
         IF (ANS .NE. 'Y' .AND. ANS .NE. 'y' ) THEN
            PRINT '($,A)', ' Input free FORMAT(I, I) '
            READ(IN,'(2I)') NSCF, IC
         END IF
         PRINT '(/$,A)', ' Default values for ACFG,TRACE ? (Y/N) '
         READ(IN,'(A)') ANS
         IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') THEN
             PRINT '($,A)', ' Input free FORMAT( F, L) '
             READ(IN,'(F,2L)') ACFG,TRACE
         END IF
      END IF
      WRITE(OUT,2) PRINT,CFGTOL,SCFTOL,NSCF,IC,ACFG,ID,TRACE
2     FORMAT(/L3,2D6.1,2I3,F3.1,I3,L3)
*
*	Determine the range of values for K*K
*
	PRINT '(//$,A)', ' Enter k: MIN, DELTA, MAX '
	READ(5,*) ELOW, EDELTA, EHIGH
*
      DO 100 EK = ELOW, EHIGH, EDELTA
* ###
	EKK = -EK
        WRITE(6,'(//A,F10.5/)') ' ***CALCULATIONS FOR k*k=',-EKK
	 CALL EIJSET(NWF,NWF,EKK)
C
C  *****  PERFORM THE MCHF ITERATION
C
      CALL SCF(ECORE,EKK,ACFG,SCFTOL,PRINT)
15    IF (FAIL) GO TO 6
100   CONTINUE
C
C  *****  DETERMINE END OF CASE
C
6     PRINT '(//A/A//A,F8.3,A//)', ' END OF CASE',' ===========',
     :      ' Total CPU time was ', TIME,' minutes'
      END
C
C     ------------------------------------------------------------------
C    3-3       D A T A
C     ------------------------------------------------------------------
C
C       Data concerning the number of configurations (NCFG), the number
C   and type of electrons in each  configuration,  as  well  as  data
C   associated with the energy expression are read and stored.
C
C
      SUBROUTINE DATA(ECORE)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :      ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
        COMMON ZZ(30),IND(30),IELI(5),NOCCSH(500)
*
	LOGICAL SETORT,FIRST,STRONG
	CHARACTER*3 EL1,EL2,EL3,EL4,EL5,EL6,ELCLSD(18),ELORT(10,2),
     :              OF(30),ELI(5),ANS*1,STRING*40
*
C
    1 FORMAT(18(1X,A3))
    7 FORMAT(A3,F6.0,I3,I3,F3.1)
C
C  *****  READ 'ATOM' CARD
C
5     PRINT '(/$,A)', ' ATOM, TERM, Z in FORMAT(A,A,F) : '
      READ(IN,'(A)') STRING
      I = INDEX(STRING,',')
      IF ( I .EQ. 0) THEN
          PRINT *,' ATOM, TERM, and Z must be separated by commas '
          GO TO 5
      END IF
      ATOM = STRING(1:I-1)
      J = INDEX(STRING(I+1:),',')
      IF ( J .EQ. 0) THEN
          PRINT *,' ATOM, TERM, and Z must be separated by commas '
          GO TO 5
      END IF
      TERM = STRING(I+1:I+J-1)
      READ(STRING(I+J+1:),'(F10.0)') Z
C
C  ***** READ CONFIGURATION CARDS AND NORMALIZE THE WEIGHTS
C
      IF (IUC .EQ. IN ) THEN
          PRINT '($,A)', 'Enter energy of Core ( in a.u.): '
          READ(IN,*) ECORE
C
C  *****  INPUT COMMON CLOSED SHELLS
C
      PRINT *,' List the common CLOSED shells - FORMAT(18(1X,A3))'
      READ(IUC,1) (ELCLSD(I),I=1,18)
C
C  *****  INPUT THE CONFIGURATIONS
C
      NCFG = 0
      PRINT '(/A/A/)', ' Enter configurations followed by weights',
     :   ' Example:  1s(2)2s(2)2p(1),1.0 '
    2 PRINT '($,I6,A)',NCFG+1,'.  '
      READ(IUC,'(A)',END=10)  STRING
      IF (STRING(1:1) .NE. '*' .AND. STRING(1:3) .NE. '   ') THEN
         ICOMMA = INDEX(STRING,',')
         IF (ICOMMA .EQ. 0) THEN
            W = D0
            ICOMMA = 41
          ELSE
            W = D0
            IF (ICOMMA .LT. 40) READ(STRING(ICOMMA+1:),'(F)') W
         END IF
         NCFG = NCFG+1
         IF (NCFG .LE. (500)) THEN
            WT(NCFG) = W
            CALL REFORM(STRING, CONFIG(NCFG))
            DO 4 I = 1,9
               COUPLE(NCFG,I) = '   '
   4        CONTINUE
            GO TO 2
           ELSE
            STOP ' TOO MANY CONFIGURATIONS: MAX = (500)'
         END IF
         END IF
      ELSE
C
C  *****  READ CONFIGURATIONS FROM A FILE
C
         READ(IUC,'(15X,F14.7/18(1X,A3))') ECORE,(ELCLSD(I),I=1,18)
         NCFG = 0
    3    READ(IUC,'(A,F16.0)',END=10) STRING,W
         IF (STRING(1:1) .NE. '*' .AND. STRING(1:3) .NE. '   ') THEN
            NCFG = NCFG+1
            IF (NCFG .LE. (500) ) THEN
               CONFIG(NCFG) = STRING
               WT(NCFG) = W
               READ(IUC,'(9(5X,A3))') (COUPLE(NCFG,J),J=1,9)
               GO TO 3
              ELSE

               STOP ' TOO MANY CONFIGURATIONS: MAX =(500)'
            END IF
         END IF
      END IF
10    CONTINUE
      ID = -1
C
C  *****  DETERMINE NCLOSD SHELLS
C
      I = 0
      SS = D0
   12 IF (ELCLSD(I+1) .NE. '   ') THEN
         I = I+1
         VARIED(I) = .TRUE.
         EL(I) = ELCLSD(I)
         OF(I) = EL(I)
         J = 3
         IF (EL(I)(1:1) .NE. ' ') J = 2
         L(I) = LVAL(EL(I)(J:J))
         N(I) = ICHAR(EL(I)(J-1:J-1)) - ICHAR('1') + 1
         IFULL = 2*(2*L(I)+1)
         S(I) = SS + IFULL/2
         SS = SS + IFULL
         METH(I) = 1
         ACC(I) = D0
         IND(I) = 0
	 SUM(I) = 4*L(I)+2
         IF (IUF .NE. 0)  IND(I) = -1
         IF( I .LT. 18) GO TO 12
         STOP ' TOO MANY CLOSED SHELLS: MAX = 18'
      END IF
      NCLOSD = I
C
C  *****  DETERMINE THE OTHER ELECTRONS
C
      MAXORB = NCLOSD
      DO 15 NC = 1,NCFG
         STRING = CONFIG(NC)
         J = 2
         I = 0
 16      IF (STRING(J:J+2) .NE. '   ' ) THEN
C
C  *****     An electron has been found; is it a new one?
C
            I = I+1
            EL1 = STRING(J:J+2)
            K = NCLOSD + 1
 17         IF (K .LE. MAXORB) THEN
               IF ( OF(K) .NE. EL1 ) THEN
                  K = K+1
                  IF (K .GT. (30)) STOP ' TOO MANY ELECTRONS: MAX= (30)'
                  GO TO 17
               END IF
              ELSE
C
C  *****         A new electron has been found; add it to the list
C
               MAXORB = K
               OF(MAXORB) = EL1
	       IF (EL1(1:1) .EQ. 'k' .OR. EL1(2:2) .EQ. 'k') ID = NC-1
            END IF
            J = J+8
            IF (J .LT. 40) GO TO 16
         END IF
         NOCCSH(NC) = I
   15 CONTINUE
      IF (ID .EQ. -1) STOP ' No continuum function found'
C
C  *****  The list of electrons has been determined
C
      PRINT 19, MAXORB,(OF(J),J=1,MAXORB)
   19 FORMAT(/' There are ',I3,' orbitals as follows:'/(1X,18(1X,A3)))
      NWF = MAXORB
       PRINT '(/$,A)',' Enter number to be varied (last in the list) '
       READ '(I)', NIT
      IB = NWF - NIT + 1
      PRINT '(/$,A)', ' Default electron parameters ? (Y/N) '
      READ '(A)', ANS
      IF ( ANS .NE. 'Y' .AND. ANS .NE. 'y') PRINT '(A/A)',
     :   ' EL, S, IND, METH, ACC for non-closed shell electrons: ',
     :   ' FORMAT(A3, F, I, I, F) with NO comma after ELectron'
      DO 20 I = NCLOSD+1,NWF
         IF ( ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
            EL(I) = OF(I)
            S(I) = SS
            METH(I) = 3
            ACC(I) = D0
            IND(I) = 0
            IF (IUF .NE. 0)  IND(I) = -1
          ELSE
            READ(IN,'(A3,F,I,I,F)') EL(I),S(I),IND(I),METH(I),ACC(I)
         END IF
	 VARIED(I) = .TRUE.
         J = 2
         IF (EL(I)(1:1) .EQ. ' ') J = 3
         L(I) = LVAL(EL(I)(J:J))
         N(I) = ICHAR(EL(I)(J-1:J-1)) - ICHAR('0')
 20   CONTINUE
C
C  *****  CHECK METHOD AND SET ORTHOGONALITY
C
	DO 35 NC = 1,NCFG
         STRING = CONFIG(NC)
         J = 2
         I = 0
 30      IF (STRING(J:J+2) .NE. '   ' ) THEN
C
C  *****     An electron has been found; find its index
C
            I = I+1
            ELI(I) = STRING(J:J+2)
            CALL EPTR(EL,ELI(I),IELI(I))
            READ(STRING(J+4:J+5),'(I2)') IQ
            J = J+8
            IF (J .LT. 40) GO TO 30
         END IF
	 EL1 = EL(NWF)
         IF (.NOT. (EL1(1:1).EQ.'k' .OR. EL1(2:2).EQ.'k'))
     :     STOP ' Last orbital not a continuum orbital'
	 METH(NWF) = 4
	 IND(NWF) = -1
C
C  *****  DEFINE ALL ORBITALS IN THE CONFIGURATION TO BE ORTHOGONAL
C
         DO 34 I1 = 2,I
            J1 = IELI(I1)
            DO 33 I2 = 1,I1-1
               J2 = IELI(I2)
               IF (L(J1) .EQ. L(J2) ) THEN
                    CALL EIJSET(J1,J2,1.D-5)
		    CALL EIJSET(J2,J1,1.D-5)
	 	 END IF
   33       CONTINUE
   34    CONTINUE
   35 CONTINUE
C
C  ***** SET THE FOLLOWING ORBITALS ORTHOGONAL
C
C        1) ORBITALS WITH DIFFERENT L'S
C        2) IN THE SAME ORTHOGONAL SET
C        3) SPECIFIED ORTHOGONALITY
C
C  *****
C
      DO 38 I = 2,NWF
         DO 39 J = 1,I-1
            IF (L(I) .EQ. L(J) .AND. SETORT(EL(I),EL(J)) ) THEN
		C = 1.D-5
		IF (I.LE.NCLOSD .AND. J.LE.NCLOSD) C = 1.D-10
	        CALL EIJSET(I,J,C)
		CALL EIJSET(J,I,C)
            END IF
   39    CONTINUE
   38  CONTINUE
C
C  *****  DETERMINE ADDITIONAL ORTHOGONALITY PAIRS
C
      I = 0
      IF ( IUC .NE. IN) THEN
   40    READ(IUC,1,END=50) EL1,EL2
         IF ( EL1 .NE. '*  ' .AND. EL2 .NE. '   ') THEN
            ELORT(I,1) = EL1
            ELORT(I,2) = EL2
            CALL EPTR(EL,EL1,I1)
            CALL EPTR(EL,EL2,I2)
	    CALL EIJSET(I1,I2,1.D-5)
	    CALL EIJSET(I2,I1,1.D-5)
            I = I +1
            IF (I .GT. (10)) STOP ' TOO MANY ORTHOGONALITIES: MAX=(10)'
            GO TO 40
         END IF
         NORT = I
      END IF
C
C  *****  ADDITIONAL PARAMETERS
C
   50 PRINT '(/$,A)', ' Default values (NO,REL,STRONG) ? (Y/N) '
      READ(IN,'(A)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
         NO = (880) 
         REL = .FALSE.
         STRONG = .FALSE.
         IF (NCFG .GT. 1) STRONG = .TRUE.
       ELSE
         PRINT *, ' Enter values in FORMAT(I,L,L) '
         READ(IN,'(I,L,L)') NO, REL, STRONG
         IF (NO .GT. (880)) STOP
     :     ' TOO MANY POINTS FOR EACH FUNCTION: MAX=(880)'
      END IF
      ND = NO - 2
      WRITE(OUT,61) ATOM,TERM,Z,NO,NWF,NIT,NCFG,REL,STRONG
61    FORMAT(/1X,2A6,F6.0,I6,3I3,2L3)
      WRITE(PRI,62) ATOM,TERM,Z,(EL(I),4*L(I)+2,I=1,NCLOSD)
62    FORMAT(1H1///9X,33HHARTREE-FOCK WAVE FUNCTIONS FOR  ,2A6,4H Z =,
     1   F5.1//14X,'CORE = ',5(A3,'(',I2,')'))
      WRITE(PRI,'(//11X,A,37X,A//)') 'CONFIGURATION','WEIGHT'
      OMIT = .NOT. STRONG
C
C *****  WRITE 'CONFIGURATION' CARDS  AND CHECK WEIGHTS
C
      W = D0
      DO 63 I = 1,NCFG
	W = W + WT(I)**2
63    CONTINUE
      IF (W .EQ. D0) STOP ' WEIGHT information omitted'
      DO 68 I = 1,NCFG
      NOCC=NOCCSH(I)
      WRITE(PRI,70) I, CONFIG(I), WT(I),(COUPLE(I,J),J=1,NOCC)
70    FORMAT(/3X,I3,6X,A40,F19.8/12X,9(5X,A3))
      WRITE(PRI,73) (COUPLE(I,J),J=NOCC+1,2*NOCC-1)
73    FORMAT(23X,4(5X,A3))
68    CONTINUE
      WRITE(PRI,71)
71    FORMAT(//9X,10HINPUT DATA/9X,10H----- ----//13X,13HWAVE FUNCTION,
     1   11H  PROCEDURE/17X,22HNL  SIGMA METH ACC OPT///)
      DO 79 I = 1,NWF
      WRITE(PRI,78) I,EL(I),N(I),L(I),S(I),METH(I),ACC(I),IND(I)
78    FORMAT(I8, 2X,A3,2I3,F7.1,I4,F4.1,I4)
79    CONTINUE
C
C  *****  INITIALIZE ARRAYS, IF NECESSARY
C
      CALL WAVEFN
      IF (IND(NWF) .NE. -2) THEN
      DO 95 J = 1,NO
	P(J,NWF) = D0
95    CONTINUE
      ENDIF
      AZ(NWF) = D1
      MAX(NWF) = NO
      DO 100 I=1,6
	 INTPTR(I) = 0
100   CONTINUE
      IF (IUD .NE. IN) CALL INTGRL
C
C  *****  DEFINE SUM(I)
C
	IBEGIN = INTPTR(5)+1
	IEND = INTPTR(6)
	DO 80 I = IBEGIN,IEND
	   IF (IEL(I,1).EQ.IEL(I,2)) SUM(IEL(I,1)) = -2*COEF(I)
 80	CONTINUE
      WRITE(70,991) ATOM,TERM,Z,EL(NWF)
991   FORMAT (25X,2A6,F5.1,5X,A3,/)
      WRITE(70,992)
992   FORMAT(20X,'      EK         DELTA    ',/)
      RETURN
99    STOP
      END
C
C     ------------------------------------------------------------------
C    3-4       D E
C     ------------------------------------------------------------------
C
C       This routine controls the solution of the differenttial equation
C   for the radial function P  .  One of three methods is selected -
C                            I1
C   M1, M2, or M3 -  for solving the equations,  the  initial  choice
C   being determined by an input paramter, METH(I), except when no
C   exchange is present, in which case M2 is selected. (For further
C   information see Sec. 7-4)
C
C        Value of METH(I)     Method
C        ---------------      ------
C        < or =1            M1 with search for an acceptable solution
C             =2            M2 with search for an acceptable solution
C             =3            M3 without any checking
C
C   If M1 fails to find an acceptable solution, the radial  functions
C   are  orthogonalized,  off-diagonal  energy parameters recomputed,
C   and the method tried again.   Should it continue to fail, METH(I)
C   is set to 2.
C
C
      SUBROUTINE DE(I1)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*

        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :      ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON P2(880),HQ(880),XX(880),AC(20,20),BC(20),JV(20),
     :     AZZ,PP,FN,EM,FM,EU,FU,DELTAE,M,NODE,MK,KK,NJ
*
	LOGICAL DIAG
	CHARACTER*2 ASTER(3)
        DATA  ASTER / '  ', '* ', '**' /      
*
      I = I1
      ED2 = E(I,I)
      KK= MAX0(1,METH(I))
      IF (NWF .EQ. 1) KK = 2
      NODE = N(I) - L(I) - 1
C
C  *****  CALL METHD1 TO SOLVE THE DIFFERENTIAL EQUATION
C
      CALL METHD1(I)
      IF ( FAIL ) GO TO 25
C
12    PN = DSQRT(QUAD(I,M,PDE,PDE))
      DO 9 J = 1,M
9     PDE(J) = PDE(J)/PN
      AZZ = AZZ/PN
C
C  *****  CHECK IF METHOD 2 SHOULD BE USED
C
      IF ( KK .NE. 1 ) GO TO 13
      IF (DABS(D1 -ED2/E(I,I)) .GT. 0.01D0  .OR.
     1    DMAX1(DABS(D1 - PN), DABS(D1/PN - D1)) .LT. 0.10D0 )
     2    GO TO 13
       METH(I) = 2
       KK = 2
       GO TO 25
C
C  *****  SET THE ACCELERATING PARAMETER
C
C
13    IF (IPR .NE. I ) GO TO 33
      ED2 = ED2 - E(I,I)
      IF (ED1*ED2 .GT. D0) ACC(I) = .75*ACC(I)
      IF (ED1*ED2 .LT. D0) ACC(I) = (D1 + D3*ACC(I))/D4
33    C = ACC(I)
      CD = D1 - C
C
C   *****  IMPROVE THE ESTIMATES
C
      MAX(I) = M
      DP     = D0
      DO 21 J = 1,M
      DIFF = P(J,I)-PDE(J)
      DP     = DMAX1(DP    ,DABS(DIFF)*R2(J))
21     P(J,I) = PDE(J) + C*DIFF
      IF (M .EQ. NO) GO TO 26
      M = M + 1
      DO 24 J = M,NO
24     P(J,I) = D0
      AZ(I) = CD*AZZ + C*AZ(I)
      AZZ = AZ(I)
     
C
C  *****  CHECK THE ORTHOGONALIZATION
C
26    NN = NWF
      IF (OMIT) NN = IB - 1
      IBEGIN = 1
      IF (I .GT. 1) IBEGIN = IEPTR(I-1) + 1
      IP = IBEGIN
      IJ = 0
50    JI = IJE(IP)
      IF (JI .NE. I) THEN
      IF (JI .GE. IB .AND. DPM(JI) .GE. DPM(I)) THEN
*		
*	 	The JI orbital should be orthogonalized
*
         C = QUADR(I,JI,0)
	 MM = MAX0(MAX(JI),MAX(I))
         DO 51 J = 1,MM
            P(J,JI) = P(J,JI) - C*P(J,I)
51       CONTINUE
         C2 = SQRT(QUADR(JI,JI,0))
         DO 52 J = 1,MM
            P(J,JI) = P(J,JI)/C2
52       CONTINUE
         VARIED(JI) = .TRUE.
         MAX(JI) = MM
         AZ(JI) = (AZ(JI) - C*AZ(I))/C2
         WRITE(OUT,63) EL(I),EL(JI),C
      ELSE
*
*              The I'th orbital must be orthogonalized
*
         IJ = IJ + 1 
         IF (IJ .GT. 20) STOP ' TOO MANY ORTHOGONALITY CONDITIONS' 
         JV(IJ) = JI 
      END IF
      END IF
      IP = IP + 1
      IF (IP .LE. IEPTR(I)) GO TO 50
      IF (IJ .NE. 0 ) THEN
         DIAG = .TRUE.
         DO 61 J = 1,IJ
            BC(J) = QUADR(I,JV(J),0)
            AC(J,J) = D1
            DO 62 JJ = J+1, IJ
               IF (E(JV(J),JV(JJ)) .NE. D0 ) THEN
                  AC(J,JJ) = D0
                  AC(JJ,J) = D0
                ELSE
                  AC(J,JJ) = QUADR(JV(J),JV(JJ),0)
                  AC(JJ,J) = AC(J,JJ)
                  DIAG = .FALSE.
               END IF
62          CONTINUE
61       CONTINUE
         IF ( .NOT. DIAG .AND. IJ .GT. 1) CALL LINEQN(20,IJ,AC,BC)
         M = MAX(I)
         DO 65 JJ = 1,IJ
            C = BC(JJ)
            WRITE(OUT,63) EL(JV(JJ)),EL(I),C
63          FORMAT(6X,'<',A3,'|',A3,'>=',1PD8.1)
            M = MAX0(M,MAX(JV(JJ)))
            DO 64 J = 1,M
               P(J,I) = P(J,I) - C*P(J,JV(JJ))
64          CONTINUE
            AZZ = AZZ - C*AZ(JV(JJ))
65       CONTINUE
         PNN = DSQRT(QUADR(I,I,0))
         DO 66 J = 1,M
            P(J,I) = P(J,I)/PNN
66       CONTINUE
         AZZ = AZZ/PNN
      END IF
      M = NO
67    IF (DABS(P(M,I)) .LT. 1.D-15) THEN
         P(M,I) = D0
         M = M-1
         GO TO 67
      END IF
      MAX(I) = M
      IF (AZZ .GT. D0) AZ(I) = DMAX1(AZZ,D5*AZ(I))
      WRITE(OUT,17) EL(I),E(I,I),AZ(I),PN,ASTER(KK),DP
17    FORMAT(20X,A3,2F15.7,F12.7, A2,1PD10.2)
      DPM(I) = DP
      IF (IPR .EQ. I1) ED1 = ED2
      IF (IPR .NE. I1) ED1 = ED2 - E(I1,I1)
      IPR = I1
      VARIED(I) = .TRUE.
      RETURN
C
C  *****  IF METHD1 FAILED TO FIND AN ACCEPTABLE SOLUTION, ORTHOGONALIZE
C  *****  THE ESTIMATES AND TRY AGAIN
C
25    IF (I .EQ. IB) GO TO 27
      CALL ORTHOG
      CALL UPDATE
      CALL GRANGE
27    CALL METHD1(I)
      IF ( FAIL ) GO TO 23
      GO TO 12
C
C  *****  ERROR RETURN FROM SECOND TRY.  IF M1 WAS USED,SWITCH TO
C         M2 AND TRY ONCE MORE.
C
23    IF ( KK .EQ. 2) RETURN
      KK = 2
      GO TO 27
      END
C
C     ------------------------------------------------------------------
C    3-19      M E T H O D
C     ------------------------------------------------------------------
C
C       Uses M1, M2, or M3 to solve the radial equation. If the input
C   data indicated METH(I) = 3, then this  solution  is  returned  to
C   DE.  Otherwise,  the routine searches for an acceptable  solution
C   which  is  both  positive  near  the  origin and has the required
C   number  of nodes.
C
C
      SUBROUTINE METHD1(I)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
      COMMON P2(880),HQ(880),XX(880),AC(20,20),BC(20),JV(20),
     1     AZZ,PP,FN,EM,FM,EU,FU,DELTAE,M,NODE,MK,KK,NJ
*
      LOGICAL V2, FIRST
      DIMENSION P1(880)
      EQUIVALENCE (PDE(1),P1(1))
C
C  *****  'FIRST' MUST BE 'TRUE' THE FIRST TIME SOLVE IS CALLED FOR
C  *****  POTENTIAL AND EXCHANGE TO BE COMPUTED
C  *****  'EU' IS THE UPPER BOUND OF THE ENERGY PARAMETER
C  *****  'EM' IS THE MINIMUM VALUE OF THE ENERGY PARAMETER
C
      FIRST = .TRUE.
      FAIL = .FALSE.
      EM = D0
      EU = ((Z - DMIN1(D5*S(I),D2*S(I)))/N(I))**2
      FU = EU
      MK = 0
17    CALL SOLVE(I,FIRST)
C
C  *****  IF KK EQUALS 3, OMIT THE NODE CHECKING
C
      IF (KK .EQ. 3) GO TO 51
C
C  *****  COUNT THE NUMBER OF NODES
C
      MN = M
      NC = NODEC(MN)
      IF (TRACE) WRITE(OUT,99) EL(I),NC,MN,NJ,PDE(MN),ED,EU,EM,DELTAE
99    FORMAT(2X,A3,' NC =',I3,' MN =',I3,' NJ =',I3,' PDE(MN) =',
     1   D10.2,' ED =',D10.2,' EU =',D10.2,' EM =',D10.2,
     2   ' DELTAE =',D10.2)
C
C  *****  IF NODE COUNT IS OFF BY NO MORE THAN 1 AND DELTAE IS STILL
C  *****  QUITE LARGE, APPLY THE DELTAE CORRECTION
C
      IF (IABS(NC-NODE) .EQ. 1 .AND. DABS(DELTAE/ED) .GT. 0.02D0)
     1      GO TO 46
C
C  *****  BRANCH ACCORDING TO WHETHER THE NODE COUNT IS TOO SMALL,
C  *****  JUST RIGHT, OR TOO LARGE
C
12    IF (NC - NODE ) 8,9,10
C
C  *****  THE SOLUTION HAS THE CORRECT NUMBER OF NODES
C
9     V2 = DABS(DELTAE)/ED .LT. 1.D-5
      IF (PDE(MN) .LT. D0 .AND. .NOT. V2) GO TO 46
      IF (PDE(MN) .GT. D0) GO TO 51
      DO 52 J = 1,NO
52    PDE(J) = - PDE(J)
      PP = -D2 - PP
51    AZZ = AZD*(D1 + PP)
      CALL EIJSET(I,I,ED)
      RETURN
C
C  *****  THE SOLUTION HAS TOO FEW NODES
C
8     IF (PDE(MN) .LE. D0) GO TO 11
      DEL = D1 - ED/EU
      EU = ED
      IF ( DEL .LT. .05D0) FU = FU*((L(I)+1+NC)/FN)**2.5
       IF (DEL  .GE. .05D0) FU = ED*((L(I)+1+NC)/FN)**2.5
      IF (FU .LT. EM) FU = D5*(EU + EM)
      IF (DABS(FU - ED) .LT. 0.001D0) GO TO 27
      ED = FU
      GO TO 33
C
C  *****  TRY A NEW VALUE OF ED WHICH MUST LIE WITHIN THE UPPER AND
C  *****  LOWER BOUND
C
11    EDP = ED
                    ED = ED*((L(I)+1+NC)/FN)**2.5
      IF (ED .GE. EU ) ED = D5*(EU + EDP)
      IF (ED .LE. EM ) ED = D5*(EM + EDP)
33    MK = MK + 1
      IF ( EU .LE. EM ) WRITE(OUT,30) EM,EU,ED
30    FORMAT(6X,48HWARNING: DIFFICULTY WITH NODE COUNTING PROCEDURE/
     1   6X,42HLOWER BOUND ON ED GREATER THAN UPPER BOUND/
     2   6X,5HEL = ,F10.6,7H  EU = ,F10.6,7H  ED = ,F10.6)
      FIRST = .FALSE.
      IF ( MK .GT. 3*N(I) .OR. EU-EM .LT. FN**(-3)) GO TO 27
      GO TO 17
C
C  *****  THE SOLUTION HAS TOO MANY NODES
C
10    IF (PDE(MN) .LT. D0) GO TO 11
      DEL = D1 - EM/ED
      EM = ED
      IF (DEL .LT. 0.05D0) FM = FM*((L(I)+1+NC)/FN)**2.5
      IF (DEL .GE. 0.05D0) FM = ED*((L(I)+1+NC)/FN)**2.5
      IF (FM .GT. EU) FM = D5*(EU + EM)
      IF (DABS(FM - ED) .LT. 0.001D0) GO TO 27
      ED = FM
       GO TO 33
C
C  *****  ADJUST ENERGY TO LIE BETWEEN UPPER AND LOWER BOUND
C
46    ED = ED - DELTAE
      IF ( ED .GE. EM .AND. ED .LE. EU ) GO TO 33
      EDP = ED
      IF ( NC-NODE .NE. 0 ) ED = (ED+DELTAE)*((L(I)+1+NC)/FN)**2.5
      IF ( ED .GE. EM .AND. ED .LE. EU ) GO TO 33
      ED = EDP + DELTAE + DELTAE
      IF ( ED .GE. EM .AND. ED .LE. EU ) GO TO 33
      ED = ED -DELTAE
      DELTAE = D5*DELTAE
      GO TO 46
C
C  *****  METHOD WAS UNABLE TO FIND AN ACCEPTABLE SOLUTION
C
27    WRITE(OUT,28) KK,EL(I),NC,NJ,ED,EM,EU
28    FORMAT(10X,6HMETHOD,I2,38H UNABLE TO SOLVE EQUATION FOR ELECTRON,
     1   A3/10X,5HNC = ,I3,3X,5HNJ = ,I3,3X,5HED = ,F10.6,3X,5HEL = ,
     2   F10.6,3X,5HEU = ,F10.6)
      FAIL = .TRUE.
      RETURN
      END
C
C
C     ------------------------------------------------------------------
C    3-20      N M R V S
C     ------------------------------------------------------------------
C
C       Given two starting values, PDE(1) and PDE(2), values of PDE(j),
C   j=3,4,...,NJ+1 are obtained by outward integration of
C               Y" = YR y + F
C   using the discretization  of  Eq.  (6-27 )  with  the  difference
C   correction.  With PDE(NJ) given, the tail procedure is applied to
C   PDE(j),j=NJ+1,  NJ+2,...,MM, where MM is determined automatically
C   and DELTA is the difference between  PDE(NJ+1)  for  outward  and
C   inward integration. (See Eq 6-32, 6-33, and 6-37 for further
C   details.)
C
C
      SUBROUTINE NMRVS(NJ,DELTA,MM,PP,F)
        IMPLICIT REAL*8(A-H,O-Z)

      INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
      COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH

      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
      DIMENSION PP(880),F(880),A(600),D(600)
      EQUIVALENCE (G,G3)
C
C  *****  INTEGRATE OUTWARD TO NJ+1
C
      Y1 = PP(1)
      Y2= PP(2)
      G1 = YR(1)
      G2 = YR(2)
      M = NJ + 1
      DO 1 I = 3,M
      G3 = YR(I)
      Y3 = (Y2+Y2-Y1 + (D10*G2*Y2 + G1*Y1) + F(I-1)) / (D1 - G3)
      PP(I) = Y3
      Y1 = Y2
      Y2 = Y3
      G1 = G2
1     G2 = G3
      DELTA = Y3
C
C  *****  APPLY THE TAIL PROCEDURE
C
      K = 1
      PP(M) = -(D1 - G1)*Y1 + F(M)
      A(1) = D1 - G
      D(1) = -(D2 + D10*G)
22    RATIO = A(K)/D(K)
C
C  *****  THE INTEGER 149 IN THE NEXT STATEMENT IS THE DIMENSION OF A
C  *****  MINUS 1
C
      IF (K .GE. (600)-1 .OR. M .EQ. ND) GO TO 23
      K = K +1
      M = M+1
      G = YR(M)
      A(K) = D1 - G
      D(K) = -(D2 + D10*G) - A(K)*RATIO
      PP(M) = -PP(M-1)*RATIO + F(M)
      IF (DABS(PP(M))+DABS(PP(M-1)) .GT. TOL .OR. K .LT. 36) GO TO 22
20    CON =DSQRT(EH)*DEXP(-DSQRT(DABS(G/CH-.25)/RR(M))*(R(M+1)-R(M)))
      PP(M) = PP(M)/(D(K) + CON*(D1-  YR(M+1)))
      J = M+1
      DO 2 I= J,NO
2     PP(I) = D0
      DO 3 J = 2,K
      I = M-J+1
      II = K-J+1
3     PP(I) = (PP(I)-A(II+1)*PP(I+1))/D(II)
C
C  *****  SET DELTA = DIFFERENCE OF THE TWO SOLUTIONS AT NJ+1
C  *****         MM = NUMBER OF POINTS IN THE RANGE OF THE SOLUTION
C
      DELTA = DELTA - PP(I)
      MM = M
      RETURN
23    WRITE(OUT,24)
24    FORMAT(6X,52HWARNING: FUNCTIONS TRUNCATED BY NMRVS IN TAIL REGION)
      GO TO 20
      END
C
C     ------------------------------------------------------------------
C    3-21      N O D E C
C     ------------------------------------------------------------------
C
C      Counts the number of nodes of the function PDE(j) in the range
C   j = 40,...,M-10.   The node counting procedure counts the local max
C   and min values.   Only nodes between sufficiently large max and
C   min values are counted.
C
C
      FUNCTION NODEC(M)
      IMPLICIT REAL*8(A-H,O-Z)
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :       ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
C
C   ***** FIND MAX|PDE(J)|
C
       MM = M - 10
      DM = 0.D0
      DO 1 J = 40,MM
1     DM = DMAX1(DM, DABS(PDE(J)))
C
C   *****  COUNT THE NUMBER OF LOCAL MAX OR MIN'S
C
      NCC = 0
      SIGN = 0.D0
      DIFF1 = PDE(160) - PDE(159)
      DO 2 J = 160, MM
      DIFF2 = PDE(J+1) - PDE(J)
      IF (DIFF2*DIFF1 .GT. 0.D0 .OR. DIFF1 .EQ. 0.D0) GO TO 2
C
C   *****  A MAX OR MIN HAS BEEN FOUND.   TEST IF IT IS
C          SUFFICIENTLY LARGE
C
      IF ( DABS(PDE(J))/DM .LT. 0.05D0 ) GO TO 2
C
C   ***** CHECK IF THIS IS THE FIRST SIGNIFICANT MAXIMUM
C
      IF (SIGN .NE. 0.D0 ) GO TO 4
      M = J
      GO TO 3
C
C   ***** IF NOT THE FIRST, TEST WHETHER A SIGN CHANGE HAS
C         OCCURRED SINCE THE LAST SIGNIFICANT MAX OR MIN
C
4     IF (PDE(J)*SIGN .GT. 0.D0 ) GO TO 2
      NCC = NCC + 1
C
C   ***** RESET FOR THE NEXT NODE
C
3     SIGN = PDE(J)
2     DIFF1 = DIFF2
      NODEC = NCC
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-23      O U T P U T
C     ------------------------------------------------------------------
C
C       The radial functions and orthogonality integrals are printed,
C   if PRINT is .TRUE.   The  functions  will  also  be  punched  (or
C   stored) on unit OUF, if OUF .NE. 0.
C
C
      SUBROUTINE OUTPUT(PRINT)
        IMPLICIT REAL*8(A-H,O-Z)
	INTEGER MMX
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
	
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
        COMMON /PHASE/DELTA
*
	LOGICAL PRINT
      DIMENSION POUT(8)
      IF ( .NOT. PRINT ) GO TO 31
C
C  *****  PRINT RADIAL FUNCTIONS, 7 PER PAGE
C
      ML = 1
2     MU = MIN0(ML+7,NWF)
      I = MU - ML + 1
      MX = 0
      DO 1 J = ML,MU
1     MX = MAX0(MX,MAX(J))
      WRITE(PRI,5) ATOM,TERM,(EL(J),J=ML,MU)
5     FORMAT(1H1,9X,19HWAVE FUNCTIONS FOR  ,2A6//10X,1HR,8(10X,A3))
      K= 0
      KK = 0
      DO 6 J = 1,MX
      DO 9 JJ = ML,MU
      IJ = JJ - ML + 1
9     POUT(IJ) = P(J,JJ)*R2(J)
      K = K+1
      IF (K .LE. 10) GO TO 6
      K = 1
      KK = KK+1
      IF (KK .LT. 5) GO TO 21
      KK = 0
      WRITE(PRI,23)
23    FORMAT(1H1//)
      GO TO 6
21    WRITE(PRI,8)
8     FORMAT(1X)
6     WRITE(PRI,10) R(J),(POUT(JJ),JJ=1,I)
10    FORMAT(F13.5,F15.6,7F13.6)
      DO 15 J = ML,MU
      IJ = J - ML + 1
15    POUT(IJ) = DPM(J)
      WRITE(PRI,16) (POUT(J),J=1,I)
16    FORMAT(4X,10HMAX. DIFF. ,F15.7,7F13.7)
      ML = ML+8
      IF (ML .LE. NWF) GO TO 2
31    IF ( NWF .LE. 1) GO TO 30
C
C  *****  PRINT ORTHOGONALITY INTEGRALS
C
      WRITE(PRI,11) ATOM,TERM
11    FORMAT(////10X,33HORTHOGONALITY INTEGRALS FOR ATOM ,A6,6H TERM ,A6
     1   //20X, 4H(NL),3X,4H(NL),7X,8HINTEGRAL //)
      LM = IB
      ML = MAX0(2,LM)
      DO 12 I = ML,NWF
      JF = I - 1
      DO 13 J = 1,JF
      IF (L(I) .NE. L(J)) GO TO 13
      T = QUADR(I,J,0)
      WRITE(PRI,17) EL(I),EL(J),T
17     FORMAT(21X,A3,4X,A3,F15.8)
13    CONTINUE
12    CONTINUE
30    IF ( OUF .EQ. 0) GO TO 14
C
C  *****  OUTPUT FUNCTIONS ON UNIT OUF FOR FUTURE INPUT
C
*	  EKI retained only for compatibility with MCHF format
*
      METH(NWF) = 4
      DO 3 I = 1,NWF
      IF (METH(I) .NE. 4) THEN
         EKI = -D5*HL(EL,I,I,REL)
      ELSE
         EKI = DELTA
      END IF
      MMX = MAX(I)
      WRITE (OUF) ATOM,TERM,EL(I),MMX,Z,E(I,I),EKI,AZ(I),
     :   (P(J,I),J=1,MMX)
3     CONTINUE
      WRITE (OUF) ATOM,TERM,EL(NWF),0,Z,E(NWF,NWF),EKI,AZ(NWF)
C
      WRITE(70,41) -E(NWF,NWF),EKI
41    FORMAT (20X,2F12.6)
14    RETURN
      END
C
C     ------------------------------------------------------------------
C    3-32      S C F
C     -----------------------------------------------------------------
C
C       This routine controls the SCF procedure described in Chapter
C   7.  If certain input parameters are zero (or blank) they will  be
C   set to their default value.
C
C          Parameter       Default Value
C          --------        -------------
C          SCFTOL          1.D-6
C          IC              (NWF + 1 - IB)/4 + 3
C          NSCF            20
C
C   The self-consistency convergence criterion is
C
C          Z2 = SQRT( SCFTOL*(Z*NWF/2) )
C
C   It is increased by a factor two at the end of each iteration 
C
C
      SUBROUTINE SCF(ECORE,EKK,ACFG,SCFTOL,PRINT)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3,ANS*1
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :      ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
        COMMON /PHASE/DELTA
*
	LOGICAL LAST,CONV,ECONV,PRINT
C
C  *****  SET THE SCF CONVERGENCE PARAMETER TO AN OPTIMISTIC VALUE
C
      ETOTAL = ECORE - D5*EKK
      TOL = DSQRT(Z)*1.D-10
      Z2 = SCFTOL
      WRITE(OUT,15)
15    FORMAT(//)
      WRITE(OUT,16) OMIT,ACFG,Z2,NO,REL
   16 FORMAT(10X,44HWEAK ORTHOGONALIZATION DURING THE SCF CYCLE=,L4/
     :       10X,44HACCELERATING PARAMETER FOR MCHF ITERATION  =,F5.2/
     :       10X,44HSCF CONVERGENCE TOLERANCE (FUNCTIONS)      =,1PD9.2
     :      /10X,44HNUMBER OF POINTS IN THE MAXIMUM RANGE      =,I4/
     :       10X,44HRELATIVISTIC DIAGONAL  ENERGY CORRECTIONS  =,L4//)
      IPR = 0
         CALL COULOM(NWF,EKK)
      IF (P(1,NWF) .EQ. D0) THEN
         CALL CSOLVE(NWF,DELTA,LAST)
C         CALL ORTHOG
      END IF
         CALL ORTHOG
C
C  *****  SET ITERATION PARAMETERS
C
      LAST = .FALSE.
      ICYCLE = 0
      CALL UPDATE
      IF ( IB .GT. NWF ) GO TO 17
19    IF ( ID .EQ. 0 .OR. NCFG .EQ. 1) GO TO 9
      CALL CDIAG(ETOTAL,ACFG,LAST)
C
C  *****  PERFORM NSCF SELF-CONSISTENT FIELD ITERATIONS
C
9     DO 100 I = 1,NSCF
      ICYCLE = ICYCLE + 1
      WRITE(OUT,7) ICYCLE,Z2
7     FORMAT(//10X,17HITERATION NUMBER ,I2/10X,16H----------------/
     1 10X,'CONVERGENCE CRITERIA =',1PD9.1/)
      DP1 = D0
      IF (IB .GT. NWF) GO TO 17
      CALL GRANGE
C
C  *****  SOLVE EACH DIFFERENTIAL EQUATION IN TURN
C
      WRITE(OUT,14)
14    FORMAT(/20X,' EL',6X,'ED/DELTA',10X,'AZ',11X,'NORM',7X,'DPM')
      DO 2 J = IB,NWF-1
      CALL DE(J)
      IF ( FAIL ) RETURN
      DP = DPM(J)*DSQRT(SUM(J))
      IF ( DP1 .GE. DP ) GO TO 2
      DP1 = DP
      JJ = J
2     CONTINUE
      CALL CSOLVE(NWF,DELTA,LAST)
      DP = DPM(NWF)*DSQRT(SUM(NWF))
      IF (DP .GT. DP1) THEN
          DP1 = DP
 	  JJ = NWF
      END IF
      IF ((NCFG .EQ. 1 .OR. ID .EQ. 0) .AND. DP1 .LT. Z2) GO TO 6
      IF ( IC .LE. 0) GO TO 6
C
C  *****  SOLVE IC DIFFERENTIAL EQUATIONS EACH TIME SELECTING THE
C  *****  ONE WITH THE LARGEST DPM
C
      DO 4 II =1,IC
      IF (JJ .EQ. NWF) THEN
        CALL CSOLVE(NWF,DELTA,LAST)
        FAIL = .FALSE.
      ELSE
        CALL DE(JJ)
      END IF
      IF ( FAIL ) RETURN
      DP1 = D0
      DO 5 J = IB,NWF
      DP = DSQRT(SUM(J))*DPM(J)
      IF ( DP1 .GT. DP ) GO TO 5
      JJ = J
      DP1 = DP
5     CONTINUE
      IF (DP1 .LT. Z2) GO TO 6
4     CONTINUE
6     CALL ORTHOG
      CALL UPDATE
      IF ( LAST ) GO TO 17
      IF ( I .EQ. NSCF ) GO TO 1
      IF (.NOT.(NCFG .EQ. 1 .OR. ID .EQ. 0)) GO TO 12
      IF (DP1 .LE. Z2 )  LAST = .TRUE.
      GO TO 1
12    CALL CDIAG(ETOTAL,ACFG,LAST)
C
C  *****  IF FUNCTIONS APPEAR TO HAVE CONVERGED,SOLVE EACH AGAIN, IN
C  *****  TURN, AND TEST AGAIN
C
      IF (DP1 .LE. Z2) LAST =.TRUE.
1     CONTINUE
      WRITE(OUT,8) EL(JJ),DP1
8     FORMAT(/ 6X,34HLEAST SELF-CONSISTENT FUNCTION IS ,A3,
     1   27H :WEIGHTED MAXIMUM CHANGE =,1PD10.2)
100   Z2=1.3*Z2
18    PRINT 13
13    FORMAT(10X/' SCF ITERATIONS HAVE CONVERGED TO THE ABOVE ACCURACY')
      WRITE(6,101) -EKK
101   FORMAT(1X,'FOR ENERGY K*K =', F10.5)
      WRITE(PRI,13)
      PRINT '($,A)',' Do you wish to continue ? (Y/N) '
      READ(IN,'(A)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
         PRINT '($,A)',' Enter the additional iterations and new IC '
         READ(IN,'(2I)') NSCF,IC
      CALL UPDATE
         GO TO 19
      END IF
      FAIL = .TRUE.
C
C  *****  PERFORM FINAL CALCULATIONS
C
17    ACFG = D0
      CALL CDIAG(ETOTAL,ACFG,.TRUE.)
      CALL SUMMRY(DELTA)
      CALL OUTPUT(PRINT,DELTA)
      NIT = NWF - IB + 1
      WRITE(PRI, 105) NIT, DP1
105   FORMAT(//10X,'NUMBER OF FUNCTIONS ITERATED          =',I6/
     1         10X,'MAXIMUM WEIGHTED CHANGE IN FUNCTIONS  =',D10.2)
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-32      S E A R C H
C     ------------------------------------------------------------------
C
C       This routine searches for the NJ>70 such that YR(j) > 0 for all
C   j > NJ.
C
C
      SUBROUTINE SEARCH(NJ,I)
        IMPLICIT REAL*8(A-H,O-Z)
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
      IA = 280
      IL = NO
4     IF (YR(IA) .LT. D0) GO TO 3
      IA = IA + 2
      IF (IA .LT. IL ) GO TO 4
      NJ = MAX0(280,MAX(I)-400)
      RETURN
3     NK = (IA + IL)/2
      IF (YR(NK) .LT. D0) GO TO 1
      IL = NK
      GO TO 2
1     IA = NK
2     IF (IL - IA .GT. 1) GO TO 3
      NJ = IL - 28 
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-34      S O L V E
C     ------------------------------------------------------------------
C
C       When FIRST is .TRUE., SOLVE computes the potential and exchange
C   function and initializes variables for the i'th radial  equation.
C   The vector P1 is the solution of the radial equation and P2 the
C   variation of the solution with respect to the energy parameter
C   E(I,I).
C
C
      SUBROUTINE SOLVE(I,FIRST)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :      ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON P2(880),HQ(880),XX(880),AC(20,20),BC(20),JV(20),
     :     AZZ,PP,FN,EM,FM,EU,FU,DELTAE,M,NODE,MK,KK,NJ
*
	LOGICAL FIRST
      DIMENSION ZERO(880),P1(880)
      EQUIVALENCE (ZERO(1),XX(1)),(PDE(1),P1(1))
C
C  *****  IF FIRST IS 'TRUE', CALL POTL AND XCH AND SET UP ARRAYS
C
      IF (.NOT. FIRST) GO TO 17
      CALL POTL(I)
      CALL XCH(I,3)
      ZINF = DMAX1(0.05D0, Z-YR(ND))
      FN = N(I)
      FL = L(I)
      V = YR(1)/R(1)
      B4 = Z*(FL+D4/D3)/((FL+D1)*(FL+D2))
      CN = (D2*Z/FN)**(L(I) +1)
      C = D4*FL +D6
      CD = (FL+D5)**2
      XY = X(1)
      XP = X(2)
      ED = E(I,I)
      X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      X4 = X(4)
      DO 1 J = 3,ND
      X5 = X(J+2)
      X(J) =CH*(-X5+24.D0*(X4+X2) + 194.D0*X3 - X1)/20.D0
      X1 = X2
      X2= X3
      X3 = X4
1     X4 = X5
      X(NO-1) = CH*(X4 + D10*X3 + X2)
      DO 4 J = 1,NO
4     YK(J) = -D2*(Z - YR(J))*R(J) + CD
      X1 =    CH*P(1,I)*(YK(1)+ED*RR(1))
      X2 =    CH*P(2,I)*(YK(2)+ED*RR(2))
      X3 =    CH*P(3,I)*(YK(3)+ED*RR(3))
      X4 =    CH*P(4,I)*(YK(4)+ED*RR(4))
      DO 7 J = 3,ND
      X5 =    CH* P(J+2,I)*(YK(J+2)+ED*RR(J+2))
      X(J) = X(J) - (X5 -D4*(X2 + X4) + D6*X3 +X1)/20.D0
      X1 = X2
      X2 = X3
      X3 = X4
7     X4 = X5
      RL = L(I) + 2.5
      X(2) = R(2)**RL*(X(5)/R(5)**RL - D3*(X(4)/R(4)**RL -
     1     X(3)/R(3)**RL))
C
C  *****  DETERMINE LOWER BOUND ON THE ENERGY PARAMETER
C
      IF (KK .NE. 3) GO TO 80
      DO 11 JJ = 15,ND
      J = NO - JJ
      IF (YK(J) .LT. D0 ) GO TO 63
11    CONTINUE
      WRITE(OUT,12)
12    FORMAT(10X,'POTENTIAL FUNCTION TOO SMALL - 2R*(Z-Y)<(L+.5)**2')
C     STOP
      GO TO 80
63    EM = -YK(J)/RR(J)
      GO TO 81
80    EM = (ZINF/(FN + D5))**2
81    FM = EM
C
C  *****  DETERMINE DIAGONAL ENERGY PARAMETER
C
      F1 = D0
      C11 = D0
      M = MIN0(MAX(I),NO-1)
      DO 5 J = 2,M
      FNUM = P(J+1,I) - P(J,I) - P(J,I) + P(J-1,I)
      FNUM = FNUM - CH*(YK(J+1)*
     1   P(J+1,I) + D10*YK(J)*P(J,I) + YK(J-1)*P(J-1,I))-X(J)
      DEL1 = RR(J+1)*P(J+1,I) + D10*RR(J)*P(J,I) + RR(J-1)*P(J-1,I)
      F1 = F1 +P(J,I)*FNUM
      C11 = C11 + P(J,I)*DEL1
5     CONTINUE
      ED = F1/(C11*CH)
      IF (ED .GT. EM) GO TO 19
C
C  *****  ERROR MESSAGE AND ENERGY ADJUSTMENT FOR AN ENERGY PARAMETER
C  *****  TOO SMALL FOR THE RANGE OF THE FUNCTION
C
      WRITE(OUT,24) ED
24    FORMAT(10X,5HED = ,F10.6,36H; ADJUSTED TO ALLOWED MINIMUM ENERGY )
      ED = EM
      IF ( DABS(FM - E(I,I)) .GT. 1.D-6 .OR. KK .EQ. 3 ) GO TO 19
C
C  ***** RETURN HYDROGENIC FUNCTION
C
      PN = HNORM(N(I),L(I),ZINF)
      DO 65 J = 1,NO
65    PDE(J) = PN*HWF(N(I),L(I),ZINF,R(J))/R2(J)
      AZD = PN*(D2*ZINF/N(I))**(L(I)+1)
      PP = D0
      WRITE(OUT,66) EL(I), ZINF
66    FORMAT(//10X, 'RETURN HYDROGENIC FUNCTION FOR ',A3,
     1   ' WITH EFFECTIVE CHARGE ',F10.3)
      RETURN
C
C  *****  CHECK IF UPPER BOUND IS CORRECT
C
19    IF ( D10*ED .LT. EU) GO TO 18
      EU = D10*ED
      FU = EU
18    AZD = AZ(I)
17    DO 26 J=1,NO
      YR(J) = (YK(J) + ED*RR(J))*CH
26    ZERO(J) = D0
C
C  *****  SEARCH FOR THE POINT AT WHICH YR BECOMES POSITIVE
C
      CALL SEARCH(NJ,I)
C
C  *****  COMPUTE STARTING VALUES FROM SERIES EXPANSION
C
      B3 = (V + V + ED - (Z/FN)**2)/C
      DO 6 J = 1,2
      HW  = HWF(N(I),L(I),Z,R(J))/CN
6     HQ(J)   = AZD*(HW + R(J)**(L(I)+3)*B3*(D1-R(J)*B4))/R2(J)
C
C  *****  OBTAIN HOMOGENEOUS SOLUTION
C
      CALL NMRVS(NJ,DELH,MH,HQ,ZERO)
      P1(1) = HQ(1) + XY/C
      P1(2) = HQ(2) + XP/C
C
C  *****  OBTAIN PARTICULAR SOLUTION
C
      CALL NMRVS(NJ,DEL1,M1,P1,X)
C
C  *****  DETERMINE THE ENERGY ADJUSTMENT REQUIRED FOR A SOLUTION WITH
C  *****  GIVEN A0
C
      M = MAX0(M1,MH)
      PNORM = D0
      DO 50 J = 1,M
50    PNORM = PNORM + RR(J)*HQ(J)*P1(J)
      Y1 = P1(NJ-1)
      Y2 = P1(NJ)
      Y3 = P1(NJ+1)
      DELTA = Y2 - Y1 + Y2 - Y3 +YR(NJ-1)*Y1 + D10*YR(NJ)*Y2
     1   + YR(NJ+1)*Y3 + X(NJ)
      DELTAE = HQ(NJ)*DELTA/(H*H*PNORM)
      PP = -DEL1/DELH
C
C  *****  MATCH AT THE JOIN FOR A SOLUTION OF THE DIFFERENTIAL EQUATION
C
      DO 13 J = 1,NO
      P1(J)   = P1(J) + PP*HQ(J)
13    CONTINUE
C  *****  IF  THE EQUATIONS APPEAR TO BE NEARLY
C  ****  SINGULAR, SOLVE THE VARIATIONAL EQUATIONS
C
      IF (KK .NE. 2) RETURN
      X1 = P(1,I)*RR(1)
      X2 = P(2,I)*RR(2)
      P2(1) = X1/C
      P2(2) = X2/C
      DO 8 J = 3,NO
      X3 = P(J,I)*RR(J)
      XX(J-1) = (D10*X2 + X1 + X3)*CH
      X1 = X2
8     X2 = X3
      CALL NMRVS(NJ,DEL2,M2,P2,XX)
      AA = -DEL2/DELH
      M = MAX0(M,M2)
      DO 9 J = 1,NO
9     P2(J) = P2(J) + AA*HQ(J)
      A11 = QUAD(I,M,P2,P2)
      B11 = QUAD(I,M,P1,P2)
      C11 = QUAD(I,M,P1,P1) - D1
      DISC = B11*B11 - A11*C11
      IF ( DISC .LT. D0 ) GO TO 70
      DE1 = -(B11+DSQRT(DISC))/A11
      DE2 = C11/A11/DE1
      IF ( P1(3)+DE1*P2(3) .LT. D0) DE1 = DE2
      GO TO 71
70    DE1 = C11/A11
71    DO 301 J = 1,NO
      P1(J) = P1(J) + DE1*P2(J)
301   CONTINUE
      PP = PP + DE1*AA
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-12      G R A N G E
C     ------------------------------------------------------------------
C
C       Controls the calculation of off-diagonal energy parameters.
C   It searches for all pairs (i,j) which are constrained through  an
C   orthogonality requirement.   When one of the pair , say P
C                                                            i
C   must be orthogonal not only to P  but also to P  where n = n ,
C                                   j              k        j   k
C   a system of equations must be solved, the exact form depending on
C   whether or not any of the functions are part of the frozen  core.
C   When  only one pair with a given set of principal quantum numbers
C   is present, ELAGR(I,J) is used to  determine  the  off-  diagonal
C   energy  parameters  as  long  as  |q  -q | > 0.05.  Otherwise Eq.
C                                       i   j
C   (7-10) modified for configuration interaction is used.
C
C
      SUBROUTINE GRANGE
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
	COMMON W(500,500),U(500),DWT(500),AC(20,20),BC(20),JV(20),IV(20)
        COMMON /PARM1/ ICASE, IPTYP(30), IORTHO
	LOGICAL DIAG, SETEQL, FIRST
*
C
C  *****  FOR EACH l COMPUTE OFF-DIAGONAL ENERGY PARAMETERS
C
	DO 10 IL = 0,5
	   IJ = 0
   	   DO 11 I = IB,NWF
	   IF ( L(I) .NE. IL ) GO TO 11
	   DO 12 J = 1,I-1
	      IF (DABS(E(I,J)) .GT. 1.D-10 ) THEN
		 IJ = IJ + 1
		 IF ( IJ .GT. 20) STOP '  TOO MANY LAGRANGE MULTIPLIERS'
	  	 IV(IJ) = I
 		 JV(IJ) = J
	      END IF
12         CONTINUE
11	   CONTINUE
C
C  ***** IJ IS THE NUMBER OF LAGRANGE MULTIPLIERS FOR l = IL
C
           IF (IJ .EQ. 0) GO TO 10
	   DO 13 II = 1,IJ
	      BC(II) = D0
	      DO 14 III = 1,IJ
		 AC(II,III) = D0
14	      CONTINUE
13	   CONTINUE
	   DO 16 I = IB,NWF
	      IF ( L(I) .NE. IL ) GO TO 16
		 FIRST = .TRUE.
		 DO 18 II = 1,IJ
		    J = 0
	            IF ( IV(II) .EQ. I) THEN
		       J = JV(II)
		     ELSE IF ( JV(II) .EQ. I) THEN
		       J = IV(II)
		    END IF
	            IF ( J .NE. 0) THEN
		       IF (FIRST) THEN
*            WRITE(*,*)'GRANGE:WT(IH),WT(JH)',WT(IH(I)),WT(JH(I))
			  CALL XCH(I,2)
* ###
*                          DO 59 JJ = 1,NO
*                            X(JJ) = D0
*59                        CONTINUE 

*            WRITE(*,*)'GRANGE:WT(IH),WT(JH)',WT(IH(I)),WT(JH(I))
			  CALL POTL(I)
			  DO 20 JJ = 1,NO
			     YK(JJ) = YR(JJ)
20			  CONTINUE
			  FIRST = .FALSE.
		       END IF
		       DO 22 JJ = 1,NO
			  YR(JJ) = P(JJ,J)
22		       CONTINUE
* ###
                       IF (ICASE .EQ. 1) THEN
		         BC(II) = BC(II) + HL(EL,I,J,REL)
     :	                            -D2*QUADS(I,J,1)-QUAD(J,NO,YR,X)
                       ELSEIF (ICASE .EQ. 2) THEN
                         BC(II) = BC(II) + HL(EL,I,J,REL)
     :                              +D2*QUADS(I,J,1)-QUAD(J,NO,YR,X)
                       ENDIF
		    END IF
18 		CONTINUE
16         CONTINUE
	   DO 24 II = 1,IJ
	      DO 26 III = 1,II
		 IF ( II .EQ. III) THEN
		    AC(II,II) = D1/SUM(IV(II))
		    IF (JV(II) .GE. IB) THEN
		       AC(II,II) = AC(II,II) + D1/SUM(JV(II))
		    END IF
		  ELSE IF (IV(II) .EQ. IV(III) .AND.
     :                    E(JV(II),JV(III)) .EQ. D0 ) THEN
		     AC(II,III) = QUADR(JV(II),JV(III),0)/SUM(IV(II))
		     AC(III,II) = AC(II,III)
		     DIAG = .FALSE.
	 	  ELSE IF (JV(II) .EQ. JV(III) .AND. JV(II) .GE. IB
     :              .AND. E(IV(II),IV(III)) .EQ. D0) THEN
		     AC(II,III) = QUADR(IV(II),IV(III),0)/SUM(JV(II))
		     AC(III,II) = AC(II,III)
		     DIAG = .FALSE.
 		  END IF
26	      CONTINUE
24	   CONTINUE
	   IF ( .NOT. DIAG ) CALL LINEQN(20,IJ,AC,BC)
	   DO 28 II = 1,IJ
              CALL EIJSET(IV(II),JV(II),BC(II)/SUM(IV(II)))
	      IF ( JV(II) .GE. IB )
     :            CALL EIJSET(JV(II),IV(II),BC(II)/SUM(JV(II)))
28	   CONTINUE
10    CONTINUE
C
C  *****  PRINT THE OFF-DIAGONAL ENERGY PARAMETERS
C
	DO 30 I = IB,NWF
	   DO 32 J = 1,I-1
	      IF (DABS(E(I,J)) .GT. 1.D-10) THEN
		 WRITE(OUT,35) EL(I),EL(J),E(I,J),EL(J),EL(I),E(J,I)
35               FORMAT(7X,2(3X,'E(',2A3,') =',1PE12.5))
	      END IF
32	   CONTINUE
30	CONTINUE
	RETURN
	END
C
C     ------------------------------------------------------------------
C    3-22      O R T H O G
C     ------------------------------------------------------------------
C
C       This routine orthogonalizes the set of radial functions when an
C   orthogonality constraint applies.  A Gram-Schmidt type of  process
C   is used.  When more than one radial function with a given (nl) is
C   present, it may be necessary to solve a 2x2 system of equations.
C
C
      SUBROUTINE ORTHOG
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON /PARM1/ ICASE, IPTYP(30), IORTHO
*
	LOGICAL DIAG
	COMMON AC(20,20),BC(20)
*
      IF (NWF .EQ. 1 .OR. IB .GT. NWF) RETURN
      II = MAX0(2,IB)
      DO 2 I = II,NWF
         DIAG = .TRUE.
	 IBEGIN = IEPTR(I-1)+1
	 IP = IBEGIN
	 IJ = 0
 60	 JV = IJE(IP)
	 IF (JV .LT. I .AND. IP .LE. IEPTR(I)) THEN
	    IJ = IJ+1
	    IF ( IJ .GT. (20)) STOP ' TOO MANY ORTHOGONALITY CONDITIONS'
            BC(IJ) = QUADR(I,JV,0)
*            WRITE (6,*) 'IJ, BC(IJ) = ', IJ, BC(IJ)
            AC(IJ,IJ) = D1
            DO 62 JJ = IBEGIN,IP-1
               IF (E(IJE(IP),IJE(JJ)) .NE. D0 ) THEN
                  AC(IJ,JJ) = D0
                  AC(JJ,IJ) = D0
                ELSE
                  AC(IJ,JJ) = QUADR(IJE(IP),IJE(JJ),0)
                  AC(JJ,IJ) = AC(IJ,JJ)
                  DIAG = .FALSE.
               END IF
62          CONTINUE
	    IP = IP+1
	    GO TO 60
	 END IF
      IF ( IJ .GT. 0) THEN
         IF ( .NOT. DIAG .AND. IJ.GT.1) CALL LINEQN(20,IJ,AC,BC)
         M = MAX(I)
         AZZ = AZ(I)
	 IP = IBEGIN
	 CTOTAL = D0
         DO 65 JJ = 1,IJ
            C = BC(JJ) 
* ###
            IF (IORTHO .EQ. 1) C = D0
            IF (DABS(C) .GT. 1.D-10) THEN
               WRITE(OUT,63) EL(IJE(IP)),EL(I),C
63             FORMAT(6X,'ORTHOG <',A3,'|',A3,'>=',G12.5)
               IF (METH(I) .NE. 4) M = MAX0(M,MAX(IJE(IP)))
               DO 64 J = 1,M
                  P(J,I) = P(J,I) - C*P(J,IJE(IP))
64             CONTINUE
*               write (6,*) 'I, J, IJE(IP) = ', I, J, IJE(IP)
*               write (6,*) 'QUADR(I,IJE(IP),0) = ', QUADR(I,IJE(IP),0)
               AZZ = AZZ - C*AZ(IJE(IP))
	    END IF
	    IP = IP + 1
	    CTOTAL = CTOTAL + ABS(C)
65       CONTINUE
*         write (6,*) 'CTOTAL, METH(I) = ', CTOTAL, METH(I)
	 IF (CTOTAL .GT. 1.D-10 ) THEN
	  IF (METH(I) .NE. 4) THEN
            PNN = DSQRT(QUADR(I,I,0))
            DO 66 JJ = 1,M
                  P(JJ,I) = P(JJ,I)/PNN
66          CONTINUE
            AZZ = AZZ/PNN
            M = NO
67          IF (DABS(P(M,I)) .LT. 1.D-15) THEN
                  P(M,I) = D0
                  M = M-1
                  GO TO 67
            END IF
          END IF
          MAX(I) = M
          AZ(I) = AZZ
	  VARIED(I) = .TRUE.
	 END IF
      END IF
2     CONTINUE
      END
C
C     ------------------------------------------------------------------
C    3-35      S U M M R Y
C     ------------------------------------------------------------------
C
C       The results of a calculation are summarized.   These include
C   the following for each electron:
C
C          E(NL)   - diagonal energy parameter
C          AZ(NL)  - starting parameter, P(r)/r**(l+1) as r -> 0.
C          SIGMA   - screening parameter as defined by Eq. (6-  ).
C          1/R**3  - expected value of <1/r**3>
C          1/R     - expected value of <1/r>
C          R       - expected mean radius
C          R**2    - expected value of <r**2>
C          I(NL)   - -(1/2)<nl|L|nl>
C          KE      - I(NL) + Z <r>
C          REL     - Relativistic shift (mass-velocity, Darwin term,
C                    spin-spin contact term)
C
C   These results are followed by:
C
C          TOTAL ENERGY--RELATIVISTIC OR NON-RELATIVISTIC (ET)
C          KINETIC ENERGY-- NON-RELATIVISTIC (EN)
C          POTENTIAL ENERGY (EP) = ET - EN
C          RATIO                 = - EP/EN
C                      k   k   k
C   The values of all F , G , R  and <nl|L|n'l> integrals which enter
C   into the calculation are printed, but only if OUD > 0.
C
C
      SUBROUTINE SUMMRY(DELTA)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
      COMMON R3(30),SS(3),R1,RM,RMM,RH,SC,QI,QJ,SP,C,CC,
     1   EKINP,EN,EPOT,RATIO,LI,LJ,K,KF,I1,I2,J1,J2,I,J,MIN
      CHARACTER*1 SYMBOL
*
      WRITE(PRI,9) ATOM,TERM
9     FORMAT(/// 24X,5HATOM ,A6,3X,5HTERM ,A6//63X,13HMEAN VALUE OF,21X,
     1   22HONE ELECTRON INTEGRALS /5X,2HNL,7X,5HE(NL),9X,6HAZ(NL),
     2   5X,5HSIGMA,7X,6H1/R**3,5X,3H1/R, 8X,1HR, 8X,4HR**2, 8X,
     3   11HI(NL)/DELTA,5X,3HREL)
      EN = D0
C
C  *****  COMPUTE AND PRINT ONE-ELECTRON PARAMETERS
C
      DO 10 I = 1,NWF
      R1 = D0
      RM = D0
      RMM = D0
      EK = DELTA
      S(I) = D0
      IF (METH(I) .NE. 4) THEN
         R1 = QUADR(I,I,-1)
         RM = QUADR(I,I,1)
         RMM = QUADR(I,I,2)
* ###
         EK = -D5*HL(EL,I,I,REL)
         RH = 3*N(I)*N(I) - L(I)*(L(I) + 1)
         SC = Z - D5*RH/RM
         S(I) = SC
      R3(I) = D0
      END IF
      IF (L(I) .NE. 0) R3(I) = QUADR(I,I,-3)
      RELS = ASHIFT(I,I)
      WRITE(PRI,15)EL(I),E(I,I),AZ(I),SC,R3(I),R1,RM,RMM,EK,RELS
15    FORMAT( 4X,A3,F14.7,F15.7,F8.3,F13.4,F11.5,F10.5,F11.6,3F13.6)
10    CONTINUE
C
C  *****  PRINT TABLES OF 'FK' AND 'GK' INTEGRALS WHICH WERE USED IN
C  *****  DETERMINING THE ENERGY
C
      IF ( OUD .EQ. 0 ) GO TO 13
      WRITE (OUD,126)
126   FORMAT(//2X,27HVALUES OF F AND G INTEGRALS        //)
      IBEGIN = 1
      IEND = INTPTR(2)
      DO 17 I = IBEGIN,IEND
         SYMBOL = 'F'
         IF (I .GT. INTPTR(1)) SYMBOL = 'G'
17	 WRITE(OUD,19) SYMBOL,KVAL(I),EL(IEL(I,1)),EL(IEL(I,2)),VALUE(I)
19       FORMAT( 2X,A1,I2,1H(,A3,1H,,A3,4H ) =, F10.7)
C
C  *****  PRINT TABLES OF 'RK' INTEGRALS
C
      WRITE (OUD,21)
21    FORMAT(//2X,21HVALUES OF R INTEGRALS  //)
      IBEGIN = INTPTR(4) + 1
      IEND = INTPTR(5)
      DO 22 I = IBEGIN,IEND
      I1 = IEL(I,1)
      I2 = IEL(I,2)
      J1 = IEL(I,3)
      J2 = IEL(I,4)
22    WRITE (OUD,23) KVAL(I),EL(I1),EL(I2),EL(J1),EL(J2),VALUE(I)
23    FORMAT(2X,1HR,I2,1H(,2A3,1H,, 2A3,3H) =, F11.7 )
C
C  *****  PRINT TABLES OF 'L' INTEGRALS
C
      WRITE (OUD,28)
28    FORMAT(//2X,21HVALUES OF L INTEGRALS //)
      IBEGIN = IEND + 1
      IEND = INTPTR(6)
      DO 29 I = IBEGIN,IEND
29    WRITE(OUD,30) EL(IEL(I,1)),EL(IEL(I,2)),VALUE(I)
30    FORMAT(2X,2HL(,A3,1H,,A3,4H) = ,F12.7)
13    RETURN
      END
