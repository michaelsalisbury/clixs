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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
        LOGICAL PRINT,LD
        CHARACTER ANS*1
        REAL TIME
      EQUIVALENCE (IUC,IU(1)),(OUC,OU(1))
*
*  ***** Define unit numbers and open files *********************
*                                                               *
*        UNIT NUMBERS AND FILE NAMES MAY BE MACHINE             *
*        DEPENDENT. CHECK THE FOLLOWING SECTION.                *
*                                                               *
*        IN - Standard input unit, normally the terminal        *
*        OUT- Standard output unit, normally the terminal       *
*        PRI- Printer output unit or file.                      *
*                                                               *
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
*                                                               *
*  ***** IN THE OPEN STATEMENTS CHECK FOR VALID FILE NAMES ******
*                                                               *
1     PRINT '(//A/A//)', ' START OF CASE',' ============='
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
*           ... The input unit numbers are 21, 22, 23 ...
                  IU(I) = 20 + I
                  IF (I .EQ. 1) THEN
                     OPEN(UNIT=IUC,FILE='CFG.INP',STATUS='OLD')
                    ELSE IF (I .EQ. 2) THEN
                     OPEN(UNIT=IUD,FILE='INV.LST',STATUS='OLD')
                    ELSE
                     OPEN(UNIT=IUF,FILE='WFN.INP',STATUS='OLD',
     :                     FORM='UNFORMATTED',RECL=128)
                  END IF
            END IF
         END IF
         IF (OU(I) .EQ. 1) OU(I) = OUT
         IF (OU(I) .EQ. 2) THEN
*           ... The output unit numbers are 31, 32, 33, 34 ...
            OU(I) = 30 + I
            IF (I .EQ. 1) THEN
               OPEN(UNIT=OUC,FILE='CFG.OUT',STATUS='NEW')
              ELSE IF (I .EQ. 2) THEN
               OPEN(UNIT=OUD,FILE='FGR.VAL',STATUS='NEW')
              ELSE IF (I .EQ. 3) THEN
               OPEN(UNIT=OUF,FILE='WFN.OUT',STATUS='NEW',
     :               FORM='UNFORMATTED')
              ELSE
               OPEN(UNIT=OUH,FILE='MTX.OUT',STATUS='NEW')
            END IF
         END IF
    3 CONTINUE
*
*  ***** END OF INPUT/OUTPUT INTERFACE **************************
*
*        The following is a non-standard procedure for timing a
*       calculation.  It may be deleted or replaced.
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
      CALL DATA
C
C
C  ***** REWIND WAVEFUNCTION FILE FOR NEXT CASE
C
         IF (IU(3) .GT. 10) REWIND(UNIT=IU(3))
C
C  *****  SET PARAMETERS TO THEIR DEFAULT VALUE
C
13    PRINT = .FALSE.
      CFGTOL = 1.D-10
      SCFTOL = 1.D-7
      NSCF = 12
      IC = 0
      IF (NCFG .EQ. 1) IC = 3 + (NWF+1-IB)/4
      ACFG = D0
      LD = .TRUE.
      TRACE = .FALSE.
      PRINT '(/$,A)',' Default values for other parameters ? (Y/N) '
      READ (IN,'(A)') ANS
      IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') THEN
         PRINT '(/$,A,A)',' Default values for',
     :   ' PRINT, CFGTOL, SCFTOL ? (Y/N) '
         READ(IN,'(A)') ANS
         IF ( ANS .NE. 'Y' .AND. ANS .NE. 'y'  ) THEN
            PRINT '(A)',' Input free FORMAT(L, F, F) '
            READ(IN,'(L,F,F)') PRINT, CFGTOL, SCFTOL
         END IF
         PRINT '(/$,A)', ' Default values for NSCF, IC ? (Y/N) '
         READ(IN,'(A)') ANS
         IF (ANS .NE. 'Y' .AND. ANS .NE. 'y' ) THEN
            PRINT '($,A)', ' Input free FORMAT(I, I) '
            READ(IN,'(2I)') NSCF, IC
         END IF
         PRINT '(/$,A)', ' Default values for ACFG,LD,TRACE ? (Y/N) '
         READ(IN,'(A)') ANS
         IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') THEN
             PRINT '($,A)', ' Input free FORMAT( F, L, L) '
             READ(IN,'(F,2L)') ACFG,LD,TRACE
         END IF
      END IF
      WRITE(OUT,2) PRINT,CFGTOL,SCFTOL,NSCF,IC,ACFG,ID,LD,TRACE
2     FORMAT(/L3,2D6.1,2I3,F3.1,I3,2L3)
C
C  *****  PERFORM THE MCHF ITERATION
C
      CALL SCF(ACFG,SCFTOL,CFGTOL,LD)
C
C  *****  OUTPUT RESULTS IF PRINT = .TRUE.
C
      CALL OUTPUT(PRINT)
15    IF (FAIL) GO TO 6
      CALL SUMMRY
C
C  *****  CHECK FOR ISOELECTRONIC SEQUENCE OR END OF CASE.
C
      PRINT '(/$,A)', ' Do you wish to continue along the sequence ? '
      READ(IN,'(A1)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ.'y') THEN
          PRINT *, ' ATOM, ZZ, (ACC(I),I=1,NWF) in ',
     ;             ' format(A6,F6.0,(20F3.1))'
          READ(IN,'(A6,F6.0,(20F3.1))') ATOM, ZZ, (ACC(I),I=1,NWF)
          WRITE(OUT,10) ATOM,ZZ,(ACC(I),I=1,NWF)
10        FORMAT(1X,A6,F6.0,(20F3.1))
C
C  *****  SCALE RESULTS FOR ANOTHER MEMBER OF THE ISOELECTRONIC SEQUENCE
C
          CALL SCALE(ZZ)
          WRITE(PRI,14) ATOM,TERM
14        FORMAT(1H1,9X,2A6)
          CALL ORTHOG
          GO TO 13
      END IF
C
C  *****  DETERMINE END OF CASE
C
6     PRINT '(//A/A//A,F8.3,A//)', ' END OF CASE',' ===========',
     :      ' Total CPU time was ', TIME/60,' minutes'
      PRINT '(//$,A)', ' Do you wish to start another case ? (Y/N) '
      READ(IN,'(A)') ANS
      IF ( ANS .EQ. 'Y' .OR. ANS .EQ. 'y' ) GO TO 1
      STOP
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
      SUBROUTINE DATA
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
     :        ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM)
*     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
        COMMON ZZ(30),IND(30),IELI(5),NOCCSH(500)
*
        LOGICAL SETORT,FIRST,STRONG
        CHARACTER*3 EL1,EL2,EL3,EL4,EL5,EL6,ELCLSD(18),ELORT(10,2),
     :              OF(30),ELI(5),ANS*1,STRING*40
*
C
    1 FORMAT(18(1X,A3))
    7 FORMAT(A3,F6.0,I3,I3,F3.1)
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
         READ(IUC,'(/18(1X,A3))') (ELCLSD(I),I=1,18)
         NCFG = 0
    3    READ(IUC,'(A,F)',END=10) STRING,W
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
      IF ( NCFG .GT. 1) THEN
         PRINT '(/$,A,A)',' Is this an MCHF calculation (Y) or a basis',
     :                  ' calculation (N) ? (Y/N) '
         ID = 0
         READ(IN,'(A)') ANS
         IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') ID = 1
      END IF
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
            END IF
            J = J+8
            IF (J .LT. 40) GO TO 16
         END IF
         NOCCSH(NC) = I
   15 CONTINUE
C
C  *****  The list of electrons has been determined
C
      PRINT 19, MAXORB,(OF(J),J=1,MAXORB)
   19 FORMAT(/' There are ',I3,' orbitals as follows:'/(1X,18(1X,A3)))
      NWF = MAXORB
      PRINT '(/$,A)', ' Are all orbitals to be varied ? (Y/N) '
      READ '(A)', ANS
      IF ( ANS .NE. 'Y' .AND. ANS .NE. 'y' ) THEN
          PRINT '($,A)',' Enter number to be varied (last in the list) '
         READ '(I)', NIT
        ELSE
         NIT = NWF
      END IF
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
            IF ( ID .EQ. 1) METH(I) = 1
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
         N(I) = ICHAR(EL(I)(J-1:J-1)) - ICHAR('1') + 1
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
            IF (NC.EQ.1 .AND. IQ.NE.D0 .AND. METH(IELI(I)).EQ.3
     :          .AND. (ANS.EQ.'Y' .OR. ANS.EQ.'y'))
     :        METH(IELI(I)) = 1
            J = J+8
            IF (J .LT. 40) GO TO 30
         END IF
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
                IF ((I.LE.NCLOSD .AND. J.LE.NCLOSD)
     :                          .OR. (ID .NE. 0))  C = 1.D-10
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
*        Clear the VIJ data-structure
*
      IBEGIN = 1
      IEND = IEPTR(NWF)
      DO 45 I = IBEGIN,IEND
        VIJ(I) = D0
 45   CONTINUE
C
C  *****  ADDITIONAL PARAMETERS
C
   50 PRINT '(/$,A)', ' Default values (NO,REL,STRONG) ? (Y/N) '
      READ(IN,'(A)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
         NO = (880) - 20
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
C *****  WRITE 'CONFIGURATION' CARDS  AND NORMALIZE THE WEIGHTS
C
      W = D0
      DO 63 I=1,NCFG
   63 W = W + WT(I)**2
C
C  *****  IF INPUT WEIGHTS ARE ALL ZERO, SET EACH = 1/SQRT(NCFG)
C
      IF (W .NE. D0) GO TO 66
      W = NCFG
      DO 65 I=1,NCFG
65    WT(I) = D1
66    W = DSQRT(W)
      DO 68 I = 1,NCFG
      WT(I) = WT(I)/W
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
 80     CONTINUE
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON P2(880),HQ(880),XX(880),AC(20,20),BC(20),JV(20),
     :     AZZ,PP,FN,EM,FM,EU,FU,DELTAE,M,NODE,MK,KK,NJ
*
        LOGICAL DIAG
        CHARACTER*2 ASTER(3)
        DATA  ASTER /'  ','* ','**'/
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
*               The JI orbital should be orthogonalized
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
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
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
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
      IF (DABS(PP(M))+DABS(PP(M-1)) .GT. TOL .OR. K .LT. 9) GO TO 22
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
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
      DIFF1 = PDE(40) - PDE(39)
      DO 2 J = 40, MM
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
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
*         EKI retained only for compatibility with MCHF format
*
      DO 3 I = 1,NWF
      MMX = MAX(I)
      WRITE (OUF) ATOM,TERM,EL(I),MMX,Z,E(I,I),EKI,AZ(I),
     :   (P(J,I),J=1,MMX)
3     CONTINUE
C
14    RETURN
      END
C
C     ------------------------------------------------------------------
C    3-31      S C A L E
C     ------------------------------------------------------------------
C
C       The current radial functions are scaled according to the
C   procedures of Sec. 7-2  .   Values of AZ and E(I,I), the starting
C   values and the diagonal energy parameters are also scaled.
C
C
      SUBROUTINE SCALE(ZZ)
        IMPLICIT REAL*8(A-H,O-Z)
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
      COMMON RATIO,SR,SC,SS,F0,F1,F2,F3,PNORM,THETA,K
      DIMENSION RS(880),PS(880)
      EQUIVALENCE (RS(1),YR(1)),(PS(1),X(1))
C
C  *****  SCALE VALUES OF R=RS, P=PS AND ONE-ELECTRON PARAMETERS.
C  *****  GENERATE NEW VALUES OF R, R*R, AND SQRT(R)
C
      RATIO = Z/ZZ
      SR = DSQRT(RATIO)
      DO 1 J = 1,NO
      R(J) = R(J)*RATIO
      RR(J) = R(J)*R(J)
1     R2(J) = R2(J)*SR
      DO 2 I = 1,NWF
      SC = (ZZ-S(I))/(Z-S(I))
      SS = SC*RATIO
      ED = E(I,I)*SC**2
      CALL EIJSET(I,I,ED)
      DO 3 J = 1,NO
      RS(J) = R(J)/SS
3     PS(J) = P(J,I)*SC
      SC = (ZZ - D5*S(I))/(Z - D5*S(I))
      AZ(I) = AZ(I)*SC**(L(I)+1)*DSQRT(SC)
      K = 3
C
C  *****  INTERPOLATE THE (RS,PS) FUNCTIONS FOR VALUES OF P AT THE SET
C  *****  OF POINTS R
C
      DO 4 J = 1,NO
C
C  *****  SEARCH FOR THE NEAREST ENTRIES IN THE (RS,PS) TABLE
C
5     IF (K .EQ. ND) GO TO 7
      IF (RS(K) .GT. R(J)) GO TO 6
      K = K + 1
      GO TO 5
C
C  *****  INTERPOLATE
C
6     THETA = DLOG(R(J)/RS(K-1))/H
      F0 = PS(K-2)
      F1 = PS(K-1)
      F2 = PS(K)
      F3 = PS(K+1)
      P(J,I) = D5*(F1+F2) + (THETA -D5)*(F2 - F1) +
     1   THETA*(THETA - D1)*(F0 - F1 - F2 + F3)/D4
      GO TO 4
7     P(J,I) = D0
4     CONTINUE
      MAX(I) = NO
C
C  *****NORMALIZE THE INTERPOLATED FUNCTION
C
      PNORM = DSQRT(QUADR(I,I,0))
      DO 10 J = 1,NO
10    P(J,I) = P(J,I)/PNORM
2     CONTINUE
      Z = ZZ
      RETURN
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
C          CFGTOL          1.D-10
C          SCFTOL          1.D-7
C          IC              (NWF + 1 - IB)/4 + 3
C          NSCF            12
C
C   The self-consistency convergence criterion is
C
C          Z2 = SQRT( SCFTOL*(Z*NWF/2) )
C
C   It is increased by a factor two at the end of each iteration whereas
C   CFGTOL is increased by SQRT(2).
C
C
      SUBROUTINE SCF(ACFG,SCFTOL,CFGTOL,LD)
        IMPLICIT REAL*8(A-H,O-Z)
        CHARACTER ANS*1
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
        COMMON /MATRIX/ETOTAL,W(500,500)
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        LOGICAL LAST,LD,CONV,ECONV
C
C  *****  SET THE SCF CONVERGENCE PARAMETER TO AN OPTIMISTIC VALUE
C
      TOL = DSQRT(Z)*1.D-10
      Z2 = SCFTOL*DSQRT(Z*NWF)
      WRITE(OUT,15)
15    FORMAT(//)
      WRITE(OUT,16) OMIT,ACFG,SCFTOL,NO,REL
   16 FORMAT(10X,44HWEAK ORTHOGONALIZATION DURING THE SCF CYCLE=,L4/
     :       10X,44HACCELERATING PARAMETER FOR MCHF ITERATION  =,F5.2/
     :       10X,44HSCF CONVERGENCE TOLERANCE (FUNCTIONS)      =,1PD9.2
     :      /10X,44HNUMBER OF POINTS IN THE MAXIMUM RANGE      =,I4/
     :       10X,44HRELATIVISTIC DIAGONAL  ENERGY CORRECTIONS  =,L4)
C
C  *****  SET ITERATION PARAMETERS
C
      IPR = 0
      ECONV = .FALSE.
      LAST = .FALSE.
      DP1 = D0
      ETOTAL = D0
      EC = D0
      ICYCLE = 0
      CALL UPDATE
      IF ( IB .GT. NWF ) GO TO 17
      IF ( .NOT. LD ) GO TO 9
19    IF ( ID .EQ. 1 .OR. NCFG .EQ. 1) GO TO 9
      CALL DIAG(ECONV,ACFG,CFGTOL,LAST)
C
C  *****  PERFORM NSCF SELF-CONSISTENT FIELD ITERATIONS
C
9     DO 100 I = 1,NSCF
      ICYCLE = ICYCLE + 1
      WRITE(OUT,7) ICYCLE,CFGTOL,Z2
7     FORMAT(//10X,17HITERATION NUMBER ,I2/10X,16H----------------//
     1 10X,50HCONVERGENCE CRITERIA:ENERGY  (CFGTOL)            =,1PD9.1/
     2 11X,49H                   :FUNCTION(SCFTOL*SQRT(Z*NWF))=,1PD9.1/)
      DP1 = D0
      IF (IB .GT. NWF) GO TO 17
      CALL GRANGE
C
C  *****  SOLVE EACH DIFFERENTIAL EQUATION IN TURN
C
      WRITE(OUT,14)
14    FORMAT(/20X,' EL',9X,'ED',13X,'AZ',11X,'NORM',7X,'DPM')
      DO 2 J = IB,NWF
      CALL DE(J)
      IF ( FAIL ) RETURN
      DP = DPM(J)*DSQRT(SUM(J))
      IF ( DP1 .GE. DP ) GO TO 2
      DP1 = DP
      JJ = J
2     CONTINUE
      IF ((NCFG .EQ. 1 .OR. ID .EQ. 1) .AND. DP1 .LT. Z2) GO TO 6
      IF ( IC .LE. 0) GO TO 6
C
C  *****  SOLVE IC DIFFERENTIAL EQUATIONS EACH TIME SELECTING THE
C  *****  ONE WITH THE LARGEST DPM
C
      DO 4 II =1,IC
      CALL DE(JJ)
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
      IF (.NOT.(NCFG .EQ. 1 .OR. ID .EQ. 1)) GO TO 12
      IF (DP1 .LE. Z2 )  LAST = .TRUE.
      GO TO 1
12    CALL DIAG(ECONV,ACFG,CFGTOL,LAST)
C
C  *****  IF FUNCTIONS APPEAR TO HAVE CONVERGED,SOLVE EACH AGAIN, IN
C  *****  TURN, AND TEST AGAIN
C
      CONV = ECONV .AND. DP1 .LE. Z2
      IF (CONV) LAST =.TRUE.
C
C  *****  INCREASE THE CONVERGENCE CRITERION FOR SELF-CONSISTENCY
C
1     Z2 = D2*Z2
      WRITE(OUT,8) EL(JJ),DP1
8     FORMAT(/ 6X,34HLEAST SELF-CONSISTENT FUNCTION IS ,A3,
     1   27H :WEIGHTED MAXIMUM CHANGE =,1PD10.2)
100   CFGTOL = 1.4D0*CFGTOL
18    PRINT 13
13    FORMAT(10X/' SCF ITERATIONS HAVE CONVERGED TO THE ABOVE ACCURACY')
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
      CALL DIAG(ECONV,ACFG,CFGTOL,.TRUE.)
      NIT = NWF - IB + 1
      WRITE(PRI, 105) NIT, DP1, CFGTOL
105   FORMAT(//10X,'NUMBER OF FUNCTIONS ITERATED          =',I6/
     1         10X,'MAXIMUM WEIGHTED CHANGE IN FUNCTIONS  =',D10.2/
     2         10X,'TOLERANCE FOR THE MCHF ITERATION      =',D10.2)
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
      IA = 70
      IL = NO
4     IF (YR(IA) .LT. D0) GO TO 3
      IA = IA + 2
      IF (IA .LT. IL ) GO TO 4
      NJ = MAX0(70,MAX(I)-100)
      RETURN
3     NK = (IA + IL)/2
      IF (YR(NK) .LT. D0) GO TO 1
      IL = NK
      GO TO 2
1     IA = NK
2     IF (IL - IA .GT. 1) GO TO 3
      NJ = IL - 7
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
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
13    P1(J)   = P1(J) + PP*HQ(J)
C
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
      SUBROUTINE SUMMRY
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
        COMMON /MATRIX/ETOTAL,W(500,500)
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
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
      COMMON R3(30),SS(3),R1,RM,RMM,RH,SC,QI,QJ,SP,C,CC,
     1   EKINP,EN,EPOT,RATIO,LI,LJ,K,KF,I1,I2,J1,J2,I,J,MIN
      CHARACTER*1 SYMBOL
*
      WRITE(PRI,9) ATOM,TERM
9     FORMAT(/// 24X,5HATOM ,A6,3X,5HTERM ,A6//63X,13HMEAN VALUE OF,21X,
     1   22HONE ELECTRON INTEGRALS /5X,2HNL,7X,5HE(NL),9X,6HAZ(NL),
     2   5X,5HSIGMA,7X,6H1/R**3,5X,3H1/R, 8X,1HR, 8X,4HR**2, 9X,
     3   5HI(NL),9X,2HKE,10X,3HREL)
      EN = D0
C
C  *****  COMPUTE AND PRINT ONE-ELECTRON PARAMETERS
C
      DO 10 I = 1,NWF
      R1 = QUADR(I,I,-1)
      RM = QUADR(I,I,1)
      RMM = QUADR(I,I,2)
      EK = -D5*HL(EL,I,I,REL)
      EKINP = EK + Z*R1
      EN = EN+ SUM(I)*EKINP
      RH = 3*N(I)*N(I) - L(I)*(L(I) + 1)
      SC = Z - D5*RH/RM
      S(I) = SC
      R3(I) = D0
      IF (L(I) .NE. 0) R3(I) = QUADR(I,I,-3)
      RELS = ASHIFT(I,I)
      WRITE(PRI,15)EL(I),E(I,I),AZ(I),SC,R3(I),R1,RM,RMM,EK,EKINP,RELS
15    FORMAT( 4X,A3,F14.7,F15.7,F8.3,F13.4,F11.5,F10.5,F11.6,3F13.6)
10    CONTINUE
C
C  *****  ADD CONTRIBUTION FROM THE 'L' INTEGRALS
C
      IBEGIN = INTPTR(5) + 1
      IEND = INTPTR(6)
      DO 32 I = IBEGIN,IEND
      IF (IEL(I,1) .NE. IEL(I,2))
     :    CONT = HL(EL,IEL(I,1),IEL(I,2),REL)
     :           -D2*Z*QUADR(IEL(I,1),IEL(I,2),-1)
32    EN = EN + CONT*COEF(I)
      EPOTL = ETOTAL - EN
      RATIO =-EPOTL/EN
      WRITE(PRI,26) ETOTAL,EN,EPOTL,RATIO
26    FORMAT(/3X,15HTOTAL ENERGY = ,F14.7,2X,17HKINETIC ENERGY = ,
     1   F14.7,2X,23HTOTAL-KINETIC ENERGY = ,F14.7,3X,8HRATIO = ,F11.9)
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
17       WRITE(OUD,19) SYMBOL,KVAL(I),EL(IEL(I,1)),EL(IEL(I,2)),VALUE(I)
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
      SUBROUTINE DIAG(ECONV,ACFG,CFGTOL,LAST)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
        COMMON /MATRIX/ETOTAL,W(500,500)
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
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
      COMMON WP(500),ET(500,500)
*
        LOGICAL ECONV,LAST
*
        DO 1 I = 1,NCFG
           DO 2 J = 1,NCFG
              ET(I,J) = D0
  2        CONTINUE
           WP(I) = WT(I)
           ET(I,I) = EC
  1     CONTINUE
C
        IBEGIN = 1
        IEND = INTPTR(6)
        J = 0
        DO 10 I = IBEGIN,IEND
 11        IF (CPTR(I) .GT. J) THEN
              J = J + 1
              C = COEFF(J)*VALUE(I)
              IF (OPTR(J) .NE. 0)  THEN
                  C = C*VALUE(OPTR(J))
                  ENDIF
              ET(IH(J),JH(J)) = ET(IH(J),JH(J)) + C
              GO TO 11
           END IF
 10     CONTINUE
C
C  ***** SYMMETRIZE THE MATRIX
C
        DO 12 I = 1,NCFG-1
           DO 13 J = I+1,NCFG
              ET(I,J) = ET(J,I)
 13        CONTINUE
 12     CONTINUE
14    IF (NCFG .EQ. 1) GO TO 37
      IF (ID .GT. 0) GO TO 38
C
C  *****  COMPUTE EIGENVALUE AND EIGENVECTOR BY THE METHOD OF
C  *****  SEC. 6-8.2  THIS METHOD MAY CAUSE DIFFICULTIES WHEN NEAR
C  *****  DEGENERACY EFFECTS ARE PRESENT SINCE IT MAY CONVERGE TO
C  *****  THE WRONG EIGENVALUE.  THE CODE UP TO, BUT NOT INCLUDING
C  *****  STATEMENT NUMBER 31, MAY BE REPLACED BY A CALL TO A MORE
C  *****  REFINED LIBRARY SUBROUTINE.
C
      ETPREV = D0
      DO 30 II=1,5
      ETL = D0
C
C  *****  DETERMINE ESTIMATE OF THE EIGENVALUE
C
      DO 16 I=1,NCFG
      CONT = D0
      DO 17 J=1,NCFG
17    CONT = CONT + WT(J)*ET(I,J)
16    ETL = ETL + WT(I)*CONT
C
C  *****  SOLVE SYSTEM OF EQUATIONS FOR EIGENVECTOR
C
      DO 18 I=1,NCFG
      DO 19 J=1,NCFG
19    W(I,J) = ET(I,J)
18    W(I,I) = W(I,I) - ETL
      WT(1) = D1
      IF (NCFG .NE. 2) GO TO 54
      WT(2) = -W(2,1)/W(2,2)
      GO TO 20
C
54    CALL SYMMEQ((500),NCFG,W,WT)
C
20    DO 26 I=2,NCFG
26    WT(1) = WT(1) + WT(I)**2
      WT(1) = D1/DSQRT(WT(1))
      DO 27 I=2,NCFG
27    WT(I) = WT(I)*WT(1)
C
C  *****  ITERATE, IF NECESSARY, OTHERWISE OUTPUT RESULTS
C
      IF (DABS((ETPREV-ETL)/ETL) .LT. 1.D-7) GO TO 31
30    ETPREV = ETL
      WRITE(OUT,40)
40    FORMAT(///10X,47HMATRIX DIAGONALIZATION PROCEDURE NOT CONVERGING )
      DELTAE = D0
      GO TO 33
31    DELTAE = ETL -ETOTAL
33    ETOTAL = ETL
      IF ( LAST ) WRITE(PRI,32) ETOTAL
      WRITE(OUT,32) ETOTAL
32    FORMAT(//10X,15HTOTAL ENERGY = ,F18.9 )
      IF ( LAST ) WRITE(PRI,34)
39    WRITE(OUT,34)
34    FORMAT(/6X,6HWEIGHT,6X,13HENERGY MATRIX )
      CC = D0
      DO 28 I = 1,NCFG
      WT(I) = WT(I) + ACFG*(WP(I) - WT(I))
28    CC = CC + WT(I)**2
      CC = D1/DSQRT(CC)
      DO 35 I=1,NCFG
35    WT(I) = WT(I)*CC
      DO 45 I = 1,NCFG
        IF ( LAST ) WRITE(PRI,36) I,WT(I),(ET(J,I),J=1,I)
        IF (I .LE. 7) WRITE(OUT,636) I,WT(I),(ET(J,I),J=1,I)
45    CONTINUE
36    FORMAT(I10,F12.8,2X,7F15.7/(24X,7F15.7))
636   FORMAT(I4,F9.5,1X,5F13.5/(14X,5F13.5))
C
C  *****  REDEFINE SUM(I)
C
        IBEGIN = INTPTR(5)+1
        IEND = INTPTR(6)
        DO 50 I = IBEGIN,IEND
           IF (IEL(I,1).EQ.IEL(I,2)) SUM(IEL(I,1)) = -2*COEF(I)
 50     CONTINUE
70    IF (.NOT. LAST .OR. OUC .EQ. 0 ) GO TO 49
C
C  *****  PUNCH CONFIGURATIONS AND WEIGHTS ON UNIT OUC
C
C
      WRITE(OUC,46) ATOM,TERM,ETOTAL
46    FORMAT(3X,2A6,F14.7)
      WRITE(OUC,'(18(1X,A3))') (EL(J),J=1,NCLOSD)
      DO 47 J = 1,NCFG
47    WRITE(OUC,48) CONFIG(J),WT(J),(COUPLE(J,JJ),JJ=1,9)
48    FORMAT(A40,F10.7/9(5X,A3))
      WRITE (OUC,'(A)') '*'
      IF ( OUH .EQ. 0 ) GO TO 49
      WRITE (OUH,'(2X,A6,I6)') 'NCFG =',NCFG
      DO 60 I = 1,NCFG
60    WRITE (OUH,61) (ET(I,J),J=1,I)
61    FORMAT(5F14.7)
49    ECONV = .FALSE.
      IF (DABS(DELTAE/ETOTAL) .LE. CFGTOL) ECONV = .TRUE.
      RETURN
37    ETOTAL = ET(1,1)
      DELTAE = D0
      WRITE(OUT,32) ETOTAL
      GO TO 70
38    DELTAE = D0
      ETOTAL = ET(1,1)
      GO TO 39
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
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON W(500,500),U(500),DWT(500),AC(20,20),BC(20),JV(20),IV(20)
        LOGICAL DIAG, SETEQL, FIRST
*
*       CLEAR THE ARRAY FOR CHANGING THE WT ARRAY

*
        DO 3 I = 1,NCFG
           DWT(I) = D0
 3      CONTINUE
C
C  *****  ROTATE PAIRS CONNECTED BY ORTHOGONALITY BUT NOT WHEN ONE OF
C         THE ORBITALS IS SIMULTANEOUSLY ORTHOGONAL TO A NON-ORTHOGONAL
C         PAIR
C
        DO 1 I = IB,NWF-1
           DO 2 J = I+1,NWF
              IF (DABS(E(I,J)) .GT. 1.D-10 .AND. SETEQL(I,J))
     :            CALL EPSILON(I,J)
2          CONTINUE
1       CONTINUE
        CALL ROTATE
*
*       Adjust the WT array, renormalize, and recompute SUM(i)
*
        C = D0
        DO 4 I = 1,NCFG
           WT(I) = WT(I) + DWT(I)
           C = C + WT(I)**2
 4      CONTINUE
        C = SQRT(C)
        DO 5 I = 1,NCFG
           WT(I) = WT(I)/C
 5      CONTINUE
*
        IBEGIN = INTPTR(5)+1
        IEND = INTPTR(6)
        DO 6 I = IBEGIN,IEND
           IF (IEL(I,1).EQ.IEL(I,2)) SUM(IEL(I,1)) = -2*COEF(I)
 6      CONTINUE
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
11         CONTINUE
C
C  ***** IJ IS THE NUMBER OF LAGRANGE MULTIPLIERS FOR l = IL
C
           IF (IJ .EQ. 0) GO TO 10
           DO 13 II = 1,IJ
              BC(II) = D0
              DO 14 III = 1,IJ
                 AC(II,III) = D0
14            CONTINUE
13         CONTINUE
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
                          CALL XCH(I,2)
                          CALL POTL(I)
                          DO 20 JJ = 1,NO
                             YK(JJ) = YR(JJ)
20                        CONTINUE
                          FIRST = .FALSE.
                       END IF
                       DO 22 JJ = 1,NO
                          YR(JJ) = P(JJ,J)
22                     CONTINUE
                       BC(II) = BC(II) +
     :                    HL(EL,I,J,REL)-D2*QUADS(I,J,1)-QUAD(J,NO,YR,X)
                    END IF
18              CONTINUE
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
26            CONTINUE
24         CONTINUE
           IF ( .NOT. DIAG ) CALL LINEQN(20,IJ,AC,BC)
           DO 28 II = 1,IJ
              CALL EIJSET(IV(II),JV(II),BC(II)/SUM(IV(II)))
              IF ( JV(II) .GE. IB )
     :            CALL EIJSET(JV(II),IV(II),BC(II)/SUM(JV(II)))
28         CONTINUE
10    CONTINUE
C
C  *****  PRINT THE OFF-DIAGONAL ENERGY PARAMETERS
C
        DO 30 I = IB,NWF
           DO 32 J = 1,I-1
              IF (DABS(E(I,J)) .GT. 1.D-10) THEN
                 WRITE(OUT,35) EL(I),EL(J),E(I,J),EL(J),EL(I),E(J,I)
35               FORMAT(7X,2(3X,'E(',2A3,') =',F12.5))
              END IF
32         CONTINUE
30      CONTINUE
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
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
 60      JV = IJE(IP)
         IF (JV .LT. I .AND. IP .LE. IEPTR(I)) THEN
            IJ = IJ+1
            IF ( IJ .GT. (20)) STOP ' TOO MANY ORTHOGONALITY CONDITIONS'
            BC(IJ) = QUADR(I,JV,0)
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
            IF (DABS(C) .GT. 1.D-10) THEN
               WRITE(OUT,63) EL(IJE(IP)),EL(I),C
63             FORMAT(6X,'<',A3,'|',A3,'>=',1PD8.1)
               M = MAX0(M,MAX(IJE(IP)))
               DO 64 J = 1,M
                  P(J,I) = P(J,I) - C*P(J,IJE(IP))
64             CONTINUE
               AZZ = AZZ - C*AZ(IJE(IP))
            END IF
            IP = IP + 1
            CTOTAL = CTOTAL + ABS(C)
65       CONTINUE
         IF (CTOTAL .GT. 1.D-10) THEN
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
            MAX(I) = M
            AZ(I) = AZZ
            VARIED(I) = .TRUE.
         END IF
      END IF
2     CONTINUE
      END
C
C     ------------------------------------------------------------------
C    3-30      R P E R T
C     -----------------------------------------------------------------
C
C       RPERT determines the effect of a perturbation in the form of a
C   rotation of the i'th and j'th orbital both on the energy and on the
C   stationary condition, due to the presence of a given RK integral.
C
C
      SUBROUTINE RPERT(IIN,JIN,II,INT,DC,DV)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
        PARAMETER (IDIM=20000,NCDIM=50000)
        COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
      DIMENSION IND(4)
      EQUIVALENCE (I1,IND(1)),(I2,IND(2)),(I3,IND(3)),(I4,IND(4))
*
        I = JIN
        J = IIN
        I1 = IEL(II,1)
        I2 = IEL(II,2)
        I3 = IEL(II,3)
        I4 = IEL(II,4)
        KK = KVAL(II)
        INC = 1
        IF (I1.EQ.I2 .AND. I3.EQ.I4) INC = 2
1     DO 10 KP = 1,2
      DI = D0
      DII = D0
      DIJ = D0
      DO 2 K = 1,4,INC
      IF (IND(K) .NE. I) GO TO 2
      IND(K) = J
      DI = DI + RK(I1,I2,I3,I4,KK,REL)
      IF (.NOT. ALL) GO TO 3
      DO 4 K2 = 1,4
      IF (IND(K2) .NE. J) GO TO 5
      IND(K2) = I
      DIJ = DIJ + RK(I1,I2,I3,I4,KK,REL)
      IND(K2) = J
5     IF (IND(K2) .NE. I) GO TO 4
      IND(K2) = J
      DII = DII + RK(I1,I2,I3,I4,KK,REL)
      IND(K2) = I
4     CONTINUE
3     IND(K) = I
2     CONTINUE
6     IF (KP .EQ. 2) THEN
          DC = INC*(DI - DJ)
          DV = INC*(DII + DJJ - DIJ - DJI)
          RETURN
      ELSE
         DJ = DI
         DJJ = DII
         DJI = DIJ
         I = IIN
         J = JIN
      END IF
10    CONTINUE
      END
      SUBROUTINE O2PERT(IIN,JIN,II,INT,DC,DV)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
        PARAMETER (IDIM=20000,NCDIM=50000)
        COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
      DIMENSION IND(6),KV(3),OV(3)
      EQUIVALENCE (I1,IND(1)),(I2,IND(2)),(I3,IND(3)),(I4,IND(4)),
     :            (I5,IND(5)),(I6,IND(6))
*
        I = JIN
        J = IIN
        I1 = IEL(II,1)
        I2 = IEL(II,2)
        I3 = IEL(II,3)
        I4 = IEL(II,4)
        KK = KVAL(II)
        KV(1) = KK/64
        KV(2) = KK - 64*KV(1)
        OV(1) = QUADR(I1,I2,0)
        OV(2) = QUADR(I3,I4,0)
        OVL = OV(1)**KV(1)*OV(2)**KV(2)
        IF (ABS(OV(1)).LE.1.E-8 .OR. ABS(OV(2)).LE.1.E-8) THEN
           WRITE(6,20)
     :             'Overlap Integrals too small for this code:',
     :             'Electrons',I1,I2,' have overlap =',OV(1),
     :             'Electrons',I3,I4,' have overlap =',OV(2)
 20        FORMAT(/A/2(1X,A,2I3,A,F/))
           STOP
        END IF
1     DO 10 KP = 1,2
      DI = D0
      DII = D0
      DIJ = D0
      DO 2 K = 1,4
      IF (IND(K) .NE. I) GO TO 2
        KI = (K-1)/2 + 1
        I5 = J
        I6 = IND(K+1 - 2*MOD(K+1,2))
        OV(3) = QUADR(I5,I6,0)
        KV(3) = 1
        CK = KV(KI)
        TI = CK*OVL/OV(KI)*OV(3)
        IF (.NOT. ALL) GO TO 3
        KV(KI) = KV(KI)-1
      DO 4 K2 = 1,6
      IF (IND(K2) .NE. J) GO TO 5
        KIJ = (K2-1)/2 + 1
        IF (KV(KIJ) .EQ. 0) GO TO 5
        IJPAIR = K2+1 - 2*MOD(K2+1,2)
        CKIJ = KV(KIJ)
        DIJ = DIJ + CKIJ*TI/OV(KIJ)*QUADR(I,IND(IJPAIR),0)
5     IF (IND(K2) .NE. I) GO TO 4
        KII = (K2-1)/2 + 1
        IF (KV(KII) .EQ. 0) GO TO 4
        IIPAIR = K2+1 - 2*MOD(K2+1,2)
        CKII = KV(KII)
        DII = DII + CKII*TI/OV(KII)*QUADR(J,IND(IIPAIR),0)
4     CONTINUE
      KV(KI) = KV(KI) + 1
3     DI = DI + TI
2     CONTINUE
6     IF (KP .EQ. 2) THEN
          DC = DI - DJ
          DV = DII + DJJ - DIJ - DJI
          RETURN
      ELSE
         DJ = DI
         DJJ = DII
         DJI = DIJ
         I = IIN
         J = JIN
      END IF
10    CONTINUE
      END
        SUBROUTINE EPSILON(IIN,JIN)
        IMPLICIT REAL*8(A-H,O-Z)
*
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
*
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
        COMMON /MATRIX/ETOTAL,W(500,500)
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
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON U(500),DWT(500)
*
        LOGICAL ICLOSD,FOUND
        IF (IIN .LT. JIN) THEN
           I = IIN
           J = JIN
        ELSE
           I = JIN
           J = IIN
        END IF
        SUMI = SUM(I)
        SUMJ = SUM(J)
        LI = L(I)
        LJ = L(J)
        FULL = 4*LI+2
        IF (ABS(SUMI-FULL).LT.1.D-8 .AND. ABS(SUMJ-FULL).LT.1.D-8) THEN
           CALL EIJSET(I,J,1.D-10)
           CALL EIJSET(J,I,1.D-10)
           RETURN
        END IF
*
        VPR = VX(I,J)
        DVPR = VX(J,I)
        IF (VPR .NE. D0 .AND. DABS(VPR) .LT. 1.D-4) RETURN
        DO 2 II = 1,NCFG
           U(II) = D0
2       CONTINUE
        EPS = 1.D-5*(SUMI*SUMJ)**0.5
        ALL = .TRUE.
        IF (VPR .NE. D0) THEN
           IF (ABS(DVPR/VPR) .LT. 0.02) ALL = .FALSE.
        END IF
        G = D0
        DG = D0
        DIFF = SUMI - SUMJ
        IF (I .LE. NCLOSD) THEN
            ICLOSD = .TRUE.
            HLIJ = HL(EL,I,J,REL)
            IF (ABS(DIFF) .GT. EPS .AND. ALL)
     :          DG=DIFF*(HL(EL,I,I,REL)-HL(EL,J,J,REL))
            VI =  -SUMI*HLIJ
            VJ =  HLIJ
            TI = D0
            TJ = D0
            TT = D0
            VV = D0
            DO 10 K = 0,2*LI,2
                IF (K .EQ. 0) THEN
                   CIJ = (D1 - CB(LI,LJ,0))
                   CII = D1
                ELSE
                   CIJ = -CB(LI,LJ,K)
                   CII = -CA(LI,K)
                END IF
                RKIJJJ = RK(I,J,J,J,K,REL)
                RKJIII = RK(J,I,I,I,K,REL)
                TI = TI + CIJ*RKIJJJ
                TT = TT + (SUMI-D1)*CII*RKJIII
                TJ = TJ + CIJ*RKJIII
                IF (ALL) THEN
                   FKIJ = FK(I,J,K,REL)
                   GKIJ = GK(I,J,K,REL)
                   FKII = FK(I,I,K,REL)
                   FKJJ = FK(J,J,K,REL)
                   VV = VV + SUMJ*CIJ*(FKJJ + FKII - 2*FKIJ - 4*GKIJ)
     :                   +(SUMI-D1)*CII*(FKIJ + 2*GKIJ - FKII)
                END IF
 10        CONTINUE
           DG = DG + D2*SUMI*VV
           VI = VI + D2*SUMI*(TT + SUMJ*TI)
           VJ = VJ - D2*SUMI*TJ
           VV = D0
           DO 20 IA = 1,NCLOSD
                IF (IA .EQ. I) GO TO 20
                T = RK(I,IA,J,IA,0,REL)
                IF (ABS(DIFF).GT.EPS.AND.ALL)
     :              VV = FK(J,IA,0,REL)-FK(I,IA,0,REL)
                DO 22 K = IABS(LI-L(IA)),LI+L(IA),2
                   CIJ = CB(LI,L(IA),K)
                   T = T - CIJ*RK(I,IA,IA,J,K,REL)
                   IF (ABS(DIFF).GT.EPS .AND. ALL)
     :                   VV = VV - CIJ*(GK(J,IA,K,REL)-GK(I,IA,K,REL))
 22             CONTINUE
                DG = DG + D2*DIFF*SUM(IA)*VV
                VI = VI + D2*SUMI*SUM(IA)*T
                VJ = VJ - D2*SUM(IA)*T
 20        CONTINUE
           G = G + VI
           VJ = -D2*VJ
           DO 24 II = 1,NCFG
                U(II) = U(II) + WT(II)*VI
 24        CONTINUE
        ELSE
*
*     ... Both orbitals are outer orbitals
*
           ICLOSD = .FALSE.
           HLCIJ = HLC(EL,I,J,REL)
           IF (ABS(DIFF) .GT. EPS .AND. ALL)
     :        DG=DIFF*(HLC(EL,I,I,REL)-HLC(EL,J,J,REL))
           VI =   D2*HLCIJ
           VJ =  -D2*HLCIJ
        END IF
*
*
*       ... Add contributions from integrals between outer electrons
*
        IEND = 0
        DO 40 INT = 1,6
           IBEGIN = IEND + 1
           IEND = INTPTR(INT)
           DO 30 II = IBEGIN,IEND
                FOUND = .FALSE.
                I1 = IEL(II,1)
                I2 = IEL(II,2)
                IR = (I1-I)*(I2-I)
                JR = (I1-J)*(I2-J)
                IF (IR .EQ. 0 .OR. JR .EQ. 0 .OR.
     :             (INT.EQ.6 .AND. I1.EQ.I2 .AND. ICLOSD)) THEN
*               ... We have found a contribution ...
                   FOUND = .TRUE.
                   IF (INT.EQ.5) THEN
                        CALL RPERT(I,J,II,INT,DC,DV)
                   ELSE IF (INT.EQ.4) THEN
                        CALL O2PERT(I,J,II,INT,DC,DV)
                   ELSE IF (INT.EQ.6 .AND. (IR.EQ.0 .OR. JR.EQ.0)
     :                  .AND. I1.EQ.I2) THEN
                      IF (I1.EQ.I) THEN
                          DC = VI
                          DV = D0
                      ELSE IF (I1.EQ.J) THEN
                          DC = VJ
                          DV = D0
                      END IF
                   ELSE
                        CALL PERT(I,J,II,INT,DC,DV)
                   END IF
                ELSE IF (INT.EQ.5 .OR. INT.EQ.4) THEN
*                  ... Continue to search for a contribution
                   I1 = IEL(II,3)
                   I2 = IEL(II,4)
                   IR = (I1-I)*(I2-I)
                   JR = (I1-J)*(I2-J)
                   IF (IR .EQ. 0 .OR. JR .EQ. 0) THEN
*                  ... Another contribution has been found
                      FOUND = .TRUE.
                      IF (INT.EQ.5) CALL RPERT(I,J,II,INT,DC,DV)
                      IF (INT.EQ.4) CALL O2PERT(I,J,II,INT,DC,DV)
                   END IF
                END IF
                IF (FOUND) THEN
                   IF (INT .NE. 3 .AND. INT .NE. 4) THEN
                      CALL CONTC(II,DC,U,C)
                   ELSE
                      CALL CONTOV(II,DC,U,C)
                   END IF
                   G = G + C*DC
                   DG = DG + C*DV
                END IF
 30        CONTINUE
 40     CONTINUE
*
        IF (.NOT. ALL) THEN
           DG = VPR
           GO TO 38
        END IF
        DO 32 II = 1,NCFG
           U(II) = U(II) - G*WT(II)
           W(II,1) = U(II)
 32     CONTINUE
        W(1,1) = D0
*
C  *****  SOLVE SYSTEM OF EQUATIONS FOR PERTURBATIONS TO THE EIGENVECTOR
C

      IF (NCFG .EQ. 1) GO TO 38
      IF (NCFG .EQ. 2) THEN
         W(2,1) = -W(2,1)/W(2,2)
      ELSE
           CALL SYMMSL((500),NCFG,W,W(1,1))
      END IF
      C = D0
      DO 36 JJ = 1,NCFG
         C = C + WT(JJ)*W(JJ,1)
 36   CONTINUE
      DO 37 JJ = 1,NCFG
         W(JJ,1) = W(JJ,1) - C*WT(JJ)
         DG = DG + D2*W(JJ,1)*U(JJ)
 37   CONTINUE
 38   IF (DABS(G)+DABS(DG) .GT. 4.D-5*((SUMI*SUMJ)**0.25)
     :         .OR. DABS(E(I,J)) .GT. 2.D-5) THEN
         EPS = -G/DG
         IF (DABS(DG) .LT. 1.D-4) THEN
            EPS = D0
         ELSE
            EPS = DSIGN(DMIN1(DABS(EPS),0.2D0),EPS)
         END IF
         IF (ALL .AND. E(IIN,JIN).NE.1.D-5) THEN
            DO 42 JJ = 1,NCFG
                DWT(JJ) = DWT(JJ) + EPS*W(JJ,1)
  42        CONTINUE
         END IF
         CALL EIJSET(JIN,IIN,EPS)
         WRITE(OUT,100) EL(I),EL(J),G,EL(I),EL(J),DG,EPS
100      FORMAT(10X,'C(',2A3,') =',F12.5,3X,'V(',2A3,') =',
     :                  F12.5,3X,'EPS =',F9.6)
       ELSE
C
C  *****  THE ENERGY IS STATIONARY WITH RESPECT TO ROTATIONS
C
        CALL EIJSET(I,J,1.D-10)
        CALL EIJSET(J,I,1.D-10)
      END IF
        IF (ALL) THEN
           CALL VIJSET(I,J,DG)
           DDG = DG - VPR
           CALL VIJSET(J,I,DDG)
        END IF
      END
        SUBROUTINE CONTC(INT,DC,U,COEF)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION U(*)
*
        INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
        PARAMETER (IDIM=20000,NCDIM=50000)
        COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        COEF = 0.D0
        IBEGIN = 1
        IF (INT .GT. 1) IBEGIN = CPTR(INT-1)+1
        IEND = CPTR(INT)
        DO 1 II = IBEGIN,IEND
           T = COEFF(II)
           IF (OPTR(II).NE.0) T = T*VALUE(OPTR(II))
           U(IH(II)) = U(IH(II)) + WT(JH(II))*T*DC
           IF (IH(II) .NE. JH(II)) THEN
                U(JH(II)) = U(JH(II)) + WT(IH(II))*T*DC
                T = T+T
           END IF
           COEF = COEF+T*WT(IH(II))*WT(JH(II))
  1     CONTINUE
        END
        SUBROUTINE CONTOV(M,DC,U,COV)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION U(*)
*
        INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
        PARAMETER (IDIM=20000,NCDIM=50000)
        COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        COV = 0.D0
        IBEGIN = INTPTR(4)+1
        IEND =INTPTR(6)
        DO 10 I = IBEGIN,IEND
           JBEGIN = CPTR(I-1)+1
           JEND = CPTR(I)
           DO 12 J = JBEGIN,JEND
              IF ( OPTR(J) .EQ. M) THEN
                 CC =  COEFF(J)*VALUE(I)
                 U(IH(J)) = U(IH(J)) + WT(JH(J))*CC*DC
                 IF (IH(J) .NE. JH(J)) THEN
                    U(JH(J)) = U(JH(J)) + WT(IH(J))*CC*DC
                    CC = 2*CC
                 END IF
                 COV = COV + CC*WT(IH(J))*WT(JH(J))
              END IF
 12        CONTINUE
 10     CONTINUE
        END
        SUBROUTINE PERT(IIN,JIN,II,INT,DC,DV)
        IMPLICIT REAL*8(A-H,O-Z)
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
     :         ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        I = IIN
        J = JIN
        DC = D0
        DV = D0
        I1 = IEL(II,1)
        I2 = IEL(II,2)
        K = KVAL(II)
        IF (I1 .GT. I2) THEN
           IT = I1
           I1 = I2
           I2 = IT
        END IF
        IF (I1.EQ.I .AND. I2.EQ.J) THEN
           IF (INT .LE. 2) THEN
                DC = 2*(RK(I,J,J,J,K,REL)-RK(J,I,I,I,K,REL))
                IF (ALL)   DV = 2*(FK(J,J,K,REL)+FK(I,I,K,REL)
     :               -2*FK(I,J,K,REL)-4*GK(I,J,K,REL))
           ELSE
                DC = HLC(EL,J,J,REL)-HLC(EL,I,I,REL)
                IF (ALL) DV = -4*HLC(EL,I,J,REL)
           END IF
        ELSE IF (I1.EQ.I2 .AND. INT.EQ.6) THEN
*          ... i is a closed subshell
           LI = L(I)
           DC = RK(I,I1,J,I1,0,REL)
           IF (ALL) DV = FK(J,I1,0,REL) - FK(I,I1,0,REL)
           DO 18 K = IABS(LI-L(I1)),LI+L(I1),2
              CIJ = CB(LI,L(I1),K)
              DC = DC - CIJ*RK(I,I1,I1,J,K,REL)
              IF (ALL) DV = DV - CIJ*(GK(J,I1,K,REL) - GK(I,I1,K,REL))
 18        CONTINUE
           DC = -D4*SUM(I)*DC
           IF (ALL) DV = -D4*SUM(I)*DV
        ELSE
           PHASE = D1
           DO 10 KK = 1,2
             IF (I1.EQ.I .OR. I2.EQ.I) THEN
                IF (I1.NE.I) THEN
                   IT = I1
                   I1 = I2
                   I2 = IT
                END IF
                IF (I1.EQ.I2) THEN
                   DC = 4*PHASE*RK(J,I,I,I,K,REL)
                   IF (ALL) DV = 4*(FK(J,I,K,REL)
     :                           -FK(I,I,K,REL)+2*GK(I,J,K,REL))
                ELSE IF (INT.EQ.1) THEN
                   DC = 2*PHASE*RK(I,I2,J,I2,K,REL)
                   IF (ALL) DV = 2*(FK(J,I2,K,REL)-FK(I,I2,K,REL))
                ELSE IF (INT.EQ.2) THEN
                   DC = 2*PHASE*RK(I,I2,I2,J,K,REL)
                   IF (ALL) DV = 2*(GK(J,I2,K,REL)-GK(I,I2,K,REL))
                ELSE IF (INT.EQ.6) THEN
                   DC = PHASE*HLC(EL,J,I2,REL)
                   IF (ALL) DV = -HLC(EL,I,I2,REL)
                ELSE
                   OV1 = QUADR(J,I2,0)
                   OV2 = QUADR(I,I2,0)
                   DC = K*PHASE*OV1*OV2**(K-1)
                   IF (ALL) THEN
                      DV = -K*OV2**K
                      IF (K.GT.1) DV = DV+K*(K-1)*OV2**(K-2)*OV1**2
                   END IF
                END IF
             END IF
             I = JIN
             J = IIN
             PHASE = -D1
 10       CONTINUE
        END IF
        END
       DOUBLE PRECISION FUNCTION VX(I,J)
       IMPLICIT REAL (A-H,O-Z)
       COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30)
     : ,ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
       IBEGIN=1
       IF (I .GT. 1) IBEGIN=IEPTR(I-1)+1
       IEND=IEPTR(I)
       VX=0.D0
       DO  10 II=IBEGIN,IEND
       IF (IJE(II) .EQ. J) THEN 
         VX=VIJ(II)
         RETURN
        ENDIF
10     CONTINUE
       END
       SUBROUTINE VIJSET(I,J,VX)
        IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        IBEGIN = 1
        IF (I .GT. 1) IBEGIN = IEPTR(I-1)+1
        IEND = IEPTR(I)
        DO 10 II = IBEGIN,IEND
           IF (IJE(II) .EQ. J) THEN
              VIJ(II) = VX
              RETURN
           END IF
 10     CONTINUE
        END
        SUBROUTINE ROTATE
        IMPLICIT REAL*8(A-H,O-Z)
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        LOGICAL SETEQL
C
C  *****  ROTATE PAIRS CONNECTED BY ORTHOGONALITY BUT NOT WHEN ONE OF
C         THE ORBITALS IS SIMULTANEOUSLY ORTHOGONAL TO A NON-ORTHOGONAL
C         PAIR
C
        DO 1 I = IB,NWF-1
           DO 2 J = I+1,NWF
              IF (DABS(E(I,J)) .GT. 1.D-10 .AND. SETEQL(I,J)
     :            .AND. DABS(VX(I,J)) .GT. 1.D-4) THEN
                 EPS = E(J,I)
                 DD = DSQRT(D1 + EPS*EPS)
                 DO 41 JJ = 1,NO
                    PI = (P(JJ,I) + EPS*P(JJ,J))/DD
                    P(JJ,J) = (P(JJ,J) - EPS*P(JJ,I))/DD
41               P(JJ,I) = PI
                 VARIED(I) = .TRUE.
                 VARIED(J) = .TRUE.
              END IF
2          CONTINUE
1       CONTINUE
        END
