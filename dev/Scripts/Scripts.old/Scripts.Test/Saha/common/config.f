*
*     ------------------------------------------------------------------
*	A N A L Y S E 1
*     ------------------------------------------------------------------
*
      SUBROUTINE ANALY1(IREAD,IWRITE,NCLOSD,MAXORB,N,NCFG,NOCCSH,LIST,
     :                   NDIM)
*
*        This routine analyzes the format of the configuration input
*        data and determines a consistent ordering of the electrons
*
      INTEGER NOCCSH(NDIM),AFTER(30,30),IEL(5)
      CHARACTER LIST(NDIM)*3, LINE*72, OF(30)*3, EL(5)*3
*
  1   FORMAT(A72)
*
      DO 2 I = 1,(30)
         DO 3 J = 1,(30)
            AFTER(I,J) = 0
  3      CONTINUE
  2   CONTINUE
*
*  ---  Determine the number of common closed subshells
*
      READ(IREAD,'(/A72)' ) LINE
      NCLOSD = 0
      J = 2
 10   IF (LINE(J:J+2) .NE. '   ' ) THEN
         NCLOSD = NCLOSD + 1
         J = J+4
         IF (J .LT. 72) GO TO 10
      END IF
*
*  ---  Determine the number or configurations and electrons
*
      MAXORB = 0
      NCFG = N
 20   READ(IREAD,1,END=55) LINE
      IF (LINE(1:1) .NE. '*'  .AND. LINE(2:2) .NE. '*' ) THEN
*
*  ------  A new configuration has been read; find the electrons
*
         NCFG = NCFG + 1
         IF (NCFG .GT. NDIM )
     :	    WRITE(IWRITE,'(A,I5)') ' TOO MANY CONFIGURATIONS: MAX=',NDIM
         J = 2
         I = 0
 30      IF (LINE(J:J+2) .NE. '   ' .AND. I.LT.(5)) THEN
*
*  --------- An electron has been found; is it a new one?
*
            I = I+1
            EL(I) = LINE(J:J+2)
            K = 1
 40         IF (K .LE. MAXORB) THEN
               IF ( OF(K) .NE. EL(I) ) THEN
                  K = K+1
                  IF (K .GT. (30)) STOP ' TOO MANY ELECTRONS: MAX= (30)'
                  GO TO 40
                 ELSE
                  IEL(I) = K
               END IF
              ELSE
*
*  ------------  A new electron has been found; add it to the list
*
               MAXORB = K
               OF(MAXORB) = EL(I)
               IEL(I) = K
            END IF
            J = J+8
            GO TO 30
         END IF
         NOCCSH(NCFG) = I
*
*  ------  Add data to the AFTER matrix
*
         DO 50 I1 = 2,I
            DO 51 I2 = 1,I1-1
               AFTER(IEL(I1),IEL(I2)) = 1
 51         CONTINUE
 50      CONTINUE
         READ(IREAD,*)
         IF (I .GT. 5) READ(IREAD,*)
         GO TO 20
      END IF
*
*  ---  Check if the ordering of the electrons is inconsistent
*
 55   DO 60 I = 1,MAXORB
         DO 61 J = 1,MAXORB
            IF (AFTER(I,J) .EQ. 1 .AND. AFTER(J,I) .EQ. 1) THEN
                WRITE(IWRITE,*) ' The order of ',OF(I),' and ',
     :                OF(J),' is inconsistent'
                STOP
            END IF
 61      CONTINUE
 60   CONTINUE
*
*  ---  Reorder the electrons to satisfy the after relations found
*         in the different configurations
*
      IORD = 1
 70   IF (IORD .LE. MAXORB ) THEN
*
*  ------  Search for a row with no 1's
*
         DO 71 I = 1,MAXORB
            DO 72 J = 1,MAXORB
               IF (AFTER(I,J) .EQ. 1 ) GO TO 71
 72         CONTINUE
*
*  ---------  The current row contains all 0's or 2's
*
            IF (AFTER(I,I) .NE. 2 ) THEN
*
*  ------------  We have the next electron; delete the corresponding
*                  rows and columns from the AFTER matrix
*
               LIST(IORD) = OF(I)
               IORD = IORD+1
               DO 73 J = 1,MAXORB
                  AFTER(I,J) = 2
                  AFTER(J,I) = 2
 73            CONTINUE
               GO TO 70
            END IF
 71      CONTINUE
      END IF
      RETURN
      END
*
*     ------------------------------------------------------------------
*	A N A L Y S 2
*     ------------------------------------------------------------------
*
      SUBROUTINE ANALY2(NCLOSI,NCLOSF,MCFG,KCFG,LIST,LORTH)
*
*        This routine analyzes the format of the configuration input
*        data, for two sets, not necessarily orthogonal.
*
      INTEGER AFTER(90,90),IEL(5),NORB(2),NCLOS(2),ICFG(2)
      CHARACTER*3 LIST(*), LINE*72, OF(30,2), ELC(30), EL(5), FIND
      CHARACTER*7 LABEL(2)
      CHARACTER*6 ANS
      LOGICAL LORTH
      COMMON/INFORM/ IREADI,IWRITE,IOUT,IREADF,ISC(6)
      COMMON/STATES/NCFG,MAXORB,IAJCMP(71),LJCOMP(71),
     :NJCOMP(71),NOCCSH(800),NELCSH(5,800),NOCORB(5,800),J1QNRD(9,3,800)
      COMMON/NOR/NCOM,NORBI,NORBF,IWAR
      DATA LABEL(1), LABEL(2) / 'Initial', 'Final ' /
*
  1   FORMAT(A72)
    4 FORMAT(/10H THERE ARE,I3,' INITIAL STATE ORBITALS AS FOLLOWS: '/
     :      (1X,18(1X,A3)))
    5 FORMAT(/10H THERE ARE,I3,' FINAL STATE ORBITALS AS FOLLOWS: '/
     :      (1X,18(1X,A3)))
    6 FORMAT(' List common orbitals, terminating with a blank orbital.'/
     :       ' Upper and lower case characters must match.'/
     :       ' Fixed format (18(1X,A3)) as inicated below:'/
     :       '  AAA AAA AAA AAA AAA AAA AAA .... etc (up to 18/line)')
    7 FORMAT(18(1X,A3))
    8 FORMAT(/10H THERE ARE,I3,' COMMON ORBITALS AS FOLLOWS: '/
     :      (1X,18(1X,A3)))
*
      DO 2 I = 1,(90)
         DO 3 J = 1,(90)
            AFTER(I,J) = 0
  3      CONTINUE
  2   CONTINUE
*
      IREAD = IREADI
      NCFG = 0
      DO 100 ISTATE = 1,2
*
*  ---  Determine the number of common closed subshells
*
      READ(IREAD,'(/A72)' ) LINE
      NCLO = 0
      J = 2
 10   IF (LINE(J:J+2) .NE. '   ' ) THEN
         NCLO = NCLO + 1
         J = J+4
         IF (J .LT. 72) GO TO 10
      END IF
      NCLOS(ISTATE) = NCLO
*
*  ---  Determine the number or configurations and electrons
*
      IORB = 0
 20   READ(IREAD,1,END=55) LINE
      IF (LINE(1:1) .NE. '*'  .AND. LINE(2:2) .NE. '*' ) THEN
*
*  ------  A new configuration has been read; find the electrons
*
         NCFG = NCFG + 1
         IF (NCFG .GT. (800) )
     :      STOP ' TOO MANY CONFIGURATIONS: MAX= (800)'
         J = 2
         I = 0
 30      IF (LINE(J:J+2) .NE. '   ' .AND. I.LT.(5)) THEN
*
*  --------- An electron has been found; is it a new one?
*
            I = I+1
            EL(I) = LINE(J:J+2)
            K = 1
 40         IF (K .LE. IORB) THEN
               IF ( OF(K,ISTATE) .NE. EL(I) ) THEN
                  K = K+1
                  IF (K .GT. (30)) STOP ' TOO MANY ELECTRONS: MAX= (30)'
                  GO TO 40
                 ELSE
                  IEL(I) = K
               END IF
              ELSE
*
*  ------------  A new electron has been found; add it to the list
*
               IORB = K
               OF(IORB,ISTATE) = EL(I)
               IEL(I) = K
            END IF
            J = J+8
            GO TO 30
         END IF
         NOCCSH(NCFG) = I
*
*  ------  Add data to the AFTER matrix
*
         DO 50 I1 = 2,I
            DO 51 I2 = 1,I1-1
               J1 = 30*ISTATE + IEL(I1)
               J2 = 30*ISTATE + IEL(I2)
               AFTER(J1,J2) = 1
 51         CONTINUE
 50      CONTINUE
         READ(IREAD,*)
         GO TO 20
      END IF
 55   NORB(ISTATE) = IORB
      ICFG(ISTATE) = NCFG
      IREAD = IREADF
  100 CONTINUE
*
*  ---   SET PARAMETERS
*
      NORBI = NORB(1)
      NORBF = NORB(2)
*
*  ---  DETERMINE THE COMMON INITIAL/FINAL STATE ORBITALS
*
      PRINT 4,NORBI,(OF(I,1),I=1,NORBI)
      PRINT 5,NORBF,(OF(I,2),I=1,NORBF)
      ANS = 'Y'
      IF (.NOT. LORTH) THEN
         PRINT '(/$,A)',
     :     ' Initial & final state orbitals an orthonormal set ? (Y/N) '
         READ(5,'(A1)') ANS
      END IF
      IF ( ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
         DO 53 I = 1,NORBI
            ELC(I) = OF(I,1)
   53    CONTINUE
         NCOM = NORBI
*
*  ---  ADD OTHERS FROM FINAL STATE
*
         DO 54 I = 1,NORBF
            DO 56 J = 1,NORBI
               IF (OF(I,2) .EQ. OF(J,1)) GO TO 54
   56       CONTINUE
            NCOM = NCOM + 1
            IF (NCOM .GT. (30))
     :         STOP ' Too many common electrons: MAX=(30)'
            ELC(NCOM) = OF(I,2)
   54    CONTINUE
        ELSE
         PRINT 6
         READ(5,7) (ELC(I),I=1,18)
         IF (ELC(18) .NE. '   ') READ(5,7) (ELC(I),I=19,(30))
         NCOM = 0
   52    IF (ELC(NCOM+1) .NE. '   ') THEN
            NCOM = NCOM + 1
            IF (NCOM .LT. (30)) GO TO 52
         END IF
      END IF
      PRINT '(//)'
*
*  ---  Transfer electrons to common orthogonal set
*
      DO 200 ISTATE = 1,2
         IORIG = (30)*ISTATE
         LAST = NORB(ISTATE)
         DO 201 I=1,NCOM
*
*        Find electron and transfer AFTER information
*
         J = 1
  202    IF (J .LE. LAST) THEN
            IF (ELC(I) .NE. OF(J,ISTATE) ) THEN
               J = J+1
               GO TO 202
              ELSE
               II = IORIG + J
               DO 210 K = 1, IORIG+LAST
                  IF (AFTER(I,K) .EQ. 0) AFTER(I,K) = AFTER(II,K)
                  IF (AFTER(K,I) .EQ. 0) AFTER(K,I) = AFTER(K,II)
                  AFTER(II,K) = 2
                  AFTER(K,II) = 2
  210          CONTINUE
               NORB(ISTATE) = NORB(ISTATE) - 1
            END IF
           ELSE
            PRINT *,' Common electron ',ELC(I),' not found in ',
     :             LABEL(ISTATE),' state'
         END IF
  201    CONTINUE
  200 CONTINUE
*
*  ---  Check if the ordering of the electrons is inconsistent
*
      DO 60 I = 1,90
         EL(1) = FIND(I,OF,ELC)
         DO 61 J = 1,90
            EL(2) = FIND(J,OF,ELC)
            IF (AFTER(I,J) .EQ. 1 .AND. AFTER(J,I) .EQ. 1) THEN
                WRITE(IWRITE,*) ' The order of ',EL(1),' and ',
     :                EL(2),' is inconsistent'
                STOP
            END IF
 61      CONTINUE
 60   CONTINUE
*
*  ---  Reorder the electrons to satisfy the after relations found
*         in the different configurations
*
      IORD = 1
 70   IF (IORD .LE. NCOM ) THEN
*
*  ------  Search for a row with no 1's in the NCOM rows
*
         DO 71 I = 1,NCOM
            DO 72 J = 1,(30)*2+NORBF
               IF (AFTER(I,J) .EQ. 1 ) GO TO 71
 72         CONTINUE
*
*  ---------  The current row contains all 0's or 2's
*
            IF (AFTER(I,I) .NE. 2 ) THEN
*
*  ------------  We have the next electron; delete the corresponding
*                  rows and columns from the AFTER matrix
*
               LIST(IORD) = ELC(I)
               IORD = IORD+1
               DO 74 J = 1,(30)*2+NORBF
                  AFTER(I,J) = 2
                  AFTER(J,I) = 2
   74          CONTINUE
               GO TO 70
            END IF
 71      CONTINUE
      END IF
      IF (IORD .NE. NCOM+1) THEN
*
*        SEARCH FOR THE ELECTRON NOT INCLUDED
*
      DO 73 I = 1,NCOM
         IF (AFTER(I,I) .NE. 2) THEN
         DO 75 J = (30)+1,(30)*2+NORBF
            IF (AFTER(I,J) .EQ. 1) THEN
               PRINT *, ELC(I),' cannot be included in the common set'
               IL = 1
               IF ( J .GT. 60 ) IL = 2
               PRINT *,' Occurs AFTER ',FIND(J,OF,ELC),' in ',LABEL(IL),
     :                 ' state'
               STOP
            END IF
   75    CONTINUE
         END IF
   73 CONTINUE
      END IF
*
*  ---  ORDER THE REMAINING ELECTRONS FOR THE INITIAL AND FINAL STATE
*
      LAST = NCOM

      LASTEL = NORBI
      DO 300 ISTATE = 1,2
         LAST = LAST + NORB(ISTATE)
  304    IF (IORD .LE. LAST) THEN
         IORIG = (30)*ISTATE
         DO 301 I = IORIG+1, IORIG+LASTEL
            DO 302 J = 1,IORIG+LASTEL
               IF (AFTER(I,J) .EQ. 1) GO TO 301
  302       CONTINUE
*
*           The current row contains no 1's
*
            IF (AFTER(I,I) .NE. 2) THEN
*
*               We have the next electron
*
                IF (IORD.GT.((71))) STOP ' Too many electrons: MAX=(71)'
                LIST(IORD) = OF(I-IORIG,ISTATE)
                IORD = IORD+1
                DO 303 J = 1,IORIG+LASTEL
                   AFTER(I,J) = 2
                   AFTER(J,I) = 2
  303           CONTINUE
                GO TO 304
             END IF
  301    CONTINUE
         END IF
         LASTEL = NORBF
  300 CONTINUE
*
      NORBI = NORB(1)
      NORBF = NORB(2)
      NCLOSI = NCLOS(1)
      NCLOSF = NCLOS(2)
      MCFG = ICFG(1)
      KCFG = ICFG(2) - MCFG
      IF (NCOM .GT. 0) WRITE(IWRITE,8) NCOM,(LIST(I),I=1,NCOM)
      WRITE(IWRITE,4) NORBI,(LIST(I),I=NCOM+1,NCOM+NORBI)
      NOR11 = NCOM + NORBI
      WRITE(IWRITE,5) NORBF,(LIST(I),I=NOR11+1,NOR11+NORBF)
      RETURN
      END
*
*     ------------------------------------------------------------------
*	C F G I N 2
*     ------------------------------------------------------------------
*
      SUBROUTINE CFGIN2(MCFG,KCFG,LORTH)
*
*	Read two sets of configurations and determine the orthogonality
*       conditions between them
*
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*3 EL(71), ELC(30), JAJCMP(71,3)*1
      CHARACTER INPUTI*16,INPUTF*16, ANS*1, HEADI*72,HEADF*72,HEADER*72
      LOGICAL LORTH
      COMMON/INFORM/ IREADI,IWRITE,IOUT,IREADF,ISC(6)
      COMMON/STATES/NCFG,MAXORB,IAJCMP(71),LJCOMP(71),
     :NJCOMP(71),NOCCSH(800),NELCSH(5,800),NOCORB(5,800),J1QNRD(9,3,800)
      COMMON/NOR/NCOM,NORBI,NORBF,IWAR
      INTEGER IEL(2),IBUFF(2)
*
    3 FORMAT(18(1X,A3))
    7 FORMAT(A72)
   22 FORMAT(// 7H STATE ,' (WITH',I3,' CONFIGURATIONS):'/1H ,31(1H-)/)
   23 FORMAT(/10H THERE ARE,I3,21H ORBITALS AS FOLLOWS://
     1 5X,21(1X,A3):/5X,21(1X,A3))
*
*
      PRINT '($,A)', ' .CFG FILE FOR THE INITIAL STATE ? '
      READ(5,'(A16)') INPUTI
      OPEN(UNIT=1,FILE=INPUTI,STATUS='OLD')
*
      PRINT '($,A)', ' .CFG FILE FOR THE FINAL  STATE ? '
      READ(5,'(A16)') INPUTF
      OPEN(UNIT=2,FILE=INPUTF,STATUS='OLD')
*
* --- ANALYZE INITIAL AND FINAL STATE DATA
*
      CALL ANALY2(NCLOSI,NCLOSF,MCFG,KCFG,EL,LORTH)
      REWIND(UNIT=IREADI)
      REWIND(UNIT=IREADF)
*
      MAXORB = NCOM + NORBI + NORBF
*   SET UP THE ELECTRONS
*
      READ(EL,'(A3)') (IAJCMP(I),I=1,MAXORB)
      READ(EL,'(3A1)')((JAJCMP(I,J),J=1,3),I=1,MAXORB)
*
*   SET UP OF LJCOMP
*
      DO 60 I = 1,MAXORB
      IF (JAJCMP(I,1) .EQ. ' ') THEN
         JAJCMP(I,1) = JAJCMP(I,2)
         JAJCMP(I,2) = JAJCMP(I,3)
         JAJCMP(I,3) = ' '
      ENDIF
      LJCOMP(I) = LVAL(JAJCMP(I,2))
      NJCOMP(I) = ICHAR(JAJCMP(I,1)) - ICHAR('1') + 1
   60 CONTINUE
*
* ---- CHECK COMMON CLOSED SHELLS
*
      IF (NCLOSI .NE. NCLOSF)
     :   STOP ' Common closed shells not the same in the two states'
*
      READ(IREADI,7) HEADI
      READ(IREADF,7) HEADF
      HEADER = HEADI(1:34)//'=>'//HEADF(1:34)
      WRITE(IOUT,7) HEADER
*
* --- CHECK CLOSED SHELLS FURTHER
*
      READ(IREADI,3) (ELC(I),I=1,NCLOSI)
      READ(IREADF,3) (EL(I),I=1,NCLOSF)
      DO 1 I = 1,NCLOSF
         J = 1
    2    IF (EL(I) .NE. ELC(J) ) THEN
            J = J+1
            IF (J .LE. NCLOSI) THEN
               GO TO 2
              ELSE
               STOP ' Common closed sub-shells not the same'
            END IF
         END IF
    1 CONTINUE
*
*  MAXORB < 72 ... LINKED TO JAJCMP(71,3)
*                            IAJCMP(71)
*                            LJCOMP(71)
*                            NJCOMP(71)
*  THE DIMENSION OF IORTH IS GIVEN BY THE PRODUCT OF THE ALLOWED
*  NORBI AND NORBF, I.E. ACTUALLY 30X30 = 900
*
*
*   GET INITIAL STATE CONFIGURATIONS
*
      CALL GSTATE(1,MCFG)
*
*
      MCFG1 = MCFG + 1
      NCFG = MCFG + KCFG
      CALL GSTATE(MCFG1,NCFG)
*
*  ---  CHECK THE DATA
*
      CALL CFGTST(NCFG,LJCOMP,NOCCSH,NELCSH,NOCORB,J1QNRD,800)
      RETURN
      END
*
*     ------------------------------------------------------------------
*	C F G T S T
*     ------------------------------------------------------------------
*
      SUBROUTINE CFGTST(NCFG,LJCOMP,NOCCSH,NELCSH,NOCORB,J1QNRD,NDIM)
*
*     THIS SUBROUTINE CHECKS ALL THE CONFIGURATION SET TO ENSURE THAT
*     IT SATISFIES ALL THE FOLLOWING CONDITIONS:
*        (1)  EACH CONFIGURATION HAS THE SAME NUMBER OF ELECTRONS
*        (2)  NO SUBSHELL HAS TOO MANY (.GT.2*(2*L+1))  ELECTRONS
*        (3)  THE ELECTRONS IN ANY ONE SUBSHELL ARE COUPLED TO FORM AN
*             ALLOWED TRIAD OF QUANTUM NUMBERS
*        (4)  THE TRIADS COUPLE TOGETHER IN AN ALLOWED WAY
*
*     IN THE EVENT OF AN ERROR, THE PROGRAM HALTS AT THE COMPLETION
*     OF THE CHECKING.  ANY NUMBER OF S, P, D  ELECTRONS ARE ALLOWED,
*     (BUT .LE.2*(2*L+1)), BUT ONLY UP TO TWO  F  OR  G  ELECTRONS,
*     A FILLED F-SHELL IS ALSO ALLOWED AS WELL AS A SINGLE ELECTRON
*     WITH L.GT.4
*
      COMMON/INFORM/IREAD,IWRITE,IOUT,ISC1,ISC2,ISC3,JSC0,JSC1,JSC2,JSC3
      COMMON/TERMS/NROWS,ITAB(14),JTAB(14),NTAB(219)
*
      DIMENSION LJCOMP(*),NOCCSH(NDIM),NELCSH(5,NDIM),NOCORB(5,NDIM),
     :          J1QNRD(9,3,NDIM)
*
    5 FORMAT(/38H THE TRIAD OF QUANTUM NUMBERS OF SHELL,I3,17H IN CONFIG
     :URATION,I3,24H IS NOT A RECOGNIZED SET)
    7 FORMAT(/22H THE COUPLING OF SHELL,I3,17H IN CONFIGURATION,I3,
     : 38H RESULTS IN AN ILLEGAL COUPLING SCHEME)
   12 FORMAT(//41H CONFIGURATION DATA WRONG, PROGRAM HALTED//)
   15 FORMAT(/17H IN CONFIGURATION,I3,7H, SHELL,I3,28H CONTAINS TOO MANY
     : ELECTRONS)
   17 FORMAT(/14H CONFIGURATION,I3,68H INCLUDES A SHELL OF ANGULAR MOMEN
     :TUM L.GE.3 WITH TOO MANY ELECTRONS)
   18 FORMAT(/14H CONFIGURATION,I3,28H HAS AN INCORRECT NUMBER OF ,
     :        9HELECTRONS)
*
      IALLOW=1
      DO 1 I=1,NCFG
         NELSUM = 0
         N=NOCCSH(I)
         DO 2 J=1,N
            NA=NOCORB(J,I)
            LQU=LJCOMP(NA)
            NC=NELCSH(J,I)
            NELSUM = NELSUM + NC
            JA=J1QNRD(J,1,I)
            JB=J1QNRD(J,2,I)
            JC=J1QNRD(J,3,I)
            LQUMAX = 4*LQU + 2
            IF (NC .GT. LQUMAX) THEN
               WRITE(IWRITE,15) I,J
               IALLOW = 0
               GO TO 2
            ELSE IF ((LQU.EQ.3 .AND. NC.GT.2 .AND. NC.LT.14) .OR.
     :          (LQU.GT.4.AND.NC.GT.1) .OR. (LQU.EQ.4.AND.NC.GT.2)) THEN
               WRITE(IWRITE,17) I
               IALLOW = 0
               GO TO 2
            ELSE IF (NC .EQ. 1) THEN
               IF (JA.EQ.1 .AND. JB.EQ.(2*LQU+1) .AND. JC.EQ.2) GO TO 21
            ELSE
               IF (NC .EQ. LQUMAX) THEN
                  NROW = 2
               ELSE
                  NROW = NTAB1(NC+1,LQU+1)
               END IF
               I1 = ITAB(NROW)
               I2 = JTAB(NROW)
               DO 4 IA = 1,I1
                  I3 = I2+3*IA-1
                  IF (JB .EQ. NTAB(I3)) THEN
                     I3 = I3+1
                     IF (JC .EQ. NTAB(I3)) THEN
                        I3 = I3-2
                        IF (JA .EQ. NTAB(I3)) GO TO 21
                     END IF
                  END IF
    4          CONTINUE
            END IF
            IALLOW = 0
            WRITE(IWRITE,5) J,I
            GO TO 2
*
*     CHECK ON THE COUPLING OF THE TRIADS
*
   21       IF (N.GT.1 .AND. J.GT.1) THEN
               J2 = N+J-1
               J1 = J2-1
               IF (J.EQ.2) J1 = 1
               JD=J1QNRD(J1,2,I)
               JE=J1QNRD(J1,3,I)
               JF=J1QNRD(J2,2,I)
               JG=J1QNRD(J2,3,I)
               IF (JF.GE.(JB+JD) .OR. JF.LE.IABS(JB-JD) .OR.
     :             JG.GE.(JC+JE) .OR. JG.LE.IABS(JC-JE) .OR.
     :             MOD(JC+JE-JG,2).EQ.0 ) THEN
                   WRITE(IWRITE,7) J,I
                   IALLOW = 0
               END IF
            END IF
    2    CONTINUE
         IF (I .EQ. 1) THEN
            NELCS = NELSUM
         ELSE IF (NELSUM .NE. NELCS) THEN
            WRITE(IWRITE,18) I
            IALLOW = 0
         END IF
    1 CONTINUE
      IF (IALLOW .EQ. 0) THEN
         WRITE(IWRITE,12)
         STOP
      END IF
      END
*

*     ------------------------------------------------------------------
*	C F G N 1
*     ------------------------------------------------------------------
*
      SUBROUTINE CFGN1
      IMPLICIT REAL*8(A-H,O-Z)
*
*	Read the configurations for a state and determine the
*	non-orthogonal orbitals
*
      CHARACTER BUFFER*3, IBUFF1*3, IBUFF2*3
      CHARACTER*1 JAJCMP(30,3)
      COMMON/INFORM/IREAD,IWRITE,IOUT,ISC1,ISC2,ISC3,JSC0,JSC1,JSC2,JSC3
      COMMON/OVRLAP/MU,NU,MUP,NUP,NONORT,NOVLPS,IROWMU,IROWNU,ICOLMU,
     : ICOLNU,NORTH,IORDER,NCALLS,LMU,LNU,LMUP,LNUP,JMU,JNU,JMUP,JNUP,
     :     IORTH(435)
      COMMON/STATES/NCFG,MAXORB,IAJCMP(30),LJCOMP(30),
     :NJCOMP(30),NOCCSH(800),NELCSH(5,800),NOCORB(5,800),J1QNRD(9,3,800)
      INTEGER IEL(2),IBUFF(2)
*
      CALL CFGO1(NCFG,MAXORB,IAJCMP,LJCOMP,NJCOMP,NOCCSH,NELCSH,
     :            NOCORB,J1QNRD,800)
*
*  ---  SEPARATE THE ELECTRON LABEL CHARACTERS AND LEFT JUSTIFY
*
      DO 10 I = 1,MAXORB
         WRITE(BUFFER,'(A3)') IAJCMP(I)
	 READ(BUFFER,'(3A1)') (JAJCMP(I,J),J=1,3)
	 IF (JAJCMP(I,1) .EQ. ' ') THEN
	    JAJCMP(I,1) = JAJCMP(I,2)
	    JAJCMP(I,2) = JAJCMP(I,3)
	    JAJCMP(I,3) = ' '
	 END IF
10    CONTINUE
*
*  ---  INITIALIZE THE ORTHOGONALITY ARRAY
*
      M1 = (MAXORB*(MAXORB-1))/2
      DO 20 I = 1,M1
         IORTH(I) = -1
20    CONTINUE
*
*  ---  SET ORBITALS IN THE SAME CONFIGURATION TO BE ORTHOGONAL
*
      DO 63 I = 1,NCFG
      N = NOCCSH(I)
      DO 66 J = 1,N-1
      DO 66 JJ = J+1,N
         I1 = NOCORB(J,I)
         J1 = NOCORB(JJ,I)
         IF (J1 .GT. I1) THEN
            M = I1
            I1 = J1
            J1 = M
         ENDIF
         IORTH(J1+((I1-1)*(I1-2))/2) = 0
   66 CONTINUE
   63 CONTINUE
*
* --- DETERMINE THE NON-ORTHOGONAL ORBITALS
*
      NORTH = 0
      DO 18 J = 1,MAXORB-1
      DO 19 I = J+1,MAXORB
         IJ = J + ((I-1)*(I-2))/2
         IF (JAJCMP(I,2) .EQ. JAJCMP(J,2) .AND.
     :       JAJCMP(I,3) .NE. ' ' .AND.
     :       JAJCMP(J,3) .NE. ' ' .AND.
     :       JAJCMP(I,3) .NE. JAJCMP(J,3) .AND.
     :       IORTH(IJ) .NE. 0 ) THEN
             NORTH = NORTH + 1
             IORTH(IJ) = 1
         ENDIF
19    CONTINUE
18    CONTINUE
*
      READ(IREAD,*,END=90)
 79   READ(IREAD,'(1X, A3, 1X, A3)',END=90) IBUFF1,IBUFF2
      IF (IBUFF1 .NE. '*  ' .AND. IBUFF1 .NE. '   ') THEN
         READ (IBUFF1, '(BN, I3)') IBUFF(1)
         READ (IBUFF2, '(BN, I3)') IBUFF(2)
         DO 80 I = 1,2
            DO 81 J = 1,MAXORB
               IF (IBUFF(I) .EQ. IAJCMP(J)) THEN
                  IEL(I) = J
                  GO TO 80
               ENDIF
 81         CONTINUE
            WRITE(*,'(A,A3,A)') ' ELECTRON ',IBUFF(I),' NOT FOUND'
            STOP
 80      CONTINUE
         IF (IEL(1) .GT. IEL(2) ) THEN
            I = IEL(1)
            IEL(1) = IEL(2)
            IEL(2) = I
         END IF
         IJ = IEL(1) + ((IEL(2)-1)*(IEL(2)-2))/2
         IF (IORTH(IJ) .EQ. 1) NORTH = NORTH - 1
         IORTH(IJ) = 0
         WRITE(IWRITE,'(1X,A3,A,A3)')
     :           IBUFF(1),' is orthogonal to ',IBUFF(2)
         GO TO 79
      END IF
  90  RETURN
      END
*
*     ------------------------------------------------------------------
*	C F G O 1
*     ------------------------------------------------------------------
*
      SUBROUTINE CFGO1(NCFG,MAXORB,IAJCMP,LJCOMP,NJCOMP,NOCCSH,
     :                  NELCSH,NOCORB,J1QNRD,NDIM)
*
*	Read configurations for one state, assuming orthogonality of
*       the orbitals
*
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER BUFFER(30)*3, HEADER*72, ANS*1, INPUT*16
      CHARACTER*1 JAJCLD(30,3),JAJCMP(30,3),J1QN(9)
      COMMON/INFORM/IREAD,IWRITE,IOUT,ISC1,ISC2,ISC3,JSC0,JSC1,JSC2,JSC3
      COMMON /CLOSED/B1ELC(4),NCLOSD,IAJCLD(30),LJCLSD(30),IBK
      INTEGER IEL(2),IBUFF(2)
*
      DIMENSION IAJCMP(30),LJCOMP(30),NJCOMP(30),NOCCSH(NDIM),
     :          NELCSH(5,NDIM),NOCORB(5,NDIM),J1QNRD(9,3,NDIM)
*
    3 FORMAT(18(1X,A3))
    4 FORMAT(3A1)
    5 FORMAT(9(1X,A3,1H(,I2,1H)))
    6 FORMAT(9(1X,4X,I1,A1,I1))
    7 FORMAT(A72)
    8 FORMAT(A3)
   22 FORMAT(// 7H STATE ,' (WITH',I3,' CONGIGURATIONS):'/1H ,31(1H-)/)
   23 FORMAT(/10H THERE ARE,I3,21H ORBITALS AS FOLLOWS://
     : 5X,21(1X,A3):/5X,21(1X,A3))
   25 FORMAT(/14H CONFIGURATION,I3,' ( OCCUPIED ORBITALS=',I2,' ):'
     : ,5(1X,A3,1H(,I2,1H)))
   26 FORMAT(26X,17H COUPLING SCHEME:,5(1X,4X,I1,A1,I1))
   27 FORMAT(54X,4(1X,4X,I1,A1,I1))
   28 FORMAT(/10H THERE ARE ,I3,31H CLOSED SUBSHELLS COMMON TO ALL ,
     :  27H CONFIGURATIONS AS FOLLOWS: //
     :  5X, 21(1X,A3))
*
*
      PRINT '($,A)', ' .CFG FILE FOR THE STATE ? '
      READ(5,'(A16)') INPUT
*
* --- ANALYZE INPUT DATA
*
      OPEN(UNIT=4,FILE=INPUT,STATUS='OLD')
      CALL ANALY1(IREAD,IWRITE,NCLOSD,MAXORB,0,NCFG,NOCCSH,BUFFER,NDIM)
      REWIND(UNIT=IREAD)
*
* ---  Process the configuration data
*
      READ(BUFFER,8) (IAJCMP(I),I=1,MAXORB)
      WRITE(IWRITE,22) NCFG
      WRITE(IWRITE,23) MAXORB,(IAJCMP(I),I=1,MAXORB)
      READ(BUFFER,4)((JAJCMP(I,J),J=1,3),I=1,MAXORB)
      DO 60 I=1,MAXORB
      IF (JAJCMP(I,1) .EQ. ' ') THEN
         JAJCMP(I,1) = JAJCMP(I,2)
         JAJCMP(I,2) = JAJCMP(I,3)
         JAJCMP(I,3) = ' '
      ENDIF
      LJCOMP(I) = LVAL(JAJCMP(I,2))
      NJCOMP(I) = ICHAR(JAJCMP(I,1)) - ICHAR('1') + 1
   60 CONTINUE
*
* --- READ HEADER CARD FOR THE CASE
*
      READ(IREAD,7) HEADER
      WRITE(IOUT,7) HEADER
*
* --- READ IN THE COMMON SET OF CLOSED SUBSHELLS
*
      READ(IREAD,3) (BUFFER(I),I=1,NCLOSD)
      IF (NCLOSD .EQ. 0) GO TO 70
      READ(BUFFER,8) (IAJCLD(I),I=1,NCLOSD)
      WRITE(IWRITE,28) NCLOSD,(IAJCLD(I),I=1,NCLOSD)
      READ(BUFFER,4) ((JAJCLD(I,J),J=1,3),I=1,NCLOSD)
      DO 71 I=1,NCLOSD
      J = 3
      IF (JAJCLD(I,1) .NE. ' ') J = 2
      LJCLSD(I) = LVAL(JAJCLD(I,J))
71    CONTINUE
 70   CONTINUE
*
* --- READ IN (AND PRINT OUT) CONFIGURATIONS ETC. FOR THE STATE UNDER
* --- CONSIDERATION
*
      DO 63 I=1,NCFG
      N=NOCCSH(I)
      READ(IREAD,5)        (NOCORB(J,I),NELCSH(J,I),J=1,N)
      WRITE(IWRITE,25) I,N,(NOCORB(J,I),NELCSH(J,I),J=1,N)
      DO 61 J=1,N
      DO 61 JJ=1,MAXORB
   61 IF(NOCORB(J,I).EQ.IAJCMP(JJ)) NOCORB(J,I)=JJ
      M=2*N-1
      N1=N+1
      READ(IREAD,6)    (J1QNRD(J,3,I),J1QN(J),J1QNRD(J,1,I),J=1,M)
      WRITE(IWRITE,26) (J1QNRD(J,3,I),J1QN(J),J1QNRD(J,1,I),J=1,N)
      IF(N.EQ.1) GO TO 64
      WRITE(IWRITE,27) (J1QNRD(J,3,I),J1QN(J),J1QNRD(J,1,I),J=N1,M)
   64 CONTINUE
      DO 62 J=1,M
   62 J1QNRD(J,2,I) = 2*LVAL(J1QN(J)) + 1
   63 CONTINUE
      CALL CFGTST(NCFG,LJCOMP,NOCCSH,NELCSH,NOCORB,J1QNRD,NDIM)
      RETURN
      END
*
*     ------------------------------------------------------------------
*	F I N D
*     ------------------------------------------------------------------
*
      CHARACTER*3 FUNCTION FIND(I,OF,EL)
*
*  ---  THIS ROUTINE FINDS ELECTRONS IN ONE OF THREE LISTS
*
      INTEGER I
      CHARACTER*3 OF(30,2),EL(30)
*
      IF ( I .LE. 30) THEN
         FIND = EL(I)
        ELSE IF ( I .LE. 60 ) THEN
         FIND = OF(I-30,1)
        ELSE
         FIND = OF(I-60,2)
      ENDIF
      RETURN
      END
*
*     ------------------------------------------------------------------
*	G S T A T E
*     ------------------------------------------------------------------
*
      SUBROUTINE GSTATE(NFIRST,NLAST)
*
      COMMON/INFORM/ IREADI,IWRITE,IOUT,IREADF,ISC(6)
      COMMON/STATES/NCFG,MAXORB,IAJCMP(71),LJCOMP(71),
     :NJCOMP(71),NOCCSH(800),NELCSH(5,800),NOCORB(5,800),J1QNRD(9,3,800)
      COMMON/NOR/NCOM,NORBI,NORBF
      CHARACTER*1 J1QN(9)
      CHARACTER*7 LABEL(2)
      DATA LABEL(1), LABEL(2) / 'INITIAL', 'FINAL' /
*
*      DATA DEFINING THE STATE IS READ IN AND PRINTED OUT.
*
    5 FORMAT(5(1X,A3,1H(,I2,1H)))
    6 FORMAT(9(1X,4X,I1,A1,I1))
   24 FORMAT(//31H INITIAL STATE CONFIGURATIONS:-)
   25 FORMAT(/5H     ,I3,3H.  ,10(1X,A3,1H(,I2,1H)))
   26 FORMAT(11X,10(1X,4X,I1,A1,I1))
   27 FORMAT(22X,9(1X,4X,I1,A1,I1))
   28 FORMAT(  31H ----------------------------  /)
   29 FORMAT(//29H FINAL STATE CONFIGURATIONS:-)
   30 FORMAT(2X,'ELECTRON ',A3,' NOT FOUND IN THE LIST OF ELECTRONS',
     :   ' FOR THE ',A8,' STATE')
      IF (NFIRST .EQ. 1) THEN
         WRITE(IWRITE,24)
         IREAD =IREADI
        ELSE
         WRITE(IWRITE,29)
         IREAD = IREADF
      END IF
      WRITE(IWRITE,28)
      DO 2 I=NFIRST,NLAST
      N=NOCCSH(I)
      READ(IREAD,5)        (NOCORB(J,I),NELCSH(J,I),J=1,N)
      K=I
      IF(NFIRST.NE.1) K=I-NFIRST+1
      WRITE(IWRITE,25) K,(NOCORB(J,I),NELCSH(J,I),J=1,N)
      NCOM1 = NCOM + 1
      NOR11 = NCOM1 + NORBI
      DO 61 J=1,N
      DO 63 JJ = 1,MAXORB
      IF (NFIRST .EQ. 1 .AND. JJ .GE. NOR11) GO TO 65
      IF(NFIRST .NE. 1 .AND. JJ .GE. NCOM1 .AND. JJ .LT. NOR11) GO TO 63
      IF(NOCORB(J,I).EQ.IAJCMP(JJ)) THEN
         NOCORB(J,I) = JJ
         GO TO 61
      END IF
   63    CONTINUE
*
*        ELECTRON NOT FOUND IN THE LIST
*
   65 WRITE(IWRITE,30) NOCORB(J,I),LABEL(NFIRST)
      STOP
   61 CONTINUE
      M=2*N-1
      N1=N+1
      READ(IREAD,6)    (J1QNRD(J,3,I),J1QN(J),J1QNRD(J,1,I),J=1,M)
      WRITE(IWRITE,26) (J1QNRD(J,3,I),J1QN(J),J1QNRD(J,1,I),J=1,N)
      IF(N.EQ.1) GO TO 64
      WRITE(IWRITE,27) (J1QNRD(J,3,I),J1QN(J),J1QNRD(J,1,I),J=N1,M)
   64 CONTINUE
      DO 62 J=1,M
   62 J1QNRD(J,2,I)=2*LVAL(J1QN(J))+1
    2 CONTINUE
      RETURN
      END
*
*     ------------------------------------------------------------------
*	O R T H
*
*     ------------------------------------------------------------------
*
      SUBROUTINE ORTH
*
*	Determine the orthogonality between initial and final state
*
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/STATES/NCFG,MAXORB,IAJCMP(71),LJCOMP(71),
     :NJCOMP(71),NOCCSH(800),NELCSH(5,800),NOCORB(5,800),J1QNRD(9,3,800)
      COMMON/OVRLAP/MU,NU,MUP,NUP,NONORT,NOVLPS,IROWMU,IROWNU,ICOLMU,
     1 ICOLNU,NORTH,IORDER,NCALLS,LMU,LNU,LMUP,LNUP,JMU,JNU,JMUP,JNUP,
     2 IORTH(900)
      COMMON/NOR/NCOM,NORBI,NORBF,IWAR
*
*   SET UP OF IORTH VECTOR
*   THE COMMON SET (NCOM) IS ASSUMED TO BE ORTHOGONAL TO BOTH
*   NORBI AND NORBF SETS
*
      IF (NORBI .EQ. 0) RETURN
      M1 = NORBI*NORBF
      DO 70 I = 1,M1
      IORTH(I) = 0
   70 CONTINUE
      NORTH = 0
*
*  | 1....NCOM | NCOM1.....NOR1 | NOR11.....NOR2|
*  |   NCOM    |     NORBI      |     NORBF     |
*  |   < 31    |     < 31       |     < 31      |
*
*  THIS LIMITATION IS LINKED TO THE DIMENSION OF BUFFER(30) IN
*  ANALYSE SUBROUTINE
*
      NCOM1 = NCOM+1
      NOR1  = NCOM + NORBI
      NOR11 = NOR1 + 1
      NOR2  = NOR1 + NORBF
      DO 78 J = NCOM1,NOR1
      DO 79 I = NOR11,NOR2
         IJ = NORBF*(J-NCOM1) + I - NOR1
         IF (LJCOMP(I) .EQ. LJCOMP(J)) THEN
            NORTH = NORTH + 1
            IORTH(IJ) = 1
         ENDIF
   79 CONTINUE
   78 CONTINUE
      RETURN
      END
*
*     ------------------------------------------------------------------
*	V I J O U T
*     ------------------------------------------------------------------
*
      SUBROUTINE VIJOUT(JA,JB)
      COMMON/INFORM/IREAD,IWRITE,IOUT,ISC1,ISC2,ISC3,JSC0,JSC1,JSC2,JSC3
      COMMON/DEBUG/IBUG1,IBUG2,IBUG3,NBUG6,NBUG7,IFULL
      COMMON/MEDEFN/IHSH,NJ(10),LJ(10),NOSH1(10),NOSH2(10),J1QN1(19,3),
     :     J1QN2(19,3),IJFUL(10)
*
*     THIS SUBROUTINE IS ENTERED ONLY IF IBUG2 IS GREATER THAN ZERO
*
* --- PRINT OUT OF QUANTUM NUMBERS AND COUPLING SCHEMES FOR EACH
*     MATRIX ELEMENT AS DEFINED BY SETUP
*
    5 FORMAT(//48H L.H.S. OF HAMILTONIAN MATRIX ELEMENT DEFINED BY)
    6 FORMAT(//48H R.H.S. OF HAMILTONIAN MATRIX ELEMENT DEFINED BY)
    7 FORMAT(9H1(CONFIG ,I2,10H/V/CONFIG ,I2,1H))
    8 FORMAT(/7H NJ,LJ ,10(I6,I3))
    9 FORMAT(/6H NOSH ,10I4)
   10 FORMAT(6H J1QN ,10(I5,2I3))
      I2HSH=2*IHSH-1
      WRITE(IWRITE,7) JA,JB
      WRITE(IWRITE,8) (NJ(I),LJ(I),I=1,IHSH)
      WRITE(IWRITE,5)
      WRITE(IWRITE,9) (NOSH1(J),J=1,IHSH)
      WRITE(IWRITE,10) ((J1QN1(J,K),K=1,3),J=1,I2HSH)
      WRITE(IWRITE,6)
      WRITE(IWRITE,9) (NOSH2(J),J=1,IHSH)
      WRITE(IWRITE,10) ((J1QN2(J,K),K=1,3),J=1,I2HSH)
    1 RETURN
      END
