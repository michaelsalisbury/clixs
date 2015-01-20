C
C     ------------------------------------------------------------------
C    3-6       D I F F
C     ------------------------------------------------------------------
C
C
C       Stores LP  in the array YK.  The difference approximation of
C                i
C   Eq. (6-14) is used.
C
C
      SUBROUTINE DIFF(I)
        IMPLICIT REAL*8(A-H,O-Z)
        INTEGER IN,OUT,PRI,OUC,OUD,OUF,OUH,IU(3),OU(4)
        COMMON /INOUT/ IN,OUT,PRI,IUC,IUD,IUF,OUC,OUD,OUF,OUH
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
C
C  *****  FORM DD + 2Z/R -L(L+1)/RR|P(I)>
C
      MM = MAX(I) - 3
      FL = L(I)
      TWOZ = Z + Z
      C = (FL+D5)**2
      HH = 180.D0*H*H
      DO 11 K =  4,MM
11    YK(K) = (D2*(P(K+3,I)+P(K-3,I)) - 27.D0*(P(K+2,I)+P(K-2,I)) +
     1   270.D0*(P(K+1,I)+P(K-1,I)) - 490.D0*P(K,I))/HH +
     2   P(K,I)*(TWOZ*R(K) - C)
C
C  *****  BECAUSE OF THE POSSIBILITY OF EXTENSIVE CANCELLATION NEAR THE
C  *****  ORIGIN, SEARCH FOR THE POINT WHERE THE ASYMPTOTIC BEHAVIOUR
C  *****  BEGINS AND SMOOTH THE ORIGIN.
C
      LEXP = L(I) + 2
      Y1 = YK(4)/R2(4)/R(4)**LEXP
      Y2 = YK(5)/R2(5)/R(5)**LEXP
      DO 1 K = 4,100
      KP = K+2
      Y3 = YK(KP)/R2(KP)/R(KP)**LEXP
      IF (Y2 .EQ. D0) GO TO 1
      IF (DABS(Y1/Y2 - D1) .LT..1D0 .OR. DABS(Y3/Y2 - D1) .LT..1D0)
     1       GO TO 2
      Y1 = Y2
      Y2 = Y3
1     CONTINUE
      WRITE(OUT,3)  I
3     FORMAT(6X, 'ASYMPTOTIC REGION NOT FOUND FOR FUNCTION NUMBER',I3)
      STOP
C
C  *****  ASYMPTOTIC REGION HAS BEEN FOUND
C
2     KP = K
      KM = KP - 1
      DO 4 K = 1,KM
4     YK(K) = Y1*R2(K)*R(K)**LEXP
      MM = MM + 1
      YK(MM) = (-(P(MM+2,I)+P(MM-2,I)) + D16*(P(MM+1,I)+P(MM-1,I))
     1      -D30*P(MM,I))/(D12*H*H) + P(MM,I)*(TZ*R(MM) - C)
      MM = MM + 1
      YK(MM) = (P(MM+1,I) + P(MM-1,I) - D2*P(MM,I))/(H*H) +
     1   P(MM,I)*(TZ*R(MM) - C)
      MM = MM+1
      DO 5 K =MM,NO
5     YK(K) = D0
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-8       E K I N
C     ------------------------------------------------------------------
C
C       Returns the value of the integral of
C
C         (2/r)P (Y P  + X )
C               j  i i    i
C
C   integrated with respect to r.
C
C
      DOUBLE PRECISION FUNCTION EKIN(I,II)
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
      CALL XCH(I,2)
      CALL POTL(I)
      DO 1 J=1,NO
      YK(J) = YR(J)
1     YR(J) = P(J,II)
      EKIN = D2*QUADS(I,II,1) + QUAD(II,NO ,YR,X)
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-25      Q U A D
C     ------------------------------------------------------------------
C
C       Evaluates the integral of F(r)G(r) with respect to r , where
C   F(r) and G(r) have the same asymptotic properties as P (r).   The
C                                                         i
C   composite Simpson's rule is used.   The integrand is zero for r >
C   r  .
C    M
C
      DOUBLE PRECISION FUNCTION QUAD(I,M,F,G)
        IMPLICIT REAL*8(A-H,O-Z)
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
	DIMENSION F(880),G(880)
*
      D = (D1 + D5*Z*R(1))/(H1*(2*L(I) + 3))
      QUAD = RR(1)* F(1)*G(1)*( D -D5)
      QUAD2 = D0
      DO 1 J = 2,M,2
      QUAD = QUAD + RR(J-1)*F(J-1)*G(J-1)
      QUAD2 = QUAD2 + RR(J)*F(J)*G(J)
1     CONTINUE
      QUAD = H1*(QUAD + D2*QUAD2)
      RETURN
      END
      LOGICAL FUNCTION SETORT(EL1,EL2)
      CHARACTER*3 EL1,EL2
      CHARACTER*1 S1, S2
C
      IF (EL1(1:1) .EQ. ' ') THEN
          S1 = ' '
        ELSE
          S1 = EL1(3:3)
      END IF
      IF (EL2(1:1) .EQ. ' ') THEN
          S2 = ' '
        ELSE
          S2 = EL2(3:3)
      END IF
C
      IF (S1 .EQ. ' ' .OR. S2 .EQ. ' ') THEN
         SETORT = .TRUE.
        ELSE IF (S1 .EQ. S2) THEN
         SETORT = .TRUE.
       ELSE
         SETORT = .FALSE.
      END IF
      RETURN
      END
C
C     ------------------------------------------------------------------
C    3-36      W A V E F N
C     ------------------------------------------------------------------
C
C       This routine initializes radial functions by the procedure
C   indicated by IND(I).
C
C         Value of IND(I)     Method
C         ---------------     ------
C             -1           Functions read from unit IU2
C              0           Screened hydrogenic functions with ZZ=Z-S(I)
C              1           Functions in memory left unchanged
C                                                  0
C   The set of functions are then orthogonalized, Y (i, i;r) and the
C   diagonal energy parameters computed, when necessary.
C
C
      SUBROUTINE WAVEFN
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
      COMMON ZZ(30),IND(30),PN,Z2,FN,M,K,ZT,
     1   ETI,EKI,AZI,PT(880),MT
*
	CHARACTER EL1*3,AT*6,TT*6,ATM(500)*6,TRM(500)*6,TITLE*24
C
C  *****  GENERATE ARRAYS FOR R,R*R AND SQRT(R) WITH A CONSTANT MESH
C  *****  SIZE IN THE LOG(Z*R) VARIABLE
C
      DO 1 I=1,NO
      R(I)= DEXP(RHO)/Z
      RR(I) = R(I)*R(I)
      R2(I) = DSQRT(R(I))
1     RHO = RHO + H
      RHO = RHO - NO*H
C
C  ***** READ THE WAVEFUNCTIONS
C
      IF (IUF .EQ. 0) GO TO 5
2     READ(IUF,END=5) AT,TT,EL1,MM,ZT,ETI,EKI,AZI,(PT(J),J=1,MM)
      M = MM
      CALL EPTR(EL,EL1,I)
      IF ( I .GT. 0 .AND. IND(I) .EQ. -1) THEN
         ATM(I) = AT
         TRM(I) = TT
         MAX(I) = M
         ZZ(I)  = ZT
         C = D1
         IF ( Z .NE. ZT ) C = Z/ZT
C
C  *****  SCALE RESULTS IF CARDS ARE FOR AN ATOM WITH A DIFFERENT Z
C
         CALL EIJSET(I,I,C*C*ETI)
         AZ(I)  = AZI*C**(L(I)+1)*DSQRT(C)
         DO 11 J = 1,M
            P(J,I) = C*PT(J)
11       CONTINUE
C
C  *****  SET REMAINING VALUES IN THE RANGE = 0.
C
         IF ( M .EQ. NO ) GO TO 12
         M = M +1
         DO 13  J=M,NO
13       P(J,I) = D0
12       IND(I) = -2
      ENDIF
      GO TO 2
C
C  *****  SET PARAMTERS FOR ELECTRONS AND INITIALIZE FUNCTIONS
C
5     DO 9 I = 1,NWF
      IF (IND(I)) 7,8,9
C
C  ***** WAVE FUNCTIONS NOT FOUND IN THE INPUT DATA, SET IND = 0
C
7     IF ( IND(I) .EQ. -2 ) GO TO 4
      IND(I) = 0
      WRITE(OUT,27) EL(I)
27    FORMAT(8X,'WAVE FUNCTIONS NOT FOUND FOR ',A3)
C
C  *****  DETERMINE ESTIMATES OF THE WAVE FUNCTIONS BY THE SCREENED
C  *****  HYDROGENIC APPROXIMATION
C
8     PN = HNORM(N(I),L(I),Z-S(I))
      DO 3 J=1,NO
      P(J,I) = PN*HWF(N(I),L(I),Z-S(I),R(J))/R2(J)
3     CONTINUE
      M = NO
30    IF ( DABS(P(M,I)) .GT. 1.D-15 ) GO TO 31
      P(M,I) = D0
      M = M-1
      GO TO 30
31    MAX(I) = M
C
C  ***** SET THE AZ(I) VALUE
C
      AZ(I) = PN*(D2*(Z - D5*S(I))/N(I))**(L(I) + 1)
      CALL EIJSET(I,I,D0)
C
C  *****  ORTHOGONALIZE TO INNER FUNCTIONS
C
4      IF (I .EQ. 1 ) GO TO 9
      IM = I - 1
      DO 6 II =1,IM
      IF (E(I,II) .EQ. D0) GO TO 6
      PN = QUADR(I,II,0)
      IF ( DABS(PN) .GT. 1.D-8 ) THEN
         PNN = DSQRT(D1 - PN*PN)
         IF (P(50,I) - PN*P(50,II) .LT. D0) PNN = -PNN
         M = MAX0(MAX(I),MAX(II))
         DO 25 J = 1,M
25          P(J,I) =(P(J,I) - PN*P(J,II))/PNN
      END IF
6     CONTINUE
9     CONTINUE
      WRITE(PRI,14)
14    FORMAT(/// 8X,18HINITIAL ESTIMATES  //10X,2HNL,
     1   4X,5HSIGMA,6X,5HE(NL),4X,6HAZ(NL),4X,9HFUNCTIONS//)
C
C  *****  COMPUTE ONE-ELECTRON ENERGY PARAMETERS IF THEY WERE NOT
C  *****  SPECIFIED ON INPUT.
C
      DO 15 I = 1,NWF
C     IF (E(I,I) .EQ. D0) E(I,I) = HL(EL,I,I,REL) - EKIN(I,I)
      K = IND(I) + 2
      IF ( IND(I) .EQ. -2 ) THEN
           TITLE = ' SCALED '//ATM(I)//TRM(I)
        ELSE IF (IND(I) .EQ. 0) THEN
           TITLE = ' SCREENED HYDROGENIC'
        ELSE
           TITLE = ' UNCHANGED'
      END IF
17    WRITE(PRI,19) EL(I),S(I),E(I,I),AZ(I),TITLE
19    FORMAT(9X,A3,F9.2,F11.3,F10.3,3X,A24)
15    CONTINUE
      IF ( IUF .NE. 0) REWIND(UNIT=IUF)
      RETURN
      END
     
*     -----------------------------------------------------------------
*     6-30-88      I N T G R L 
*     -----------------------------------------------------------------

        SUBROUTINE INTGRL
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
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
	CHARACTER END*1, EL1*3, EL2*3, EL3*3, EL4*3
*
    1 FORMAT(1X,A1,I2,1X,A3,1X,A3,1X,I5)
    2 FORMAT(1X,A1,I2,1X,2A3,1X,2A3,1X,I5)
    3 FORMAT(1X,A1,I2,1X,A3,1X,A3,2X,I2,1X,A3,1X,A3,I5)
    4 FORMAT(F14.8,A1,2I3,I5)
C
C ***** READ  THE LIST OF INTEGRALS
C
	LAST = 0
	IC = 1
	I = 1
	READ(IUD,'()')
	DO 10 INT = 1,6
	  IF (INT.NE.4 .AND. INT.NE.5) THEN
*
*            ...F, G, L, or O1 integrals....
*
   12	     READ(IUD,1) END, KVAL(I), EL1, EL2, ICPTR
	     IF (END .EQ. '*') GO TO 16
	     IF (ICPTR+LAST .LE. (NCDIM)) THEN
    	 	   CPTR(I) = ICPTR + LAST
	     ELSE
		   PRINT *,' Too much data - current dimensions =',NCDIM
		   STOP
	     END IF
	     CALL EPTR(EL, EL1,IEL(I,1))
	     CALL EPTR(EL, EL2,IEL(I,2))
	     I = I + 1
	     IF (I .LE. (IDIM) ) GO TO 12
	     PRINT *, ' Too many integrals - MAX =',IDIM
	     STOP
	   ELSE
   14         IF (INT.EQ.5) THEN
*
*	        ... R integrals ...
*
     	        READ(IUD,2) END, KVAL(I), EL1, EL2, EL3, EL4, ICPTR
*
	      ELSE
*
*	         ... O2 integrals ...
*
    	     	READ(IUD, 3) END, K1, EL1, EL2, K2, EL3, EL4
	        KVAL(I) = 64*K1 + K2
	      END IF
	      IF (ICPTR+LAST .LE. (NCDIM)) THEN
    	 	   CPTR(I) = ICPTR + LAST
	      ELSE
		   STOP ' Too much data - current dimensions = (NCDIM)'
	      END IF
*
	     IF ( END .EQ. '*') GO TO 16
             CALL EPTR(EL, EL1, IEL(I,1))
             CALL EPTR(EL, EL2, IEL(I,2))
             CALL EPTR(EL, EL3, IEL(I,3))
             CALL EPTR(EL, EL4, IEL(I,4))
	     I = I + 1
	     IF (I .LE. (IDIM) ) GO TO 14
	     STOP ' Too many integrals - MAX = (IDIM)'
	  END IF
 16	  IF (INT .EQ. 3 .OR. INT .EQ. 4) GO TO 18
*
*	... Read the data ...
*
   20	  READ(IUD,4) COEFF(IC), END, IH(IC), JH(IC), OPTR(IC)
	  IF ( END .NE. '*') THEN
	    IF (INT .LE. 2) THEN
	      COEFF(IC) = ACURAT(COEFF(IC))
	    ELSE
*
*	  ... Shift origin for overlap integrals
*
	      IF (OPTR(IC).LT.0) THEN
	        OPTR(IC) = INTPTR(3) - OPTR(IC)
	      ELSE IF (OPTR(IC).GT.0) THEN
	        OPTR(IC) = INTPTR(2) + OPTR(IC)
	      END IF
	    END IF
	    IC = IC + 1
	    GO TO 20
	  END IF
*
*	... Initialize for next set ..
*
   18	  INTPTR(INT) = I-1
	  LAST = IC-1
   10 	CONTINUE
	RETURN
*
  999   PRINT *,' Electron in ',END,'-data not found in ',
     :          'configuration data'
        STOP
	END

*     -----------------------------------------------------------------
*     6-30-88       A C U R A T
*     -----------------------------------------------------------------

	DOUBLE PRECISION FUNCTION ACURAT(C)
	IMPLICIT REAL*8(A-H,O-Z)
	INTEGER NUM, DEN, I
        DIMENSION DEN(11)
	DATA (DEN(I), I=1,11) /2,3,7,9,15,35,49,175,189,315,441/
	DATA D1/1.D0/
*
	C2 = C*C
	ACURAT = C
	DO 1 I = 1,11
	   PROD = DEN(I)*C2
	   NUM = NINT(PROD)
	   EPS = ABS(NUM-PROD)/DEN(I)
	   IF (EPS .LE. 1.E-8) THEN
	      IF (EPS .NE. 0.) THEN
	         ACURAT = SQRT((NUM*D1)/DEN(I))
	  	 IF (C .LT. 0.) ACURAT = -ACURAT
		 RETURN
	      END IF
	   END IF
  1	CONTINUE
	END

*     -----------------------------------------------------------------
*     6-30-88         S E T E Q L
*     -----------------------------------------------------------------

	LOGICAL FUNCTION SETEQL(I,J)
        IMPLICIT REAL*8(A-H,O-Z)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
	IBEGIN = 1
	IF (I .GT. 1) IBEGIN = IEPTR(I-1) + 1
	IEND = IEPTR(I)
	JBEGIN = 1
	IF (J .GT. 1) JBEGIN = IEPTR(J-1) + 1
	JEND = IEPTR(J)
	SETEQL = .FALSE.
	DO 10 II = IBEGIN,IEND
	   DO 11 JJ = JBEGIN,JEND
	      IF (IJE(II) .EQ. IJE(JJ)) GO TO 10
 11        CONTINUE
	   RETURN
 10	CONTINUE
	SETEQL = .TRUE.
	END
	
*     -----------------------------------------------------------------
*     6-30-88      E I J S E T
*     -----------------------------------------------------------------

        SUBROUTINE EIJSET(I,J,E)
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
	      EIJ(II) = E
	      RETURN
	   END IF
 10     CONTINUE
*
* ***** J-value not found - enter into list
*
	IF (IJE(199) .NE. 0)
     : 	 STOP ' Too many off-diagonal energy parameters'
*
*  ***** Find point at which the insertion should be made
*
	IEND = IEPTR(I)
	IF (IEND .NE. 0) THEN
	   IP = 1
	   IF (I .GT. 1) IP = IEPTR(I-1)+1
 30	   IF (IJE(IP) .LT. J .AND. IP .LE. IEND) THEN
	      IP = IP + 1
              GO TO 30
	   END IF
	ELSE
	   IP = 1
	END IF
*
* *****  IP is the location in which EIJ should be stored
*        Move other data
*
	
	DO 40 JJ = (199)-1,IP,-1
	   IJE(JJ+1) = IJE(JJ)
	   EIJ(JJ+1) = EIJ(JJ)
 40	CONTINUE
*
* ***** Space has been made - insert data
*
	IJE(IP) = J
	EIJ(IP) = E
*
* ***** Update pointers
*
	DO 50 II = I,NWF
	   IEPTR(II) = IEPTR(II) +1
 50	CONTINUE
	END

*     -----------------------------------------------------------------
*     6-30-88      E
*     -----------------------------------------------------------------

	DOUBLE PRECISION FUNCTION E(I,J)
        IMPLICIT REAL*8(A-H,O-Z)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
	IBEGIN = 1
	IF (I .GT. 1) IBEGIN = IEPTR(I-1) + 1
	IEND = IEPTR(I)
	E = 0.D0
	DO 10 II = IBEGIN,IEND
	   IF (IJE(II) .EQ. J) THEN
	      E = EIJ(II)
	      RETURN
	   END IF
 10	CONTINUE
	END

*     -----------------------------------------------------------------
*     6-30-88        C O E F
*     -----------------------------------------------------------------

	DOUBLE PRECISION FUNCTION COEF(INT)
	IMPLICIT REAL*8(A-H,O-Z)
*
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
	COEF = 0.D0
	IBEGIN = 1
	IF (INT .GT. 1) IBEGIN = CPTR(INT-1)+1
	IEND = CPTR(INT)
	DO 1 II = IBEGIN,IEND
	   T = WT(IH(II))*WT(JH(II))*COEFF(II)
 	   IF (OPTR(II).NE.0) T = T*VALUE(OPTR(II))
	   IF (IH(II) .NE. JH(II)) THEN
               T = T+T
           ENDIF
	   COEF = COEF+T
  1	CONTINUE
	END

*     -----------------------------------------------------------------
*     6-30-88        C O V
*     -----------------------------------------------------------------

	DOUBLE PRECISION FUNCTION COV(M)
        IMPLICIT REAL*8(A-H,O-Z)
*
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
	COV = 0.D0
	IBEGIN = INTPTR(4)+1
	IEND =INTPTR(6)
	DO 10 I = IBEGIN,IEND
	   JBEGIN = CPTR(I-1)+1
	   JEND = CPTR(I)
	   DO 12 J = JBEGIN,JEND
	      IF ( OPTR(J) .EQ. M) THEN
                 CC =  COEFF(J)*WT(IH(J))*WT(JH(J))*VALUE(I)
		 IF (IH(J) .NE. JH(J)) THEN
                   CC = 2*CC
                   ENDIF
		 COV = COV + CC
	      END IF
 12	   CONTINUE
 10	CONTINUE
	END
C
C     ------------------------------------------------------------------
C    3-24      P O T L
C     ------------------------------------------------------------------
C
C       Computes and stores the potential function
C                              2(k-1)
C              YR = SUM  a    Y      (j,j;r)
C                   j,k   ijk
C
        SUBROUTINE POTL(I)
        IMPLICIT REAL*8(A-H,O-Z)
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
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
      DO 1 J=1,NO
1     YR(J) = D0
	DO 2 J = 1,NWF
	   IF (I.GT.NCLOSD .AND. J.GT.NCLOSD) GO TO 2
	   C = SUM(J)
	   IF ( I.EQ.J ) C = C - D1
	   CALL YKF(J,J,0,REL)
	   DO 3 JJ = 1,NO
	      YR(JJ) = YR(JJ) + C*YK(JJ)
3          CONTINUE
	   IF ( I.EQ.J .AND. L(I) .GT. 0) THEN
	      DO 4 K = 2,2*L(I)
		 CC = -C*CA(L(I),K)
		 CALL YKF(I,I,K,REL)
		 DO 5 JJ = 1,NO
		    YR(JJ) = YR(JJ) + CC*YK(JJ)
5		 CONTINUE
4	      CONTINUE
	   END IF
2	CONTINUE
*
	SUMI = SUM(I)
	IBEGIN = 1
	IEND = INTPTR(1)
	DO 10 J = IBEGIN,IEND
	   IE = 0
	   IF (IEL(J,1) .EQ. I) THEN
	      IE = IEL(J,2)
           ELSE IF (IEL(J,2) .EQ. I) THEN
              IE = IEL(J,1)
           END IF
           IF (IE .NE. 0) THEN
	      C = COEF(J)/SUMI
	      IF (IEL(J,1) .EQ. IEL(J,2)) C = 2*C
	      CALL YKF(IE,IE,KVAL(J),REL)
	      DO 12 JJ = 1,NO
		 YR(JJ) = YR(JJ) + C*YK(JJ)
 12	      CONTINUE
	   END IF
 10	CONTINUE
	END

*     -----------------------------------------------------------------
*     6-30-88      U P D A T E
*     -----------------------------------------------------------------

	SUBROUTINE UPDATE
	IMPLICIT REAL*8(A-H,O-Z)
*
	INTEGER KVAL, IEL, CPTR, IH, JH, OPTR
	PARAMETER (IDIM=20000,NCDIM=50000)
	COMMON/STATE/WT(500),INTPTR(6),KVAL(IDIM),IEL(IDIM,4),CPTR(IDIM)
     :	       ,VALUE(IDIM),COEFF(NCDIM),IH(NCDIM),JH(NCDIM),OPTR(NCDIM)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :   ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
	LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
*
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
	LOGICAL CHANGE
	IBEGIN =  1
	IEND = INTPTR(3)
	DO 1 I = IBEGIN,IEND
	   IF (VARIED(IEL(I,1)) .OR. VARIED(IEL(I,2))) THEN
	      IF (I .LE. INTPTR(1)) THEN
		 VALUE(I) = FK(IEL(I,1),IEL(I,2),KVAL(I),REL)
              ELSE IF (I .LE. INTPTR(2)) THEN
		 VALUE(I) = GK(IEL(I,1),IEL(I,2),KVAL(I),REL)
	      ELSE
		 VALUE(I) = QUADR(IEL(I,1),IEL(I,2),0)**KVAL(I)
	      END IF
	   END IF
  1	CONTINUE
*
	IBEGIN = IEND + 1
	IEND = INTPTR(4)
	DO 30 I = IBEGIN,IEND
	   CHANGE = .FALSE.
	   DO 31 J = 1,4
	     CHANGE = CHANGE .OR. VARIED(IEL(I,J))
 31	   CONTINUE
	   IF (CHANGE) THEN
	      K1 = KVAL(I)/64
	      K2 = KVAL(I) - 64*K1
              VALUE(I) = QUADR(IEL(I,1),IEL(I,2),0)**K1
     :                  *QUADR(IEL(I,3),IEL(I,4),0)**K2
	   END IF
 30	CONTINUE
	IBEGIN = IEND + 1
	IEND = INTPTR(5)
	DO 10 I = IBEGIN,IEND
	  CHANGE = .FALSE.
	  DO 11 J = 1,4
	     CHANGE = CHANGE .OR. VARIED(IEL(I,J))
 11	  CONTINUE
	  IF (CHANGE) VALUE(I)
     :        = RK(IEL(I,1),IEL(I,2),IEL(I,3),IEL(I,4),KVAL(I),REL)
 10	CONTINUE
*
	IBEGIN = IEND + 1
	IEND = INTPTR(6)
	DO 20 I = IBEGIN,IEND
	   IF (VARIED(IEL(I,1)) .OR. VARIED(IEL(I,2))) 
     :                VALUE(I) = HLC(EL,IEL(I,1),IEL(I,2),REL)
 20	CONTINUE
*
*      ... Test if any of the core functions have changed
*
	CHANGE = .FALSE.
	DO 35 I = 1,NCLOSD
	   CHANGE = CHANGE .OR. VARIED(I)
  35	CONTINUE
	IF (CHANGE .OR. EC.EQ.D0) CALL ECORE(EL,EC,REL)
*
	DO 40 I = 1,NWF
	   VARIED(I) = .FALSE.
 40	CONTINUE
	END

*     -----------------------------------------------------------------
*     6-30-88      X C H
*     -----------------------------------------------------------------

        SUBROUTINE XCH(I,IOPT)
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
     :          ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        COMMON /PARM1/ ICASE, IPTYP(30), IORTHO
 	LOGICAL SAME,EXIT
*
        DO 1 J=1,NO
  1     X(J) = D0
	DO 2 J = 1,NWF
	   IF ((I.LE.NCLOSD .AND. I.NE.J) .OR.
     :         (I.GT.NCLOSD .AND. J.LE.NCLOSD))  THEN
	      DO 4 K = IABS(L(I)-L(J)),L(I)+L(J),2
		 C = - D2*CB(L(I),L(J),K)*SUM(J)
		 CALL YKF(J,I,K,REL)
		 DO 6 JJ = 1,NO
		    X(JJ) = X(JJ) + C*YK(JJ)*P(JJ,J)
  6		 CONTINUE
  4	      CONTINUE
	   END IF
  2	CONTINUE
	SUMI = SUM(I)
	IF (I .LE. NCLOSD) GO TO 51
*
	IBEGIN = INTPTR(1)+1
	IEND = INTPTR(2)
	DO 7 INT = IBEGIN,IEND
	   IE1 = 0
	   IF (IEL(INT,1) .EQ. I) THEN
	      IE1 = IEL(INT,1)
	      IE2 = IEL(INT,2)
	   ELSE IF (IEL(INT,2) .EQ. I) THEN
	      IE1 = IEL(INT,2)
	      IE2 = IEL(INT,1)
	   END IF
	   IF (IE1 .NE. 0) THEN
	      C = D2*COEF(INT)/SUMI
	      CALL YKF(IE1,IE2,KVAL(INT),REL)
	      DO 8 JJ = 1,NO
		 X(JJ) = X(JJ) + C*YK(JJ)*P(JJ,IE2)
 8	      CONTINUE
	   END IF
 7	CONTINUE
*
	IBEGIN = INTPTR(4) + 1
	IEND = INTPTR(5)
	DO 50 INT = IBEGIN,IEND
	   I1 = IEL(INT,1)
	   I2 = IEL(INT,2)
	   J1 = IEL(INT,3)
	   J2 = IEL(INT,4)
	   KK = KVAL(INT)
	   IF ((I1-I)*(I2-I) .EQ. 0 .OR. (J1-I)*(J2-I) .EQ. 0) THEN
	      C = COEF(INT)/SUMI
	      CC = C
C
C  ***** COUNT THE NUMBER OF OCCURRENCES OF I
C
	      IK = 0
	      IF (I1 .EQ. I) IK = IK + 1
	      IF (I2 .EQ. I) IK = IK + 1
	      IF (J1 .EQ. I) IK = IK + 1
	      IF (J2 .EQ. I) IK = IK + 1
	      EXIT = .FALSE.
	      DO 11 II2=1,2
	      DO 12 II1=1,2
	      GO TO (10, 20, 30, 40) IK
10	    CONTINUE
C
C  ***** I OCCURS JUST ONCE IN RK
C
	      IF (I1 .NE. I) GO TO 13
	      GO TO 16
20	      CONTINUE
C
C  ***** I OCCURS TWICE IN THE RK INTEGRAL
C
	      IF (I1 .NE. I) GO TO 13
	      IF (J1 .EQ. I) GO TO 17
C
C  ***** TEST IF THE PAIR (I1,J1) = PAIR (I2,J2)
C
	      ICODE1 = 100*I1 + J1
	      ICODE2 = 100*I2 + J2
	      ICODE3 = 100*J2 + I2
	      SAME = ICODE1 .EQ. ICODE2 .OR. ICODE1 .EQ. ICODE3
	      IF ( .NOT. SAME ) GO TO 15
	      GO TO 17
30	      CONTINUE
C
C  ***** I OCCURS THREE TIMES IN THE RK INTEGRAL
C
C
	      IF (I1 .EQ. I) GO TO 13
	      CALL YKF(I2, J2, KK, REL)
	      DO 33 J = 1,NO
33	        X(J) = X(J) + CC*P(J,I1)*YK(J)
	      CALL YKF(I1, J1, KK, REL)
	      CC = D2*CC
	      DO 34 J = 1,NO
34	         X(J) = X(J) + CC*P(J,I2)*YK(J)
	      GO TO 50
C
C  ***** I OCCURS FOUR TIMES IN RK INTEGRAL
C
40	      CC = D4*CC
	      GO TO 16
17	      CC = D2*CC
16	      EXIT = .TRUE.
15	      CALL YKF(I2,J2,KK,REL)
	      DO 14 J=1,NO
14	         X(J) = X(J) +CC*P(J,J1)*YK(J)
	      IF (EXIT) GO TO 50
13    	      III = I1
	      I1= I2
	      I2= III
	      III = J1
	      J1 = J2
12	      J2 = III
	      III = I1
	      I1 = J1
	      J1 = III
	      III = I2
	      I2= J2
11	      J2= III
	   END IF
50	CONTINUE
*
51	IBEGIN = INTPTR(5) + 1
	IEND = INTPTR(6)
	DO 60 INT = IBEGIN,IEND
C	  ... Include only if off-diagonal ...
	  IF (IEL(INT,1).NE.IEL(INT,2)) THEN
	   I1 = IEL(INT,1)
	   I2 = IEL(INT,2)
	   IF (I1 .NE. I) THEN
	      ITEMP = I1
	      I1 = I2
	      I2 = ITEMP
	   END IF
	   IF (I1 .EQ. I) THEN
	      C = COEF(INT)/SUMI
	      CALL DIFF(I2)
	      DO 62 J = 1,NO
		 X(J) = X(J) + C*YK(J)/R(J)
 62   	      CONTINUE
	      DO 64 II = 1,NCLOSD
		 CC = -D2*(4*L(II)+2)*C
		 CALL YKF(II,II,0,REL)
		 DO 65 J = 1,NO
		    X(J) = X(J) + CC*YK(J)*P(J,I2)
 65		 CONTINUE
		 DO 66 K = IABS(L(I)-L(II)),L(I)+L(II),2
		    CCC = CC*CB(L(I),L(II),K)
		    CALL YKF(I2,II,K,REL)
		    DO 67 J = 1,NO
	 	       X(J) = X(J) - CCC*YK(J)*P(J,II)
 67		    CONTINUE
 66		 CONTINUE
 64	      CONTINUE
	   END IF
	   IF (I .LE. NCLOSD) THEN
	      C = -D2*COEF(INT)
	      CALL YKF(I1,I2,0,REL)
	      CC = D2*C
	      DO 61 J = 1,NO
		X(J) = X(J) + CC*YK(J)*P(J,I)
 61	      CONTINUE
	      DO 63 K = IABS(L(I)-L(I1)),L(I)+L(I1),2
		CC = C*CB(L(I),L(I1),K)
		CALL YKF(I2,I,K,REL)
		DO 68 J = 1,NO
		   X(J) = X(J) - CC*YK(J)*P(J,I1)
 68		CONTINUE
		CALL YKF(I1,I,K,REL)
		DO 69 J = 1,NO
		   X(J) = X(J) - CC*YK(J)*P(J,I2)
 69		CONTINUE
 63	      CONTINUE
	   END IF
	  END IF
 60	CONTINUE
	IF (I .LE. NCLOSD) GO TO 71
*
	IBEGIN = INTPTR(2) + 1
	IEND = INTPTR(3)
	DO 70 INT = IBEGIN,IEND
	   I1 = IEL(INT,1)
	   I2 = IEL(INT,2)
	   K1 = KVAL(INT)
	   IF (I1 .NE. I) THEN
	      ITEMP = I1
	      I1 = I2
	      I2 = ITEMP
   	   END IF
	   IF (I1 .EQ. I) THEN
              WRITE(*,*) '5. '
	      C = COV(INT)/SUMI
	      IF (K1 .GT. 1) C = C*K1*QUADR(I1,I2,0)**(K1-1)
	      DO 72 J = 1,NO
	         X(J) = X(J) + C*P(J,I2)*R(J)
 72	      CONTINUE
	   END IF
 70	CONTINUE
*
	IBEGIN = IEND + 1
	IEND = INTPTR(4)
	DO 80 INT = IBEGIN,IEND
	   I1 = IEL(INT,1)
	   I2 = IEL(INT,2)
	   I3 = IEL(INT,3)
	   I4 = IEL(INT,4)
	   K1 = KVAL(INT)/64
	   K2 = KVAL(INT) - 64*K1
	   OV1 = D0
	   OV2 = D0
	   DO 82 II = 1,2
	      IF (I1 .NE. I) THEN
		 ITEMP = I1
		 I1 = I2
		 I2 = ITEMP
	      END IF
	      IF (I1 .EQ. I) THEN
		 IF (OV2 .EQ. D0) OV2 = QUADR(I3,I4,0)
	         C = OV2**K2*COV(INT)/SUMI
		 IF (OV1 .EQ. D0 .AND. K1 .GT. 1)
     :		    OV1 = QUADR(I1,I2,0)
		 IF (K1 .GT. 1) C = K1*C*OV1**(K1-1)
		 DO 84 J = 1,NO
		    X(J) = X(J) + C*P(J,I2)*R(J)
 84		 CONTINUE
	      END IF
	      ITEMP = I1
	      I1 = I3
	      I3 = ITEMP
	      ITEMP = I2
	      I2 = I4
	      I4 = ITEMP
	      ITEMP = K1
	      K1 = K2
	      K2 = ITEMP
	      OTEMP = OV1
	      OV1 = OV2
	      OV2 = OTEMP
 82        CONTINUE
 80 	CONTINUE

* ###
 71     IF (ICASE .EQ. 2 .AND. I .EQ. NWF) THEN
          DO 101 J=1,NO
            X(J) = 0.D0
101       CONTINUE
          GOTO (75,75,102), IOPT
        ELSE
          GOTO (75,76,77), IOPT
        ENDIF
        WRITE (*,*) 'ILLEGAL VALUE FOR IOPT = ', IOPT
	STOP

 76     DO 78 J=1,NO
          X(J) = X(J)/R(J)
 78     CONTINUE
        GOTO 75

 77     DO 79 J=1,NO
          X(J) = R(J)*X(J)
 79     CONTINUE

102     DO 74 J = 1,NWF
           IF (J .NE. I) THEN
             C = E(I,J)
* ###
             IF (IORTHO .EQ. 1) C = 0.D0
             IF (DABS(C) .LE. 1.D-10 ) GO TO 74
             DO 73 JJ = 1,NO
 73          X(JJ) = X(JJ) + C*P(JJ,J)*RR(JJ)
	   END IF
 74     CONTINUE
C
C  *****  CHECK IF EXCHANGE IS ZERO: IF SO, METHOD 2 SHOULD BE USED.
C
 75     IF (METH(I) .EQ. 2) RETURN
* ###
        IF ( DABS(X(1)) + DABS(X(2)) + DABS(X(3)) .EQ. D0 ) METH(I) = 2
        END

* ---------------------------------------------------------------------

        REAL*8 FUNCTION CHKSUM(X)
	INTEGER I, J, K
	REAL*8 X(880), TMP

	TMP = 0.D0
	DO 10 I=1,880
          TMP = TMP + X(I)
10      CONTINUE
	CHKSUM = TMP
	RETURN
	END

* ----------------------------------------------------------------------

