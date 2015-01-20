*
*     ------------------------------------------------------------------
*               C N M R V
*     ------------------------------------------------------------------
*
*       Solutions of the differential equation
*               Y" = YR y + F
*   are obtained in the asymptotic region, NJ .. M, and a particular
*   solution in the region 1 .. NJ+1 by
*   outward integration.   A possibly small correction due to
*   exchange in the asymptotic region is obtained through inward
*   integration.
*
*
      SUBROUTINE CNMRV(I,M,AZZ,PDE)
        IMPLICIT REAL*8(A-H,O-Z)
        DIMENSION PDE(*)
        character ans*1
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :  ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
      COMMON /CONTIN/FK(30,880),FH(30,1040),XH(1040),CD(30),FL(30),
     :               ZL(30),ZF(30),V(30),NJ(30),MP(30),IX(30)
*
*  *****  INTEGRATE OUTWARD TO NJ+1
*
*      write (6,*) 'Output CNMRV data? '
*      read (5,1000) ans
      ans = '*'
      if (ans .eq. 'Y' .or. ans .eq. 'y') then
        open (8, file='cnmrv.out', status='new')
        write (8,*) m, nj(i), azz
        write (8,*)
        write (8,1005) (pde(j), fk(i,j), j=1,880)
        write (8,*)  
        write (8,1005) (fh(i,j), xh(j), j=1,1040)
        write (8,*)
        write (8,1010) (x(j), j=1,880)
        write (8,*)
      endif
      Y1 = PDE(1)
      Y2= PDE(2)
      G1 = FK(I,1)
      G2 = FK(I,2)
      DO 11 J = 3,NJ(I)+1
      G3 = FK(I,J)
      Y3 = (Y2+Y2-Y1 + (D10*G2*Y2 + G1*Y1) + X(J-1)) / (D1 - G3)
      PDE(J) = Y3
      Y1 = Y2
      Y2 = Y3
      IF (ABS(Y3) .GT. 1.E5) THEN
*
*       ... Scale down to avoid overflow
*
         AZZ = AZZ/100.D0
         DO 12 JJ = 1,J
            PDE(JJ) = PDE(JJ)/100.D0
12       CONTINUE
         Y1 = Y1/100.D0
         Y2 = Y2/100.D0
      END IF
*
C      IF (ABS(Y3) .LT. 1.E-5) THEN
*
*       ... Scale down to avoid overflow
*
C         AZZ = AZZ*100.D0
C         DO 1201 JJ = 1,J
C            PDE(JJ) = PDE(JJ)*100.D0
C1201     CONTINUE
C         Y1 = Y1*100.D0
C         Y2 = Y2*100.D0
C      END IF
*
      G1 = G2
11     G2 = G3
*
*     Redefine Y2 for half the step-size
*
13    CONTINUE
      G1 = FH(I,1)
      G2 = FH(I,2)
      G3 = FH(I,3)
      Y2 = (-XH(2) + Y1+Y3 - G1*Y1 - G3*Y3)/(D2 + D10*G2)
      Y1 = Y2
      Y2 = Y3
      G1 = G2
      G2 = G3
      JH = 4
      DO 20 J = NJ(I)+2,M
         DO 22 K = 1,2
            G3 = FH(I,JH)
            Y3 = (Y2+Y2-Y1 + (D10*G2*Y2 + G1*Y1) + XH(JH-1)) / (D1 - G3)
            Y1 = Y2
            Y2 = Y3
            G1 = G2
            G2 = G3
            JH = JH+1
22       CONTINUE
         PDE(J) = Y3
20    CONTINUE
15    CONTINUE
      if (ans .eq. 'Y' .or. ans .eq. 'y') then
        write (8,1010) (pde(j), j=1,880)
        write (8,*)
        close (8)
      endif
1000  format (a1)
1005  format (2x, g20.14, 2x, g20.14)
1010  format (2x, g20.14)
      END
*
*     ------------------------------------------------------------------
*               C S O L V E
*     ------------------------------------------------------------------
*
*        SOLVE computes the potential and exchange
*   function and initializes variables for the i'th radial  equation.
*
*
      SUBROUTINE CSOLVE(I,DELTA,LAST)
        IMPLICIT REAL*8(A-H,O-Z)
        LOGICAL CONV,LAST,DIAG
        REAL*8 K
        CHARACTER CONFIG*40,EL*3,ATOM*6,TERM*6,COUPLE*3
        COMMON /LABEL/CONFIG(500),EL(30),ATOM,TERM,COUPLE(500,9)
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :  ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
*
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
        COMMON /WAVE/EC,ED,AZD,PDE(880),SUM(30),S(30),DPM(30),
     :      ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
      COMMON /CONTIN/FK(30,880),FH(30,1040),XH(1040),CD(30),FL(30),
     :               ZL(30),ZF(30),V(30),NJ(30),MP(30),IX(30)
      COMMON FC(30),FCP(30),GC(30),GCP(30)
      COMMON /PARM1/ ICASE, IPTYP(30), IORTHO
      COMMON /PHASE/ EKK(30),MJ(30),DELT(30)
*
        PI = ACOS(-D1)
        ED = E(I,I)
        K = SQRT(-ED)
        AZD = AZ(I)
*
        IPR = I 
        M = MAX(I)
*
*        WRITE(6,*) I,E(I,I),K,AZ(I),MAX(I),NJ(I),MP(I),CD(I),FL(I)
*     :             ,ZL(I),ZF(I),V(I),NJ(I),MP(I),IX(I)
       CALL XCH(I,3)
*       write (6,*) 'CSOLVE SUM X(J) = ', CHKSUM(X)
*       write (6,*) 'CSOLVE EXCHANGE: j, r(j), x(j) ='
*       write (6,1000) (j, r(j), x(j), j=1,no)
*
*     ...Find M and MJ
*
        IF (IX(I) .EQ. 0) THEN
5          IF (K*(R(M)-R(M-1)) .GT. 2.D0) THEN
              M = M-1
              IF (M .GT. NJ(I) ) GO TO 5
           END IF
           MJ(I) = M
           MR = MP(I)
           CN = ZF(I)*ZF(I) + FL(I)*(FL(I)+1)*K*K
8          IF (CN/(2*R(MR)*K*K*(R(MR)*K*K + ZF(I))) .GT. 1D-6) THEN
              MR = MR+1
              IF (MR .LT. M) GO TO 8
           END IF
           PRINT *, '    M  =',M
           PRINT *, '    MR =',MR
        ELSE
2          IF (ABS(X(MJ(I))/FK(I,MJ(I))) .LT. 0.04096) THEN
              MJ(I) = MJ(I) -1
              IF (MJ(I) .GT. MP(I)) THEN 
               GO TO 2
               ELSE
                    MJ(I) = MJ(I) + 1
            END IF
           END IF
           PRINT *, '    MJ =',MJ(I)
        END IF
        IF (M-NJ(I) .GE. (520)) THEN
           WRITE(6,*) ' Outer region contains ',M-NJ(I)+1,' points'
           WRITE(*,*) ' Maximum allowed value is (520)'
           STOP
        END IF
        IF (MJ(I) .EQ. M .AND. IX(I) .NE. 0) THEN
           WRITE(6,'(/1X,A,I4)')
     :       'WARNING: Outer region may be truncated. M = MJ(I) =',M
        ELSE
           IX(I) = 1
        END IF
*
*        WRITE(6,*) I,K*K,NJ(I),MJ(I),M,IX(I)
*     ...Interpolate X
*
        JH = 1
        DO 3 J = NJ(I),M
           XH(JH) = X(J)
           XH(JH+1) = (9.*(X(J)+X(J+1)) -X(J-1)-X(J+2))/D16
           JH = JH + 2
3       CONTINUE
*     ... Compute the RHS of the Numerov equation for the outer region
*
        CHH = CH/D4
        JH = JH-2
        X1 = XH(1)
        X2 = XH(2)
        DO 4 J = 2,JH
           X3 = XH(J+1)
           XH(J) = CHH*(X1 + D10*X2 +X3)
           X1 = X2
           X2 = X3
4       CONTINUE
*
*     ... Compute the RHS of the Numerov equation
*
      XY = X(1)
      XP = X(2)
*      write (6,*) 'XY, XP = ', XY, XP
      X1 = X(1)
      X2 = X(2)
      X3 = X(3)
      X4 = X(4)
      DO 1 J = 3,NJ(I)
      X5 = X(J+2)
      X(J) =CH*(-X5+24.D0*(X4+X2) + 194.D0*X3 - X1)/20.D0
      X1 = X2
      X2= X3
      X3 = X4
1     X4 = X5
*
*       ... Add the deferred difference correction to the exchange
*           for the outward integration region
*
      X1 =    P(1,I)*FK(I,1)
      X2 =    P(2,I)*FK(I,2)
      X3 =    P(3,I)*FK(I,3)
      X4 =    P(4,I)*FK(I,4)
      DO 7 J = 3,NJ(I)
      X5 =     P(J+2,I)*FK(I,J+2)
      X(J) = X(J) - (X5 -D4*(X2 + X4) + D6*X3 +X1)/20.D0
      X1 = X2
      X2 = X3
      X3 = X4
7     X4 = X5
      RL = L(I) + 2.5
      X(2) = R(2)**RL*(X(5)/R(5)**RL - D3*(X(4)/R(4)**RL -
     :          X(3)/R(3)**RL))
*
*  *****  COMPUTE STARTING VALUES FROM SERIES EXPANSION
*
99      CC = D2*FL(I) + D3
      IF (ICASE .EQ. 1) THEN
        A2 = (V(I) + ED/D2 + ZL(I)*Z)/CC
        A3 = -((V(I) + ED/D2)*ZL(I) + Z*A2)/(D3*(FL(I)+D2))
      ELSEIF (ICASE .EQ. 2) THEN
        A2 = (-V(I) + ED/D2 + ZL(I)*Z)/CC
        A3 = ((-V(I) + ED/D2)*ZL(I) + Z*A2)/(D3*(FL(I)+D2))
      ENDIF
*      write (6,*) 'A2 = ', A2
*      write (6,*) 'A3 = ', A3
      DO 6 J = 1,2
        IF (ICASE .EQ. 1) THEN
          PDE(J) = AZ(I)*R(J)**L(I)*R2(J)*
     :                       (R(J)*(R(J)*(R(J)*A3 + A2) -ZL(I)) +D1)
        ELSEIF (ICASE .EQ. 2) THEN
          PDE(J) = AZ(I)*R(J)**L(I)*R2(J)*
     :                       (R(J)*(R(J)*(R(J)*A3 + A2) +ZL(I)) +D1)
        ENDIF
6     CONTINUE
      PDE(1) = PDE(1) + XY/(D2*CC)
      PDE(2) = PDE(2) + XP/(D2*CC)
*
*  *****  OBTAIN PARTICULAR SOLUTION
*
*      write (6,*) 'EXCHANGE BEFORE CNMRV: j, r(j), x(j) = '
*      write (6,1000) (j, r(j), x(j), j=1,no)
*      write (6,*) 'BEFORE CNMRV SUM  X(J) = ', CHKSUM(X) 
*      write (6,*) '             SUM XH(J) = ', CHKSUM(XH)
*      write (6,*) 'before CNMRV: quadr(2,1,0) = ', quadr(2,1,0)
*      write (6,*) '              quadr(2,2,0) = ', quadr(2,2,0)
      CALL CNMRV(I,M,AZ(I),PDE)
      do 812 j=1,m
812     p(j,i) = pde(j)
      do 813 j=m+1,no
813     p(j,i) = 0.d0

*      write (6,*) 'after CNMRV: quadr(2,1,0) = ', quadr(2,1,0)
*      write (6,*) '             quadr(2,2,0) = ', quadr(2,2,0)
*      write (6,*) 'AFTER CNMRV: j, r(j), pde(j) = '
*      write (6,1000) (j, r(j), pde(j), j=1, no)
*
        LL=L(I)
        DO 90 J = MJ(I) -1,MJ(I)
           IF (ICASE .EQ. 1) THEN
         CALL RCWFN(K*R(J),-ZF(I)/K,LL,LL,FC,FCP,GC,GCP,1.E-8,IRET)
           ELSEIF (ICASE .EQ. 2) THEN
         CALL RCWFN(K*R(J),ZF(I)/K,LL,LL,FC,FCP,GC,GCP,1.E-8,IRET)
           ENDIF

           IF (IRET .NE. 0) THEN
              PRINT '(A,I2,A,I2,A,E12.4)', ' IRET =',IRET,' FOR L=',LL,
     :                                 ' AND R=',R(J)
              STOP
           END IF
           IF ( J.EQ.MJ(I) ) THEN
              F2 = FC(LL+1)
              G2 = GC(LL+1)
           ELSE
              F1 = FC(LL+1)
              G1 = GC(LL+1)
           END IF
90      CONTINUE
*        write (6,*) 'F1 = ', F1
*        write (6,*) 'G1 = ', G1
*        write (6,*) 'F2 = ', F2
*        write (6,*) 'G2 = ', G2
        DNUM = R2(MJ(I))*PDE(MJ(I))*F1-R2(MJ(I) -1)*PDE(MJ(I) -1)*F2
        DENO = R2(MJ(I)-1)*PDE(MJ(I)-1)*G2 - R2(MJ(I))*PDE(MJ(I))*G1
        DELTA = ATAN( DNUM/DENO )
        AMP = R2(MJ(I))*PDE(MJ(I))/(F2 + G2*(DNUM/DENO) )
*        write (6,*) 'AMP = ', AMP, 'DELTA=',DELTA
         IF (AMP .LT. D0) THEN 
            AMPP = -AMP
         ELSE
            AMPP = AMP
         ENDIF
        CNORM = SQRT(D2)* COS(DELTA) / AMPP / SQRT(PI*K)
* ###
*        write (6,*) 'before CNORM: quadr(2,1,0) = ', quadr(2,1,0)
*        write (6,*) '              quadr(2,2,0) = ', quadr(2,2,0)
800     CONTINUE
        DO 92 J = 1,M
           PDE(J) = PDE(J)*CNORM
92      CONTINUE
*        write (6,*) 'AFTER NORMALIZATION: j, r(j), pde(j) ='
*        write (6,1000) (j, r(j), pde(j), j=1,m)
           AZ(I)=CNORM*AZ(I)
*      WRITE(6,*) I,AZ(I)
      DO 13 J = 1,M
      DIFF = P(J,I) - PDE(J)
13    P(J,I) = PDE(J)
*      write (6,*) 'after CNORM: quadr(2,1,0) = ',quadr(2,1,0)
*      write (6,*) '             quadr(2,2,0) = ',quadr(2,2,0)
      IF (TRACE) THEN
        PRINT *,'PDE(M-1)',PDE(M-1)
        PRINT *,'PDE(M)',PDE(M)
        PRINT *,'NUMERATOR  =',DNUM
        PRINT *,'DENOMINATOR=',DENO
      END IF
      MAX(I) = M
*
*      WRITE(6,*) I,K*K,NJ(I),MJ(I),M,DELTA,AMP,CNORM
*       Orthogonalize
*
      IBEGIN = 1
      IF (I .GT. 1) IBEGIN = IEPTR(I-1)+1
      IP = IBEGIN
50    JI = IJE(IP)
      IF ( JI .NE. I) THEN
* ###          
         CC = QUADR(I,JI,0)
         IF (IORTHO .EQ. 1) CC = D0
         DO 51 J = 1,M
* ###
            P(J,I) = P(J,I) - CC*P(J,JI)
51       CONTINUE
*         write (6,*) 'AFTER CC: j, r(j), p(j,i) = '
*         write (6,1000) (j, r(j), p(j,i), j=1,m)
         AZ(I) = AZ(I) - CC*AZ(JI)
         WRITE(6,63) EL(JI),EL(I),CC
63       FORMAT(6X,'CSOLVE <',A3,'|',A3,'>=',G12.5)
         IP = IP+1
         IF (IP .LE. IEPTR(I)) GO TO 50
      END IF
      VARIED(I) = .TRUE.
      DP = ABS((AZD - AZ(I))/AZ(I))
      DPM(I) = DP
      WRITE(6,17) EL(I),DELTA,AZ(I),CNORM,'c',DP
17    FORMAT(20X,A3,2F15.7,F12.7,A2,1PD10.2)
      IF (LAST) THEN
      WRITE(6,*) M-1,PDE(M-1)
      WRITE(6,*) M, PDE(M)
      DO 130 J= 1,NWF
 130      MAX(I)= MAX0(MAX(J),MAX(I))
      IF (MAX(I) .LE. M) GO TO 193
      IF(-ED .NE. 0.) THEN
      DO  105 J=M-1,M
        CALL RCWFN(K*R(J),-ZF(I)/K,LL,LL,FC,FCP,GC,GCP,1.E-8,IRET)
          FLH = FC(LL+1)
          GLH = GC(LL+1)
          PDE(J) = (AMP*(FLH+(DNUM/DENO)*GLH))/R2(J)
*          PDE(J) = CNORM * PDE(J)
          P(J,I) = CNORM * PDE(J)
 105  CONTINUE
      WRITE(6,*) M-1,PDE(M-1),M,PDE(M)
      ENDIF
 193  CONTINUE
      WRITE(6,*) (MAX(J),J=1,NWF)
      WRITE(6,1001) K*K,DELTA,NJ(I),M,MJ(I)
1001  FORMAT(2X,'K*K=',F10.6,2X,'DELTA=',F10.6,2X,'NJ=',I5,2X,'M=',I5,
     :      2X,'MJ=',I5)
      ENDIF 
1000  format (1x, i3, 2x, g10.5, 2x, g12.5)
*      DO 255 J=1,M
*255    WRITE(*,*) 'P(',J,') = ',P(J,I)
      END
*
*     ------------------------------------------------------------------
*               C O U L O M
*     ------------------------------------------------------------------
*
*       Solutions of the differential equation
*               Y" = YR y + F
*   are obtained in the asymptotic region, NJ .. M,
*
*
      SUBROUTINE COULOM(I,EK)
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL FOUND
*
      COMMON /PARAM/H,H1,H3,CH,EH,RHO,Z,TOL,NO,ND,NWF,MASS,NCFG,IB,IC,ID
     :  ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
*
        COMMON /RADIAL/R(880),RR(880),R2(880),P(880,30),YK(880),YR(880)
     :          ,X(880),AZ(30),L(30),MAX(30),N(30)
      COMMON/CONTIN/FK(30,880),FH(30,1040),XH(1040),CD(30),FL(30), 
     :              ZL(30), ZF(30),V(30),NJ(30),MP(30),IX(30)
        LOGICAL FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED
        COMMON /TEST/FAIL,OMIT,EZERO,REL,ALL,TRACE,VARIED(30)
        COMMON /PARM1/ ICASE, IPTYP(30), IORTHO
*
*  *****  Set arrays for the asymptotic region
*
        DEP = 1.35D0
        ALPHA = 0.d0
        JJ = 0
        M = MAX(I)
        CALL POTL(I)
*        write (6,*) 'IN COULOM: j, r(j), yr(j) = '
*        write (6,1000) (j, r(j), yr(j), j=1,m)
        FL(I) = L(I)
        ZL(I) = Z/(FL(I)+D1)
        ZF(I) = (Z - YR(NO))
        V(I) = YR(1)/R(1)
        CD(I) = (FL(I)+D5)**2
*
*     ... Compute the direct function
*
      DO 4 J = 1,M
        VEP = -ALPHA * R(J)**4 / (RR(J) + DEP*DEP)**3
        IF (ICASE .EQ. 1) THEN
          FK(I,J) = (-D2*(Z - YR(J))*R(J) + CD(I) + EK*RR(J))*CH
        ELSEIF (ICASE .EQ. 2) THEN
      FK(I,J) =(D2*(Z - YR(J))*R(J) + VEP + CD(I) + EK*RR(J))*CH
        ENDIF
4     CONTINUE
*      write (6,*) 'IN COULOM: j, r(j), fk(j) = '
*      write (6,1000) (j, r(j), fk(j), j=1,m)
*
*  *****  SEARCH FOR THE POINT AT WHICH FK(j)<0 FOR j>NJ
*
        NJ(I) = M
5       IF ( FK(I,NJ(I)) .LT. D0 ) THEN
           NJ(I) = NJ(I) -1
           IF (NJ(I) .GT. 360 ) GO TO 5
        END IF
        NJ(I) = NJ(I) +1
        PRINT *, '    NJ =',NJ(I)
        MP(I) = M
6       IF ( YR(MP(I)) - YR(MP(I) -1) .LT. Z*1.D-6 ) THEN
           MP(I) = MP(I)-1
           IF (MP(I) .GE. NJ(I)) GO TO 6
        END IF
        MP(I) = MP(I) +1
        PRINT *, '    MP =',MP(I)
*
        EXPH = EXP(H/D2)
        CHH = CH/D4
        JH = 1
        MM = MIN0(NO-2,NJ(I)+396)
        DO 50 J = NJ(I),MM
           FH(I,JH) = FK(I,J)/D4
           YRH = (9.*(YR(J)+YR(J+1))-YR(J-1)-YR(J+2))/D16
           RH = R(J)*EXPH
           RRH = RH*RH
           VEP = -ALPHA*RRH*RRH/(RRH + DEP*DEP)**3
           IF (ICASE .EQ. 1) THEN
             FH(I,JH+1) =(-D2*(Z-YRH)*RH +CD(I) + EK*RRH)*CHH
           ELSEIF (ICASE .EQ. 2) THEN
             FH(I,JH+1)=(D2*(Z-YRH)*RH+VEP +CD(I)+EK*RRH)*CHH
           ENDIF
           JH = JH+2
50      CONTINUE
*        write (6,*) 'IN COULOM: j, r(j), fh(j) = '
*        write (6,1000) (j, r(j), fh(j), j=1,mm-nj+1)
        IX(I) = 0
        WRITE(6,*) I,IX(I)
C      WRITE(6,*) I,EK,MAX(I),FL(I),ZL(I),ZF(I),V(I),CD(I),NJ(I),MP(I)
C     :           ,IX(I)
1000  format (1x, i3, 2x, g10.5, 2x, g12.5)
        END
C
C     ------------------------------------------------------------------
C    3-5       D I A G
C     ------------------------------------------------------------------
C
C       The CDIAG subroutine computes an energy matrix and if the number
C   of configurations is greater than  1,  finds  an  eigenvalue  and
C   eigenvector of this matrix.
C
C       Given the accelerating parameter for configuration mixing,
C   ACFG, the  new mixing coefficients are stored in COMMON.
C
C
      SUBROUTINE CDIAG(ETOTAL,ACFG,LAST)
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
     :  ,D0,D1,D2,D3,D4,D5,D6,D8,D10,D12,D16,D30,FINE,NSCF,NCLOSD,RMASS
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
     :     ACC(30),METH(30),IEPTR(30),IJE(199),EIJ(199),VIJ(199),IPR
*
        LOGICAL LAST
        COMMON WP(500),W(500,500)
*
        DO 1 I = 1,NCFG
           DO 2 J = 1,NCFG
              W(I,J) = D0
  2        CONTINUE
          WP(I) = WT(I)
           W(I,I) = EC-ETOTAL
  1     CONTINUE
        IBEGIN = 1
        IEND = INTPTR(6)
        J = 0
        DO 10 I = IBEGIN,IEND
 11        IF (CPTR(I) .GT. J) THEN
              J = J + 1
              C = COEFF(J)*VALUE(I)
              IF (OPTR(J) .NE. 0) C = C*VALUE(OPTR(J))
              W(IH(J),JH(J)) = W(IH(J),JH(J)) + C
              GO TO 11
           END IF
 10     CONTINUE
C  ***** SYMMETRIZE THE MATRIX
C
        DO 12 I = 1,NCFG-1
           DO 13 J = I+1,NCFG
              W(I,J) = W(J,I)
 13        CONTINUE
 12     CONTINUE
        IF (TRACE) THEN
            WRITE(6,'(/10X,A,F16.8,10X,A,F16.8/)')
     :          'EC =',EC,'ETOTAL =',ETOTAL
            DO 15 I=1,ID
                WRITE(6,'(I4,6F12.7/(4X,6F12.7))')
     :                 I,(W(I,J),J=1,NCFG)
15         CONTINUE
        END IF
        IF (LAST) THEN
            WRITE(PRI,'(/10X,A,F16.8,10X,A,F16.8//10X,A/)')
     :          'EC =',EC,'ETOTAL =',ETOTAL,'Interaction matrix (H-E)'
            DO 16 I=1,ID
                WRITE(PRI,'(I4,6F12.7/(4X,6F12.7))')
     :                 I,(W(I,J),J=1,NCFG)
16         CONTINUE
        IF (ID .GE. 1) THEN
        V = D0
        DO 20 J = 1,ID
           V = WT(I)*W(I,NCFG)
20      CONTINUE
        V = V*V
        PI = ACOS(-D1)
        WIDTH = PI*V/D2
        A = 0.6494E+17*V
        TAU = D1/A
        WRITE (PRI,19) WIDTH,109737.*WIDTH,A,TAU
19      FORMAT(/10X,'Auto-ionization Data'/
     :         10X,'--------------------'//
     :         20X,' Line Width            ',1PE13.4,' Ryd'/
     :         20X,'                       ',1PE13.4,' cm-1'/
     :         20X,' Auto-ionization rate =',1PE13.4,' sec-1'/
     :         20X,' Half-Life            =',1PE13.4,' sec'//)
        END IF
        END IF
*
14    IF (NCFG .EQ. 1) GO TO 37
C
C  *****  SOLVE SYSTEM OF FIRST ID EQUATIONS
C
54     CONTINUE
       NMID = ID - 1
      DO 21 I=1,NMID
      IP =I+1
      DO 21 J=IP,ID
      RATIO = W(J,I)/W(I,I)
      DO 23 K=IP,NCFG
23    W(J,K) = W(J,K) - RATIO*W(I,K)
21    CONTINUE
      DO 24 J = ID,1,-1
      JP = J+1
      WT(J) = D0
      DO 25 K = JP,NCFG
25    WT(J) = WT(J) - W(J,K)*WT(K)
24    WT(J) = WT(J)/W(J,J)
      DO 28 I = 1,ID
      WT(I) = WT(I) + ACFG*(WP(I) - WT(I))
28    CONTINUE
37    WRITE(6,636) (I,WT(I),I=1,ID)
636   FORMAT(/(3X,5(I4,F11.7)))
C
C  *****  REDEFINE SUM(I)
C
        IBEGIN = INTPTR(5)+1
        IEND = INTPTR(6)
        DO 50 I = IBEGIN,IEND
           IF (IEL(I,1).EQ.IEL(I,2)) SUM(IEL(I,1)) = -2*COEF(I)
 50     CONTINUE
C
70    IF (.NOT. LAST .OR. OUC .EQ. 0 ) RETURN
C
C  *****  PUNCH CONFIGURATIONS AND WEIGHTS ON UNIT OUC
C
C
      WRITE(3,'(//A/)')  '     Final Mixing'
      WRITE(OUC,46) ATOM,TERM,ETOTAL
46    FORMAT(3X,2A6,F14.7)
      WRITE(OUC,'(18(1X,A3))') (EL(J),J=1,NCLOSD)
      DO 47 J = 1,NCFG
      WRITE(3,648) J,CONFIG(J),WT(J),(COUPLE(J,JJ),JJ=1,9)
47    WRITE(OUC,48) CONFIG(J),WT(J),(COUPLE(J,JJ),JJ=1,9)
48    FORMAT(A40,F10.7/9(5X,A3))
648   FORMAT(I4,2X,A40,F10.7/(6X,9(5X,A3)))
      WRITE (OUC,'(A)') '*****'
      END
