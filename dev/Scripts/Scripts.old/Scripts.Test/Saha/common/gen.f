*     -----------------------------------------------------------------

	PROGRAM GENCI
* 
*                By C. Froese Fischer 
*		    Bin Liu
*                   Vanderbilt University 
*                   Nashville, TN 37235 USA 
* 
*                May, 1983 
* 
*     ------------------------------------------------------------ 
* 
*
*	This program computes all possible couplings for each member of
*   the reference set, generates all unique, possible configurations 

*   from the active set and their couplings, and for each replacement,
*   generate the configurations and their couplings, then prints out the
*   configurations and couplings that agree with the given final term.
*     Input : (Interactive)
*        i)   Header ;
*       ii)   List of closed shells ;
*      iii)   The "reference" set ;
*       iv)   The "active" set ;
*        v)   Replacements from the reference set ;
*	      Virtual Set if Replacement is 's' or 'd' or 'sd'
*       vi)   Final Term
*     Output :
*	i )   Header ;
*       ii)   List of closed shells ;
*      iii)   Configurations (FORMAT(5(1X,A3,'(',I2,')'))
*		and their couplings (FORMAT(9(5X,A3))


*   I/O allocation
*    FILE1(.)  :  Configurations by Active Set	(internal file)
*    FILE2(.)  :  Configurations by Replacement	(internal file)
*    FILE3(.)  :  couplings			(internal file)
*	    6  :  Terminal output
*	    7  :  File CI.LST
*  FBETA(1,.)  :  Information of Beta2		(internal file )
*  FBETA(2,.)  :  Information of Beta3		(internal file )
*  FBETA(3,.)  :  Information of Beta4		(internal file )
*  FBETA(4,.)  :  Information of Beta5		(internal file )
*
*
*
* ---------------------------------------------------------------------
*		M A I N		P R O G R A M
* ---------------------------------------------------------------------
*
	PARAMETER      (NSHEL=20,NCOUPL=2*NSHEL-1)
	CHARACTER*48   REF(20),REPL(20),FINAL(20)
	CHARACTER*48   STRL,STRR,DEL
	CHARACTER*72   HEADER,SHELLS,ACT,VIRTUL,TEMP
	CHARACTER*3    EL(NSHEL),ELL(NSHEL),ELR(NSHEL),ELS(NSHEL,20),
     :		       ELA(30)
  	CHARACTER      FBETA*8, FILE1*30, FILE2*50, FILE3*27
	CHARACTER*3    COUPLE(NCOUPL),CH3,ELB(NSHEL),ELC(NSHEL),ELV(30)
	CHARACTER      CH0,CH1,CH2*2,FTM(20)*2
	INTEGER        ORDLA,ORDLZ,ORDUA,ORDUZ,ORD0,ORD9
	INTEGER        Q(NSHEL),QL(NSHEL),QR(NSHEL),
     :		        QS(NSHEL,20),MS(NSHEL),RL(30),
     :		        Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :		        Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30
        INTEGER	       F(30),PL,PR,QA(30),QB(NSHEL),VL(30)
	INTEGER        LEFT,RIGHT,PARITY,CONST,SFLAG,DFLAG
	COMMON         NF,NR,NFTM,MAX,MIN,PARITY,CONST,NQ
     :		       /BLK0/ORDLA,ORDLZ,ORDUA,ORDUZ,ORD0,ORD9
     :		       /BLK1/HEADER,SHELLS,ACT,VIRTUL
     :		       /BLK2/REF,REPL,FTM
     :		       /BLK3/EL,ELL,ELR,ELS,ELA
     :		       /BLK4/Q,QL,ML,QR,MR,M,QS,MS,MA,RL,NREF,
     :		             Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :			     Q10,Q11,Q12,Q13,Q14,Q15,
     :		             Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			     Q25,Q26,Q27,Q28,Q29,Q30
		COMMON /FILES/FBETA(4,2000),FILE1(200),FILE2(200),
     :			      FILE3(2000)


***************
*	Declaration of variable for the input data
*
*	 HEADER  =  header of output
*	 SHELLS  =  Closed Shells
*	    REF  =  Reference set
*	    ACT  =  Active Set
*	   REPL  =  Replacement
*        VIRTUL  = Virtual Set
*           FTM  = Final Term (20)
*


***************
*	Obtain ASCII value for the bound of character and digit
*
	ORDLA = ICHAR('a')
	ORDLZ = ICHAR('z')
	ORDUA = ICHAR('A')
	ORDUZ = ICHAR('Z')
	ORD0 = ICHAR('0')
	ORD9 = ICHAR('9')



****************
*	Disply for the beginning of the program
*

010	PRINT *
	PRINT *
	PRINT *,'     -------------------------------------------------
     :---------'
	PRINT *
	PRINT *,'     You are under the program GENCI'
	PRINT *, '            which generates a configuration list'
	PRINT *
	PRINT *
	PRINT *,'     Type H (Help) to obtain information about the
     : input format '
	PRINT *
	PRINT *,'     Type <RETURN> if you already know '
	PRINT *
	PRINT *,'     -------------------------------------------------
     :---------'
	PRINT *

***************
*	If the user needs to obtain information about input format,
*    call subroutine HELP .
*
	READ '(A)', STRL
	I = INDEX(STRL,'H')
	J = INDEX(STRL,'h')
	IF (I .OR. J) CALL HELP





***********************************************************************
*	     READ AND ANALYSIS THE INPUT DATA    	
*
*
*		     Input Header  
*
100	PRINT 101
101	FORMAT(T13,'Header  ?  ' )
	READ '(A)', HEADER


***************
*	Input Closed Shell, check if it satisfies the FORMAT(18(1X,A3))
*
110	PRINT 113
113	FORMAT('     Closed Shells  ?  ' )
	READ '(A)', TEMP
	I = INDEX(TEMP,'B')
	J = INDEX(TEMP,'b')
	IF (I .OR. J) GOTO 100
	N = 2
	SHELLS = ' '
115	IF (INDEX(TEMP,'     ') .GT. 2) THEN
	    TEMP = DEL(TEMP)
	    J = ICHAR(TEMP(1:1))
	    IF (J.LT.ORD0 .OR. J.GT.ORD9) THEN
		PRINT *, '     INPUT ERROR !'
		GOTO 110
	    ENDIF
	    SHELLS(N:N+2) = TEMP(:3)
	    N = N+4
	    TEMP = TEMP(4:)
	    GO TO 115
	END IF


***************
*	   Input Reference Set, and check the input error
*
*	 LEFT  =  position of the first '(' in the string
*	RIGHT  =  position of the first ')' in the string
*	  SUM  =  total number of Q(i) for each configuration
*          NL  =  Total quantum number for each reference set
*       CONST  =  Total number of Qi for the first reference set
*      PARITY  =  Parity for the first reference set
*
*
120	PRINT 121
121	FORMAT('     Reference Set  ?  ' )
	CALL INPUT (NREF,REF,IFLAG,SFLAG,DFLAG,*110,*120)
	DO 122 I=1,NREF
	    SUM = 0
	    NL = 0
	    TEMP = REF(I)
123	    IF (TEMP(1:5) .NE. '     ') THEN
		TEMP = DEL(TEMP)
*
*	Error if the input has unmatched parenthesis
*
		LEFT = INDEX(TEMP,'(')
		RIGHT = INDEX(TEMP(:7),')')
		IF (LEFT.EQ.0 .OR. RIGHT.EQ.0) THEN
		    PRINT *,'       Unmatched parenthesis !  Please 
     : input again .'
		    GOTO 120
		ENDIF
*
*	Error if number of electron is more than FULL
*
		CH3 = TEMP(LEFT+1:RIGHT-1)
		N = ICTOI(CH3)
		L = LVAL(TEMP(2:2))
		IF (N .GT. L*4+2) THEN
		    PRINT *,'     Number of electrons in a shell is 
     :more than FULL !'
		    GOTO 120
		ENDIF
		SUM = SUM+N
		NL = NL+N*L
		TEMP = TEMP(RIGHT+1:)
	        GO TO 123
	    END IF

***************
*	The first Reference Set is defined as the standard value
*
	    IF (I .EQ. 1) THEN
		CONST = SUM
		PARITY = MOD(NL,2)
	    ELSE

*
*	Error if members of the reference set have different electron
*   numbers or parity
*
	        IF (SUM .NE. CONST) THEN
		    PRINT *,'     Total number of electrons is wrong !'
		    GOTO 120
	        ENDIF
	        IF (MOD(NL,2) .NE. PARITY) THEN
		    PRINT *,'       Parity is wrong !'
		    GOTO 120
	        ENDIF
	    ENDIF
122	CONTINUE



*******************
*		   Input Active Set 
*
130	PRINT 131
131	FORMAT('        Active Set  ?  ' )
	READ '(A)', ACT
	I = INDEX(ACT,'B')
	J = INDEX(ACT,'b')
	IF (I .OR. J) GOTO 120



********************
*		   Input Replacement 
*
140	PRINT 141
141	FORMAT('      Replacements  ?  ')
	IFLAG = 1
	CALL INPUT (NREPL,REPL,IFLAG,SFLAG,DFLAG,*130,*150)

********************
*	If Replacement = S or D or SD, input Virtual Set
*
*      ELV  =  Electron label for virtual set
*       MV  =  Number of electron for Virtual Set
*       VL  =  Parity for each Qi in Virtual Set
*
142	IF (SFLAG.EQ.0 .AND. DFLAG.EQ.0) GOTO 150

	PRINT 143
143	FORMAT ('       Virtual Set  ?  ')
	READ '(A)', VIRTUL
	TEMP = VIRTUL
	J = INDEX(TEMP,'     ')
	TEMP(J:J) = ','
	MV = 0

*
*	Decompose the input of Virtual Set
*
144	IF (TEMP(:5) .NE. '     ') THEN
	    MV = MV+1
	    IF (MV .GT. (30)) STOP ' Virtual set too large: MAX=(30)'
	    TEMP = DEL(TEMP)
	    J = INDEX(TEMP,',')
	    VL(MV) = MOD(LVAL(TEMP(2:2)),2)

*
*	Convert the input of uppercase to lowercase and assign value 
*    ELVi
*
	    N = ICHAR(TEMP(2:2))
	    IF (N.GE.ORDUA .AND. N.LE.ORDUZ) TEMP(2:2) = CHAR(N-ORDUA+ORDLA)
	    ELV(MV) = TEMP(:J-1)
	    TEMP = TEMP(J+1:)
	    GO TO 144
        END IF

145	PRINT 146
146	FORMAT ('  From which shell  ?  ')
	READ '(A)', TEMP
	TEMP = DEL(TEMP)
	NVIR = ICHAR(TEMP(1:1))-ORD0
	IF (NVIR.LT.1 .OR. NVIR.GT.9) THEN
	    PRINT *,'    Please input digit for the shell position !'
	    GOTO 145
	ENDIF

147	PRINT 148
148	FORMAT ('    To which shell  ?  ')
	READ '(A)', TEMP
	TEMP = DEL(TEMP)
	LVIR = ICHAR(TEMP(:1))-ORD0
	IF (LVIR.LT.0 .OR. LVIR.GT.9) THEN
	    PRINT *,'     Please input digit for the shell position !'
	    GOTO 147
	ENDIF


*******************
*		     Input Final Terms
*
*      NFTM  =  Number of input for final term
*
150	PRINT 151
151	FORMAT ('       Final Terms  ?  ')
	CALL INPUT (NFTM,FINAL,IFLAG,SFLAG,DFLAG,*140,*160)
	DO 152 I=1,NFTM
	    CH2 = FINAL(I)(:2)

*
*	Conver the input of lowercase to uppercase
*
	    N = ICHAR(CH2(2:2))
	    IF (N.GE.ORDLA .AND. N.LE.ORDLZ) CH2(2:2) = CHAR(N-ORDLA+ORDUA)
	    FTM(I) = CH2
152	CONTINUE





***********************************************************************
*	Open the file for CI.LST
*
160	OPEN (7,FILE='CI.LST',STATUS='NEW',RECL=80)    



***********************************************************************
*	     PRINT OUT ALL INPUT DATA OF THE USER
*	
	WRITE (6,170)
170	FORMAT ('1'//////////T15,'***************          I N P U T   
     : D A T A          **********'///)
	WRITE (6,171) HEADER
171	FORMAT (T15,'          Header  :  ',A48/)
	WRITE (6,172) SHELLS
172	FORMAT (T15,'    Closed shell  :  ',A48/)
	WRITE (6,173) REF(1)
173	FORMAT (T15,'   Reference Set  :  ',A48/)
174	FORMAT (T29,I2,'  :  ',A48/)
	DO 175 I=2,NREF
	    WRITE (6,174) I,REF(I)
175	CONTINUE
	WRITE (6,176) ACT
176	FORMAT (T15,'      Active Set  :  ',A48/)
	WRITE (6,178) REPL(1)
178	FORMAT (T15,'    Replacements  :  ',A48/)
	IF (SFLAG .OR. DFLAG) THEN
	    WRITE (6,179) VIRTUL
179	    FORMAT (T15,'     Virtual Set  :  ',A72/)
	    WRITE (6,*)'             From which shell  :  ',
     : CHAR(NVIR+ORD0)
	    WRITE (6,*)
	    WRITE (6,*)'               To which shell  :  ',
     : CHAR(LVIR+ORD0)
	    WRITE (6,*)
	ENDIF
	DO 180 I=2,NREPL
	    WRITE (6,174) I,REPL(I)
180	CONTINUE
	WRITE (6,181) FINAL(1)
181	FORMAT (T15,'     Final Terms  :  ',A48/)
	DO 182 I=2,NFTM
	    WRITE (6,174) I,FINAL(I)
182	CONTINUE

	WRITE (7,183) HEADER
183	FORMAT (' ',A48)
	WRITE (7,*) SHELLS



***********************************************************************
*		     PROCESS THE REFERENCE SET  		  
*
*       M  =  Maximin number of ELi for reference set
*     ELS  =  Storage of ELi for different Reference Set
*      QS  =  Storage of Qi for each Reference Set
*      MS  =  Storage of M for each Reference Set
*    STRL  =  Temporary string
*
*
200	DO 201 NF=1,NREF
	    M = 0
	    TEMP = REF(NF)
	    DO 207 I=1,NSHEL

***************
*	Decompose the input for Reference Set
*
		IF (TEMP(:5) .EQ. '     ') THEN
		    Q(I) = 0
		    GOTO 204
		ENDIF
		TEMP = DEL(TEMP)
		LEFT = INDEX(TEMP,'(')
		RIGHT = INDEX(TEMP,')')
		M = M+1
	        IF (M.GT.NSHEL) THEN
		    PRINT *, ' Too many shells in reference set: MAX=',
     :				NSHEL
		    STOP
		END IF

***************
*	Convert the input of uppercase to lowercase, and assign initial
*   values for ELi and Qi
*
		CH3 = TEMP(:LEFT-1)
		N = ICHAR(CH3(2:2))
		IF (N.GE.ORDUA .AND. N.LE.ORDUZ) CH3(2:2) = CHAR(N-ORDUA+ORDLA)
		EL(M) = CH3
		ELS(M,NF) = CH3
		CH3 = TEMP(LEFT+1:RIGHT-1)
		Q(M) = ICTOI(CH3)
		QS(M,NF) = Q(M)
		TEMP = TEMP(RIGHT+1:)
207	    CONTINUE
204	    MS(NF) = M
	    DO 212 J=M+1,NSHEL
		EL(J) = '   '
		Q(J) = 0
212	    CONTINUE

***************
*	Unify the second line of the reference set
*
	    IF (NREF .GE. 2) THEN
		N = MS(1)
		DO 208 I=1,N
		    IF (EL(I)(1:2) .EQ. ELS(I,1)(1:2)) GOTO 208
		    DO 210 J=N-1,I,-1
			EL(J+1) = EL(J)
			Q(J+1) = Q(J)
210		    CONTINUE
		    EL(I) = ELS(I,1)
		    Q(I) = 0
208	        CONTINUE
		DO 209 I=1,N
		    ELS(I,NF) = EL(I)
		    QS(I,NF) = Q(I)
209		CONTINUE
		MS(NF) = N
		M = N
	    ENDIF

****************
*	   Generate all couplings for the reference set
*	   MAX = -5  means the first time to call subroutine COUPLD
*
	    K = 0
	    DO 206 I=1,M
		IF (Q(I) .NE. 0) THEN
		    K = K+1
		    ELB(K) = EL(I)
		    QB(K) = Q(I)
		ENDIF
206	    CONTINUE

	    MAX = -5
	    CALL COUPLD (ELB,QB,K,ELC,NC,*500)
	    IF (NC .NE. 0) THEN
	        WRITE (6,202)
202	        FORMAT ('1     GENERATE ALL COUPLINGS FOR EACH MEMBER
     : OF THE REFERENCE SET'//)
	        CALL PRINT (ELC,QB,K,NC,1)
	    ENDIF

***************
*	     Compute MAX and MIN value for the finals .
*	     Rule :  MAX = 2*|S+L| ,  MIN = 2*|S-L|  
*
	    IF (NF .EQ. 1) THEN
		MAX = 0
		MIN = 100
		DO 215 I=1,NC
		    N = 2*K-1
		    READ (FILE3(I),205) (COUPLE(J),J=1,N)
205		    FORMAT (9(A3))
		    CH3 = COUPLE(N)(1:1)
		    CH1 = COUPLE(N)(2:2)
		    S = (ICTOI(CH3)-1)/2.
		    L = LVAL(CH1)
		    LMIN = 2*ABS(S-L)
		    LMAX = 2*ABS(S+L)
		    IF (LMIN .LT. MIN) MIN = LMIN
		    IF (LMAX .GT. MAX) MAX = LMAX
215		CONTINUE
	    ENDIF
201	CONTINUE
	IF (ACT(:5) .EQ. '     ') GOTO 400



***********************************************************************
*		     PROCESS THE ACTIVE SET    		
*
*	The member of the input is limited to 15 .
*
*      NQ  =  Number of configurations
*     ELA  =  ELi for Active Set
*      QA  =  Qi for Active Set
*      MA  =  Number of ELi for Active Set
*      RL  =  L-value for each shell
*       F  =  Full value for each shell
*


	MA = 0
300	TEMP = ACT

***************
*	Decompose the input of Active Set, Conver the input of uppercase
*    to lowercase, assign values to ELA,QA,RL,F .
*
301	IF (MA .LT. 30) THEN
	    IF (TEMP(1:5) .EQ. '     ') GOTO 302
	    TEMP = DEL(TEMP)
	    MA = MA+1

	    N = ICHAR(TEMP(2:2))
	    IF (N.GE.ORDUA .AND. N.LE.ORDUZ) TEMP(2:2) = CHAR(N-ORDUA+ORDLA)

	    ELA(MA) = TEMP(:2)
	    RL(MA) = LVAL(TEMP(2:2))
	    F(MA) = 4*RL(MA)+2
	    TEMP = TEMP(4:)
	    GO TO 301
	END IF

****************
*	30 is maximun value of active electrons
*
302	DO 303 I=MA+1,30
	    F(I) = 0
303	CONTINUE


***************
*	Generate other possible configurations from the given set 
*
	DO 310 Q30=0,F(30)
	  DO 310 Q29=0,F(29)
	    DO 310 Q28=0,F(28)
	      DO 310 Q27=0,F(27)
		DO 310 Q26=0,F(26)
		  DO 310 Q25=0,F(25)
		    DO 310 Q24=0,F(24)
		      DO 310 Q23=0,F(23)
			DO 310 Q22=0,F(22)
			  DO 310 Q21=0,F(21)
			    DO 310 Q20=0,F(20)
			      DO 310 Q19=0,F(19)
				DO 310 Q18=0,F(18)
				  DO 310 Q17=0,F(17)
				    DO 310 Q16=0,F(16)
	DO 310 Q15=0,F(15)
	  DO 310 Q14=0,F(14)
	    DO 310 Q13=0,F(13)
	      DO 310 Q12=0,F(12)
		DO 310 Q11=0,F(11)
		  DO 310 Q10=0,F(10)
		    DO 310 Q9=0,F(9)
		      DO 310 Q8=0,F(8)
			DO 310 Q7=0,F(7)
			  DO 310 Q6=0,F(6)
			    DO 310 Q5=0,F(5)
			      DO 310 Q4=0,F(4)
				DO 310 Q3=0,F(3)
				  DO 310 Q2=0,F(2)
				    DO 310 Q1=0,F(1)
			      		CALL CONFIG 
310	CONTINUE

***************
*		  Print Header of the output file 
*
340	IF (NQ .EQ. 0) GOTO 400
	WRITE (6,341)
341	FORMAT ('1     GENERATE ALL POSSIBLE CONFIGURATIONS FROM THE
     : ACTIVE SET'//)
	J = INDEX(HEADER,'         ')
	WRITE (6,321) HEADER(1:J)
321	FORMAT (' '/T10,'-------------       ',A,'  ---
     :-----'/)
	WRITE (6,322) SHELAS
322	FORMAT ('          Closed Shells  :  ',A48/)
	WRITE (6,323) ACT
323	FORMAT ('             Active Set  :  ',A48/)

***************
*	Print all configurations generated from Active Set
*
	WRITE (6,*) '         Configurations  :'
	DO 325 I=1,NQ
	    READ (FILE1(I),326) (QA(J), J=1,MA)
326	    FORMAT (30(I2))
	    IF (ELA(1)(1:1) .EQ. ' ') THEN
	        N = 27
	    ELSE
	        N = 28
	    ENDIF

	    K = 0
	    DO 370 J=1,MA
		IF (QA(J).NE.0 .OR. MA.EQ.2) THEN
		    K = K+1
		    ELB(K) = ELA(J)
		    QB(K) = QA(J)
		ENDIF
370	    CONTINUE
	    WRITE (6,327) (ELB(J),QB(J), J=1,K)
327	    FORMAT (T28,5(1X,A3,'(',I2,')'))
325	CONTINUE

****************
*	For each new configuration, generate all possible couplings
*
	N1 = 0
	N2 = 0
	DO 328 I=1,NQ
	    READ (FILE1(I),326) (QA(J), J=1,MA)

***************
*		Omit ELA(i) if corresponding QA(i)=0
*
	    K = 0
	    DO 350 J=1,MA
		IF (QA(J).NE.0 .OR. MA.EQ.2) THEN
		    K = K+1
		    ELB(K) = ELA(J)
		    QB(K) = QA(J)
		ENDIF
350	    CONTINUE

****************
*	Omit configurations which have more than 5 shells
*
	    IF (K .LE. 5) THEN
	        CALL COUPLD (ELB,QB,K,ELC,NC,*500)
	        IF (NC .GT. 0) THEN
	            WRITE (6,343)
343	            FORMAT ('1   GENERATE THEIR COUPLINGS FOR EACH 
     :CONFIGURATION GENERATED BY THE ACTIVE SET'//)
	            CALL PRINT (ELC,QB,K,NC,2)
		ELSE
		    N2 = N2+1
	 	ENDIF
	    ELSE
		N1 = N1+1
	    ENDIF
328	CONTINUE
	IF (N1) PRINT 344,N1
344	FORMAT (T5,'Too many occuplied shells --- ',I3,
     : ' configuration omitted !')
	IF (N2) PRINT 345, N2
345	FORMAT (T5,'No final term as your selection for ',I3,
     :' Active set !')
349	IF (NREPL .EQ. 0) GOTO 500



***********************************************************************
*		     PROCESS THE REPACEMENTS   		
*
*	STRL  :  String of the left side of '='
*       STRR  =  String of the right side of '='
*

400	IF (SFLAG .OR. DFLAG) GOTO 450
	N1 = 0
	DO 401 NR=1,NREPL
	    STRR = REPL(NR)
	    J = INDEX(STRR,'=')
	    STRL = STRR(1:J-1)
	    STRR = STRR(J+1:)

***************
*		Decompose the substring on the left of ''=''
*
*     ELL  =  Old value of ELi to be replaced
*      QL  =  Old value of Qi to be replaced
*      ML  =  Number of Old value of ELi to be replaced
*
	    STRL = DEL(STRL)
	    CALL DECOMP (STRL,ELL,QL,ML)

***************
*		Decompose the substring on the right of ''=''
*
*     ELR  =  New value of ELi
*      QR  =  New vaRue of Qi
*      MR  =  Number of the new value of ELi
*
	    STRR = DEL(STRR)
	    CALL DECOMP (STRR,ELR,QR,MR)

***************
*	     For each Replacement, replace all Reference Sets
*
	    DO 401 NF=1,NREF
		M = MS(NF)
		IF (M .LT. ML) GOTO 401
	        DO 402 I=1,M
		    EL(I) = ELS(I,NF)
		    Q(I) = QS(I,NF)
402		CONTINUE
		CALL REPLAC(ELA,QA,K,*401)
		CALL COUPLD (ELA,QA,K,ELC,NC,*500)
		IF (NC .GT. 0) THEN
		    WRITE (6,403)
403		    FORMAT ('1     FOR EACH REPLACEMENT, GENERATE
     : CONFIGURATIONS AND COUPLINGS FOR THE REFERENCE SET'//)
		    CALL PRINT (ELC,QA,K,NC,3)
		ELSE
		    N1 = N1+1
		ENDIF
401	CONTINUE
	IF (N1) PRINT 405, N1
405	FORMAT (T5,'No Final Term as your selection for ',I3,
     :' Replacements omitted !')
	GOTO 500



***********************************************************************
*		PROCESS THE VIRTUAL SET
*
*     RL  =  Parity of ELi of Reference Set
*     VL  =  Parity of ELi of Virtual Set
*
*
450	DO 451 NF=1,NREF

***************
*	Set the initial values of Reference Set
*
	    M = MS(NF)
	    DO 452 I=NVIR,NSHEL
		CH3 = ELS(I,NF)
		EL(I) = CH3
		ELA(I) = CH3
		Q(I) = QS(I,NF)
		QA(I) = Q(I)
		RL(I) = MOD(LVAL(CH3(2:2)),2)
452	    CONTINUE

	    IF (SFLAG.EQ.0 .AND. DFLAG) GOTO 460

***************
*	Preparation for the Single Replacement
*
	    QL(1) = 1
	    ML = 1
	    QR(1) = 1
	    MR = 1
	    DO 454 I=NVIR,LVIR
		IF (Q(I).EQ.0) GOTO 454
		ELL(1) = EL(I)
		IF (RL(I)) THEN
		    N = 1
		ELSE
		    N = 0
		ENDIF
		DO 454 J=1,MV

***************
*	Replace Qi of Reference Set which has the same parity with Qj 
*    of Virtual Set
*
		    IF (VL(J) .EQ. N) THEN
			ELR(1) = ELV(J)
			CALL REPLAC(ELA,QA,K,*454)
			CALL COUPLD (ELA,QA,K,ELC,NC,*500)
			IF (NC .GT. 0) THEN
			    WRITE (6,453)
453			    FORMAT ('1       FOR VIRTUAL SET, GENERATE
     : CONFIGURATION AND COUPLINGS FOR S-REPLACEMENT'//)
			    NR = 20
			    TEMP = ELL(1)//' = '//ELR(1)
			    REPL(NR) = DEL(TEMP)
			    CALL PRINT (ELC,QA,K,NC,4)
			ENDIF
		    ENDIF
454	    CONTINUE


**************
*	Rreplace pairs of Q(i) and Q(j) by Double Virtual Set
*
*     PL  =  Pairty for the pair Qi and Qj in Reference Set
*
460	    IF (DFLAG .EQ. 0) GOTO 500
	    ML = 2
	    QL(1) = 1
	    QL(2) = 1
	    DO 461 I=NVIR,LVIR-1
		ELL(1) = EL(I)
		DO 461 J=I+1,M
		    IF (Q(I).EQ.0 .OR. Q(J).EQ.0) GOTO 461
		    ELL(2) = EL(J)
		    TEMP = ELL(1)//'.'//ELL(2)//' = '
		    PL = MOD(LVAL(ELL(1)(2:2))+LVAL(ELL(2)(2:2)), 2)
		    CALL VPAIR (ELV,MV,PL,TEMP,*500)
461	    CONTINUE


***************
*	Replace pairs of (Qi)=2 by Double Virtual Set
*
	    ML = 1
	    QL(1) = 2
	    DO 464 I=NVIR,LVIR
		IF (Q(I) .GT. 1) THEN
		    ELL(1) = EL(I)
		    TEMP = ELL(1)//'(2) = '
		    PL = MOD(LVAL(ELL(1)(2:2))*2, 2)
		    CALL VPAIR (ELV,MV,PL,TEMP,*500)
		ENDIF
464	    CONTINUE
451	CONTINUE




***********************************************************************
*		     THE END OF THE PROGRAM    
*
500	CLOSE (1)
	CLOSE (2)
	CLOSE (3)
	CLOSE (7)
	CLOSE (12)
	CLOSE (13)
	CLOSE (14)
	CLOSE (15)
	PRINT *
	PRINT *,'          OK !'
 	PRINT *,'          List of configurations and couplings is in
     : the file CI.LST .'
	END



*
* ----------------------------------------------------------------------
*		FUNCTION	D E L
* ----------------------------------------------------------------------
*
*   	Delete the leading space of the string 
*
	CHARACTER*(*) FUNCTION DEL(STR)
	    CHARACTER*(*)   STR

10	    IF (STR(1:1) .EQ. ' ') THEN
		STR = STR(2:)
	 	GO TO 10
	END IF
	    DEL = STR
	    RETURN
	END



*
* ----------------------------------------------------------------------
*		FUNCTION	I C T O I
* ----------------------------------------------------------------------
*
*	Convert character string into its corresponding integer   
*
	INTEGER FUNCTION ICTOI(STR)
	    CHARACTER*(*)   STR

	    N = ICHAR(STR(1:1))-ICHAR('0')
	    IF (STR(2:2) .NE. ' ') N = N*10+ICHAR(STR(2:2))-ICHAR('0')
	    ICTOI = N
	    RETURN
	END



*
* ----------------------------------------------------------------------
*		FUNCTION	L V A L
* ----------------------------------------------------------------------
*
*	convert the symbol into its corresponding quantum number
*
	INTEGER FUNCTION LVAL(SYMBOL)
	    CHARACTER      SYMBOL
	    CHARACTER*22   SET
            DATA SET/'spdfghiklmnSPDFGHIKLMN'/

	    LOCATE = INDEX(SET,SYMBOL)
	    IF (LOCATE .LE. 11) THEN
		LVAL = LOCATE-1
	    ELSE
		LVAL = LOCATE-12
	    ENDIF
	    RETURN
	END



*
* ----------------------------------------------------------------------
*		FUNCTION	S Y M B
* ----------------------------------------------------------------------
*
*	Convert the quantum number into its corresponding symbol 
*
	CHARACTER FUNCTION SYMB(L)
	    CHARACTER*11   SET
            DATA SET/'SPDFGHIKLMN'/

	    SYMB = SET(L+1:L+1)
	    RETURN
	END



*
* ----------------------------------------------------------------------
*		SUBROUTINE	I N P U T
* ----------------------------------------------------------------------
*
*	Process the input set and check the input error
*
	SUBROUTINE INPUT (NSET,SET,MARK,SFLAG,DFLAG,*,*)
	    CHARACTER*48   SET(20),TEMP,DEL,CH1
	    CHARACTER*72   HEADER,SHELLS,ACT,VIRTUL
	    INTEGER        SFLAG,DFLAG,MARK
	    COMMON         /BLK1/HEADER,SHELLS,ACT,VIRTUL

*     NSET  =  Number of members in the set
*      SET  =  Character array with NSET elements
*       *1  =  Return label if input is 'B'
*       *2  =  Return label if the set is empty
*     MARK  =  1 if input is Replacement, 0 otherwise

	    NSET = 0
12	    IF (NSET .LT. 30) THEN
		READ '(A48)', TEMP

***************
* 	If input is 'B' or 'b', go back one step
*
		I = INDEX(TEMP,'B')
  		J = INDEX(TEMP,'b')
		IF (I .OR. J) THEN
		    IF (NSET .EQ. 0) THEN
			RETURN 1
		    ELSE
			NSET = NSET-1
			GOTO 13
		    ENDIF
		ENDIF

***************
*	Go to for next input if the input is empty
*   	Return if the input is finished
*
		IF (TEMP(1:5) .EQ. '     ') THEN
		    IF (NSET .EQ. 0) THEN
			RETURN 2
		    ELSE
			RETURN
		    ENDIF
	    	ENDIF
		TEMP = DEL(TEMP)

***************
*	If Replacement is 's' or 'd' or 'sd', set single and
*    double flag for Virtual Set
*
		IF (MARK) THEN
		    CH1 = TEMP(:1)
		    IF (CH1.EQ.'S' .OR. CH1.EQ.'s') SFLAG = 1
		    I = INDEX(TEMP,'SD')
		    J = INDEX(TEMP,'sd')
		    IF (CH1.EQ.'D' .OR. CH1.EQ.'d' .OR. I .OR. J)
     :		       DFLAG = 1
		    IF (SFLAG .OR. DFLAG) THEN
		        NSET = 1
		        SET(1) = TEMP
		        RETURN
		    ENDIF
		ENDIF

***************
*	READ the input and delete the repeated member
*
		MARK = 4
		DO 14 I=1,NSET
		    IF (SET(I) .EQ. TEMP) THEN
			PRINT *,'     You give a repeated input !'
			GOTO 13
		    ENDIF
14		CONTINUE
		NSET = NSET+1
		SET(NSET) = TEMP
13	        PRINT 16, NSET+1,'  ?  '
16		FORMAT(T7,I10,A)
		GO TO 12
	    END IF
	    RETURN
	END



*
* ---------------------------------------------------------------------
*  		SUBROUTINE	C O U P L D
* ---------------------------------------------------------------------
*
*          This subroutine generates all possible couplings .
*	First, compute Alpha value from the given configuration,
*	then compute Beta from each value of Alpha .		

	SUBROUTINE COUPLD (EL,Q,M,ELC,NC,*)

*   INPUT :
*	El  =  electron label 
*	       where EL(I)(1=1) ---  blank
*		     EL(I)(2=2)  ---  n-symble
*		     EL(I)(3=3)  ---  L-symble
*	Q  =  Occupation number
*		0 (empty) <= Q(i)  <= 2(2Li+1) (full)
*	M  =  number of configuration
*		0  <=  M  <=  5
*   OUTPUT :
*	NC  =  number of couplings
*	 *  =  Return label if the maximun number of couplings > 500
*
	    PARAMETER      (NSHEL=20,NCOUPL=2*NSHEL-1)
	    CHARACTER*48   REF(20),REPL(20)
	    CHARACTER*65   TERM(17)
	    CHARACTER*3    EL(NSHEL),ELC(NSHEL),ALFA(NSHEL,50)
	    CHARACTER*3    COUPLE(NCOUPL,650),CH3
	    CHARACTER*2    CH2,A2,FTM(20)
	    CHARACTER      CH1,CALFA*15,SYMB,B1*2
  	CHARACTER      FBETA*8, FILE1*30, FILE2*50, FILE3*27
	    INTEGER        ORDLA,ORDLZ,ORDUA,ORDUZ,ORD0,ORD9
	    INTEGER        Q(NSHEL),NTERM(17),POSIT(NSHEL),FULLALF
	    INTEGER        PTR,PARENT,CHILD,BETA(NSHEL),PARITY,CONST
	    COMMON         NF,NR,NFTM,MAX,MIN,PARITY,CONST,NQ
     :			   /BLK0/ORDLA,ORDLZ,ORDUA,ORDUZ,ORD0,ORD9
     :			   /BLK2/REF,REPL,FTM
		COMMON /FILES/FBETA(4,2000),FILE1(200),FILE2(200),
     :			      FILE3(2000)

***************
*	    Number of possible terms for configurations P(1-3),
*	    D(1-5), F(1-2), G(1-2)
*
	DATA  (NTERM(I), I=1,17) / 1,3,3,1,5,8,16,16,1,7,0,0,0,0,0,1,9 /

***************
*	 Possible terms for configurations P(1-3),D(1-5),F(1-2),G(1-2)
*
	DATA  (TERM(I),I=1,17) /
     :  '2P1',
     :  '1S0 1D2 3P2',
     :  '2P1 2D3 4S3',
     :  '2D1',
     :  '1S0 1D2 1G2 3P2 3F2',
     :  '2D1 2P3 2D3 2F3 2G3 2H3 4P3 4F3',
     :  '1S0 1D2 1G2 3P2 3F2 1S4 1D4 1F4 1G4 1I4 3P4 3D4 3F4 3G4 3H4
     :   5D4',
     :  '2D1 2P3 2D3 2F3 2G3 2H3 4P3 4F3 2S5 2D5 2F5 2G5 2I5 4D5 4G5
     :   6S5',
     :  '2F1',
     :  '1S0 1D2 1G2 1I2 3P2 3F2 3H2',
     :  '   ',
     :  '   ',
     :  '   ',
     :  '   ',
     :  '   ',
     :  '2G1',
     :  '1S0 1D2 1G2 1I2 1L2 3P2 3F2 3H2 3K2' /



***********************************************************************
*		COMPUTE THE POSSIBLE VALUES FOR ALPHA
*
*   EMPTY  =  1 when Q(i)=0
*    FULL  =   2*(2L+1)
*    HALF  =  2L+1
*    ALFA  =  matrix of (5,50)
*   NALFA  =  Number of ALFA
*   POSIT  =  array of 5, store the position in table NTERM and TERM
*	      corresponding Q(i). Rule  :
*	      position = (L-1)*2+Q(i)        if 1 <= Qi <= HALF
*	      position = (L-1)*2+(FULL-Qi)   if Qi > HALF
*

	    NALFA = 1
	    DO 10 I=1,M
		CH3 = EL(I)
		CH1 = CH3(2:2)
		IF (CH3(3:3) .EQ. ' ') CH3 = ' '//CH3
		ELC(I) = CH3
	        CH1 = CHAR(ICHAR(CH1)-ORDLA+ORDUA)
	        L = LVAL(CH1)
	        FULL = 4*L+2
	        K = Q(I)

***************
*		If shell is full, ALFA(i) = 1S0
*
	        IF (K.EQ.0 .OR. K.EQ.FULL) THEN
	    	    CH3 = '1S0'

***************
*		If Q(i) = 1,  then ALFA(i)=2<L-symbol>1
*
	        ELSE IF (K .EQ. 1) THEN
	    	    CH3 = '2'//CH1//'1'

***************
*	Otherwise, get the possible value from array NTERM and TERM
*
	        ELSE
		    HALF = FULL/2
		    POSIT(I) = 0
		    DO 14 J=1,L
			POSIT(I) = POSIT(I)+(J-1)*2+1
14		    CONTINUE
	    	    IF (K .LE. HALF) THEN
	    	        POSIT(I) = POSIT(I)+K-1
	    	    ELSE
	    	        POSIT(I) = POSIT(I)+(FULL-K)-1
	    	    ENDIF
		    NALFA = NALFA*NTERM(POSIT(I))
	    	    CH3 = '   '
	        ENDIF

***************
*	CALFA is a string storing the elements in one ALFA
*
	        CALFA(3*I-2:3*I) = CH3
10	    CONTINUE

***************
*	     Assign values to all elements of ALFA  
*
*	NT  =  
*     LOCT  =  Current position in the table TERM
*     LOCA  =  Current position in the matrix ALFA
*
	    NT = 1
	    DO 12 I=M,1,-1
		CH3 = CALFA(I*3-2:I*3)
		IF (CH3 .NE. '   ') THEN
		    DO 11 J=1,NALFA
			ALFA(I,J) = CH3
11		    CONTINUE
		ELSE
		    LOCT = POSIT(I)
		    N = NTERM(LOCT)
		    LOCA = 1
15		    IF (LOCA .LE. NALFA) THEN
    		        DO 13 J=1,N
    			    CH3 = TERM(LOCT)(J*4-3:J*4-1)
			    DO 13 K=1,NT
	    	    	        ALFA(I,LOCA) = CH3
	    	    	        LOCA = LOCA+1
13	    	        CONTINUE
		        GO TO 15
		    END IF
		    NT = NT*N
	        ENDIF
12	    CONTINUE



***********************************************************************
*	GENERATE POSSIBLE VALUE OF BETA FROM ALFA
*

	    NC = 0
	    DO 20 NB=1,NALFA

***************
*	There is only one coupling if M = 1
*
		IF (M .EQ. 1) THEN
		    BETA(1) = 1
		    COUPLE(1,1) = ALFA(1,NB)
		    NBETA = 1
		    GOTO 37
		ENDIF

***************
*	Define BETA(1)=ALFA(1), then the next basic coupling steps is :
*	S1 = (BETA(1)(1:1)-1)/2 ,   S2 = (ALFA(2)(1:1)-1)/2 ;
*		| S1-S2 | <= BETA(2)(1:1) <= | S1+S2 |
*	L1 = L-number of BETA(1)(2:2),   L2 = L-number of ALFA(2)(2:2) ;
*	Symbole(| L1-L2 |) <= BETA(2)(2:2) <= Symbol(| L1+L2 |) .
*
		B1 = ALFA(1,NB)(1:2)
	        DO 21 J=2,M
	    	    A2 = ALFA(J,NB)(1:2)
		    PARENT = 1
		    CHILD = 1
		    BETA(J) = 0
26        	    S1 = (ICHAR(B1(1:1))-ORD0-1)/2.
        	    S2 = (ICHAR(A2(1:1))-ORD0-1)/2.
        	    S3 = ABS(S1-S2)
        	    S4 = ABS(S1+S2)
        	    L1 = LVAL(B1(2:2))
        	    L2 = LVAL(A2(2:2))
        	    L3 = ABS(L1-L2)
        	    L4 = ABS(L1+L2)
		    MBETA = (S4-S3+1)*(L4-L3+1)

**************
*	Generate Beta from each alpha .
*	There are four scratch files for storing the information about
*   Beta(i), 1<i<6, shown as follows :
*	    --------------------------------------------
*	    |    Parent    |    Value    |    Child    |
*	    --------------------------------------------
*	Define   Beta(i) is child of Beta(i-1) and parent of beta(i+1) ;
*   Parent is a pointer to the parent of Beta(i), that is Beta(i-1) ;
*   Value is one of the possible value for BETA(i), and Child is the 
*   number of children for Beta(i) .
*
*	MBETA  =  Number of couplings generated from ALFA(i)
*     PARENT  =  Current pointer to the parent of Beta(i)
*
        	    DO 22 S=S3,S4
			KS = 2*S
			CH1 = CHAR(ORD0+1+KS)
        	        DO 22 L=L3,L4
			    WRITE (FBETA(J-1,CHILD),25)
     :				 PARENT,CH1//SYMB(L),1
25			    FORMAT(I3,A2,I3)
			    CHILD = CHILD+1
22		    CONTINUE
50		    BETA(J) = BETA(J)+MBETA
		    IF (J .EQ. 2) GOTO 23

***************
*	Correct the number of its children for Beta(j-1)
*
		    LOCB = PARENT
		    DO 24 K=J-1,2,-1
			READ (FBETA(K-1,LOCB),25) PTR,CH2,N
			WRITE (FBETA(K-1,LOCB),25) PTR,CH2,N+MBETA-1
			LOCB = PTR
24		    CONTINUE

***************
*	If pointer to the parent is the end of file for Beta(j-1),
*    prepare to generate Beta(i+1) ; otherwise, generate next Beta(j)
*    according Alfa(j-1) and Beta(j-1) .
*
52		    PARENT = PARENT+1
		    IF (PARENT .GT. BETA(J-1)) THEN
			GOTO 23
		    ELSE
		        READ (FBETA(J-2,PARENT),25) PTR,B1,N
			GOTO 26
		    ENDIF
23		    READ (FBETA(J-1,1),25) PTR,B1,N
21	        CONTINUE


***************
*	Assign values to the couplings forward ,
*	COUPLE(I) = Alpha(i) for coupling(j) if i <= M ;
*
		NBETA = BETA(M)
55	        DO 27 J=1,NBETA
	    	    DO 31 I=1,M
	    	        COUPLE(I,J) = ALFA(I,NB)
31		    CONTINUE
		    DO 32 K=2*M-1,9
			COUPLE(K,J) = '   '
32		    CONTINUE
27	        CONTINUE

***************
*	Assign values to the couplings backward ,
*	COUPLE(i) = Beta(i-m) for coupling(j) if i > m
*
53	        DO 28 I=M,2,-1
		    N = 1
	    	    DO 28 J=1,BETA(I)
			READ (FBETA(I-1,J),25) PTR,CH2,NT
			DO 28 K=1,NT
	    	    	    COUPLE(M+I-1,N) = CH2//'0'
			    N = N+1
28	        CONTINUE

***************
*    Selection from generated couplings according the following rules
*
37		DO 29 I=1,NBETA
		    N = 2*M-1

***************
*	If the first time to call COUPLD, not compute MAX and MIN
*
		    IF (MAX .EQ. -5) GOTO 34

***************
*	Compute MAX and MIN value for each final term, keep it if 
*   intersection is non-empty
*
		    CH2 = COUPLE(N,I)(1:2)
		    CH3 = CH2(1:1)
		    CH1 = CH2(2:2)
		    S = (ICTOI(CH3)-1)/2.
		    L = LVAL(CH1)
		    LMIN = 2*ABS(S-L)
		    LMAX = 2*ABS(S+L)
		    IF (LMIN.GT.MAX .OR. LMAX.LT.MIN) GOTO 29

***************
*	If Final Terms are given, do selection
*
34		    IF (NFTM .NE. 0) THEN
		        CH2 = COUPLE(N,I)(:2)
		        DO 40 K=1,NFTM
			    IF (CH2 .EQ. FTM(K)) GOTO 42
40		        CONTINUE
41		        GOTO 29
		    ENDIF

***************
*	Waining if the number of couplings > 500
*
		    IF (NC .EQ. 500) THEN
		        PRINT *, '          WARNING !'
		        PRINT *, '          The number of couplings is
     : greater than 500 .'
		        PRINT *, '          Please select the Final Term .'
		        RETURN 1
		    ENDIF

***************
*	Write configurations and couplings to CI.LST
*
42		    NC = NC+1
		    WRITE (FILE3(NC),30) (COUPLE(J,I),J=1,N)
30		    FORMAT(9(A3))
		    WRITE (7,35) (ELC(J),Q(J), J=1,M)
35		    FORMAT (5(1X,A3,'(',I2,')'))
		    WRITE (7,36) (COUPLE(J,I), J=1,N)
36		    FORMAT (9(5X,A3))
29		CONTINUE
20	    CONTINUE
	    RETURN
	END


*
* ---------------------------------------------------------------------
*		SUBROUTINE	C O N F I G
* --------------------------------------------------------------------
*
*	     	Examine if the new configuration has the same
*	electron number and parity .          
*
	SUBROUTINE CONFIG 
	    PARAMETER     (NSHEL=20,NCOUPL=2*NSHEL-1)
	CHARACTER*3    EL(NSHEL),ELL(NSHEL),ELR(NSHEL),ELS(NSHEL,20),
     :		       ELA(30)
	CHARACTER*3    CH3
  	CHARACTER      FBETA*8, FILE1*30, FILE2*50, FILE3*27
	    INTEGER       QA(30),TOTAL,PARITY,CONST
	INTEGER        Q(NSHEL),QL(NSHEL),QR(NSHEL),
     :		        QS(NSHEL,20),MS(NSHEL),RL(30),
     :		        Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :		        Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30
	    COMMON        NF,NR,NFTM,MAX,MIN,PARITY,CONST,NQ
     :		       /BLK3/EL,ELL,ELR,ELS,ELA
     :		       /BLK4/Q,QL,ML,QR,MR,M,QS,MS,MA,RL,NREF,
     :		             Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :			     Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30
		COMMON /FILES/FBETA(4,2000),FILE1(200),FILE2(200),
     :			      FILE3(2000)
	    EQUIVALENCE   (Q1,QA(1)),(Q2,QA(2)),(Q3,QA(3)),(Q4,QA(4)),
     :			  (Q5,QA(5)),(Q6,QA(6)),(Q7,QA(7)),(Q8,QA(8)),
     :			  (Q9,QA(9)),(Q10,QA(10)),(Q11,QA(11)),
     :			  (Q12,QA(12)),(Q13,QA(13)),(Q14,QA(14)),
     :			  (Q15,QA(15)),         
     : 	                  (Q16,QA(16)),(Q17,QA(17)),(Q18,QA(18)),
     :                    (Q19,QA(19)),
     :			  (Q20,QA(20)),(Q21,QA(21)),(Q22,QA(22)),
     :                    (Q23,QA(23)),
     :			  (Q24,QA(24)),(Q25,QA(25)),(Q26,QA(26)),
     :			  (Q27,QA(27)),(Q28,QA(28)),(Q29,QA(29)),
     :			  (Q30,QA(30))

*   INPUT :
*     Q,Qi  =  Occupation number
*   	 L  =  L-value corresponding ELi
*   OUTPUT :
*	NQ  =  Number of configurations

***************
*  Return if the new configuraton has the different number of electron
*
	    TOTAL = Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10
	    IF (TOTAL .NE. CONST) RETURN

***************
*	Return if the new configuration has different pairty
*
	    NEWP = 0
	    DO 11 I=1,MA
		NEWP = NEWP+QA(I)*RL(I)
11	    CONTINUE

	    NEWP = MOD(NEWP,2)
	    IF (NEWP .NE. PARITY) RETURN

***************
*   Return if the new condiguration is the same as the Reference Set
*
	    DO 12 I=1,NREF
	  	M = MS(I)
		N = 0
		DO 14 J=1,M
		    CH3 = ELS(J,I)
		    IQ = QS(J,I)
		    DO 14 K=1,MA
			IF (CH3.EQ.ELA(K) .AND. IQ.EQ.QA(K)) N = N+1
14		CONTINUE
		IF (N .EQ. M) RETURN
12	    CONTINUE

***************
*	Otherwise, write them into the configuration file
*
	    NQ = NQ+1
	    WRITE (FILE1(NQ),10) (QA(J), J=1,MA)
10	    FORMAT (30(I2))
	    RETURN
	END



* ---------------------------------------------------------------------
*	SUBROUTINE	D E C O M P
* ---------------------------------------------------------------------
*
*	Decompose the string of Replacement
*
	SUBROUTINE DECOMP (STR,EL,Q,MR)
	    PARAMETER      (NSHEL=20,NCOUPL=2*NSHEL-1)
	    CHARACTER*48   STR,DEL
	    CHARACTER*3    CH3,EL(NSHEL)
	    INTEGER        Q(NSHEL),LEFT,RIGHT,LVAL
	    INTEGER        ORDLA,ORDLZ,ORDUA,ORDUZ,ORD0,ORD9
	    COMMON         /BLK0/ORDLA,ORDLZ,ORDUA,ORDUZ,ORD0,ORD9

*
*	INPUT  :
*         STR  =  String to be decomposed
*         EL and Q
*	OUTPUT  :
*	    MR  =  Number of EL to be replaced
*
	    DO 10 I=1,5
		IF (STR(:5) .EQ. '     ') THEN
		    MR = I-1
		    RETURN
		ENDIF
		STR = DEL(STR)

		LEFT = INDEX(STR,'(')
		RIGHT = INDEX(STR,')')

***************
*	If the Replacement is like 2s.2p = 3s.3p
*
		IF (LEFT .EQ. 0) THEN
		    K = 1
		    N = ICHAR(STR(3:3))
		    IF (N.GE.ORD0 .AND. N.LE.ORD9) THEN
			J = 3
		    ELSE
			J = 2
		    ENDIF
		    CH3 = STR(:J)

***************
*	If the Replacement is like 2p(2) = 3p(2)
*
		ELSE
		    CH3 = STR(LEFT+1:RIGHT-1)
		    K = ICTOI(CH3)
		    CH3 = STR(:LEFT-1)
		ENDIF

***************
*	Convert uppercase to lowercase, and assign value to ELi,Qi
*
		N = ICHAR(CH3(2:2))
		IF (N.GE.ORDUA .AND. N.LE.ORDUZ) CH3(2:2) = CHAR(N-ORDUA+ORDLA)
		EL(I) = CH3
		Q(I) = K
		IF (LEFT .EQ. 0) THEN
		    STR = STR(J+2:)
		ELSE
		    STR = STR(RIGHT+1:)
		ENDIF
10	    CONTINUE
	    RETURN
	END


* ---------------------------------------------------------------------
*	SUBROUTINE	R E P L A C E
* ---------------------------------------------------------------------
*
	SUBROUTINE REPLAC(ELB,QB,MB,*)

*   OUTPUT :
*	ELB,QB,MB

	    PARAMETER     (NSHEL=20,NCOUPL=2*NSHEL-1)
	CHARACTER*3    EL(NSHEL),ELL(NSHEL),ELR(NSHEL),ELS(NSHEL,20),
     :		       ELA(30)
	CHARACTER*3    ELB(NSHEL),ELC(NSHEL),CH3,DEL
	    CHARACTER     CH0,CH1
  	CHARACTER      FBETA*8, FILE1*30, FILE2*50, FILE3*27
	INTEGER        Q(NSHEL),QL(NSHEL),QR(NSHEL),
     :		        QS(NSHEL,20),MS(NSHEL),RL(30),
     :		        Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :		        Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30
         INTEGER       QA(30),QB(NSHEL),QC(NSHEL),PARITY,CONST,PP(200),
     :			NP,QQ
	    COMMON        NF,NR,NFTM,MAX,MIN,PARITY,CONST,NQ
     :		       /BLK3/EL,ELL,ELR,ELS,ELA
     :		       /BLK4/Q,QL,ML,QR,MR,M,QS,MS,MA,RL,NREF,
     :		             Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :			     Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30
		COMMON /FILES/FBETA(4,2000),FILE1(200),FILE2(200),
     :			      FILE3(2000)


	    DO 10 I=1,NSHEL
		ELC(I) = EL(I)
		QC(I) = Q(I)
10	    CONTINUE

*
**********     Correct Q(i) by subtraction
*
50	    DO 16 I=1,ML
	        CH3 = ELL(I)
	        MARK = 1
	        DO 16 J=1,M
	    	IF (CH3 .EQ. ELC(J)) THEN
	    	    QC(J) = QC(J)-QL(I)
		    IF (QC(J) .GE. 0) MARK = 0
	    	ENDIF
16	    CONTINUE
51	    IF (MARK) RETURN 1

*
**********     Correct QC(i) by adding
*
	    MC = M
	    DO 15 I=1,MR
	        CH3 = ELR(I)
	        MARK = 0
	        DO 17 J=1,MC
	            IF (CH3 .EQ. ELC(J)) THEN
	        	QC(J) = QC(J)+QR(I)
	    	        MARK = 1
	            ENDIF
17	        CONTINUE
52		IF (MARK .EQ. 0) THEN
		    MC = MC+1
		    ELC(MC) = CH3
		    QC(MC) = QR(I)
		ENDIF
15	    CONTINUE

***************
*	Delete EL(i) if Q(i) = 0
*
65	    MB = 0
	    DO 30 I=1,MC
		IF (QC(I).NE.0 .OR. MC.EQ.2) THEN
		    MB = MB+1
		    ELB(MB) = ELC(I)
		    QB(MB) = QC(I)
		ENDIF
30	    CONTINUE

*
**********     Check the input error after replacement  
*
	    J = 0
	    K = 0
	    DO 27 I=1,MB
	        CH3 = ELB(I)
	        L = LVAL(CH3(2:2))
	        IF (QB(I) .GT. L*4+2) THEN
		    RETURN 1
	        ENDIF
	        J = J+QB(I)
	        K = K+QB(I)*L
27	    CONTINUE

53	    IF (J .NE. CONST) RETURN 1

	    IF (MOD(K,2) .NE. PARITY) RETURN 1

*
**********    If the replacement duplicates a configuration in the
*	    active set, it should not be sent to CI.LST
*
	    DO 14 I=1,NQ
	        READ (FILE1(I),25) (QA(J), J=1,MA)
25	        FORMAT (30(I2))
		N = 0
		DO 35 J=1,MB
		    CH3 = ELB(J)
		    DO 35 K=1,MA
			IF (CH3.EQ.ELA(K) .AND. QB(K).EQ.QA(K)) N = N+1
35		CONTINUE
		IF (N .EQ. MB) RETURN 1
14	    CONTINUE

***************
*	If the replacement duplicates a configuration in the
*   Reference Set, it should not be sent to CI.LST
*
	    DO 31 I=1,NREF
		L = MS(I)
		N = 0
		DO 36 J=1,L
		    CH3 = ELS(J,I)
		    QQ = QS(J,I)
		    DO 36 K=1,MB
			IF (CH3.EQ.ELB(K) .AND. QQ.EQ.QB(K)) N = N+1
36		CONTINUE
		IF (N .EQ. MB) RETURN 1
31	    CONTINUE

***************
*	If the replacement duplicates a configuration in the previous
*   replacement, it should not sent to CI.LST
*
	    DO 40 I=1,NP
		L = PP(I)
		READ (FILE2(I),42) (ELC(J),QC(J), J=1,L)
42		FORMAT (10(A3,I2))
		N = 0
		DO 41 J=1,L
		    CH3 = ELC(J)
		    DO 41 K=1,MB
			IF (CH3.EQ.ELB(K) .AND. QC(K).EQ.QB(K)) N = N+1
41		CONTINUE
		IF (N .EQ. MB) RETURN 1
40	    CONTINUE

	    NP = NP+1
	    PP(NP) = MB
	    WRITE (FILE2(NP),37) (ELB(J),QB(J), J=1,MB)
37	    FORMAT (20(A3,I2))
	    RETURN
	END



* ---------------------------------------------------------------------
*	SUBROUTINE	V P A I R
* ---------------------------------------------------------------------
*
*	Generate occupied or virtual pairs for D-Replacement
*
	SUBROUTINE VPAIR (ELV,MV,PL,STR,*)

*   INPUT :
*     	ELV  =  ELi for Virtual Set
*        MV  =  Number of ELi for Virtual Set
*        PL  =  Parity of Qi for Reference Set
*       STR  =  String to be packed as output for Replacement
*


	    PARAMETER     (NSHEL=20,NCOUPL=2*NSHEL-1)
	    CHARACTER*72  HEADER,SHELLS,VIRTUL,STR,DEL
	    CHARACTER*48  REF(20),ACT(20),REPL(20)
	CHARACTER*2    FTM(20)
	CHARACTER*3    EL(NSHEL),ELL(NSHEL),ELR(NSHEL),ELS(NSHEL,20),
     :		       ELA(30)
	    CHARACTER*3   ELV(30),ELC(NSHEL)
	INTEGER        Q(NSHEL),QL(NSHEL),QR(NSHEL),
     :		        QS(NSHEL,20),MS(NSHEL),RL(30),
     :		        Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :		        Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30
	    INTEGER       QA(NSHEL),PL,PR,PARITY,CONST
	    COMMON	  NF,NR,NFTM,MAX,MIN,PARITY,CONST,NQ
     :			  /BLK1/HEADER,SHELLS,ACT,VIRTUL
     :			  /BLK2/REF,REPL,FTM
     :		       /BLK3/EL,ELL,ELR,ELS,ELA
     :		       /BLK4/Q,QL,ML,QR,MR,M,QS,MS,MA,RL,NREF,
     :		             Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,
     :			     Q10,Q11,Q12,Q13,Q14,Q15,
     :		        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,
     :			Q25,Q26,Q27,Q28,Q29,Q30

	    N = INDEX(STR,'=')
	    NR = 20

***************
*	D-Replacement for the pair of two single ELi
*
	    MR = 2
	    QR(1) = 1
	    QR(2) = 1
	    DO 10 I = 1,MV-1
		ELR(1) = ELV(I)
		DO 10 J=I+1,MV
		    ELR(2) = ELV(J)
		    PR = MOD(LVAL(ELR(1)(2:2))+LVAL(ELR(2)(2:2)), 2)

*
*	If it has the same parity with the left side, replace them,
*   then generate couplings for the new configuration
*
		    IF (PL .NE. PR) GOTO 10
		    CALL REPLAC(ELA,QA,K,*20)
		    CALL COUPLD (ELA,QA,K,ELC,NC,*25)
		    IF (NC .GT. 0) THEN
		        WRITE (6,12) 
12		        FORMAT ('1  FOR VIRTUAL SET, GENERATE
     : CONFIGURATION AND COUPLINGS FOR D-REPLACEMENT'//)
		        STR = STR(:N)//ELR(1)//'.'//ELR(2)
		        REPL(NR) = DEL(STR)
		        CALL PRINT (ELC,QA,K,NC,4)
		    ENDIF
10	    CONTINUE

***************
*	D-Replacement for the pairs which has the value Qi=2
*
50	    MR = 1
	    QR(1) = 2
	    DO 20 I=1,MV
		ELR(1) = ELV(I)
		PR = MOD(LVAL(ELR(1)(2:2))*2, 2)

*
*	If it has the same parity with the left side, replace them,
*   then generate couplings for the new configuration
*
		IF (PL .EQ. PR) THEN
		    CALL REPLAC(ELA,QA,K,*20)
		    CALL COUPLD (ELA,QA,K,ELC,NC,*25)
		    IF (NC .GT. 0) THEN
		        WRITE (6,22)
22		        FORMAT ('1  FOR VIRTUAL SET, GENERATE
     : CONFIGURATION AND COUPLINGS FOR D-REPLACEMENT'//)
		        STR = STR(:N)//ELR(1)//'(2)'
		        REPL(NR) = DEL(STR)
		        CALL PRINT (ELC,QA,K,NC,4)
		    ENDIF
		ENDIF
20	    CONTINUE

	    RETURN
25	    RETURN 1
	END




* ----------------------------------------------------------------------
*		SUBROUTINE	P R I N T 
* ----------------------------------------------------------------------
*
*	Print out the values of couplings 
*
	SUBROUTINE PRINT (EL,Q,M,NC,MARK)
	    PARAMETER      (NSHEL=20,NCOUPL=2*NSHEL-1)
	    CHARACTER*48   REF(20),REPL(20)
	    CHARACTER*72   HEADER,SHELLS,ACT,VIRTUL
	    CHARACTER*3    EL(NSHEL),COUPLE(NCOUPL)
	    CHARACTER*2    FTERM,FTM(20)
	    CHARACTER      CH1
  	CHARACTER      FBETA*8, FILE1*30, FILE2*50, FILE3*27
	    INTEGER        Q(NSHEL),PARITY,CONST
	    COMMON         NF,NR,NFTM,MAX,MIN,PARITY,CONST,NQ
     :			   /BLK1/HEADER,SHELLS,ACT,VIRTUL
     :			   /BLK2/REF,REPL,FTM
		COMMON /FILES/FBETA(4,2000),FILE1(200),FILE2(200),
     :			      FILE3(2000)


*   MARK  =  1 for Reference Set
*	  =  2 for Reference Set, Active Set
*	  =  3 for Reference Set, Replacement
*	  =  4 for Reference Set, Virtual Set
*


***************
*		Print the input of Header
*
	    J = INDEX(HEADER,'          ')
	    WRITE (6,11) HEADER(1:J)
11	    FORMAT (' '/T10,'-------------       ',A,'  ---
     :-----',/)

***************
*		Print the input of Closed Shells
*
	    J = INDEX(SHELLS,'     ')
	    WRITE (6,12) SHELLS(:J)
12	    FORMAT ('          Closed Shells  :  ',A/)
	    IF (MARK.EQ.1 .OR. MARK.EQ.2) GOTO 20

***************
*		Print the input of Reference Set
*
	    WRITE (6,10) REF(NF)
10	    FORMAT ('          Reference set  :  ',A48/)
	    IF (MARK .EQ. 3) GOTO 21

***************
*		Print the input of Virtual Set
*
	    IF (MARK .EQ. 4) CH1 = 'S'
	    IF (MARK .EQ. 5) CH1 = 'D'
	    IF (MARK.EQ.4 .OR. MARK.EQ.5) THEN
		WRITE (6,30) VIRTUL
30		FORMAT ('            Virtual Set  :  ',A48/)
		WRITE (6,31) CH1,REPL(20)
31		FORMAT ('          ',A1,'-Replacement  :  ',A48/)
		GOTO 20
	    ENDIF

***************
*		Print the input of Active Set
*
	    WRITE (6,19) ACT
19	    FORMAT ('             Active Set  :  ',A48/)
	    IF (MARK .EQ. 2) GOTO 20

***************
*		Print the input of Replacement
*
21	    WRITE (6,22) REPL(NR)
22	    FORMAT ('            Replacement  :  ',A48/)

***************
*	Print the new configuration by Replacement
*
20	    IF (EL(1)(1:1) .EQ. ' ') THEN
		K = 1
	    ELSE
		K = 2
	    ENDIF
	    WRITE (6,13) (EL(J),Q(J), J=1,M)
13	    FORMAT ('          Configuration  :',
     :		5(2X,A3,'(',I2,')')/)

***************
*	Print couplings generated from the configuration
*
	    K = 2*M-1
	    IF (M .LE. 3) THEN
	        READ (FILE3(1),17) (COUPLE(J), J=1,K)
	        WRITE (6,23) (COUPLE(J), J=1,K)
23	        FORMAT ('        Their couplings  :  ',5(A3,5X))
	        DO 24 I=2,NC
	    	    READ (FILE3(I),17) (COUPLE(J), J=1,K)
	    	    WRITE (6,25) (COUPLE(J), J=1,K)
25	    	    FORMAT (T29,9(A3,5X))
24	        CONTINUE
	    ELSE
	        WRITE (6,15)
15	        FORMAT ('         Their couplings  :'/)
	        DO 16 I=1,NC
	    	    READ (FILE3(I),17) (COUPLE(J), J=1,K)
17	    	    FORMAT (9(A3))
	    	    WRITE (6,18) (COUPLE(J), J=1,K)
18	    	    FORMAT (T5,9(5X,A3))
16	        CONTINUE
	    ENDIF
	    RETURN
	END



* ---------------------------------------------------------------------
*	SUBROUTINE	H E L P
* ---------------------------------------------------------------------
*
*	Explanation of the input format 
*
	SUBROUTINE HELP ()
	    CHARACTER*10   STR

20	    PRINT *
	    PRINT *
	    PRINT *,'     This program gives prompts for each required 
     :input. The user'
	    PRINT *,'     should enter data or a RETURN after question
     : mark ''?''.'
	    PRINT *
	    PRINT *,'     Example 1 :'
	    PRINT *,'    --------------------'
	    PRINT *,'                 Header  ?  S II ground state'
	    PRINT *,'          Closed shells  ?  1s 2s'
	    PRINT *,'          Reference Set  ?  3s(2) 3p(3)'
	    PRINT *,'                      2  ?  RETURN'
	    PRINT *,'             Active Set  ?  3s,3p'
	    PRINT *,'            Replacement  ?  3s(2) = 3d(2)'
	    PRINT *,'                      2  ?  3s = 3d'
	    PRINT *,'                      3  ?  3s.3p = 4s.3d'
	    PRINT *,'                      4  ?  <RETURN>'
	    PRINT *,'             Final Term  ?  3S'
	    PRINT *,'                      2  ?  RETURN'
	    PRINT *
	    PRINT *,'          Header and Closed Shells cannot exceed 72
     : characters.  They will'
	    PRINT *,'     be sent to CI.LST.  The two items are
     : separated by a blank in the'
	    PRINT *,'     Closed Shells.'
	    PRINT *
	    PRINT 11
11	    FORMAT ('     Press RETURN for more... ')
	    READ '(A)', STR

21	    DO 12 I=1,7
	        PRINT *
12	    CONTINUE
	    PRINT *,'          Items are separated by a blank
     : in the Reference Set, by a comma'
	    PRINT *,'    or a blank in the Active Set, and by a
     : period or a blank in Replacements.'
	    PRINT *,'          The blanks in the Reference set and
     : Replacement can be omitted.'
	    PRINT *,'          Reference Set, Replacement and Final Term
     : are three input sets. '
	    PRINT *,'     Each set has 0 up to 20 members.  Each member
     : is entered on a separate'
	    PRINT *,'     line.  '
	    PRINT *,'          PRINT RETURN to terminate the input set.'
	    PRINT *,'          PRINT RETURN if the set is empty.'
	    PRINT *
	    PRINT *,'     Example 2 :'
	    PRINT *,'    --------------------'
	    PRINT *
	    PRINT *,'         Reference Set  ?  2s(1) 2p(2) 3s(1)'
	    PRINT *,'                     2  ?  RETURN'
	    PRINT *,'            Active Set  ?  2s,2p,3s'
	    PRINT *,'           Replacement  ?  RETURN '
	    PRINT *,'            Final Term  ?  RETURN'
	    PRINT *
	    PRINT *,'     Where the Replacement and the Final Term are
     : empty.'
	    PRINT *
	    PRINT *
	    PRINT 11
	    READ '(A)', STR
	    I = INDEX(STR,'B')
	    J = INDEX(STR,'b')
	    IF (I.OR. J) GOTO 20

22	    DO 19 I=1,7
	    	PRINT *
19	    CONTINUE
	    PRINT *,'          By inputing ''s'' or ''d'' or ''sd'' you
     : can compute the configurations'
	    PRINT *,'     from the Virtual Set, where  S means Single
     : Replacement, D means Double '
	    PRINT *,'     Replacement, SD  means Single and Double
     : Replacement.'
	    PRINT *
	    PRINT *,'          GENCI will give you prompts for the Virtual
     : Set automatically, then '
	    PRINT *,'    you need to specify the range of shells that 
     :are to participate in the'
	    PRINT *,'    replacements. For instance, a response of 2 to
     : "From which shell" and of'
	    PRINT *,'    3 to "To which shell" implies that shells 2
     : and 3 participates in the'
	    PRINT *,'     replacements, and shell 1 does not enter 
     :into any replacements.'
	    PRINT *
	    PRINT *,'     Example 3  :'
	    PRINT *,'    -------------------------'
	    PRINT *,'                ...'
	    PRINT *,'         Reference Set  ?  2s(1) 2p(1) 3s(1)'
	    PRINT *,'            Active Set  ?  RETURN'
	    PRINT *,'           Replacement  ?  sd'
	    PRINT *,'           Virtual Set  ?  3p,3d,4s'
	    PRINT *,'      From which shell  ?  2'
	    PRINT *,'        To which shell  ?  3'
	    PRINT *,'            Final Term  ?  RETURN'
	    PRINT *
	    PRINT *
	    PRINT *
	    PRINT 11
	    READ '(A)', STR
	    I = INDEX(STR,'B')
	    J = INDEX(STR,'b')
	    IF (I.OR. J) GOTO 21


23	    DO 13 I=1,7
	        PRINT *
13	    CONTINUE
	    PRINT *,'          After terminating an input line, if you find
     : previous input to be '
	    PRINT *,'     wrong, type ''B'' or ''b'' to go back one step.'
	    PRINT *
	    PRINT *,'          For example, before inputing Active Set, 
     :if you find the wrong'
	    PRINT *,'     spelling in the Header, type "B" and GENCI 
     :will prompt Header again.'
	    PRINT *
	    PRINT *
	    PRINT *,'     Example 3 :'
	    PRINT *,'    ----------------------'
	    PRINT *,'                Header  ?  OXYYEN'
	    PRINT *,'            Active Set  ?  B'
	    PRINT *,'                Header  ?  OXYGEN '
	    PRINT *,'            Active Set  ?  2s '
	    PRINT *
	    PRINT *,'     Then the following prompts will continue.'
	    PRINT *
	    PRINT *
	    PRINT 11
	    READ '(A)', STR
	    I = INDEX(STR,'B')
	    J = INDEX(STR,'b')
	    IF (I.OR. J) GOTO 22

24	    DO 16 I=1,7
	        PRINT *
16	    CONTINUE
	    PRINT *,'         Example 4 shows the procedure for
     : going back four steps'
	    PRINT *,'     to correct the Closed Shells from  5s  to
     :  1s 2s.'
	    PRINT *
	    PRINT *
	    PRINT *,'     Example 4 :'
	    PRINT *,'    -----------------------'
	    PRINT *
	    PRINT *,'            Active Set  ?  5s'
	    PRINT *,'         Reference Set  ?  2s(1) 2p(2) 3s(1)'
	    PRINT *,'                     2  ?  2P(4)'
	    PRINT *,'                     3  ?  b '
	    PRINT *,'                     2  ?  b '
	    PRINT *,'         Reference Set  ?  b '
	    PRINT *,'         Closed Shells  ?  1s 2s '
	    PRINT *,'         Reference Set  ?  '
	    PRINT *
	    PRINT *
	    PRINT *,'          Then reenter the data for the Reference
     : Set, and continue the input.'
	    PRINT *
	    PRINT 11
	    READ '(A)', STR
	    I = INDEX(STR,'B')
	    J = INDEX(STR,'b')
	    IF (I.OR. J) GOTO 23

25	    DO 17 I=1,7
	        PRINT *
17	    CONTINUE
	    PRINT *,'          When the following error conditions 
     : are detected, give the error'
	PRINT *,'     message.'
	    PRINT *
	    PRINT *,'     1). The parentheses are not matched'
	    PRINT *,'     2). The number of electrons in a shell is more 
     : than FULL '
	    PRINT *
	    PRINT *,'         For each member of the Reference Set ,'
	    PRINT *,'     3). The number of electrons is not the same '
	    PRINT *,'     4). The parity is not the same '
	    PRINT *
	    PRINT *,'     5). The number of couplings generated by a 
     :configuration is more than 500'
	    DO 14 I=1,9
	        PRINT *
14	    CONTINUE
	    PRINT 15
15	    FORMAT ('     Press RETURN to begin the program.')
	    READ '(A)', STR
	    I = INDEX(STR,'B')
	    J = INDEX(STR,'b')
	    IF (I.OR. J) GOTO 24
	    DO 18 I=1,30
	        PRINT *
18	    CONTINUE
	    FLAG = 1
	    RETURN
	END

