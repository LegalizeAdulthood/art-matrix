      SUBROUTINE MAND4F (XL,YL,SIDE,NITER,COL)
C
C     XL IS X LOW
C     YL IS Y LOW
C     SIDE IS SIDE LENGTH AND APPLIES TO BOTH X AND Y.
C     NITER IS NUMBER OF ITERATIONS TO ITERATE EACH PIXEL.
C     COL IS A 401 BY 401 ARRAY TO HOLD FRACTAL IMAGE DATA.
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     FORMAT ASSUMES THAT VARIABLES STARTING WITH THE LETTERS
C     I THROUGH N (FOR INTEGER) ARE INTEGER*4 UNLESS OTHERWISE
C     DELCARED.  ALL OTHERS ARE ASSUMED TO BE REAL*4 WHICH MEANS
C     A 4 BYTE REAL NUMBER.  THE ABOVE IMPLICIT STATEMENT ASSURES
C     THAT ALL VARIABLES STARTING WITH OTHER THAN I THROUGH N
C     WILL BE REAL*8 OR DOUBLE PRECISION BY DEFAULT.
C
      REAL*8  XL,YL,SIDE
C
C     IN LIGHT OF THE ABOVE IMPLICIT STATEMENT THIS IS REDUNDANT.
C
      INTEGER*4   COL(0:400,0:400)
C
C     DECLARES COL TO BE A 401 BY 401 INTEGER MATRIX.  EACH DIMENSION
C     IS NUMBERED FROM 0 TO 400.
C
      COMPLEX*16  Z,C,FX
C
C     DECLARES Z C AND FX TO BE COMPLEX NUMBERS WITH TWO REAL*8 PARTS.
C
C***********************************************************************
C***********************************************************************
C
      ESC   =  100

C     THE ESC  VALUE IS THE REPRESENTATIVE OF INFINITY WHICH
C     DENOTES THAT THE ITERATION IS GOING OFF TO INFINITY.
C
      DO 2000 IY = 0,400
C
         Y = YL + (IY*SIDE)/400
C
         CALL SHOWIY (IY,Y)
C
         DO 1000 IX = 0,400
C
            X = XL + (IX*SIDE)/400

C     THE PRECEEDING TWO DO LOOPS ARE USED TO SCAN THE SPACE OF THE
C     MATRIX.  THE MATRIX PIXELS ARE SCANNED WITH THE VARIABLES
C     IX AND IY FOR THE X AND Y AXES.  THE NUMBER SPACE SCANNED
C     BY THE SAME DO LOOPS IS PROVIDED BY THE TWO VARIABLES X AND Y.
C     THEY ARE COMPUTED BY DIVIND THE SIDE LENGTH BY THE NUMBEER OF
C     PIXELS IN THE MATRIX (400, NOT 401!) AND MULTIPLYING BY THE
C     NUMBER OF PIXELS SCANNED OVER FROM THE LOWER LEFT.
C          NORMALLY THIS WOULD BE DONE WITH
C     X = XL + IX*(SIDE/400).  IN FACT (SIDE/400) COULD BE
C     STORED AS A CONSTANT AND NOT RECOMPUTED EVERY TIME.  HOWEVER
C     UNDER GREAT MAGNIFICATION THIS PRODUCES UNCERTAIN RESULTS AS
C     (SIDE/400) BECOMES A VERY SMALL NUMBER THAT LOOSES RESOLUTION.
C     BY MULTIPLYING SIDE BY IX FIRST (IX*SIDE) THE NUMBER IS MADE
C     LARGER FIRST BEFORE IT IS DIVIDED BY 400, MAINTAINING
C     RESOLUTION TO A SLIGHTLY DEEPER LEVEL.
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(X,Y)
C
C     THE NUMBER COORDINATES OF (X,Y) ARE STORED IN THE COMPLEX VARIABLE
C     C WHICH IS USED IN THE EQUATION Z = Z*Z + C.
C
      FX = 1 - CDSQRT(1-4*C)
      IF (REAL(FX)*REAL(FX) + IMAG(FX)*IMAG(FX) .LT. 1)     GOTO 410
C
C     IT IS POSSIBLE TO KNOW THAT C IS INSIDE THE MAIN CARDIOD
C     BY LOOKING AT THE DERIVATIVE OF THE FIXEDPOINT.  IF IT IS
C     LESS THAN ONE THAT MEANS C IS IN THE MAIN CARDIOD SO IT
C     IT UNNECESSARY TO ITERATE THAT BLACK AREA WHICH TAKES A LONG TIME.
C     A FIXED POINT IS FOUND BY SOLVING Z = Z*Z + C.  ITS A QUADRATIC
C     AND YOU USE THE QUADRATIC FORMULA TO FIND IT.  THERE ARE TWO
C     ANSWERS SINCE IT IS A QUADRATIC, YOU WANT THE ONE THAT IS IN
C     THE RIGHT HAND HALF OF THE COMPLEX PLANE.  FOR YOUR PC BASED
C     PROGRAMS YOU MIGHT WANT TO LEAVE OUT THIS CODE, ESPECIALLY
C     SINCE IT DEPENDS ON THE COMPLEX SQUARE ROOT.
C     HOWEVER YOUR CODE WILL RUN A LOT SLOWER IN THE MAIN CARDIOD
C     AREA.
C
      FX = 1 + C
      IF (REAL(FX)*REAL(FX) + IMAG(FX)*IMAG(FX) .LE. 1/16.) GOTO 410
C
C     IF C IS WITHIN A CIRCLE OF 1/4 RADIUS CENTERED AT -1 THAT MEANS
C     THAT C IS WITHIN THE 2 BALL SO THIS AREA NEED NOT BE ITERATED
C     EITHER.
C
C     THESE TWO KLUDGES ARE USED ONLY TO SPEED UP THE PROCESS OF
C     MAKING THE PICTURES.  THEY ARE NOT HELPFUL IN AREAS OTHER
C     THAN THE MAIN CARDIOD OR THE 2 BALL.
C
      Z = 0
      DO 400 I = 1,NITER
C
         Z = Z*Z + C
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESC ) GOTO 420
C
400   CONTINUE
C
C     THIS IS THE MAIN ITERATION LOOP.  IT IS RUN NITER TIMES OR
C     UNTIL THE ABSOLUTE VALUE OF Z GETS GREATER THAN ESC .
C     THE SQUARE OF THE VALUE OF Z IS USED TO AVOID THE USE OF
C     OF THE SQUARE ROOT FUNCTION WHICH IS REALLY SLOW.
C
C     IF THE LOOP IS ALLOWED TO COMPLETE THEN IT FALLS THROUGH
C     TO THE NEXT LINE WHERE THE COL MATRIX IS SET TO 0 AT THAT
C     PIXEL.  IF THE ESC  VALUE IS EXCEEDED DURING THEE ITERATION,
C     THEN THE ITERATION IS INTERRUPTED AND A BRANCH TAKES PLACE
C     TO THE NEXT LINE BELOW WHERE IN THE COL MATRIX IS SET
C     TO THE ITERATION NUMBER AT WHICH POINT THE ESC  VALUE
C     WAS EXCEEDED.
C
410   COL(IX,IY) = 0
      GOTO 1000
C
C420   IF (ABS(IMAG(Z)) .LT. .1*ABS(REAL(Z))) THEN
C            COL(IX,IY) = 7 + 8*I
C      ELSE

C     THE COMMENTED OUT CODE PRODUCES A SLIGHTLY DIFFERENT ANGLE
C     PICTURE AND SHOULD BE IGNORED FOR YOUR FIRST ATTEMPTS.
C
420      IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = 1 + 5*I
         ELSE
            COL(IX,IY) = 4 + 5*I
         ENDIF
C
C     THE ABOVE STORES THE ITERATIONS RESULT ALONG WITH THE ANGLE DATA.
C     IF YOU WISH TO IGNORE THE ANGLE DATA FOR STARTERS, JUST REPLACE
C     THE ENTIRE PIECE OF CODE WITH
C
C     COL(IX,IY) = I
C
C     ENDIF
C
C     THIS ENDIF IS PART OF THE COMMENTED OUT CODE ABOVE THAT PRODUCES
C     THE SLIGTHLY DIFFERENT ANGLE DATA.
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
C     THESE TWO CONTINUE STATEMENTS END THE RANGE OF THE TWO
C     PRIMARY DO LOOPS THAT SCAN THE MATRIX.
C
      RETURN
      END
