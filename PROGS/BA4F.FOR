      SUBROUTINE BA4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
         REAL*8  ARG
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,V,A,B,C,CB
C
C
C***********************************************************************
C***********************************************************************
C
      TPI     =  2*3.141592653589793
C
      TPI14   =    TPI/4
      TPI24   =    TPI/2
      TPI34   =  3*TPI/4
C
C***********************************************************************
C***********************************************************************
C
      SQRT3   =  3**.5
      SQRT3M  = -SQRT3
C
      ESCAPE  =  100.0
      EPSLON  = .000001
C
      DELTA   =   SIDE/400.0
      XLOW    =   XL - DELTA
      YLOW    =   YL - DELTA
C
      Y = YLOW
      DO 2000 M = 0,400
C
         Y = Y + DELTA
C
         X = XLOW
         DO 1000 N = 0,400
C
            X = X + DELTA
C
            C  = DCMPLX(X,Y)
            CB = DCMPLX(REAL(C),-IMAG(C))
C
            A = 2*C
C
            B = C*C + CB
C
C***********************************************************************
C***********************************************************************
C
      Z = 0
C
      DO 400 I=1,NITER
C
         Z =  Z*Z*Z*Z + A*Z*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 430
C
C        V = Z - V
C        IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSLON) GOTO 420
C
400   CONTINUE
C
      COL(M,N) =  0
      GOTO 1000
C
C20   COL(M,N) = -1
C
430   ANGLE = ARG(REAL(Z),IMAG(Z))
      IF     (ANGLE .LE. TPI14)   THEN
         COL(M,N) = 1 + 5*I
      ELSEIF (ANGLE .LE. TPI24)   THEN
         COL(M,N) = 2 + 5*I
      ELSEIF (ANGLE .LE. TPI34) THEN
         COL(M,N) = 4 + 5*I
      ELSE
         COL(M,N) = 3 + 5*I
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE
C
2000  CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C***********************************************************************
C
      FUNCTION ARG (X,Y)
C
      REAL*8  X,Y,ARG
C
      IF (X .GT. 0 .AND. ABS(Y) .LE. X) THEN
         ARG = ATAN(Y/X)
         IF (Y .LT. 0) ARG = ARG + 6.283185307179586D00
C
      ELSEIF (Y .GT. 0 .AND. ABS(X) .LE. Y) THEN
         ARG = ATAN(X/Y)
         ARG = 1.570796326794897D00 - ARG
C
      ELSEIF (X .LT. 0 .AND. ABS(Y) .LE. -X) THEN
         ARG = ATAN(Y/X)
         ARG = ARG + 3.141592653589793D00
C
      ELSE
         ARG = ATAN(X/Y)
         ARG = 4.71238898038469D00 - ARG
C
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
