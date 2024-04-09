      SUBROUTINE JNS4F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,RCOEFF(0:20),RROOTS(1:20)
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  A,B,C,Z,Z1,V,P,PP,T1,T2
      COMPLEX*16  COEFF(0:10),ROOTS(10)
C
      COMMON /INPUT/ RCOEFF,RROOTS,NROOT
      EQUIVALENCE (RCOEFF(0),COEFF(0))
      EQUIVALENCE (RROOTS(1),ROOTS(1))

C
C***********************************************************************
C***********************************************************************
C
      MODULO = 4
C
      PI2   =  6.283185307179586D00/4
C
      ROOTS(1) = 0
      ROOTS(2) = PI2
C
      EPS    =  1.E-10
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
C
      Y = YLOW
      DO 2000  M = 0,400
C
         Y = Y + DELTA
C
         X = XLOW
         DO 1000 N = 0,400
C
            X = X + DELTA
C
C***********************************************************************
C***********************************************************************
C
            Z = DCMPLX(X,Y)
C
            DO 400 I = 1,NITER
C
               Z = Z - (SIN(Z)/COS(Z))
C
               DO 460 J = 1,2
                  V = Z - ROOTS(J)
                  IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) THEN
                     COL(M,N) = J + MODULO*I
                     GOTO 1000
                  ENDIF
460            CONTINUE
C
400         CONTINUE
C
            COL(M,N) = 3
            GOTO 1000
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C***********************************************************************
C
      FUNCTION CDIFF (X,Y)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
         REAL*8   CDIFF
      COMPLEX*16  X,Y,Z
C
      Z = X - Y
C
      CDIFF = (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z))
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
