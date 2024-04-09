      SUBROUTINE CL4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,V,A,B,AA3,C,C2,C3,L,X1
C
C
C***********************************************************************
C***********************************************************************
C
      SQRT3   =  3**.5
      SQRT3M  = -SQRT3
C
      ESCAPE  =  100.0
      EPSLON  = .001
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
C
            C2 = C*C
C
            A  =  SQRT(  (C2*C2 + 4*C2 + 1)/(3*C)**2  )
            L  = (C2 - 1)/(3*C)
            B  =  2*L*L*L
C
            AA3 = 3*A*A
C
            X1 = (C2 + 2)/(3*C)
C
C***********************************************************************
C***********************************************************************
C
      Z = -A
C
      DO 400 I=1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 430
C
         V = Z - X1
400      IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSLON) GOTO 420
C
      COL(M,N) =  0
      GOTO 1000
C
C***********************************************************************
C***********************************************************************
C
420   Z = A
C
      DO 440 I=1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 430
C
         V = Z - X1
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSLON) GOTO 450
C
440   CONTINUE
C
      COL(M,N) =  0
      GOTO 1000
C
450   COL(M,N) = -1
      GOTO 1000
C
C***********************************************************************
C***********************************************************************
C
430   IF (REAL(Z) .GE. 0) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(M,N) = 1 + 5*I
         ELSE
            COL(M,N) = 4 + 5*I
         ENDIF
      ELSE
         ZIZR = IMAG(Z)/REAL(Z)
         IF (ZIZR .LE. SQRT3M) THEN
            COL(M,N) = 1 + 5*I
         ELSEIF   (ZIZR .GT. SQRT3) THEN
            COL(M,N) = 4 + 5*I
         ELSE
            COL(M,N) = 2 + 5*I
         ENDIF
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
