      SUBROUTINE CH4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C,C2,C3,A,B,AA3
C
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  100.0
C
      DELTA   =  SIDE/400.0
      XLOW    =  XL - DELTA
      YLOW    =  YL - DELTA
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
            C2 = C*C
            C3 = C2*C
C
            A = SQRT(C2 - 1./3.)
C
            AA3 = 3*C2 - 1
            B   = 2*C3
C
C***********************************************************************
C***********************************************************************
C
      Z = -A
C
      DO 400 I = 1,NITER
C
         Z =  Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GO TO 420
C
400   CONTINUE
C
      COL(M,N) = 0
      GOTO 500
C
420   COL(M,N) = I
C
C***********************************************************************
C***********************************************************************
C
500   Z = A
C
      DO 600 I = 1,NITER
C
         Z =  Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GO TO 620
C
600   CONTINUE
      GOTO 1000
C
620   COL(M,N) = INSERT(COL(M,N),32,16,I)
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
