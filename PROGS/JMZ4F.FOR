      SUBROUTINE JMZ4F (XL,YL,SIDE,NITER,CR,CI,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,CR,CI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C
C
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(CR,CI)
C
      ESCAPE  =  4
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
C
      Y = YLOW
      DO 2000  M = 0,400

         Y = Y + DELTA

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
         Z = Z*Z + C
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
420   COL(M,N) = 1000*ABS(Z)
      GOTO 1000
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
