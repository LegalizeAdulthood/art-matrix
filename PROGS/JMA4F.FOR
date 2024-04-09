      SUBROUTINE JMA4F (XL,YL,SIDE,NITER,CR,CI,COL)
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
      DO 100 J = 0,400
      DO 100 I = 0,400
100   COL(I,J) = 0
C
      ESCAPE = 100
C
      C = DCMPLX(CR,CI)
C
      CPU = 400/SIDE
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
         IX = ( REAL(Z) - XL)*CPU
         IY = (IMAG(Z) - YL)*CPU
         IF (IX .LT. 0 .OR. IX .GT. 400 .OR.
     *       IY .LT. 0 .OR. IY .GT. 400) GOTO 410
C
         COL(IX,IY) = COL(IX,IY) + 1
         COL(IX,IY) = MIN(COL(IX,IY),1000)
C
         Z = Z*Z + C
410   IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 1000
C
400   CONTINUE
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
