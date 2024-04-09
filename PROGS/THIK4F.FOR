      SUBROUTINE THIK4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      PARAMETER (MSIZE = 400)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
         REAL*8  PX
      INTEGER*4   COL(0:MSIZE,0:MSIZE)
      COMPLEX*16  Z,ZP,C
C
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  250000.
C
      DELTA  =  SIDE/MSIZE
      PX     = .35*DELTA
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
C
      Y = YLOW
      DO 2000 M = 0,MSIZE
C
         Y = Y + DELTA
C
         X = XLOW
         DO 1000 N = 0,MSIZE
C
            X = X + DELTA
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(X,Y)
C
      Z  = 0
      ZP = 0
      DO 400 I = 1,NITER
C
         ZP = 2*Z*ZP + 1
          Z = Z*Z + C
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
      COL(M,N) = 0
      GOTO 1000
C
420   IF (ABS(LOG(ABS(Z))*ABS(Z/ZP)) .LE. PX) THEN
         COL(M,N) = -1
      ELSE
      IF (IMAG(Z) .GE. 0) THEN
         COL(M,N) = 1 + 5*I
      ELSE
         COL(M,N) = 4 + 5*I
      ENDIF
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
1000      CONTINUE
C
2000  CONTINUE
C
      RETURN
      END
