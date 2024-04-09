      SUBROUTINE JP24F (XL,YL,SIDEX,SIDEY,NITER,ALPHA,BETA,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDEX,SIDEY,ALPHA,BETA
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C
C
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  NITER*NITER
C
      DELTAX  =  SIDEX/400.
      DELTAY  =  SIDEY/400.
      XLOW   =  XL - DELTAX
      YLOW   =  YL - DELTAY
C
      YY = YLOW
      DO 2000  IY = 0,400

         YY = YY + DELTAY

         XX = XLOW
         DO 1000 IX = 0,400
C
            XX = XX + DELTAX
C
C***********************************************************************
C***********************************************************************
C
      X = XX
      Y = YY
C
      DO 400 I = 1,NITER
C
         TX = -2*X*X*X + 5*X*Y*Y - X + ALPHA
          Y =  2*Y*Y*Y - 5*X*X*Y - Y + BETA
          X =  TX
C
         IF (X*X + Y*Y .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
420   COL(IX,IY) = I
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
