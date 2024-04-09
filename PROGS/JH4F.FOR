      SUBROUTINE JH4F (XL,YL,SIDE,NITER,CR,CI,AR,AI,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,CR,CI,AR,AI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C,ALPHA
C
C
C***********************************************************************
C***********************************************************************
C
      ALPHA = DCMPLX(AR,AI)
C
      C = DCMPLX(CR,CI)
C
      ESCAPE  =  100
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
C
      Y = YLOW
      DO 2000 IY = 0,400

         Y = Y + DELTA

         X = XLOW
         DO 1000 IX = 0,400
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
         Z = Z*Z + ALPHA*CONJG(Z) + C
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
420   IF (ABS(IMAG(Z)) .LT. .1*ABS(REAL(Z))) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = 7 + 8*I
         ELSE
            COL(IX,IY) = 7 + 8*I
         ENDIF
      ELSE
         IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = 4 + 8*I
         ELSE
            COL(IX,IY) = 2 + 8*I
         ENDIF
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
