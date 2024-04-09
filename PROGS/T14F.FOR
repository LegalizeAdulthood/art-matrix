      SUBROUTINE T14F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C,FX,Z0,Z1,Z2
C
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  100
C
      DO 2000 IY = 0,400
C
         Y = YL + (IY*SIDE)/400
C
         DO 1000 IX = 0,400
C
            X = XL + (IX*SIDE)/400
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(X,Y)
C
      Z0 = 0
      Z1 = 0
      DO 400 I = 1,NITER
C
         Z2 = Z1*Z1 + Z0 + C
C
         IF (REAL(Z2)*REAL(Z2) + IMAG(Z2)*IMAG(Z2) .GT. ESCAPE)
     &   GOTO 420
C
         Z0 = Z1
         Z1 = Z2
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
420   IF (ABS(IMAG(Z2)) .LT. .1*ABS(REAL(Z2))) THEN
            COL(IX,IY) = 7 + 8*I
      ELSE
         IF (IMAG(Z2) .GE. 0) THEN
            COL(IX,IY) = 1 + 8*I
         ELSE
            COL(IX,IY) = 4 + 8*I
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
