      SUBROUTINE PIC24F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C,FX
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  100
      EPSILN  = .01
C
      DO 2000 IY = 0,400
C
         Y = YL + (IY*SIDE)/400
C
         CALL SHOWIY (IY,Y)
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
      Z = 0
      DO 400 I = 1,NITER
C
         Z = Z*Z + C
C
         IF (ABS(REAL(Z)) .LT. EPSILN .OR.
     *       ABS(IMAG(Z)) .LT. EPSILN) GOTO 430
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
410   COL(IX,IY) = 0
      GOTO 1000
C
420   COL(IX,IY) = 0
      GOTO 1000
C
430   COL(IX,IY) = I
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
