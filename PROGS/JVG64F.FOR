      SUBROUTINE JVG64F (XL,YL,SIDE,NITER,R1,R2,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,R1,R2
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,CZ
C
C
C***********************************************************************
C***********************************************************************
C
      A = R1
      B = R2
      C = .5*(A*B - SQRT((4-A*A)*(4-B*B)))
C
      EPSILN  =  .0025
      ESCAPE  =  10000
      SQRT3   =  3**.5
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
         ABSZ = (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z))
         CZ   =  CONJG(Z)
C
         Z = Z**5 + ((1 + C*B - A)*Z + C*CZ)*(1 - ABSZ)**2 -
     *       ((C + B)*Z**3 + (A + 1)*Z*Z*CZ + B*Z*CZ*CZ +
     *       CZ*CZ*CZ)*(1 - ABSZ)
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .LT. EPSILN) GOTO 420
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 430
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
420   COL(IX,IY) =  I
      GOTO 1000
C
430   COL(IX,IY) = -I
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
