      SUBROUTINE BERK4F (XL,YL,SIDE,NITER,CR,CI,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,CR,CI
C
         REAL*8  ARG,R,THETA,PI25,R20
      INTEGER*4   COL(0:400,0:400),IB32,NB32(31)
      COMPLEX*16  Z,C,E,Z2,E32
C
      DATA NB32/1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,
     * 3,3,4,2,3,3,4,3,4,4,5/
C
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(CR,CI)
C
      PI25    =  6.283185307179586D00/5
      ESCAPE  =  4.
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
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
C***********************************************************************
C***********************************************************************
C
      E = DCMPLX(X,Y)
C
      E32   = 3*E/2
      R     = ABS(E32)
      THETA = ARG(REAL(E32),IMAG(E32))/5
      R20   = R**.20
C
C***********************************************************************
C***********************************************************************
C
      IB32     = 0
      COL(M,N) = 0
C
      DO 500 J = 0,4
C
      Z = R20*EXP(DCMPLX(0D00,THETA))
      DO 400 I = 1,NITER
C
         Z2 = Z*Z
         Z = Z2 + C + E/(Z*Z2)
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
      GOTO 500
C
420   COL(M,N) = COL(M,N) + I
      IB32     = IB32 + 2**J
C
      THETA = THETA + PI25
C
500   CONTINUE
C
      IF (IB32 .EQ. 0) GOTO 1000
C
      COL(M,N) = COL(M,N)/NB32(IB32)
      COL(M,N) = IB32 + 32*COL(M,N)
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C***********************************************************************
C
      FUNCTION ARG (X,Y)
C
      REAL*8 X,Y,ARG
C
      IF (X .GT. 0 .AND. ABS(Y) .LE. X) THEN
         ARG = ATAN(Y/X)
         IF (Y .LT. 0) ARG = ARG + 6.283185307179586D00
C
      ELSEIF (Y .GT. 0 .AND. ABS(X) .LE. Y) THEN
         ARG = ATAN(X/Y)
         ARG = 1.570796326794897D00 - ARG
C
      ELSEIF (X .LT. 0 .AND. ABS(Y) .LE. -X) THEN
         ARG = ATAN(Y/X)
         ARG = ARG + 3.141592653589793D00
C
      ELSE
         ARG = ATAN(X/Y)
         ARG = 4.71238898038469D00 - ARG
C
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
