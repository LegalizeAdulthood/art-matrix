      SUBROUTINE JPN4F (XL,YL,SIDE,CR,CI,NITER,ZN,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,CR,CI,ZN
C
         REAL*8  ARG
      INTEGER*4   COL(0:400,0:400),PENC(0:1)
      COMPLEX*16  Z,C
C
C
         DATA  PENC/1,4/
C
C***********************************************************************
C***********************************************************************
C
      C      =   DCMPLX(CR,CI)
C
      ZNPI   =   ZN/(2*3.141592653589793)
C
      ESCAPE =  1.E8
C
      DELTA  =   SIDE/400.
      XLOW   =   XL - DELTA
      YLOW   =   YL - DELTA
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
            Z = DCMPLX(X,Y)
C
      DO 400 I = 1,NITER
C
         Z     =  C*(1 + Z/ZN)**ZN
         ZRPN  =  REAL(Z) + ZN
C
400   IF (ZRPN*ZRPN + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 420
C
         COL(M,N) = 0
         GOTO 1000
C
420      NA = ZNPI*ARG(ZRPN,IMAG(Z))
C
         IF (NA .EQ. 0) THEN
            COL(M,N) = 7 + 8*I
         ELSE
            COL(M,N) = PENC(MOD(NA,2)) + 8*I
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
C
C***********************************************************************
C***********************************************************************
C
      FUNCTION ARG (X,Y)
C
         REAL*8  X,Y,ARG
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
C
         ARG = ATAN(X/Y)
         ARG = 4.71238898038469D00 - ARG
C
      ENDIF
C
      RETURN
      END
