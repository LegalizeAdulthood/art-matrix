      SUBROUTINE JCR24F (XL,YL,SIDE,NITER,A,B,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,A,B
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z
C
C
C***********************************************************************
C***********************************************************************
C
      AA3  =   A*A*3
C
      SQRT3   =   3**.5
      SQRT3M  =  -SQRT3
C
      ESCAPE  =   100
C
      DELTA   =   SIDE/400.0
      XLOW    =   XL - DELTA
      YLOW    =   YL - DELTA
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
         Z = AA3*Z - Z*Z*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 420
C
400   CONTINUE
C
      COL(M,N) = 0
      GOTO 1000
C
420   IF (REAL(Z) .GE. 0) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(M,N) = 1 + 5*I
         ELSE
            COL(M,N) = 4 + 5*I
         ENDIF
      ELSE
         ZIZR = IMAG(Z)/REAL(Z)
         IF (ZIZR .LE. SQRT3M) THEN
            COL(M,N) = 1 + 5*I
         ELSEIF   (ZIZR .GT. SQRT3) THEN
            COL(M,N) = 4 + 5*I
         ELSE
            COL(M,N) = 2 + 5*I
         ENDIF
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE
C
2000  CONTINUE
C
      RETURN
      END
