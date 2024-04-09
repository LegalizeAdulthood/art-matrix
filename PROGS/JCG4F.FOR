      SUBROUTINE JCG4F (XL,YL,SIDE,NITER,AR,AI,THETA,SHRINK,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,THETA,SHRINK,AR,AI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,A,AA,AA3,AAA2,L
C
C
C***********************************************************************
C***********************************************************************
C
      TPI     =  2*3.141592653589793
C
      L       =  SHRINK*SHRINK*SHRINK*EXP(DCMPLX(0D0,TPI*THETA))
      ABSL    =  REAL(L)*REAL(L) + IMAG(L)*IMAG(L)
C
      SQRT3   =  3**.5
      SQRT3M  = -SQRT3
C
      ESCAPE  =  ABSL
C
      A = DCMPLX(AR,AI)
C
      AA   = A*A
      AA3  = AA*3
      AAA2 = AA*A*2
C
C***********************************************************************
C***********************************************************************
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
      DO 410 I = 1,NITER
C
         Z = Z*Z*Z - AA3*Z + L + AAA2
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GO TO 420
C
410   CONTINUE
C
      COL(M,N) =  0
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
      GOTO 1000
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
