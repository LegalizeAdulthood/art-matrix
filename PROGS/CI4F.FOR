      SUBROUTINE CI4F (XL,YL,SIDE,NITER,THETA,SHRINK,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,THETA,SHRINK
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,A,B,AA3,A2,L,U
      LOGICAL  WHICH
C
C
C***********************************************************************
C***********************************************************************
C
      TPI     =  2*3.141592653589793
      L       =  SHRINK*EXP(DCMPLX(0D0,TPI*THETA))
C
      SQRT3   =  3**.5
      SQRT3M  = -SQRT3
C
      ESCAPE  =  100.0
      WHICH   = .FALSE.
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
            U  =  DCMPLX(X,Y)
C
            A  =  U/4 - L/(3*U)
            Z  =  U/4 + L/(3*U)
            B  = -Z*Z*Z + 3*A*A*Z + Z
C
            AA3 = A*A*3
            A2  = A + A
C
C***********************************************************************
C***********************************************************************
C
      IF (WHICH) GOTO 500
C
C***********************************************************************
C***********************************************************************
C
400   Z = A
C
      DO 410 I = 1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 420
C
410   CONTINUE
C
      IF (.NOT. WHICH) GOTO 500
C
         COL(M,N) =  0
         GOTO 1000
C
420   IF (REAL(Z) .GE. 0) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(M,N) = 1 + 13*I
         ELSE
            COL(M,N) = 4 + 13*I
         ENDIF
      ELSE
         ZIZR = IMAG(Z)/REAL(Z)
         IF (ZIZR .LE. SQRT3M) THEN
            COL(M,N) = 1 + 13*I
         ELSEIF   (ZIZR .GT. SQRT3) THEN
            COL(M,N) = 4 + 13*I
         ELSE
            COL(M,N) = 2 + 13*I
         ENDIF
      ENDIF
C
      WHICH  =  .FALSE.
      GOTO 1000
C
C***********************************************************************
C***********************************************************************
C
500   Z = -A
C
      DO 510 I = 1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 520
C
510   CONTINUE
C
      IF (WHICH) GOTO 400
C
         COL(M,N) =  0
         GOTO 1000
C
520   IF (REAL(Z) .GE. 0) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(M,N) = -(9 + 13*I)
         ELSE
            COL(M,N) = -(12 + 13*I)
         ENDIF
      ELSE
         ZIZR = IMAG(Z)/REAL(Z)
         IF (ZIZR .LE. SQRT3M) THEN
            COL(M,N) = -(9 + 13*I)
         ELSEIF   (ZIZR .GT. SQRT3) THEN
            COL(M,N) = -(12 + 13*I)
         ELSE
            COL(M,N) = -(10 + 13*I)
         ENDIF
      ENDIF
C
      WHICH  =  .TRUE.
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
