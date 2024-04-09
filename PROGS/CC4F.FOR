      SUBROUTINE CC4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,V,A,B,AA3,T,T2,T3
C
C
C***********************************************************************
C***********************************************************************
C
      SQRT3   =  3**.5
      SQRT3M  =  -SQRT3
C
      ESCAPE  =  100.0
      EPSLON  = .000001
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
            T = DCMPLX(X,Y)
C
            A = T
            B = T + 2*T*T*T
C
            AA3 = A*A*3
C
C***********************************************************************
C***********************************************************************
C
      Z = -A
C
      DO 400 I=1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 430
C
         V = Z - A
400      IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSLON) GOTO 420
C
         COL(M,N) =  0
         GOTO 1000
C
420      COL(M,N) = -1
         GOTO 1000
C
430   IF (REAL(Z) .GE. 0) THEN
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
