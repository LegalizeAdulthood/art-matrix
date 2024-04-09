      SUBROUTINE MN34F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C1,C2,C3,C,ROOTS(4),V
C
C
C***********************************************************************
C***********************************************************************
C
      C1 = DCMPLX(2D00,0D00)
      C2 = DCMPLX(1D00,0D00)
      C3 = DCMPLX(1D00,0D00)
C
      CALL SLVCUB (C1,C2,C3,ROOTS)
      ROOTS(4) = 0D00
C
      EPSLON    =  1.E-6
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
C
      Y = YLOW
      DO 2000  M = 0,400
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
         C = DCMPLX(X,Y)
C
         DO 3000 I = 1,NITER
C
         C = C - (C*C*C*C + 2*C*C*C + C*C + C) /
     *           (4*C*C*C + 6*C*C   + 2*C + 1)
C
            V = C - ROOTS(4)
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSLON) GOTO 100
            V = C - ROOTS(1)
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPSLON) GOTO 200
            V = C - ROOTS(2)
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPSLON) GOTO 300
            V = C - ROOTS(2)
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPSLON) GOTO 400
C
3000     CONTINUE
C
         COL(M,N) = 0
         GOTO 1000
C
100      COL(M,N) = 1 + 5*I
         GOTO 1000
C
200      COL(M,N) = 2 + 5*I
         GOTO 1000
C
300      COL(M,N) = 3 + 5*I
         GOTO 1000
C
400      COL(M,N) = 4 + 5*I
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
C
C***********************************************************************
C***********************************************************************
C
      SUBROUTINE SLVCUB (A,B,C,Z)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
         REAL*8  TPI,EPSILN,PR,PI
      COMPLEX*16   A,B,C,Z(3),U(3),V(3),P,Q,T
C
C***********************************************************************
C***********************************************************************
C
      TPI     =  6.28318530717958E00
      EPSILN  =  1.0E-24
C
      P = B - A*A/3
      Q = 2*A*A*A/27 - A*B/3 + C
C
      IF     (REAL(P)*REAL(P) + IMAG(P)*IMAG(P) .GE. EPSILN) THEN
         T = .5D00*(-Q + CDSQRT(Q*Q + 4*P*P*P/27))
      ELSEIF (REAL(Q)*REAL(Q) + IMAG(Q)*IMAG(Q) .GE. EPSILN) THEN
         T = -Q
      ELSE
         DO 10 I = 1,3
10       Z(I) = -A/3
         GOTO 9999
C
      ENDIF
C
      DO 20 K = 1,3
         V(K) = EXP( (LOG(T) + TPI*DCMPLX(0D00,DFLOAT(K)) )/3)
         U(K) = V(K) - P/(3*V(K))
         Z(K) = U(K) - A/3
20    CONTINUE
C
C***********************************************************************
C***********************************************************************
C
9999  RETURN
      END
