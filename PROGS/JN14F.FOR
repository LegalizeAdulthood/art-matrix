      SUBROUTINE JN14F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,RCOEFF(0:20),RROOTS(1:20)
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,V,P,PP
      COMPLEX*16  COEFF(0:10),ROOTS(10)
C
      COMMON /INPUT/ RCOEFF,RROOTS,NROOT
      EQUIVALENCE (RCOEFF(0),COEFF(0))
      EQUIVALENCE (RROOTS(1),ROOTS(1))

C
C***********************************************************************
C***********************************************************************
C
      MODULO = NROOT + 1
C
      EPS    =  1.E-3
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
            Z = DCMPLX(X,Y)
C
            DO 400 I = 1,NITER
C
C              P  = Z*Z*Z + A*Z*Z + B*Z + C
C
               P = COEFF(0)
               DO 410 J = 1,NROOT
410            P = P*Z + COEFF(J)
C
C              PP = 3*Z*Z + 2*A*Z + B
C
               PP = NROOT*COEFF(0)
               DO 420 J = 1,NROOT-1
420            PP = PP*Z + (NROOT-J)*COEFF(J)
C
               Z = Z - (P/PP)
C
               DO 460 J = 1,NROOT
                  V = Z - ROOTS(J)
                  IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) THEN
                     COL(M,N) = J + MODULO*I
                     GOTO 1000
                  ENDIF
460            CONTINUE
C
400         CONTINUE
C
            COL(M,N) = 0
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
