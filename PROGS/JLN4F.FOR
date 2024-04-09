      SUBROUTINE JLN4F (XL,YL,SIDE,NITER,LR,LI,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,LR,LI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  L,L2,LM5,LP5,A,B,Z,Z2,V
C
C
C***********************************************************************
C***********************************************************************
C
      L = DCMPLX(LR,LI)
C
      L2  =     L*L
      A   =     ( .25,0) - L2
      B   =     (-.75,0) - L2
      LM5 = L - (.5,0)
      LP5 = L + (.5,0)
C
      EPS    =  1.E-6
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
            Z   = DCMPLX(X,Y)
C
            DO 400 I = 1,NITER
C
               Z2 =  Z*Z
               Z  =  (2*Z*Z2 + A)/(3*Z2 + B)

               V = Z - (1,0)
            IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) GOTO 100
               V = Z - LM5
            IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPS) GOTO 200
               V = Z + LP5
            IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPS) GOTO 300
C
400         CONTINUE
C
            COL(M,N) = 3
            GOTO 1000
C
100         COL(M,N) = 1 + 5*I
            GOTO 1000
C
200         COL(M,N) = 4 + 5*I
            GOTO 1000
C
300         COL(M,N) = 2 + 5*I
            GOTO 1000
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
