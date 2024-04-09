      SUBROUTINE Z24F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  A,B,C,Z,V,ROOTS(0:10)
C
C
C***********************************************************************
C***********************************************************************
C
      MODULO = 4
C
      EPS    =  1.E-1
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
            ROOTS(1) = SQRT(-C)
            ROOTS(2) = -ROOTS(1)
C
            Z = 0

            DO 400 I = 1,NITER
C
               Z = Z - (Z*Z + C)/(2*Z)
C
               DO 460 J = 1,2
                  V = Z - ROOTS(J)
                  IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) THEN
                     COL(M,N) = J + MODULO*I
                     GOTO 1000
                  ENDIF
460            CONTINUE
C
400         CONTINUE
C
            COL(M,N) = 3
            GOTO 1000
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
