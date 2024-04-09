      SUBROUTINE JN24F (XL,YL,SIDE,NITER,CR,CI,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,CR,CI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  C,Z,V,R1,R2
C
C***********************************************************************
C***********************************************************************
C
      CALL XUFLOW (0)
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(CR,CI)
C
      R1 =  SQRT(-C)
      R2 = -SQRT(-C)
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
               Z  =  (Z*Z - C)/(2*Z)

               V = Z - R1
            IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPS) GOTO 200
               V = Z + R2
            IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPS) GOTO 300
C
400         CONTINUE
C
            COL(M,N) = 3
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
