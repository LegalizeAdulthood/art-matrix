      SUBROUTINE CANT4F (XL,YL,SIDE,NITER,LR,LI,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,LR,LI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  L,L2,LP5,LM5,Z,Z2,A,B,V
C
C
C***********************************************************************
C***********************************************************************
C
      L = DCMPLX(LR,LI)
C
      LM5 = L - (.5,0)
      LP5 = L + (.5,0)
C
      EPSILN  =   1.E-6
      T7P6    =   .5773502691896246
      T7P6M   =  -.5773502691896246
      A       =   (.25,0) - L*L
      B       =  -(.75,0) - L*L
C
      DELTA =  SIDE/400.
      XLOW  =  XL - DELTA
      YLOW  =  YL - DELTA
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
            Z = DCMPLX(X,Y)
C
C***********************************************************************
C***********************************************************************
C
            DO 400 I = 1,NITER
C
               Z2 = Z*Z
               Z  = (2*Z*Z2 + A)/(3*Z2 + B)
C
C***********************************************************************
C***********************************************************************
C
               V = Z - (1,0)
               IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSILN) THEN
C
                  IF (IMAG(Z) .GE. 0) THEN
                     COL(M,N)  =  1 + 8*I
                     GOTO 1000
                  ELSE
                     COL(M,N)  =  2 + 8*I
                     GOTO 1000
                  ENDIF
C
               ENDIF
C
C***********************************************************************
C***********************************************************************
C
               V = Z - LM5
               IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSILN) THEN
C
                  IF (REAL(Z) .NE. 0) THEN
C
                     ZIZR  =  IMAG(Z)/REAL(Z)
                     IF (ZIZR .LE. T7P6M) THEN
                        COL(M,N)  =  3 + 8*I
                        GOTO 1000
                     ELSEIF (ZIZR .GT. T7P6) THEN
                        COL(M,N)  =  6 + 8*I
                        GOTO 1000
                     ELSE
                        COL(M,N)  =  5 + 8*I
                        GOTO 1000
                     ENDIF
C
                  ELSEIF (IMAG(Z) .GE. 0) THEN
                     COL(M,N)  =  6 + 8*I
                     GOTO 1000
                  ELSE
                     COL(M,N)  =  5 + 8*I
                     GOTO 1000
C
                  ENDIF
C
               ENDIF
C
C***********************************************************************
C***********************************************************************
C
               V = Z + LP5
               IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSILN) THEN
C
                  IF (IMAG(Z) .GE. 0) THEN
                     COL(M,N)  =  4 + 8*I
                     GOTO 1000
                  ELSE
                     COL(M,N)  =  7 + 8*I
                     GOTO 1000
                  ENDIF
C
               ENDIF
C
C***********************************************************************
C***********************************************************************
C
400         CONTINUE
            COL(M,N) = 0
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE
C
2000  CONTINUE
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
