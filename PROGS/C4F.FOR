      SUBROUTINE C4F (XL,YL,SIDE,NITER,AR,AI,BR,BI,GR,GI,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,AR,AI,BR,BI,GR,GI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  ALPHA,BETA,GAMMA,T,A,B,Z,AA3,GA,GB
      LOGICAL  IFLAG
C
C
C***********************************************************************
C***********************************************************************
C
      ALPHA   =  DCMPLX(AR,AI)
      BETA    =  DCMPLX(BR,BI)
      GAMMA   =  DCMPLX(GR,GI)
C
      GA      =  GAMMA/ALPHA
      GB      =  GAMMA/BETA
C
      IFLAG = ABS(ALPHA) .GT. ABS(BETA)
C
      ESCAPE  =  100.0
C
      DELTA   =  SIDE/400.0
      XLOW    =  XL - DELTA
      YLOW    =  YL - DELTA
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
            IF (IFLAG) THEN
               A = (-BETA*T) + GA
               B = ALPHA*T
            ELSE
               A = -BETA*T
               B = (ALPHA*T) + GB
            ENDIF
C
            AA3 = 3*A*A
C
C***********************************************************************
C***********************************************************************
C
C     FIRST TIME LOOP  Z = -A
C
      Z = -A
C
      DO 400 I = 1,NITER
C
      Z = Z*Z*Z - AA3*Z + B
C
400   IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 420
C
         COL(M,N) = 0
         GOTO 500
C
420      COL(M,N) = I
C
C***********************************************************************
C***********************************************************************
C
C     SECOND TIME LOOP  Z = A
C
500   Z = A
C
      DO 600 I = 1,NITER
C
      Z = Z*Z*Z - AA3*Z + B
C
600   IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 620
C
         GOTO 1000
C
620      COL(M,N) = INSERT(COL(M,N),32,16,I)
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
