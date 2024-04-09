      SUBROUTINE JME4F (XL,YL,SIDE,NITER,THETA,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,THETA
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C
C
C
C***********************************************************************
C***********************************************************************
C
      PI  =  3.141592653589793D00
      TPI =  2*PI
C
      C = DCMPLX(0D0,TPI*THETA)
      C = EXP(C)/EXP(EXP(C))
C
      ESCAPE  =  8.0
C
      DELTA   =   SIDE/400.
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
            DO 400 I = 1,NITER
C
               Z = C*EXP(Z)

400            IF (REAL(Z) .GE. ESCAPE) GOTO 420
C
               COL(M,N) = 0
               GOTO 1000

420            COL(M,N) = I
               IF (IMAG(Z) .GE. 0) THEN
                  II = (IMAG(Z) + PI)/TPI
               ELSE
                  II = (IMAG(Z) - PI)/TPI
               ENDIF
               COL(M,N) = INSERT(COL(M,N),48,6,II)
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
