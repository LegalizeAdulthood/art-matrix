      SUBROUTINE EXP4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE, PI, TPI, ESCAPE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,L
C
C
C***********************************************************************
C***********************************************************************
C
      CALL NOOUF
C
      PI  =  3.141592653589793
      TPI =  2*PI
C
      ESCAPE  =  10.00
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
         CALL SHOWIY (IY,Y)
C
         X = XLOW
         DO 1000 N = 0,400
C
            X = X + DELTA
C
            L = DCMPLX(X,Y)
C
C***********************************************************************
C***********************************************************************
C
            Z = 0
C
            DO 400 I = 1,NITER
C
               Z = L*EXP(Z)
C
400         IF (REAL(Z) .GT. ESCAPE) GOTO 420
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
