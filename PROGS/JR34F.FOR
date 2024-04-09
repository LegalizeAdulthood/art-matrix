      SUBROUTINE JR34F (XL,YL,SIDE,NITER,AR,AI,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,AR,AI
C
      INTEGER*4   COL(0:400,0:400)
      INTEGER*4   PENP(0:2),PENN(0:2)
      COMPLEX*16  Z,A,A1,AZ,D,ALPHA
C
C
         DATA  PENP/1, 2, 4/
         DATA  PENN/9,10,12/
C
C***********************************************************************
C***********************************************************************
C
      A       =  DCMPLX(AR,AI)
      A1      =  A + 1
C
      ALPHA   =  A/(A*A - 1)
      ESCAPE  =  4/ABS(ALPHA)
      ESCAPE  =  ESCAPE*ESCAPE
      EPSILN  =  .000001/ESCAPE
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
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
      DER =  1.0
C
      Z   =  DCMPLX(X,Y)
      Z   =  1/(A*Z*Z - A1*Z + 1)
      Z   =  1/(A*Z*Z - A1*Z + 1)
      Z   =  1/(A*Z*Z - A1*Z + 1)
C
      DO 400 I = 4,NITER
C
         AZ   =  A*Z
         Z    =  1/(AZ*Z - A1*Z + 1)
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 200
C
         D    =   (2*AZ - A1)*Z*Z
         DER  =   DER*(REAL(D)*REAL(D) + IMAG(D)*IMAG(D))
         IF (DER .LT. EPSILN) GOTO 300
C
400   CONTINUE
C
      COL(M,N) = 7
      GOTO 1000
C
200   IF (IMAG(ALPHA*Z) .LE. 0) THEN
         COL(M,N) = PENP(MOD(I,3)) + 13*I
         GOTO 1000
      ELSE
         COL(M,N) = PENN(MOD(I,3)) + 13*I
         GOTO 1000
      ENDIF
C
300   COL(M,N) = 13*I
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
