      SUBROUTINE JR44F (XL,YL,SIDE,NITER,CR,CI,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,CR,CI
C
      INTEGER*4   COL(0:400,0:400)
      INTEGER*4   PENP(0:3),PENN(0:3)
      COMPLEX*16  Z,A,AZ,B,C,CMCC,D,ALPHA
C
C
         DATA  PENP/1, 2, 3, 4/
         DATA  PENN/9,10,11,12/
C
C***********************************************************************
C***********************************************************************
C
      C       =   DCMPLX(CR,CI)
C
      CMCC    =   C - C*C
C
      A       =   (2 - C)/CMCC
      B       =  -(CMCC + 1)/CMCC
C
      ALPHA   =   1/(  C*C * (B + B + B*B/A) * (2*A*C + B) )
      ESCAPE  =   4/ABS(ALPHA)
      ESCAPE  =   ESCAPE*ESCAPE
      EPSILN  =   .000001/ESCAPE
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
      DER  =   1.0
      Z    =   DCMPLX(X,Y)
      Z    =   1/(A*Z*Z + B*Z + 1)
      Z    =   1/(A*Z*Z + B*Z + 1)
      Z    =   1/(A*Z*Z + B*Z + 1)
      Z    =   1/(A*Z*Z + B*Z + 1)
C
      DO 400 I = 5,NITER
C
         AZ   =  A*Z
         Z    =  1/(AZ*Z + B*Z + 1)
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 200
C
         D    =   (2*AZ + B)*Z*Z
         DER  =   DER*(REAL(D)*REAL(D) + IMAG(D)*IMAG(D))
         IF (DER .LT. EPSILN) GOTO 300
C
400   CONTINUE
C
      COL(M,N) = 7
      GOTO 1000
C
200   IF (IMAG(ALPHA*Z) .LE. 0) THEN
         COL(M,N) = PENP(MOD(I,4)) + 13*I
         GOTO 1000
      ELSE
         COL(M,N) = PENN(MOD(I,4)) + 13*I
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
