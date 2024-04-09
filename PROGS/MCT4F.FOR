      SUBROUTINE MCT4F (THL,ZL,SIDETH,SIDEZ,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  THL,ZL,SIDETH,SIDEZ
      COMPLEX*16 Z,C
C
      INTEGER*4   COL(0:400,0:400)
C
C
C***********************************************************************
C***********************************************************************
C
      DO 100 J = 0,400
      DO 100 I = 0,400
100   COL(I,J) = 0
C
      ESCAPE = 2
C
      CPUZ = 400./SIDEZ
C
      DELTA  =  SIDETH/400.
      THLOW   =  THL - DELTA
C
         TH = THLOW
         DO 1000 N = 0,400
C
            TH = TH + DELTA
C           C = 1.02*EXP(DCMPLX(0D0,TH))/2 - EXP(DCMPLX(0D0,2*TH))/4
            C = DCMPLX(-.123D00,TH)
C
C***********************************************************************
C***********************************************************************
C
      Z = 0
      DO 410 I = 1,NITER
      IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. 100) GOTO 1000
         IZ = (IMAG(Z) - ZL)*CPUZ
         IF (IZ .LT. 0 .OR. IZ .GT. 400) GOTO 420
         COL(IZ,N) = I
420      Z = Z*Z + C
410   CONTINUE
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

      RETURN
      END
