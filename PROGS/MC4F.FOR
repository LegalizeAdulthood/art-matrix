      SUBROUTINE MC4F (CL,ZL,SIDEC,SIDEZ,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  CL,ZL,SIDEC,SIDEZ
C
      INTEGER*4   COL(0:400,0:400)
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
      DELTA  =  SIDEC/400.
      CLOW   =  CL - DELTA
C
         C = CLOW
         DO 1000 N = 0,400
C
            C = C + DELTA
C
C***********************************************************************
C***********************************************************************
C
      Z = 0
      DO 410 I = 1,NITER
      IF (ABS(Z) .GT. 10000) GOTO 1000
         IZ = (Z - ZL)*CPUZ
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
