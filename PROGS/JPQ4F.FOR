      SUBROUTINE JPQ4F (XL,YL,SIDEX,SIDEY,U,NITER,CR,CI,COL,ICOUNT)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDEX,SIDEY,U,CR,CI
C
C     PARAMETER (400 = 400)

         REAL*8   DERV(0:300)
      INTEGER*4     COL(0:400,0:400)
      INTEGER*4    TREE(0:300),DEPTH,PNT0,PNT1,IU
      COMPLEX*16   Z(0:300),FXDPTS(2),C
C
C
C***********************************************************************
C***********************************************************************
C
      DO 10 J = 0,400
      DO 10 I = 0,400
10    COL(I,J) = 0
C
      C = DCMPLX (CR,CI)
C
      DEPTH = 300
      IU = U
C
      DO 2000 I = 0,DEPTH
         TREE(I)  = -1
2000  CONTINUE
C
      PNT0   =  0
      PNT1   =  1
C
      FXDPTS(1)  =  (1 + CDSQRT(1 - 4*C))/2
      FXDPTS(2)  =  (1 - CDSQRT(1 - 4*C))/2
C
         Z(0) =  FXDPTS(1)
      DERV(0) =  1
C
      CPU = 400/SIDEX

      ICOUNT = 0
      IMAX = NITER
C
C***********************************************************************
C***********************************************************************
C
C          ADVANCE
C
2100  ICOUNT = ICOUNT + 1
C
      IX = (REAL(Z(PNT0)) - XL)*CPU
      IY = (IMAG(Z(PNT0)) - YL)*CPU
C
      IF (IX .LT. 0 .OR. IX .GT. 400) GOTO 2105
      IF (IY .LT. 0 .OR. IY .GT. 400) GOTO 2105
C
      COL(IY,IX) = 7
C
2105     Z(PNT1) =   TREE(PNT0)*CDSQRT(Z(PNT0) - C)
      DERV(PNT1) =   DERV(PNT0)*2*ABS(Z(PNT1))
      TREE(PNT0) =   TREE(PNT0) + 2
      PNT0       =   PNT1
      PNT1       =   PNT1 + 1
C
      IF (ICOUNT .GT. IMAX) GOTO 2400
C
      IF       ((PNT0 .LT. DEPTH) .AND.
     *    (DERV(PNT0) .LE.    IU))      GOTO 2100
C
C          RETREAT
C
2300  PNT1 = PNT0
      PNT0 = PNT0 - 1
      IF (     PNT0  .EQ. 0) GOTO 2400
      IF (TREE(PNT0) .LT. 2) GOTO 2100
      TREE(PNT0) = -1
      GOTO 2300
C
C          END RETREAT
C
2400  CONTINUE
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
