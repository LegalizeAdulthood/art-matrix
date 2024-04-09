      SUBROUTINE JPZ4F (XL,YL,SIDE,U,NITER,LR,LI,COL,ICOUNT)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,U,LR,LI
C
         REAL*8  DERV(0:300)
      INTEGER*4     COL(0:400,0:400)
      INTEGER*4    TREE(0:300),DEPTH,PNT0,PNT1,IU
      COMPLEX*16   Z(0:300),FXDPTS(2),T,AC,BC
C
C
C***********************************************************************
C***********************************************************************
C
      DO 10 J = 0,400
      DO 10 I = 0,400
10    COL(I,J) = 0
C
      T = DCMPLX (LR,LI)
      AC = -.75 - T*T
      BC = T*T - .25
C
      DEPTH = 300
      IU = U
C
      DO 2000 I = 0,DEPTH
         TREE(I)  = 1
2000  CONTINUE
C
      PNT0   =  0
      PNT1   =  1
C
         Z(0) =  SQRT(-AC/3)
      DERV(0) =  1
C
      CPU = 400/SIDE

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
      IX = ( REAL(Z(PNT0)) - XL)*CPU
      IY = (IMAG(Z(PNT0)) - YL)*CPU
C
      IF (IX .LT. 0 .OR. IX .GT. 400) GOTO 2105
      IF (IY .LT. 0 .OR. IY .GT. 400) GOTO 2105
C
      COL(IY,IX) = 7
C
2105  CALL SLVCBK
     * (-1.5D00*Z(PNT0),(0D00,0D00),
     * .5D00*(-BC-AC*Z(PNT0)),Z(PNT1),TREE(PNT0))
C
      DERV(PNT1) = DERV(PNT0)*
     *    ABS((Z(PNT1)*Z(PNT1)*Z(PNT1) +
     *    AC*Z(PNT1) + BC)*(6*Z(PNT1))/
     *    ((3*Z(PNT1)*Z(PNT1) + AC)**2))
C
      TREE(PNT0) =   TREE(PNT0) + 1
      PNT0       =   PNT1
      PNT1       =   PNT1 + 1
C
      IF (ICOUNT .GT. IMAX) GOTO 2400
C
      IF ( POIN .LT. DEPTH  .AND.
     *     (1 + ( REAL(Z(PNT0))*REAL(Z(PNT0)) +
     *            IMAG(Z(PNT0))*IMAG(Z(PNT0)) ) ) *
     *      DERV(PNT0) .LE. U ) GOTO 2100
C
C          RETREAT
C
2300  PNT1 = PNT0
      PNT0 = PNT0 - 1
      IF (     PNT0  .EQ. 0) GOTO 2400
      IF (TREE(PNT0) .LT. 4) GOTO 2100
      TREE(PNT0) = 1
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
C
C***********************************************************************
C***********************************************************************
C
      SUBROUTINE SLVCBK (A,B,C,Z,K)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
         REAL*8   TPI,EPSILN,PR,PI
      COMPLEX*16  A,B,C,Z,U,V,P,Q,T
C
C***********************************************************************
C***********************************************************************
C
      TPI     =  6.283185307179586D00
      EPSILN  =  1.0D-24
C
      P = B - A*A/3
      Q = 2*A*A*A/27 - A*B/3 + C
C
      IF (REAL(P)*REAL(P) + IMAG(P)*IMAG(P) .GE. EPSILN) THEN
         T = .5D00*(-Q + SQRT(Q*Q + 4*P*P*P/27))
      ELSE
         T = -Q
      ENDIF
C
      V = EXP( (LOG(T) + TPI*DCMPLX(0D0,DFLOAT(K)) )/3)
      U = V - P/(3*V)
      Z = U - A/3
C
      RETURN
      END
