      SUBROUTINE LOR4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
C
      DIMENSION X(3),T(3),M1(3),M2(3),M3(3),M4(3)
      REAL*8 M1, M2, M3, M4
C
C***********************************************************************
C***********************************************************************
C
      DO 7 I = 0,400
      DO 7 J = 0,400
7     COL(I,J) = 0
C
      CPU = 400./SIDE
C
      A  =  10
      C  =  8./3.
      B  =  28
      H  = .0005
C
      X(1) = .00001
      X(2) = .00001
      X(3) = .00001
C
C***********************************************************************
C***********************************************************************
C
      DO 400 I = 1,NITER
C
         CALL F(X,M1,A,B,C,H)
C
         T(1) = X(1) + M1(1)/2
         T(2) = X(2) + M1(2)/2
         T(3) = X(3) + M1(3)/2
         CALL F(T,M2,A,B,C,H)
C
         T(1) = X(1) + M2(1)/2
         T(2) = X(2) + M2(2)/2
         T(3) = X(3) + M2(3)/2
         CALL F(T,M3,A,B,C,H)
C
         T(1) = X(1) + M3(1)
         T(2) = X(2) + M3(2)
         T(3) = X(3) + M3(3)
         CALL F(T,M4,A,B,C,H)
C
         X(1) = X(1) + (M1(1) + 2*M2(1) + 2*M3(1) + M4(1))/6
         X(2) = X(2) + (M1(2) + 2*M2(2) + 2*M3(2) + M4(2))/6
         X(3) = X(3) + (M1(3) + 2*M2(3) + 2*M3(3) + M4(3))/6
C
         IX = (X(1) - XL)*CPU
         IY = (X(3) - YL)*CPU
C
         IF (IX .LT. 0 .OR. IX .GT. 400     .OR.
     *       IY .LT. 0 .OR. IY .GT. 400)   GOTO 400
C
         COL(IX,IY) = I
C
400    CONTINUE
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
      SUBROUTINE F(X,M,A,B,C,H)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X(3),M(3)
      REAL*8 M
C
      M(1)  =  A*(X(2) - X(1))
      M(2)  =  B*X(1) - X(2) - X(1)*X(3)
      M(3)  = -C*X(3) + X(1)*X(2)
C
      M(1)  =  H*M(1)
      M(2)  =  H*M(2)
      M(3)  =  H*M(3)
C
      RETURN
      END
