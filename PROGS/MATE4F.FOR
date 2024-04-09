      SUBROUTINE MATE4F (XL,YL,SIDE,NITER,COL)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,L,OZ,T
C
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  10.E20
      EPSILN  =  .01
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
            ABSL = X*X + Y*Y
C
            IF (ABSL .GT. 1.0) THEN
               COL(M,N) = -32766
               GOTO 1000
            ENDIF
C
            L = DCMPLX(X,Y)
C
C***********************************************************************
C***********************************************************************
C
      Z = 1
C
      DO 300 I = 1,100
300   Z = L*(Z + 1/Z)
C
      D  = 1
      OZ = 1/Z
      DO 400 I = 1,NITER
C
         Z = L*(Z + OZ)
C
         OZ = 1/Z
C
         T = 1 - OZ/Z
         D = D*ABSL*(REAL(T)*REAL(T) + IMAG(T)*IMAG(T))
C
         IF (D .LE. EPSILN) GOTO 420
         IF (D .GT. ESCAPE) GOTO 410
C
400   CONTINUE
C
            COL(M,N) =  0
            GOTO 1000
C
410         COL(M,N) = -I
            GOTO 1000
C
420         COL(M,N) =  I
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
