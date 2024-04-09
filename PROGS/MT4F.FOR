      SUBROUTINE MT4F (XL,YL,SIDE,NITER)
C
C     COPYRIGHT 1984  HOMER W. SMITH AND JOHN HUBBARD
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,L,OZ,T
C
       COMMON  /COLORS/ COL
       COMMON  /  ZRZI/ ZR,ZI
      EQUIVALENCE (Z,ZR)
       COMMON  /  TRTI/ TR,TI
      EQUIVALENCE (T,TR)
       COMMON  /    XY/ X,Y
      EQUIVALENCE (L,X)
C
C
C***********************************************************************
C***********************************************************************
C
      ESCAPE  =  10000.
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
C***********************************************************************
C***********************************************************************
C
      Z = 0
C
      DO 300 I = 1,100
300   Z = Z*Z + L
C
      D  = 1
      DO 400 I = 1,NITER
C
         Z = Z*Z + L
C
         D = D*ABS(Z+Z)
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
