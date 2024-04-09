      SUBROUTINE JTRI4F (XL,YL,XS,YS,NITER,K,W,B,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,XS,YS,K,W,B
C
         REAL*8  KTPI
      INTEGER*4   COL(0:400,0:400)
C
C
C***********************************************************************
C***********************************************************************
C
      TPI  = 2*3.141592653589793D00
      KTPI = K/TPI
C
      ESCAPE = KTPI/(1 - B)
C
      DELTAX =  XS/400
      DELTAY =  YS/400
      XLOW   =  XL - DELTAX
      YLOW   =  YL - DELTAY
C
      Y = YLOW
      DO 2000  IY = 0,400

         Y = Y + DELTAY

         X = XLOW
         DO 1000 IX = 0,400
C
            X = X + DELTAX
C
C***********************************************************************
C***********************************************************************
C
      U = X
      V = Y
C
      DO 400 I = 1,NITER
C
         U  =   U - V - W
         V  =  (V - KTPI*SIN(TPI*U))/B
C
         IF (ABS(V) .GT. ESCAPE) GOTO 420
C
400   CONTINUE
C
            COL(IX,IY) = 0
            GOTO 1000

420   IF (V .GT. 0) THEN
         COL(IX,IY) =  I
      ELSE
         COL(IX,IY) = -I
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE
C
2000  CONTINUE
C
      RETURN
      END
