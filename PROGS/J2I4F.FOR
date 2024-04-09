      SUBROUTINE J2I4F (XL,YL,SIDE,NITER,AR,AI,COL)
C
         IMPLICIT REAL*8 (A-H,O-Z)
C
         REAL*8 XL,YL,SIDE,AR,AI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  A,B,C,Z,V,P,PP
      COMPLEX*16  Z0,Z1,Z2,Z3,Z4,Z5,Z6,ZP,ZLAST
C
C***********************************************************************
C***********************************************************************
C
      CALL NOERR
C
      A = DCMPLX(AR,AI)
      WRITE (6,*) A
C
      EPS    =  1.E-6
C
C***********************************************************************
C***********************************************************************
C
      DO 2000 IY = 0,400
C
         Y = YL + (IY*SIDE)/400
C
         DO 1000 IX = 0,400
C
            X = XL + (IX*SIDE)/400
C
C***********************************************************************
C***********************************************************************
C
      ZLAST = 1E10
C
      Z0 = DCMPLX(X,Y)
C
      DO 400 I = 1,NITER
C
         Z1 = Z0*Z0 + A
         Z2 = Z1*Z1 + A
C
         ZP = 2**2*Z1*Z0
C
         Z0 = Z0 - (Z2 - Z0)/(ZP - 1)
C
         V = Z0 - ZLAST
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) GOTO 800
C
         ZLAST = Z0
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
420   COL(IX,IY) = -1
      GOTO 1000
C
800   COL(IX,IY) = COL(IX,IY) + 2*I
C
      JX = (REAL(Z0) - XL)*400/SIDE
      JY = (IMAG(Z0) - YL)*400/SIDE
C
      IF (JX .LT. 0 .OR. JX .GT. 400 .OR.
     *    JY .LT. 0 .OR. JY .GT. 400) GOTO 1000
C
      COL(JX,JY) = 1 + 2*(COL(JX,JY)/2)
C
      GOTO 1000
C
C***********************************************************************
C***********************************************************************
C
1000  CONTINUE

2000  CONTINUE
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
