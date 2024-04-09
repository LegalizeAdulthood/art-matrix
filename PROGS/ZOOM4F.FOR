      SUBROUTINE ZOOM4F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,Z2,L,L2,LM5,LP5,A,B,V
C
C***********************************************************************
C***********************************************************************
C
      EPSLON    =  1.E-6
C
      DO 2000 IY = 0,400
C
         Y = YL + (IY*SIDE)/400
C
         CALL SHOWIY (IY,Y)
C
         DO 1000 IX = 0,400
C
            X = XL + (IX*SIDE)/400
C
C***********************************************************************
C***********************************************************************
C
            L = DCMPLX(X,Y)
C
            L2  =     L*L
            A   =     ( .25,0) - L2
            B   =     (-.75,0) - L2
            LM5 = L - (.5,0)
            LP5 = L + (.5,0)
C
C***********************************************************************
C***********************************************************************
C
         Z = 0
C
         DO 400 I = 1,NITER
         Z2 = Z*Z
C
            Z  =  (2*Z*Z2 + A)/(3*Z2 + B)
C
            V = Z - (1,0)
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPSLON) GOTO 100
            V = Z - LM5
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPSLON) GOTO 200
            V = Z + LP5
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE .EPSLON) GOTO 300
C
400      CONTINUE
C
         COL(IX,IY) = 3
         GOTO 1000
C
100      COL(IX,IY) = 1 + 5*I
         GOTO 1000
C
200      COL(IX,IY) = 4 + 5*I
         GOTO 1000
C
300      COL(IX,IY) = 2 + 5*I
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
