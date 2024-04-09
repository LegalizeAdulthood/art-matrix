      SUBROUTINE JNN4F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,RCOEFF(0:20),RROOTS(1:20)
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  A,B,C,Z,Z1,V,P,PP,T1,T2
      COMPLEX*16  COEFF(0:10),ROOTS(10)
C
      COMMON /INPUT/ RCOEFF,RROOTS,NROOT
      EQUIVALENCE (RCOEFF(0),COEFF(0))
      EQUIVALENCE (RROOTS(1),ROOTS(1))

C
C***********************************************************************
C***********************************************************************
C
      MODULO = NROOT + 1
C
      PI2N    =  6.283185307179586D00/NROOT
C
      EPS    =  1.E-6
C
      DELTA  =  SIDE/400.
      XLOW   =  XL - DELTA
      YLOW   =  YL - DELTA
C
      Y = YLOW
      DO 2000  M = 0,400
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
            Z = DCMPLX(X,Y)
C
            DO 400 I = 1,NITER
C
C              P  = Z*Z*Z + A*Z*Z + B*Z + C
C
               P = COEFF(0)
               DO 410 J = 1,NROOT
410            P = P*Z + COEFF(J)
C
C              PP = 3*Z*Z + 2*A*Z + B
C
               PP = NROOT*COEFF(0)
               DO 420 J = 1,NROOT-1
420            PP = PP*Z + (NROOT-J)*COEFF(J)
C
               Z1 = Z**NROOT - (P/PP)*NROOT*Z**(NROOT-1)
      IF (REAL(Z1)*REAL(Z1) + IMAG(Z1)*IMAG(Z1) .GT. 1D10 ) GOTO 800
C
               R  = ABS(Z1)**(1./NROOT)
               TH = ARG(REAL(Z1),IMAG(Z1))/NROOT
C
               T1 = R*EXP(DCMPLX(0D00,TH))
C
               DO 450 J = 1,NROOT - 1
C
                  TH = TH + PI2N
                  T2 = R*EXP(DCMPLX(0D00,TH))
                  IF (CDIFF(Z,T2) .LT. CDIFF(Z,T1)) T1 = T2
C
450            CONTINUE
C
               Z = T1
C
               DO 460 J = 1,NROOT
                  V = Z - ROOTS(J)
                  IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) THEN
                     COL(M,N) = J + MODULO*I
                     GOTO 1000
                  ENDIF
460            CONTINUE
C
400         CONTINUE
C
800         COL(M,N) = 0
            GOTO 1000
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C***********************************************************************
C
      FUNCTION CDIFF (X,Y)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
         REAL*8   CDIFF
      COMPLEX*16  X,Y,Z
C
      Z = X - Y
C
      CDIFF = (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z))
C
      RETURN
      END
C
C***********************************************************************
C***********************************************************************
C
      FUNCTION ARG (X,Y)
C
      REAL*8 X,Y,ARG
C
      IF (X .GT. 0 .AND. ABS(Y) .LE. X) THEN
         ARG = ATAN(Y/X)
         IF (Y .LT. 0) ARG = ARG + 6.283185307179586D00
C
      ELSEIF (Y .GT. 0 .AND. ABS(X) .LE. Y) THEN
         ARG = ATAN(X/Y)
         ARG = 1.570796326794897D00 - ARG
C
      ELSEIF (X .LT. 0 .AND. ABS(Y) .LE. -X) THEN
         ARG = ATAN(Y/X)
         ARG = ARG + 3.141592653589793D00
C
      ELSE
         ARG = ATAN(X/Y)
         ARG = 4.71238898038469D00 - ARG
C
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
