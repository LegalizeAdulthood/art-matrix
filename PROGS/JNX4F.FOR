      SUBROUTINE JNX4F (XL,YL,SIDE,NITER,COL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,Z1,V,P,PP
      COMPLEX*16  COEFF(0:100),ROOTS(100)
C
      COMMON /INPUT/ COEFF,ROOTS,NROOT
C
C***********************************************************************
C***********************************************************************
C
      MODULO = NROOT + 1
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
            Z = DCMPLX(X,Y)
C
            DO 400 I = 1,NITER
C
               P = COEFF(NROOT)
               DO 410 J = NROOT-1,0
410            P = P*Z + COEFF(J)
C
               PP = NROOT*COEFF(NROOT)
               DO 420 J = NROOT-1,1
420            PP = PP*Z + J*COEFF(J)
C
               Z = Z - P/PP
C
               DO 460 J = 1,NROOT
                  V = Z - ROOTS(J)
                  IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LE. EPS) THEN
                     COL(IX,IY) = J + MODULO*I
                     GOTO 1000
                  ENDIF
460            CONTINUE
C
400         CONTINUE
C
            COL(IX,IY) = 0
            GOTO 1000
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
C***********************************************************************
C***********************************************************************
C
      RETURN
      END
