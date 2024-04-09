      SUBROUTINE JQ24F (XL,YL,SIDEX,SIDEY,NITER,ZR,ZI,WR,WI,COL)
C
C     COPYRIGHT (C) 1989 BY ART MATRIX.  ALL RIGHTS RESERVED.
C        PO 880 ITHACA, NY 14851-0880 USA (607) 277-0959
C           CORNELL NATIONAL SUPERCOMPUTER FACILITY
C       DUPLICATION PERMITTED FOR NON COMMERCIAL PURPOSES.
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 XL,YL,SIDEX,SIDEY,ZR,ZI,WR,WI,RX,RY
C
      INTEGER*4   COL(0:400,0:400)
C
      COMPLEX*16  Z,ZZ,W,VZ,VW,Z0,W0,Z1,W1
C
C***********************************************************************
C***********************************************************************
C
      EPS =  1.E-6
C
      RY  =  (-1 + SQRT(5D00))/2
      RX  =  SQRT(1 - RY*RY)
C
      Z0 = DCMPLX( RX,0D00)
      W0 = DCMPLX( RY,0D00)
C
      Z1 = DCMPLX(-RX,0D00)
      W1 = DCMPLX( RY,0D00)
C
C      WRITE (6,*) R1
C      WRITE (6,*) R2
C
C***********************************************************************
C***********************************************************************
C
      DO 2000 IY = 0,400
C
         Y = YL + (IY*SIDEY)/400
C
         CALL SHOWIY (IY,Y)
C
         DO 1000 IX = 0,400
C
            X = XL + (IX*SIDEX)/400
C
C***********************************************************************
C***********************************************************************
C
      IF (ZR .EQ. 99) THEN
         Z = DCMPLX(X,ZI)
      ELSE
         Z = DCMPLX(ZR,X)
      ENDIF
C
      IF (WR .EQ. 99) THEN
         W = DCMPLX(Y,WI)
      ELSE
         W = DCMPLX(WR,Y)
      ENDIF
C
C      WRITE (6,*) Z
C      WRITE (6,*) W
      DO 400 I = 1,NITER
C
         IF (Z .EQ.   0D00) GOTO 1000
         IF (W .EQ. -.5D00) GOTO 1000
C
         ZZ = Z + (1 - Z*Z + W*(W - 2*Z*Z))/(2*Z*(1 + 2*W))
         W  = W + (1 - W*(1 + W))/(1 + 2*W)
         Z  = ZZ
C      WRITE (6,*) Z
C      WRITE (6,*) W
C
         IF (ABS(REAL(Z)) .GT. 1E6 .OR. ABS(IMAG(Z)) .GT. 1E6 .OR.
     *       ABS(REAL(W)) .GT. 1E6 .OR. ABS(IMAG(W)) .GT. 1E6) GOTO 100

         VZ = Z - Z0
         VW = W - W0
         IF (REAL(VZ)*REAL(VZ) + IMAG(VZ)*IMAG(VZ) +
     *       REAL(VW)*REAL(VW) + IMAG(VW)*IMAG(VW) .LT. EPS) GOTO 200
C
         VZ = Z - Z1
         VW = W - W1
         IF (REAL(VZ)*REAL(VZ) + IMAG(VZ)*IMAG(VZ) +
     *       REAL(VW)*REAL(VW) + IMAG(VW)*IMAG(VW) .LT. EPS) GOTO 300
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
100   COL(IX,IY) = -1
      GOTO 1000
C
200   COL(IX,IY) = 1 + 5*I
      GOTO 1000
C
300   COL(IX,IY) = 2 + 5*I
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
