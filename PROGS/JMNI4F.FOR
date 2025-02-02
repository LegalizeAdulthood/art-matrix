      SUBROUTINE JMNI4F (XL,YL,SIDE,NITER,CR,CI,IPER,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XL,YL,SIDE,CR,CI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,C,V,PER
C
C
C***********************************************************************
C***********************************************************************
C
      C = DCMPLX(CR,CI)
C
      Z = 0
      DO 50 I = 1,IPER*1000
50    Z = Z*Z + C
      PER = Z
C
      ESCAPE  =  4
      EPSILN  =  1E-6
C
      DO 2000 IY = 0,400
C
         Y = YL + (SIDE*IY)/400
C
         DO 1000 IX = 0,400
C
            X = XL + (SIDE*IX)/400
C
C***********************************************************************
C***********************************************************************
C
      Z = DCMPLX(X,Y)
C
      DO 400 I = 1,NITER
C
         Z = Z*Z + C
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GT. ESCAPE) GOTO 420
C
         V = Z - PER
         IF (REAL(V)*REAL(V) + IMAG(V)*IMAG(V) .LT. EPSILN) GOTO 430
C
400   CONTINUE
C
      COL(IX,IY) = 0
      GOTO 1000
C
420   IF (ABS(IMAG(Z)) .LT. .1*ABS(REAL(Z))) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = 7 + 8*I
         ELSE
            COL(IX,IY) = 7 + 8*I
         ENDIF
      ELSE
         IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = 4 + 8*I
         ELSE
            COL(IX,IY) = 2 + 8*I
         ENDIF
      ENDIF
      GOTO 1000
C
430   IF (ABS(IMAG(Z)) .LT. .1*ABS(REAL(Z))) THEN
         IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = -(7 + 8*I)
         ELSE
            COL(IX,IY) = -(7 + 8*I)
         ENDIF
      ELSE
         IF (IMAG(Z) .GE. 0) THEN
            COL(IX,IY) = -(4 + 8*I)
         ELSE
            COL(IX,IY) = -(2 + 8*I)
         ENDIF
      ENDIF
C
C***********************************************************************
C***********************************************************************
C
1000     CONTINUE

2000  CONTINUE
C
      RETURN
      END
