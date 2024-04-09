      SUBROUTINE CS4F (XL,YL,SIDE,NITER,AR,AI,COL)
C
C     COPYRIGHT 1984 BY ART MATRIX (TM).  ALL RIGHTS RESERVED.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XL,YL,SIDE,AR,AI
C
      INTEGER*4   COL(0:400,0:400)
      COMPLEX*16  Z,A,B,ALPHA,BETA,AA3
C
C
C***********************************************************************
C***********************************************************************
C
      CALL XUFLOW (0)
C
      TPI     = 4*ASIN(1D0)
C
      ESCAPE  =  100.0
      EPSILN  = .000001
C
      ALPHA  = DCMPLX(AR,AI)
C
C***********************************************************************
C***********************************************************************
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
      BETA = DCMPLX(X,Y)
C
      AA3 = -(1 - BETA*BETA/3 - ALPHA)
      B   =  (2*ALPHA*BETA)/3 -    (2*BETA**3)/27
      A   =  CDSQRT(AA3/3)
C
C***********************************************************************
C***********************************************************************
C
      Z = -A
C
      DO 400 I = 1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .LT. EPSILN) GOTO 420
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 430
C
400   CONTINUE
C
      RA = -1
      GOTO 450
C
420   RA =  0
      GOTO 450
C
430   RA =  I
      GOTO 450
C
C***********************************************************************
C***********************************************************************
C
450   Z = A
C
      DO 500 I = 1,NITER
C
         Z = Z*Z*Z - AA3*Z + B
C
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .LT. EPSILN) GOTO 520
         IF (REAL(Z)*REAL(Z) + IMAG(Z)*IMAG(Z) .GE. ESCAPE) GOTO 530
C
500   CONTINUE
C
      RB = -1
      GOTO 600
C
520   RB =  0
      GOTO 600
C
530   RB =  I
      GOTO 600
C
C***********************************************************************
C***********************************************************************
C
600   CONTINUE
C
C          * BOTH ATTRACTED TO 0.
      IF (RA .EQ. 0 .AND. RB .EQ. 0) THEN
         COL(IX,IY) = 3
C
C          * BOTH ARE BOUNDED
      ELSEIF (RA .EQ. -1 .AND. RB .EQ. -1) THEN
         COL(IX,IY) = 6
C
C          * BOTH ESCAPE
      ELSEIF (RA .GT.  0 .AND. RB .GT.  0) THEN
         COL(IX,IY) = 5
C
C          * ONE ATTRACTED TO 0, THE OTHER BOUNDED.
      ELSEIF (RA .EQ.  0 .AND. RB .EQ. -1) THEN
         COL(IX,IY) = 1
      ELSEIF (RA .EQ. -1 .AND. RB .EQ.  0) THEN
         COL(IX,IY) = 9
C
C          * ONE ATTRACTED TO 0 AND THE OTHER ESCAPES.
      ELSEIF (RA .EQ.  0 .AND. RB .GT.  0) THEN
         COL(IX,IY) = 2
      ELSEIF (RA .GT.  0 .AND. RB .EQ.  0) THEN
         COL(IX,IY) = 10
C
C          * ONE BOUNDED AND THE OTHER ESCAPES.
      ELSEIF (RA .EQ. -1 .AND. RB .GT.  0) THEN
         COL(IX,IY) = 4
      ELSEIF (RA .GT.  0 .AND. RB .EQ. -1) THEN
         COL(IX,IY) = 12
C
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
