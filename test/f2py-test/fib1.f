C FILE: FIB1.F
      SUBROUTINE FIB(A,N)
      use tobeincuded
      implicit none
      INTEGER N, I
      REAL*8 A(N)
      DO I=1,N
         IF (I.EQ.1) THEN
            A(I) = 0.0D0
         ELSEIF (I.EQ.2) THEN
            A(I) = 1.0D0
         ELSE 
            A(I) = A(I-1) + A(I-2)
         ENDIF
      ENDDO
      A = A + ncol
      END
C END FILE FIB1.F