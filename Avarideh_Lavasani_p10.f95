PROGRAM DIRICHLET
IMPLICIT NONE
DOUBLE PRECISION , ALLOCATABLE :: POTENTIAL(:,:)
INTEGER :: I, J, k 
INTEGER, PARAMETER :: N = 100 
INTEGER, PARAMETER :: R = 20000 
!----------------------------------------
!GUID
!POTENTIAL(:,:)==> POTENTIALS
!N = 100 ==> MATRIX SIZE .
!R = 200000 ! ITERATIONS
!----------------------------------------
WRITE (*,*) "HI, PLEASE WAIT...."
!----------------------------------------
ALLOCATE(POTENTIAL(N,N))
POTENTIAL = 0.0
POTENTIAL(1,:) = 25
POTENTIAL(100,:)=25
!----------------------------------------
DO K=1, R
	DO I=2, N-1
		DO J=2, N-1
			POTENTIAL(I,J) = (1.0/4.0)*(POTENTIAL(I,J+1)+POTENTIAL(I,J-1)+POTENTIAL(I+1,J)+POTENTIAL(I-1,J))
		END DO
	END DO
END DO
!----------------------------------------
OPEN (100 , FILE="POTENTIAL WITH DIRICHLET.txt")
  DO I=1, N
WRITE(100,"(100F20.15)") (POTENTIAL(I,J), J=1, N)
END DO

!----------------------------------------

DEALLOCATE(POTENTIAL)
CLOSE(100)
!----------------------------------------
WRITE(*,*) "NOW YOU CAN OPEN TEXT FILE...."
STOP
END PROGRAM DIRICHLET