PROGRAM ABCDEF
IMPLICIT NONE
DOUBLE PRECISION :: X, PI, EPSILONN, SUMMATION, ERROR
INTEGER ::  J, K, FACTORIAL

WRITE(*,*) " PLEASE ENTER EPSILON :"
READ(*,*) EPSILONN
! ----------------
PI = ACOS(-1.0)
X = 0.0
! ----------------
DO X = 0, PI/2, PI/6.0
SUMMATION = 0.0
 ERROR = 100.0
K = 1
	DO WHILE (ERROR > EPSILONN)
		FACTORIAL = 1
        J = 2*K -1
         	DO WHILE (J .GE. 1) 
            FACTORIAL = FACTORIAL * J
            J = J -1
            END DO
    SUMMATION = SUMMATION + ((-1)**(K-1))*(X**(2*K-1))/FACTORIAL
    ERROR = ABS(SIN(X) - SUMMATION)
    K = K + 1
	END DO
	WRITE(*,*) "X=", X,      "      APPROXIMATION =", SUMMATION
    WRITE(*,*) "        ", "EXACT =", SIN(X)
    WRITE(*,*) " ---------------- "
END DO
STOP 
END PROGRAM ABCDEF
