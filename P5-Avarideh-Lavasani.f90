PROGRAM P5
IMPLICIT NONE
DOUBLE PRECISION:: STD, SUM, AVE,S
DOUBLE PRECISION, ALLOCATABLE:: NUM(:)
INTEGER:: I, J, K, N, C
OPEN(100, FILE="random.txt")
OPEN(200,FILE="results.txt")

!-----------------------------------------

 WRITE(*,*)	"PLEASE ENTER NUMBER"
 READ (*,*) N
 WRITE (200,*) "THE SELECTED NUMBER BY USER IS:", N
 ALLOCATE (NUM(N))
 NUM = 0.0 
!-----------------------------------------
 DO I=1, N
	READ (100,*) NUM(I)
 END DO

!-----------------------------------------
DO J = 1, N
	WRITE(200,*) NUM(J)
END DO

!-----------------------------------------

!S= SUM OF THE N NUMERS
!AVE= AVERAGE OF N NUMBER
S = 0.0
DO I=1,N
	S=S+NUM(I)
END DO

AVE = S/N
WRITE (200,*) "THE SUM OF THE SELECTED NUMBER IS:", S
WRITE (200,*) "THE AVERAGE OF THE SELECTED NUMBER IS:", AVE

!---------------------------
!SUM= EACH NUMBER - AVE
SUM=0

DO K=1, N

	SUM=SUM +(NUM(k)-AVE)**2

END DO

 STD=SQRT (SUM/(N-1.0))

 WRITE(200,*) "STD=", STD

!-------------------------------
C = 0
	DO I = 1, N
    	IF (NUM(I) .EQ. 110) THEN
        C =C  + 1
        END IF
    END DO
WRITE(*,*) "COUNTS OF 110 = ", C
!-------------------------------
CLOSE (100)
CLOSE (200)
 END PROGRAM P5





