
Program DIFFERENTIAL
IMPLICIT NONE
DOUBLE PRECISION :: X, Y, H, F1, F2, F3, PI
INTEGER::  N , i


!---------------------------------------------------------------------------
!PROBLEM DESCRIPTIN
!WE ARE GOING TO CALCULATE  Y'= 3X-SIN(X+2PI/3)+Y BY TAYLORE . 
!WE KNOW THAT Y(X=3)= -2/5
!AND [0,3]
!AND H=0.001
!---------------------------------------------------------------------------
!LESSON DESCRIPTIN
!Yi+1= Yi+hf(xi,yi)+ f'(xi,yi)(h^2)/2!+......
 !---------------------------------------------------------------------------
 
OPEN(100,FILE="results.TXT")
!---------------------------------------------------------------------------
H=0.001
N=(3.0-0)/H
Y=-2.0/5.0
X=3.0
PI=ACOS(-1.0)
DO i=1,N
F1=(3.0)*X-SIN(X+(2.0)*PI/3.0)+Y
F2=3.0-COS(X+2.0*PI/3.0)+F1
F3=SIN(X+2.0*PI/3.0)+F2
    Y=Y+H*(F1)+((H**2)*(F2)/2.0)+((H**3)*(F3)/6.0)
	WRITE (100,*) X,Y
  	X=X-H
END DO
CLOSE (100)
END Program DIFFERENTIAL

!---------------------------------------------------------------------------
