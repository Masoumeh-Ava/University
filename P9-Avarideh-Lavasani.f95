!Requiered 	Formuls:
!y(x+h)= y(x)-(1.0/6.0) (K1+2*K2+2*K3+K4)
!f(x.y)= y'
!h=((Xn-X0)/n
!K1=hf(x,y)
!K2=hf(x+h/2, y+(K1)/2)
!K3=hf(x+h/2, y+(K2)/2)
!K4=hf(x+h, y+K3)
!-------------------------------------------------
!PROBLEM:
!X"+0.5X'+X=0 ===>, F=X",  X"=-(0.5F+X)
!X(T=0)=-3/7
!X'(T=0)=2
!T:[0,10]
!H=0.02
!-------------------------------------------------
Program P9
IMPLICIT NONE
DOUBLE PRECISION :: X0, V0, H
Integer ::T0,T10

T0 = 0  
T10 = 10  
H = 0.02
V0 = 2.0 
X0 = -3.7 
CALL RungeKutta (T0,T10,H,V0,X0)
T0 = 0  
T10 = 10  
H = 0.02
V0 = 2.0 
X0 = -3.7 
CALL TAYLOR(T0,T10,H,X0,V0)
WRITE(*,*) "THE RESULTS ARE IN TEXT FILES"



STOP
END Program P9
!-------------------------------------------------
!RungeKutta
!-------------------------------------------------
SUBROUTINE RungeKutta (A,B,H,V,X)
IMPLICIT NONE
DOUBLE PRECISION :: FUN01,FUN02,T,V,X,H
DOUBLE PRECISION :: K1,K2,K3,K4
DOUBLE PRECISION :: L1,L2,L3,L4
Integer :: I,N,A,B
!-------------------------------------------------

N = (B-A)/H

V = 2.0 
X = -3/7
T = A 
!-------------------------------------------------
OPEN(100,FILE="RKXT_3.TXT")
OPEN(200,FILE="RKVT.TXT")
DO I=1,N
K1 = H*FUN01(T,X,V)
L1 = H*FUN02(T,X,V)
!-------------------------------------------------
K2 = H*FUN01(T+H/2.0,X+K1/2.0,V+L1/2.0)
L2 = H*FUN02(T+H/2.0,X+K1/2.0, V+L1/2.0)
!-------------------------------------------------
K3 = H*FUN01(T+H/2.0,X+K2/2.0,V+L2/2.0)
L3 = H*FUN02(T+H/2.0,X+K2/2.0,V+L2/2.0)
!-------------------------------------------------
K4 = H*FUN01(T + H,X+K3 ,V+L3)
L4 = H*FUN02(T + H,X+K3 ,V+L3)
!-------------------------------------------------
X = X + (1.0/6.0)*(K1+2*K2+2*K3+K4)
V = V + (1.0/6.0)*(L1+2*L2+2*L3+L4)
!-------------------------------------------------
T = T + H
!-------------------------------------------------
WRITE(100,*) T,X
WRITE(200,*) T,V

END DO
CLOSE(100)
CLOSE(200)
RETURN
END SUBROUTINE
!-------------------------------------------------
FUNCTION FUN01(T,X,V)
IMPLICIT NONE
DOUBLE PRECISION ::X,V,FUN01, T
FUN01 = V
Return
END FUNCTION Fun01
!-------------------------------------------------
FUNCTION FUN02(T,X,V)
IMPLICIT NONE
DOUBLE PRECISION :: FUN02, X,V,T
FUN02= -(0.5*V)-X
Return
END FUNCTION FUN02

!-------------------------------------------------
!TAYLOR
!Xi(t+h)=Xi(t)+hX'i(t)+((h^2)/2)X"i(t)+((h^3)/6)X"'i(t)
!-------------------------------------------------
!PROBLEM:
!X"+0.5X'+X=0 ===>, F=X",  X"=-(0.5F+X)
!X(T=0)=-3/7
!X'(T=0)=2
!T:[0,10]
!H=0.02
!-------------------------------------------------

SUBROUTINE TAYLOR(A,B,H,V,X)
!DOUBLE PRECISION ::Z1,Z1D1,Z1D1,Z1D3
!DOUBLE PRECISION ::Z2, Z2D1,Z2D2,Z2D3
DOUBLE PRECISION :: T, X, V, H
DOUBLE PRECISION ::X1,X2,X3, V1,V2,V3
INTEGER :: I, N, A, B

OPEN(300,FILE="TAYLORXT.TXT")
OPEN(400,FILE="TAYLORVT.TXT")

N=(B-A)/H

T=A
DO I=0,N
    X1=V
    V1=-0.5*V-X
    X2=V1
    V2=-0.5*V1-X1
    X3=V2
    V3=-0.5*V2-X2
    X=X + H*(X1 + (H/2.0)*(X2+(H/3.0)*X3))
    V=V + H*(V1 + (H/2.0)*(V2+(H/3.0)*V3))
    T=T+H
	  
  WRITE (300,*) T, X
  WRITE (400,*) T, V
  
  END DO
CLOSE (300)
CLOSE (400)
RETURN 
END SUBROUTINE

