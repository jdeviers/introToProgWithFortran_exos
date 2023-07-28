PROGRAM Break
	IMPLICIT NONE
	REAL :: Hi , Hr , Hlow , High , Half , Xl
	REAL :: Xh , Xm , D
	REAL :: Tol
! problem - find hi from expression given
! in function f
! F=A*(1.0-0.8*EXP(-0.6*C/A))-B
! HI IS INCIDENT WAVE HEIGHT (C)
! HR IS REFORMED WAVE HEIGHT (B)
! D IS WATER DEPTH AT TERRACE EDGE (A)
	PRINT*,' Give reformed wave height, and water depth'
	READ*,Hr,d
	Tol = .05_8*Hr
!
! for Hlow- let Hlow=hr
! for high- let high=Hlow*2.0
!
! check that signs of function results are different
!
	Hlow = Hr
	High = Hlow*2.0
	Xl = F( Hlow, Hr, D)
	Xh = F( High, Hr, D)
!
	DO WHILE ((XL*XH) >= 0.0)
		HIGH = HIGH*2.0
		XH = F(HIGH,HR,D)
	END DO
!
	DO
		HALF=(HLOW+HIGH)*0.5
		XM=F(HALF,HR,D)
		IF((XL*XM) < 0.0)THEN
			XH=XM
			HIGH=HALF
		ELSE
			XL=XM
			HLOW=HALF
		ENDIF

		IF(ABS(HIGH-HLOW)<= TOL)EXIT
	END DO

	PRINT*,' Incident Wave Height Lies Between'
	PRINT*,Hlow,' and ',High,' metres'

	CONTAINS
	REAL FUNCTION F(A,B,C)
		IMPLICIT NONE
		REAL , INTENT (IN) :: A
		REAL , INTENT (IN) :: B
		REAL , INTENT (IN) :: C

		F=A*(1.0-0.8*EXP(-0.6*C/A))-B
	END FUNCTION F
END PROGRAM Break