PROGRAM ch1603
	IMPLICIT NONE
!
! Simple case statement example
!
	INTEGER :: I,J,K
	CHARACTER :: Operator
	DO
		PRINT *,' Type in two integers'
		READ *, I,J
		PRINT *,' Type in operator'
		READ '(A)', Operator

		calculator:SELECT CASE (Operator)	! calculator is simply the label of the case statement, as can be found on named do loops or if
		CASE ('+') calculator
			K=I+J
			PRINT *,' Sum of numbers is ',K
		CASE ('-') calculator
			K=I-J
			PRINT *,' Difference is ',K
		CASE ('/') calculator
			K=I/J
			PRINT *,' Division is ',K
		CASE ('*') calculator
			K=I*J
			PRINT *,' Multiplication is ',K
		CASE DEFAULT calculator
			EXIT
		END SELECT calculator
	END DO
END PROGRAM ch1603