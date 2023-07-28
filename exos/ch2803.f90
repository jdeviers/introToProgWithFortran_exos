PROGRAM ch2803
	IMPLICIT NONE

	TYPE Ragged
		REAL , DIMENSION(:) , ALLOCATABLE :: Ragged_row
	END TYPE Ragged
	INTEGER :: I
	INTEGER , PARAMETER :: N=3
	TYPE (Ragged) , DIMENSION(1:N) :: Lower_Diag

	DO I=1,N
		ALLOCATE(Lower_Diag(I)%Ragged_Row(1:I))
		PRINT *,' Type in the values for row ' , I
		READ *,Lower_Diag(I)%Ragged_Row(1:I)
	END DO
	DO I=1,N
		PRINT *,Lower_Diag(I)%Ragged_Row(1:I)
	END DO
END PROGRAM ch2803