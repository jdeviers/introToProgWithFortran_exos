PROGRAM skip

	implicit none

	INTEGER,PARAMETER	:: ii2=SELECTED_INT_KIND(2)
	INTEGER(KIND=ii2)	:: i,j,k

	OPEN(unit=10,file='skip.dat',status='old',action='read')
100 FORMAT(I2,1X,I2/)					! Read the 1st line as I2, then goes to the next line and reads nothing
	DO i=1,8
		READ(10,100) j,k
		WRITE(*,'(2I2)') j,k
		BACKSPACE(10)			! This goes up one position in the file reading. It negates the format-mediated line-skipping.
!		REWIND(10)				! This goes back to the beginning of the file
	END DO
	CLOSE(10)

END PROGRAM skip