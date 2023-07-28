PROGRAM ch1801
	implicit none

	LOGICAL			:: FLAG=.FALSE.
	INTEGER(KIND=1)	:: i

	DO
		print*, 'Enter an integer comprised between -128 and 127.'
		READ(*,'(I4)') i
			IF ( i .lt. 0 ) THEN
				FLAG=.FALSE.
				i=-i
			END IF
		print'(i4,L2)',i,FLAG
		FLAG=.TRUE.
	END DO



END PROGRAM ch1801