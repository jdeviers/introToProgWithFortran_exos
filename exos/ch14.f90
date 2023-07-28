PROGRAM ch14

	implicit none

	INTEGER,PARAMETER	:: i2=SELECTED_INT_KIND(2),long=SELECTED_REAL_KIND(15,307)
	INTEGER(KIND=i2)	:: counter
	CHARACTER(LEN=8)	:: tmp

	OPEN(unit=10,file='datach14.in',status='old',action='read')
	DO counter=1,100
		READ(10,'(A)') tmp
		print*, 'In 1st loop: ', tmp
		IF (tmp=='END') THEN
			BACKSPACE(10);BACKSPACE(10)	! Need 2 backspaces otherwise the next read jumps to the next value i.e END
			READ(10,'(A)') tmp
			print*, 'Penultimate entry is: ', tmp
			EXIT
		END IF
	END DO

	print*, 'At line: ', counter-1

END PROGRAM ch14