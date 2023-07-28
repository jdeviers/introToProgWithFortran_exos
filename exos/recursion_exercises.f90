RECURSIVE INTEGER FUNCTION rfact(n) result(answer)
	implicit none

	INTEGER(KIND=4)	:: n

	if ( n == 1 ) then
		answer = 1
	else if ( n .gt. 1 ) then
		answer = n * rfact(n-1)
	end if

END FUNCTION rfact

RECURSIVE REAL(KIND=16) FUNCTION bigfact(n) result(answer)
	implicit none

	REAL(KIND=16)	:: n

	if (n == 1.0_16 ) then
		answer = 1.0_16
	else if ( n .gt. 1.0_16 ) then
		answer = n * bigfact(n-1.0_16)
	end if

END FUNCTION bigfact

PROGRAM recursion
	implicit none

	INTEGER(KIND=4)	:: rfact
	REAL(KIND=16)	:: bigfact,n

	write(*,*) rfact(11)
	print*, 'Enter a float. It shall be truncated to the nearest inferior integer.'
	read(*,*) n
	write(*,'(E10.4)') bigfact(real(int(n),16))

END PROGRAM recursion