REAL(KIND=4) FUNCTION PYTH(a,b)

	implicit none

	REAL(KIND=4),INTENT(IN)	:: a,b

	PYTH=sqrt(a*a+b*b)

END FUNCTION PYTH


PROGRAM ch1503

	implicit none

	REAL(KIND=4)	:: a,b,c
	REAL(KIND=4)	:: PYTH

	WRITE(*,*) 'Enter the lengths of the 2 smaller sides of a right-angled triangle.'
	READ(*,*) a,b
	c=PYTH(a,b)
	print*, 'The hypothenuse length is: ',c

END PROGRAM ch1503