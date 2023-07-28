PROGRAM cubic_roots

	implicit none

	REAL(KIND=8)	:: a0,a1,a2,q,r
	INTEGER(KIND=4)	:: c


	READ(*,*) a2, a1, a0

	q = (a1/3) - (a2*a2)/9
	r = ((a1*a2 - 3*a0)/6) - (a2*a2*a2)/27
	c = INT((q**3)+(r**2))

	print*, 'c = ', (q**3)+(r**2)


	IF ( ((q**3)+(r**2)) .gt. 0 ) THEN
		print*, 'case 1'
	ELSE IF ( ((q**3)+(r**2)) == 0 ) THEN
		print*, 'case 2'
	ELSE
		print*, 'case 3'
	END IF

! Case cannot take real expressions, so it sucks in this case
	SELECT CASE (c)
		CASE (:-1)
			print*, 'case 1'
		CASE (0)
			print*, 'case 2'
		CASE (1:)
			print*, 'case 3'
	END SELECT

END PROGRAM cubic_roots