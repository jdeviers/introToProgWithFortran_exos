INTEGER(KIND=1) FUNCTION MOD2(i,j)
!
! This version of mod works with negative values
!	
	implicit none

	INTEGER(KIND=1),INTENT(INOUT)	:: i,j
	i=ABS(i);j=ABS(j)

	DO WHILE ( i .gt. j )
		i=i-j
	END DO
	MOD2=i

END FUNCTION MOD2


RECURSIVE INTEGER(KIND=1) FUNCTION RMOD2(i,j) RESULT(RMOD)
!
! This one uses recursion
!
	implicit none

	INTEGER(KIND=1)	:: i,j
	i=ABS(i);j=ABS(j)

	IF ( i .lt. j ) THEN
		RMOD=i
	ELSE
		RMOD=RMOD2(i-j,j)
	END IF

END FUNCTION RMOD2


PROGRAM ch1501
!
! WARNING: can't pass a KIND descriptor
!
	implicit none

	INTEGER,PARAMETER	:: i2=SELECTED_INT_KIND(2)
	INTEGER(KIND=i2)	:: a,b,c,MOD2,RMOD2

	print*, "Enter a and b, integers comprised between -128 and 127."
	READ(*,*) a,b
	c=RMOD2(a,b)
	print*,c

END PROGRAM ch1501