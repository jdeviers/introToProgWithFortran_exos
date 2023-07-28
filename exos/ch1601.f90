REAL(KIND=4) FUNCTION PERIOD(l)
	implicit none

	REAL(KIND=16),PARAMETER	:: pi=3.14159
	REAL(KIND=4),INTENT(IN) :: l

	PERIOD=2.0_4 * pi * (l/9.81_4) ** .5_4

END FUNCTION PERIOD

PROGRAM PENDULUM
	implicit none

	INTEGER(KIND=4)	:: length,i
	REAL(KIND=4)	:: PERIOD

! This block is for exercise 2, nothing to do with pendulum
	i=-1
	DO WHILE ( i .lt. 0_4)
		Print*, 'Enter a positive integer:'
		READ(*,*) i
	END DO
!

	DO length=1_4,200_4
		WRITE(*,'(A,f5.1,A,f5.2,A)') 'Period at length = ',length/2._4,' cm is ',PERIOD(length/(100*2._4)), ' s.'
	END DO

END PROGRAM PENDULUM