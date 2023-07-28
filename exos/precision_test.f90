PROGRAM precision_test

	implicit none

	integer, parameter	:: custom = SELECTED_REAL_KIND(1,310)
	real(custom)		:: f

	print*, 'KIND: ', KIND(f)
	print*, 'HUGE: ', HUGE(f)
	print*, 'PREC: ', PRECISION(f)
	print*, 'EPSI: ', EPSILON(f)
!	print*, f

END PROGRAM precision_test