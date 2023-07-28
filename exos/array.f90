PROGRAM array
	implicit none
!
! An array need not start at test(1) : here a vector is defined, ranging from test(25) to test(100).
!
	integer								:: i,j
	integer, dimension(1:5,2:6)	:: test

	test=0
	do i=1,5
		write(*,'(6I4)') (test(i,j),j=1,6)
	end do

END PROGRAM array