PROGRAM problems_array

	implicit none

	integer, parameter				:: a=SELECTED_INT_KIND(3),b=KIND(1)
	integer(a)						:: t,u,i,r
	real(b)							:: c,f,w1,w2
	integer(a),dimension(1:3,1:3)	:: array
	integer(a),dimension(1:3,1:2)	:: sums=0
	real(b),dimension(1:3,1:2)		:: rmean=0.0,ssq=0.0,sd

! EXO 3
!	do t=-50,250
!		c = (5.0/9.0) * (t-32)
!		f = 32 + (9.0*t)/5.0
!		write(*,*) "for t =", t, "c = ", c, "and f = ", f
!	end do



! EXO 4
!	do t=1,10
!		write(*,'(A5,I2,A3,I4)') "12 x ",t ," = ", 12*t
!	end do



! EXO 5
!	i=0
!	do u=1,3
!		do t=1,3
!			i = i+1
!			array(t,u) = i
!		end do
!	end do
!
!	do u=1,3
!		do t=1,3
!			sums(u,1) = sums(u,1) + array(t,u) ! Same order as was initialised (t,u -> t,u): go along row.
!			sums(u,2) = sums(u,2) + array(u,t) ! Inverse order as was initialised (t,u -> u,t): go down column.
!		end do
!	end do
!
!	do u=1,3
!		write(*,*) (array(t,u), t=1,3), sums(u,1)
!	end do
!	write(*,*) (sums(u,2), u=1,3)



! EXO 6
	i=0
	do u=1,3
		do t=1,3
			i = i+1
			array(t,u) = i
		end do
	end do

	do u=1,3
		do t=1,3
			sums(u,1) = sums(u,1) + array(t,u)
			sums(u,2) = sums(u,2) + array(u,t)

			w1 = array(t,u) - rmean(u,1)
			w2 = array(t,u) - rmean(u,2)
			r = t-1

			rmean(u,1) = (r * rmean(u,1) + array(t,u)) / t
			rmean(u,2) = (r * rmean(u,2) + array(u,t)) / t

			ssq(u,1) = ssq(u,1) + w1*w1*r/i
			ssq(u,2) = ssq(u,2) + w2*w2*r/i
		end do
	end do

	sd = (ssq/r)**0.5

	do u=1,3
		write(*,'(5I7,F7.2)') (array(t,u), t=1,3), sums(u,1), int(rmean(u,1)), sd(u,1)
	end do
	write(*,'(3I7)') (sums(u,2), u=1,3)
	write(*,'(3I7)') (int(rmean(u,2)), u=1,3)
	write(*,'(3F7.2)')  (sd(u,2), u=1,3)

!	do u=1,2
!		write(*,*) (sd(t,u), t=1,3)
!	end do

END PROGRAM problems_array