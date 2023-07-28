program ch1107
	implicit none
	
	integer , dimension(1:4,1:2) :: x
	integer , dimension(1:8) :: y=(/1,2,3,4,5,6,7,8/)
	integer , dimension(1:6) :: z=(/1,2,3,4,5,6/)
	integer :: r,c

	print *,' Source array y'
	print *,y
	print *,' Source array z'

	print *,z
	
	print *,' Simple reshape sizes match'
	x=reshape(y,(/4,2/))		! 4 columns, 2 rows
	do r=1,2
		print *,(x(c,r),c=1,4)	! These loops write row-major i.e x11,x12,...,x1nb_c,x21,x22,...,...,xnb_r1,xnb_rnb_c.
	end do
	
	print *,' Source 2 elements smaller pad with 0'
	x=reshape(z,(/4,2/),(/0,0/))
	do r=1,2
		print *,(x(c,r),c=1,4)
	end do
	
	print *,' As previous now specify order as 1*2'
	x=reshape(z,(/4,2/),(/0,0/),(/1,2/))
	do r=1,2
		print *,(x(c,r),c=1,4)
	end do
	
	print *,' As previous now specify order as 2*1'
	x=reshape(z,(/4,2/),(/0,0/),(/2,1/))
	do r=1,2
		print *,(x(c,r),c=1,4)
	end do

end program ch1107