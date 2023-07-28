SUBROUTINE rotate_ccw(x1,y1,x2,y2,radians)
	implicit none

	REAL(KIND=4),INTENT(IN)	:: x1,y1
	REAL(KIND=4),INTENT(OUT):: x2,y2
	REAL(KIND=4),INTENT(IN)	:: radians

	x2 = x1*cos(radians)+y1*sin(radians)
	y2 = -x1*sin(radians)+y1*cos(radians)

END SUBROUTINE rotate_ccw


PROGRAM ch2201
	implicit none

	INTERFACE
		SUBROUTINE rotate_ccw(x1,y1,x2,y2,radians)
			implicit none
			REAL(KIND=4),INTENT(IN)	:: x1,y1
			REAL(KIND=4),INTENT(OUT):: x2,y2
			REAL(KIND=4),INTENT(IN)	:: radians
		END SUBROUTINE rotate_ccw
	END INTERFACE

	REAL(KIND=4)	:: x,y
	REAL(KIND=4)	:: xr,yr
	REAL(KIND=4)	:: angle

	INTEGER(KIND=1)	:: io

	DO
		print*, 'Enter the REAL, DEGREES angle by which the axes are rotated counterclockwise. INCLUDE DECIMAL PART.'
		READ(*,'(F8.2)',iostat=io) angle
		IF ( io == 0 ) EXIT
		print*, 'There was an error, retry'
	END DO

	angle = (2.*angle*3.14159)/360.
	print*,angle
	x = 14._4
	y = 5._4

	CALL rotate_ccw(x,y,xr,yr,angle)

	print'(A,F8.2,A)','after a ',angle/3.14159_4,'-pi (radians) rotation,'
	print'(F8.2,A,F8.2)',x,' becomes ',xr
	print'(F8.2,A,F8.2)',y,' becomes ',yr

END PROGRAM ch2201