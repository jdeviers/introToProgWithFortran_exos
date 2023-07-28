PROGRAM ch1403

	implicit none

	INTEGER,PARAMETER						:: i3=SELECTED_INT_KIND(3)
	INTEGER(KIND=i3)						:: i,j,io
	INTEGER(KIND=i3),DIMENSION(0:49,1:10)	:: A

100 FORMAT(1X,10(I3,2X))

	OPEN(unit=10,file='ch1403.out',status='new',action='readwrite',iostat=io)
	DO i=1,491,10
		WRITE(10,100) (i+j, j=0,9)
	END DO
	REWIND(10)

	DO i=0,49
		READ(10,100) (A(i,j), j=1,10)
	END DO
	print*, 'Should be 205: ', A(20,5)
	REWIND(10)								
! If this REWIND is commented out, the descending sequence is appended to the ascending one. 
! Keeping it overwrites the file.
	A=0_i3
	DO i=0,49
		READ(10,100) A(i,1:10)
	END DO
	print*, 'Should be 205: ', A(20,5)
	REWIND(10)


	DO i=49,0,-1
		WRITE(10,100) (A(i,j), j=10,1,-1)
	END DO
	CLOSE(10)
	

END PROGRAM ch1403