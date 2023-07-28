PROGRAM test_maxloc
	implicit none

	INTEGER(KIND=4)					:: i,j
	INTEGER(KIND=4),DIMENSION(1:9)	:: A=(/(i,i=1,9)/)
	INTEGER(KIND=4),DIMENSION(9,3)	:: C
	INTEGER(KIND=4),DIMENSION(3,3)	:: B

	B=reshape(A,(/3,3/),(/2,1/))

	DO i=1,SIZE(B,dim=1)
		WRITE(*,*) (B(i,j),j=1,SIZE(B,dim=2))
	END DO

	print*,'maxloc(B): '
	WRITE(*,*) MAXLOC(B,MASK=B.le.8)
	print*

	C=SPREAD(A,2,3)

END PROGRAM test_maxloc