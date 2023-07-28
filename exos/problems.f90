PROGRAM problems_ch11

	implicit none

	INTEGER,PARAMETER	:: i2=SELECTED_INT_KIND(2),srp=SELECTED_REAL_KIND(6,37)

	INTEGER(KIND=i2)					:: r,c
	REAL(KIND=srp),DIMENSION(1:16)		:: A=(/(1.3_srp*r,r=1,16)/)
	REAL(KIND=srp)						:: B(1:4,1:4),D(-3:4,6:7)
	REAL(KIND=srp)						:: Bsum(1:2,1:4),Dsum(1:2,1:8)

	B=reshape(A,(/4,4/))
	DO r=1,4
		WRITE(*,'(4F5.1)') (B(c,r), c=1,4)
	END DO
	print*

	D=reshape(A,(/8,2/))
	DO r=6,7
		WRITE(*,'(8F5.1)') (D(c,r), c=-3,4)
	END DO

	print*

	Bsum(1,:)=SUM(B,DIM=1)					! sum along each column
	Bsum(2,:)=SUM(B,DIM=2)					! sum along each row

	DO r=1,4
		WRITE(*,'(2F5.1)') (Bsum(c,r), c=1,2)
	END DO
	print*

	Dsum(1,:)=SUM(D,DIM=2)
	Dsum(2,3:8)=1.0_srp
	Dsum(2,1:2)=SUM(D,DIM=1)

	DO r=1,8
		WRITE(*,'(2F5.1)') (Dsum(c,r), c=1,2)
	END DO



END PROGRAM problems_ch11