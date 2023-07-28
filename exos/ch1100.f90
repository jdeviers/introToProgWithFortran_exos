PROGRAM ch1100
!
! spr = single precision real i.e 32 bits; dpr = double precision real i.e 64 bits.
! i2 can actually take values between -128 and +126 (1 sign bit + 8 bits from 2**0 to 2**7)
! Using 127_i2 compiles normally but using it as un upper bound in a for loop generates infinite interations. No such problem with -128.
!
	implicit none

	INTEGER,PARAMETER	:: spr=SELECTED_REAL_KIND(6,37),dpr=SELECTED_REAL_KIND(15,307)
	INTEGER,PARAMETER	:: i2=SELECTED_INT_KIND(2)

	REAL(KIND=spr),DIMENSION(1:4)	:: A
	REAL(KIND=spr),DIMENSION(1:5)	:: B,C=0.0_spr
	INTEGER(KIND=i2)				:: i,j

	INTEGER,DIMENSION(1:2,1:3)		:: D

	A=2_i2
	B(1_i2:4_i2)=2.0_spr;B(5_i2)=3.0_spr
	C(1_i2:4_i2)=A+B(1_i2:4_i2)
!	WRITE(*,*) C(1:4)

	DO i=-128,126
		print*, i
	END DO

	DO i=1,3
		D(1,i)=i
		D(2,i)=3_i2+i
	END DO



	DO j=1,3	! NBR COL in outer loop
		WRITE(*,*) (D(i,j), i=1,2)	! NBR ROW in inner loop, reverse variables 
	END DO

	PRINT*
	PRINT*, D 	! Column-major

END PROGRAM ch1100