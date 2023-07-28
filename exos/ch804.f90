PROGRAM ch0804
	IMPLICIT NONE
	
	REAL :: Light_Minute, Distance, Elapse
	INTEGER :: Minute, Second
	REAL , PARAMETER :: Light_Year=9.46*10**12

! In the above parameter, 10 is of default kind INTEGER(4), i.e integer encoded with 4 bytes (32 bits), 1 of which stores the sign. 
! It takes values between -(2^(31) - 1) and +(2^(31) - 1), i.e -2,147,483,648 to 2,147,483,647. 10^12 is outside this range.
! This can be changed by making it a real (single precision 10.0 or double 10.0d0) or an INTEGER(8) (10_8) 

	Light_minute = Light_Year/(365.25 * 24.0 * 60.0)
	Distance = 150.0 * 10 ** 6
	Elapse = Distance / Light_minute
	Minute = Elapse
	Second = (Elapse - Minute) * 60
	
	Print *,' Light takes ' , Minute,' Minutes'
	Print *,' ' , Second,' Seconds'
	Print *,' To reach the earth from the sun'
END PROGRAM ch0804