PROGRAM ch703
	
	implicit none

!
!	This program reads a multiple-line address and a name, and writes them back
!

	CHARACTER(LEN=30) :: line1,line2,line3,name

	PRINT*,'Type in your address:'
	PRINT*,'"Line 1","line 2","line 3" as separated inputs'
	READ(*,*) line1,line2,line3
	PRINT*,'And now your name, as one input:'
	READ(*,*) name

	WRITE(*,*)line1,name //NEW_LINE('A')// line2 //NEW_LINE('A')// line3

END PROGRAM ch703