PROGRAM ch14EOF
!
! This program reads through a file to get its length, then re-reads it to store its contents
! into an array of just the right size. The alternative method is to store data as it is read in the first run
! into an arbitrarily large array, and once EOF is reached to transfer it to another array of just the right size.
! This second method runs faster but requires more memory; and it requires to have some idea of the size of the file you're reading.
! The method implemented here adapts to any array size, and saves memory at the cost of an additional read loop.
!
	implicit none

	INTEGER,PARAMETER			:: i2=SELECTED_INT_KIND(2),long=SELECTED_REAL_KIND(15,307)
	INTEGER(KIND=i2)			:: i,j, FileStat
	REAL(KIND=long)				:: a	
	CHARACTER(LEN=8)			:: tmp
	CHARACTER(LEN=8),ALLOCATABLE:: store(:)

	OPEN(unit=10,file='datach14.in',status='old',iostat=FileStat,action='read')
	READ(10,'(A)') tmp
	IF (FileStat .gt. 0) THEN
		print*, 'Error opening file, please check'
		STOP
	END IF
	REWIND(10)		! This avoids performing the FileStat .gt. 0 test at each read 

	i=0_i2
	DO
		READ(10,'(A)',iostat=FileStat) tmp
		IF (FileStat .lt. 0) EXIT
		i=i+1_i2
	END DO
	print*, 'File length (# of lines): ', i

	ALLOCATE(store(i))
	REWIND(10)


	OPEN(unit=11,file='datach14.out',status='new',action='write',form='unformatted')
	DO j=1,i
		READ(10,'(A)') store(j)
		WRITE(11) store(j)
	END DO
	CLOSE(11)

	CLOSE(10)

	OPEN(unit=12,file='datach14.out',status='old',action='read')
	DO j=1,i
		READ(12,'(A)') tmp
		WRITE(*,'(A)') tmp
	END DO


END PROGRAM