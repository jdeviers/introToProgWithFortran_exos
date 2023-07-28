PROGRAM dyna_alloc

	implicit none

	TYPE link
		CHARACTER(LEN=1)	:: C
		TYPE(link),POINTER	:: next
	END TYPE link
	TYPE (link),POINTER		:: start,current
!
	INTEGER(KIND=1)								:: io=0_1
	INTEGER(KIND=4)								:: counter=0_4
	CHARACTER(LEN=1),DIMENSION(:),ALLOCATABLE	:: A
!
	ALLOCATE(start)
	OPEN(10,file='test1',action='read')
	READ(10,'(A)',iostat=io) start%C

	IF ( io == -1_1 ) THEN
		NULLIFY(start%next)
	ELSE
		ALLOCATE(start%next)
		counter=counter+1_4
	END IF

	current => start
	DO WHILE ( ASSOCIATED(current%next) )
		current => current%next
		READ(10,'(A)',iostat=io) current%C

		IF (io == -1 ) THEN
			NULLIFY(current%next)
		ELSE
			ALLOCATE(current%next)
			counter=counter+1_1
		END IF
	END DO
	CLOSE(10)

	ALLOCATE(A(1:counter))
	counter=0_4
	current => start
	DO WHILE ( ASSOCIATED(current%next) )
		counter=counter+1
		A(counter) = current%C
		print*, 'Data into array: ',A(counter),', data in current pointer: ',current%C
		current => current%next
	END DO


END PROGRAM dyna_alloc