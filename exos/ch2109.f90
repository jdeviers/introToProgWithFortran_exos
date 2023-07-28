PROGRAM C2109
	implicit none

	TYPE Link
		CHARACTER 			:: C
		TYPE (Link),POINTER :: Next
	END TYPE Link
	TYPE (Link),POINTER 	:: Root,Current
	INTEGER 				:: IO_Stat_Number=0

	ALLOCATE(Root)
	
	OPEN(10,file='test1',action='read')

	READ (UNIT = 10, FMT = '(A)' , ADVANCE = 'NO' , &
	IOSTAT = IO_Stat_Number) Root%C
	
	IF (IO_Stat_Number == -1) THEN
		NULLIFY(Root%Next)
	ELSE
		ALLOCATE(Root%Next)
	ENDIF

	Current => Root
	DO WHILE (ASSOCIATED(Current%Next))
		Current => Current%Next
		READ (UNIT=10,FMT='(A)',ADVANCE='NO', &
		IOSTAT=IO_Stat_Number) Current%C
!
		print*, 'iostat: ', IO_Stat_Number
!
		IF (IO_Stat_Number == -1) THEN
			NULLIFY(Current%Next)
		ELSE
			ALLOCATE(Current%Next)
		ENDIF
	END DO
	CLOSE(10)

	Current => Root
	DO WHILE (ASSOCIATED(Current%Next))
		PRINT * , Current%C
		Current => Current%Next
	END DO

END PROGRAM C2109