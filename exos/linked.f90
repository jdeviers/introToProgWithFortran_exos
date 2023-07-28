PROGRAM linked
	implicit none

	TYPE Link
		REAL(KIND=16)		:: C
		TYPE(Link),POINTER	:: Next
	END TYPE Link
	TYPE(Link),POINTER		:: First=>NULL(),Current=>NULL()

	INTEGER(KIND=4)			:: a,b,count=0,io=0
	REAL(KIND=16)			:: rd

	REAL(KIND=16),DIMENSION(100,100)	:: M=0.0_16,N
	REAL(KIND=16),ALLOCATABLE			:: list(:)

!
	OPEN(10,file='mat.dat',status='unknown',action='write')
	DO b=1,100
		DO a=1,100
			CALL RANDOM_NUMBER(rd)
			IF ( rd.gt.0.5_16 ) THEN
				M(a,b) = REAL(2.6*a+1.3*b,16)
			END IF
		END DO
	END DO

	DO b=1,100
		WRITE(10,'(100f8.3)') (M(a,b), a=1,100)
	END DO
	CLOSE(10)
!

	OPEN(10,file='mat.dat',status='old',action='read',iostat=io)
	ALLOCATE(First)

	IF ( io /= 0 ) THEN
		print*, 'there was an error.'
	END IF

! This matrix is read row-major; no other way to read in a file array
	DO a=1,100
		READ(10,*,iostat=io) (M(a,b),b=1,100)
	END DO
	N = TRANSPOSE(M)	! N is as if M was read column-major.
!
	DO b=1,100
		DO a=1,100
			IF ( M(a,b) == 0.0_16 ) THEN	! read column-major (b then a loops) a transposed matrix is like reading M row-major.
! using CYCLE might iterate a instead of b, i.e jump to next row instead of reading the next col.			
			ELSE		
				IF ( a == 1 .AND. b == 1 ) THEN
					print*, 'init'
					First%C = M(a,b)
					IF ( io == -1 ) THEN
						NULLIFY(first%Next)
					ELSE
						ALLOCATE(first%Next)
						count=count+1
					END IF

					Current => First
				ELSE
					Current => Current%Next
					Current%C = M(a,b)
					IF ( io == -1 ) THEN
						NULLIFY(Current%Next)
					ELSE
						ALLOCATE(Current%Next)
						count=count+1
					END IF
				END IF
			END IF	
		END DO
	END DO
	print*, count

	ALLOCATE(list(1:count))
	Current => first

	DO WHILE (ASSOCIATED(Current%Next))
		count=count+1
		list(count) = Current%C
		WRITE(*,*) list(count)
		Current => Current%Next
	END DO
	DEALLOCATE(list)
	CLOSE(10)

END PROGRAM linked