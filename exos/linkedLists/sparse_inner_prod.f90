MODULE precision
	implicit none

	INTEGER,PARAMETER	:: long=SELECTED_REAL_KIND(15,307)
	INTEGER,PARAMETER	:: i5=SELECTED_INT_KIND(5)

END MODULE precision

MODULE mat
	use precision
	implicit none

	contains

	SUBROUTINE writemat(s_v)
		implicit none

		REAL(long)	:: x,y
		REAL(long)	:: r1,r2
		INTEGER(i5)	:: a,s_v

		OPEN(10,file='vec_xy.dat',action='write')
		DO a=1,s_v
			CALL RANDOM_NUMBER(r1)
			IF ( r1 .gt. 0.5_i5 ) THEN
				x=a*r1
			ELSE
				x=0._long
			END IF

			CALL RANDOM_NUMBER(r2)
			IF ( r2 .gt. 0.5_i5 ) THEN
				y=a*r2 
			ELSE
				y=0._long
			END IF

			WRITE(10,'(2f8.3)') x,y
		END DO
		CLOSE(10)
		WRITE(*,'(A)') 'New pair of sparse vectors written to file.'
	END SUBROUTINE writemat

	SUBROUTINE readmat(x_vec,y_vec)
		implicit none

		TYPE Link
			REAL(Long)			:: value
			INTEGER(i5)			:: index
			TYPE(Link),POINTER	:: next
		END TYPE Link
		TYPE(Link),POINTER		:: start_x,current_x
		TYPE(Link),POINTER		:: start_y,current_y

		INTEGER(i5)				:: i,j,io=0_i5,count,siz
		CHARACTER(LEN=1)		:: rubbish
		REAL(long)				:: tmp

		REAL(long),ALLOCATABLE	:: x_vec(:,:),y_vec(:,:)
! SWITCHES
		CHARACTER(LEN=1)		:: switch2,switch3,switch4
		LOGICAL					:: switch_2,switch_3,switch_4
!
		WRITE(*,'(A)') 'Print nonzero elements at initial sparse vector read? (y/n)'
		READ(*,'(A)') switch2
		SELECT CASE (switch2)
			CASE ('y','Y')
				switch_2 = .TRUE.
			CASE ('n','N')
				switch_2 = .FALSE.
			CASE DEFAULT
				switch_2 = .FALSE.
		END SELECT

		OPEN(10,file='vec_xy.dat',action='read',iostat=io)
		IF ( io /= 0_i5 ) THEN
			ERROR STOP 'Error opening the file.'
		END IF
! X vector read loop
		count=0_i5
		siz=0_i5
		tmp=0._long
		ALLOCATE(start_x)
		DO WHILE ( tmp == 0._long )
			READ(10,'(F8.3,A8)',iostat=io) tmp,rubbish
			count=count+1			
		END DO
		start_x%value = tmp
		start_x%index = count
		siz=siz+1_i5
!		
		IF (switch_2) WRITE(*,'(A,f8.3,I5)') 'x initialisation: ',start_x%value,start_x%index
!
		current_x => start_x
		IF ( io == 0_i5) THEN
			ALLOCATE(current_x%next)
		ELSE IF ( io > 0_i5 ) THEN
			ERROR STOP 'Error in file reading.'
		ELSE
			NULLIFY(current_x%next)
		END IF

		DO WHILE(ASSOCIATED(current_x%next))
			READ(10,'(F8.3,A8)',iostat=io) tmp,rubbish
			count=count+1

			IF ( io == 0_i5 .AND. tmp /= 0._long ) THEN
				current_x => current_x%next
				current_x%value = tmp
				current_x%index = count
				siz=siz+1_i5
!				
				IF (switch_2) WRITE(*,'(A,f8.3,I5)') 'read x until EOF: ',current_x%value,current_x%index
!				
				ALLOCATE(current_x%next)
			END IF

			IF ( io /= 0_i5) NULLIFY(current_x%next)

		END DO
		ALLOCATE(x_vec(siz,2))
!		
		IF (switch_2) WRITE(*,'(A)')'----------'
!
! Y vector read loop
		REWIND(10)
		count=0_i5
		siz=0_i5
		tmp=0._long
		ALLOCATE(start_y)
		DO WHILE ( tmp == 0._long )
			READ(10,'(A8,F8.3)',iostat=io) rubbish,tmp
			count=count+1			
		END DO
		start_y%value = tmp
		start_y%index = count
		siz=siz+1_i5
!		
		IF (switch_2) WRITE(*,'(A,f8.3,I5)') 'y initialisation: ',start_y%value,start_y%index
!
		current_y => start_y
		IF ( io == 0_i5) THEN
			ALLOCATE(current_y%next)
		ELSE IF ( io > 0_i5 ) THEN
			ERROR STOP 'Error in file reading.'
		ELSE
			NULLIFY(current_y%next)
		END IF

		DO WHILE(ASSOCIATED(current_y%next))
			READ(10,'(A8,F8.3)',iostat=io) rubbish,tmp
			count=count+1

			IF ( io == 0_i5 .AND. tmp /= 0._long ) THEN
				current_y => current_y%next
				current_y%value = tmp
				current_y%index = count
				siz=siz+1_i5
!				
				IF (switch_2) WRITE(*,'(A,f8.3,I5)') 'read y until EOF: ',current_y%value,current_y%index
!				
				ALLOCATE(current_y%next)
			END IF

			IF ( io /= 0_i5) NULLIFY(current_y%next)

		END DO
		ALLOCATE(y_vec(siz,2))
!		
		IF (switch_2) WRITE(*,'(A)')'----------'
!
		CLOSE(10)

		WRITE(*,'(A)') 'Print elements during dense array writing? (y/n)'
		READ(*,'(A)') switch3
		SELECT CASE (switch3)
			CASE ('y','Y')
				switch_3 = .TRUE.
			CASE ('n','N')
				switch_3 = .FALSE.
			CASE DEFAULT
				switch_3 = .FALSE.
		END SELECT
!
! stripped X vector written to array
		current_x => start_x
		siz=0_i5
		DO WHILE(ASSOCIATED(current_x%next))
			siz=siz+1_i5
			x_vec(siz,1) = current_x%value
			x_vec(siz,2) = current_x%index
!
			IF (switch_3) WRITE(*,'(A,f8.3,I5)') 'vector writing: ', x_vec(siz,1),int(x_vec(siz,2))
!
			current_x => current_x%next
		END DO
		siz=siz+1
		x_vec(siz,1) = current_x%value
		x_vec(siz,2) = current_x%index
!		
		IF (switch_3) WRITE(*,'(A,f8.3,I5)') 'vector writing: ', x_vec(siz,1),int(x_vec(siz,2))
!		
		IF (switch_3) WRITE(*,'(A)')'----------'
!
! stripped X vector written to array
		current_y => start_y
		siz=0_i5
		DO WHILE(ASSOCIATED(current_y%next))
			siz=siz+1_i5
			y_vec(siz,1) = current_y%value
			y_vec(siz,2) = current_y%index
!
			IF (switch_3) WRITE(*,'(A,f8.3,I5)') 'y vector writing: ', y_vec(siz,1),int(y_vec(siz,2))
!
			current_y => current_y%next
		END DO
		siz=siz+1_i5
		y_vec(siz,1) = current_y%value
		y_vec(siz,2) = current_y%index
!
			IF (switch_3) WRITE(*,'(A,f8.3,I5)') 'y vector writing: ', y_vec(siz,1),int(y_vec(siz,2))
!			
			IF (switch_3) WRITE(*,'(A)') '----------'
!

		WRITE(*,'(A)') 'Print the dense array after writing? (y/n)'
		READ(*,'(A)') switch4
		SELECT CASE (switch4)
			CASE ('y','Y')
				switch_4 = .TRUE.
			CASE ('n','N')
				switch_4 = .FALSE.
			CASE DEFAULT
				switch_4 = .FALSE.
		END SELECT

! X linked list printout
		current_x => start_x
		DO WHILE(ASSOCIATED(current_x%next))
			IF (switch_4) WRITE(*,'(A,f8.3,I5)') 'Final printout x: ',current_x%value,int(current_x%index)
			current_x => current_x%next
		END DO
		IF (switch_4) WRITE(*,'(A,f8.3,I5)') 'Final printout x: ',current_x%value,int(current_x%index)
		print*,'----------'
!
! Y linked list printout
		current_y => start_y
		DO WHILE(ASSOCIATED(current_y%next))
			IF (switch_4) WRITE(*,'(A,f8.3,I5)') 'Final printout y: ',current_y%value,int(current_y%index)
			current_y => current_y%next
		END DO
		IF (switch_4) WRITE(*,'(A,f8.3,I5)') 'Final printout y: ',current_y%value,int(current_y%index)
		IF (switch_4) WRITE(*,'(A)') '----------'

	END SUBROUTINE readmat

	REAL FUNCTION inner_prod(xv,yv)
		implicit none

		INTEGER(i5)				:: i,j
		REAL(long)				:: tmp=0._long
		REAL(long),ALLOCATABLE	:: xv(:,:),yv(:,:)
! SWITCH
		CHARACTER(LEN=1)		:: switch5
		LOGICAL					:: switch_5
!

		WRITE(*,'(A)') 'Print the inner product calculation steps? (y/n)'
		READ(*,'(A)') switch5
		SELECT CASE (switch5)
			CASE ('y','Y')
				switch_5 = .TRUE.
			CASE ('n','N')
				switch_5 = .FALSE.
			CASE DEFAULT
				switch_5 = .FALSE.
		END SELECT

		IF (switch_5) THEN
			WRITE(*,'(A)') '  x  |  y  | inner product'
			WRITE(*,'(A)') '---------------------------'
		END IF

		DO i=1,SIZE(xv,1)
			DO j=1,SIZE(yv,1)
				IF ( int(xv(i,2)) .gt. int(yv(j,2)) ) THEN
					CYCLE
				ELSE IF ( int(xv(i,2)) .eq. int(yv(j,2)) ) THEN
					tmp=tmp+(xv(i,1)*yv(j,1))
!
					IF (switch_5) WRITE(*,'(2I5,f10.1)') int(xv(i,2)),int(yv(j,2)),tmp		
!				
				END IF
			END DO
		END DO

		inner_prod=tmp
		IF (switch_5) WRITE(*,'(A)') '----------'

	END FUNCTION inner_prod

END MODULE mat

PROGRAM sparse_inner_prod
	use precision
	use mat
	implicit none

	REAL(long),ALLOCATABLE	:: a(:,:),b(:,:)
	REAL(long)				:: prod
	INTEGER(i5)				:: size_vec
! SWITCHES
	CHARACTER(LEN=1)		:: switch1
	LOGICAL					:: switch_1
!
	print*,'Write new sparse vectors? (y/n)'
	READ(*,'(A)') switch1

	SELECT CASE (switch1)
		CASE ('y','Y')
			switch_1 = .TRUE.
		CASE ('n','N')
			switch_1 = .FALSE.
		CASE DEFAULT
			switch_1 = .FALSE.
	END SELECT

	IF (switch_1) THEN
		WRITE(*,'(A)') 'Type in the size of these vectors: '
		READ(*,*) size_vec
		CALL writemat(size_vec)
	END IF

	CALL readmat(a,b)
	prod = inner_prod(a,b)

	WRITE(*,'(A,F10.1)') 'The inner product of sparse vectors x and y is: ', prod

END PROGRAM sparse_inner_prod