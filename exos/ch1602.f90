INTEGER(KIND=4) FUNCTION FACT010(n)
	implicit none

	INTEGER(KIND=4),INTENT(IN)	:: n
	INTEGER(KIND=4)				:: i,tmp

	tmp=1
	IF ( n .gt. 1 ) THEN
		DO i=1,n
			tmp=tmp*i
		END DO
	END IF

	FACT010=tmp

END FUNCTION FACT010

RECURSIVE INTEGER(KIND=4) FUNCTION RFACT010(n) RESULT(answer)
	implicit none

	INTEGER(KIND=4)	:: n

	IF ( n == 1 .OR. n == 0 ) THEN
		answer = 1
	ELSE IF ( n .gt. 1 ) THEN
		answer=n*RFACT010(n-1)
	END IF

END FUNCTION RFACT010

REAL(KIND=16) FUNCTION BIGFACT()
	implicit none

	INTEGER(KIND=1)	:: i
	REAL(KIND=16)	:: tmp

	tmp=1
	DO i=1,76
		tmp=tmp*i
	END DO
	BIGFACT=tmp

END FUNCTION BIGFACT

RECURSIVE REAL(KIND=16) FUNCTION RBIGFACT(i) RESULT(answer)
	implicit none

	INTEGER(KIND=1)	:: i

	IF ( i == 1 ) THEN
		answer=1. 
	ELSE IF ( i .gt. 1  ) THEN
		answer=i*RBIGFACT(i-1_1)	! 1_1 specify nonstandard KIND b/c i is KIND=1, otherwise get mismatch
	END IF

END FUNCTION RBIGFACT

REAL(KIND=8) FUNCTION xton(x,n)
	implicit none

	INTEGER(KIND=1),INTENT(IN)	:: n
	INTEGER(KIND=1)				:: i
	REAL(KIND=8),INTENT(IN)		:: x
	REAL(KIND=8)				:: tmp

	tmp=1.
	DO i=1,n
		tmp=tmp*i
	END DO
	xton=(x**n)/tmp

END FUNCTION xton

RECURSIVE REAL(KIND=8) FUNCTION rxton(x,i) RESULT(answer)
	implicit none

	INTEGER(KIND=1),INTENT(IN)	:: i
	REAL(KIND=8),INTENT(IN)		:: x 
	REAL(KIND=8)				:: tmp

	IF ( i == 1 ) THEN
		answer=x/i
	ELSE IF ( i .gt. 1 ) THEN
		answer=(x/i)*rxton(x,i-1_1)
	END IF

END FUNCTION rxton

PROGRAM eval
	implicit none

	INTEGER(KIND=1)	:: n
	INTEGER(KIND=4)	:: i,FACT010,RFACT010
	REAL(KIND=16)	:: BIGFACT,RBIGFACT
	REAL(KIND=8)	:: xton,rxton,x

	print*
	print*, 'Factorial 0 to 10, looped and recursive functions'
	DO i=0,10
		WRITE(*,*) FACT010(i),RFACT010(i)
	END DO
	print*, '-----------------------------'

	print*, 'Factorial 76, looped and recursive function'
	WRITE(*,'(E10.4)') BIGFACT()
	WRITE(*,'(E10.4)') RBIGFACT(76_1)	! Default integer kind is 4, must specify when passing nonstandard byte nbr.
	print*, '-----------------------------'

	x=13.2_8;n=20_1
	print'(A,f6.2,A,I3)', ' (x**n)/n!, looped and recursive functions where x =',x,', n =',n
	WRITE(*,'(f9.3)') xton(x,n)	
	WRITE(*,'(f9.3)') rxton(x,n)	
	print*, '-----------------------------'

END PROGRAM eval