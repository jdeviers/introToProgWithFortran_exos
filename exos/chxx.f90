MODULE chxx_mod
	implicit none

	INTEGER(KIND=4)					:: cnt
	INTEGER(KIND=4),DIMENSION(1:10)	:: stack

END MODULE chxx_mod

PROGRAM chxx
!
! Variables can be passed from prog to subr using a module, no interface needed.
! Variables in a module have an implicit SAVE attribute.
!
	use chxx_mod
	implicit none

	INTEGER(KIND=4)	:: val
	stack=(/9,8,7,6,5,4,3,2,1,0/)
	cnt=0
	READ(*,*) val
	print*, 'cnt=', cnt
	CALL push(val)
	print*,'cnt=', cnt
	CALL pop(val)

	contains

	SUBROUTINE push(val)
		use chxx_mod
		implicit none

		INTEGER(KIND=4)	:: val

		cnt=cnt+1
		print*, stack(cnt)
		stack(cnt) = val
		print*,stack(cnt)
	END SUBROUTINE push

	SUBROUTINE pop(val)
		use chxx_mod
		implicit none

		INTEGER(KIND=4)	:: val

		val=stack(cnt)
		cnt=cnt+1
	END SUBROUTINE pop
END PROGRAM chxx