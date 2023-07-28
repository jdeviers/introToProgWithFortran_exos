RECURSIVE SUBROUTINE rec(n,a,b)
	implicit none
!
! tmp2 gets converges properly to exp(a) in n iterations, but the return variable is b,
! and whenever b is assigned the value of tmp2, then it is the value of b or of tmp2 which
! is fed into rec instead of tmp1 for some reason, and tmp2 diverges.
!
	INTEGER(KIND=1)					:: n
	REAL(KIND=8),INTENT(IN)			:: a
	REAL(KIND=8)					:: b
	REAL(KIND=8),SAVE				:: tmp1,tmp2

	IF ( n == 0_1 ) THEN
		tmp1 = 1._8
		tmp2 = 0._8
		print*, tmp1, tmp2
	ELSE
		CALL rec(n-1_1,a,tmp1)
		tmp2=tmp2+(a/n)*tmp1
		tmp1=(a/n)*tmp1
!		b=tmp2
		print'(4f8.2)',tmp1,tmp2,(a/n),b	
	END IF

END SUBROUTINE rec

PROGRAM etox
	implicit none

	INTEGER(KIND=1)	:: i
	REAL(KIND=8)	:: x,res1,res2

	READ(*,*) i,x

	CALL rec(i,x,res1)
	WRITE(*,*) res1+1._8, exp(5._8)

END PROGRAM etox