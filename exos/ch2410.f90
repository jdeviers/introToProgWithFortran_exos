MODULE precision
	implicit none

	INTEGER,PARAMETER	:: long=SELECTED_REAL_KIND(15,307)

END MODULE precision

MODULE math_ctes

	use precision

	implicit none

	REAL(KIND=long),PARAMETER	:: pi=3.1415926535897932384626433832795

END MODULE math_ctes

MODULE fcts

	use precision
	use math_ctes

	implicit none	

	contains

	REAL(KIND=long) FUNCTION vol(a,b)
		implicit none

		REAL(KIND=long)	:: a,b

		vol=pi*a*a*b

	END FUNCTION vol

	REAL(KIND=long) FUNCTION base(a)
		implicit none

		REAL(KIND=long)	:: a

		base=pi*a*a

	END FUNCTION base

END MODULE fcts

PROGRAM ch2410

	use precision
	use fcts

	implicit none

	REAL(KIND=long)	:: r,l

	print*, 'Enter radius and height, in m.'
	READ(*,*) r,l

	WRITE(*,'(2(A,f10.4),A)') 'The volume is ',vol(r,l),' cubic metres and the base is ',base(r),' square metres.'

END PROGRAM ch2410