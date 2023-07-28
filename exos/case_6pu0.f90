PROGRAM case_6pu0

	implicit none

	INTEGER(KIND=1)		:: chg,io
	INTEGER(KIND=2)		:: i,resid,mem
	CHARACTER(LEN=3)	:: resname

100 FORMAT(17X,A3,3X,I3,40X)
	chg=0
	mem=0

	OPEN(10,file='allfram_7HIP.pdb',status='old',action='read')
	DO
		READ(10,100,iostat=io) resname,resid
		IF ( io /= 0 ) EXIT
		IF (resid == mem+1 ) THEN
			SELECT CASE (resname)

				CASE ('ARG')
					chg=chg+1
				CASE ('LYS')
					chg=chg+1
				CASE ('HIP')
					chg=chg+1
! The above block is equivalent to:
!			IF (resname=='ARG' .OR. resname=='LYS' .OR. resname=='HIP') THEN
!				chg=chg+1
! Which is a bit less legible. Same efficiency though.

				CASE ('GLU')
					chg=chg-1
				CASE ('ASP')
					chg=chg-1

				CASE ('FAD')
					EXIT

				CASE DEFAULT	! Do nothing; could be omitted

			END SELECT
		END IF
		mem=resid		
	END DO
	CLOSE(10)

	print*, 'Final charge is ',chg

END PROGRAM case_6pu0