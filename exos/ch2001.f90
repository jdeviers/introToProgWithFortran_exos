PROGRAM ch2001
	IMPLICIT NONE
	
	INTEGER(KIND=1)			:: io=0
	INTEGER(KIND=4)			:: i

	TYPE pdb
		CHARACTER(LEN=6)	:: atom='ATOM'	! Default value of atom
		INTEGER(KIND=2)		:: atom_number
		CHARACTER(LEN=5)	:: atom_name
		CHARACTER(LEN=3)	:: residue_name
		INTEGER(KIND=2)		:: residue_number
		REAL(KIND=8)		:: x
		REAL(KIND=8)		:: y
		REAL(KIND=8)		:: z
		REAL(KIND=4)		:: alpha
		REAL(KIND=4)		:: beta	
	END TYPE pdb
	TYPE (pdb),DIMENSION(1:7805,1:10)	:: content	! This is an array (8000x10) of type pdb, i.e which can contain various types of data i.e char, real and integers in this case.

100 FORMAT(A6,1X,I4,1X,A5,A3,3X,I3,5X,3F8.3,2F8.4)

	OPEN(unit=10,file='6pu0.pdb',status='old',action='read',iostat=io)
	DO i=1,7805
		READ(10,100) content(i,1)%atom,content(i,2)%atom_number,content(i,3)%atom_name,content(i,4)%residue_name,&
		content(i,5)%residue_number,content(i,6)%x,content(i,7)%y,content(i,8)%z,content(i,9)%alpha,content(i,10)%beta
	END DO
	CLOSE(10)

	print*, content(498,4)%residue_name


END PROGRAM ch2001