PROGRAM ch1701
	implicit none

	CHARACTER(LEN=111)				:: string
	INTEGER(KIND=1)					:: i,j

	string='If a programmer is found to be indispensable, the best thing to do is to get rid of him as quickly as possible.'

	i=1
	DO WHILE ( i+1 .le. LEN(string)+1 )
		SELECT CASE (string(i:i+1))
			CASE ('is','IS')
				print*, 'Occurrence'
			CASE DEFAULT
		END SELECT
		i=i+1
	END DO


END PROGRAM ch1701