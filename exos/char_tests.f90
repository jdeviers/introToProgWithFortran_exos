PROGRAM char_tests

	implicit none

	CHARACTER(LEN=5)	:: first,second
	CHARACTER(LEN=11)	:: third

	first='Three'
	second='blind'

	third=first//second
	print*, third


END PROGRAM char_tests