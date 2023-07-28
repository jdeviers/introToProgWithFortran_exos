PROGRAM ch1502

	implicit none

	INTEGER(KIND=1)		:: i
	REAL*16,PARAMETER	:: pi=3.14159265358979323846264338327950288419716939938

100 FORMAT(f8.3,'    sin(i)=',f8.3,'    cos(i)=',f8.3,'    tan(i)=',f8.3)

	OPEN(10,file='real4.dat',status='unknown',action='write')
	DO i=-1,91
		WRITE(10,100) 2.0_4*pi*real(i,4)/360.0_4,sin(real(2.0_4*pi*i/360.0_4,4)),&
		cos(real(2.0_4*pi*i/360.0_4,4)),tan(real(2.0_4*pi*i/360.0_4,4))
	END DO
	CLOSE(10)

	OPEN(10,file='real8.dat',status='unknown',action='write')
	DO i=-1,91
		WRITE(10,100) 2.0_8*pi*real(i,8)/360.0_8,sin(real(2.0_8*pi*i/360.0_8,8)),&
		cos(real(2.0_8*pi*i/360.0_8,8)),tan(real(2.0_8*pi*i/360.0_8,8))
	END DO
	CLOSE(10)

	OPEN(10,file='real16.dat',status='unknown',action='write')
	DO i=-1,91
		WRITE(10,100) 2.0_16*pi*real(i,16)/360.0_16,sin(real(2.0_16*pi*i/360.0_16,16)),&
		cos(real(2.0_16*pi*i/360.0_16,16)),tan(real(2.0_16*pi*i/360.0_16,16))
	END DO
	CLOSE(10)

END PROGRAM ch1502