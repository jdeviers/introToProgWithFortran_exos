PROGRAM ch902
	implicit none
!
! Call as: ./ch902.x < data.in > data.out
!
	integer,parameter					:: kilos=SELECTED_INT_KIND(2),metres=KIND(1),nb_ppl=10
	integer(kilos)						:: i,j
	integer(kilos),dimension(1:nb_ppl)	:: weight
	real(metres),dimension(1:nb_ppl)	:: height
	real(metres)						:: avgh,avgw,bmi
	character(len=20)					:: status

!	weight(1)=85;weight(2)=76;weight(3)=85;weight(4)=90;weight(5)=69;&
!	weight(6)=83;weight(7)=64;weight(8)=57;weight(9)=65;weight(10)=76
!	height(1)=1.85;height(2)=1.80;height(3)=1.85;height(4)=1.70;height(5)=1.75;&
!	height(6)=1.67;height(7)=1.65;height(8)=1.63;height(9)=1.79;height(10)=1.78

	

	avgw=0;avgh=0.0
	do i=1,nb_ppl

		read(*,'(F4.2,I3)') height(i), weight(i)

		bmi = weight(i)/(height(i)**2.0)

		if (bmi .lt. 20.0) then
			status="Underweight"
		else if (bmi .ge. 20.0 .AND. bmi .le. 24.9) then
			status="Desirable"
		else if (bmi .ge. 25.0 .AND. bmi .le. 29.9) then
			status="Overweight"
		else if (bmi .ge. 30.0 .AND. bmi .le. 39.9) then
			status="Obese"
		else if (bmi .ge. 40.0) then
			status="Morbidly obese"
		else
			status="precision failure"
		end if	

		write(*,'(A5,F5.2,3X,A20)') "bmi: ", bmi, status

		avgw = avgw+weight(i)
		avgh = avgh+height(i)
	end do

	print*, "----------------------"
	avgw=avgw/nb_ppl; write(*,'(A16,F4.1)') "Average weight: ", avgw
	avgh=avgh/nb_ppl; write(*,'(A16,F4.2)') "Average height: ", avgh

END PROGRAM ch902