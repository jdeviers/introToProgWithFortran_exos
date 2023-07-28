PROGRAM ch0702
!
! This program reads in three numbers and sums
! and averages them
!
IMPLICIT NONE
REAL :: N1,N2,N3,Average = 0.0, Total = 0.0
INTEGER :: N = 3
PRINT *,' Type in three numbers.'
PRINT *,' Separated by spaces or commas'
READ *,N1,N2,N3
Total= N1 + N2 + N3
Average=Total/N
PRINT *,'Total of numbers is ',Total
PRINT *,'Average of the numbers is ',Average
END PROGRAM ch0702