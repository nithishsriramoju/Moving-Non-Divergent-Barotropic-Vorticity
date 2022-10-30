module poi
implicit none
contains
	subroutine calc_poison(psi,zeta,m)
	integer :: i,j,m
	real::beta = 1.5
	real :: b
	real,dimension(101,101) :: psi
	real,dimension(101,101) :: zeta
	real,dimension(101,101) :: g
	real,dimension(101,101) :: k
	real,dimension(101,101) :: ps
	b = 2.1e-11
	m = 0
	do i =1,100
   		do j =1,100
   			k(i,j)=0
   		end do
   	end do
   	
	do
		do i =1,100
   			do j =1,100
   				g(i,j)=k(i,j)
   			end do
   		end do
		do i = 2,99
			do j = 2,99
				k(i,j) = (zeta(i+1,j)+zeta(i-1,j)+zeta(i,j-1)+zeta(i,j+1)-4*zeta(i,j))/(20*1000)**2
			end do
		end do
		if ( k(2,2)-g(2,2) <= 0.000001) exit
	end do
	do 
		m=m+1
		do i =1,100
   			do j =1,100
   				g(i,j)=psi(i,j)
   			end do
   		end do 
   		do i  = 2,99
  			do j = 2,99
				psi(i,j)=beta*(g(i+1,j)+psi(i-1,j)+g(i,j+1)+psi(i,j-1)-b*0.5*(zeta(i+1,j)-zeta(i-1,j))*(20*1000)+5*0.5*(k(i+1,j)-k(i-1,j))*(20*1000))/4+(1-beta)*g(i,j)
   			end do
  		end do
  		if ( psi(2,2)-g(2,2) <= 0.000001) exit
   	end do
   	print*,m
   	end subroutine
end module
	
