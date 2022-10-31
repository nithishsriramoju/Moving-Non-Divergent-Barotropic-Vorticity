!     Program to moving non-diveregnt barotropic vorticity stream fuction evolution in 2-10th Time Evolution.
!     Record of Revisions
!     Date                 Programmer              Description of change
!     =====                ==========              =====================
!   10/04/2021        Nithish Kumar Sriramoju            Original Code
program l
use poi
implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       intiating variables
	integer::i,j,n,z
	real, dimension(101,101) :: psi
	real, dimension(101,101) :: psi1
	real, dimension(101,101) :: chi
	character*20::f
	n = 0
do z=1,9
	write(f,"(a,i1,a)") "psi",z,".txt"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
!       reading stream funtion values from text file
	open (1,file = f,status = 'old')
	do i = 1,101
   		read(1,*) ( psi(i,j), j=1,101 )
    	end do
    	close(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    	do i = 1,101
    		do j=1,101
	   		psi1(i,j)=0
	   	end do
    	end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calling poisson solver 
   	call calc_poison(chi,psi,n)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! evolving stream function
	do i  = 2,99
  		do j = 2,99
			psi1(i,j) = psi(i,j)+480*chi(i,j)
   		end do
  	end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! writing values of evolved stream function
	write(f,"(a,i1,a)") "psi",z+1,".txt"
	open(2,file = f, status = 'unknown')
    	do i = 1,101
   		write(2,*) ( psi1(i,j), j=1,101 )
   	end do
   	close(2)
end do			
end program
