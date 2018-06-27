program loops
  implicit none


  ! TODO define parameters nx and ny
   integer,parameter :: nx=10, ny=10

  ! TODO: define real-valued array A
  real, dimension(nx,ny) :: A
  integer :: i, j
  real :: x,y, valx, valy
 
 write(*,*) nx, ny
  
 !read data
! write(*,*) 'Enter the value of the 2 arrays'
! read(*,*) nx, ny
 
 valx = 1./nx
 valy = 1./ny
 write(*,*) valx 
 write(*,*) valy
  ! TODO initialize array A here
!  y = 0.0 
   
 first_loop: do j = 1,ny
               x = 0.0        
               y = 0.0 
                inner_loop: do i =1,nx
                 A(i,j) = x**2 + y**2
                 x = x + valx     
                 y = y + valy     
                end do inner_loop
 !   y = y + valy
  end do first_loop

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 1, nx
     write(*, '(12F6.1)') A(i,:)
  end do

end program loops
