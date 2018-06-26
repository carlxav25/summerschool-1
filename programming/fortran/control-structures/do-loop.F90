program loops
  implicit none
  ! TODO define parameters nx and ny
  integer :: nx, ny
  ! TODO: define real-valued array A
  integer :: i, j
  real, dimension(:,:), allocatable :: A
  real :: x, y, delx, dely
  
  write(*,*) 'Enter the value for the 2 dimensions nz and ny'
  read*, nx
  read*, ny
  
  
  ! TODO initialize array A here
 allocate(A(nx,ny)) ! allocation
 
 delx = 1.0/nx
 dely = 1.0/ny
 
 y = 0.0
  
 do j = 1,ny
 
   x = 0.0
 
     do i=1,nx
  
     A(i,j) = x**2 +y**2 
     x = x + delx   
   
    end do
 
    y = y + dely
 end do  
	
 
 
 

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 1, nx
     write(*, '(12F6.1)') A(i,:)
  end do

end program loops
