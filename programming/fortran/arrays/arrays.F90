program arrays
  implicit none
  ! TODO: Define the array A
  real, dimension(:,:), allocatable :: A 
  real :: x, y, valx, valy
  integer :: nx, ny, i, j, alloc_stat

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  valx = 1.0/nx
  valy = 1.0/ny

  ! TODO: allocate the array A
  allocate(A(nx,ny))
  

  ! TODO: initalize the array A
y = 0.0

outer_loop: do j = 1,ny
              x = 0.0
!              y = 0.0
               do i=1,nx
                A(i,j) = x**2 + y**2 
                x = x + valx
!                y = y + valy 
               end do
   y = y + valy
end do  outer_loop   
 

  ! TODO: Print out the array

do i = 1,nx
 write(*,*) A(i,:)
end do

end program arrays
