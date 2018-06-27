module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
! TODO: implement a subroutine that initializes the input array
  implicit none
  real, dimension(:,:),intent(inout):: field0
  integer::nx, ny
  integer ::  i,j
  real :: x,y
 ! real, dimension(nx,ny),intent(inout):: field0
  nx = size(field0,1)
  ny = size(field0,2)
 
! allocate(field0(nx,ny))

  y = 0.0
outer_loop: do j = 1,ny
              x = 0.0
              do i =1,nx
               field0(i,j) = x**2 + y**2
               x = x +dx     
              end do
   y = y + dy
end do outer_loop
  
  end subroutine initialize
   
  subroutine laplacian(curr, prev)
! TODO: insert a subroutine that computes a laplacian of the
! array "prev" and returns it as an array "curr"

  implicit none
  integer::nx, ny
  integer ::  i,j
  real, dimension(:,:),intent(inout):: prev,curr
  nx = size(curr, 1)
  ny = size(curr,2)

!  allocate(prev(nx,ny), curr(nx,ny))
  curr = 0 

  do  i = 2,nx-1
     do j = 2,ny-1
    
       curr(i,j) = (prev(i-1,j) - 2*(prev(i,j)) +prev(i+1,j))/(dx**2) + &
                 (prev(i,j-1) - 2*(prev(i,j)) +prev(i,j+1))/(dy**2)
   
    end do  
  end do


  end subroutine laplacian

  subroutine write_field(array)
! TODO: write a subroutine that prints "array" on screen

  implicit none
  integer::nx, ny
  integer ::  i
  real, dimension(:,:),intent(inout):: array
  nx = size(array, 1)
  ny = size(array, 2)

 
  do i = 2,nx-1
  write(*,*) ARRAY(i,1:ny)
  end do


  end subroutine write_field

end module laplacian_mod
