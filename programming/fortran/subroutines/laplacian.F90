module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
 ! integer :: nxx,nyy
contains

  subroutine initialize(field0, nxx, nyy )
    ! TODO: implement a subroutine that initializes the input array
    implicit none
    real :: x,y 
    integer :: i,j
    integer,intent(in) :: nxx, nyy
    real, dimension(nxx,nyy), intent(inout) :: field0
    
    y = 0.0
    
    do j=1,nyy
      x = 0.0
       do i=1,nxx
        field0(i,j)=x**2 + y**2
        x = x + dx
       end do
       y = y + dy
    end do
    
  end subroutine initialize

  subroutine laplacian(curr, prev, nxx, nyy)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
  IMPLICIT NONE
  
  real, dimension(nxx,nyy), intent(inout) :: curr,prev
  integer :: i,j
  integer,intent(in) :: nxx, nyy  
    
    do  i = 2,nxx-1
     do j = 2,nyy-1
    
       curr(i,j) = (prev(i-1,j) - 2*(prev(i,j)) +prev(i+1,j))/(dx**2) + &
                 (prev(i,j-1) - 2*(prev(i,j)) +prev(i,j+1))/(dy**2)
   
    end do  
  end do
  
  end subroutine laplacian

  subroutine write_field(array,nxx,nyy)
    ! TODO: write a subroutine that prints "array" on screen
  
    real, dimension(nxx,nyy), intent(inout) :: array
    integer,intent(in) :: nxx, nyy 
    integer ::i
    
    write(*,*) "Array:" 
   do i = 2, nxx-1
     write(*,*)  array(i,2:nyy-1)
  end do

  end subroutine write_field

end module laplacian_mod
