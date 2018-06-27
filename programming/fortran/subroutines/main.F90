program subroutines
  use laplacian_mod
  implicit none
! TODO: define the arrays
  integer :: nx, ny,i
  real, dimension(:,:),allocatable:: previous, current

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(previous(nx,ny), current(nx,ny))

  ! initialize the array
  call initialize(previous)
!  do i = 1,nx
!  write(*,*) previous(i,1:nx)
!  end do
write(*,*)'Intial array'  
call write_field(previous)

  ! compute the Laplacian
  call laplacian(current, previous)
!  write(*,*) 'Laplacian'
!  do i = 2,nx-1
!  write(*,*) current(i,1:nx)
!  end do

  ! print the result array
  write(*,*)'Laplacian array'  
  call write_field(current)
 
end program subroutines

