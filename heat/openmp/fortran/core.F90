! Main solver routines for heat equation solver
module core

contains

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    use heat
    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny
    real :: dyy, dxx     
    real(dp),dimension(:,:), allocatable :: Lap   

    nx = curr%nx
    ny = curr%ny
    dyy = curr%dy
    dxx = curr%dx 



    ! TODO: implement the heat equation update
 

    allocate(Lap(nx, ny))
   Lap = 0

!!!!!$omp parallel private(i,j) 
!!!!!$omp shared(nx, ny, Lap, curr, prev,dt)
!!!!!$omp firstprivate(a)
 !$omp do 
   do i  = 2,nx-1
     do j = 2,ny-1
    
       Lap(i,j) = (prev%data(i-1,j) - 2*(prev%data(i,j)) +prev%data(i+1,j))/(dxx**2) + &
                 (prev%data(i,j-1) - 2*(prev%data(i,j)) +prev%data(i,j+1))/(dyy**2)
   
    end do  
  end do
 !$omp end do

 !$omp do 
 do i = 1, nx
     do j = 1,ny 
       curr%data(i,j) = prev%data(i,j) + a * dt * Lap(i,j)
     end do
 end do
                               
 !$omp end do
!!!!!$omp  end parallel

  end subroutine evolve

end module core
