
! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    use mpi

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel
    integer :: status(MPI_status_size)
    integer :: ierr

    ! TODO start: implement halo exchange
    ! Send to left, receive from right
   call mpi_sendrecv(field0%data(:,1),field0%nx,mpi_double_precision,parallel%nleft, &
                         100,field0%data(:,field0%ny+1), field0%nx,mpi_double_precision, &
                            parallel%nright,100,mpi_comm_world,status,ierr) 
    ! Send to right, receive from left
   call mpi_sendrecv(field0%data(:,field0%ny),field0%nx,mpi_double_precision,parallel%nright, &
                         100,field0%data(:,1), field0%nx,mpi_double_precision, &
                            parallel%nleft,100,mpi_comm_world,status,ierr) 

    ! TODO end

  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value

  
  subroutine evolve(curr, prev, a, dt)
   use omp_lib
   use heat
    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

!$omp parallel private(i,j) shared(nx, ny,curr, prev,dt) firstprivate(a)  
!$omp do 
    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
              & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
!$omp end do
!$omp end parallel

  end subroutine evolve

end module core
