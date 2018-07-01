program coll_exer
  use mpi
  implicit none

  integer, parameter :: n_mpi_tasks = 4
  
  integer :: ntasks, rank, ierr, i, color, sub_comm, subid
  integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf
  integer, dimension(2*n_mpi_tasks**2) :: printbuf
  integer, dimension(0:n_mpi_tasks-1) ::  counts, displs

  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

  if (ntasks /= n_mpi_tasks) then
     if (rank == 0) then
        print *, "Run this program with ", n_mpi_tasks, " tasks."
     end if
     call mpi_abort(MPI_COMM_WORLD, -1, ierr)
  end if
 
  counts(0:3) = [1,1,2,4]
  displs(0:3) = [0,1,2,4] 

  ! Initialize message buffers
  call init_buffers

  ! Print data that will be sent
  call print_buffers(sendbuf)

  ! TODO: use a single collective communication call (and maybe prepare
  !       some parameters for the call)
! Bcast
!   call mpi_bcast(sendbuf,2*n_mpi_tasks,mpi_integer,0,mpi_comm_world,ierr)

!scatter exer b
! call mpi_scatter(sendbuf, 2,  mpi_integer, recvbuf, 2, &
!                    mpi_integer,0, mpi_comm_world, ierr)
 
! gatherv 
! call mpi_gatherv(sendbuf,counts(rank), mpi_integer, recvbuf,counts,displs,&
!                    mpi_integer,1, mpi_comm_world, ierr )
! call mpi_ALLTOALL(sendbuf,2, mpi_integer, recvbuf,2,&
!                    mpi_integer, mpi_comm_world, ierr )

if(rank/2 ==0) then
   color = 1
else
  color = 2
end if

call MPI_Comm_split(mpi_comm_world, color, rank, sub_comm, ierr)
call mpi_comm_rank(sub_comm,subid,ierr)

call mpi_reduce(sendbuf, recvbuf,2*n_mpi_tasks, mpi_integer,&
                 mpi_sum,0,sub_comm,ierr) 
   






 ! Print data that was received
  ! TODO: add correct buffer
  call print_buffers(recvbuf)

  call mpi_finalize(ierr)

contains

  subroutine init_buffers
    implicit none
    integer :: i

    do i = 1, 2*n_mpi_tasks
       recvbuf(i) = -1
       sendbuf(i) = i + 2*n_mpi_tasks * rank - 1
    end do
  end subroutine init_buffers


  subroutine print_buffers(buffer)
    implicit none
    integer, dimension(:), intent(in) :: buffer
    integer, parameter :: bufsize = 2*n_mpi_tasks
    integer :: i
    character(len=40) :: pformat

    write(pformat,'(A,I3,A)') '(A4,I2,":",', bufsize, 'I3)'

    call mpi_gather(buffer, bufsize, MPI_INTEGER, &
         & printbuf, bufsize, MPI_INTEGER, &
         & 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
       do i = 1, ntasks
          write(*,pformat) 'Task', i - 1, printbuf((i-1)*bufsize+1:i*bufsize)
       end do
       print *
    end if
  end subroutine print_buffers

end program coll_exer
