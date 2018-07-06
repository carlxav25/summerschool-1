program hello
  use omp_lib
  use mpi
  implicit none
  integer :: rank, tid, rc, size
  integer :: provided, required=MPI_THREAD_MULTIPLE

  ! TODO: Initialize MPI with thread support.
 call mpi_init_thread(required, provided,rc)
 call mpi_comm_size(mpi_comm_world, size,rc)
 call mpi_comm_rank(mpi_comm_world, rank,rc)

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.

 !$omp parallel private(tid)
 tid = omp_get_thread_num() 

  write(*,*) 'Hello World! by process' , rank, '& thread', tid
!$omp end parallel

  ! TODO: Investigate the provided thread support level.
  
  if (required==mpi_thread_funneled) then
    write(*,*) 'tHREAD LEVEL SUPPORT IS', REQUIRED
  end if

  call MPI_Finalize(rc)
end program hello
