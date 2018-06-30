program hello_world
USE MPI
 implicit none
  integer :: rc, rank , size

 call mpi_init(rc)
 call mpi_comm_size(mpi_comm_world, size,rc)
 call mpi_comm_rank(mpi_comm_world, rank,rc)
 
 !write(*,*) 'Hello World !','','from process rank', rank ,'','Number of processes', size

 if (rank == 0) then
   write(*,*) 'Hello World!','Rank' , rank, 'processes=', size
  ! write(*,*) 'The rank in the loop is ', rank 
  ! write(*,*) 'The total number of processes equals to = ', size
 else
   write(*,*) 'Rank is = ', rank
   write(*,*) 'Not in main if'
 end if

 call mpi_finalize(rc)


end program

