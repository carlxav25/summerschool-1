program datatype1
  use mpi
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  integer :: i, j, dest
  integer :: rowtype
  integer :: status(MPI_status_size)
  integer :: blocklen(4)
  integer :: displs(4)

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO: create datatype describing one row, use mpi_type_vector
blocklen=(/1,2,3,4/)
displs= (/0,17,34,51/)

!call mpi_type_vector(8,1,8,mpi_integer,rowtype, ierr)
call mpi_type_indexed(10,blocklen,displs,mpi_integer,rowtype, ierr)
call mpi_type_commit(rowtype, ierr)

  !TODO: send first row of matrix from rank 0 to 1
if (rank==0) then
 call mpi_send(array, 1, rowtype, 1,10,mpi_comm_world,ierr)
end if
if (rank==1) then
call mpi_recv(array(:,1), 1, rowtype, 0, 10, mpi_comm_world,status, ierr)
 end if




call mpi_type_free(rowtype, ierr)

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype

  call mpi_finalize(ierr)

end program datatype1
