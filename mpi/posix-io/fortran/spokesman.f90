program pario
  use mpi
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector
  integer :: status(MPI_STATUS_SIZE)
  character(len=30) :: filename
  integer(kind=mpi_offset_kind):: disp

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_writer()
    

    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io

 !call mpi_gather(localvector, localsize, mpi_integer,fullvector,localsize, mpi_integer, &
 !                  2,mpi_comm_world, rc)

 call mpi_file_open(mpi_comm_world, 'mpi_file_write.dat',mpi_mode_create,&
                    mpi_info_null, 25)



! write(FILENAME, '(A9, I4.4, A4)') 'multiple_', my_id, '.dat'
! print *, FILENAME
 !FILENAME = 'multiple_',my_id,'dat'

do i = 1, ntasks
disp(i) = my_id * localsize 
end do

 if (my_id ==2) then
! open(15,file='read_file.dat', position='append')
! write(15,*) fullvector
! close(15)
call mpi_file_write_at(25,disp, fullvector, localsize,mpi_integer,status,rc)

 !write(*,*) fullvector
! else
! open(my_id+20,file=filename,position='append')
! write(my_id+20, *) localsize
! close(my_id+20)
 end if

  end subroutine single_writer


 
end program pario
