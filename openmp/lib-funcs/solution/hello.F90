program hello
  use omp_lib
  implicit none
  integer :: tid, nthreads

  print *, 'Hello world!'
  !$omp parallel private(tid) shared(nthreads)
  tid = omp_get_thread_num()
   PRINT*, TID
  !$omp single
  nthreads = omp_get_num_threads()
  print*, nthreads,'tid', tid
  !$omp end single
  !$omp critical
  print *, '  ... from thread ID', tid
  !$omp end critical
  !$omp end parallel
  print *, 'There were', nthreads, 'threads in total.'

end program hello
