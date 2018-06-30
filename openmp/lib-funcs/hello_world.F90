PROGRAM hello
 use omp_lib
 integer:: nid, ath

  !$omp parallel private(nid, ath)
   ath = omp_get_thread_num()  
   nid = omp_get_num_threads()

 write(*,*) 'active Threads', ath
!$omp single
 write(*,*) 'IN omp single nthread is', nid, 'and the active thread is', ath
!$omp end single

!$omp critical(iid)
  write(*,*) 'number of threads', nid, '&&' ,'Active thread', ath
!$omp end critical(iid)
   !$omp end parallel

end program
 
