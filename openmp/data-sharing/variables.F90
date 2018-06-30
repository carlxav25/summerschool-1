program exer1
  implicit none
  integer :: var1, var2
  var1 = 1
  var2 = 2 
     
  !$omp parallel private(var1, var2)
   
  print *, 'Region 1:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  print *, 'Region 1 after increment:       var1=', var1, 'var2=', var2
  !$omp end parallel
  
  print *, 'After private region 1: var1=', var1, 'var2=', var2
  write(*,*)
  
  !$omp parallel firstprivate(var1, var2)
   
  print *, 'Region 1:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  print *, 'Region 1 after increment:       var1=', var1, 'var2=', var2
  !$omp end parallel
  !end here :)
  print *, 'After first private region 1: var1=', var1, 'var2=', var2
  write(*,*)
  
  !$omp parallel shared(var1, var2)
   
  print *, 'Region 1:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  print *, 'Region 1 after increment:       var1=', var1, 'var2=', var2
  !$omp end parallel
  !end here :)
  print *, 'After shared region 1: var1=', var1, 'var2=', var2
  write(*,*)
end program exer1
