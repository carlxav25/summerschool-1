program fibonacci
 implicit none
 integer ::  f0, f1, fn
 
 f0 = 0 
 f1 = 1
 fn = f0 + f1
 
 do while(fn.lt.100) 
 
  write(*,*)fn
  ! swap
  f0 = f1
  f1 = fn
  fn = f0 + f1 
!  write(*,*)fn
 end do

end program
