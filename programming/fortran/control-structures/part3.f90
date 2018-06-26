program loop
  implicit none
  
  real :: f0,f1
  real :: Fn
   
  f0 = 0.0
  f1 = 1.0
  fn = f0 + f1
 
 do while (fn.lt.100)
   write(*,*), Fn
   f0 = f1
   f1 = fn
   fn = f0 + f1
 end do
  
 
  
end program loop

