program loop
  implicit none
  
integer :: num
  
  write(*,*) 'Enter the number'
  read(*,*)  num
  
  if (num.le.0) then
   write(*,*), 'Number is negative' 
  elseif(num.eq.0) then
   write(*,*), 'Number is 0'
  elseif(num.gt.100) then
    write(*,*), 'Number is > 100'
  else
    write(*,*), 'Number is > 0 and <100'
  end if  
  
 
  
end program loop

