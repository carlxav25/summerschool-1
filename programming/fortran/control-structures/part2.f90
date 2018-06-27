program part2
 implicit none
  
  integer :: i
  write(*,*) 'Eneter the value of the integer'
  read(*,*) i
 
! control structure

main_if: if (i .lt.0) then
          Write(*,*) i,'Less than 0'
         elseif (i.eq.0) then 
          Write(*,*) i,'Equal to 0'
         elseif (i.gt.100) then 
          Write(*,*) i,'Larger than 100'
         else
          Write(*,*) i,'Somewhere over the horizon'
         end if main_if

        

end program
