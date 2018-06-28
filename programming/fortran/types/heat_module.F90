module heat_module
 Use ISO_FORTRAN_ENV, ONLY : REAL64
 implicit none
  
 type temp_field

   real(KIND=REAL64) :: dx, dy
   integer ::  nx, ny
   real(Kind=Real64), dimension(:,:), allocatable :: array
  
 end type temp_field  

 contains

  subroutine input(nx, ny, field)
    implicit none
    type(temp_field),intent(out):: field
    integer, intent(in):: nx, ny
   
    !intialize the values of field
   field%dx = 0.1
   field%dy = 0.1
   field%nx = nx
   field%ny = ny
   

  end subroutine input


end module heat_module
  


