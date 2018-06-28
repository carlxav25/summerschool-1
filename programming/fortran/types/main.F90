program test_module
 
Use heat_module
Use ISO_FORTRAN_ENV, ONLY : REAL64

 implicit none
  
real(kind=Real64), dimension(:,:), allocatable ::  field0
integer :: nxx, nyy
type(temp_field) ::  field

Write(*,*) 'Enter the grid point values'
read(*,*) nxx, nyy

call input(nxx, nyy, field)
allocate(field0(field%nx, field%ny))

write(*,*) field%nx
write(*,'(f6.1)') field%dx
write(*,*) size(field0)
write(*,*) field%ny
write(*,'(f6.1)') field%dy
write(*,*) shape(field0)

end program
