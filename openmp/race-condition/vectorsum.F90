program vectorsum
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex, sumcri, asum
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

   sum  = 0
   psum = 0
   asum = 0
   
  
  ! TODO: Parallelize the computation

 !$OMP PARALLEL Shared(vecA, sum) private(i)
 !$omp do
  do i = 1, nx
     sum = sum + vecA(i)
  end do
!$omp end do
!$omp end parallel
  write(*,*) 'Sum without reduction: ', sum

 !$OMP PARALLEL Shared(vecA) private(i) REDUCTION(+:psum)
 !$omp do
  do i = 1, nx
     psum = psum + vecA(i)
  end do
!$omp end do
!$omp end parallel
  write(*,*) 'Sum with reduction: ', psum

 !$OMP PARALLEL Shared(vecA, asum) private(i,sumcri)
  sumcri = 0
 !$omp do
  do i = 1, nx
     sumcri = sumcri + vecA(i)
  end do
!$omp end do
 !$omp critical(dosum) 
  asum = asum + sumcri
!$omp end critical(dosum)
!$omp end parallel
  write(*,*) 'Sum with critical: ', asum

end program vectorsum
