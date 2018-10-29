module thomas
implicit none 
contains
subroutine tmsalg(n, a, b, c, d, x )
implicit none
integer :: i, j, k, n
real(8):: a(n), b(n), c(n), d(n), x(n)
real(8) :: m

do i=2, n
  m = a(i)/b(i-1)
  b(i) = b(i)-m*c(i-1)
  d(i) = d(i)-m*d(i-1)
end do

x(n) = d(n)/b(n)

do j =n-1 , 1, -1
  x(j) = (d(j)-(c(j)*x(j+1)))/b(j)
end do
end subroutine tmsalg
end module thomas
