module nu_turbulent
implicit none 
contains

subroutine nu_turbu(YPAD,U,N1,H,nu,nu_turb,Ub,ut)

integer :: i, j,k,l,N1,m,n,o,p,q
real(8)::YPAD(N1),U(N1),nu_turb(N1),lm(N1),lo(N1),A(N1),ut(N1)
real(8) :: nu,H,YTAB,Ub,UTAB

YTAB=(YPAD(n1-1)+YPAD(n1))/2.0
UTAB=(U(n1-1)+U(n1))/2.0

n=int(n1/2)

!ut(1)=0
!ut(n1)=0

!print*,U(2),YPAD(2)
do o=1,n
ut(o)=(abs((U(2)-U(1))*nu/(2*(YPAD(2)-YPAD(1)))))**0.5
end do

do m=n+1,n1
ut(m)=(abs((U(N1)-U(N1-1))*nu/(2*(YPAD(N1)-YPAD(N1-1)))))**0.5
end do


!print*,ut

!A(1)=0
!A(n1)=0

do l=1,n1
A(l)=26*nu/(ut(l))  
end do

!print*,A
!lo(1)=0
!lo(n1)=0
do k=1,n
lo(k)=H*0.5*(0.21-0.43*(1-(YPAD(k)*2/H))**4+0.22*(1-(YPAD(k)*2/H))**6)
end do

do p=n+1,n1,1
 lo(p) = H*0.5*(0.21-0.43*(1-(abs(YTAB-YPAD(p))*2/H))**4+0.22*(1-(abs(YTAB-YPAD(p))*2/H))**6)
end do
!print*,lo
!lm(1)=0
!lm(n1)=0
do j=1,n
lm(j)=lo(j)*(1-exp(-(YPAD(j)/A(j))))
end do

do q=n+1,n1
lm(q)=lo(q)*(1-exp(-(abs(YTAB-YPAD(q))/A(q))))
end do

!print*,lm
!nu_turb(1)=0 
!nu_turb(n1)=0
do i=1,n1
nu_turb(i)=(lm(i)**2)*abs((U(i+1)-U(i))/(YPAd(i+1)-YPAD(i)))
end do


end subroutine nu_turbu
end module nu_turbulent
