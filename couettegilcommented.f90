!==================================================================
! Program to simulate 2-d bounded flow between flat plates using mixing length modelling methods.
!==================================================================
! Compile with::::::::::::::::::::::::::::::::::::::::::::::::::::: 
! gfortran -c grid.f90
! gfortran -c nu_turbulent.f90
! gfortran -c thomas.f90
! gfortran nu_turbulent.o thomas.o grid.o couette.f90 -o couette.exe
! ./couette.exe 
!==================================================================

 program couette_poiseulle_main
 use thomas
 use grid
 use nu_turbulent
implicit none

real(8),allocatable::YHAD(:),YPAD(:),a(:),b(:),c(:),d(:),u(:),nu_turb(:),up(:),ut(:)
real(8):: dpdx,Ub,rho,alpha,h,nu,stretch,res!h is the width of the plate,res is residual for velocity computed with turbulent viscosity
real(8)::UQ,REQ,UT1,UT2
character(len=30) :: myFileName
integer::i,j,k,l,m,o,p,N1,N,iter,q!N is the case number

N1=101 !NO.OF POINTS REQUIRED + TWO BOUNDARY CONDITIONS


allocate(YHAD(N1))
allocate(YPAD(N1))
allocate(U(N1))
allocate(a(N1))
allocate(b(N1))
allocate(c(N1))
allocate(d(N1))
allocate(nu_turb(N1))
allocate(UP(N1))
allocate(ut(N1))


do n=1,18

  if (n==1) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=0.0 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid
  print*,'case',n

  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

 elseif (n==2) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=0.808 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==3) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=1.486 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==4) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=1.510 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==5) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=1.960 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    print*,'case',n
    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==6) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=1.430 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=8.59 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==7) then

    nu=0.000015
    H=0.101
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=3.548 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=17.08 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==8) then

    nu=0.000015
    H=0.101
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=2.323 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==9) then

    nu=0.000015
    H=0.101
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=1.212 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=8.59 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==10) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=4.830 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==11) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=7.500 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==12) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=14.30 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==13) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=18.50 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=12.84 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==14) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=18.50 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=8.59 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==15) then

    nu=0.000015
    H=0.066
    stretch=0.01
    !!!!!!!!!!!!!!!calculation of pressure gradient!!!!!!!!!!!!!!!!!!!!!
    alpha=20.80 !high stress parameter from the table
    dpdx=alpha !constant pressure difference calculated based on the parameters 
    !!!!!!!!!!!!!!!end!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Ub=0 !ms-1 of the plate and the liquid

  print*,'case',n
  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==16) then

    nu=0.000015
    stretch=0.01
    UQ=2.50
    REQ=4800
    UT1=0.15
    UT2=0.09
    UB=1.8 !ms-1 of the plate and the liquid


    H=REQ*NU/UQ

    dpdx=abs((UT1**2)-(UT2**2))*(1)/H !constant pressure difference calculated based on the parameters

  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==17) then

    nu=0.000015
    stretch=0.01
    UQ=2.50
    REQ=4700
    UT1=0.14
    UT2=0.04
    UB=3.09 !ms-1 of the plate and the liquid


    H=REQ*NU/UQ

    dpdx=abs((UT1**2)-(UT2**2))*(1)/H !constant pressure difference calculated based on the parameters

  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

  elseif (n==18) then

    nu=0.000015
    stretch=0.01
    UQ=2.55
    REQ=5100
    UT1=0.15
    UT2=0.05
    UB=3.75 !ms-1 of the plate and the liquid


    H=REQ*NU/UQ

    dpdx=abs((UT1**2)-(UT2**2))*(1)/H !constant pressure difference calculated based on the parameters

  print*,'H=',H,'','dpdx=',dpdx,'' ,'Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch

end if
!xxxxxxxxxxxxxxxxxxxxxxxxxxEND OF DEFINITION OF CASESxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




!-----------------------initialising the values for turbulent viscosity and thomas alogrithm-------------------
    call sGrid_Generator(N1,stretch,YPAD,YHAD)
    YPAD=YPAD*H


  iter=0
  res=1

do q=1,N1
    nu_turb(q)=0 !initialising the turbulent viscosity
end do

  do while (iter<10000) 
    !======================PUT SUBROUTINE FOR TURBULENT VISCOSITY==============
    !nut depends on the YPAD,N1,H,nu,ut

    !======================END SUBROUTINE FOR TURBULENT VISCOSITY==============
    do j=2,N1-1
     a(1)=0  !creating A(:) for tridiagonal aray
     a(j)=(nu+nu_turb(j-1))/(YPAD(J)-YPAD(J-1))
     a(N1)=1 !from boundary conditions
     b(1)=1 !from boundary conditions
     b(j)=-(((nu+nu_turb(j))/(YPAD(J+1)-YPAD(J)))+((nu+nu_turb(j-1))/(YPAD(J)-YPAD(J-1)))) !!!!!problem
     b(N1)=1 !from boundary conditions
     c(1)=1
     c(j)=(nu+nu_turb(j))/(YPAD(J+1)-YPAD(J))
     c(N1)=0
     d(1)=0 !from boundary conditions
     d(j)=(dpdx)*(YPAD(j+1)-YPAD(j-1))*(-1)/(2.0)  !!!!!!!!!!!!problem
     d(N1)=2.0*Ub !from boundary conditions
    end do

    call tmsalg(N1, a, b, c, d, Up )
    !Remember thomas needs to be solved from 1 to N1

    call nu_turbu(YPAD,U,N1,H,nu,nu_turb,Ub,ut)

    !======================CALL THOMAS AGAIN FOR U PRIME AND COMPARE THE RESIDUALS=======================
    !IF RESIDUAL IS VERY SMALL PUT U = UPRIME AND END
    res=abs(maxval(U(:))-maxval(Up(:)))

    U(:)=Up(:)


    iter=iter+1
  end do
!print*,res
!print*,iter

    !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    !xxxxxxxwrite data to filexxxxxxxxxxxxxxxx
    !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

if (n==15) then
     write(myFileName, "(A10,I3.3,A4)") 'test_case_',n,'.dat'   !Statement to create the file and name it
     open(n,file=myFileName)
!    write(n,*)'H=',H,'','dpdx=',dpdx,'','Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch
     do l=1,N1
     write(n,*)YPAD(l)/h, U(l)/maxval(U(:)),nu_turb(l),iter,ut(l),res,n1,dpdx,h,u(l)
     end do
     close(n)

elseif (n==16) then
     write(myFileName, "(A10,I3.3,A4)") 'test_case_',n,'.dat'   !Statement to create the file and name it
     open(n,file=myFileName)
!    write(n,*)'H=',H,'','dpdx=',dpdx,'','Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch
     do l=1,N1
     write(n,*)YPAD(l)/h, U(l)/Uq ,nu_turb(l),iter,ut(l),res,n1,dpdx,h,u(l)
     end do
     close(n)

elseif (n==17) then
     write(myFileName, "(A10,I3.3,A4)") 'test_case_',n,'.dat'   !Statement to create the file and name it
     open(n,file=myFileName)
!    write(n,*)'H=',H,'','dpdx=',dpdx,'','Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch
     do l=1,N1
     write(n,*)YPAD(l)/h, U(l)/Uq ,nu_turb(l),iter,ut(l),res,n1,dpdx,h,u(l)
     end do
     close(n)

elseif (n==18) then
     write(myFileName, "(A10,I3.3,A4)") 'test_case_',n,'.dat'   !Statement to create the file and name it
     open(n,file=myFileName)
!    write(n,*)'H=',H,'','dpdx=',dpdx,'','Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch
     do l=1,N1
     write(n,*)YPAD(l)/h, U(l)/Uq ,nu_turb(l),iter,ut(l),res,n1,dpdx,h,u(l)
     end do
     close(n)

else

     write(myFileName, "(A10,I3.3,A4)") 'test_case_',n,'.dat'   !Statement to create the file and name it
     open(n,file=myFileName)
!    write(n,*)'H=',H,'','dpdx=',dpdx,'','Ub=',Ub,'','rho=',rho,'','nu=',nu,'','alpha=',alpha,'','stretch=',stretch
     do m=1,N1
     write(n,*)YPAD(m)/h, U(m)/Ub,nu_turb(m),iter,ut(m),res,iter,n1,dpdx,h,u(l)
     end do
     close(n)
     !xxxxxxxxxxxend writexxxxxxxxxxxxxxxxxxxxx
end if
end do

end program
