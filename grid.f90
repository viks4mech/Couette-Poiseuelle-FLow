!==================================================================
! Program to generate stretched grid between 0 and 1 with N1 points
! with correction of the first mesh for finite volume methods
!==================================================================
! Compil with gfortran grid_main.f90 -o grid_main 
!==================================================================
module grid

contains

!function grid_coordinate(ALPHA,N)

!IMPLICIT NONE  

!REAL(8)	            ::ALPHA
!REAL(8),ALLOCATABLE ::YHAD(:),YPAD(:)
!INTEGER		    ::N,I


!ALPHA = 0.05
!N     = 101

!ALLOCATE(YPAD(N),YHAD(N))

!CALL sGrid_Generator(N,ALPHA,YPAD,YHAD)

!DO I=1,N
!   WRITE(*,11)I,YPAD(I),YHAD(I)
!ENDDO

!DEALLOCATE(YPAD,YHAD)	

!11 FORMAT(I3,2x,2(F10.4,2x))	
!END function



!==================================================================
! Subroutines to generate stretched grid between 0 and 1 with N1 points
!==================================================================

SUBROUTINE sGrid_Generator(N1,ALPHA,YPAD,YHAD)

IMPLICIT NONE 
	
REAL(8),INTENT(IN)				::ALPHA
INTEGER,INTENT(IN)				::N1
REAL(8),DIMENSION(N1),INTENT(INOUT)		::YPAD,YHAD
INTEGER						::J
REAL(8)						::ALPHA1,ALPHA2
INTEGER						::N05,N06
REAL(8)						::COEF

WRITE(*,3)N1,ALPHA
3 FORMAT('GRID: NOMBRE DE POINTS: ',I3,' COEFFICIENT ALPHA: ',F7.3)

ALPHA1=ALPHA
ALPHA2=ALPHA

N05=(N1+1)/2
N06=N05+1

!---  Mesh for half lower channel ---

COEF=DEXP(ALPHA1*(1.-N05))
DO J=2,N05
   YPAD(J)=.5*(DEXP(ALPHA1*(J-N05))-COEF)/(1.-COEF)
ENDDO

!---  Mesh for half upper channel ---

COEF=DEXP(ALPHA2*(N05-N1))
DO J=N06,N1-1
  YPAD(J)=(1.-.5*(DEXP(ALPHA2*(N05-J))-COEF)/(1.-COEF))
ENDDO

!---  Near wall points ---

YPAD(1)=-YPAD(2)
YPAD(N1)=2.-YPAD(N1-1)

YHAD(1)=0.
DO J=2,N1
  YHAD(J)=YPAD(J)-YPAD(J-1)
ENDDO

END SUBROUTINE

end module
