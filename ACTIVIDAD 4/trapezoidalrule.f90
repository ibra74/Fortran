


  ! REGLA DEL TRAPECIO PARA (X**2 - 3X + 5) DE 1 A 5

PROGRAM trapezoidal
 

  IMPLICIT NONE
  INTEGER :: n
  REAL :: u, b , a
  INTEGER :: i
  WRITE(*,*)" Ingrese valor de n"
  READ(*,*)n
  WRITE(*,*)"Ingrese valor de a"
  READ(*,*)a
  WRITE(*,*)"Ingrese valor de b"
  READ(*,*) b
  
  DO i=0,n
     u =( b-a)*i/float(n)
     WRITE (*,*) u, INTEGRAND(u)
  END DO

  STOP

  CONTAINS

     FUNCTION INTEGRAND(x) RESULT(value)
       IMPLICIT NONE
       REAL :: x
       REAL :: value

       value=x**2 - 3*x + 5
 END FUNCTION INTEGRAND
  
END PROGRAM trapezoidal
