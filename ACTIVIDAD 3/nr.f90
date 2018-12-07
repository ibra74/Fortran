FUNCTION F(n) !FUNCIÓN
  IMPLICIT NONE
  real, intent(in) ::n
  real:: F
  F = n**3 - n - 2
END FUNCTION F

FUNCTION Fp(n) !DERIVADA
  IMPLICIT NONE
  real, intent(in) ::n
  real::Fp
  Fp = 3*n**2 - n
END FUNCTION Fp

PROGRAM nr
  IMPLICIT NONE
  !x buscada  xi= x inicial , err = error
  REAL:: x, xi, err
  REAL:: F , Fp
  PRINT*,"Inserte el valor de x inicial para aplicar newton-raphson a la funcion"
  READ*, xi

  DO
     !xi - F(xi)/F(xi)
     x = xi - (F(xi)/Fp(xi))
     err = abs((x-xi)/x)*100
    PRINT*, "X= ", x, "err= ", err
    xi=x

    IF(err<.00001)EXIT

 END DO
 END PROGRAM nr 
