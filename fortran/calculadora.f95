program Calculadora
  implicit none
  real :: numero1, numero2, resultado
  character(1) :: operador

  write(*,*) "Calculadora en Fortran"
  write(*,*) "----------------------"
  write(*,*) "Ingrese el primer número:"
  read(*, *) numero1
  write(*,*) "Ingrese el operador (+, -, *, /):"
  read(*, *) operador
  write(*,*) "Ingrese el segundo número:"
  read(*, *) numero2

  select case (operador)
    case ("+")
      resultado = numero1 + numero2
    case ("-")
      resultado = numero1 - numero2
    case ("*")
      resultado = numero1 * numero2
    case ("/")
      if (numero2 /= 0) then
        resultado = numero1 / numero2
      else
        write(*,*) "Error: División por cero."
        stop
      end if
    case default
      write(*,*) "Operador no válido."
      stop
  end select

  write(*,*) "Resultado: ", resultado

end program Calculadora

