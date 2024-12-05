program Calculadora;

var
  numero1, numero2, resultado: real;
  operador: char;

begin
  writeln('Calculadora en Pascal');
  writeln('----------------------');
  writeln;

  write('Ingresa el primer número: ');
  readln(numero1);

  write('Ingresa el operador (+, -, *, /): ');
  readln(operador);

  write('Ingresa el segundo número: ');
  readln(numero2);

  case operador of
    '+': resultado := numero1 + numero2;
    '-': resultado := numero1 - numero2;
    '*': resultado := numero1 * numero2;
    '/':
      begin
        if numero2 = 0 then
          writeln('Error: No se puede dividir por cero')
        else
          resultado := numero1 / numero2;
      end;
    else
      writeln('Operador no válido');
  end;

  writeln('El resultado es: ', resultado:0:2);
end.
