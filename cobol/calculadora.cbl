       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calculadora.
       DATA DIVISION.
        WORKING-STORAGE SECTION.
       01 Numero1       PIC 9(5).
       01 Numero2       PIC 9(5).
       01 Operacion     PIC X.
       01 Resultado     PIC 9(10).

        PROCEDURE DIVISION.
        DISPLAY "Ingrese el primer número: " WITH NO ADVANCING.
       ACCEPT Numero1.

        DISPLAY "Ingrese la operación (+, -, *, /): " WITH NO ADVANCING.
       ACCEPT Operacion.

       DISPLAY "Ingrese el segundo número: " WITH NO ADVANCING.
       ACCEPT Numero2.

       COMPUTE Resultado = 0

        IF Operacion = "+" THEN
        ADD Numero1 TO Resultado GIVING Resultado.
        ELSE IF Operacion = "-" THEN
        SUBTRACT Numero2 FROM Numero1 GIVING Resultado.
        ELSE IF Operacion = "*" THEN
        MULTIPLY Numero1 BY Numero2 GIVING Resultado.
        ELSE IF Operacion = "/" THEN
        DIVIDE Numero1 BY Numero2 GIVING Resultado.
        ELSE
        DISPLAY "Operación no válida".
        GOBACK.
       END-IF.

        DISPLAY "El resultado es: " Resultado.

        GOBACK.

       STOP RUN.
