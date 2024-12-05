       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-FILES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL EMPLEADOS-ARCHIVO
       ASSIGN TO "C:\Users\javi morales\OneDrive\cursos\Documentos\github_repo\cobol\empleados.dat"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADOS-ARCHIVO.
           01 EMPLEADOS-REGISTRO.
               05 EMPLEADOS-ID PIC X(6).
               05 EMPLEADOS-NOMBRE PIC X(25).
               05 EMPLEADOS-APELLIDOS PIC X(35).
               05 EMPLEADOS-EDAD PIC 99.
               05 EMPLEADOS-TELEFONO PIC X(9).
               05 EMPLEADOS-DIRECCION PIC X(35).

       WORKING-STORAGE SECTION.
       01 SI-NO PIC X VALUE "S".
       01 ENTRADA PIC X.
       01 RESPUESTA-VALIDA PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       PROGRAM-BEGIN.

           PERFORM PROCEDIMIENTO-DE-APERTURA
           PERFORM AGREGAR-REGISTROS
           PERFORM PROCEDIMIENTO-DE-CIERRE
           DISPLAY "Proceso de creación de archivos COBOL finalizado.".

       PROGRAM-DONE.
       STOP RUN.

       PROCEDIMIENTO-DE-APERTURA.
           OPEN EXTEND EMPLEADOS-ARCHIVO.

       PROCEDIMIENTO-DE-CIERRE.
           CLOSE EMPLEADOS-ARCHIVO.

       AGREGAR-REGISTROS.
           PERFORM UNTIL SI-NO = "N"
               PERFORM OBTENER-CAMPOS
               PERFORM ESCRIBIR-REGISTRO
               PERFORM REINICIAR
           END-PERFORM.

       OBTENER-CAMPOS.
           MOVE SPACES TO EMPLEADOS-REGISTRO.
           DISPLAY IDENTIFICADOR " ? ".
           ACCEPT EMPLEADOS-ID.
           DISPLAY NOMBRE " ? ".
           ACCEPT EMPLEADOS-NOMBRE.
           DISPLAY APELLIDOS " ? ".
           ACCEPT EMPLEADOS-APELLIDOS.
           DISPLAY EDAD " ? ".
           ACCEPT ENTRADA.
           IF ENTRADA IS NUMERIC
               MOVE ENTRADA TO EMPLEADOS-EDAD
           ELSE
               DISPLAY "Edad no válida. Debe ser un número.".
               PERFORM OBTENER-CAMPOS
           END-IF.
           DISPLAY TELEFONO " ? ".
           ACCEPT EMPLEADOS-TELEFONO.
           DISPLAY DIRECCION " ? ".
           ACCEPT EMPLEADOS-DIRECCION.

       ESCRIBIR-REGISTRO.
           WRITE EMPLEADOS-REGISTRO.

       REINICIAR.
           PERFORM VALIDAR-RESPUESTA
           IF RESPUESTA-VALIDA = "S"
               MOVE "S" TO SI-NO
           ELSE
               MOVE "N" TO SI-NO
           END-IF.

       VALIDAR-RESPUESTA.
           DISPLAY "¿Desea almacenar otro registro en la base de datos? (S/N) ".
           ACCEPT RESPUESTA-VALIDA
           IF RESPUESTA-VALIDA NOT = "S" AND RESPUESTA-VALIDA NOT = "N"
               DISPLAY "Respuesta no válida. Introduzca S para sí o N para no."
               PERFORM VALIDAR-RESPUESTA
           END-IF.

       END PROGRAM CREATE-FILES.
