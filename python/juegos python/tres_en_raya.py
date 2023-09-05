# Función para imprimir el tablero
def imprimir_tablero(tablero):
    for fila in tablero:
        print(" | ".join(fila))
        print("-" * 9)

# Función para verificar si alguien ha ganado
def verificar_ganador(tablero, jugador):
    for fila in tablero:
        if all(cell == jugador for cell in fila):
            return True

    for col in range(3):
        if all(tablero[row][col] == jugador for row in range(3)):
            return True

    if all(tablero[i][i] == jugador for i in range(3)) or all(tablero[i][2 - i] == jugador for i in range(3)):
        return True

    return False

# Función principal del juego
def jugar_tres_en_raya():
    tablero = [[" " for _ in range(3)] for _ in range(3)]
    jugador_actual = "X"
    jugadas = 0

    while True:
        imprimir_tablero(tablero)
        fila = int(input(f"Jugador {jugador_actual}, elige una fila (0, 1, 2): "))
        columna = int(input(f"Jugador {jugador_actual}, elige una columna (0, 1, 2): "))

        if tablero[fila][columna] == " ":
            tablero[fila][columna] = jugador_actual
            jugadas += 1
        else:
            print("Esa casilla ya está ocupada. Intenta de nuevo.")
            continue

        if verificar_ganador(tablero, jugador_actual):
            imprimir_tablero(tablero)
            print(f"¡Jugador {jugador_actual} ha ganado!")
            break
        elif jugadas == 9:
            imprimir_tablero(tablero)
            print("¡Es un empate!")
            break

        jugador_actual = "O" if jugador_actual == "X" else "X"

# Iniciar el juego
jugar_tres_en_raya()
