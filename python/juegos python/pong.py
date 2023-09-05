import os
import time

# Tamaño de la ventana de la consola
WIDTH = 50
HEIGHT = 20

# Posiciones iniciales de las palas y la pelota
paddle1_y = HEIGHT // 2
paddle2_y = HEIGHT // 2
ball_x = WIDTH // 2
ball_y = HEIGHT // 2
ball_dx = 1
ball_dy = 1

# Función para dibujar el campo de juego
def draw_field():
    os.system("clear" if os.name == "posix" else "cls")
    print("-" * (WIDTH + 2))
    for y in range(HEIGHT):
        print("|", end="")
        for x in range(WIDTH):
            if x == ball_x and y == ball_y:
                print("O", end="")
            elif x == 0 and y == paddle1_y:
                print("▓", end="")
            elif x == WIDTH - 1 and y == paddle2_y:
                print("▓", end="")
            else:
                print(" ", end="")
        print("|")
    print("-" * (WIDTH + 2))

# Función principal del juego
def main():
    global ball_x, ball_y, ball_dx, ball_dy, paddle1_y, paddle2_y

    while True:
        draw_field()

        # Control de las palas
        key = input()
        if key == 'w' and paddle1_y > 0:
            paddle1_y -= 1
        elif key == 's' and paddle1_y < HEIGHT - 1:
            paddle1_y += 1
        elif key == 'i' and paddle2_y > 0:
            paddle2_y -= 1
        elif key == 'k' and paddle2_y < HEIGHT - 1:
            paddle2_y += 1

        # Actualización de la posición de la pelota
        ball_x += ball_dx
        ball_y += ball_dy

        # Colisión con las palas
        if ball_x == 1 and paddle1_y <= ball_y <= paddle1_y + 2:
            ball_dx = 1
        elif ball_x == WIDTH - 2 and paddle2_y <= ball_y <= paddle2_y + 2:
            ball_dx = -1

        # Colisión con las paredes superior e inferior
        if ball_y == 0 or ball_y == HEIGHT - 1:
            ball_dy *= -1

        # Punto marcado por el jugador 1
        if ball_x == 0:
            print("¡Jugador 2 gana!")
            break

        # Punto marcado por el jugador 2
        if ball_x == WIDTH - 1:
            print("¡Jugador 1 gana!")
            break

        time.sleep(0.05)

if __name__ == "__main__":
    main()
