import random

def jugar_ruleta():
    # Saldo inicial del jugador
    saldo = 1000
    
    while saldo > 0:
        # Imprimir el saldo actual
        print(f"Tu saldo actual es: ${saldo}")
        
        # Pedir al jugador que elija un número y haga una apuesta
        while True:
            try:
                numero_elegido = int(input("Elige un número entre 0 y 36: "))
                if 0 <= numero_elegido <= 36:
                    break
                else:
                    print("Por favor, elige un número entre 0 y 36.")
            except ValueError:
                print("Entrada inválida. Debes ingresar un número entre 0 y 36.")
        
        while True:
            try:
                apuesta = int(input(f"Tu saldo actual es: ${saldo}. ¿Cuánto deseas apostar?: "))
                if 0 <= apuesta <= saldo:
                    break
                else:
                    print("No puedes apostar más de lo que tienes.")
            except ValueError:
                print("Entrada inválida. Debes ingresar una cantidad válida.")
        
        # Gira la ruleta (número aleatorio entre 0 y 36)
        numero_ruleta = random.randint(0, 36)
        print(f"La ruleta ha caído en el número {numero_ruleta}")
        
        # Verifica si el jugador ganó o perdió
        if numero_elegido == numero_ruleta:
            saldo += apuesta * 35  # Gana 35 veces su apuesta
            print(f"¡Felicidades! Ganaste ${apuesta * 35}")
        else:
            saldo -= apuesta
            print(f"Lo siento, perdiste ${apuesta}")
        
        # Preguntar si el jugador quiere jugar de nuevo
        jugar_de_nuevo = input("¿Quieres jugar de nuevo? (s/n): ").lower()
        if jugar_de_nuevo != 's':
            break
    
    print("Gracias por jugar. ¡Hasta la próxima!")

if __name__ == "__main__":
    jugar_ruleta()
