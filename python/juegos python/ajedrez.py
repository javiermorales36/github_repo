class ChessGame:
    def __init__(self):
        self.board = self.initialize_board()
        self.current_player = "white"
        self.game_over = False

    def initialize_board(self):
        # Tablero de ajedrez inicial
        board = {}
        pieces = ["R", "N", "B", "Q", "K", "B", "N", "R"]
        for i, piece in enumerate(pieces):
            board[chr(97 + i) + "1"] = piece
            board[chr(97 + i) + "8"] = piece.lower()
            board[chr(97 + i) + "2"] = "P"
            board[chr(97 + i) + "7"] = "p"
        return board

    def display_board(self):
        print("   a b c d e f g h")
        print("  -----------------")
        for row in range(8, 0, -1):
            print(row, "|", end=" ")
            for col in range(97, 105):
                square = chr(col) + str(row)
                piece = self.board.get(square, " ")
                print(piece, end=" ")
            print("|", row)
        print("  -----------------")
        print("   a b c d e f g h")

    def is_valid_move(self, move):
        # Aquí puedes implementar tu lógica de validación de movimiento
        pass

    def make_move(self, move):
        if self.is_valid_move(move):
            source_square, target_square = move.split()
            self.board[target_square] = self.board[source_square]
            del self.board[source_square]
            self.current_player = "black" if self.current_player == "white" else "white"
        else:
            print("Movimiento no válido. Intenta de nuevo.")

    def play_game(self):
        while not self.game_over:
            self.display_board()
            print(f"Es el turno de las piezas {self.current_player}")
            move = input("Ingresa tu movimiento (por ejemplo, 'e2 e4'): ")
            self.make_move(move)

if __name__ == "__main__":
    game = ChessGame()
    game.play_game()
