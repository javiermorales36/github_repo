import re

# Función para descifrar un mensaje usando el cifrado aditivo
def decrypt_additive_cipher(ciphertext, key):
    # Define el alfabeto y su correspondencia numérica
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    reverse_map = {idx: char for idx, char in enumerate(alphabet)}
    
    # Limpiar el texto cifrado para eliminar puntuaciones y caracteres no numéricos
    cleaned_ciphertext = re.sub(r"[^0-9\s]", "", ciphertext)
    
    # Separar los números en el texto cifrado
    cipher_numbers = []
    for block in cleaned_ciphertext.split(","):
        block_numbers = [int(num) for num in block.split()]
        cipher_numbers.extend(block_numbers)
    
    # Descifrar cada número
    plaintext = ""
    for num in cipher_numbers:
        if num in range(26):  # Asegurar que es un carácter válido
            decrypted_num = (num - key) % 26
            plaintext += reverse_map[decrypted_num]
        else:
            plaintext += " "  # Agregar espacio para separadores o no válidos
    
    return plaintext

# Texto cifrado proporcionado
ciphertext = """
08 07 05 18    13 01 08 12 24    16 01 08    23 20 11 24    13 08    25 20 02 05    00 11 24 20 13 05 18    22 20 07    20 22 01 02 24 15 24    00 11 24 20 13 05 18.    - 11 08 21 24 11 13    25.    04 24 07 07 24 23 18
"""

# Fuerza bruta para probar todas las claves
for key in range(26):
    decrypted_message = decrypt_additive_cipher(ciphertext, key)
    print(f"Clave {key}: {decrypted_message}\n")
