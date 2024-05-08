import random

def escribir_palabras_azar(lista, n, archivo):
  # Abre el archivo en modo escritura
  with open(archivo, "w") as f:
    # Repite n veces
    for i in range(n):
      # Elige una palabra al azar de la lista
      palabra = random.choice(lista)
      # Escribe la palabra en el archivo con un salto de línea
      f.write(palabra + "\n")

# Ejemplo de uso
lista = ["while", "for", "+", "-", "hola"]
n = 1000000
archivo = "largo.txt"
escribir_palabras_azar(lista, n, archivo)