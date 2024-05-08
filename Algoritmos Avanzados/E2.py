import random
import numpy as np
from scipy.spatial import Voronoi, voronoi_plot_2d
import matplotlib.pyplot as plt

def leer(archivo):
    with open(archivo, 'r') as archivo:
        contenido = archivo.read()  

    lineas = contenido.strip().split('\n')
    N = int(lineas[0])  

    matrizAdyacencia = []
    for i in range(2, 2+N):
        l = list(map(int, lineas[i].split()))
        matrizAdyacencia.append(l)    

    matrizFlujo = []
    for i in range(2+N+1, 2+N+1+N):
        l = list(map(int, lineas[i].split()))
        matrizFlujo.append(l)   

    listaPuntos = []
    for i in range(2+N+1+N+1, 2+N+1+N+1+N):
        valores = lineas[i].strip('()').split(',')
        tupla = tuple(map(int, valores))
        listaPuntos.append(tupla)

    return matrizAdyacencia, matrizFlujo, listaPuntos

def arbolMinimo(matriz):
   resultado = []
   V = len(matriz)
   abiertos = [False]*V
   abiertos[0] = abiertos
   n = 0
   suma = 0

   while n < V - 1:
      min = float('inf')
      a = 0
      b = 0
      for i in range(V):
         if abiertos[i]:
            for j in range(V):
               if not abiertos[j] and matriz[i][j] != 0:
                  if matriz[i][j] < min:
                     min = matriz[i][j]
                     a = i
                     b = j
      abiertos[b] = True         
      resultado.append((a,b))
      suma += matriz[a][b]
      n += 1

   print("Forma de cablear las colonias con fibra:")
   for i in resultado:
      print(i, end=" ")
   print("")
   print("Cable total usado:", suma)
                  
def viajero(matriz):

    P = len(matriz)
    numGen = len(matriz)

    def crearSolucion():
        N = len(matriz)
        L = []
        for i in range(1,N):
            L.append(i)
        random.shuffle(L)
        return L
    
    def aptitud(solucion:list):
       L = solucion.copy()
       L.insert(0, 0)
       L.append(0)
       suma = 0
       for i in range(len(L)-1):
          n = matriz[L[i]][L[i+1]]
          suma += n
       return suma

    def crearSolucionValida():
       sol = crearSolucion()
       while aptitud(sol) == 0:
          sol = crearSolucion()
       return sol

    def crearPoblacion():
       poblacion = []
       for i in range(P):
          poblacion.append(crearSolucionValida())
       return poblacion
        
    def seleccion(poblacion):
        aptitudes = []
        for solucion in poblacion:
           aptitudes.append(aptitud(solucion))
        promedio = np.mean(aptitudes)
        supervivientes = []
        for i in range(len(poblacion)):
           if aptitudes[i] <= promedio:
              supervivientes.append(poblacion[i])
        return supervivientes
    
    def mutar(solucion):
       x = random.randrange(0, len(solucion))
       y = random.randrange(0, len(solucion))
       while y == x:
          y = random.randrange(0, len(solucion))
        
       valorX = solucion[x]
       valorY = solucion[y]

       solucion[x] = valorY
       solucion[y] = valorX

    def cruza(solucion1, solucion2):
        T = len(solucion1)
        hijo = [-1]*T
        inicio = random.randrange(0, T-1)
        fin = random.randrange(inicio+1, T)
        hijo[inicio:fin] = solucion1[inicio:fin]
        indice = 0
        for i in range(len(hijo)):
            if hijo[i] == -1:
                while solucion2[indice] in hijo and indice < len(solucion2):
                    indice += 1
                hijo[i] = solucion2[indice]
        return hijo

    def repoblar(poblacion):
       N = P - len(poblacion)
       for i in range(N):
          n1 = random.randrange(0, len(poblacion))
          n2 = random.randrange(0, len(poblacion))
          padre1 = poblacion[n1]
          padre2 = poblacion[n2]
          hijo = cruza(padre1, padre2)
          r = random.randint(1,100)
          p = 20
          if r < p:
             mutar(hijo)
          poblacion.append(hijo)

    def mejorarPoblacion(poblacion):
       poblacion = seleccion(poblacion)
       repoblar(poblacion)

    mejorAptitud = float('inf')
    mejorSolucion = []
    poblacion = crearPoblacion()

    for i in range(numGen):
        mejorarPoblacion(poblacion)
        for solucion in poblacion:
           apt = aptitud(solucion)
           if apt < mejorAptitud:
              mejorAptitud = apt
              mejorSolucion = solucion
    
    print("Ruta a seguir por el personal que reparte correspondencia:")
    print(0, "->", end=" ")
    for i in mejorSolucion:
       print(i, "->", end=" ")
    print(0)
    print("Distancia total:", mejorAptitud)

def flujoMaximo(matriz, inicio, destino):

   grafo = {}
   for i in range(len(matriz)):
      grafo[i] = {}
      for j in range(len(matriz[i])):
         if matriz[i][j] > 0:
            grafo[i][j] = matriz[i][j]
   
   def bfs():
      padres = [-1]*len(grafo)
      visitados = [False]*len(grafo)
      fila = []
      fila.append(inicio)
      visitados[inicio] = True

      while len(fila) > 0:
         nodoActual = fila.pop(0)

         for nodo, capacidad in grafo[nodoActual].items():
            if not visitados[nodo] and capacidad > 0:
               fila.append(nodo)
               visitados[nodo] = True
               padres[nodo] = nodoActual
      
      if visitados[destino]:
         return padres
      return 0

   flujoMax = 0
   padres = bfs()
   while padres != 0:
      flujo = float('inf')
      nodoActual = destino
      
      while nodoActual != inicio:
         flujo = min(flujo, grafo[padres[nodoActual]][nodoActual])
         nodoActual = padres[nodoActual]

      flujoMax += flujo
      nodoActual = destino

      while nodoActual != inicio:
         padre = padres[nodoActual]
         grafo[padre][nodoActual] -= flujo
         grafo[nodoActual][padre] += flujo
         nodoActual = padre

      padres = bfs()

   print("Valor de flujo máximo de información del nodo inicial al nodo final:", flujoMax)

M, MF, LP = leer("archivo.txt")

print("1.-")
arbolMinimo(M)
print("\n2.-")
viajero(M)
print("\n3.-")
flujoMaximo(MF, 0, 3)
print("\n4.-")
print("Diagrama de voronoi:")
vor = Voronoi(LP)
fig = voronoi_plot_2d(vor)
plt.show()