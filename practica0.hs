module Practica0 where 

{-- Recursión y recursión de Cola --}

-- Función buscar: Dada una lista de Enteros y un elemento,
-- regresa True si el elemento se encuentra en la lista; de lo contrario, False.
buscar :: [Int] -> Int -> Bool
buscar [] _ = False
buscar (x:xs) n
    | x == n    = True
    | otherwise = buscar xs n

-- Función sumar_lista: Dada una lista de enteros, regresa la suma de sus elementos.
-- Se implementa con recursión de cola usando un acumulador.
sumar_lista :: [Int] -> Int
sumar_lista xs = sumar_aux xs 0
  where
    sumar_aux [] acc     = acc
    sumar_aux (x:xs) acc = sumar_aux xs (acc + x)

-- Función recursiva "normal" (no usa recursión de cola)
-- Nota : Se probó con n = 1000000
-- Tiempo: 0.21 secs, Memoria: 218,535,216 bytes 
-- Se aprovecha que GHC optimiza patrones comunes de suma,
-- por lo que esta versión es más rápida y usa menos memoria.
recursiva :: Int -> Int
recursiva 0 = 0
recursiva n = n + recursiva (n-1)

-- Función recursiva con recursión de cola usando un acumulador
-- Nota : Se probó con n = 1000000
-- Tiempo: 0.40 secs, Memoria: 377,692,664 bytes
-- Aunque usa recursión de cola, el manejo del acumulador hace que:
-- 1. Se necesite más memoria para guardar el acumulador.
-- 2. GHC no pueda aplicar las mismas optimizaciones que en la versión anterior.
recursiva_cola :: Int -> Int
recursiva_cola n = recursiva_aux n 0
  where
    recursiva_aux 0 acc = acc
    recursiva_aux n acc = recursiva_aux (n-1) (acc + n)


{-- Funciones de orden superior --}

-- La función filterB toma un predicado (función que regresa booleano) y filtra los elementos
-- de la lista de entrada que satisfacen la condición.
filterB :: (a -> Bool) -> [a] -> [a]
filterB p [] = []
filterB p (x:xs)
    | p x       = x : filterB p xs  -- Si el elemento cumple el predicado, se incluye
    | otherwise = filterB p xs       -- Si no cumple, se descarta

-- La función mapear aplica una función a cada elemento de una lista.
mapear :: (a -> b) -> [a] -> [b]
mapear f []     = []
mapear f (x:xs) = f x : mapear f xs

-- Versión de mapear usando list comprehension.
mapear_ :: (a -> b) -> [a] -> [b]
mapear_ f list = [f x | x <- list]

{-- Tipos, Clases y Estructuras de Datos --}

-- Definición de árbol binario.
data Tree a = Empty 
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Recorrido preorden de un árbol.
-- primero la raíz, luego subárbol izquierdo, finalmente subárbol derecho
preorder :: Tree a -> [a]
preorder Empty              = []
preorder (Node root left right) = [root] ++ preorder left ++ preorder right

-- Buscar un elemento en el árbol. Regresa True si se encuentra, de lo contrario False.
buscar_tree :: Eq a => Tree a -> a -> Bool
buscar_tree Empty _ = False
buscar_tree (Node root left right) e
    | root == e  = True
    | otherwise = buscar_tree left e || buscar_tree right e

-- Punto Extra: Función que cuenta la cantidad de hojas del árbol.
-- Se considera que una hoja es un nodo que no tiene hijos.
hojas :: Tree a -> Int
hojas Empty = 0
hojas (Node _ Empty Empty) = 1
hojas (Node _ left right)  = hojas left + hojas right

-- Definición de gráfica.
type Vertex = Int
type Graph = [(Vertex, [Vertex])]

-- Devuelve los vecinos (lista de adyacencia) de un vértice en la gráfica.
vecinos :: Graph -> Vertex -> [Vertex]
vecinos [] _ = []  
vecinos ((v, ns):xs) x
    | v == x    = ns 
    | otherwise = vecinos xs x

-- Recorrido en profundidad (DFS) de la gráfica.
dfs :: Graph -> Vertex -> [Vertex] -> [Vertex]
dfs graph v visited
    | v `elem` visited = visited  
    | otherwise        = foldl (\acc n -> dfs graph n acc) (v : visited) (vecinos graph v)

-- Función auxiliar para obtener todos los vértices de una gráfica
vertices :: Graph -> [Vertex]
vertices [] = []
vertices ((v,_):gs) = v : vertices gs

-- Función auxiliar para verificar si hay ciclos usando DFS (Búsqueda en Profundidad)
-- Un ciclo existe si encontramos un vértice que ya está en la pila de recursión
hasCycle :: Graph -> Bool
hasCycle g = any (\v -> hasCycleUtil g v Nothing []) (vertices g)
  where
    hasCycleUtil :: Graph -> Vertex -> Maybe Vertex -> [Vertex] -> Bool
    hasCycleUtil graph curr parent visitados
        | curr `elem` visitados = True     -- Si encontramos un vértice ya visitado, hay ciclo
        | otherwise = 
            let vecindad = vecinos graph curr 
                visitados' = curr : visitados
            in any (\next -> 
                    case parent of
                        Just p -> next /= p && hasCycleUtil graph next (Just curr) visitados'
                        Nothing -> hasCycleUtil graph next (Just curr) visitados'
                  ) vecindad

-- Función que verifica si la gráfica es conexa
-- Una gráfica es conexa si existe un camino entre cualquier par de vértices
isConnected :: Graph -> Bool   
isConnected [] = True
isConnected g@((v,_):_) = length (dfs g v []) == length (vertices g)

-- La siguiente función verifica que la gráfica es un árbol
-- Un árbol es una gráfica conexa y sin ciclos
isTree :: Graph -> Bool
isTree [] = True  -- Una gráfica vacía se considera un árbol
isTree g = isConnected g && not (hasCycle g)

-- Ejemplos de gráficas.
connectedGraph :: Graph
connectedGraph = [(1, [2,3]), (2, [4]), (3, [4,5]), (4, [6]), (5, [6]), (6, [])]  
-- Debe regresar True

disconnectedGraph :: Graph
disconnectedGraph = [(1, [2]), (2, [1]), (3, [4]), (4, [3])] 
-- Debe regresar False 

-- La función leafSum regresa la suma de los valores almacenados en los nodos hoja del árbol.
leafSum :: Tree Int -> Int 
leafSum Empty = 0
leafSum (Node x Empty Empty) = x  -- Esto es una hoja
leafSum (Node _ left right) = leafSum left + leafSum right  -- No es una hoja, hacemos recursión sobre los hijos
