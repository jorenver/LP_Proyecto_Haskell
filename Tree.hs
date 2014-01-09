data Tree a = Vacio| Nodo a [Tree a] deriving (Show)

singletonTree:: a->Tree a
singletonTree x = Nodo x []

--recibe un elemento y un arbol y lo inserta al arbol
insertTree::Tree a->Tree a->Tree a
insertTree x Vacio = x
insertTree Vacio x = x
insertTree x (Nodo s [] )= Nodo s [x]
insertTree x (Nodo s [t]) = Nodo s (x:[t])