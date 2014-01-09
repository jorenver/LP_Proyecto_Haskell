data Tree a = Vacio| Nodo a [Tree a] deriving (Show)

singletonTree:: a->Tree a
singletonTree x = Nodo x []

--recibe un elemento y un arbol y lo inserta al arbol
insertTree::Tree a->Tree a->Tree a
insertTree x Vacio = x
insertTree Vacio x = x
insertTree (Nodo s [] ) x= Nodo s [x]
insertTree (Nodo s [t]) x= Nodo s (x:[t])

treeEsHoja:: Tree a-> Bool
treeEsHoja Vacio = False
treeEsHoja (Nodo _ []) = True
treeEsHoja  (Nodo _ [_]) =False
