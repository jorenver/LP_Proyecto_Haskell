module Tree
(Tree (..)
,singletonTree
,insertTree
,treeEsHoja
)where




data Tree a = Vacio| Nodo a [Tree a] deriving (Show)

singletonTree:: a->Tree a
singletonTree x = Nodo x []

--recibe dos arboles e inserta al a primer arbol el segundo
insertTree::Tree a->Tree a->Tree a
insertTree x Vacio = x
insertTree Vacio x = x
insertTree (Nodo s [] ) x= Nodo s [x]
insertTree (Nodo s [t]) x= Nodo s (x:[t])

treeEsHoja:: Tree a-> Bool
treeEsHoja Vacio = False
treeEsHoja (Nodo _ []) = True
treeEsHoja  (Nodo _ [_]) =False
