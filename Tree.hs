module Tree
(Tree (..)
,singletonTree
,insertTree
,treeEsHoja
,insertElement
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


insertElement ::Tree a ->Tree a->[Int]->Tree a
insertElement (Nodo s []) arbol [] =Nodo s [arbol]
insertElement (Nodo s [y]) arbol [] =Nodo s ([y]++[arbol])
insertElement Vacio arbol _  = arbol
insertElement (Nodo s [y]) arbol (x:xs) = Nodo s (unirListas (unirListas(take (x-1) [y]) [insertElement (getByIndex x [y]) arbol xs]) (drop (x+1) [y]))
  
getByIndex :: Int->[a]->a
getByIndex i (x:xs) =if i==1 then
						x
					else
						getByIndex (i-1) xs
						
unirListas :: [a]->[a]->[a]
unirListas p []=p
unirListas [] s=s
unirListas p (s:ss) =p++[s]++unirListas p ss