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
insertElement (Nodo s y) arbol [] =Nodo s (arbol:y)
insertElement Vacio arbol _  = arbol
insertElement (Nodo s y) arbol (x:xs) = Nodo s (((take (x-1) y)++ [(insertElement (getByIndex x y) arbol xs)])++(drop (x) y))
  
getByIndex :: Int->[a]->a
getByIndex i (x:xs) =if i==1 then
						x
					else
						getByIndex (i-1) xs
						
unirListas :: [a]->[a]->[a]
unirListas [] []=[]
unirListas (p:ps) []=[p]++(unirListas ps [])
unirListas [] s=(unirListas [] (init s))++[last s]
unirListas (p:ps) s =[p]++(unirListas ps (init s))++[last s]