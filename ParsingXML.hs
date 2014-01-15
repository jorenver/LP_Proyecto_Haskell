import Control.Monad
import Data.Char
import qualified Data.List as A
import Tree
import System.IO

main = do 
		handle <- openFile "prueba.xml" ReadMode
		contents <- hGetContents handle
		let arbol = sembrarArbol $listaFiltrada contents
		print("\n")

procesarCapability ::String->(String,String)
procesarCapability etiqueta= (procesarAtributo $ head $tail $ words et,procesarAtributo $ head $tail $tail $ words et)
	where et =limpiarEtiqueta etiqueta
		

procesarAtributo:: String->String
procesarAtributo atributo=quitarcomillas $ A.dropWhile (/='\"') atributo 


limpiarEtiqueta ::String ->String
limpiarEtiqueta etiqueta =[x| x<-etiqueta,x/='<',x/='>',x/='/',x /='\"']

-- imprimir ::(String,String)->String
-- imprimir (x,y) = "("++x++","++y++")"

quitarcomillas palabra =[x| x<- palabra ,x /='\"']

procesarEtiqueta ::String->[(String,String)]
procesarEtiqueta [] = []
procesarEtiqueta etiqueta= (crearTupla $ A.takeWhile (/=' ') $ limpiarEtiqueta  etiqueta):(procesarEtiqueta $ cola $ A.dropWhile (/=' ') etiqueta)  
		  
crearTupla ::String->(String,String)
crearTupla palabra = (A.takeWhile (/='=') $ limpiarEtiqueta palabra,cola $ A.dropWhile (/='=') palabra)

cola [] = []
cola (x:xs)=xs

obtenerTag :: [(String,String)]->String
obtenerTag [] = []
obtenerTag (x:xs) = fst x

crearEtiquetaCierre ::String->String
crearEtiquetaCierre tag = "</"++tag++">"
			
esEtiqueta ::String->Bool
esEtiqueta linea = if (head linea =='<' && last linea =='>' && (length [x| x<-linea ,x=='/'] <= 1)) then
						True
					else
						False
						
esEtiquetaApertura ::String->Bool
esEtiquetaApertura linea = esEtiqueta linea && (length [x| x<-linea ,x=='/'] == 0)

esEtiquetaLinea ::String->Bool
esEtiquetaLinea linea = if length linea >=4 then
				if (head $ cola $reverse linea) == '/' then
					True
				else
					False
			else
				False

obtenerLinea:: String->String
-- lee un string hasta que encuentre el caracter del proxima linea y lo retorna
obtenerLinea []=[]
obtenerLinea entrada= takeWhile (/='\n') entrada

guardarLineas:: String->[String]
guardarLineas []=[]
guardarLineas entrada=obtenerLinea entrada:guardarLineas ( cola $ dropWhile (/='\n') entrada )


limpiarComentarios:: [Char]->[Char]
limpiarComentarios []=[]
limpiarComentarios entrada 
	     	| (x == '<') && ( head xs == '!' ) = ' ':limpiarComentarios ( cola ( dropWhile (/='>') xs ) )
		| otherwise = x:limpiarComentarios xs	
		where x = head entrada
		      xs = cola entrada


-- Recibe un String y quita las tabulaciones o espacios al principio de la cadena
quitarTab:: String->String
quitarTab []=[]
quitarTab (x:xs) | (x=='\t') || (x==' ') =  quitarTab xs
                  |  otherwise  =  x:xs

-- Retorna una lista en donde cada elemento hay una linea de texto que va a ser procesada en el arbol
listaFiltrada::String->[String]
listaFiltrada [] = []
listaFiltrada entrada=  [ x | x<-lista, length(x)>1] 
                        where lista = map (quitarTab) (guardarLineas(limpiarComentarios entrada))

listaFiltrada' entrada = ignorarLineaNormal(listaFiltrada entrada)
listaFiltrada'' entrada=  [ x | x<-lista, length(x)>1] 
                          where lista=  listaFiltrada' entrada

sembrarArbol :: [String]->[String]->Tree [(String,String)]->Int->Tree [(String,String)]
sembrarArbol [] [] arbol  _ = arbol
sembrarArbol [] _ _  _ = error "error de Parsing 1"
sembrarArbol (x:xs) [] Vacio nivel =if  (esEtiquetaApertura x )then
												sembrarArbol xs [crearEtiquetaCierre $ obtenerTag $ procesarEtiqueta x]  (insertElemento Vacio (singletonTree $ procesarEtiqueta x) 0) 0
											else
												error "error de Parsing 2"
sembrarArbol [x] [y] arbol _ =arbol
sembrarArbol (x:xs) (y:ys) arbol nivel 
			| esEtiquetaApertura x =sembrarArbol xs ((crearEtiquetaCierre $ obtenerTag $ procesarEtiqueta x):(y:ys)) (insertElemento arbol (singletonTree $ procesarEtiqueta x) nivel) (nivel+1)
        		| esEtiquetaLinea x = sembrarArbol xs (y:ys) (insertElemento arbol (singletonTree $ procesarEtiqueta x) nivel ) (nivel)
			| y==x = sembrarArbol xs ys arbol (nivel-1)
			-- | esLineaNormal x = sembrarArbol xs (y:ys) (insertElemento arbol (singletonTree $ procesarLinea x) nivel ) (nivel)
  			| otherwise = error ("error de Parsing se esperaba"++x++",se econtro"++y)	
	
esEtiquetaCierre::String->Bool
esEtiquetaCierre (x:xs)= head(xs) == '/'
		
esLineaNormal::String->Bool
esLineaNormal [] = False
esLineaNormal entrada = not(esEtiquetaApertura entrada) && not(esEtiquetaLinea entrada)&&not(esEtiquetaCierre entrada)

ignorarLineaNormal []=[]
ignorarLineaNormal (x:xs) 
			| esLineaNormal x = "":(ignorarLineaNormal xs)
			| otherwise = x: ignorarLineaNormal xs 
								
listInsertByIndex :: [a]->a->Int->[a]
listInsertByIndex [] y _=[y]
listInsertByIndex (x:xs) y n=if n==1 then
								y:xs
							else
								x:listInsertByIndex xs y (n-1)