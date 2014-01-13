import Control.Monad
import Data.Char
import qualified Data.List as A
import Tree

--main = forever $ do
	--			 l <- getLine
		--		 putStrLn $ imprimir $ procesarCapability l

procesarCapability ::String->(String,String)
procesarCapability etiqueta= (procesarAtributo $ head $tail $ words et,procesarAtributo $ head $tail $tail $ words et)
	where et =limpiarEtiqueta etiqueta
		

procesarAtributo:: String->String
procesarAtributo atributo=quitarcomillas $ A.dropWhile (/='\"') atributo 


limpiarEtiqueta ::String ->String
limpiarEtiqueta etiqueta =[x| x<-etiqueta,x/='<',x/='>',x/='/']

imprimir ::(String,String)->String
imprimir (x,y) = "("++x++","++y++")"

quitarcomillas palabra =[x| x<- palabra ,x /='\"']

procesarEtiqueta ::String->[(String,String)]
procesarEtiqueta [] = []
procesarEtiqueta etiqueta= (crearTupla $ A.takeWhile (/=' ') $ limpiarEtiqueta etiqueta):(procesarEtiqueta $ cola $ A.dropWhile (/=' ') etiqueta)  
				--where palabra = A.takeWhile (/=' ') $ limpiarEtiqueta etiqueta
		              --sobrante = tail $ A.dropWhile (/=' ') etiqueta

		  
crearTupla ::String->(String,String)
crearTupla palabra = (A.takeWhile (/='=') $ limpiarEtiqueta palabra,cola $ A.dropWhile (/='=') palabra)
		   --where primero=A.takeWhile (/='=') $ limpiarEtiqueta palabra
			    -- segundo=tail $ A.dropWhile (/='=') palabra

cola [] = []
cola (x:xs)=xs

crearEtiquetaCierre ::String->String
crearEtiquetaCierre tag = "<"++tag++"/>"
			
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

-- recibira un cadena que representa el archivo de texto y removera los comentarios
limpiarComentarios:: [Char]->[Char]
limpiarComentarios []=[]
limpiarComentarios entrada 
	     	| (x == '<') && ( head xs == '!' ) = ' ':limpiarComentarios ( cola ( dropWhile (/='>') xs ) )
		| otherwise = x:limpiarComentarios xs	
		where x = head entrada
		      xs = cola entrada

obtenerLinea:: String->String
-- lee un string hasta que encuentre el caracter del proxima linea y lo retorna
obtenerLinea []=[]
obtenerLinea entrada= takeWhile (/='\n') entrada

-- retorna una lista en donde cada elemento es una linea de texto
guardarLineas:: String->[String]
guardarLineas []=[]
guardarLineas entrada=obtenerLinea entrada:guardarLineas ( cola $ dropWhile (/='\n') entrada )

-- Recibe un String y quita las tabulaciones o espacios al principio de la cadena
quitarTab:: String->String
quitarTab []=[]
quitarTab (x:xs) | (x==' ') =  dropWhile (==' ') xs
 		 |  x/=' '  =  x:xs


-- Retorna una lista en donde cada elemento hay una linea de texto que va a ser procesada en el arbol
listaFiltrada::String->[String]
listaFiltrada [] = []
listaFiltrada entrada=  [ x | x<-lista, length(x)>1] 
			where lista = map (quitarTab) (guardarLineas(limpiarComentarios entrada))
-- Se evita las lineas que solo tienen un caracter 