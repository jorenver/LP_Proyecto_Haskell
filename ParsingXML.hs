import Control.Monad
import Data.Char
import qualified Data.List as A

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