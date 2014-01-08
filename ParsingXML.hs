import Control.Monad
import Data.Char
import qualified Data.List as A

main = forever $ do
				 l <- getLine
				 putStrLn $ imprimir $ procesarCapability l

procesarCapability ::String->(String,String)
procesarCapability etiqueta= (procesarAtributo $ head $tail $ words et,procesarAtributo $ head $tail $tail $ words et)
	where et =limpiarEtiqueta etiqueta
		

procesarAtributo:: String->String
procesarAtributo atributo=quitarcomillas $ A.dropWhile (/='\"') atributo 


limpiarEtiqueta ::String ->String
limpiarEtiqueta etiqueta =[x| x<-etiqueta,x/='<',x/='>',x/='/']

imprimir ::(String,String)->String
imprimir (x,y) = "("++x++","++y++")"

procesarAtributo:: String->String
procesarAtributo atributo=quitarcomillas $ A.dropWhile (/='\"') atributo 

quitarcomillas palabra =[x| x<- palabra ,x /='\"']
