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
