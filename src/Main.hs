module Main (main) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Graphics.Gloss
import Interp (Conf(name), initial)
import Dibujos.Ejemplo (ejemploConf)
import Dibujos.Feo (feoConf)
-- import Grilla (grillaConf)
import Dibujos.Escher (escherConf)


-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [feoConf,ejemploConf,escherConf]

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
    putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n = 
    if n == name c then
        initial c 400
    else
        initial' cs n
 

confToList :: [Conf] -> [String]
confToList = map name

printList :: [String] -> IO ()
printList [] = return ()
printList (x:xs) = do
    putStrLn $ "-" ++ x
    printList xs


main :: IO ()
main = do
    args <- getArgs
    let choicePrint =
            if args == ["--lista"] then 
            do
            putStrLn "Elegir dibujos disponibles: "
            printList $ confToList configs
            arg <- getLine
            initial' configs arg
            else
            initial' configs $ head args

    choicePrint