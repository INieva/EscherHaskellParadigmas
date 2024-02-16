module Dibujos.Escher (
    interpBas,
    escherConf
) where
    
import Graphics.Gloss (white, line, polygon, pictures)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar, cuarteto, encimar4,r180, r270, ciclar)

import Interp (Conf(..), interp)

import FloatingPic (Output, half, zero, vacia)


data Triangulo =
    Isosceles | Vacio
    deriving (Eq, Show)

type Escher = Triangulo


-- El dibujoU.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 p2
            where p2 = espejar $ rot45 p
-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar p3 p4)
            where p3 = espejar (rot45 p)               
                  p4 = r270 p3

---- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 _ =  figura Vacio
esquina n p = cuarteto (esquina (n-1) p)                --
                    (lado (n-1) p)                      --
                    (rotar (lado (n-1) p ))             --
                    (dibujoU p)                         -- 

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 _ = figura Vacio
lado n p = cuarteto (lado (n-1) p)                     --
                  (lado (n-1) p)                       --
                  (rotar (dibujoT p))                  --
                  (dibujoT p)                          --

fila a b c = juntar 1 2 a (juntar 1 1 b c)
-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = apilar 2 1 (juntar 2 1 p (juntar 1 1 q r)) (apilar 1 1 (juntar 2 1 s (juntar 1 1 t u)) (juntar 2 1 v (juntar 1 1 w x)))

-- El dibujo de Escher:
escher :: Int -> Dibujo Escher -> Dibujo Escher
escher n p = noneto (esquina n p) (lado n p) (r270 $ esquina n p) (rotar $ lado n p) (dibujoU p) (r270 $ lado n p) (rotar $ esquina n p) (r180 $ lado n p) (r180 $ esquina n p)
-- Defino mi figura en base a Dibujo y utilizo a escher
--ejemplo = dibujoU (figura Isosceles)
ejemplo :: Dibujo Escher
ejemplo =  escher 3 (figura Isosceles)
-- Indico la interpretacion de mi f
interpBas :: Output Escher
interpBas Isosceles a b c = pictures [line $ triangulo a b c, cara a b c]
  where
      triangulo a b c = map (a V.+) [zero, c, b, zero]
      cara a b c = polygon $ triangulo (a V.+ half c) (half b) (half c)
interpBas Vacio a b c = vacia a b c


escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = interp interpBas ejemplo
}
