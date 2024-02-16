module Dibujos.Grilla (
    grilla,
    grillaConf
) where

import Dibujo (Dibujo, juntar, apilar, figura, espejar, rot45)
import Interp ( Conf(..), interp)
import Graphics.Gloss (Vector, Picture(..), text, scale, translate, rotate)
import FloatingPic (Output)


type Pair = (Int, Int)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar (fromIntegral $ length ds) 1 d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar (fromIntegral $ length ds) 1 d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

dibPair :: Pair -> Dibujo Pair
dibPair = figura


interpVec :: Output Pair
interpVec p (a1,a2) (b1,b2) (c1,c2) = vecTranslate a b c $ scale 0.1 0.1 $ text $ show p
                    where a = (a1,a2)
                          b = (b1,b2)
                          c = (c1,c2)
                          vecTranslate (x1,y1) (x2,y2) (x3,y3) pic = translate (x3-yAxisOffset) (y3-yAxisOffset) $ translate (x2-xAxisOffset) (y2-xAxisOffset) $ translate x1 y1 pic
                          xAxisOffset = 28
                          yAxisOffset = 4

dibStr :: String -> Dibujo String
dibStr = figura

allNums :: Dibujo Pair
allNums = grilla [
        [dibPair (0, 0),dibPair (0, 1),dibPair (0, 2),dibPair (0, 3),dibPair (0, 4),dibPair (0, 5),dibPair (0, 6),dibPair (0, 7)],
        [dibPair (1, 0),dibPair (1, 1),dibPair (1, 2),dibPair (1, 3),dibPair (1, 4),dibPair (1, 5),dibPair (1, 6),dibPair (1, 7)],
        [dibPair (2, 0),dibPair (2, 1),dibPair (2, 2),dibPair (2, 3),dibPair (2, 4),dibPair (2, 5),dibPair (2, 6),dibPair (2, 7)],
        [dibPair (3, 0),dibPair (3, 1),dibPair (3, 2),dibPair (3, 3),dibPair (3, 4),dibPair (3, 5),dibPair (3, 6),dibPair (3, 7)],
        [dibPair (4, 0),dibPair (4, 1),dibPair (4, 2),dibPair (4, 3),dibPair (4, 4),dibPair (4, 5),dibPair (4, 6),dibPair (4, 7)],
        [dibPair (5, 0),dibPair (5, 1),dibPair (5, 2),dibPair (5, 3),dibPair (5, 4),dibPair (5, 5),dibPair (5, 6),dibPair (5, 7)],
        [dibPair (6, 0),dibPair (6, 1),dibPair (6, 2),dibPair (6, 3),dibPair (6, 4),dibPair (6, 5),dibPair (6, 6),dibPair (6, 7)],
        [dibPair (7, 0),dibPair (7, 1),dibPair (7, 2),dibPair (7, 3),dibPair (7, 4),dibPair (7, 5),dibPair (7, 6),dibPair (7, 7)]
        ]


grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla",
    pic = interp interpVec allNums
}

