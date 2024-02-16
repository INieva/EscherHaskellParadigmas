module Interp (
    interp,
    Conf(..),
    interpConf,
    initial,
    FloatingPic,
    trianguloIsos
) where

import Graphics.Gloss(Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display, line)
import Dibujo (Dibujo, foldDib)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import FloatingPic (Output,FloatingPic,half, grid) 


rotarInterp :: FloatingPic -> FloatingPic
rotarInterp pic x w h = pic (x V.+ w) h (V.negate w)

rot45Interp :: FloatingPic -> FloatingPic
rot45Interp pic x w h = pic (x V.+ half (w V.+ h)) (half (w V.+ h)) (half (h V.- w))

espejarInterp :: FloatingPic -> FloatingPic
espejarInterp pic x w = pic (x V.+ w) (V.negate w) 

u :: Picture -> Picture -> Picture
u pic1 pic2 = pictures [pic1, pic2]

encimarInterp :: FloatingPic -> FloatingPic -> FloatingPic
encimarInterp pic1 pic2 x w h = u (pic1 x w h) (pic2 x w h)

juntarInterp :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
juntarInterp n m pic1 pic2 x w h = u (pic1 x w' h) (pic2 (x V.+ w') (r' V.* w) h)
                             where r = m / (m + n)
                                   r' = n / (m + n)
                                   w' = r V.* w 


apilarInterp :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
apilarInterp n m pic1 pic2 x w h = u (pic1 (x V.+ h') w (r V.* h)) (pic2 x w h')
                             where r = m / (m + n)
                                   r' = n / (m + n)
                                   h' = r' V.* h 
trianguloIsos :: FloatingPic
trianguloIsos x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]

-- Interpretación de un dibujo
-- formulas sacadas del enunciado
interp :: Output a -> Output (Dibujo a)
interp fig = foldDib fig rotarInterp espejarInterp rot45Interp apilarInterp juntarInterp encimarInterp    

-- Configuración de la interpretación
data Conf = Conf {
        name :: String,
        pic :: FloatingPic
    }

interpConf :: Conf -> Float -> Float -> Picture 
interpConf (Conf _ p) x y = p (0, 0) (x,0) (0,y)

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial cfg size = do
    let n = name cfg
        win = InWindow n (ceiling size, ceiling size) (0, 0)
    display win white $ withGrid (interpConf cfg size size) size size
  where withGrid p x y = translate (-size/2) (-size/2) $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
        grey = makeColorI 120 120 120 120
