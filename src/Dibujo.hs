{-# LANGUAGE LambdaCase #-}
module Dibujo (
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar,
    foldDib, mapDib,
    figuras
) where



data Dibujo a =
    Basica a
    | Rotar (Dibujo a)
    | Espejar (Dibujo a)
    | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

figura :: a -> Dibujo a
figura = Basica


rotar :: Dibujo a -> Dibujo a
rotar = Rotar


espejar :: Dibujo a -> Dibujo a
espejar = Espejar


rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45


apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar


juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar


encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar


--3 Definir los siguinetes combinadores--

--Composición n-veces de una función con sí misma. Componer 0 veces es 
--la función constante, componer 1 vez es aplicar la función 1 vez, etc.
--Componer negativamente es un error.
comp :: (a -> a) -> Int -> a -> a
comp f 0 x = f x
comp f n x = f (comp f (n-1) x)

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 = comp Rotar 1

r270 :: Dibujo a -> Dibujo a
r270 = comp Rotar 2

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (.-.) x y
                    where x = (///) a b
                          y = (///) c d


-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 a = (^^^) x y
              where x = (^^^) a (r180 a)
                    y = rotar x


-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar a = cuarteto a b c d
            where b = rotar a
                  c = r270 a
                  d = r180 a

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (b -> b -> b) ->
       Dibujo a -> b
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Basica fig) = bas fig
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Rotar dib) = rotar' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar'  dib)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Rot45 dib) = rot45' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar'  dib)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Espejar dib) =  espejar'  (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar'  dib)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Apilar x y dib1 dib2) = apilar' x y (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib1) (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib2)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Juntar x y dib1 dib2) = juntar' x y (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib1) (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib2)
foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' (Encimar dib1 dib2) = encimar' (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib1) (foldDib bas rotar' espejar' rot45' apilar' juntar' encimar' dib2)



mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f (Basica fig) = f fig
mapDib f (Rotar dib) = rotar (mapDib f dib)
mapDib f (Rot45 dib) = rot45 (mapDib f dib)
mapDib f (Espejar dib) = espejar (mapDib f dib)
mapDib f (Apilar x y dib1 dib2) = apilar x y (mapDib f dib1) (mapDib f dib2)
mapDib f (Juntar x y dib1 dib2) = juntar x y (mapDib f dib1) (mapDib f dib2)
mapDib f (Encimar dib1 dib2) = encimar (mapDib f dib1) (mapDib f dib2)

-- Junta todas las figuras básicas de un dibujo.

figuras :: Dibujo a -> [a]
figuras = foldDib (:[]) id id id concFigs concFigs (++)
    where concFigs _ _ = (++) 
