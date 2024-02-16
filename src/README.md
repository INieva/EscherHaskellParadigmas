# Laboratorio 1 de Paradigmas: Un lenguaje vale más que mil palabras 
## Paradigmas de la Programación 2023-FAMAF UNC.

## Integrantes:

- [Tomas Agustin Colazo](https://bitbucket.org/I_Nieva/)
  (tomas.colazo@mi.unc.edu.ar)
- [Francisco Cecchi](https://bitbucket.org/I_Nieva/)
  (francisco.cecchi@mi.unc.edu.ar)
- [Iván Nieva](https://bitbucket.org/I_Nieva/)
  (i.nieva@mi.unc.edu.ar)


## **Introducción**
En éste laboratorio de la materia Paradigmas de la programación se pretende lograr la implementación de un pequeño lenguaje de dominio especifico (DSL) para lograr diseños (dibujos) complejos a partir de un conjunto de objetos gráficos primitivos convenientemente compuestos mediante un lenguaje funcional.

Uno de los grandes objetivos es lograr reproducir el dibujo de Escher.
Los dibujos de Escher juegan con la geometría y la repetición de un elemento básico que esta adecuadamente orientado, escalado, coloreado. En la siguiente figura se aprecian las ideas antes mecionadas, podemos ver un figura compleja lograda a partir de múltiples repeticiones de un patrón simple.

<p align="center">
  <img src=GraficasLogradas/escherReadmi.jpg />

<i> Obra de Escher creada con un patrón simple (pez) logrando un mosaico con peces, una de sus muchas obras de arte. </i>
</p>

En su obra, Escher juega con la geometría para plasmar figuras imposibles en las que conviven varias interpretaciones espaciales incompatibles entre sí. Para ello emplea diestros trazos o sombras que fuerzan al cerebro a dotar de profundidad a una imagen.

Para poder implementar convenientemente las ideas anteriores es necesario un lenguaje que tenga la funcionalidades de lograr estas operaciones. En el presente laboratorio se pretende definir una sintaxis y una semántica de tal lenguaje. 



## **Desarrollo**
  En ésta sección brindamos la información acerca del desarrollo del proyecto. Daremos algunas definiciones para establecer claridad en los conceptos que aquí tratamos y la explicación acerca de como fue el desarrollo y la línea de trabajo seguida.

### **Breve explicación de los requerimientos** 
  El lenguaje que pretendemos construir está parametrizado sobre una colección de figuras básicas y un conjunto de instrucciones para la manipulación de las mismas. La tarea reside en definir de forma apropiada éstas instrucciones. A continuación listamos la tarea a realizar.
  
  - Definir el lenguaje como un tipo de datos.
  - Definir un conjunto de combiandores.
  - Definir esquemas que permitan manipular figuras.
  - Definir un conjunto de predicados. 
  - Escribir un conjunto de tests.

### **Metodología de trabajo**
El trabajo se llevo a cabo de manera virtual/presencial. La presencialidad se puedo lograr durante los días del cursado de la materia haciendo uso de la infraestructura dispuesta por la facultad.
El complemento fue virtual, cada uno de los integrantes del grupo trabajó de forma independiente en su hogar y posteriormente fue subiendo los avances logrados al repositorio del laboratorio.

### **Preguntas**

**1  ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.**

El proposito de que las diferentes funcionalidades se encuentren separadas en los módulos correspondientes se debe a que con éste hecho podemos modificar la implementación de nuestra sintaxis o semántica del lenguaje de manera independiente.

Los módulos principales son:

**Dibujo.hs**

Aquí tenemos definidos los tipos de datos para las figuras a ser dibujadas. En éste caso esta implementado de forma polimorfica dado que a priori no teniamos conocimiento de que figuras basicas teniamos.
Así mismo, aquí tambien tenemos definidos los distintos combinadores para éstas figuras. Los combinadores nos permiten la manipulación de figuras para lograr distintas operaciones sobre las mismas como por ejemplo, rotaciones, encimar figuras, solaparlas, etc.

**Interp.hs**

En Interp.hs tenemos la implementación de como realmente van a ser dibujadas las figuras en pantalla. Usamos la libreria Gloss. 
También tenemos la implementación de algunas figuras básicas.

**Escher.hs**

Aquí se define la manera en que se dibuja la figura de Escher. Se define el tipo con el que se va a instanciar Dibujo y además como será el proceso de dibujado de manera recursiva.


**Pred.hs**

En éste módulo tenemos definido una serie de predicados que se aplican a las figuras básicas.

**Main.hs**

Éste es el archivo principal de nuestro código, desde aquí se hacen las llamadas necesarias a los distintos módulos para lograr las gráficas deseadas.

**2 ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez es un parámetro del tipo?**

Esto es debido a que Basica puede cambiar de acuerdo a la implemenatción, esto se pude observar en los archivos de Escher.hs y Ejemplo.hs donde hay una definición de Basica propia para cada uno de estos archivos.

**3 ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?**

Una ventaja de utilizar una función de fold es que tiene mayor flexibilidad y reutilización. 

Con una función de fold podemos hacer recursión y es más general y abstracta que hacer pattern-matching. Cuando hacemos uso de pattern-matching debemos especificar casos para cada posible patrón que podamos encontrar y si deseamos hacer algo difrenete, debemos agregar un nuevo caso, esto puede llevar a un código más extenso.

Con una función de fold podemos además reutilizar el mismo código con diferentes tipos de datos dado que es más general y abstracta.

### **Implementaciones realizadas**

Aquí brindamos información acerca de como hemos implementando los diferentes requerimientos de éste laboratorio.

- Definición del lenguaje:
```haskell
data Dibujo a =
    Basica a
    | Rotar (Dibujo a)
    | Espejar (Dibujo a)
    | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)
```
- Combinadores:
```haskell
--Composición n-veces de una función con sí misma. 
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
                  c = r180 a
                  d = r270 a
```
- Esquemas para manipular figuras Básicas
```haskell
-- Estructura general para la semántica.
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

--Funcion map para nuestro lenguage DSL.
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
```
```haskell
--Predicados.
type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a ->Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred fun = mapDib predFun
                   where predFun n = if pred n then fun n else figura n

              
-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred = foldDib pred id id id orFold orFold (||)
       where orFold _ _ = (||)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred = foldDib pred id id id andFold andFold (&&)
       where andFold _ _ = (&&)


-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP pred1 pred2 = newPred
              where newPred n = pred1 n && pred2 n

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP pred1 pred2 = newPred
              where newPred n = pred1 n || pred2 n
```
Una vez realizado todo el trabajo anterior podemos darle uso a nuestro lenguaje. Esto se muestra en la sección siguiente.

### **Usos de nuestro lenguaje**

#### **Dibujos logrados** 

Aquí mostramos algunas de las figuras que hemos obtenido.

**Dibujo Feo:**

Compilacion: ghc Main.hs

Ejecucion: ./Main Feo



<p align="center">
  <img src=GraficasLogradas/Feo.png />

  <i> Dibujo Feo usado como una prueba de la distintas operaciones.</i>
</p>


**Dibujo Grilla:**

Compilacion: ghc Main.hs

Ejecucion: ./Main Grilla


<p align="center">
  <img src=GraficasLogradas/Grilla.png />

  <i> Dibujo Grilla, una grilla numerada de 8x8. Se ennumeran los cuadrantes de la forma (fila, columna). </i>
</p>

**Dibujo Escher:**

Compilacion: ghc Main.hs

Ejecucion: ./Main Escher

<p align="center">
  <img src=GraficasLogradas/Escher.png />

  <i> El dibujo de Escher, uno de los objetivos principales del presente trabajo.</i>
</p>

## **Observaciones y Conclusiones**
Como observaciones finales mencionamos que para lograr un buen desarrollo del presente trabajo ha sido de mucha utilidad toda la información brindada por la cátedra de la materia.

Así mismo, la bibliografía que hemos usado también tuvo un impacto positivo durante todo el desarrollo del laboratorio.

Destacamos también que el proyecto ha sido muy interesante, aprendimos a separar la sintaxis de la semántica y logramos generar las abstracciones necesarias.

## **Referencias**
- https://mapio.github.io/programming-with-escher/
- https://eprints.soton.ac.uk/257577/1/funcgeo2.pdf
- https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Picture.html



