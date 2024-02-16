module TestDibujo where 

import Test.HUnit
import Dibujo
type Basica = Int 

ejemplo :: Dibujo Basica
ejemplo = figura 5

-- Test de la funcion r180
testR180 = TestCase $ assertEqual "testRotar" (rotar $ rotar ejemplo) (r180 ejemplo)
-- Test de la funcion r270
testR270 = TestCase $ assertEqual "testRotar" (rotar $ rotar $ rotar ejemplo) (r270 ejemplo)
-- Test de la funcion (.-.)
testPunto = TestCase $ assertEqual "testPunto" ((.-.) ejemplo ejemplo) (apilar 1 1 ejemplo ejemplo)
--Test de la funcion (///)
testBarra = TestCase $ assertEqual "testBarra" ((///) ejemplo ejemplo) (juntar 1 1 ejemplo ejemplo)
--Test de la funcion (^^^)
testElevar = TestCase $ assertEqual "testElevar" ((^^^) ejemplo ejemplo) (encimar ejemplo ejemplo)
--Test de la funcion cuarteto
testCuarteto = TestCase $ assertEqual "testCuarteto" (cuarteto ejemplo ejemplo ejemplo ejemplo) (apilar 1 1 (juntar 1 1 ejemplo ejemplo) (juntar 1 1 ejemplo ejemplo))
--Test de la funcion encimar4
testEncimar4 = TestCase $ assertEqual "testEncimar4" (encimar4 ejemplo) (encimar (encimar ejemplo (r180 ejemplo)) (rotar $ encimar ejemplo (r180 ejemplo)))
--Test de la funcion ciclar
testCiclar = TestCase $ assertEqual "testCiclar" (ciclar ejemplo) (cuarteto ejemplo (rotar ejemplo) (r270 ejemplo) (r180 ejemplo))
--Test de la funcion figuras
testFiguras = TestCase $ assertEqual "testFiguras" (figuras ejemplo) [5]
--Test de la funcion foldDib
testFoldDib = TestCase $ assertEqual "testFoldDib" (foldDib figura rotar espejar rot45 apilar juntar encimar ejemplo) (figura 5)
--Test de la funcion mapDib
testMapDib = TestCase $ assertEqual "testMapDib" (mapDib id (figura ejemplo)) ejemplo


tests = TestList [testR180, testR270, testPunto, testBarra, testElevar, testCuarteto, testEncimar4, testCiclar, testFiguras, testFoldDib, testMapDib]   
