module TestsPred where 

import Test.HUnit
import Pred (Pred, cambiar, anyDib, allDib, orP, andP )
import Dibujo
import GHC.Unit.State (UnitErr(TrustFlagErr))

type Basica = Int 
ejemplo :: Dibujo Basica
ejemplo = figura 5

intTbas :: Basica -> Dibujo Basica
intTbas n = figura (n-4) 

intbas :: Int -> Basica
intbas n = n

ejemplo2 :: Dibujo Basica
ejemplo2 = figura 1 

ejemplo3 :: Dibujo Basica
ejemplo3 = apilar 1 1 (figura 5) (figura 6)

--Test funcion cambiar
testCambiar = TestCase $ assertEqual "testCambiar" (cambiar (== 5) intTbas ejemplo) ejemplo2
--Test funcion anyDib
testAnyDib = TestCase $ assertEqual "testAnyDib" (anyDib (== 5) ejemplo3) True
--Test funcion allDib
testAllDib = TestCase $ assertEqual "testAllDib" (allDib (== 5) ejemplo3) False 
--test funcion orP True
testOrP = TestCase $ assertEqual "testOrP" (orP (== figura 5) (== figura 3) ejemplo) True
-- test funcion orP False
testOrp2 = TestCase $ assertEqual "testOrp2" (orP (== figura 5) (== figura 3) ejemplo2) False
-- test funcion andP True
testAndP = TestCase $ assertEqual "testandP" (andP (== figura 5) (== figura 5) ejemplo) True
-- test funcion andP False
testAndP2 = TestCase $ assertEqual "testandP2" (andP (== figura 5) (== figura 3) ejemplo) False


tests = TestList [testCambiar, testAnyDib, testAllDib, testOrP, testOrp2, testAndP,testAndP2]