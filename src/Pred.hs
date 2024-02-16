module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where
import Dibujo


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
