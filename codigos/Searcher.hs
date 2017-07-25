
module Searcher
(hayCamino,caminoEuleriano) where

import I1M.Grafo
import I1M.BusquedaEnEscalada
import I1M.BusquedaEnEspaciosDeEstados
import Data.Maybe
import Auxiliars


-- ---------------------------------------------------------------------
-- § Verificar si es un grafo con camino euleriano                    --
-- ---------------------------------------------------------------------

nNodosImp :: GrafoI -> Int
nNodosImp g = length (filter odd (map (gradoPos g) (nodos g)))

hayCamino :: GrafoI -> Bool
hayCamino g = nNodosImp g  `elem`  [0,2]

hayCiclo :: GrafoI -> Bool
hayCiclo g = nNodosImp g == 0


-- ---------------------------------------------------------------------
-- § Heurísticas                                                      --
-- ---------------------------------------------------------------------

-- Resolveremos este problema mediante búsqueda de espacios de estado.
-- El tipo de los nodos o estados es:
data Estado = EST ([Int],MultiConj)
   deriving Show

instance Eq Estado
  where est1 == est2 = heur est1 == heur est2
instance Ord Estado
  where est1 <= est2 = heur est1 <= heur est2

heur :: Estado -> Int
heur (EST (_,m)) = cardinal m


-- ---------------------------------------------------------------------
-- § Funciones básicas en búsqueda en espacios de estados             --
-- ---------------------------------------------------------------------

sucesores :: GrafoI -> Estado -> [Estado]
sucesores g (EST (x:xs,ys)) = [EST (y:x:xs, insAr h ys) | y <- adyacentes g x,
                                                          let h = arista x y,
                                                          prop h ys]
  where prop h ys | x == 0     =  True
                  | otherwise  =  x < nAr h (conjAristas g)
          where x = nAr h ys

esFinal :: GrafoI -> Estado -> Bool
esFinal g (EST (_,m)) = cardinal m == nAristas g

inicial :: GrafoI -> Int -> Estado
inicial g k = EST ([nodos g !! (k-1)],vacio)
     -- k debe variar entre 1 y el número de nodos


-- ---------------------------------------------------------------------
-- § Búsqueda del camino en espacios de estados (general)             --
-- ---------------------------------------------------------------------

caminosEE :: GrafoI -> [Estado]
caminosEE g = concat [buscaEE (sucesores g) (esFinal g) (inicial g k) | k <- [1..n]]
  where n = length (nodos g)

caminoEE :: GrafoI -> Maybe [Int]
caminoEE g = aux (caminosEE g)
  where aux []          = Nothing
        aux ((EST x):_) = Just $ fst x


-- ---------------------------------------------------------------------
-- § Búsqueda del camino en espacios de estados (escalada)            --
-- ---------------------------------------------------------------------

caminosEsc :: GrafoI -> [Estado]
caminosEsc g = concat
  [buscaEscalada (sucesores g) (esFinal g) (inicial g k) | k <- [1..n]]
  where n = length (nodos g)

caminoEsc :: GrafoI -> Maybe [Int]
caminoEsc g = aux (caminosEsc g)
  where aux []          = Nothing
        aux ((EST x):_) = Just $ fst x


-- ---------------------------------------------------------------------
-- § Búsqueda del camino euleriano                                    --
-- ---------------------------------------------------------------------

caminoEuleriano :: GrafoI -> Maybe [Int]
caminoEuleriano g | isNothing x && hayCamino g  =  caminoEE g
                  | otherwise                   =  x
  where x = caminoEsc g


-- ---------------------------------------------------------------------
-- § Funciones útiles para espacios de estados usando multiconjuntos  --
-- ---------------------------------------------------------------------

conjAristas :: GrafoI -> MultiConj
conjAristas g = (`div` 2)  <$>  aux (aristas g) vacio
  where aux []     m = m
        aux (x:xs) m | prop x m   =  aux xs (inserta x' m)
                     | otherwise  =  aux xs (inserta x  m)
          where x'       = aristaOp x
                prop x m = x `noPertenece` m  &&  x' `pertenece` m

-- Los multiconjuntos (diccionarios) son functores.

nAr :: Arista -> MultiConj -> Int
nAr x m | x `pertenece` m   =  ocurrencias x m
        | x' `pertenece` m  =  ocurrencias x' m
        | otherwise         =  0
  where x' = aristaOp x

insAr :: Arista -> MultiConj -> MultiConj
insAr x m | x' `pertenece` m  =  inserta x' m
          | otherwise         =  inserta x m
  where x' = aristaOp x
