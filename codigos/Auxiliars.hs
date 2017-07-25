
module Auxiliars
(GrafoI,Arista,MultiConj,vacio,inserta,borra,esVacio,cardinal,pertenece,
 noPertenece,ocurrencias,nAristas,gradoPos,arista,aristaOp,grafo) where

import I1M.Grafo
import qualified Data.Map as M

type GrafoI = Grafo Int Int
type Arista = (Int,Int,Int)
type MultiConj = M.Map Arista Int


-- ---------------------------------------------------------------------
-- § Funciones del TAD de los multiconjuntos                          --
-- --------------------------------------------------------------------- 

vacio :: MultiConj
vacio = M.empty

inserta :: Arista -> MultiConj -> MultiConj
inserta x = M.insertWith (+) x 1

borra :: Arista -> MultiConj -> MultiConj
borra = M.update f
    where f m | m <= 1    = Nothing
              | otherwise = Just (m - 1)

esVacio :: MultiConj -> Bool
esVacio = M.null

cardinal :: MultiConj -> Int
cardinal = sum . M.elems

pertenece :: Arista -> MultiConj -> Bool
pertenece = M.member

noPertenece :: Arista -> MultiConj -> Bool
noPertenece = M.notMember

ocurrencias :: Arista -> MultiConj -> Int
ocurrencias = M.findWithDefault 0


-- ---------------------------------------------------------------------
-- § Funciones auxiliares                                             --
-- ---------------------------------------------------------------------

grafo :: [(Int,Int)] -> GrafoI
grafo as = creaGrafo ND (m,n) [(x,y,0) | (x,y) <- as]
    where ns = map fst as ++ map snd as
          m  = minimum ns
          n  = maximum ns

lazos :: GrafoI -> [(Int,Int)]
lazos g = [(x,y) | (x,y,z) <- aristas g, x == y]

nLazos :: GrafoI ->  Int
nLazos = length . lazos

nAristas :: GrafoI ->  Int
nAristas g 
    | dirigido g = length (aristas g)
    | otherwise  = (length (aristas g) `div` 2) + nLazos g

gradoPos :: GrafoI -> Int -> Int
gradoPos g  = length . adyacentes g

arista :: Int -> Int -> Arista
arista x y = (x,y,0)

aristaOp :: Arista -> Arista
aristaOp (x,y,z) = (y,x,z)
