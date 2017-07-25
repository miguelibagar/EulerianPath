
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Properties
(GrafoI,esConexo,genG,prop_caminosEulerianos1,prop_caminosEulerianos2) where

import Searcher
import I1M.Grafo
import Test.QuickCheck
import I1M.BusquedaEnEspaciosDeEstados
import Data.Maybe

type GrafoI = Grafo Int Int


-- ---------------------------------------------------------------------
-- § Grafos conexos                                                   --
-- ---------------------------------------------------------------------

caminos :: Grafo Int Int -> Int -> Int -> [[Int]]
caminos g a b = buscaEE sucesores esFinal inicial
    where inicial          = [b]
          sucesores (x:xs) = [z:x:xs | z <- adyacentes g x
                                     , z `notElem` (x:xs)] 
          esFinal (x:xs)   = x == a

esConexo :: Grafo Int Int -> Bool
esConexo g = and [not (null (caminos g v w)) | v <- xs, w <- xs]
  where xs = nodos g


-- ---------------------------------------------------------------------
-- § Propiedades                                                      --
-- ---------------------------------------------------------------------

prop_caminosEulerianos1 :: GrafoI -> Property
prop_caminosEulerianos1 g =  prop g  ==>
  if not (hayCamino g) then isNothing ce else isJust ce
   where prop g = not (dirigido g)  &&  esConexo g
         ce     = caminoEuleriano g

prop_caminosEulerianos2 :: GrafoI -> Property
prop_caminosEulerianos2 g = prop g  ==>  hayCamino g
   where ce     = caminoEuleriano g
         prop g = not (dirigido g)  &&  isJust ce


-- ---------------------------------------------------------------------
-- § Generador de grafos                                              --
-- ---------------------------------------------------------------------

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    ghci> generaGND 3 [4,2,5]
--    (ND,array (1,3) [(1,[(2,4),(3,2)]),
--                     (2,[(1,4),(3,5)]),
--                      3,[(1,2),(2,5)])])
--    ghci> generaGND 3 [4,-2,5]
--    (ND,array (1,3) [(1,[(2,4)]),(2,[(1,4),(3,5)]),(3,[(2,5)])])
generaGND :: Int -> [Int] -> GrafoI
generaGND n ps  = creaGrafo ND (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- (generaGD n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    ghci> generaGD 3 [4,2,5]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[]),
--                    (3,[])])
--    ghci> generaGD 3 [4,2,5,3,7,9,8,6]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[(1,3),(2,7),(3,9)]),
--                    (3,[(1,8),(2,6)])])
generaGD :: Int -> [Int] -> GrafoI
generaGD n ps = creaGrafo D (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n]]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGD es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGD
--    (D,array (1,4) [(1,[(1,1)]),(2,[(3,1)]),(3,[(2,1),(4,1)]),(4,[(4,1)])])
--    (D,array (1,2) [(1,[(1,6)]),(2,[])])
--    ...
genGD :: Gen GrafoI
genGD = do n <- choose (1,10)
           xs <- vectorOf (n*n) arbitrary
           return (generaGD n xs)

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGND
--    (ND,array (1,1) [(1,[])])
--    (ND,array (1,3) [(1,[(2,3),(3,13)]),(2,[(1,3)]),(3,[(1,13)])])
--    ...
genGND :: Gen GrafoI
genGND = do n <- choose (1,10)
            xs <- vectorOf (n*n) arbitrary
            return (generaGND n xs)

-- genG es un generador de grafos. Por ejemplo,
--    ghci> sample genG
--    (D,array (1,3) [(1,[(2,1)]),(2,[(1,1),(2,1)]),(3,[(3,1)])])
--    (ND,array (1,3) [(1,[(2,2)]),(2,[(1,2)]),(3,[])])
--    ...
genG :: Gen GrafoI
genG = do d <- choose (True,False)
          n <- choose (1,10)
          xs <- vectorOf (n*n) arbitrary
          if d then return (generaGD n xs)
               else return (generaGND n xs)

-- Los grafos está contenido en la clase de los objetos generables
-- aleatoriamente. 
instance Arbitrary GrafoI where
    arbitrary = genG
