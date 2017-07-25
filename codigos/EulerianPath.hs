
-- EL PROBLEMA DE LOS PUENTES DE KÖNIGSBERG. Versión final
-- Suposiciones: grafos conexos y no dirigidos

-- ---------------------------------------------------------------------
-- § Importación de librerías                                         --
-- ---------------------------------------------------------------------

import System.Exit
import Data.Char
import Data.Maybe

import Examples
import Properties
import Searcher
import Auxiliars (grafo)
import Test.QuickCheck


-- ---------------------------------------------------------------------
-- § Main                                                             --
-- ---------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ("Welcome to the graph Eulerian path searcher.\n"
            ++ "Miguel Ibanez Garcia, University of Seville, 2017.\n" ++ "\n" ++
            "Differences between capital and small" ++
            " letters are not considered.\n")
  putStrLn (". Type EXAMPLE in order to work with our graphs library.\n"
            ++ ". Type CREATE to work with your own graph.\n"
            ++ ". Type PROPS to check the properties with QuickCheck.\n"
            ++ ". Type END to exit.\n")
  entrada' <- getLine
  let entrada = interpr entrada'
  putStr "\n"
  if entrada == "create"
    then do
        putStrLn "Type a list containing pairs of edges."
        putStrLn ("It is not needed to include inverse edges,"
                  ++ " as we are working only with\nundirected graphs.\n")
        as' <- getLine
        let as = interpr as'
        putStr "\n"
        let ps = read as :: [(Int,Int)]
        let g = grafo ps
        let x = caminoEuleriano g
        if isNothing x
          then putStrLn "An Eulerian path cannot be found in the graph.\n"
          else putStrLn ("An Eulerian path has been found,"
                         ++ " and it is: " ++ show (fromJust x) ++ "\n")
        putStrLn "\n\n ............................. \n\n"
        main
        else if entrada == "example"
            then do
                putStrLn "Type the name of the graph you want to work with."
                putStrLn "It must be gX, X <- ({1,2,...,17,Fallo}\n"
                bs' <- getLine
                let bs = interpr bs'
                putStr "\n"
                let y = caminoEuleriano (asocia bs)
                if isNothing y
                  then putStrLn "An Eulerian path cannot be found in the graph.\n"
                  else putStrLn ("An Eulerian path has been found,"
                                 ++ " and it is: " ++ show (fromJust y) ++ "\n")
                putStrLn "\n\n ............................. \n\n"
                main
        else if entrada == "props"
            then do
                putStrLn "There are two properties you can check:"
                putStrLn ("1. If G is a connected graph and only 2 or 0"
                          ++ " vertices have odd degree,\n then an Eulerian"
                          ++ " path can be found in G.\n"
                          ++ "2. If it has been found an Eulerian path in G,\n"
                          ++ " then there are only 2 or 0 vertices with"
                          ++ " odd degree.\n")
                putStrLn ("Type PROP1 to check the first, and PROP2 to check"
                          ++ " the second.\n")
                cs' <- getLine
                putStr "\n"
                let cs = interpr cs'
                if cs == "prop1"
                  then do verboseCheck prop_caminosEulerianos1
                          putStr "\n\n ............................. \n\n"
                          main
                  else if cs == "prop2"
                      then do verboseCheck prop_caminosEulerianos2
                              putStrLn "\n\n ............................. \n\n"
                              main
                      else do putStrLn "ERROR: Incorrect input"
                              putStrLn "\n\n ............................. \n\n"
                              main
        else if entrada == "end"
               then exitSuccess
               else do putStrLn "ERROR: Incorrect input"
                       putStrLn "\n\n ............................. \n\n"
                       main


-- ---------------------------------------------------------------------
-- § Auxiliares                                                             --
-- ---------------------------------------------------------------------

interpr :: String -> String
interpr = map toLower

asocia :: String -> GrafoI
asocia "g1"     = g1
asocia "g2"     = g2
asocia "g3"     = g3
asocia "g4"     = g4
asocia "g5"     = g5
asocia "g6"     = g6
asocia "g7"     = g7
asocia "g8"     = g8
asocia "g9"     = g9
asocia "g10"    = g10
asocia "g11"    = g11
asocia "g12"    = g12
asocia "g13"    = g13
asocia "g14"    = g14
asocia "g15"    = g15
asocia "g16"    = g16
asocia "g17"    = g17
asocia "gFallo" = gFallo
