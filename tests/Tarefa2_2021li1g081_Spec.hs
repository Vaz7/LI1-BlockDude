module Tarefa2_2021li1g081_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g081
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Mapa m2" ~: m2r ~=? constroiMapa m2
    , "Tarefa 2 - Teste Desconstruir Mapa m2" ~: sort m2 ~=?  sort (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Construir Mapa m3" ~: m3r ~=? constroiMapa m3
    , "Tarefa 2 - Teste Desconstruir Mapa m3" ~: sort m3 ~=?  sort (desconstroiMapa m3r)
    , "Tarefa 2 - Teste Construir Mapa m7" ~: m7r ~=? constroiMapa m7
    , "Tarefa 2 - Teste Desconstruir Mapa m7" ~: sort m7 ~=?  sort (desconstroiMapa m7r)
    , "Tarefa 2 - Teste Construir Mapa m8" ~: m8r ~=? constroiMapa m8
    , "Tarefa 2 - Teste Desconstruir Mapa m8" ~: sort m8 ~=?  sort (desconstroiMapa m8r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Identidade m2" ~: sort m2 ~=?  sort (desconstroiMapa (constroiMapa m2))
    , "Tarefa 2 - Teste Identidade m2r" ~: m2r ~=?  constroiMapa (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Identidade m3" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m3r" ~: m3r ~=?  constroiMapa (desconstroiMapa m3r)
    ]