module Tarefa1_2021li1g081_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g081
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m2" ~: validaPotencialMapa m2 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m3" ~: validaPotencialMapa m3 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m4" ~: validaPotencialMapa m4 ~=? False
    , "Tarefa 1 - Teste Valida Mapa m5" ~: validaPotencialMapa m5 ~=? False
    , "Tarefa 1 - Teste Valida Mapa m6" ~: validaPotencialMapa m6 ~=? False
    , "Tarefa 1 - Teste Valida Mapa m7" ~: validaPotencialMapa m7 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m8" ~: validaPotencialMapa m8 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m9" ~: validaPotencialMapa m9 ~=? False
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    ]
