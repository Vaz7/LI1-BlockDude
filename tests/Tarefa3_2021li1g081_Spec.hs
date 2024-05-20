module Tarefa3_2021li1g081_Spec where

import Test.HUnit
import Tarefa3_2021li1g081
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m1e3" ~: "      >\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e3
    , "Tarefa 3 - Teste Imprime Jogo m1e4" ~: "       \n      X\n      X\nP > C X\nXXXXXXX" ~=?  show m1e4
    , "Tarefa 3 - Teste Imprime Jogo m2e1" ~: " X                 \n X   XXXXXXXXXXXXX \nX X X             X\nX  X              X\nX                CX\nX               CCX\nX XXX    >   XC XX \nX X X    X  XXXXX  \nX X XCC XX  X      \nXPX XXXXXX XX      \nXXX XX   XXX       " ~=?  show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m2e2" ~: " X                 \n X   XXXXXXXXXXXXX \nX X X             X\nX  X              X\nX                CX\nX               CCX\nX XXX        XC XX \nX X X    X  XXXXX  \nX X XCC<XX  X      \nXPX XXXXXX XX      \nXXX XX   XXX       " ~=?  show m2e2
    , "Tarefa 3 - Teste Imprime Jogo m3e1" ~: "X             X\nX             X\nX             X\nX             X\nX            CX\nX     XX   CCX \nXP   X X< XXX  \nXXXXX  XXX     " ~=?  show m3e1
    , "Tarefa 3 - Teste Imprime Jogo m3e2" ~: "X             X\nX             X\nX             X\nX             X\nX            CX\nX     XX  >CCX \nXP   X X  XXX  \nXXXXX  XXX     " ~=?  show m3e2
    , "Tarefa 3 - Teste Imprime Jogo m7e1" ~: "X                 X\nX                 X\nX                 X\nX                 X\nX                 X\nX           X     X\nXP  X   X C X C < X\nXXXXXXXXXXXXXXXXXXX" ~=?  show m7e1
    , "Tarefa 3 - Teste Imprime Jogo m7e2" ~: "X                 X\nX                 X\nX                 X\nX                 X\nX           >     X\nX           X     X\nXP  X   X C X C   X\nXXXXXXXXXXXXXXXXXXX" ~=?  show m7e2
    , "Tarefa 3 - Teste Imprime Jogo m8e1" ~: "     XXX    XXXXXXXXXX \n XXXX   XXXX          X\nX                     X\nX                     X\nX                     X\nX     X               X\nX     X               X\nX     XCCCC           X\nXP   XXXXXXX<         X\nXX XXX     XX X      CX\n X X        X XX    CCX\n X X        X XX   CCCX\n XXX        X XXXXXXXXX\n            XXX        " ~=?  show m8e1
    , "Tarefa 3 - Teste Imprime Jogo m8e2" ~: "     XXX    XXXXXXXXXX \n XXXX   XXXX          X\nX                     X\nX                     X\nX                     X\nX     X               X\nX     X >             X\nX     XCCCC           X\nXP   XXXXXXX          X\nXX XXX     XX X      CX\n X X        X XX    CCX\n X X        X XX   CCCX\n XXX        X XXXXXXXXX\n            XXX        " ~=?  show m8e2
    ]