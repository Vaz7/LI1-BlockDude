module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m2 :: [(Peca, Coordenadas)]
m2 = [(Bloco,(0,2)),(Bloco,(0,3)), (Bloco,(0,4)),(Bloco,(0,5)), (Bloco,(0,6)),(Bloco,(0,7)),
      (Bloco,(0,8)),(Bloco,(0,9)), (Bloco,(0,10)),(Bloco,(1,0)), (Bloco,(1,1)),(Porta,(1,9)),
      (Bloco,(1,10)),(Bloco,(2,2)), (Bloco,(2,6)),(Bloco,(2,7)), (Bloco,(2,8)),(Bloco,(2,9)), 
      (Bloco,(2,10)),(Bloco,(3,3)), (Bloco,(3,6)),(Bloco,(4,2)), (Bloco,(4,6)),(Bloco,(4,7)),
      (Bloco,(4,8)),(Bloco,(4,9)), (Bloco,(4,10)),(Bloco,(5,1)), (Caixa,(5,8)),(Bloco,(5,9)),
      (Bloco,(5,10)),(Bloco,(6,1)), (Caixa,(6,8)),(Bloco,(6,9)), (Bloco,(7,1)),(Bloco,(7,9)),
      (Bloco,(8,1)),(Bloco,(8,8)), (Bloco,(8,9)),(Bloco,(9,1)), (Bloco,(9,7)),(Bloco,(9,8)),
      (Bloco,(9,9)),(Bloco,(9,10)), (Bloco,(10,1)),(Bloco,(10,10)), (Bloco,(11,1)),(Bloco,(11,9)),
      (Bloco,(11,10)),(Bloco,(12,1)), (Bloco,(12,7)),(Bloco,(12,8)), (Bloco,(12,9)),(Bloco,(13,1)),
      (Bloco,(13,6)),(Bloco,(13,7)), (Bloco,(14,1)),(Caixa,(14,6)),(Bloco,(14,7)),(Bloco,(15,1)),
      (Bloco,(15,7)),(Bloco,(16,1)),(Caixa,(16,5)),(Bloco,(16,6)),(Bloco,(16,7)),(Bloco,(17,1)),
      (Caixa,(17,4)),(Caixa,(17,5)),(Bloco,(17,6)),(Bloco,(18,2)),(Bloco,(18,3)),(Bloco,(18,4)),
      (Bloco,(18,5))
     ]

m3 :: [(Peca, Coordenadas)]
m3 = [ (Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,4)),
       (Bloco, (0,5)), (Bloco, (0,6)), (Bloco, (0,7)), (Porta, (1,6)), (Bloco, (1,7)),
       (Bloco, (2,7)), (Bloco, (3,7)), (Bloco, (4,7)), (Bloco, (5,6)), (Bloco, (6,5)),
       (Bloco, (7,5)), (Bloco, (7,6)), (Bloco, (7,7)), (Bloco, (8,7)), (Bloco, (9,7)),
       (Bloco, (10,6)), (Caixa, (11,5)), (Bloco, (11,6)), (Caixa, (12,5)), (Bloco, (12,6)), 
       (Caixa, (13,4)), (Bloco, (13,5)), (Bloco, (14,0)), (Bloco, (14,1)), (Bloco, (14,2)), 
       (Bloco, (14,3)), (Bloco, (14,4))
     ]

m4 :: [(Peca,Coordenadas)] -- Falso
m4 = [(Porta, (0,4)), (Bloco, (0,5)), (Bloco, (0,6)),
      (Bloco, (1,6)), (Bloco, (2,6)), (Bloco, (3,5)),
      (Bloco, (4,4)), (Bloco, (5,5)), (Bloco, (6,3)),
      (Bloco, (7,3)), (Bloco, (7,4)), (Bloco, (7,5)),
      (Bloco, (7,6))
     ]

m5 :: [(Peca,Coordenadas)] -- Falso
m5 = [(Porta, (0,1)), (Bloco, (0,2)), (Bloco, (1,0)),
      (Bloco, (2,1)), (Bloco, (3,2)), (Bloco, (4,2)),
      (Bloco, (5,2))
     ]

m6 :: [(Peca,Coordenadas)] -- Falso
m6 = [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),
      (Bloco, (4,2)), (Bloco, (4,3)), (Bloco, (5,1)),
      (Bloco, (5,2)), (Bloco, (5,3))
     ]

m7 :: [(Peca,Coordenadas)]
m7 = [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,4)), (Bloco, (0,5)),
      (Bloco, (0,6)), (Bloco, (0,7)), (Porta, (1,6)), (Bloco, (1,7)), (Bloco, (2,7)), (Bloco, (3,7)),
      (Bloco, (4,6)), (Bloco, (4,7)), (Bloco, (5,7)), (Bloco, (6,7)), (Bloco, (7,7)), (Bloco, (8,6)),
      (Bloco, (8,7)), (Bloco, (9,7)), (Caixa, (10,6)), (Bloco, (10,7)), (Bloco, (11,7)), (Bloco, (12,5)),
      (Bloco, (12,6)), (Bloco, (12,7)), (Bloco, (13,7)), (Caixa, (14,6)), (Bloco, (14,7)), (Bloco, (15,7)),
      (Bloco, (16,7)), (Bloco, (17,7)), (Bloco, (18,0)), (Bloco, (18,1)), (Bloco, (18,2)), (Bloco, (18,3)),
      (Bloco, (18,4)), (Bloco, (18,5)), (Bloco, (18,6)), (Bloco, (18,7))
     ]

m8 :: [(Peca,Coordenadas)]
m8 = [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),
      (Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(1,1)),(Porta,(1,8)),(Bloco,(1,9)),(Bloco,(1,10)),
      (Bloco,(1,11)),(Bloco,(1,12)),(Bloco,(2,1)),(Bloco,(2,12)),(Bloco,(3,1)),(Bloco,(3,9)),
      (Bloco,(3,10)),(Bloco,(3,11)),(Bloco,(3,12)),(Bloco,(4,1)),(Bloco,(4,9)),(Bloco,(5,0)),
      (Bloco,(5,8)),(Bloco,(5,9)),(Bloco,(6,0)),(Bloco,(6,5)),(Bloco,(6,6)),(Bloco,(6,7)),
      (Bloco,(6,8)),(Bloco,(7,0)),(Caixa,(7,7)),(Bloco,(7,8)),(Bloco,(8,1)),(Caixa,(8,7)),
      (Bloco,(8,8)),(Bloco,(9,1)),(Caixa,(9,7)),(Bloco,(9,8)),(Bloco,(10,1)),(Caixa,(10,7)),
      (Bloco,(10,8)),(Bloco,(11,1)),(Bloco,(11,8)),(Bloco,(11,9)),(Bloco,(12,0)),(Bloco,(12,9)),
      (Bloco,(12,10)),(Bloco,(12,11)),(Bloco,(12,12)),(Bloco,(12,13)),(Bloco,(13,0)),(Bloco,(13,13)),
      (Bloco,(14,0)),(Bloco,(14,9)),(Bloco,(14,10)),(Bloco,(14,11)),(Bloco,(14,12)),(Bloco,(14,13)),
      (Bloco,(15,0)),(Bloco,(15,10)),(Bloco,(15,11)),(Bloco,(15,12)),(Bloco,(16,0)),(Bloco,(16,12)),
      (Bloco,(17,0)),(Bloco,(17,12)),(Bloco,(18,0)),(Bloco,(18,12)),(Bloco,(19,0)),
      (Caixa,(19,11)),(Bloco,(19,12)),(Bloco,(20,0)),(Caixa,(20,10)),(Caixa,(20,11)), (Bloco,(20,12)),
      (Bloco,(21,0)),(Caixa,(21,9)),(Caixa,(21,10)),(Caixa,(21,11)),(Bloco,(21,12)), (Bloco,(22,1)),
      (Bloco,(22,2)),(Bloco,(22,3)),(Bloco,(22,4)),(Bloco,(22,5)),(Bloco,(22,6)), (Bloco,(22,7)),
      (Bloco,(22,8)),(Bloco,(22,9)),(Bloco,(22,10)),(Bloco,(22,11)),(Bloco,(22,12))
     ]

m9 :: [(Peca, Coordenadas)] -- Falso
m9 = [ (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,4)), (Bloco, (0,5)), (Bloco, (1,4)),
       (Bloco, (1,5)), (Bloco, (1,7)), (Bloco, (2,5)), (Porta, (2,7)), (Bloco, (3,5)),
       (Bloco, (2,7)), (Bloco, (3,7)), (Bloco, (4,7)), (Bloco, (5,6)), (Bloco, (6,5)),
       (Bloco, (3,7)), (Bloco, (4,5)), (Bloco, (4,4)), (Bloco, (5,2)), (Bloco, (6,2)),
       (Bloco, (6,3)), (Caixa, (6,4)), (Bloco, (7,4)), (Caixa, (8,4)), (Bloco, (9,3)), 
       (Caixa, (9,4)), (Bloco, (10,0)), (Bloco, (10,1)), (Bloco, (10,2)), (Bloco, (10,3)), 
       (Bloco, (10,4))
      ]


m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa
m2r = [[Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
       [Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio],
       [Bloco, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
       [Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Vazio, Bloco, Bloco, Vazio],
       [Bloco, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio],
       [Bloco, Vazio, Bloco, Vazio, Bloco, Caixa, Caixa, Vazio, Bloco, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
       [Bloco, Porta, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
       [Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
      ]

m3r :: Mapa
m3r = [[Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco, Vazio],
       [Bloco, Porta, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio],
       [Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio]
      ]

m7r :: Mapa
m7r = [[Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
      ]

m8r :: Mapa
m8r = [[Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio],
       [Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Caixa, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Porta, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
       [Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
       [Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
       [Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Caixa, Caixa, Caixa, Bloco],
       [Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
       [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
      ]
      


m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m1r (Jogador (6, 0) Este False)

m1e4 :: Jogo
m1e4 = Jogo m1r (Jogador (2, 3) Este False)

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (9, 6) Este False)

m2e2 :: Jogo
m2e2 = Jogo m2r (Jogador (7, 8) Oeste False) 

m3e1 :: Jogo
m3e1 = Jogo m3r (Jogador (8, 6) Oeste False)

m3e2 :: Jogo
m3e2 = Jogo m3r (Jogador (10, 5) Este False)

m7e1 :: Jogo
m7e1 = Jogo m7r (Jogador (16, 6) Oeste False)

m7e2 :: Jogo
m7e2 = Jogo m7r (Jogador (12, 4) Este False)

m8e1 :: Jogo
m8e1 = Jogo m8r (Jogador (12, 8) Oeste False)

m8e2 :: Jogo
m8e2 = Jogo m8r (Jogador (8, 6) Este False)







