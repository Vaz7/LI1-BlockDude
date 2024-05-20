{- |
Module      : Tarefa3_2021li1g081
Description : Representação textual do jogo
Copyright   : Milena Figueira de Araújo Carreira <a95062@alunos.uminho.pt>;
            : Henrique Almeida Vaz <a95533@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g081 where

import LI12122


import Tarefa1_2021li1g081

import Tarefa2_2021li1g081

import Data.List


-- ver se podemos alterar o ficheiro LI2122.hs
instance Show Jogo where
   show (Jogo m j) = showJogador j (showMapa m)
      where showMapa m = (map (map show) m) -- para tranformar lista de peças numa lista de strings
            showJogador j m = (intercalate "\n" (map (intercalate "") (insereJogador j m))) --primeiro intercala cada um dos elementos de uma linha para formar linha sem espaço e depois intercala com mudança de linha


-- | A função 'insereJogador' insere o jogador 
insereJogador :: Jogador -> [[String]] -> [[String]]
insereJogador (Jogador (x,y) d c) m | c = setCaixaAt (setStringAt m x y d) x (y-1)
                                    | otherwise = setStringAt m x y d


-- | A função 'setLinhaStringAt' seleciona a coluna onde vamos introduzir o jogador (o seu x)
setLinhaStringAt ::  [String] -> Int -> Direcao -> [String]
setLinhaStringAt l i d = take i l ++ [show d] ++ drop (i + 1) l -- vai inserir o show do d na lista de strings

-- | A função 'setStringAt' procura a posição a linha onde está o jogador 
setStringAt :: [[String]] -> Int -> Int -> Direcao -> [[String]]
setStringAt m i j d = take j m ++ [setLinhaStringAt (m!!j) i d] ++ drop (j + 1) m

-- | A função 'setCaixaAt' procura a posição a linha onde está a caixa transportada
setCaixaAt :: [[String]] -> Int -> Int -> [[String]]
setCaixaAt m i j = take j m ++ [setCaixaLinhaAt (m!!j) i] ++ drop (j + 1) m

-- | A função 'setCaixaLinhaAt' seleciona a coluna onde vamos introduzir a caixa (o seu x)
setCaixaLinhaAt ::  [String] -> Int -> [String]
setCaixaLinhaAt l i = take i l ++ [show Caixa] ++ drop (i + 1) l -- vai inserir o show do d na lista de strings
