{- |
Module      : Tarefa2_2021li1g081
Description : Construção/Desconstrução do mapa
Copyright   : Milena Figueira de Araújo Carreira <a95062@alunos.uminho.pt>;
            : Henrique Almeida Vaz <a95533@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g081 where

import LI12122

import Tarefa1_2021li1g081


-- | A função 'constroiMapa' é a função principal. Esta vai chamar todos os testes individuais e, dada uma lista de peças e coordenadas contrói um mapa.
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa l = constroiMapaAux l (replicate ty (replicate tx Vazio)) 
                       where tx = testeTamanhoX (testeTamanho l) + 1
                             ty = testeTamanhoY (testeTamanho l) + 1

-- | A função 'constroiMapaAux' chama a função 'setMapaAt' com os dados do objeto. 
constroiMapaAux :: [(Peca, Coordenadas)] -> Mapa -> Mapa
constroiMapaAux [] m = m 
constroiMapaAux ((p,(a,b)):xs) m = constroiMapaAux xs (setMapaAt m a b p) 

-- | A função 'setLinhaAt' altera a linha onde aparece o objeto.
setLinhaAt ::  [Peca] -> Int -> Peca -> [Peca]
setLinhaAt l i p = take i l ++ [p] ++ drop (i + 1) l

-- | A função 'setMapaAt' procura a lista (linha a linha, chamando a função 'setLinhaAt') dentro da "matriz".
setMapaAt :: Mapa -> Int -> Int -> Peca -> Mapa
setMapaAt l i j p = take j l ++ [setLinhaAt (l!!j) i p] ++ drop (j + 1) l

{- | A função 'desconstroiMapa' inicializa considerando a primeira linha como 0.
     Esta vai chamar os testes seguintes e, dando um certo mapa, vai desconstruí-lo devolvendo uma lista de peças e coordenadas.
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa m = desconstroiMapaAux m 0

-- | A função 'desconstroiMapaAux' percorre todas as linhas e chama a função 'desconstruirLinha' para desconstruir cada linha.
desconstroiMapaAux :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiMapaAux [] _ = []
desconstroiMapaAux (m:ms) i = desconstruirLinha m i 0 ++ desconstroiMapaAux ms (i+1)    

-- | A função 'desconstruirLinha' percorre a linha e converte a lista de peças em listas de peças, coordenadas.
desconstruirLinha :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
desconstruirLinha [] _ _ = []-- o i é o y
desconstruirLinha (p:ps) i j | p==Vazio=desconstruirLinha ps i (j+1)
                             |otherwise = [(p,(j,i))] ++ desconstruirLinha ps i (j+1)


-- haddock -h -o doc/html Tarefa2_2021li1g081.hs




