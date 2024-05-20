{- |
Module      : Tarefa4_2021li1g081
Description : Movimentação do personagem
Copyright   : Milena Figueira de Araújo Carreira <a95062@alunos.uminho.pt>;
            : Henrique Almeida Vaz <a95533@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g081 where

import LI12122

import Tarefa1_2021li1g081

import Tarefa2_2021li1g081

import Tarefa3_2021li1g081

{- | A função 'moveJogador' aplica as funções abaixo definidas de modo a que o jogador se movimente. Esta testa se o movimento é possível e se 
existem quedas do jogador e/ou de caixas.
-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo mov | mov == AndarEsquerda = andarEsquerda jogo --  a acao de andar para a esquerda
                     | mov == AndarDireita = andarDireita jogo --  a acao de andar para a direita
                     | mov == Trepar = trepar jogo --  a acao de trepar uma caixa ou bloco
                     | mov == InterageCaixa = pegaLarga jogo --  a acao de pegar ou largar a caixa
                     | otherwise = jogo


{- | A função 'andarEsquerda' verifica se é possível o jogador se movimentar para a esquerda através da função 'possivelEsquerda'. Depois, caso seja
possível, o jogador é movido através da função 'andaEsquerda'.
-}
andarEsquerda :: Jogo -> Jogo
andarEsquerda (Jogo m j) | possivelEsquerda m j = andaEsquerda(Jogo m j)
                         | otherwise = (Jogo m (virarJogador j Oeste)) -- nao e' possivel andar esquerda, por isso devolve o mesmo jogo com alteracao da direcao


-- | A função 'virarJogador' vira o jogador para a direção Direção (Este ou Oeste).
virarJogador :: Jogador -> Direcao -> Jogador
virarJogador (Jogador coord d c) dir = Jogador coord dir c

-- | A função 'possivelEsquerda' verifica se é possível o jogador se movimentar para a esquerda, incluindo o caso deste ter uma caixa.
possivelEsquerda :: Mapa -> Jogador -> Bool
possivelEsquerda m (Jogador (i,j) d c) | i==0 = False -- O jogador está encostado à esquerda.
                                       | c && vazioMapa m (i-1,j) && vazioMapa m (i-1,j-1) = True-- Verifica se o jogador e a caixa podem andar para a esquerda, sendo que, ambos tem de conseguir passar.
                                       | c && portaMapa m (i-1,j) && vazioMapa m (i-1,j-1) = True
                                       | not(c) && vazioMapa m (i-1,j) = True-- No caso de não carregar uma caixa então basta ter espaço para o jogador passar.
                                       | not(c) && portaMapa m (i-1,j) = True
                                       | otherwise = False
-- | A função 'vazioMapa' verifica, dado o mapa e as coordenadas pretendidas se o local é vazio ou não.
vazioMapa :: Mapa -> Coordenadas -> Bool
vazioMapa m (i,j) | i>length(head m)-1 || j>length(m)-1 = False
                  | ((m!!j)!!i)==Vazio = True
                  | otherwise = False

-- | A função 'caixaMapa' verifica, dado o mapa e as coordenadas pretendidas se o local é uma caixa ou não.
caixaMapa :: Mapa -> Coordenadas -> Bool
caixaMapa m (i,j) | i>length(head m)-1 || j>length(m)-1 = False
                  | ((m!!j)!!i)==Caixa = True--encontra a posiçao de m em j e de seguida essa posiçao em i e testa se é caixa
                  | otherwise = False

-- | A função 'portaMapa' verifica, dado o mapa e as coordenadas pretendidas se o local é uma porta ou não.
portaMapa :: Mapa -> Coordenadas -> Bool
portaMapa m (i,j) | i>length(head m)-1 || j>length(m)-1 = False
                  | ((m!!j)!!i)==Porta = True--encontra a posiçao de m em j e de seguida essa posiçao em i e testa se é porta
                  | otherwise = False
-- | A função 'blocoMapa' verifica, dado o mapa e as coordenadas pretendidas se o local é um bloco ou não.
blocoMapa :: Mapa -> Coordenadas -> Bool
blocoMapa m (i,j) | i>length(head m)-1 || j>length(m)-1 = False
                  | ((m!!j)!!i)==Bloco = True -- encontra a posiçao de m em j e de seguida essa posiçao em i e testa se é bloco
                  | otherwise = False

{- | A função 'andaEsquerda', dado um determinado jogo, ou seja, um mapa e as coordenadas do jogador, move o jogador para a esquerda.
-}
andaEsquerda :: Jogo -> Jogo -- atualizacao do jogador nao altera mapa, exceto se levar caixa
andaEsquerda (Jogo m (Jogador (i,j) d c)) = Jogo m (Jogador (a,b) Oeste c) -- move o jogador para a posicao (a,b)
                                            where (a,b) = caiJogador m (i-1,j)

-- | A função 'colocaCaixa' adiciona a caixa "virtual" que o jogador carrega no mapa
colocaCaixa :: Mapa -> Coordenadas -> Mapa
colocaCaixa m (i,j) | caixaMapa m (i,j) = m -- já ha' caixa em i j-1
                    | otherwise = setMapaAt m a b Caixa
                        where (a,b) = caiCaixa m (i,j)
-- | A função 'removeCaixa' remove a caixa do mapa quando o jogador pega nela
removeCaixa :: Mapa -> Coordenadas -> Mapa
removeCaixa m (i,j) | not(caixaMapa m (i,j)) = m -- nao ha' caixa em i j-1
                    | otherwise = setMapaAt m i j Vazio
-- | A função 'caiJogador' realiza, no caso de existir, uma queda do jogador após um movimento. Recorrendo, assim às funções 'vazioMapa' e 'portaMapa'
caiJogador :: Mapa -> (Int, Int) -> (Int, Int)
caiJogador m (a,b) | (vazioMapa m (a,b+1)) || (portaMapa m (a,b+1)) = caiJogador m (a,b+1)--  Caso exista um vazio em baixo ou uma porta, a função é chamada novamente com as coordenadas do bloco abaixo.
                   | otherwise = (a,b) --No caso de ter um bloco ou caixa em baixo, o jogador fica em cima do bloco/caixa.
-- | A função 'caiCaixa' faz com que quando o jogador largue a caixa ela vá para o bloco mais abaixo possivel
caiCaixa :: Mapa -> (Int, Int) -> (Int, Int)
caiCaixa m (a,b) | (vazioMapa m (a,b+1)) = caiCaixa m (a,b+1)
                 | otherwise = (a,b)

{- | A função 'andarDireita' verifica se é possível o jogador se movimentar para a direita através da função 'possivelDireita'. Depois, caso seja
possível, o jogador é movido através da função 'andaDireita'. Recorre ainda à função 'virarJogador'.
-}
andarDireita :: Jogo -> Jogo
andarDireita (Jogo m j) | possivelDireita m j = andaDireita(Jogo m j)
                        | otherwise = (Jogo m (virarJogador j Este))--  Não é possível andar para a direita, por isso devolve o mesmo jogo com alteração da direção.

-- | A função 'possivelDireita' verifica se é possível o jogador se movimentar para a direita, incluindo o caso deste ter uma caixa.
possivelDireita :: Mapa -> Jogador -> Bool
possivelDireita m (Jogador (i,j) d c) | i==tx-1 = False --  O jogador está encostado à direita.
                                      | c && vazioMapa m (i+1,j) && vazioMapa m (i+1,j-1) = True--  Verifica se o jogador e a caixa podem andar para a direita, sendo que, ambos tem de conseguir passar.
                                      | c && portaMapa m (i+1,j) && vazioMapa m (i+1,j-1) = True
                                      | not(c) && vazioMapa m (i+1,j)  = True-- No caso de não carregar uma caixa então basta ter espaço para o jogador passar.
                                      | not(c) && portaMapa m (i+1,j) = True
                                      | otherwise = False
                                        where tx=length(head m)

{- | A função 'andaDireita', dado um determinado jogo, ou seja, um mapa e as coordenadas do jogador, move o jogador para a direita.
-}
andaDireita :: Jogo -> Jogo
andaDireita (Jogo m (Jogador (i,j) d c)) = Jogo m (Jogador (a,b) Este c)
                                            where (a,b) = caiJogador m (i+1,j)

-- | A função 'trepar' permite ao jogador trepar um bloco, são efetuados testes para o jogador conseguir trepar tanto com caixa como sem ela.
trepar :: Jogo -> Jogo
trepar (Jogo m (Jogador (i,j) d c)) | not(c) && d==Este &&
                                       (caixaMapa m (i+1,j) || blocoMapa m (i+1,j)) && vazioMapa m (i+1,j-1) || portaMapa m (i+1,j-1)
                                         =  Jogo m (Jogador (i+1,j-1) d c) -- so' move jogador
                                    | c && d==Este &&
                                       (caixaMapa m (i+1,j) || blocoMapa m (i+1,j)) && (vazioMapa m (i+1,j-1) || portaMapa m (i+1,j-1)) && vazioMapa m (i+1,j-2)
                                         = Jogo m (Jogador (i+1,j-1) d c)
                                    | not(c) && d==Oeste &&
                                       (caixaMapa m (i-1,j) || blocoMapa m (i-1,j)) && vazioMapa m (i-1,j-1) || portaMapa m (i-1,j-1)
                                         = Jogo m (Jogador (i-1,j-1) d c)
                                    | c && d==Oeste &&
                                       (caixaMapa m (i-1,j) || blocoMapa m (i-1,j)) && (vazioMapa m (i-1,j-1) || portaMapa m (i-1,j-1)) && vazioMapa m (i-1,j-2)
                                         = Jogo m (Jogador (i-1,j-1) d c)
                                    | otherwise = (Jogo m (Jogador (i,j) d c)) -- nao e' possivel trepar
-- | A função 'pegaLarga' é a função que permite ao jogador pegar ou largar a caixa, fazendo todos os testes necessários
pegaLarga :: Jogo -> Jogo
pegaLarga (Jogo m (Jogador (i,j) d c)) | c && d==Oeste && vazioMapa m (i-1,j) && vazioMapa m (i-1,j-1)
                                         = Jogo (colocaCaixa m (i-1,j)) (Jogador (i,j) d False)-- drop da caixa
                                       | c && d==Oeste && (vazioMapa m (i-1,j) || caixaMapa m (i-1,j) || blocoMapa m (i-1,j)) && vazioMapa m (i-1,j-1) 
                                         = Jogo (colocaCaixa m (i-1,j-1)) (Jogador (i,j) d False)   
                                       | c && d==Este && (vazioMapa m (i+1,j) || caixaMapa m (i+1,j) || blocoMapa m (i+1,j)) && vazioMapa m (i+1,j-1) 
                                         = Jogo (colocaCaixa m (i+1,j-1)) (Jogador (i,j) d False)  
                                       | c && d==Este && vazioMapa m (i+1,j) && vazioMapa m (i+1,j-1) 
                                         = Jogo (colocaCaixa m (i+1,j)) (Jogador (i,j) d False) -- drop da caixa
                                       | not(c) && d==Oeste && caixaMapa m (i-1,j) && vazioMapa m (i,j-1) && vazioMapa m (i-1,j-1) -- a caixa tem de ter vazio em cima
                                         = Jogo (removeCaixa m (i-1,j)) (Jogador (i,j) d True) -- pega da caixa
                                       | not(c) && d==Este && caixaMapa m (i+1,j) && vazioMapa m (i,j-1) && vazioMapa m (i+1,j-1) -- a caixa tem de ter vazio em cima
                                         = Jogo (removeCaixa m (i+1,j)) (Jogador (i,j) d True) -- pega da caixa
                                       | otherwise = (Jogo m (Jogador (i,j) d c))

-- | A função 'correrMovimentos' corre os movimentos do jogo
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j 
correrMovimentos j (m:ms) = correrMovimentos (moveJogador j m) ms

