{- |
Module      : Tarefa1_2021li1g081
Description : Validação de um potencial mapa
Copyright   : Milena Figueira de Araújo Carreira <a95062@alunos.uminho.pt>;
            : Henrique Almeida Vaz <a95533@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g081 where

import LI12122


--TAREFA_1
-- | A função 'validaPotencialMapa' é a função principal, ela é que vai chamar todos os testes individuais e confirmar que todos retornam True.

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool 
validaPotencialMapa [] = False
validaPotencialMapa l = validaPosicaoAux (validaPosicao l) && validaPortaAux (validaPorta l) && validaCaixa l l && validaVazio l tx ty && caminhoChao l tx ty tytx (-1,-1) (0, ty0)
                                           where tx = testeTamanhoX (testeTamanho l)
                                                 ty = testeTamanhoY (testeTamanho l)
                                                 ty0 = testeTamanhoMaxY l 0 -- descobrir o maior y em x=0
                                                 tytx = testeTamanhoMaxY l tx -- descobrir o maior y em x max
--0  
-- | A função 'testeTamanho' separa as coordenadas da lista inicial do seu respetivo nome.

testeTamanho :: [(Peca,Coordenadas)] -> [Coordenadas]
testeTamanho [] = []
testeTamanho ((p,c):xs) = c:testeTamanho xs

-- | A função 'testeTamanhoX' dá-nos o maior valor de x existente na lista das coordenadas.
testeTamanhoX :: [Coordenadas] -> Int
testeTamanhoX [] = 0
testeTamanhoX ((a,b):xs) = max a (testeTamanhoX xs)

-- | A função 'testeTamanhoY' dá-nos o maior valor de x existente na lista de coordenadas.
testeTamanhoY :: [Coordenadas] -> Int
testeTamanhoY [] = 0
testeTamanhoY ((a,b):xs) = max b (testeTamanhoY xs)

-- | A função 'testeTamanhoMaxY' descobre o maior valor de y quando o x=0 e quando x é máximo
testeTamanhoMaxY ::  [(Peca,Coordenadas)] -> Int -> Int
testeTamanhoMaxY [] _ = -1
testeTamanhoMaxY ((p,(a,b)):xs) t = if a==t && p == Bloco then max b (testeTamanhoMaxY xs t) else testeTamanhoMaxY xs t

--1
-- | A função 'validaPosicao' separa as coordenadas da lista inicial do seu respetivo nome.

validaPosicao :: [(Peca,Coordenadas)] -> [Coordenadas]
validaPosicao [] = []
validaPosicao ((p,c):xs) = c:validaPosicao xs


{- | A função 'validaPosicaoAux' pega em todas as coordenadas, uma de cada vez e confirma se elas não existem no restante da lista,
 caso existam então temos duas peças no mesmo sítio e devolve False. -}
validaPosicaoAux :: [Coordenadas] -> Bool 
validaPosicaoAux [] = True  
validaPosicaoAux ((x,y):xs) | x<0 || y<0 = False
                            | (x,y) `elem` xs = False
                            | otherwise = validaPosicaoAux xs

--2
-- | A função 'validaPorta' separa da lista inicial apenas os elementos com nome "Porta"
validaPorta :: [(Peca,Coordenadas)] -> [Peca]
validaPorta [] = []
validaPorta ((p,c):xs) = if p==Porta then p:validaPorta xs else validaPorta xs 

{- | A função 'validaPortaAux' verifica se a lista das portas tem mais do que um elemento, caso tenha então retorna False,
 pois temos mais que uma porta no mapa.-} 
validaPortaAux :: [Peca] -> Bool
validaPortaAux [] = False
validaPortaAux (x:xs) |x `elem` xs = False
                      |otherwise = True

--3
{- | A função 'validaCaixa' recebe a lista inicial duas vezes, 
para os elementos "Caixa" é chamada a função auxiliar com a lista e as coordenadas onde terá de ter um bloco ou caixa.-}
validaCaixa :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Bool 
validaCaixa _ [] = True
validaCaixa l ((p,(x,y)):xs) | p == Caixa = validaCaixaBlocoAux l x (y+1) && validaCaixa l xs
                             | otherwise = validaCaixa l xs
-- | A função 'validaCaixaBlocoAux' apenas confirma se em baixo da caixa tem outra caixa ou um bloco, caso não tenha retorna False.
validaCaixaBlocoAux :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
validaCaixaBlocoAux [] _ _ = False
validaCaixaBlocoAux ((p,(a,b)):xs) x y | (p==Caixa || p==Bloco) && (a==x && b==y) = True
                                       | otherwise = validaCaixaBlocoAux xs x y 

--4
-- | A função 'validaVazio' gera todas as coordenadas possíveis do mapa, e chama a função 'validaVazlioAux2' com a lista inicial e a lista de posiveis coordenadas.
validaVazio :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
validaVazio l tx ty = validaVazioAux2 l [(x,y) | x <- [0..tx], y <- [0..ty]] --pega em todos os elementos possiveis, um de cada vez e verifica se existem na lista inicial, basta que 1 nao exista para dar true e termos um elemento vazio

 {-A função 'validaVazioAux2' vai chamar a função 'validaVazioAux' com a lista inicial e com uma das possíveis coordenadas.
  Caso esta função receba um False, -}

-- | A função 'validaVazioAux2' chama a lista inicial e a função 'validaVazioAux' e, se receber o valor True, então é verdade que a peça é um vazio.
validaVazioAux2 :: [(Peca,Coordenadas)] -> [(Int,Int)]-> Bool
validaVazioAux2 _ [] = False
validaVazioAux2 l ((x,y):xs) | validaVazioAux l (x,y) == True = True
                             | otherwise = validaVazioAux2 l xs
-- |A função 'validaVazioAux' vai testar se a dada coordenada pertence à lista inicial.
validaVazioAux :: [(Peca,Coordenadas)] -> (Int,Int) -> Bool 
validaVazioAux [] _  = True
validaVazioAux ((p,(a,b)):xs) (x,y) | (a,b) == (x,y) = False
                                    | otherwise = validaVazioAux xs (x,y)


--5

{- | A função 'caminhoChao' chama a função 'existeBloco' para verificar se há um caminho do primeiro bloco com maior y até ao ultimo x com maior y, ou seja,
se há um caminho de blocos ligados para que que o jogador consiga passar. -}
caminhoChao :: [(Peca,Coordenadas)] -> Int -> Int -> Int -> (Int,Int) -> (Int, Int) -> Bool
caminhoChao l tx ty tytx (c,d) (a,b) | tytx<0 && b<0 = False
                                     | a==tx && b == tytx = True
                                     | (a==c && (b-1)==d || not (existeBloco l ty a (b-1))) &&
                                        ( (a+1)==c && (b+1)==d || not (existeBloco l ty (a+1) (b+1))) &&
                                         ( (a+1)==c && (b-1)==d || not (existeBloco l ty (a+1) (b-1))) &&
                                          ( (a+1)==c && b==d || not (existeBloco l ty (a+1) b)) &&
                                           ( a==c && (b+1)==d || not (existeBloco l ty a (b+1))) == True = False
                                     | (c/=a || d/=(b+1)) && existeBloco l ty a (b+1) = caminhoChao l tx ty tytx (a,b) (a,b+1)
                                     | (c/=(a+1) || d/=b) && existeBloco l ty (a+1) b = caminhoChao l tx ty tytx (a,b) (a+1,b)  
                                     | (c/=a || d/=(b-1)) && existeBloco l ty a (b-1) = caminhoChao l tx ty tytx (a,b) (a,b-1) 
                                     | (c/=(a+1) || d/=(b+1)) && existeBloco l ty (a+1) (b+1) = caminhoChao l tx ty tytx (a,b) (a+1,b+1) 
                                     | (c/=(a+1) || d/=(b-1)) && existeBloco l ty (a+1) (b-1) = caminhoChao l tx ty tytx (a,b) (a+1,b-1) 
                                     | otherwise = False
                                      
-- | A função 'existeBloco' verifica se existe algum bloco na coordenada dada.
existeBloco :: [(Peca,Coordenadas)] ->Int -> Int -> Int -> Bool                                       
existeBloco [] _ _ _  = False
existeBloco ((p,(a,b)):xs) ty x y | y>ty = False
                                  | p==Bloco && a==x && b==y = True
                                  | otherwise = existeBloco xs ty x y

-- haddock -h -o doc/html Tarefa1_2021li1g081.hs
