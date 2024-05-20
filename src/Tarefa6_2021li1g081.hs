module Tarefa6_2021li1g081 where

import LI12122

import Tarefa1_2021li1g081

import Tarefa2_2021li1g081

import Tarefa3_2021li1g081

import Tarefa4_2021li1g081

-- import Tarefa5_2021li1g081

import Data.List

-- | A funcao 'resolveJogo' é a funcao principal desta tarefa, é a ela que passamos o mapa e o limite de movimentos para a resolucao do nivel
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 (Jogo m (Jogador (i,j) d c)) | portaMapa m (i,j) = Just [] -- resolvido
                                           | otherwise = Nothing
resolveJogo n (Jogo m (Jogador (i,j) d c)) | n<0 = Nothing
                                           | otherwise = tentaRec n [] (Jogo m (Jogador (i,j) d c)) (-1,-1) [] -- começamos com as coordenadas ultima caixa movimentada a (-1,-1), para nao calhar de ser uma caixa valida.

-- | A funcao 'tentaRec' tenta recursivamente resolver o nivel
tentaRec :: Int -> [Movimento] -> Jogo -> (Int, Int) -> [Jogo] -> Maybe [Movimento]
tentaRec n mov (Jogo m (Jogador (i,j) d c)) (x,y) jogos | length (mov) > n = Nothing -- se excedermos os maximo de movimentos devolve nothing
                                                        | portaMapa m (i,j) = Just mov -- se ja estivermos na porta devolve os movimentos
                                                        | repetidoJogo (Jogo m (Jogador (i,j) d c)) jogos = Nothing -- se o jogo atual for repetido devolve nothing
                                                        | possivelTerminar (Jogo m (Jogador (i,j) d c)) (px,py) = if (length(mov)+length(movTerminar)) <= n then Just (mov ++ movTerminar) -- se for possivel terminar da posicao onde estamos entao devolvemos os movimentos atuais mais os movimentos até terminar
                                                                                                                                                            else Nothing
                                                        | otherwise = if tCaixa==Nothing then Nothing -- caso todas as outras condicoes falhem entao vamos tentar fazer combinacoes com todas as caixas
                                                                                         else tCaixa
                                                          where (px,py) = (head (procuraPortas m))
                                                                movTerminar = (movimentosTerminar (Jogo m (Jogador (i,j) d c)) (px,py))
                                                                tCaixa=tentaCaixa n mov (procuraCaixas (Jogo m (Jogador (i,j) d c)) (x,y)) (Jogo m (Jogador (i,j) d c)) (jogos++[(Jogo m (Jogador (i,j) d c))])

-- | A funcao 'repetidoJogo' verifica se ja visitamos o jogo atual, caso ja o tenhamos feito entao iremos tentar de novo
repetidoJogo :: Jogo -> [Jogo] -> Bool
repetidoJogo _ [] = False
repetidoJogo (Jogo m jogador) (Jogo mx jogadorx:jogos) | m==mx = True
                                                       | otherwise = repetidoJogo (Jogo m jogador) jogos
-- | A funcao 'possivelTerminar' verifica se conseguimos terminar o nivel sem movimentar mais caixas
possivelTerminar :: Jogo -> (Int, Int) -> Bool
possivelTerminar (Jogo m (Jogador (i,j) d c)) (px,py) | (px==i && py==j)  || -- porta na posicao do boneco
                                                        (px==i-1 && py==j-1) || (px==i-1 && py==j) || -- porta à esquerda
                                                        (px==i+1 && py==j-1) || (px==i+1 && py==j) || -- porta à direita
                                                        ((possivelEsquerda m (Jogador (i,j) d c)) && px==lx && py==ly) ||
                                                        ((possivelDireita m (Jogador (i,j) d c))  && px==rx && py==ry) = True -- verificamos se a porta esta abaixo do jogador ao alcance de uma queda
                                                      | px<=i && possivelEsquerda m (Jogador (i,j) d c) && possivelTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) (px,py) = True --se a caixa esta á esquerda e for possivel terminar para a esquerda entao é true
                                                      | px>=i && possivelDireita m (Jogador (i,j) d c) && possivelTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita) (px,py) = True
                                                      | possivelTrepar (Jogo m (Jogador (i,j) d c)) && possivelTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) Trepar) (px,py) = True
                                                      | otherwise = False
                                                          where (lx,ly) = caiJogador m (i-1,j)
                                                                (rx,ry) = caiJogador m (i+1,j)

-- | A funcao 'movimentosTerminar' é muito semelhante à anterior, mas em vez de verificar se é possivel terminar calcula os movimentos (nesta fase ja temos a certeza que é possivel terminar) 
movimentosTerminar :: Jogo -> (Int, Int) -> ([Movimento])
movimentosTerminar (Jogo m (Jogador (i,j) d c)) (px,py) | (px==i && py==j) = [] -- porta na posicao do boneco, terminado
                                                        | px<i && possivelEsquerda m (Jogador (i,j) d c) = [AndarEsquerda] ++ movimentosTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) (px,py)
                                                        | px<i && d==Este = [AndarEsquerda] ++ movimentosTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) (px,py)
                                                        | px>i && possivelDireita m (Jogador (i,j) d c) = [AndarDireita] ++ movimentosTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita) (px,py)
                                                        | px>i && d==Oeste = [AndarDireita] ++ movimentosTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita) (px,py)
                                                        | px<i && d==Oeste && possivelTrepar (Jogo m (Jogador (i,j) d c)) = [Trepar] ++ movimentosTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) Trepar) (px,py)
                                                        | px>i && d==Este && possivelTrepar (Jogo m (Jogador (i,j) d c)) = [Trepar] ++ movimentosTerminar (moveJogador (Jogo m (Jogador (i,j) d c)) Trepar) (px,py)
                                                        | otherwise = [] -- nao devia acontecer, porque verificamos que ha caminho

-- | tenta caixa recebe uma lista de todas as caixas e vai pegar numa caixa de cada vez e chama a tentalocalcaixa para tentar pousar cada caixa em cada local possivel
tentaCaixa :: Int -> [Movimento] -> [(Int,Int)] -> Jogo -> [Jogo] -> Maybe [Movimento]
tentaCaixa _ _ [] _ jogos = Nothing -- mais nenhuma caixa para movimentar
tentaCaixa n mov (x:xs) (Jogo m (Jogador (i,j) d c)) jogos | movPegaCaixa /= [] && locaisCaixas /= [] && tentaLCaixas /= Nothing = tentaLCaixas -- caso haja movimentos para pegar na caixa e haja locais para colocar a caixa e tentar colocar a caixa nao devolva nothing, entao devolvemos o seu resultado
                                                           | otherwise = (tentaCaixa n mov xs (Jogo m (Jogador (i,j) d c)) jogos) -- caso contrario tenta com a caixa seguinte
                                                             where (movPegaCaixa, movCaixaJogo) = (pegaCaixa (Jogo m (Jogador (i,j) d c)) x)
                                                                   locaisCaixas = (possivelPousarCaixa movCaixaJogo)-- onde é possivel colocar a caixa no jogo resultante de pegar na caixa
                                                                   tentaLCaixas = tentaLocalCaixa n (mov++movPegaCaixa) locaisCaixas movCaixaJogo jogos -- tentar colocar a caixa nos locais possiveis para pousar uma caixa

-- | a funcao 'possivelPousarCaixa' calcula os locais onde e possivel pousar a caixa depois de a agarrar
possivelPousarCaixa :: Jogo -> [(Int,Int)]
possivelPousarCaixa (Jogo m (Jogador (i,j) d c)) | d==Oeste = nub $ sort (possivelPousarCaixaEsquerda m (i-1,j) (i-1,j)) ++ (possivelPousarCaixaDireita m (i-1,j) (i-1,j)) -- chamamos as funções possivelpousarcaixaesquerda e possivelpousarcaixadireita com as coordenadas antes e depois do jogador, nao a vamos pousar onde estamos
                                                 | d==Este = nub $ sort (possivelPousarCaixaEsquerda m (i+1,j) (i+1,j)) ++ (possivelPousarCaixaDireita m (i+1,j) (i+1,j)) -- passamos duas vezes a mesma coordenada para que nao se tente pousar a caixa no mesmo sitio onde pegamos nela

-- | A funcao 'tentaLocalCaixa' tenta pousar cada caixa em todos os locais possiveis chamando recursivamente a funcao tentaRec
tentaLocalCaixa :: Int -> [Movimento] -> [(Int,Int)] -> Jogo -> [Jogo] -> Maybe [Movimento]
tentaLocalCaixa _ _ [] _ jogos = Nothing -- mais nenhum local para tentar colocar a caixa
tentaLocalCaixa n mov (x:xs) (Jogo m (Jogador (i,j) d c)) jogos | movCaixaLarga/=[] && solMov /= Nothing = solMov -- se conseguirmos pousar a caixa e tivermos solucao apos pousar entao devolvemos o solmov
                                                                | otherwise = (tentaLocalCaixa n mov xs (Jogo m (Jogador (i,j) d c)) jogos) -- no caso de o solmov nao ter soluçao tentamos esta função com uma nova posicao para a caixa
                                                                  where (movCaixaLarga,movCaixaLargaJogo) = (largaCaixa (Jogo m (Jogador (i,j) d c)) x) -- aqui tentamos largar a caixa, caso nao seja possivel movcaixalarga sao os movimentos para largar a caixa movcaixalargajogo é o jogo resultante
                                                                        solMov = (tentaRec n (mov++movCaixaLarga) movCaixaLargaJogo x jogos) -- tentamos recursivamente o resto do percurso, aqui passamos a coordenada onde fica a caixa, para não ser considerada na proxima tentativa

-- | A funcao 'procuraCaixas' recebe as coordenadas da ultima caixa que pegamos e procura todas as outras caixas possiveis de pegar e devolve as coordenadas
procuraCaixas :: Jogo -> (Int,Int) -> [(Int,Int)]
procuraCaixas (Jogo m jogador) (x,y) = nub $ sort ([(i,j) | i<-[0..(length (head m))-1], j<-[0..(length m)-1],--nao verificamos aqui se tem vazio em cima, verificamos depois na funçao pegaCaixa
                                                caixaMapa m (i,j) && (((i-1>0) && vazioMapa m (i-1,j)) || (i<(length (head m))-1 && vazioMapa m (i+1,j))) && (i,j)/=(x,y)])

-- | a funcao 'pegaCaixa' devolve os Movimentos para pegar na caixa. Devolve os movimentos e o jogo (para evitar ser recalculado)
pegaCaixa :: Jogo -> (Int, Int) -> ([Movimento], Jogo)
pegaCaixa (Jogo m (Jogador (i,j) d c)) (x,y) | d==Oeste && x==i-1 && caixaMapa m (i-1,j) && vazioMapa m (i-1,j-1) = ([InterageCaixa], moveJogador (Jogo m (Jogador (i,j) d c)) InterageCaixa)-- neste caso a caixa está antes do jogador (á esquerda)
                                             | d==Oeste && x==i-1 && vazioMapa m (i-1,j) && caixaMapa m (cEsqi,cEsqj+1) && movEsqEsq/=[] = ([AndarEsquerda,AndarEsquerda]++movEsqEsq, jogoEsqEsq)-- neste caso a caixa está á esquerda mais abaixo do jogador e ele cai em cima dela, logo tem de andar 2x a esquerda para conseguir pegar nela (neste caso a cai jogador da a coordenada acima da caixa, loho temos de adicionar 1 ao j para ter a coordenada da caixa)
                                             | d==Este && x==i+1 && caixaMapa m (i+1,j) && vazioMapa m (i+1,j-1) = ([InterageCaixa], moveJogador (Jogo m (Jogador (i,j) d c)) InterageCaixa)-- pega na caixa caso ela esteja na direita do jogador
                                             | d==Este && x==i+1 && vazioMapa m (i+1,j) && caixaMapa m (cDiri,cDirj+1) && movDirDir/=[] = ([AndarDireita,AndarDireita]++movDirDir, jogoDirDir)-- neste caso a caixa está á direita mais abaixo do jogador e ele cai em cima dela, logo tem de andar 2x a direita para conseguir pegar nela 
                                             | ((d==Este && x<i) || (x<i && possivelEsquerda m (Jogador (i,j) d c))) && movEsquerda/=[] = ([AndarEsquerda]++movEsquerda,jogoEsquerda)  -- neste caso o boneco esta virado para este e a caixa esta a oeste, o jogador tem de andar sempre para a esquerda  
                                             | ((d==Oeste && x>i) || (x>i && possivelDireita m  (Jogador (i,j) d c))) && movDireita/=[] = ([AndarDireita]++movDireita,jogoDireita) -- neste caso o boneco esta virado para oeste e a caixa esta a este, o jogador tem de andar sempre para a direita
                                             | x<i && possivelTrepar (Jogo m (Jogador (i,j) d c)) && movTrepar/=[] = ([Trepar]++movTrepar,jogoTrepar) -- a possivel trepar devolve se é possivel trepar para o lado em que o jogador está virado
                                             | x>i && possivelTrepar (Jogo m (Jogador (i,j) d c)) && movTrepar/=[] = ([Trepar]++movTrepar,jogoTrepar) 
                                             | x==i = contornarCaixa  (Jogo m (Jogador (i,j) d c)) (x,y) -- no caso de a caixa estar na mesma coluna do jogador entao contornamos a caixa
                                             | otherwise = ([], Jogo m (Jogador (i,j) d c)) -- nao e possivel
                                                   where (cEsqi,cEsqj)=caiJogador m (i-1,j) -- usamos a caijogador sempre que queremos determinar a altura de uma queda
                                                         (cDiri,cDirj)=caiJogador m (i+1,j)
                                                         (movEsquerda,jogoEsquerda)= pegaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) (x,y)
                                                         (movDireita,jogoDireita)=   pegaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita)  (x,y)
                                                         (movEsqEsq,jogoEsqEsq)= pegaCaixa (moveJogador (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) AndarEsquerda) (x,y)
                                                         (movDirDir,jogoDirDir)= pegaCaixa (moveJogador (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita)  AndarDireita)  (x,y)
                                                         (movTrepar,jogoTrepar)= pegaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) Trepar) (x,y)

-- | A funcao 'contornarCaixa' caixa so é chamada no caso de o jogador esta em cima de uma caixa que estamos a tentar pegar
contornarCaixa :: Jogo -> (Int,Int) -> ([Movimento], Jogo)
contornarCaixa (Jogo m (Jogador (i,j) d c)) (x,y) | possivelDireita m  (Jogador (i,j) d c) && vazioMapa m (i+1,j+1) && caiJogadorDirj <= j+1 && movDireita/=[] = ([AndarDireita]++movDireita,jogoDireita) -- se for possivel andar direita e estiver vazio do lado direito da caixa e houver continuaçao a direita o jogaodor anda a direita, o jogador nao pode cair mais do que um bloco, pois ai nao consege pegar na caixa
                                                  | possivelEsquerda m  (Jogador (i,j) d c) && vazioMapa m (i-1,j+1) && caiJogadorEsqj <= j+1 && movEsquerda/=[] = ([AndarEsquerda]++movEsquerda,jogoEsquerda) -- se for possivel andar esquerda e estiver vazio do lado esquerdo da caixa e houver continuaçao a esquerda o jogaodor anda a esquerda, o jogador nao pode cair mais do que um bloco, pois ai nao consegue pegar na caixa
                                                  | otherwise = ([],Jogo m (Jogador (i,j) d c)) -- nao consegue chegar a caixa
                                                    where (movEsquerda,jogoEsquerda)= pegaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) (x,y)
                                                          (movDireita,jogoDireita)=   pegaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita)  (x,y)
                                                          (caiJogadorDiri, caiJogadorDirj) = caiJogador m (i+1,j) 
                                                          (caiJogadorEsqi, caiJogadorEsqj) = caiJogador m (i-1,j)

-- | A funcao 'possivelPousarCaixaDireita' verifica se  é possivel pousar a caixa á direita do jogador
possivelPousarCaixaDireita :: Mapa -> (Int, Int) -> (Int, Int) -> [(Int,Int)]
possivelPousarCaixaDireita m (x,y) (cx,cy) | x >= sx = []-- verificar se nao passamos do limite do mapa
                                           | vazioMapa m (x+1,y) && vazioMapa m (x+1,y-1) && y1>y+1 = [(x1,y1)] ++ possivelPousarCaixaDireita m (x1,y1) (cx,cy) -- cai mais que um bloco logo queremos tentar colocar la uma caixa
                                           | vazioMapa m (x+1,y) && not (vazioMapa m (x+1,y-1)) = procuraEscadaDir m (x+1,y-1) -- para o caso de mapa em escada invertida
                                           | vazioMapa m (x+1,y) = possivelPousarCaixaDireita m (x1,y1) (cx,cy) -- Depois de testar as outras condiçoes continuamos a tentar um bloco a direita
                                           | not(vazioMapa m (x+1,y)) && not(vazioMapa m (x+1,y-1)) && (x/=cx || y/=cy) = [(x,y)] -- dois nao vazios em frente e nao era onde a caixa estava antes de pegar nela
                                           | not(vazioMapa m (x+1,y)) && not(vazioMapa m (x+1,y-1)) = [] -- dois nao vazios em frente, mas era onde a caixa estava
                                           | not(vazioMapa m (x+1,y)) && vazioMapa m (x+1,y-1) = possivelPousarCaixaDireita m (x+1,y-1) (cx,cy) -- trepa
                                           | otherwise = []
                                              where sx=(length (head m))-1 -- valor maximo do x
                                                    (x1,y1)=caiJogador m (x+1,y) -- usamos a cai jogador para medir a altura

-- | A funcao 'procuraEscadaDir' procura estando voltado à direita uma escada invertida no mapa à esquerda (caso muito particular, nivel 5 do jogo)
procuraEscadaDir :: Mapa -> (Int,Int)  -> [(Int,Int)]
procuraEscadaDir m (x,y)  | vazioMapa m (x,y) = [caiJogador m (x,y)]
                          | otherwise = procuraEscadaDir m (x-1,y-1) 

-- | A funcao 'possivelPousarCaixaEsquerda' verifica se é possivel pousar a caixa á esquerda do jogador
possivelPousarCaixaEsquerda :: Mapa -> (Int, Int) -> (Int, Int) -> [(Int,Int)]
possivelPousarCaixaEsquerda m (x,y) (cx,cy) | x <= 1 = []-- vericar se estamos dentro do limite do mapa
                                            | vazioMapa m (x-1,y) && vazioMapa m (x-1,y-1) && y1>y+1 = [(x1,y1)] ++ possivelPousarCaixaEsquerda m (x1,y1) (cx,cy) -- cai mais que um bloco e por isso deve colocar caixa
                                            | vazioMapa m (x-1,y) && not (vazioMapa m (x-1,y-1)) = procuraEscadaEsq m (x-1,y-1)  -- quando o mapa tem uma escada invertida, se tiver um sitio a frente com apenas um vazio ao nivel do jogador, ele pousa a caixa no sitio onde está.
                                            | vazioMapa m (x-1,y) = possivelPousarCaixaEsquerda m (x1,y1) (cx,cy) -- esquerda
                                            | not(vazioMapa m (x-1,y)) && not(vazioMapa m (x-1,y-1)) && (x/=cx || y/=cy) = [(x,y)] -- dois nao vazios em frente (muro), nao pode caminhar mais
                                            | not(vazioMapa m (x-1,y)) && not(vazioMapa m (x-1,y-1)) = [] -- dois nao vazios em frente, mas era onde estava a caixa e por isso mais valia nao ter levantado
                                            | not(vazioMapa m (x-1,y)) && vazioMapa m (x-1,y-1) = possivelPousarCaixaEsquerda m (x-1,y-1) (cx,cy) -- trepa
                                            | otherwise = []
                                               where (x1,y1)=caiJogador m (x-1,y)

-- | A funcao 'procuraEscadaEsq' Procura estando à esquerda uma caixa invertida a direita
procuraEscadaEsq :: Mapa -> (Int,Int) -> [(Int,Int)]
procuraEscadaEsq m (x,y) | vazioMapa m (x,y) = [caiJogador m (x,y)] -- o cai jogador vai devolver a coordenada mais acima no fim da escada
                         | otherwise = procuraEscadaEsq m (x+1,y-1) 

-- esta funcao verifica se e possivel trepar
possivelTrepar :: Jogo -> Bool
possivelTrepar (Jogo m (Jogador (i,j) d c)) | not(c) && d==Este &&
                                       (caixaMapa m (i+1,j) || blocoMapa m (i+1,j)) && vazioMapa m (i+1,j-1) || portaMapa m (i+1,j-1)
                                         =  True
                                    | c && d==Este &&
                                       (caixaMapa m (i+1,j) || blocoMapa m (i+1,j)) && (vazioMapa m (i+1,j-1) || portaMapa m (i+1,j-1)) && vazioMapa m (i+1,j-2)
                                         = True
                                    | not(c) && d==Oeste &&
                                       (caixaMapa m (i-1,j) || blocoMapa m (i-1,j)) && vazioMapa m (i-1,j-1) || portaMapa m (i-1,j-1)
                                         = True
                                    | c && d==Oeste &&
                                       (caixaMapa m (i-1,j) || blocoMapa m (i-1,j)) && (vazioMapa m (i-1,j-1) || portaMapa m (i-1,j-1)) && vazioMapa m (i-1,j-2)
                                         = True
                                    | otherwise = False -- nao e' possivel trepar

-- | A funcao 'procuraPortas' procura a porta e devolve as coordenadas
procuraPortas :: Mapa -> [(Int,Int)]
procuraPortas m = [(i,j) | i<-[0..length (head m)], j<-[0..length m], portaMapa m (i,j)]

-- | A funcao 'largaCaixa' calcula os Movimentos para largar a caixa. Devolve os movimentos e o jogo (para evitar ser recalculado), recebe um jogo e as coordenadas de onde colocar a caixa
largaCaixa :: Jogo -> (Int, Int) -> ([Movimento], Jogo)
largaCaixa (Jogo m (Jogador (i,j) d c)) (x,y) | x==i = colocaLateral (Jogo m (Jogador (i,j) d c)) -- queremos colocar a caixa no sitio onde esta o jogador, logo temos de mover para um dos lados 
                                              | x==i-1 && d==Oeste && ((vazioMapa m (i-1,j) && vazioMapa m (i-1,j-1)) || vazioMapa m (i-1,j-1))= ([InterageCaixa], moveJogador (Jogo m (Jogador (i,j) d c)) InterageCaixa)-- caso queiramos colocar a caixa imediatamente á esquerda do jogador e este estiver virado para a esquerda e caso haja sitio possivel para pousar a caixa entao esta é pousada
                                              | x==i-1 && d==Este  = colocaCostas (Jogo m (Jogador (i,j) d c)) (x,y)-- caso o jogador esteja de costas para onde quer colocar a caixa
                                              | ((x<i && d==Este) || (x<i && possivelEsquerda m (Jogador (i,j) d c))) && movEsquerda/=[] = ([AndarEsquerda]++movEsquerda,jogoEsquerda)  -- Vira ou anda para a esquerda verificando se tem um caminho a partir de cada movimento para a esquerda
                                              | x<i && possivelTrepar (Jogo m (Jogador (i,j) d c)) && movTrepar/=[] = ([Trepar]++movTrepar,jogoTrepar) -- no caso de ao andar para esquerda ser necessario trepar
                                              | x==i+1 && d==Este && ((vazioMapa m (i+1,j) && vazioMapa m (i+1,j-1)) || vazioMapa m (i+1,j-1)) = ([InterageCaixa], moveJogador (Jogo m (Jogador (i,j) d c)) InterageCaixa)-- larga a caixa
                                              | x==i+1 && d==Oeste = colocaCostas (Jogo m (Jogador (i,j) d c)) (x,y)-- esta de costas
                                              | ((x>i && d==Oeste) || (x>i && possivelDireita m (Jogador (i,j) d c))) && movDireita/=[] = ([AndarDireita]++movDireita,jogoDireita) -- Vira ou anda para a direita verificano se tem um caminho cada vez que anda a direita
                                              | x>i && possivelTrepar (Jogo m (Jogador (i,j) d c)) && movTrepar/=[]= ([Trepar]++movTrepar,jogoTrepar) -- verificar se precisa de trepar ao andar para a direita e se ha solucao apos trepar
                                              | otherwise = ([], Jogo m (Jogador (i,j) d c)) -- nao e possivel
                                                where (movEsquerda,jogoEsquerda)=largaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) AndarEsquerda) (x,y)
                                                      (movDireita,jogoDireita)= largaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) AndarDireita) (x,y)
                                                      (movTrepar,jogoTrepar)= largaCaixa (moveJogador (Jogo m (Jogador (i,j) d c)) Trepar) (x,y)

-- | A funcao 'colocaLateral' nesta funçao calculamos os movimentos que o jogador tem de fazer para pousar a caixa no sitio onde está
colocaLateral :: Jogo -> ([Movimento], Jogo)
colocaLateral (Jogo m (Jogador (i,j) d c)) | possivelEsquerda m (Jogador (i,j) d c) && possivelEsquerda m (Jogador (caiEsquerdai,caiEsquerdaj) d c) && caiEsquerdaj<=j+1 && caiEsqEsqj<=j+1 = -- nao pode cair mais que um bloco para baixo em nenhuma das vezes que anda para a esquerda
                                                   ([AndarEsquerda,AndarEsquerda]++movEsq2x, jogoEsq2x)
                                           | possivelEsquerda m (Jogador (i,j) d c) && caiEsquerdaj<=j+1 && possivelTrepar (Jogo m (Jogador (caiEsquerdai,caiEsquerdaj) d c)) = --verificar se consegue andar para a esquerda e trepar, sendo que nao pode cair mais do que um
                                                   ([AndarEsquerda,Trepar]++movEsqTre, jogoEsqTre)
                                           | possivelDireita m (Jogador (i,j) d c) && possivelDireita m (Jogador (caiDireitai,caiDireitaj) d c) && caiDireitaj<=j+1 && caiDirDirj<=j+1= -- nao pode cair mais que um bloco para baixo em nenhuma das vezes que anda para a direita
                                                   ([AndarDireita,AndarDireita]++movDir2x, jogoDir2x)
                                           | possivelDireita m (Jogador (i,j) d c) && possivelTrepar (Jogo m (Jogador (caiDireitai,caiDireitaj) d c)) && caiDireitaj<=j+1 = -- verificar se consegue andar para a direita e trepar, sendo que nao pode cair mais do que um
                                                   ([AndarDireita,Trepar]++movDirTre, jogoDirTre)
                                           | d==Oeste && possivelTrepar (Jogo m (Jogador (i,j) d c)) && possivelTrepar (Jogo m (Jogador (i-1,j-1) d c)) = -- verificar se e possivel trepar duas vezes
                                                   ([Trepar,Trepar]++movTre2x, jogoTre2x) -- trepar, trepar esquerda
                                           | d==Este && possivelTrepar (Jogo m (Jogador (i,j) d c)) && possivelTrepar (Jogo m (Jogador (i-1,j-1) d c)) = -- verificar se e possivel trepar duas vezes
                                                   ([AndarEsquerda,Trepar,Trepar]++movEsqTre2x, jogoEsqTre2x) -- trepar, trepar esquerda, mas virado à direita
                                           | d==Oeste && possivelTrepar (Jogo m (Jogador (i,j) d c)) && possivelTrepar (Jogo m (Jogador (i+1,j-1) d c)) = -- verificar se é possivel andar para a direita e trepar duas vezes, mas neste caso esta virado para a esquerda
                                                   ([AndarDireita,Trepar,Trepar]++movDirTre2x, jogoDirTre2x) -- trepar, trepar direita, mas virado à esquerda
                                           | d==Este && possivelTrepar (Jogo m (Jogador (i,j) d c)) && possivelTrepar (Jogo m (Jogador (i+1,j-1) d c)) = -- verificar se é possivel andar para a esquerda e trepar duas vezes, mas neste caso esta virado para a direita
                                                   ([Trepar,Trepar]++movTre2x, jogoTre2x) -- trepar, trepar esquerda, mas virado à direita
                                           | otherwise = ([],Jogo m (Jogador (i,j) d c))
                                             where (caiEsquerdai,caiEsquerdaj)=caiJogador m (i-1,j)
                                                   (caiEsqEsqi,caiEsqEsqj)=caiJogador m (i-2,j)
                                                   (caiDireitai,caiDireitaj)=caiJogador m (i+1,j)
                                                   (caiDirDiri,caiDirDirj)=caiJogador m (i+2,j)
                                                   (movEsq2x,jogoEsq2x)=largaCaixa (andaEsquerda $ andaEsquerda (Jogo m (Jogador (i,j) d c))) (i,j) --movimentos para largar a caixa apos andar duas vezes para a direita
                                                   (movEsqTre,jogoEsqTre)=largaCaixa (trepar $ andaEsquerda (Jogo m (Jogador (i,j) d c))) (i,j)
                                                   (movDir2x,jogoDir2x)=largaCaixa (andaDireita $ andaDireita (Jogo m (Jogador (i,j) d c))) (i,j)
                                                   (movDirTre,jogoDirTre)=largaCaixa (trepar $ andaDireita (Jogo m (Jogador (i,j) d c))) (i,j)
                                                   (movTre2x,jogoTre2x)=largaCaixa (trepar $ trepar (Jogo m (Jogador (i,j) d c))) (i,j)
                                                   (movEsqTre2x,jogoEsqTre2x)=largaCaixa (andaEsquerda $ trepar $ trepar (Jogo m (Jogador (i,j) d c))) (i,j)
                                                   (movDirTre2x,jogoDirTre2x)=largaCaixa (andaDireita $ trepar $ trepar (Jogo m (Jogador (i,j) d c))) (i,j)

-- | A funcao 'colocaCostas' devolve a lista de movimentos necessarios para colocar a caixa numa posicao que estava anteriormente atras do jogador
colocaCostas :: Jogo -> (Int,Int) -> ([Movimento], Jogo)
colocaCostas (Jogo m (Jogador (i,j) d c)) (x,y) | d==Oeste && possivelEsquerda m (Jogador (i,j) d c) && caiEsquerdaj<=j+1 && movEsqDir/=[] = -- nao pode cair mais que um
                                                   ([AndarEsquerda,AndarDireita]++movEsqDir, jogoEsqDir)
                                                | d==Oeste && possivelTrepar (Jogo m (Jogador (i,j) d c)) && movTreDir/= [] =
                                                   ([Trepar,AndarDireita]++movTreDir, jogoTreDir)
                                                | d==Este && possivelDireita m (Jogador (i,j) d c) && caiDireitaj<=j+1 && movDirEsq/=[]=
                                                   ([AndarDireita,AndarEsquerda]++movDirEsq, jogoDirEsq)
                                                | d==Este && possivelTrepar (Jogo m (Jogador (i,j) d c)) && movTreEsq/=[] =
                                                   ([Trepar,AndarEsquerda]++movTreEsq, jogoTreEsq)
                                                | otherwise = ([],Jogo m (Jogador (i,j) d c))
                                                  where (caiEsquerdai,caiEsquerdaj)=caiJogador m (i-1,j)
                                                        (caiDireitai,caiDireitaj)=caiJogador m (i+1,j)
                                                        (movEsqDir,jogoEsqDir)=largaCaixa (andaDireita $ andaEsquerda (Jogo m (Jogador (i,j) d c))) (x,y) -- o resto do nivel apos mudar a direção
                                                        (movTreEsq,jogoTreEsq)=largaCaixa (andaEsquerda $ trepar (Jogo m (Jogador (i,j) d c))) (x,y)
                                                        (movTreDir,jogoTreDir)=largaCaixa (andaDireita $ trepar (Jogo m (Jogador (i,j) d c))) (x,y)
                                                        (movDirEsq,jogoDirEsq)=largaCaixa (andaEsquerda $ andaDireita (Jogo m (Jogador (i,j) d c))) (x,y)
