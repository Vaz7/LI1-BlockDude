module Main where

import LI12122

import Tarefa1_2021li1g081

import Tarefa2_2021li1g081

import Tarefa3_2021li1g081

import Tarefa4_2021li1g081

import Tarefa6_2021li1g081

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.Time.Clock
import System.Exit

data Opcoes = Jogar | Load | Ajuda | Sair
         deriving (Show)

data SubOpcoes = Mapa1 | Mapa2 | Mapa3 | Mapa4 | Mapa5 | Anterior
         deriving (Show)

data Menu = Controlador Opcoes | ModoJogo | VenceuJogo | Controla SubOpcoes | Help | Resolver | Resolvido (Maybe [Movimento]) | Terminado
         deriving (Show)

type Level = Int

type GameScore = Int

type World = (Menu,Jogo,Level,GameScore)

-- | A funcao 'drawWorld' realiza os desenhos de todos os ecras que podem aparecer durante o jogo
drawWorld :: World -> [Picture] -> IO Picture
drawWorld (VenceuJogo,jogo,l,s) _ = return $ Translate (-600) 0 $ Scale (0.5) (0.5) $ Color red $ Text ("Ganhou: O score = " ++ show s ++ " No nivel = " ++ show l)
drawWorld (Controlador Jogar,jogo,_,_) _ = return $ Pictures [Color green $ Translate (-150) (150) $ Text "Play", Color white $ Translate (-150) (30) $ Text "Load",  Color white  $ Translate (-150) (-90) $ Text "Help", Color white $ Translate (-150) (-210) $ Text "Exit"]
drawWorld (Controlador Load, jogo,_,_) _ = return $ Pictures [Color white  $ Translate (-150) (150) $ Text "Play", Color green $ Translate (-150) (30) $ Text "Load",  Color white  $ Translate (-150) (-90) $ Text "Help", Color white $ Translate (-150) (-210) $ Text "Exit"]
drawWorld (Controlador Sair, jogo,_,_) _ = return $ Pictures [Color white  $ Translate (-150) (150) $ Text "Play", Color white $ Translate (-150) (30) $ Text "Load", Color white $ Translate (-150) (-90) $ Text "Help", Color green $ Translate (-150) (-210) $ Text "Exit"]
drawWorld (Controlador Ajuda, jogo,_,_) _ = return $ Pictures [Color white  $ Translate (-150) (150) $ Text "Play", Color white $ Translate (-150) (30) $ Text "Load", Color green $ Translate (-150) (-90) $ Text "Help", Color white $ Translate (-150) (-210) $ Text "Exit"]
drawWorld (ModoJogo, (Jogo m jog),h,s) imgs = return $ Translate 70 70 $ Scale (1.3) (1.3) $ Pictures  ([Color white  $ Translate (-350) 200 $ Scale (0.1) (0.1) $ Text "Press F1 to exit and save current game!",Color white  $ Translate (-350) 220 $ Scale (0.1) (0.1) $ Text "Press F2 to see the game solution!",Color white  $ Translate (-350) 240 $ Scale (0.1) (0.1) $ Text "Press Q to get back to the main menu!"] ++ (drawMapa m 0 imgs) ++ (drawBoneco jog imgs) ++ drawScore h s)
drawWorld (Controla Mapa1, jogo,_,_) _ =    return $ Pictures [Color green $ Translate (-150) (150) $ Text "World 1", Color white $ Translate (-150) (30) $ Text "World 2", Color white $ Translate (-150) (-90) $ Text "World 3", Color white $ Translate (-150) (-210) $ Text "World 4",Color white $ Translate (-150)(-330) $ Text "World 5",Color white $ Translate (-150)(-500) $ Text "Back"]
drawWorld (Controla Mapa2, jogo, _, _) _ =  return $ Pictures [Color white $ Translate (-150) (150) $ Text "World 1", Color green $ Translate (-150) (30) $ Text "World 2", Color white $ Translate (-150) (-90) $ Text "World 3", Color white $ Translate (-150) (-210) $ Text "World 4",Color white $ Translate (-150)(-330) $ Text "World 5",Color white $ Translate (-150)(-500) $ Text "Back"]
drawWorld (Controla Mapa3, jogo, _, _) _ =  return $ Pictures [Color white $ Translate (-150) (150) $ Text "World 1", Color white $ Translate (-150) (30) $ Text "World 2", Color green $ Translate (-150) (-90) $ Text "World 3", Color white $ Translate (-150) (-210) $ Text "World 4",Color white $ Translate (-150)(-330) $ Text "World 5",Color white $ Translate (-150)(-500) $ Text "Back"]
drawWorld (Controla Mapa4, jogo, _, _) _ =  return $ Pictures [Color white $ Translate (-150) (150) $ Text "World 1", Color white $ Translate (-150) (30) $ Text "World 2", Color white $ Translate (-150) (-90) $ Text "World 3", Color green $ Translate (-150) (-210) $ Text "World 4",Color white $ Translate (-150)(-330) $ Text "World 5",Color white $ Translate (-150)(-500) $ Text "Back"]
drawWorld (Controla Mapa5, jogo, _, _) _ =  return $ Pictures [Color white $ Translate (-150) (150) $ Text "World 1", Color white $ Translate (-150) (30) $ Text "World 2", Color white $ Translate (-150) (-90) $ Text "World 3", Color white $ Translate (-150) (-210) $ Text "World 4",Color green $ Translate (-150)(-330) $ Text "World 5",Color white $ Translate (-150)(-500) $ Text "Back"]
drawWorld (Controla Anterior, jogo,_,_) _ = return $ Pictures [Color white $ Translate (-150) (150) $ Text "World 1", Color white $ Translate (-150) (30) $ Text "World 2", Color white $ Translate (-150) (-90) $ Text "World 3", Color white $ Translate (-150) (-210) $ Text "World 4",Color white $ Translate (-150)(-330) $ Text "World 5",Color green $ Translate (-150)(-500) $ Text "Back"] 
drawWorld (Help,jogo,_,_) _ = return $ Pictures [Translate (-150) (300) $ Scale (0.5) (0.5) $ Color green $ Text ("Help:"),
 Translate (-800) (200) $ Scale (0.25) (0.25) $ Color white $ Text ("The goal of this game is to get to the door, to do so you can climb blocks and move boxes!"),
 Translate (-800) (150) $ Scale (0.25) (0.25) $ Color white $ Text ("To Walk Righ Press the right arrow or D."),
 Translate (-800) (100) $ Scale (0.25) (0.25) $ Color white $ Text ("To Walk Left Press the right arrow or A."),
 Translate (-800) (50) $ Scale (0.25) (0.25) $ Color white $ Text ("To pick up or let go of a box press the Down arrow or S."),
 Translate (-800) (0) $ Scale (0.25) (0.25) $ Color white $ Text ("To climb boxes or blocks press the Up arrow or."),
 Translate (-800) (-50) $ Scale (0.25) (0.25) $ Color white $ Text ("To Save Game press F1."),
  Translate (-800) (-100) $ Scale (0.25) (0.25) $ Color white $ Text ("To Load the previous saved game press Load on the Main Menu."),
 Translate (-800) (-150) $ Scale (0.25) (0.25) $ Color white $ Text ("If you are finding a level too hard press F2 during a game to see the solution."),
 Translate (-800) (-200) $ Scale (0.25) (0.25) $ Color white $ Text ("To Restart the level press R."),
 Translate (-150) (-300) $ Scale (0.5) (0.5) $ Color red $ Text ("Back")]
drawWorld (Resolver,_,_,_) _ = return $ Translate (-70) 0 $ Scale (0.2) (0.2) $ Pictures [Color green $ Text "Solving the puzzle"]
drawWorld (Resolvido mov, (Jogo m jog),h,s) imgs = return $ Translate 70 70 $ Scale (1.3) (1.3) $ Pictures  ([Color white  $ Translate (-350) 200 $ Scale (0.3) (0.3) $ Text "Simulating..."] ++ (drawMapa m 0 imgs) ++ (drawBoneco jog imgs) ++ drawScore h s)
drawWorld (Terminado, (Jogo m jog),h,s) imgs = return $ Translate 70 70 $ Scale (1.3) (1.3) $ Pictures  ([Color white  $ Translate (-350) 200 $ Scale (0.2) (0.2) $ Text "Simulation ended.",Color white $ Translate (-350) 170 $ Scale (0.2) (0.2) $ Text "Press Q or Enter to get back to the main menu."] ++ (drawMapa m 0 imgs) ++ (drawBoneco jog imgs) ++ drawScore h s)

-- | A funcao 'drawmapa' separa as linhas do mapa para posteriormente serem convertidas para imagens
drawMapa:: Mapa -> Int -> [Picture] -> [Picture]
drawMapa [] _ p = []
drawMapa (p:m) linha imgs = (drawMapa m (linha+1) imgs) ++ (drawMapaLinha p linha 0 imgs) --vamos chamando a funcao seguinte com cada uma das linhas, aumentando sempre o valor de i para termos sempre as coordenadas corretas

-- | A funcao 'drawmapalinha' pega numa linha do mapa de cada vez e separa os espacos vazios do resto, sendo que nos espaços nao vazios ira introduzir as imagens, para as restantes peças chama a funcao seguinte
drawMapaLinha:: [Peca] -> Int -> Int -> [Picture] -> [Picture]
drawMapaLinha [] _ _ p = []
drawMapaLinha (p:m) i j imgs | p==Vazio = (drawMapaLinha m i (j+1) imgs)
                             | otherwise =  [(translate x y $ drawPeca p imgs)] ++ (drawMapaLinha m i (j+1) imgs) -- O translate da a posicao de cada bloco, ao longo da lista vamos somando ao valor de j para irmos tendo as coordenadas corretas
                               where (x,y)=gridToSpace (i,j)

-- | A funçao 'drawpeca' é a funçao que a cada figura associa uma imagem
drawPeca :: Peca -> [Picture] -> Picture
drawPeca p imgs | p==Bloco = (imgs !! 0)
                | p==Porta = (imgs !! 1)
                | otherwise = (imgs !! 2) -- caixa

-- | A funçao 'drawBoneco' faz o mesmo que a drawmapa, adiciona o jogador na determinada posicao onde se encontra
drawBoneco :: Jogador -> [Picture] -> [Picture]
drawBoneco (Jogador (i,j) d c) imgs | not(c) && d==Oeste = [translate x y (imgs !! 3)] -- se nao tiver caixa e estiver virado para oeste entao usamos a imagem 3
                                    | not(c) && d==Este =  [translate x y (imgs !! 4)] -- se nao tiver caixa e estiver virado para este entao usamos a imagem 4
                                    | c &&  d==Oeste = [translate x y (imgs !! 3), translate x1 y1 (imgs !! 2)] -- se tiver caixa e estiver virado para oeste entao usamos as images 3 e 2 e calculamos as coordenadas da caixa através da posiçao do jogador
                                    | c &&  d==Este = [translate x y (imgs !! 4), translate x1 y1 (imgs !! 2)] -- se tiver caixa e estiver virado para este entao usamos as images 4 e 2 e calculamos as coordenadas da caixa através da posiçao do jogador
                                      where (x,y)=gridToSpace (j,i)
                                            (x1,y1)=gridToSpace (j-1,i)

-- | A funçao 'gridToSpace' converte as coordenadas do nosso referencial cartesiano para as coordenadas do gloss, sendo que no nosso caso optamos pelo uso de imagens com 32 pixeis de largura e altura                                          
gridToSpace :: (Int,Int) -> (Float,Float)
gridToSpace (i,j) = ((fromIntegral j)*32.0-320.0-16.0,-(fromIntegral i)*32.0-16.0)

-- | A funçao 'drawscore' é usada para imprimir o score enquanto o estado de jogo for jogar ou resolver (simulacao)
drawScore :: Int -> Int -> [Picture]
drawScore l s = [Translate (-200) 0 $ Scale (0.1) (0.1) $ Color white $ Text ("Score = " ++ show s ++ " Nivel = " ++ show l)]

-- | A funçao 'printMapa' é usada para escrever o mapa na forma de "Matriz" para posteriormente ser gravado no ficheiro txt
printMapa :: Mapa -> String
printMapa [] = ""
printMapa (x:xs) = printRow x ++ "\n" ++ (printMapa xs)

-- | A funçao 'printrow' é auxiliar da printMapa, é ela que imprime cada linha separadamente
printRow :: [Peca] -> String
printRow [] = ""
printRow (x:xs) = show x ++ printRow xs

-- | A funçao 'printJogador' imprime os dados do jogador separadas por diferentes linhas para as coordenas, direcao e estado de caixa
printJogador :: Jogador -> String
printJogador (Jogador (i,j) dir c) = show i ++ "\n" ++ show j ++ "\n" ++ show dir ++ "\n" ++ show c


-- | A funçao 'saveGame' grava os dados do mapa, do jogador e do nivel, incluindo o score em 3 ficheiros separados
saveGame :: Jogo -> (Int, Int) -> IO Jogo -- poderiamos usar IO() e nao retornar nada
saveGame (Jogo m j) (l,s) = do
                     writeFile "mapa.txt" (printMapa m) -- Gravamos o ficheiro mapa com o resultado da aplicaçao da funçao printMapa ao mapa atual
                     writeFile "jogador.txt" (printJogador j) -- Gravamos o ficheiro jogador com o resultado da aplicaçao da funçao ao estado de jogador atual 
                     writeFile "level.txt" (show l ++ "\n" ++ show s) -- Guardamos os dados do nivel atual e score atual no ficheiro
                     return (Jogo m j)

-- | A funcao 'readgame' é a funcao principal que permite ler os ficheiros txt
readGame :: IO Jogo
readGame = do
            m<-readMapa
            j<-readJogador
            return (Jogo m j)

-- | A funçao 'readMapa' le o ficheiro do mapa, separa-o por linhas e chama a funçao seguinte
readMapa :: IO Mapa
readMapa = do
             mapaString <- readFile "mapa.txt"
             let m = readLinhas $ lines mapaString -- lines separa o ficheiro em linhas
             return(m)

-- | A funçao 'readLinhas' pega em cada uma das linas e transforma-as num mapa usando uma funçao auxiliar
readLinhas :: [String] -> Mapa
readLinhas [] = []
readLinhas (x:xs) = [readLinha x] ++ readLinhas xs

-- | A funcao 'readLinha' pega em cada elemento de cada linha e transforma as strings numa lista de pecas
readLinha :: String -> [Peca]
readLinha [] = []
readLinha (x:xs) | x=='X' = [Bloco] ++ readLinha (xs)
                 | x=='P' = [Porta] ++ readLinha (xs)
                 | x=='C' = [Caixa] ++ readLinha (xs)
                 | otherwise = [Vazio] ++ readLinha (xs)

-- | A funçao 'readJogador' le o ficheiro de save do jogador, lendo cada uma das 4 linhas do ficheiro separadamente 
readJogador :: IO Jogador
readJogador = do
                jogador <- readFile "jogador.txt"
                let [i,j,d,c] = lines jogador
                return (Jogador (read i :: Int, read j :: Int) (readDir d) (readCaixa c)) -- O read transforma uma string num Int forçando se a que o tipo do read seja Int

-- | A funçao 'readDir' volta a associar o sinal de maior ou menor a uma direçao no nosso tipo de dados
readDir :: String -> Direcao
readDir d | d=="<" = Oeste
          | otherwise = Este

-- | A funçao 'readCaixa' Transforma o texto do ficheiro com o estado da caixa num valor do nosso tipo de dados
readCaixa :: String -> Bool
readCaixa c | c=="True" = True
            | otherwise = False

-- | A funçao 'readLevelScore' le o score e o nivel do respetivo ficheiro  transformando as strings em inteiros
readLevelScore :: IO (Int, Int)
readLevelScore = do
                  level <- readFile "level.txt"
                  let [l,s]= lines level
                  return (read l :: Int, read s :: Int) -- transformar string para int

-- | A funçao 'eventStep' é onde definimos o que cada tecla fará durante o nosso jogo
eventStep :: Event -> World -> IO World 

-- tecla enter menu principal
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,j,l,s) = return $ (Controla Mapa1,j,l,s) -- pressionou enter quando jogo estava selecionado
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Load,j,l,s) = do --aqui é onde se vai buscar o save
                                                                                j<-readGame
                                                                                (l,s)<-readLevelScore
                                                                                return (ModoJogo, j, l, s)
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair,j,l,s) = exitSuccess -- enter para sair
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Ajuda,j,l,s) = return $ (Help,j,l,s)
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo,j,l,s) = return $ (Controla Mapa1,j,l,s) -- enter depois de vencer

--tecla enter no sub menu
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controla Mapa1,j,l,s) = return $ (ModoJogo, jogo, 1, 0)
                                                                                  where (_,jogo,_,_) = initWorld 1 -- pressionou enter quando jogo estava selecionado
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controla Mapa2,j,l,s) = return $ (ModoJogo, jogo, 2, 0)
                                                                                  where (_,jogo,_,_) = initWorld 2
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controla Mapa3,j,l,s) = return $ (ModoJogo, jogo, 3, 0)
                                                                                  where (_,jogo,_,_) = initWorld 3
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controla Mapa4,j,l,s) = return $ (ModoJogo, jogo, 4, 0)
                                                                                  where (_,jogo,_,_) = initWorld 4
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controla Mapa5,j,l,s) = return $ (ModoJogo, jogo, 5, 0)
                                                                                  where (_,jogo,_,_) = initWorld 5
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Controla Anterior,j,l,s) = return $ (Controlador Jogar, j, l, s)


--sair do ecrã no fim da simulação
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Terminado,j,l,s) = return $ (Controlador Jogar, j, l, s)
eventStep (EventKey (Char char) Down _ _) (Terminado,j,l,s) | char == 'q' = return $ (Controlador Jogar,j,l,s)
                                                           | otherwise = return $ (ModoJogo,j,l,s)

-- tecla down no menu
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar,j,l,s) = return $ (Controlador Load,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Load,j,l,s) = return $ (Controlador Ajuda,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Ajuda,j,l,s) = return $ (Controlador Sair,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair,j,l,s) = return $ (Controlador Jogar,j,l,s)

--tecla down no sub menu
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controla Mapa1,j,l,s) = return $ (Controla Mapa2,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controla Mapa2,j,l,s) = return $ (Controla Mapa3,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controla Mapa3,j,l,s) = return $ (Controla Mapa4,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controla Mapa4,j,l,s) = return $ (Controla Mapa5,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controla Mapa5,j,l,s) = return $ (Controla Anterior,j,l,s)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (Controla Anterior,j,l,s) = return $ (Controla Mapa1,j,l,s)

-- tecla up no menu
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar,j,l,s) = return $ (Controlador Sair,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair,j,l,s) = return $ (Controlador Ajuda,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Ajuda,j,l,s) = return $ (Controlador Load,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Load,j,l,s) = return $ (Controlador Jogar,j,l,s)
-- tecla up no sub menu
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controla Mapa1,j,l,s) = return $ (Controla Anterior,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controla Mapa2,j,l,s) = return $ (Controla Mapa1,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controla Mapa3,j,l,s) = return $ (Controla Mapa2,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controla Mapa4,j,l,s) = return $ (Controla Mapa3,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controla Mapa5,j,l,s) = return $ (Controla Mapa4,j,l,s)
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (Controla Anterior,j,l,s) = return $ (Controla Mapa5,j,l,s)

-- teclas dentro do jogo
eventStep (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo,j,l,s) =    return $ (ModoJogo, moveJogador j Trepar,l,s+1)
eventStep (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo,j,l,s) =  return $ (ModoJogo, moveJogador j InterageCaixa,l,s+1) 
eventStep (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo,j,l,s) =  return $ (ModoJogo, moveJogador j AndarEsquerda,l,s+1)
eventStep (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo,j,l,s) = return $ (ModoJogo, moveJogador j AndarDireita,l,s+1)
eventStep (EventKey (SpecialKey KeyF1) Down _ _) (ModoJogo,j,l,s) = do
                                                                      jj <- saveGame j (l,s)
                                                                      exitSuccess


eventStep (EventKey (SpecialKey KeyF2) Down _ _) (ModoJogo,j,l,s) = return $ (Resolver, j, l, s) -- ao carregarmos no F2 para baixo muda para o estado resolver
eventStep (EventKey (SpecialKey KeyF2) Up _ _) (Resolver,j,l,s) = do -- ao largarmos a tecla, apenas fazemos isto para quando está a calcular as jogadas ficar no ecra que diz que esta a calcular
                                                                    let (_,jogo,l2,s2)=initWorld l 
                                                                    let movimentos = (resolveJogo 250 jogo)
                                                                    putStrLn ("Movimentos= " ++ show movimentos) -- so para o programa nao passar para a linha de baixo antes de termninar o calculo do vetor completo (o haskell é preguiçoso :P) 
                                                                    return (Resolvido movimentos, jogo, l2, s2)

eventStep (EventKey (Char char) Down _ _) (ModoJogo,j,l,s) | char == 'r' = return $ resetWorld l
                                                           | char == 'q' = return $ (Controlador Jogar,j,l,s)
                                                           | char == 'w' = return $ (ModoJogo, moveJogador j Trepar,l,s+1) --so para tambem poder andar no wsad
                                                           | char == 's' = return $ (ModoJogo, moveJogador j InterageCaixa,l,s+1)
                                                           | char == 'a' = return $ (ModoJogo, moveJogador j AndarEsquerda,l,s+1)
                                                           | char == 'd' = return $ (ModoJogo, moveJogador j AndarDireita,l,s+1)
                                                           | otherwise = return $ (ModoJogo,j,l,s)

                                                       
--tecla q no menu da ajuda
eventStep (EventKey (SpecialKey KeyEnter) Down _ _) (Help,j,l,s) = return $ (Controlador Jogar,j,l,s)
eventStep (EventKey (Char char) Down _ _) (Help,j,l,s) | char == 'q' = return $ (Controlador Jogar,j,l,s)
                                                       | otherwise = return $ (Help,j,l,s)

-- outros eventos sem accao
eventStep _ w = return $ w

-- | A funçao 'timeStep' verifica, varias vezes por segundo se deve terminar o jogo (o jogo termina quando o jogador chega à porta)
timeStep :: Float -> World -> IO World
timeStep _ (ModoJogo, Jogo m (Jogador (i,j) d c), h, s) | portaMapa m (i,j) = return $ (VenceuJogo, Jogo m (Jogador (i,j) d c), min h s, s)
                                                        | otherwise = return $ (ModoJogo, Jogo m (Jogador (i,j) d c), h, s)

-- | esta parte do time step serve para quando o estado de jogo mudar para resolvido se mostrar a simulaçao, caso nao haja solucao entao apenas aparece terminado.
timeStep t (Resolvido Nothing, j, l, s) = return $ (Terminado, j, l, s)
timeStep t (Resolvido (Just []), j, l, s) = return $ (Terminado, j, l, s)
timeStep t (Resolvido (Just (x:xs)), j, l, s) = do
                                                 time <- integralTime
                                                 if (mod time 10) == 0 then return $ (Resolvido (Just xs), moveJogador j x, l, s+1) --se o tempo for multiplo de 10 desenha a prox jogada
                                                                       else return $ (Resolvido (Just (x:xs)), j, l, s) -- senao começa esta parte de novo ate ser multiplo de 10

timeStep _ w = return $ w

-- | A funcao 'integralTime' calcula o tempo do pc em segundos
integralTime :: (Integral a) => IO a
integralTime = getCurrentTime >>= return.floor.(*) 25.utctDayTime --floor arredonda, da o valor da data em segundos vezes o numero de frames, o ponto faz com que as funçoes corram da direita para a esquerda


-- | A funcao 'resetWorld' permite que quando o jogador pressione para recomecar o nivel este recomece com o score a 0
resetWorld :: Int -> World
resetWorld 1 = (ModoJogo, jogo, 1, 0)
                        where (_,jogo,_,_) = initWorld 1
resetWorld 2 = (ModoJogo, jogo, 2, 0)
                        where (_,jogo,_,_) = initWorld 2
resetWorld 3 = (ModoJogo, jogo, 3, 0)
                        where (_,jogo,_,_) = initWorld 3
resetWorld 4 = (ModoJogo, jogo, 4, 0)
                        where (_,jogo,_,_) = initWorld 4
resetWorld 5 = (ModoJogo, jogo, 5, 0)
                        where (_,jogo,_,_) = initWorld 5


-- | A funcao initWorld é usada na primeira vez que cada nivel é selecionado carregando o mapa e o jogador
initWorld :: Int -> World
initWorld 1 =  (Controlador Jogar, Jogo mapa jogador, 1, 0)
                 where mapa = constroiMapa [ (Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,4)),(Bloco, (0,5)), (Bloco, (0,6)), (Bloco, (0,7)), (Porta, (1,6)), (Bloco, (1,7)),(Bloco, (2,7)), (Bloco, (3,7)), (Bloco, (4,7)), (Bloco, (5,6)), (Bloco, (6,5)),(Bloco, (7,5)), (Bloco, (7,6)), (Bloco, (7,7)), (Bloco, (8,7)), (Bloco, (9,7)),(Bloco, (10,6)), (Caixa, (11,5)), (Bloco, (11,6)), (Caixa, (12,5)), (Bloco, (12,6)), (Caixa, (13,4)), (Bloco, (13,5)), (Bloco, (14,0)), (Bloco, (14,1)), (Bloco, (14,2)), (Bloco, (14,3)), (Bloco, (14,4))]
                       jogador = Jogador (8, 6) Oeste False

initWorld 2 = (Controlador Jogar, Jogo mapa jogador, 2, 0)
                 where mapa = constroiMapa [(Bloco,(0,2)),(Bloco,(0,3)), (Bloco,(0,4)),(Bloco,(0,5)), (Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)), (Bloco,(0,10)),(Bloco,(1,0)), (Bloco,(1,1)),(Porta,(1,9)),(Bloco,(1,10)),(Bloco,(2,2)), (Bloco,(2,6)),(Bloco,(2,7)), (Bloco,(2,8)),(Bloco,(2,9)), (Bloco,(2,10)),(Bloco,(3,3)), (Bloco,(3,6)),(Bloco,(4,2)), (Bloco,(4,6)),(Bloco,(4,7)),(Bloco,(4,8)),(Bloco,(4,9)), (Bloco,(4,10)),(Bloco,(5,1)), (Caixa,(5,8)),(Bloco,(5,9)),(Bloco,(5,10)),(Bloco,(6,1)), (Caixa,(6,8)),(Bloco,(6,9)), (Bloco,(7,1)),(Bloco,(7,9)),(Bloco,(8,1)),(Bloco,(8,8)), (Bloco,(8,9)),(Bloco,(9,1)), (Bloco,(9,7)),(Bloco,(9,8)),(Bloco,(9,9)),(Bloco,(9,10)), (Bloco,(10,1)),(Bloco,(10,10)), (Bloco,(11,1)),(Bloco,(11,9)),(Bloco,(11,10)),(Bloco,(12,1)), (Bloco,(12,7)),(Bloco,(12,8)), (Bloco,(12,9)),(Bloco,(13,1)),(Bloco,(13,6)),(Bloco,(13,7)), (Bloco,(14,1)),(Caixa,(14,6)),(Bloco,(14,7)),(Bloco,(15,1)),(Bloco,(15,7)),(Bloco,(16,1)),(Caixa,(16,5)),(Bloco,(16,6)),(Bloco,(16,7)),(Bloco,(17,1)),(Caixa,(17,4)),(Caixa,(17,5)),(Bloco,(17,6)),(Bloco,(18,2)),(Bloco,(18,3)),(Bloco,(18,4)),(Bloco,(18,5))]
                       jogador = Jogador (9, 6) Este False

initWorld 3 = (Controlador Jogar, Jogo mapa jogador, 3, 0)
                 where mapa = constroiMapa [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(1,1)),(Porta,(1,8)),(Bloco,(1,9)),(Bloco,(1,10)),(Bloco,(1,11)),(Bloco,(1,12)),(Bloco,(2,1)),(Bloco,(2,12)),(Bloco,(3,1)),(Bloco,(3,9)),(Bloco,(3,10)),(Bloco,(3,11)),(Bloco,(3,12)),(Bloco,(4,1)),(Bloco,(4,9)),(Bloco,(5,0)),(Bloco,(5,8)),(Bloco,(5,9)),(Bloco,(6,0)),(Bloco,(6,5)),(Bloco,(6,6)),(Bloco,(6,7)),(Bloco,(6,8)),(Bloco,(7,0)),(Caixa,(7,7)),(Bloco,(7,8)),(Bloco,(8,1)),(Caixa,(8,7)),(Bloco,(8,8)),(Bloco,(9,1)),(Caixa,(9,7)),(Bloco,(9,8)),(Bloco,(10,1)),(Caixa,(10,7)),(Bloco,(10,8)),(Bloco,(11,1)),(Bloco,(11,8)),(Bloco,(11,9)),(Bloco,(12,0)),(Bloco,(12,9)),(Bloco,(12,10)),(Bloco,(12,11)),(Bloco,(12,12)),(Bloco,(12,13)),(Bloco,(13,0)),(Bloco,(13,13)),(Bloco,(14,0)),(Bloco,(14,9)),(Bloco,(14,10)),(Bloco,(14,11)),(Bloco,(14,12)),(Bloco,(14,13)),(Bloco,(15,0)),(Bloco,(15,10)),(Bloco,(15,11)),(Bloco,(15,12)),(Bloco,(16,0)),(Bloco,(16,12)),(Bloco,(17,0)),(Bloco,(17,12)),(Bloco,(18,0)),(Bloco,(18,12)),(Bloco,(19,0)),(Caixa,(19,11)),(Bloco,(19,12)),(Bloco,(20,0)),(Caixa,(20,10)),(Caixa,(20,11)), (Bloco,(20,12)),(Bloco,(21,0)),(Caixa,(21,9)),(Caixa,(21,10)),(Caixa,(21,11)),(Bloco,(21,12)), (Bloco,(22,1)),(Bloco,(22,2)),(Bloco,(22,3)),(Bloco,(22,4)),(Bloco,(22,5)),(Bloco,(22,6)), (Bloco,(22,7)),(Bloco,(22,8)),(Bloco,(22,9)),(Bloco,(22,10)),(Bloco,(22,11)),(Bloco,(22,12))]
                       jogador = Jogador (12, 8) Oeste False

initWorld 4 = (Controlador Jogar, Jogo mapa jogador, 4, 0)
                 where mapa = constroiMapa  [(Bloco,(0,8)),(Bloco,(1,8)),(Bloco,(2,8)),(Bloco,(3,8)),(Bloco,(4,8)),(Bloco,(4,9)),(Bloco,(4,10)),(Bloco,(4,11)),(Bloco,(5,11)),(Bloco,(6,11)),(Bloco,(6,10)),(Bloco,(6,9)),(Bloco,(6,8)),(Bloco,(6,7)),(Bloco,(6,6)),(Bloco,(7,10)),(Bloco,(8,10)),(Bloco,(9,10)),(Bloco,(10,10)),(Bloco,(10,9)),(Bloco,(11,10)),(Bloco,(12,10)),(Bloco,(12,9)),(Bloco,(13,10)),(Bloco,(14,10)),(Bloco,(15,10)),(Bloco,(15,9)),(Bloco,(15,8)),(Bloco,(16,8)),(Bloco,(17,8)),(Bloco,(17,7)),(Bloco,(17,6)),(Bloco,(18,7)),(Bloco,(19,7)),(Bloco,(20,7)),(Bloco,(21,7)),(Bloco,(21,6)),(Bloco,(21,5)),(Bloco,(22,5)),(Bloco,(23,5)),(Bloco,(23,4)),(Bloco,(23,3)),(Bloco,(23,2)),(Bloco,(23,1)),(Bloco,(23,0)),(Bloco,(0,7)),(Bloco,(0,6)),(Bloco,(0,5)),(Bloco,(0,4)),(Bloco,(0,3)),(Bloco,(0,2)),(Bloco,(0,1)),(Bloco,(0,0)),(Porta,(1,7)),(Caixa,(22,3)),(Caixa,(22,4)),(Caixa,(21,4)),(Caixa,(13,9)),(Caixa,(12,8)),(Caixa,(8,9)),(Caixa,(8,8)),(Caixa,(8,7))]
                       jogador = Jogador (18,6) Este False

initWorld 5 = (Controlador Jogar, Jogo mapa jogador,1,0)
                 where mapa = constroiMapa [(Bloco,(1,1)),(Bloco,(2,1)),(Bloco,(3,1)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(7,4)),(Bloco,(8,4)),(Bloco,(0,2)),(Bloco,(0,1)),(Porta,(1,0)),(Caixa,(6,3)),(Caixa,(7,3)),(Caixa,(8,3)),(Caixa,(8,2))]
                       jogador = Jogador (2,3) Oeste False

-- | Na funcao 'window' é onde temos a opcao de correr o jogo numa janela ou em fullscreen
window :: Display
window = FullScreen  --InWindow "Block Dude" (720, 720) (0, 0) FullScreen 

-- | A funcap 'fr' da-nos o frame rate do jogo
fr :: Int
fr = 25

-- | A funcao 'main' é onde importamos as imagens e onde é chamado todo o resto do codigo
main :: IO ()
main = do 
  blocoImg    <- loadBMP "imagens/bloco.bmp"
  portaImg    <- loadBMP "imagens/porta.bmp"
  caixaImg    <- loadBMP "imagens/caixa.bmp"
  esquerdaImg <- loadBMP "imagens/esquerda.bmp"
  direitaImg  <- loadBMP "imagens/direita.bmp"
  playIO window black fr (Controlador Jogar, Jogo [] (Jogador (0,0) Este False), 0, 0) (`drawWorld` [blocoImg, portaImg, caixaImg, esquerdaImg, direitaImg]) eventStep timeStep
