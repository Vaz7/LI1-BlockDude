type Coordenadas = (Int, Int)
data Peca = Bloco | Porta | Caixa | Vazio deriving (Show, Eq)
type Mapa = [[Peca]]

data Direcao = Este | Oeste
data Jogador = Jogador Coordenadas Direcao Bool

data Jogo = Jogo Mapa Jogador


--TAREFA_1
m1 :: [(Peca,Coordenadas)]
m1 = [(Porta,(0,1)), (Bloco,(0,2)), (Bloco,(1,0)), (Bloco,(2,1))]

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l = validaPosicaoAux (validaPosicao l) tx ty  && validaPortaAux (validaPorta l) && validaCaixa l l && validaVazio l tx ty && validaChao l tx ty
                                           where tx = testeTamanhoX (testeTamanho l) 
                                                 ty = testeTamanhoY (testeTamanho l)



--0 codigo que vai decidir qual o maior valor de x e y para um dado mapa 
testeTamanho :: [(Peca,Coordenadas)] -> [Coordenadas]
testeTamanho [] = []
testeTamanho ((p,c):xs) = c:testeTamanho xs

testeTamanhoX :: [Coordenadas] -> Int
testeTamanhoX [] = 0
testeTamanhoX ((a,b):xs) = max a (testeTamanhoX xs)

testeTamanhoY :: [Coordenadas] -> Int
testeTamanhoY [] = 0
testeTamanhoY ((a,b):xs) = max b (testeTamanhoY xs)


--1
validaPosicao :: [(Peca,Coordenadas)] -> [Coordenadas]
validaPosicao [] = []   --Nesta funcao verificamos se alguma coordenada se repete na lista, para isso criamos uma lista so com coordenadas e verificamos se ha repeticoes, alem disso verifica se se há coordenadas com numeros negativos
validaPosicao ((p,c):xs) = c:validaPosicao xs


validaPosicaoAux :: [Coordenadas] -> Int -> Int -> Bool 
validaPosicaoAux [] _ _ = True  
validaPosicaoAux ((x,y):xs) tx ty | x<0 || y<0 = False
                                  | (x,y) `elem` xs = False
                                  | otherwise = validaPosicaoAux xs tx ty

--2
validaPorta :: [(Peca,Coordenadas)] -> [Peca]
validaPorta [] = [] -- atraves da lista original criamos uma lista apenas com as pecas porta, na funcao auxiliar, caso tenha mais que um elemento "porta" dá false, caso contrario da true
validaPorta ((p,c):xs) = if p==Porta then p:validaPorta xs else validaPorta xs 

validaPortaAux :: [Peca] -> Bool
validaPortaAux [] = False
validaPortaAux (x:xs) |x `elem` xs = False
                      |otherwise = True

--3
validaCaixa :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Bool 
validaCaixa _ [] = True
validaCaixa l ((p,(x,y)):xs) | p == Caixa = validaCaixaBlocoAux l x (y+1) && validaCaixa l xs --pegamos na lista original e verificamos, usando a funçao auxiliar se em baixo da caixa tem ou uma caixa ou um bloco, caso tenha uma caixa por baixo da primeira caixa tambem é necessario ver o que ela tem em baixo
                             | otherwise = validaCaixa l xs

validaCaixaBlocoAux :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
validaCaixaBlocoAux [] _ _ = False
validaCaixaBlocoAux ((p,(a,b)):xs) x y | (p==Caixa || p==Bloco) && (a==x && b==y) = True --verificamos se em baixo da caixa tem uma caixa ou um bloco
                                       | otherwise = validaCaixaBlocoAux xs x y 

--4
validaVazio :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
validaVazio l tx ty = validaVazioAux2 l [(x,y) | x <- [0..tx], y <- [0..ty]] --pega em todos os elementos possiveis, um de cada vez e verifica se existem na lista inicial, basta que 1 nao exista para dar true e termos um elemento vazio

validaVazioAux2 :: [(Peca,Coordenadas)] -> [(Int,Int)]-> Bool
validaVazioAux2 _ [] = False
validaVazioAux2 l ((x,y):xs) | validaVazioAux l (x,y) == True = True
                             | otherwise = validaVazioAux2 l xs

validaVazioAux :: [(Peca,Coordenadas)] -> (Int,Int) -> Bool 
validaVazioAux [] _  = True
validaVazioAux ((p,(a,b)):xs) (x,y) | (a,b) == (x,y) = False
                                    | otherwise = validaVazioAux xs (x,y)


--5

validaChao :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
validaChao l tx ty = validaChaoAux l [(x,ty) | x <- [0..tx]]  --Esta funçao gera as coordenadas do chao

validaChaoAux :: [(Peca,Coordenadas)] -> [(Int,Int)]-> Bool
validaChaoAux _ [] = True -- esta funçao vai chamar a aux 2 com a lista inicial e com a coordenada do primeiro elemento do chao
validaChaoAux  l ((a,b):xs) | validaChaoAux2 l a b == False = False--caso ele nao exista na lista inicial como um bloco entao devolve falso
                            | otherwise = validaChaoAux l xs

validaChaoAux2 :: [(Peca, Coordenadas)] -> Int -> Int -> Bool    
validaChaoAux2 [] _ _ = False--aqui fazemos o teste para verificar se uma certa coordenada do chao existe na lista unicial (mapa)
validaChaoAux2 ((p,(a,b)):xs) x y | p==Bloco && (a==x || a == (x+1)) && (b==y || b==(y-1) || b == (y+1)) = True
                                  | otherwise = validaChaoAux2 xs x y                                          




-- haddock -h -o doc/html projeto1.h

--na tarefa 2 é mais facil começar com uma matriz com todos os dados vazios e alterar
