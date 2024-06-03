{- |
Module      : Tarefa2_2022li1g034
Description : Geração contínua de um mapa
Copyright   : Edgar Ferreira <a99890@alunos.uminho.pt>
              Oleksii Tantsura <a102131@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g034 where

import LI12223

import System.Random

{-| 
==Função Principal
A função 'estendeMapa', que recebe um __Mapa__ e retorna __Mapa__, é composta pela conjunção de funções auxiliares, que deve gerar e adicionar uma linha valida ao topo do mapa recebido. 
Basicamente, a funcao aceita o seed que e usado para generar o terreno valido. depois usando aquele terreno genera os obstaculos validos. Usando estes dados construe a nova mapa valida. 
a funcao auxiliar forLoop e usada para generar a lista valida dos obstaculos.
A função é assim definida por : 

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa width []) randomNumber= 
    let 
        velocidadeArr = [1, -1, 2, -2]
        terrenoGenerated = (proximosTerrenosValidos (Mapa width []))!!(mod randomNumber 3)
        finalTerreno = (if(terrenoGenerated == Rio 0) then Rio (velocidadeArr !! (mod randomNumber 4)) else if (terrenoGenerated == Estrada 0) then Estrada (velocidadeArr !! (mod randomNumber 4)) else Relva)
        randomList = geraListaAleatorios randomNumber width
        forLoop :: Int -> Int -> [Obstaculo] -> [Obstaculo]
        forLoop i j accum | i<j = (forLoop (i+1) j (accum ++ [chosenObstacle]))
                          | otherwise = accum
            where
                obstacleOptions = proximosObstaculosValidos width (finalTerreno, accum)
                chosenObstacle = obstacleOptions !! (if (length obstacleOptions) == 2 then mod (randomList !! i) 2 else mod (randomList !! i) 1)
        finalObstaculos = forLoop 0 width []
    in
        (Mapa width [(finalTerreno, finalObstaculos)])
estendeMapa (Mapa width (x:xs)) randomNumber = 
    let 
        velocidadeArr = [1, -1, 2, -2]
        terrenoGenerated = (proximosTerrenosValidos (Mapa width (x:xs))) !! (if length ((proximosTerrenosValidos (Mapa width (x:xs)))) == 3 then mod randomNumber 3 else mod randomNumber 2)
        finalTerreno = (if(terrenoGenerated == Rio 0) then Rio (velocidadeArr !! (mod randomNumber 4)) else if (terrenoGenerated == Estrada 0) then Estrada (velocidadeArr !! (mod randomNumber 4)) else Relva)
        randomList = geraListaAleatorios randomNumber width
        forLoop :: Int -> Int -> [Obstaculo] -> [Obstaculo]
        forLoop i j accum | i<j = (forLoop (i+1) j (accum ++ [chosenObstacle]))
                          | otherwise = accum
            where
                obstacleOptions = proximosObstaculosValidos width (finalTerreno, accum)
                chosenObstacle = obstacleOptions !! (if (length obstacleOptions) == 2 then mod (randomList !! i) 2 else mod (randomList !! i) 1)
        finalObstaculos = forLoop 0 width []
    in
        (Mapa width ([(finalTerreno, finalObstaculos)]++(x:xs)))
@
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa width []) randomNumber= 
    let 
        velocidadeArr = [1, -1, 2, -2]
        terrenoGenerated = (proximosTerrenosValidos (Mapa width []))!!(mod randomNumber 3)
        finalTerreno = (if(terrenoGenerated == Rio 0) then Rio (velocidadeArr !! (mod randomNumber 4)) else if (terrenoGenerated == Estrada 0) then Estrada (velocidadeArr !! (mod randomNumber 4)) else Relva)
        randomList = geraListaAleatorios randomNumber width
        forLoop :: Int -> Int -> [Obstaculo] -> [Obstaculo]
        forLoop i j accum | i<j = (forLoop (i+1) j (accum ++ [chosenObstacle]))
                          | otherwise = accum
            where
                obstacleOptions = proximosObstaculosValidos width (finalTerreno, accum)
                chosenObstacle = obstacleOptions !! (if (length obstacleOptions) == 2 then mod (randomList !! i) 2 else mod (randomList !! i) 1)
        finalObstaculos = forLoop 0 width []
    in
        (Mapa width [(finalTerreno, finalObstaculos)])
estendeMapa (Mapa width (x:xs)) randomNumber = 
    let 
        velocidadeArr = [1, -1, 2, -2]
        terrenoGenerated = (proximosTerrenosValidos (Mapa width (x:xs))) !! (if length ((proximosTerrenosValidos (Mapa width (x:xs)))) == 3 then mod randomNumber 3 else mod randomNumber 2)
        finalTerreno = (if(terrenoGenerated == Rio 0) then Rio (velocidadeArr !! (mod randomNumber 4)) else if (terrenoGenerated == Estrada 0) then Estrada (velocidadeArr !! (mod randomNumber 4)) else Relva)
        randomList = geraListaAleatorios randomNumber width
        forLoop :: Int -> Int -> [Obstaculo] -> [Obstaculo]
        forLoop i j accum | i<j = (forLoop (i+1) j (accum ++ [chosenObstacle]))
                          | otherwise = accum
            where
                obstacleOptions = proximosObstaculosValidos width (finalTerreno, accum)
                chosenObstacle = obstacleOptions !! (if (length obstacleOptions) == 2 then mod (randomList !! i) 2 else mod (randomList !! i) 1)
        finalObstaculos = forLoop 0 width []
    in
        (Mapa width ([(finalTerreno, finalObstaculos)]++(x:xs)))

{-|
==Função Auxiliar
'geraListaAleatorios' gera uma lista de aleatórios de acordo com a semente e o comprimento que pretende.  
-}

geraListaAleatorios :: Int -> Int -> [Int]
geraListaAleatorios s c = take c $ randoms (mkStdGen s)

{-| 
==Função Auxiliar
'proximosTerrenosValidos' é uma função auxiliar que retorna a lista dos proximos terrenos validos (possiveis) para a mapa dada.
-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Rio _, _) : (Rio _, _) : (Rio _, _) : (Rio _, _) : _)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):_)) = [Estrada 0, Rio 0]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):_)) = [Relva, Rio 0]
proximosTerrenosValidos (Mapa _ _) = [Estrada 0, Relva, Rio 0]


{-| 
==Função Auxiliar
'proximosObstaculosValidos' é uma função auxiliar que retorna a lista de obstaculos validos para o terreno dado.
-}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos largura (Relva, []) = [Nenhum, Arvore]
proximosObstaculosValidos largura (Relva, (x:xs)) | length (x:xs) == (largura-1) = if(elem Nenhum (x:xs)) then [Nenhum, Arvore] else [Nenhum]
                                                  | length (x:xs) < largura = [Nenhum, Arvore]
                                                  | otherwise = []
proximosObstaculosValidos largura (Rio _, []) = [Nenhum, Tronco]
proximosObstaculosValidos largura (Rio v, (x:xs)) |length (x:xs) == largura - 1 && x==Tronco = if (obsSeguidos 0 Tronco ((x:xs)++(obsIniciais (x:xs))))>=5 then [Nenhum] else [Nenhum,Tronco]
                                                  |length (x:xs) ==largura -1 && (elem Tronco (x:xs)==False) = [Tronco]
                                                  |length (x:xs) < largura = if obsSeguidos 0 Tronco (x:xs)>5 then [Nenhum] else [Nenhum,Tronco]
                                                  | otherwise = []

proximosObstaculosValidos largura (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos largura (Estrada _, (x:xs)) |length (x:xs) == largura - 1 && x==Carro = if (obsSeguidos 0 Carro ((x:xs)++(obsIniciais (x:xs))))>=3 then [Nenhum] else [Nenhum,Tronco]
                                                      |length (x:xs) ==largura - 1 && (elem Nenhum (x:xs)==False) = [Nenhum]
                                                      |length (x:xs) < largura = if obsSeguidos 0 Carro (x:xs)>3 then [Nenhum] else [Nenhum,Carro]
                                                      |otherwise = []

{-| 
==Função Auxiliar
'obsIniciais' é uma função auxiliar que cria uma lista com os obstaculos iniciais iguais.
-}

obsIniciais :: [Obstaculo]->[Obstaculo]
obsIniciais [] = []
obsIniciais [x] = [x]
obsIniciais (x:xs) |x==head xs = x:obsIniciais xs
                   |otherwise = [x]

{-| 
==Função Auxiliar
'obsSeguidos' é uma função auxiliar que diz quantos obstaculos seguidos no tem no final da lista.
-}

obsSeguidos :: Int -> Obstaculo -> [Obstaculo] -> Int
obsSeguidos _ _ [] = 0
obsSeguidos n obs (x:xs) |obs==last (x:xs) = obsSeguidos (n+1) obs (init (x:xs))
                         |otherwise = n