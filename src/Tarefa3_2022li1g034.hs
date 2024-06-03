{- |
Module      : Tarefa3_2022li1g034
Description : Movimentação do personagem e obstáculos
Copyright   : Edgar Ferreira <a99890@alunos.uminho.pt>
              Oleksii Tantsura <a102131@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g034 where

import LI12223

{-| 
==Função Principal
'animaJogo' é a função principal pedida nesta Tarefa e pretende animar o Jogo, ou seja, o Jogador e o Mapa, consoante a respetiva __Jogada__ e lista de __Obstaculo__ apresentados no mapa. 

Constituída por funções auxiliares, a função principal é definida por:

@
animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) Parado = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0))
                                                        where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Direita) | l <= (x+1) = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0))
                                                               | posArvore (h:t) (x+1,y) 0 = Jogo (Jogador (x,y)) (Mapa l (movObs (h:t) (x,y) 0 0))  
                                                               | otherwise = Jogo jogador2 (Mapa l (movObs (h:t) (x+1,y) 0 0))
                                                                 where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)  
                                                                       jogador2 = (paradoTronco (Jogo (Jogador (x+1,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Esquerda) | x<=0 = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0))
                                                                | posArvore (h:t) (x-1,y) 0 = Jogo (Jogador (x,y)) (Mapa l (movObs (h:t) (x-1,y) 0 0)) 
                                                                | otherwise = Jogo jogador2 (Mapa l (movObs (h:t) (x-1,y) 0 0))
                                                                  where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)
                                                                        jogador2 = (paradoTronco (Jogo (Jogador (x-1,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Baixo) | y>=(length (h:t))-1= Jogo (Jogador (x,y)) (Mapa l (movObs (h:t) (x,y) 0 0))
                                                             | posArvore (h:t) (x,y+1) 0 = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0)) 
                                                             | otherwise = Jogo (Jogador (x,y+1)) (Mapa l (movObs (h:t) (x,y) 0 0))
                                                              where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Cima) | y<=0 = Jogo (Jogador (x,y)) (Mapa l (movObs(h:t) (x,y) 0 0))
                                                            | posArvore (h:t) (x,y-1) 0 = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0)) 
                                                            | otherwise = Jogo (Jogador (x,y-1)) (Mapa l (movObs (h:t) (x,y) 0 0))
                                                             where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)
                                                              
animaJogo (Jogo (Jogador (x,y)) (Mapa l [])) _ = Jogo (Jogador (x,y)) (Mapa l [])
@

-}
animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) Parado = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0))
                                                        where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Direita) | l <= (x+1) = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0))
                                                               | posArvore (h:t) (x+1,y) 0 = Jogo (Jogador (x,y)) (Mapa l (movObs (h:t) (x,y) 0 0))  
                                                               | otherwise = Jogo jogador2 (Mapa l (movObs (h:t) (x+1,y) 0 0))
                                                                 where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)  
                                                                       jogador2 = (paradoTronco (Jogo (Jogador (x+1,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Esquerda) | x<=0 = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0))
                                                                | posArvore (h:t) (x-1,y) 0 = Jogo (Jogador (x,y)) (Mapa l (movObs (h:t) (x-1,y) 0 0)) 
                                                                | otherwise = Jogo jogador2 (Mapa l (movObs (h:t) (x-1,y) 0 0))
                                                                  where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)
                                                                        jogador2 = (paradoTronco (Jogo (Jogador (x-1,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Baixo) | y>=(length (h:t))-1= Jogo (Jogador (x,y)) (Mapa l (movObs (h:t) (x,y) 0 0))
                                                             | posArvore (h:t) (x,y+1) 0 = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0)) 
                                                             | otherwise = Jogo (Jogador (x,y+1)) (Mapa l (movObs (h:t) (x,y) 0 0))
                                                              where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)

animaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Cima) | y<=0 = Jogo (Jogador (x,y)) (Mapa l (movObs(h:t) (x,y) 0 0))
                                                            | posArvore (h:t) (x,y-1) 0 = Jogo jogador1 (Mapa l (movObs (h:t) (x,y) 0 0)) 
                                                            | otherwise = Jogo (Jogador (x,y-1)) (Mapa l (movObs (h:t) (x,y) 0 0))
                                                             where jogador1 = (paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) 0)
                                                              
animaJogo (Jogo (Jogador (x,y)) (Mapa l [])) _ = Jogo (Jogador (x,y)) (Mapa l [])
                                                
{-|
==Função Auxiliar 
'movObs' é uma função auxiliar que gere o movimento dos __Obstáculos__ consoante a velocidade do terreno, seja ela positiva ou negativa, ou inexistente no caso da Relva.
-} 

movObs :: [(Terreno,[Obstaculo])] -> (Int,Int) -> Int -> Int -> [(Terreno, [Obstaculo])]
movObs [] _ _ _= []
movObs ((Rio v, obs):t) jogador n n2| (abs v)==n = (Rio v, obs) : movObs t jogador 0 (n2+1)
                                    | v < 0 = movObs ((Rio v,(tail obs ++ [head obs])):t) jogador (n+1) n2
                                    | otherwise = movObs ((Rio v,([last obs] ++ init obs)):t) jogador (n+1) n2 
movObs ((Estrada v, obs):t) jogador@(x,y) n n2|n2==y && atropelado (Estrada v, obs) x = (Estrada v, obs) : movObs t jogador 0 (n2+1)
                                              |(abs v)==n = (Estrada v, obs) : movObs t jogador 0 (n2+1)  
                                              |v < 0 = movObs ((Estrada v,(tail obs ++ [head obs])):t) jogador (n+1) n2  
                                              |otherwise = movObs ((Estrada v,([last obs] ++ init obs)):t) jogador (n+1) n2
movObs ((Relva, obs):t) jogador n n2= (Relva, obs) : movObs t jogador 0 (n2+1)

{-|
==Função Auxiliar 
'paradoTronco' é uma função auxiliar que permite o movimento do jogador enquanto este se encontra __Parado__ na mesma localização de um tronco, ou seja, segue o movimento do tronco, dependendo da __velocidade__ do terreno e __posição__ do jogador. Também foi utilizado para movimentos laterais, para a soma de vetores da velocidade e jogada, e para movimentos verticais, caso se encontre num rio e queira se movimentar para a mesma posição de uma árvore.
-}

paradoTronco :: Jogo -> Int -> Jogador 
paradoTronco (Jogo (Jogador (x,y)) (Mapa l [])) n = Jogador (x,y)
paradoTronco (Jogo (Jogador (x,y)) (Mapa l ((Rio v, obs):t))) n |n==y && ((!!) obs x == Tronco) && (x+v)>=l = (Jogador (l-1,y))
                                                                |n==y && ((!!) obs x == Tronco) && (x+v)<=0 = (Jogador (0,y))
                                                                |n==y && ((!!) obs x == Tronco) = (Jogador (x+v,y))
                                                                |otherwise = paradoTronco (Jogo (Jogador (x,y)) (Mapa l t)) (n+1)

paradoTronco (Jogo (Jogador (x,y)) (Mapa l (h:t))) n | n<y = paradoTronco (Jogo (Jogador (x,y)) (Mapa l t)) (n+1) 
                                                     | otherwise = Jogador (x,y) 

{-|
==Função Auxiliar
'posArvore' é uma função auxiliar que verifica se a posição que o jogador se encontra é igual à posição duma __Arvore__. 
-}

posArvore :: [(Terreno, [Obstaculo])] -> (Int,Int) -> Int -> Bool
posArvore [] (_,_) _ = False  
posArvore (((Relva,obs):t)) (x,y) n | n==y && ((!!) obs x == Arvore) = True 
                                    | n==y = False 
                                    | n>y = False
                                    | otherwise = posArvore t (x,y) (n+1)
posArvore (h:t) (x,y) n | n < y = posArvore t (x,y) (n+1)
                        | otherwise = False                                         

{-|
==Função Auxiliar
'atropelado' é uma função auxiliar que verifica se a posição pedida apresenta um __Carro__. 
-}

atropelado :: (Terreno, [Obstaculo]) -> Int -> Bool
atropelado (Estrada v,obs) x = ((!!) obs x) == Carro    
                               
atropelado h x = False    

{-| 
==Função Principal
'animaJogador' é uma função derivada da 'animaJogo',em que anima apenas os movimentos do __Jogador__. 
-}

animaJogador :: Jogo -> Jogada -> Jogo 
animaJogador jogo Parado = jogo

animaJogador jogo@(Jogo j@(Jogador (x,y)) mapa@(Mapa l (h:t))) (Move Direita) | l <= (x+1) = jogo
                                                                              | posArvore (h:t) (x+1,y) 0 = jogo
                                                                              | otherwise = Jogo (Jogador (x+1,y)) mapa

animaJogador jogo@(Jogo j@(Jogador (x,y)) mapa@(Mapa l (h:t))) (Move Esquerda) | x<=0 = jogo
                                                                               | posArvore (h:t) (x-1,y) 0 = jogo
                                                                               | otherwise = Jogo (Jogador (x-1,y)) mapa

animaJogador jogo@(Jogo j@(Jogador (x,y)) mapa@(Mapa l (h:t))) (Move Baixo) | y>=length (h:t)-1= jogo
                                                                            | posArvore (h:t) (x,y+1) 0 = jogo 
                                                                            | otherwise = Jogo (Jogador (x,y+1)) mapa

animaJogador jogo@(Jogo j@(Jogador (x,y)) mapa@(Mapa l (h:t))) (Move Cima) | y<=0 = jogo
                                                                           | posArvore (h:t) (x,y-1) 0 = jogo
                                                                           | otherwise = Jogo (Jogador (x,y-1)) mapa
                                                               
                                                              
animaJogador jogo _ = jogo
