{- |
Module      : Tarefa4_2022li1g034
Description : Determinar se o jogo terminou
Copyright   : Edgar Ferreira <a99890@alunos.uminho.pt>
              Oleksii Tantsura <a102131@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g034 where

import LI12223
{-|
==Função Principal
'jogoTerminou' é a função principal pedida nesta Tarefa que indica se o jogador perdeu o jogo, retornando assim um __True__, caso o personangem se encontre fora do mapa, na água, ou atropelado (na mesma posição de um carro).

Constituída por funções auxiliares, a função principal é definida por:

@
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l [])) = True
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l t)) = (x < 0) ||
                                                 (x >= l) || 
                                                 (y < 0) || 
                                                 (y >= length t) || 
                                                 (naAgua t (x,y) 0) ||  
                                                 (atropelado t (x,y) 0) || 
                                                 (emArvore t (x,y) 0)
@
-}
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l [])) = True
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l t)) = x<0 || x>=l || y<0 || y>=length t || naAgua t (x,y) 0 ||  atropelado t (x,y) 0 || emArvore t (x,y) 0 

{-|
==Função Auxiliar
'naAgua' é uma função auxiliar que verifica se a posição que o jogador se encontra é dentro da água do Rio, ou seja, na mesma posição do obstáculo __Nenhum__.
-}

naAgua :: [(Terreno, [Obstaculo])] -> (Int,Int) -> Int -> Bool
naAgua [] (_,_) _ = False 
naAgua (((Rio v,obs):t)) (x,y) n | n==y && ((!!) obs x == Nenhum) = True
                                 | n==y = False 
                                 | otherwise = naAgua t (x,y) (n+1)
naAgua (h:t) (x,y) n | n < y = naAgua t (x,y) (n+1)
                     | otherwise = False 

{-|
==Função Auxiliar
'atropelado' é uma função auxiliar que verifica se a posição que o jogador se encontra é a mesma posição do obstáculo __Carro__, ou seja, verifica se foi atropelado.
-}

atropelado :: [(Terreno, [Obstaculo])] -> (Int,Int) -> Int -> Bool
atropelado [] (_,_) _ = False
atropelado (((Estrada v,obs):t)) (x,y) n | n==y && ((!!) obs x)== Carro = True 
                                         | n==y = False  
                                         | otherwise = atropelado t (x,y) (n+1)
atropelado (h:t) (x,y) n | n < y = atropelado t (x,y) (n+1)
                         | otherwise = False 

{-|
==Função Auxiliar
'emArvore' é uma função auxiliar que verifica se a posição que o jogador se encontra é igual à posição duma __Arvore__.  
-}

emArvore :: [(Terreno, [Obstaculo])] -> (Int,Int) -> Int -> Bool
emArvore [] (_,_) _ = False  
emArvore (((Relva,obs):t)) (x,y) n | n==y && ((!!) obs x == Arvore) = True 
                                   | n==y = False 
                                   | otherwise = emArvore t (x,y) (n+1)
emArvore (h:t) (x,y) n | n < y = emArvore t (x,y) (n+1)
                       | otherwise = False                          