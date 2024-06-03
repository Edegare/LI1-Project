{- |
Module      : Tarefa5_2022li1g034
Description : Deslizamento do mapa
Copyright   : Edgar Ferreira <a99890@alunos.uminho.pt>
              Oleksii Tantsura <a102131@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}

module Tarefa5_2022li1g034 where 

import LI12223
import Tarefa1_2022li1g034
import Tarefa2_2022li1g034
import System.Random
{-|
==Função principal
'deslizaJogo' é uma função principal da Tarefa 5 que "desliza" o jogo uma linha, ou seja, cria um novo mapa, retirando a linha final de e acrescentando uma ao topo da lista de ('Terreno', ['Obstaculo']). 
-}
deslizaJogo :: Jogo -> Jogo
deslizaJogo (Jogo (Jogador (x,y)) (Mapa l [])) = Jogo (Jogador (x,y)) (Mapa l [])
deslizaJogo jogo@(Jogo (Jogador (x,y)) (Mapa l (h:t))) = deslizaJogoValido jogo 1

{-|
==Função auxiliar
'headMapa' é uma função auxiliar que recebe um mapa e entrega a cabeça da lista de ('Terreno', ['Obstaculo']).
-}    
headMapa :: Mapa -> (Terreno,[Obstaculo])
headMapa (Mapa _ []) = error "Não existe"
headMapa (Mapa l (h:t)) = h

{-|
==Função auxiliar
'deslizaJogoValido' é uma função auxiliar que recebe um jogo e um inteiro, criando o mecanismo de deslize do mapa em 'deslizaJogo', tendo em conta se o mapa é valido e possível de ultrapassar, com ajuda das funções anteriormente feitas 'estendeMapa', 'mapaValido' e 'validaPassagem'.
-}    
deslizaJogoValido :: Jogo -> Int -> Jogo
deslizaJogoValido (Jogo (Jogador (x,y)) (Mapa l [])) _ =  Jogo (Jogador (x,y)) (Mapa l [])
deslizaJogoValido (Jogo (Jogador (x,y)) (Mapa l (h:t))) n = if mapaValido (estendeMapa (Mapa l (h:t)) (x+(y+1)+n)) && validaPassagem (Mapa l [newLine1,h])
    then Jogo (Jogador (x,y+1)) (Mapa l (newLine1:h:(init t))) 
    else deslizaJogoValido (Jogo (Jogador (x,y)) (Mapa l (h:t))) (n+1)
        where 
            newLine1 = headMapa (estendeMapa (Mapa l (h:t)) (x+(y+1)+n)) 

