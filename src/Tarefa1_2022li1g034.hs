{- |
Module      : Tarefa1_2022li1g034
Description : Validação de um mapa
Copyright   : Edgar Ferreira <a99890@alunos.uminho.pt>
              Oleksii Tantsura <a102131@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}

module Tarefa1_2022li1g034 where

import LI12223


{-| 
==Função Principal
A função 'mapaValido', que recebe um __Mapa__ e retorna __Bool__, é composta pela conjunção de funções auxiliares, que a permitem validar o mapa recebido. 
Basicamente, se todas as funções auxiliares validarem o mapa recebido, este é válido e pode ser usado no jogo. 

A função é assim definida por : 

@
mapaValido (Mapa l []) = True  
mapaValido (Mapa l t) = obsImproprios (Mapa l t) && 
                        veloRios (Mapa l t) && 
                        troncosSeguidos (Mapa l t) &&
                        carrosSeguidos (Mapa l t) &&
                        elemNenhum (Mapa l t) &&
                        larguraObs (Mapa l t) &&
                        terrSeguidos (Mapa l t)
@
-}

mapaValido :: Mapa -> Bool
mapaValido (Mapa l []) = True  
mapaValido (Mapa l t) = obsImproprios (Mapa l t) && 
                        veloRios (Mapa l t) && 
                        troncosSeguidos (Mapa l t) &&
                        carrosSeguidos (Mapa l t) &&
                        elemNenhum (Mapa l t) &&
                        larguraObs (Mapa l t) &&
                        terrSeguidos (Mapa l t)

{-| 
==Função Auxiliar
'obsImproprios' é uma função auxiliar que verifica se existem obstáculos em terrenos impróprios, e.g. troncos em estradas ou relvas, árvores em rios ou estradas, etc.
Caso exista é um Mapa __inválido__.
-}

obsImproprios :: Mapa -> Bool 
obsImproprios (Mapa l []) = True
obsImproprios (Mapa l ((Relva,obs):t)) |elem Tronco obs || elem Carro obs = False  
                                       |otherwise = obsImproprios (Mapa l t)
obsImproprios (Mapa l ((Estrada _, obs):t)) |elem Tronco obs || elem Arvore obs = False
                                            |otherwise = obsImproprios (Mapa l t)
obsImproprios (Mapa l ((Rio _, obs):t)) |elem Arvore obs || elem Carro obs = False 
                                        |otherwise = obsImproprios (Mapa l t)

{-| 
==Função Auxiliar
'veloRios' é uma função auxiliar que verifica se os rios contíguos têm velociades __opostas__.
Se rios contíguos tiverem velociades paralelas (ambas positivas ou ambas negativas) o Mapa é __inválido__. 
-}

veloRios :: Mapa -> Bool 
veloRios (Mapa l []) = True 
veloRios (Mapa l ((Rio v1, l1):(Rio v2, l2):t)) = (v1<0 && v2>0 || v1>0 && v2<0) 
                                                     && veloRios (Mapa l ((Rio v2, l2):t))
veloRios (Mapa l ((_,_):t)) = veloRios (Mapa l t)            

{-| 
==Função Auxiliar
'troncosSeguidos' é uma função auxiliar que verifica se os __troncos__ têm, no máximo, 5 unidades de comprimento.
Caso haja mais de 5 __troncos__ contíguos numa linha circular, o Mapa é __inválido__. 
-}
           
troncosSeguidos :: Mapa -> Bool
troncosSeguidos (Mapa l []) = True  
troncosSeguidos (Mapa l ((Rio _,[Tronco]):t)) = False 
troncosSeguidos (Mapa l ((Rio _,[Tronco,Tronco]):t)) = False  
troncosSeguidos (Mapa l ((Rio _,ob:obs):t)) |ob==Tronco && last obs == Tronco = troncosSeguidos' ((ob:obs) ++ (ob:obs)) 1 && troncosSeguidos (Mapa l t)
                                            |otherwise = troncosSeguidos' (ob:obs) 1 && troncosSeguidos (Mapa l t)
troncosSeguidos (Mapa l (h:t)) = troncosSeguidos (Mapa l t)  

{-| 
==Função Auxiliar da Auxiliar 
'troncosSeguidos'' é uma função auxiliar da função 'troncosSeguidos'.
-}

troncosSeguidos' :: [Obstaculo] -> Int -> Bool
troncosSeguidos' [] n = True
troncosSeguidos' (Tronco:obs) n | n>5 = False 
                                | (Tronco:obs)==[Tronco] = True
                                | Tronco==head obs = troncosSeguidos' obs (n+1)
                                | otherwise = troncosSeguidos' obs 1
troncosSeguidos' (ob:obs) n = troncosSeguidos' obs n

{-| 
==Função Auxiliar
'carrosSeguidos' é uma função auxiliar que verifica se os __carros__ têm, no máximo, 3 unidades de comprimento.
Caso haja mais de 3 __carros__ contíguos numa linha, o Mapa é __inválido__. 
-}

carrosSeguidos :: Mapa -> Bool
carrosSeguidos (Mapa l []) = True 
carrosSeguidos (Mapa l ((Estrada _,[Carro]):t)) = False  
carrosSeguidos (Mapa l ((Estrada _,ob:obs):t)) |ob==Carro && last obs == Carro = carrosSeguidos' ((ob:obs) ++ (ob:obs)) 1 && carrosSeguidos (Mapa l t) 
                                               |otherwise = carrosSeguidos' (ob:obs) 1 && carrosSeguidos (Mapa l t)
carrosSeguidos (Mapa l (h:t)) = carrosSeguidos (Mapa l t)  

{-| 
==Função Auxiliar da Auxiliar
'carrosSeguidos'' é uma função auxiliar da função 'carrosSeguidos'.
-}

carrosSeguidos' :: [Obstaculo] -> Int -> Bool
carrosSeguidos' [] n = True
carrosSeguidos' (Carro:obs) n  | n>3 = False 
                               | (Carro:obs)==[Carro] = True
                               | Carro==head obs = carrosSeguidos' obs (n+1)
                               | otherwise = carrosSeguidos' obs 1
carrosSeguidos' (ob:obs) n = carrosSeguidos' obs n


{-| 
==Função Auxiliar
'elemNenhum' é uma função auxiliar que verifica a existência de, pelo menos um, "obstáculo" __Nenhum__.
Ou seja, cada linha deve conter um espaço livre. Se isso não acontecer, o Mapa é __inválido__.
-}

elemNenhum :: Mapa -> Bool 
elemNenhum (Mapa l []) = True 
elemNenhum (Mapa l ((_, obs):t)) = elem Nenhum obs && elemNenhum (Mapa l t)

{-| 
==Função Auxiliar
'larguraObs' é uma função auxiliar da função que verifica se o __comprimento da lista de obstáculos__ corresponde exatamente à __largura do Mapa__.
Caso o comprimento da lista seja maior ou menor, o Mapa é __inválido__.  
-}

larguraObs :: Mapa -> Bool 
larguraObs (Mapa l []) = True
larguraObs (Mapa l ((_,obs):t)) = l==length obs && larguraObs (Mapa l t)

{-| 
==Função Auxiliar
'terrSeguidos' é uma função auxiliar que verifica se existem mais de 4 rios contíguos ou mais de 5 estradas contíguas ou 5 relvas contíguas.
Caso exista, o Mapa é __inválido__.
-}

terrSeguidos :: Mapa -> Bool 
terrSeguidos (Mapa l ((ter, obs):t)) = terrSeguidos' ((ter,obs):t) 1
terrSeguidos (Mapa l _) = True

{-| 
==Função Auxiliar da Auxiliar
'terrSeguidos'' é uma função auxiliar da função 'terrSeguidos'.
-}
terrSeguidos' :: [(Terreno, [Obstaculo])] -> Int -> Bool 
terrSeguidos' [] n = True
terrSeguidos' ((Rio v1, obs1):(Rio v2, obs2):t) 4 = False 
terrSeguidos' ((Rio v1, obs1):(Rio v2, obs2):t) n = terrSeguidos' ((Rio v2, obs2):t) (n+1)
terrSeguidos' ((Estrada v1, obs1):(Estrada v2,obs2):t) 5 = False
terrSeguidos' ((Estrada v1, obs1):(Estrada v2,obs2):t) n = terrSeguidos' ((Estrada v2, obs2):t) (n+1)
terrSeguidos' ((Relva, obs1):(Relva,obs2):t) 5 = False                                                    
terrSeguidos' ((Relva, obs1):(Relva,obs2):t) n = terrSeguidos' ((Relva, obs2):t) (n+1)  
terrSeguidos' (h:t) n = terrSeguidos' t 1                                            

{-| 
==Função Auxiliar
'terrSeguidos'' é uma função auxiliar que verifica se existem passagens possíveis de uma linha para outra ao longo do mapa.
-}

validaPassagem :: Mapa -> Bool
validaPassagem (Mapa l []) = True 
validaPassagem (Mapa l ((ter1,xs):[])) = True
validaPassagem (Mapa l ((Rio v1, xs):(Rio v2,ys):t)) = (Tronco, Tronco) `elem` zip xs ys && validaPassagem (Mapa l ((Rio v2,ys):t))
validaPassagem (Mapa l ((Rio v1, xs):(_,ys):t)) = (Tronco, Nenhum) `elem` zip xs ys && validaPassagem (Mapa l ((Relva,ys):t))
validaPassagem (Mapa l ((_, xs):(Rio v2,ys):t)) = (Nenhum, Tronco) `elem` zip xs ys && validaPassagem (Mapa l ((Rio v2,ys):t))
validaPassagem (Mapa l ((ter1,xs):(ter2,ys):t)) = (Nenhum, Nenhum) `elem` zip xs ys && validaPassagem (Mapa l ((ter2,ys):t))