module Tarefa2_2022li1g034_Spec where

import LI12223
import Tarefa2_2022li1g034
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1 (sem outros terrenos)" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 5 []),
                                              "Teste 2 (5 troncos no rio 3 nenhum)" ~: [Nenhum, Tronco] ~=? proximosObstaculosValidos 10 (Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
                                              "Teste 3 (5 troncos no rio 4 nenhum)" ~: [Nenhum] ~=? proximosObstaculosValidos 10 (Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum, Nenhum]),
                                              "Teste 4 (sem espaco para obstaculos)" ~: [] ~=? proximosObstaculosValidos 5 (Estrada 1, [Carro, Carro, Nenhum, Carro, Nenhum]),
                                              "Teste 5 (sem terrenos anteriores)" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 5 []),
                                              "Teste 6 (com 4 rios consecutivos)" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 1 [(Rio 1, [Nenhum]), (Rio 1, [Nenhum]), (Rio 1, [Tronco]), (Rio 1, [Tronco]), (Relva, [Nenhum])]),
                                              "Teste 7 (casos nao particulares)" ~: [Estrada 0, Relva, Rio 0] ~=? proximosTerrenosValidos (Mapa 1 [(Relva, [Nenhum]), (Relva, [Nenhum]), (Rio 1, [Tronco])]) ] 
