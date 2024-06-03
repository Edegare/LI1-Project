module Tarefa1_2022li1g034_Spec where

import LI12223
import Tarefa1_2022li1g034
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1 (obstáculos improprios na Relva)" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Tronco, Nenhum, Arvore, Nenhum,Arvore])]),
                                              "Teste 2 (obstáculos improprios no Rio)" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum,Arvore]),(Rio 1, [Tronco, Carro,Nenhum,Nenhum,Nenhum])]),
                                              "Teste 3 (obstáculos improprios no Estrada)" ~: False ~=? mapaValido (Mapa 5 [(Estrada 2, [Arvore, Nenhum, Carro, Nenhum,Carro]),(Rio 1, [Tronco, Nenhum,Nenhum,Nenhum,Nenhum])]),
                                              "Teste 4 (exemplo simples de mapa válido)" ~: True ~=? mapaValido (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum, Carro,Carro])]),
                                              "Teste 5 (rios contiguos com a mesma velocidade)" ~: False ~=? mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum])]),
                                              "Teste 6 (carros seguidos)" ~: False ~=? mapaValido (Mapa 5 [(Rio (-1), [Tronco, Nenhum, Nenhum,Tronco,Nenhum]), (Estrada 1, [Carro, Nenhum, Carro, Carro,Carro])]),
                                              "Teste 7 (troncos seguidos)" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]), (Rio 2, [Tronco, Tronco, Tronco,Tronco,Tronco])]), 
                                              "Teste 8 (largura)" ~: False ~=? mapaValido (Mapa 5 [(Rio (-1), [Tronco, Nenhum,Tronco,Nenhum])]),
                                              "Teste 9 (mais de 4 rios seguidos)" ~: False ~=? mapaValido (Mapa 5 [(Rio (-1), [Tronco, Nenhum, Nenhum,Tronco,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Rio 1, [Tronco, Nenhum, Nenhum,Tronco,Tronco]),(Rio 1, [Tronco, Nenhum, Nenhum,Tronco,Tronco])]),
                                              "Teste 10 (mais de 5 estradas seguidas)" ~: False ~=? mapaValido (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum, Carro,Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro,Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro,Carro]),(Estrada 1, [Nenhum, Carro, Nenhum, Carro,Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro,Carro]),(Estrada 2, [Nenhum, Nenhum, Nenhum, Carro,Carro])]),
                                              "Teste 11 (mais de 5 relvas seguidas)" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum,Nenhum]),(Relva, [Arvore, Nenhum, Arvore, Nenhum,Arvore]),(Relva, [Arvore, Arvore, Arvore, Nenhum,Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum,Arvore]),(Relva, [Arvore, Nenhum, Arvore, Nenhum,Arvore]),(Relva, [Nenhum, Nenhum, Arvore, Nenhum,Arvore])]),
                                              "Teste 12 (exemlpo de mapa valido 1)" ~: True ~=? mapaValido (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Nenhum,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])]),
                                              "Teste 13 (exemplo de mapa valido 2)" ~: True ~=? mapaValido (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum, Carro,Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro,Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro,Carro]),(Estrada 1, [Nenhum, Carro, Nenhum, Carro,Carro]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco,Tronco]),(Estrada 2, [Nenhum, Nenhum, Nenhum, Carro,Carro])])] 
