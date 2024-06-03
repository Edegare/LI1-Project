module Tarefa3_2022li1g034_Spec where

import LI12223
import Tarefa3_2022li1g034
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1 (limitado por uma Arvore à direita)" ~: (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Direita),
                                              "Teste 2 (limitado por uma Arvore à esquerda)" ~: (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Esquerda), 
                                              "Teste 3 (limitado por uma Arvore abaixo)" ~: (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Relva, [Arvore,Nenhum,Arvore, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Relva, [Arvore,Nenhum,Arvore, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Baixo),
                                              "Teste 4 (limitado por uma Arvore acima))" ~: (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore] ), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 5 (limite inferior do mapa)" ~: (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Baixo),
                                              "Teste 6 (limite lateral do mapa)" ~: (Jogo (Jogador (4,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Nenhum])])) ~=? animaJogo (Jogo (Jogador (4,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Nenhum])])) (Move Direita),
                                              "Teste 7 (limite superior do mapa)" ~: (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 8 (movimento normal para cima)" ~: (Jogo (Jogador (0,0)) (Mapa 5 [(Estrada 1, [Nenhum, Nenhum, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 5 [(Estrada 1, [Nenhum, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 9 (movimento normal para baixo)" ~: (Jogo (Jogador (3,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Nenhum,Arvore])])) ~=? animaJogo (Jogo (Jogador (3,2)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Nenhum,Arvore])])) (Move Baixo),
                                              "Teste 10 (movimento normal para a direita)" ~: (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Direita),
                                              "Teste 11 (movimento normal para a esquerda)" ~: (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Esquerda),
                                              "Teste 12 (parado num Tronco)" ~: (Jogo (Jogador (1,2)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (3,2)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Parado),
                                              "Teste 13 (parado num Tronco no limite lateral)" ~: (Jogo (Jogador (4,1)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (3,1)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Parado),
                                              "Teste 14 (parado normal)" ~: (Jogo (Jogador (1,0)) (Mapa 5 [(Estrada 1, [Nenhum, Carro, Nenhum,Nenhum,Carro]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) Parado,
                                              "Teste 15 (movimento horizontal para a direita num Tronco)" ~: (Jogo (Jogador (1,0)) (Mapa 5 [(Rio (-1), [Tronco,Tronco,Tronco,Nenhum,Tronco]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Rio (-1), [Tronco, Tronco, Tronco,Tronco,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Direita),
                                              "Teste 16 (movimento horizontal para a esquerda num Tronco)" ~: (Jogo (Jogador (0,0)) (Mapa 5 [(Rio (-1), [Tronco,Tronco,Tronco,Nenhum,Tronco]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Rio (-1), [Tronco, Tronco, Tronco,Tronco,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Esquerda),
                                              "Teste 17 (tentativa vertical para baixo num Tronco com Arvore embaixo)" ~: (Jogo (Jogador (2,2)) (Mapa 5 [(Rio (-1), [Tronco,Tronco,Tronco,Nenhum,Tronco]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-1), [Nenhum, Nenhum, Tronco, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (3,2)) (Mapa 5 [(Rio (-1), [Tronco, Tronco, Tronco,Tronco,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-1), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Baixo),
                                              "Teste 18 (tentativa vertical para cima num Tronco com Arvore encima)" ~: (Jogo (Jogador (2,1)) (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum,Arvore,Nenhum]), (Rio 2, [Tronco,Nenhum,Tronco, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Nenhum, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum,Arvore,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 19 (animaJogador limitado por uma Arvore à direita)" ~: (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Direita),
                                              "Teste 20 (animaJogador limitado por uma Arvore à esquerda)" ~: (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Esquerda), 
                                              "Teste 21 (animaJogador limitado por uma Arvore abaixo)" ~: (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Relva, [Arvore,Nenhum,Arvore, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Relva, [Arvore,Nenhum,Arvore, Nenhum, Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Baixo),
                                              "Teste 22 (animaJogador limitado por uma Arvore acima))" ~: (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 23 (animaJogador limite inferior do mapa)" ~: (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Baixo),
                                              "Teste 24 (animaJogador limite lateral do mapa)" ~: (Jogo (Jogador (4,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Nenhum])])) ~=? animaJogador (Jogo (Jogador (4,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Nenhum])])) (Move Direita),
                                              "Teste 25 (animaJogador limite superior do mapa)" ~: (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 26 (animaJogador movimento normal para cima)" ~: (Jogo (Jogador (0,0)) (Mapa 5 [(Estrada 1, [Nenhum, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (0,1)) (Mapa 5 [(Estrada 1, [Nenhum, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Cima),
                                              "Teste 27 (animaJogador movimento normal para baixo)" ~: (Jogo (Jogador (3,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Nenhum,Arvore])])) ~=? animaJogador (Jogo (Jogador (3,2)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Nenhum,Arvore])])) (Move Baixo),
                                              "Teste 28 (animaJogador movimento normal para a direita)" ~: (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (1,0)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Direita),
                                              "Teste 29 (animaJogador movimento normal para a esquerda)" ~: (Jogo (Jogador (1,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) ~=? animaJogador (Jogo (Jogador (2,3)) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum,Carro,Nenhum]), (Rio 2, [Tronco, Nenhum, Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum, Nenhum, Nenhum,Tronco,Nenhum]), (Relva, [Arvore, Nenhum, Nenhum,Arvore,Arvore])])) (Move Esquerda)]
