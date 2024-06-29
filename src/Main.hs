module Main where

import LI12223
import Tarefa1_2022li1g034
import Tarefa2_2022li1g034
import Tarefa3_2022li1g034
import Tarefa4_2022li1g034
import Tarefa5_2022li1g034
import Tarefa6_2022li1g034

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--Invoca o jogo no terminal
main :: IO ()
main = do
  images <- carregarImagens
  play window blue fr (initialState images) drawState event time
