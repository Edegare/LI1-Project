module Tarefa6_2022li1g034 where

import LI12223
import Tarefa1_2022li1g034
import Tarefa2_2022li1g034
import Tarefa3_2022li1g034
import Tarefa4_2022li1g034
import Tarefa5_2022li1g034

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--Opções no menu inicial
data Option = PlayAgain
            | Continue
            | Quit

--Opções no menu de pausa
data OptionsPause = ContinuePlay
                  | Restart
                  | BackToMenu
--Modo jogo (em jogo e em pausa)
data Mode = Play
          | Pause OptionsPause

-- Modo de menu (inicial, jogo, ao perder)
data Menu = Options Option
          | GameMode Mode
          | Lost

--Mundo
type World = (Menu, Jogo, Images, Time, Score)
--Imagens necessárias
data Images =
   Images { playMenu :: Picture
          , continueMenu :: Picture
          , quitMenu :: Picture
          , pauseContinue :: Picture
          , pauseRestart :: Picture
          , pauseQuit :: Picture
          , background :: Picture
          , lost :: Picture
          , paiNatal :: Picture
          , treno :: Picture
          , gelo :: Picture
          , arvore :: Picture
          , trenoEspelhado ::Picture
          , estradaGelo :: Picture
          , rioAgua :: Picture
          , rioAguaEspelhado :: Picture
          }
--Tempo
type Time = Float
--Pontuação
type Score = Int

--Janela de jogo 
window :: Display
window = InWindow "CrossyXmas" (630,810) (600,100)


--frame-rate
fr :: Int
fr = 50

--Jogo inicial 
jogoInicial :: Jogo
jogoInicial = Jogo (Jogador (3, 8)) (Mapa 7 [(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                             (Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                             (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum,Carro, Nenhum]),
                                             (Relva, [Arvore, Nenhum, Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                             (Rio (-1), [Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum]),
                                             (Estrada 1, [Carro,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro]),
                                             (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore]),
                                             (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore]),
                                             (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])])
--Estado inicial
initialState :: Images -> World
initialState images = (Options PlayAgain,jogoInicial,images,0,0)
--Desenho do estado
drawState :: World -> Picture
drawState (Lost, jogo, images, t, score) = Pictures [background images, lost images, Translate (-100) (-80) $ Color black $ scale 0.3 0.3 $ Text ((show score)++" points!"),Translate (-220) (-250) $ Color black $ scale 0.3 0.3 $ Text ("Click 'Enter' to continue") ]
drawState (Options PlayAgain, jogo, images, t, score) = Pictures [background images, playMenu images]
drawState (Options Continue, jogo,images,t,score) = Pictures [background images, continueMenu images]
drawState (Options Quit, jogo,images,t,score) = Pictures [background images, quitMenu images]
drawState (GameMode (Pause ContinuePlay), Jogo (Jogador (x,y)) mapa ,images,t,score) = Pictures [drawMapa mapa images,Translate i j $ (paiNatal images), Translate 270 400 $ Color blue $ rectangleSolid 75 25, Translate 240 390 $ Color white $ Scale 0.1 0.1 $ Text ("Score: " ++ (show score)), pauseContinue images]
   where
      i = -270+(90*(fromIntegral x))
      j = 360-(90*(fromIntegral y))
drawState (GameMode (Pause Restart), Jogo (Jogador (x,y)) mapa ,images,t,score) = Pictures [drawMapa mapa images,Translate i j $ (paiNatal images), Translate 270 400 $ Color blue $ rectangleSolid 75 25, Translate 240 390 $ Color white $ Scale 0.1 0.1 $ Text ("Score: " ++ (show score)), pauseRestart images]
   where
      i = -270+(90*(fromIntegral x))
      j = 360-(90*(fromIntegral y))
drawState (GameMode (Pause BackToMenu), Jogo (Jogador (x,y)) mapa ,images,t,score) = Pictures [drawMapa mapa images,Translate i j $ (paiNatal images), Translate 270 400 $ Color blue $ rectangleSolid 75 25, Translate 240 390 $ Color white $ Scale 0.1 0.1 $ Text ("Score: " ++ (show score)), pauseQuit images]
   where
      i = -270+(90*(fromIntegral x))
      j = 360-(90*(fromIntegral y))
drawState (GameMode Play, Jogo (Jogador (x,y)) mapa,images,t,score) = Pictures $ [drawMapa mapa images,Translate i j $ (paiNatal images),Translate 270 400 $ Color blue $ rectangleSolid 75 25, Translate 240 390 $ Color white $ Scale 0.1 0.1 $ Text ("Score: " ++ (show score))]
   where
      i = -270+(90*(fromIntegral x))
      j = 360-(90*(fromIntegral y))


--Desenho do Mapa 
drawMapa :: Mapa -> Images -> Picture
drawMapa mapa images= pictures $ (drawTerMapa mapa 0 images) ++ (drawObsMapa mapa 0 images)

drawTerMapa :: Mapa -> Float -> Images -> [Picture] --Desenho do terreno
drawTerMapa (Mapa l []) j images = []
drawTerMapa (Mapa l ((ter,obs):t)) j images = (Translate 0 (360-(90*j)) $ pictures (drawTer ter 0 images)): drawTerMapa (Mapa l t) (j+1) images

drawObsMapa :: Mapa -> Float -> Images -> [Picture] --Desenho dos obstáculos
drawObsMapa (Mapa l []) j images= []
drawObsMapa (Mapa l [(ter,obs)]) j images= [Translate 0 (360-(90*j)) $ pictures (drawLine (ter,obs) 0 images)]
drawObsMapa (Mapa l ((ter,obs):t)) j images= (Translate 0 (360-(90*j)) $ pictures (drawLine (ter,obs) 0 images)):drawObsMapa (Mapa l t) (j+1) images

drawLine :: (Terreno,[Obstaculo]) -> Float -> Images -> [Picture]
drawLine (_,[]) i images= []
drawLine (ter,(o:os)) i images= (Translate (-270+(90*i)) 0 $ drawObs (ter,o) images):drawLine (ter,os) (i+1) images

drawObs :: (Terreno, Obstaculo) -> Images -> Picture
drawObs (Relva,Arvore) images= arvore images
drawObs (Estrada v,Carro) images= if v<0 then treno images else trenoEspelhado images
drawObs (Rio v,Tronco) images= gelo images
drawObs (_,Nenhum) images= Blank

drawTer :: Terreno -> Float -> Images -> [Picture]
drawTer _ 7 images = []
drawTer Relva i images= [Color white $ rectangleSolid 630 90]
drawTer (Rio v) i images = if v >0 then (Translate (-270+(90*i)) 0 $ rioAgua images): drawTer (Rio v) (i+1) images else (Translate (-270+(90*i)) 0 $ rioAguaEspelhado images): drawTer (Rio v) (i+1) images
drawTer (Estrada v) i images= (Translate (-270+(90*i)) 0 $ estradaGelo images): drawTer (Estrada v) (i+1) images

-- Reage aos eventos do teclado
-- Teclas in-game
newState :: Key -> World -> World
newState k (m, jogo, images, t, score) = case k of
  (SpecialKey KeyUp) -> if animaJogador jogo (Move Cima) == jogo
   then (m,animaJogador jogo (Move Cima),images,t,score)
   else (m,animaJogador jogo (Move Cima),images,t,score+1)
  (SpecialKey KeyDown) -> if animaJogador jogo (Move Baixo) == jogo
   then (m,animaJogador jogo (Move Baixo),images,t,score)
   else (m,animaJogador jogo (Move Baixo),images,t,score-1)
  (SpecialKey KeyLeft) -> (m,animaJogador jogo (Move Esquerda),images,t,score)
  (SpecialKey KeyRight) -> (m,animaJogador jogo (Move Direita),images,t,score)

event :: Event -> World -> World
-- Menu
-- Play Again
event (EventKey (SpecialKey KeyEnter) Down _ _) (Options PlayAgain, jogo, images, t, score) =
   (GameMode Play, jogoInicial, images, 0, 0)
event (EventKey (SpecialKey KeyUp) Down _ _) (Options PlayAgain, jogo, images, t, score) =
   (Options Quit, jogo, images, t, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Options PlayAgain, jogo, images, t, score) =
   (Options Continue, jogo, images, t, score)
-- Quit
event (EventKey (SpecialKey KeyUp) Down _ _) (Options Quit, jogo, images, t, score) =
   (Options Continue, jogo, images, t, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Options Quit, jogo, images, t, score) =
   (Options PlayAgain, jogo, images, t, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Options Quit, jogo, images, t, score) =
   error "Fim de Jogo"
-- Continue
event (EventKey (SpecialKey KeyEnter) Down _ _) (Options Continue, jogo, images, t, score) =
   (GameMode Play, jogo, images, t, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (Options Continue, jogo, images, t, score) =
   (Options PlayAgain, jogo, images, t, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Options Continue, jogo, images, t, score) =
   (Options Quit, jogo, images, t, score)

-- Perdeu o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Lost, jogo, images, t, score) =
   initialState images

-- Pause
event (EventKey (Char 'p') Down _ _) (GameMode Play, jogo, images, t, score) = (GameMode (Pause ContinuePlay), jogo,images,t,score)

event (EventKey (SpecialKey KeyEnter) Down _ _) (GameMode (Pause ContinuePlay), jogo,images,t,score) = (GameMode Play, jogo, images, t, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (GameMode (Pause ContinuePlay), jogo,images,t,score) = (GameMode (Pause BackToMenu), jogo,images,t,score)
event (EventKey (SpecialKey KeyDown) Down _ _) (GameMode (Pause ContinuePlay), jogo,images,t,score) = (GameMode (Pause Restart), jogo,images,t,score)

event (EventKey (SpecialKey KeyEnter) Down _ _) (GameMode (Pause Restart), jogo,images,t,score) = (GameMode Play, jogoInicial, images, 0, 0)
event (EventKey (SpecialKey KeyUp) Down _ _) (GameMode (Pause Restart), jogo,images,t,score) = (GameMode (Pause ContinuePlay), jogo,images,t,score)
event (EventKey (SpecialKey KeyDown) Down _ _) (GameMode (Pause Restart), jogo,images,t,score) = (GameMode (Pause BackToMenu), jogo,images,t,score)

event (EventKey (SpecialKey KeyEnter) Down _ _) (GameMode (Pause BackToMenu), jogo,images,t,score) = (Options PlayAgain, jogo, images, t, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (GameMode (Pause BackToMenu), jogo,images,t,score) = (GameMode (Pause Restart), jogo,images,t,score)
event (EventKey (SpecialKey KeyDown) Down _ _) (GameMode (Pause BackToMenu), jogo,images,t,score) = (GameMode (Pause ContinuePlay), jogo,images,t,score)

-- Jogando
event (EventKey k Down _ _) w@(GameMode Play, jogo, images, t, score) = newState k w

-- Nos restantes casos não existem alterações
event _ w = w

--Reage ao tempo passado
time :: Float -> World -> World
time n w@(GameMode Play, jogo@(Jogo (Jogador (x,y)) mapa), images, t, score)
  | t < 1 = if jogoTerminou jogo then (Lost, jogo, images, n+t, score) else (GameMode Play, jogo, images, n+t, score)
  | jogoTerminou jogo = (Lost, jogo, images, n+t, score)
  | y<=1 = if mod (round (t*100)) 260 == 0 then (GameMode Play, deslizaJogoValido (animaJogo jogo Parado) score, images,t+n,score) else (GameMode Play, deslizaJogoValido jogo score, images, n+t, score)
  | mod (round (t*100)) 260 == 0 =
  if mod (round (t*100)) 520 == 0 then (GameMode Play, deslizaJogoValido (animaJogo jogo Parado) score, images,0+n,score) else
    (GameMode Play, (animaJogo jogo Parado), images,n+t,score)
  | otherwise = (GameMode Play, jogo, images, n+t, score)
time _ w = w

--Carregamento das imagens necessárias
carregarImagens :: IO Images
carregarImagens = do
  background <- loadBMP "src/Background.bmp"
  playMenu <- loadBMP "src/MenuInicialPlay.bmp"
  continueMenu <- loadBMP "src/MenuInicialContinue.bmp"
  quitMenu <- loadBMP "src/MenuInicialQuit.bmp"
  pauseContinue <- loadBMP "src/PauseContinue.bmp"
  pauseRestart <- loadBMP "src/PauseRestart.bmp"
  pauseQuit <- loadBMP "src/PauseQuit.bmp"
  lost <- loadBMP "src/Lost.bmp"
  paiNatal <- loadBMP "src/PaiNatal.bmp"
  treno <- loadBMP "src/treno.bmp"
  gelo <- loadBMP "src/gelo.bmp"
  arvore <- loadBMP "src/arvore.bmp"
  trenoEspelhado <- loadBMP "src/trenoEspelhado.bmp"
  estradaGelo <- loadBMP "src/EstradaGelo.bmp"
  rioAgua <- loadBMP "src/RioAgua.bmp"
  rioAguaEspelhado <- loadBMP "src/RioAguaEspelhado.bmp"
  return Images {lost = lost,
                 background = scale 2.5 2.5 background,
                 playMenu = scale 1.5 2 playMenu,
                 continueMenu = scale 1.5 2 continueMenu,
                 quitMenu = scale 1.5 2 quitMenu,
                 pauseContinue = scale 1.5 2 pauseContinue,
                 pauseRestart = scale 1.5 2 pauseRestart,
                 pauseQuit = scale 1.5 2 pauseQuit,
                 paiNatal = scale 0.1 0.1 paiNatal,
                 treno = scale 0.3 0.3 treno,
                 gelo = scale 0.5 0.5 gelo, 
                 arvore = scale 0.2 0.2 arvore,
                 trenoEspelhado = scale 0.3 0.3 trenoEspelhado,
                 estradaGelo = estradaGelo,
                 rioAgua = rioAgua,
                 rioAguaEspelhado = rioAguaEspelhado
                 }

