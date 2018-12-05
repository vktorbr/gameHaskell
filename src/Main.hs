module Main where

--Pacote Gloss para fazer o jogo 2D
import Graphics.Gloss
--import Graphics.Gloss.Game
--import Graphics.Gloss.Juicy
--import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 800
height = 500
offset = 250

data EstadoJogo = Game
 {
    pontos :: Float
 ,  nivel :: Int
 ,  texto :: String
 , comecar :: Bool
 }

window :: Display
window = InWindow "Dodger" (width,height) (offset,offset)

background :: Color
background = white

drawing :: EstadoJogo -> Picture
drawing game 
 | comecar game == False = menu game
 | otherwise = estadoRodando game

estadoRodando :: EstadoJogo -> Picture
estadoRodando game = pictures [
        bloco,
        pontoAtual
    ]
    where
        bloco = 
            translate (-100) (-200) $
            Color blue $
            rectangleSolid 50 50
        
        pontoAtual =
            translate 280 180 $
            Scale 0.5 0.5 $
            Color red $
            Text (show (pontos game)) 

menu :: EstadoJogo -> Picture
menu game = pictures [
        nomeJogo ,
        pontuacao ,
        botaoInicial 
    ]   

    where
        nomeJogo = 
            translate (-150) 150 $
            Scale 0.7 0.7 $
            Color red $
            Text "Dodger" 
        
        pontuacao = 
            translate (-100) 0 $
            Scale 0.2 0.2 $
            Text (texto game) 
        
        botaoInicial = translate 0 (-150) $ pictures [
            translate (-20) 0 $ rectangleSolid 450 50,
            translate (-225) (-10) $ Scale 0.2 0.2 $ Color red $ Text "aperte space bar para comecar"
            ]

fps :: Int
fps = 60

estadoInicial :: EstadoJogo
estadoInicial = Game {
       pontos = 0
    ,  nivel = 1
    , texto = "pontuacao = 0"
    , comecar = False
    }


evento :: Event -> EstadoJogo -> EstadoJogo
evento (EventKey (SpecialKey KeySpace) _ _ _) game = game {comecar = True}
evento _ game = game

atualizar :: Float -> EstadoJogo -> EstadoJogo
atualizar n game = game {pontos = (pontos game) +1 }

main :: IO ()
main = play window background fps estadoInicial drawing evento atualizar
