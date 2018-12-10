module Main where

--Pacote Gloss para fazer o jogo 2D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import FuncoesAux

width, height, offset :: Int
width = 800
height = 500
offset = 250
--posBloco :: Point
--posBloco = ((-150),(-200))

data EstadoJogo = Game
 {
    pontos :: Float
 ,  nivel :: Float
 ,  texto :: String
 ,  start :: Bool
 ,  fim :: Bool
 ,  posicaoBloco :: Point
 ,  tempo :: Float
 ,  tempoInicio :: Float
 ,  posicaoInim :: Point
 ,  gerarInimigoPosX :: Float
 ,  criarIni :: Bool
 ,  irEsquerda :: Bool
 ,  irDireita :: Bool
 }

window :: Display
window = InWindow "Dodger" (width,height) (offset,offset)

background :: Color
background = white

drawing :: EstadoJogo -> Picture
drawing game 
 | (fim game) == True = fimJogo game
 | (start game) == False = menu game
 | otherwise = estadoRodando game

fimJogo :: EstadoJogo -> Picture
fimJogo game = 
    translate 0 0 $
    Scale 1 1 $
    Color red $
    Text "Fim de jogo!!"

estadoRodando :: EstadoJogo -> Picture
estadoRodando game = pictures [
        bloco,
        pontoAtual,
        inimigos
    ]
    where
        --game{posicaoBloco = (x, y)}
        bloco = 
            translate x y $
            Color blue $
            rectangleSolid 50 50
            where 
                (x,y) = (posicaoBloco game)
        
        pontuacao = (tempo game)--toInt ((tempo game) / 10)

        pontoAtual =
            translate 280 180 $
            Scale 0.5 0.5 $
            Color red $
            Text (show pontuacao) 
        
        posIniX = 0

        inimigos = 
            translate xInimigo yInimigo $ 
            Color red $ 
            circleSolid 20
            where
                (xInimigo, yInimigo) = (posicaoInim game)


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
    , nivel = 1
    , texto = "pontuacao = 0"
    , start = False
    , fim = False
    , posicaoBloco = ((-150),(-200))
    , tempo =0
    , tempoInicio =0
    , posicaoInim = (0,300)
    , gerarInimigoPosX = 0
    , criarIni = False
    , irEsquerda = False
    , irDireita = False
    }


evento :: Event -> EstadoJogo -> EstadoJogo
evento (EventKey (SpecialKey KeySpace) (Down) _ _) game = game {posicaoBloco = ((-150),(-200)), pontos = 0, gerarInimigoPosX = (toFloat((mod (toInt(tempo game) * 100) 800)-400)), tempoInicio =0, start = True, tempo = 0}
evento (EventKey (Char 'p') _ _ _) game = game {start = False}
evento (EventKey (Char 'q') _ _ _) game = game {nivel = (nivel game) + 1}
evento (EventKey (SpecialKey KeyLeft) (Down) _ _) game = game {irEsquerda = True}
evento (EventKey (SpecialKey KeyLeft) (Up) _ _) game = game {irEsquerda = False}
evento (EventKey (SpecialKey KeyRight) (Down) _ _) game = game {irDireita = True}
evento (EventKey (SpecialKey KeyRight) (Up) _ _) game = game {irDireita = False}
evento _ game = game

atualizar :: Float -> EstadoJogo -> EstadoJogo
atualizar n game = 
    if (fim game)
        then game
        else if (colidir (posicaoBloco game) (posicaoInim game))
            then game {fim = True, start = False}
        else if (irEsquerda game)
            then game {pontos = (pontos game) +1 , tempo = (tempo game) + n, tempoInicio = (tempoInicio game) + 1, posicaoBloco = (moverX (posicaoBloco game) (-2)), posicaoInim = ((gerarInimigoPosX game),((300 - (tempoInicio game))))}
        else if (irDireita game)
            then game {pontos = (pontos game) +1 , tempo = (tempo game) + n, tempoInicio = (tempoInicio game) + 1, posicaoBloco = (moverX (posicaoBloco game) (2)), posicaoInim = ((gerarInimigoPosX game),((300 - (tempoInicio game))))}
        else
            game {pontos = (pontos game) +1 , tempo = (tempo game) + n, tempoInicio = (tempoInicio game) + 1, posicaoInim = ((gerarInimigoPosX game),((300 - (tempoInicio game))))}


main :: IO ()
main = play window background fps estadoInicial drawing evento atualizar
