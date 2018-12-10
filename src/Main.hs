module Main where

--Pacote Gloss para fazer o jogo 2D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Concurrent

--Pacote proprio das funções auxiliares
import FuncoesAux

data EstadoJogo = Game
 {
    pontos :: Int
 ,  nivel :: Float
 ,  recorde :: Int
 ,  start :: Bool
 ,  fim :: Bool
 ,  posicaoBloco :: Point
 ,  tempo :: Float
 ,  contadorPosInimigo :: Float
 ,  posicaoInim :: Point
 ,  irEsquerda :: Bool
 ,  irDireita :: Bool
 }

window :: Display
window = InWindow "Dodger" (width,height) (offset,offset)

background :: Color
background = black

drawing :: EstadoJogo -> Picture
drawing game 
 | (fim game) == True = fimJogo game
 | (start game) == False = menu game
 | otherwise = estadoRodando game

fimJogo :: EstadoJogo -> Picture
fimJogo game = pictures [
        textoFim,
        textoVoltar
    ]
    where
        textoFim = 
            translate (-300) 0 $
            Scale 0.7 0.7 $
            Color red $
            Text "Fim de jogo!!"

        textoVoltar =
            translate (-200) (-200) $
            Scale 0.2 0.2 $
            Color orange $
            Text "Aperte \" r \" para voltar ou menu"

estadoRodando :: EstadoJogo -> Picture
estadoRodando game = pictures [
        bloco,
        pontoAtual,
        inimigos
    ]
    where

        bloco = 
            translate x y $
            Color blue $
            rectangleSolid 50 50
            where 
                (x,y) = (posicaoBloco game)
        
        pontoAtual =
            translate 280 180 $
            Scale 0.5 0.5 $
            Color green $
            Text (show (pontos game)) 
        
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
            Color orange $
            Text ("Recorde: " ++ show(recorde game)) 
        
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
    , recorde = 0
    , start = False
    , fim = False
    , posicaoBloco = (0,(-200))
    , tempo =0
    , contadorPosInimigo =0
    , posicaoInim = (0,300)
    , irEsquerda = False
    , irDireita = False
    }

evento :: Event -> EstadoJogo -> EstadoJogo
evento (EventKey (SpecialKey KeySpace) (Down) _ _) game = game {posicaoBloco = (0,(-200)), pontos = 0, posicaoInim = (geradorPosX (tempo game),300), contadorPosInimigo =0, start = True}
--evento (EventKey (Char 'p') _ _ _) game = game {start = False}
evento (EventKey (Char 'r') (Down) _ _) game = game {start = False, fim = False, recorde = max (recorde game) (pontos game)}
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
        then game {tempo = (tempo game) + n, contadorPosInimigo = 0, pontos = (pontos game) +1, posicaoInim = ((geradorPosX (tempo game)),300)}
    else if (perder (posicaoInim game))
        then game {tempo = (tempo game) + n, fim = True, start = False, contadorPosInimigo = 0, posicaoInim = ((geradorPosX (tempo game)),300)}
    else if (irEsquerda game)
        then game { tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoBloco = (moverX (posicaoBloco game) (-2)), posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game))}
    else if (irDireita game)
        then game {tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoBloco = (moverX (posicaoBloco game) (2)), posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game))}
    else if (start game)==False
        then game {tempo = (tempo game) + n}
    else if ((toFloat(pontos game)/(nivel game))>=5)
        then game {tempo = (tempo game) + n, nivel = (nivel game) + 1}
    else
        game {tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game))}


main :: IO ()
main = do
    play window background fps estadoInicial drawing evento atualizar
