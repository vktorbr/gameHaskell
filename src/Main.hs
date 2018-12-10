module Main where

--Pacote Gloss para fazer o jogo 2D
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import System.IO.Unsafe

--Pacote proprio das funções auxiliares
import FuncoesAux
import FuncoesFrames

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
 ,  speedAtivado :: Bool
 ,  sp :: Float
 }

varSpeed :: Float
varSpeed = 100

speedVisualizar:: MVar Float -> IO Float
speedVisualizar vel = do
    s <- takeMVar vel
    putMVar vel s
    return s

speedAlimentar :: MVar Float -> IO ()
speedAlimentar vel = do
    s <- takeMVar vel
    if s<=99
        then if s>95
            then putMVar vel 100
            else putMVar vel (s+5)
        else do
            putMVar vel s
    threadDelay 1000000
    speedAlimentar vel


speeder :: MVar Float -> IO Float
speeder vel = do
    s <- takeMVar vel
    if s>0
        then do
            putMVar vel (s-1)
            return 1
        else do
            putMVar vel s
            return 0

window :: Display
window = InWindow "BallVSSquare" (width,height) (offset,offset)

background :: Color
background = black

drawing :: EstadoJogo -> IO Picture
drawing game
 | (fim game) == True = fimJogo game
 | (start game) == False = menu game
 | otherwise = estadoRodando game

fimJogo :: EstadoJogo -> IO Picture
fimJogo game = do
    return(fots)
    where
        fots = pictures [
                    textoFim,
                    textoVoltar
                ]
        

estadoRodando :: EstadoJogo -> IO Picture
estadoRodando game = do 
    return(fotos)
    where
        fotos = pictures [
                    bloco (posicaoBloco game),
                    pontoAtual (pontos game),
                    speedAtual (toInt(sp game)),
                    nivelAtual (toInt(nivel game)),
                    inimigos (posicaoInim game)
                ]

menu :: EstadoJogo -> IO Picture
menu game = do
    return(fot)
    where
        fot = pictures [
                    nomeJogo ,
                    pontuacao (recorde game) ,
                    botaoInicial 
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
    , speedAtivado = False
    , sp = 100
    }

evento :: Event -> EstadoJogo -> IO EstadoJogo
evento (EventKey (SpecialKey KeySpace) (Down) _ _) game = return game {posicaoBloco = (0,(-200)), pontos = 0, posicaoInim = (geradorPosX (tempo game),300), contadorPosInimigo =0, start = True, nivel = 1}
evento (EventKey (Char 'p') _ _ _) game = do return game {start = False}
evento (EventKey (Char 'r') (Down) _ _) game = do return game {start = False, fim = False, recorde = max (recorde game) (pontos game)}
evento (EventKey (SpecialKey KeyLeft) (Down) _ _) game = do return game {irEsquerda = True}
evento (EventKey (SpecialKey KeyLeft) (Up) _ _) game = do return game {irEsquerda = False}
evento (EventKey (SpecialKey KeyRight) (Down) _ _) game = do return game {irDireita = True}
evento (EventKey (SpecialKey KeyRight) (Up) _ _) game = do return game {irDireita = False}
evento (EventKey (SpecialKey KeyShiftL) (Down) _ _) game = do return game {speedAtivado = True}
evento (EventKey (SpecialKey KeyShiftR) (Down) _ _) game = do return game {speedAtivado = True}
evento (EventKey (SpecialKey KeyShiftL) (Up) _ _) game = do return game {speedAtivado = False}
evento (EventKey (SpecialKey KeyShiftR) (Up) _ _) game = do return game {speedAtivado = False}
evento _ game = return game

atualizar :: MVar Float -> Float -> EstadoJogo -> IO EstadoJogo
atualizar speed n game = do
    return(jogo)
    where
        jogo =  if (fim game)
                    then game
                else if (colidir (posicaoBloco game) (posicaoInim game))
                    then game {tempo = (tempo game) + n, contadorPosInimigo = 0, pontos = (pontos game) +1, posicaoInim = ((geradorPosX (tempo game)),300)}
                else if (perder (posicaoInim game))
                    then game {tempo = (tempo game) + n, fim = True, start = False, contadorPosInimigo = 0, posicaoInim = ((geradorPosX (tempo game)),300)}
                else if ((irEsquerda game) && (speedAtivado game))
                    then game { tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoBloco = (moverX (posicaoBloco game) ((-2)*((unsafePerformIO (speeder speed))+1))), posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game)), sp = (unsafePerformIO(speedVisualizar speed))}
                else if (irEsquerda game)
                    then game { tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoBloco = (moverX (posicaoBloco game) (-2)), posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game)), sp = (unsafePerformIO(speedVisualizar speed))}
                else if ((irDireita game) && (speedAtivado game))
                    then game { tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoBloco = (moverX (posicaoBloco game) ((2)*((unsafePerformIO (speeder speed))+1))), posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game)), sp = (unsafePerformIO(speedVisualizar speed))}
                else if (irDireita game)
                    then game {tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoBloco = (moverX (posicaoBloco game) (2)), posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game)), sp = (unsafePerformIO(speedVisualizar speed))}
                else if (start game)==False
                    then game {tempo = (tempo game) + n}
                else if ((toFloat(pontos game)/(nivel game))>=5)
                    then game {tempo = (tempo game) + n, nivel = (nivel game) + 1}
                else
                    game {tempo = (tempo game) + n, contadorPosInimigo = (contadorPosInimigo game) + 1, posicaoInim = moverY (posicaoInim game) ((contadorPosInimigo game)*(nivel game)), sp = (unsafePerformIO(speedVisualizar speed))}


main :: IO ()
main = do
    speed <- newMVar 100
    forkIO $ speedAlimentar speed
    playIO window background fps estadoInicial drawing evento (atualizar speed)
    --putStrLn show recordeMVar
