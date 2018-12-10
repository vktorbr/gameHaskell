--Modulo para as funcoes auxiliares
module FuncoesAux (
    toInt,
    toFloat,
    posBlocoLim,
    posIniX,
    posIniLim,
    colidirEixo,
    colidir,
    moverX
) where

import Graphics.Gloss

toInt :: Float -> Int
toInt x = round x

toFloat :: Int -> Float
toFloat x = fromIntegral x

posBlocoLim :: Float -> Float
posBlocoLim x 
 | x >= 400 = 390
 | x <= (-400) = (-390)
 | otherwise = x

posIniX :: Float
posIniX = 0

posIniLim :: Float -> Bool
posIniLim x
 | x < (-250) = True
 | otherwise = False


colidirEixo :: Float -> Float -> Bool
colidirEixo bloco inimigo = ((inimigo - 20) < (bloco + 25)) && ((inimigo + 20) > (bloco - 25))

colidir :: Point -> Point -> Bool
colidir (xBloco, yBloco) (xInimigo, yInimigo) = ((colidirEixo xBloco xInimigo) && (colidirEixo yBloco yInimigo))

moverX :: Point -> Float -> Point
moverX (x,y) n = (posBlocoLim((x + n)),(-200))
    
