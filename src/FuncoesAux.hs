--Modulo para as funcoes auxiliares
module FuncoesAux (
    width,
    height,
    offset,
    toInt,
    toFloat,
    posBlocoLim,
    posIniX,
    posIniLim,
    colidirEixo,
    colidir,
    moverX,
    moverY,
    geradorPosX
) where

import Graphics.Gloss


width, height, offset :: Int
width = 800
height = 500
offset = 250

toInt :: Float -> Int
toInt x = round x

toFloat :: Int -> Float
toFloat x = fromIntegral x

posBlocoLim :: Float -> Float
posBlocoLim x 
 | x >= (toFloat(width)/2) = 385
 | x <= (-(toFloat(width)/2)) = (-385)
 | otherwise = x

posIniX :: Float
posIniX = 0

posIniLim :: Float -> Bool
posIniLim x
 | x <= (-(toFloat(width)/2)) = True
 | otherwise = False

colidirEixo :: Float -> Float -> Bool
colidirEixo bloco inimigo = ((inimigo - 20) < (bloco + 25)) && ((inimigo + 20) > (bloco - 25))

colidir :: Point -> Point -> Bool
colidir (xBloco, yBloco) (xInimigo, yInimigo) = ((colidirEixo xBloco xInimigo) && (colidirEixo yBloco yInimigo))

moverX :: Point -> Float -> Point
moverX (x,y) n = (posBlocoLim((x + n)),(-200))

moverY :: Point -> Float -> Point
moverY (x,y) n = (x,300-n)
   
geradorPosX :: Float -> Float
geradorPosX t = (toFloat((mod (toInt(t) * 100) 800)-400))
