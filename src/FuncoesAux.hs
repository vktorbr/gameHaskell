
module FuncoesAux (
    toInt,
    toFloat,
    posBlocoLim,
    posIniX,
    posIniLim
) where

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
