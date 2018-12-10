--Modulo para as funções Picture do jogo
module FuncoesFrames(
    textoFim,
    textoVoltar,
    bloco,
    pontoAtual,
    speedAtual,
    inimigos,
    nomeJogo,
    pontuacao,
    botaoInicial
)where

import Graphics.Gloss

textoFim = translate (-300) 0 $
    Scale 0.7 0.7 $
    Color red $
    Text "Fim de jogo!!"

textoVoltar = translate (-200) (-200) $
    Scale 0.2 0.2 $
    Color orange $
    Text "Aperte \" r \" para voltar ou menu"

bloco posicaoBloco = 
    translate x y $
    Color blue $
    rectangleSolid 50 50
    where 
        (x,y) = posicaoBloco

pontoAtual pontos =
    translate 280 180 $
    Scale 0.5 0.5 $
    Color green $
    Text (show pontos) 

speedAtual speed =
    translate (-280) 180 $
    Scale 0.5 0.5 $
    Color green $
    Text (show speed) 

inimigos posicaoInim = 
    translate xInimigo yInimigo $ 
    Color red $ 
    circleSolid 20
    where
        (xInimigo, yInimigo) = posicaoInim

nomeJogo = 
    translate (-150) 150 $
    Scale 0.7 0.7 $
    Color red $
    Text "Dodger" 

pontuacao recorde = 
    translate (-100) 0 $
    Scale 0.2 0.2 $
    Color orange $
    Text ("Recorde: " ++ show(recorde) ) 

botaoInicial = 
    translate (-225) (-160) $ 
    Scale 0.2 0.2 $ Color red $ 
    Text "aperte space bar para comecar"