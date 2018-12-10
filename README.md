# gameHaskell

para rodar, segue os passos:
```bash
cabal update 
cabal install --only-dependencies 
cabal build 
cabal run
```

O jogo:
```bash
Dodger é um jogo em que você terá que fazer o quadrado pegar as bolas que estão caindo (uma por vez)
Com o avançar dos pontos, aumentará o nível e as bolas cairão mais rapidamente
Ele termina quando você deixa uma bola passar pelo quadrado sem pegar
```
Como jogar:
```bash
A tela inicial do jogo é o menu, em que tem o ponto recorde até o momento
Para começar o jogo, precione a tecla 'space bar' do teclado e será direcionado a tela do jogo
Para mover o quadrado você tme a opção das teclas de seta para esquerda e direita para move-lo para os respectivos lados
Tem a opção do speed em que você aumenta a velocidade de movimentação do quadrado apertando, junto com as setas, a tecla 'shiftLeft' ou 'shiftRight'
Para pegar as bolas basta fazer com que o quadrado se colida com elas
Quando terminar o jogo você poderá ir ao menu apertando a tecla r
```