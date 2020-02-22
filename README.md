# PGC

Objetivo : Exploração multithread de árvore de busca utilizando functores representáveis 

Primeiramente será feita a modelagem de alguns jogos em Haskell, e posterior utilização de inteligência articial para resolução dos problemas de cada jogo.
Os jogos estão dividos em competitivos e não competitivos.

Competitivos     : Jogo da Velha, Damas.
Não Competitivos : Torre de Hanoi, 8 Damas, 8 puzzle. 

As modelagens estão em na subpasta src/ dentro da pasta de cada jogo.

Até o momento, a modelagem dos jogos não competitivos está completa. Falta revisar o funcionamento interno e validar com o QuickCheck. A modelagem dos jogos competitivos está incompleta.

O próximo passo é criação do Main para cada jogo, e a construção de árvores de busca em largura e em profundidade, e utilizá-las para resolução de cada jogo. 
