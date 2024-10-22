
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ME918-T3-GB

## API para Manipulação de um Banco de Dados e Inferência

<!-- badges: start -->
<!-- badges: end -->

Querido cliente,

Este produto possui o intuito de facilitar inserções, exclusões e
modificações no banco de dados no qual trabalhamos. Apenas para
relembrar, o mesmo possui três variáveis.

- *x*: Variável preditora numérica;
- *grupo*: Variável preditora categórica, com níveis A, B e C;
- *y*: Variável resposta numérica.

Também é indicado no banco de dados a data na qual os dados foram
acrescentados.

Para manipulação dos dados, possuímos as seguintes rotas.

- *new_obs*: Informando valores x e y e fator grupo válidos, acrescenta
  a informação em uma nova linha, indicando a data na qual foi
  acrescentada e a posição desta nova observação;
- *delete_obs*: Informando o número da observação, permite excluí-la do
  conjunto de dados;
- *switch_obs*: Informando o número da observação, valores x e y e fator
  grupo válidos, atualiza os dados de determinada observação específica.
