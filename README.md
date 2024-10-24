
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

Os dados iniciais foram gerados da seguinte forma:

- A variável *x* foi gerada através de uma soma independente de uma v.a.
  Poisson com média 4 e uma v.a. uniforme sobre o intervalo (-3, 3);
- O *grupo* foi determinado através de amostragem aleatória com
  reposição entre as categorias “A”, “B” e “C”;
- O *y* foi obtido através de uma equação de regressão, com intercepto e
  coeficiente relacionado à variável *x* v.a. uniformes sobre o
  intervalo (-2, 2), e efeitos de grupo iguais a 2 e 3 para “B” e “C”,
  respectivamente. A variância do erro estabelecida é igual a 4.

Também é indicado no banco de dados a data na qual os dados foram
acrescentados.

Para manipulação dos dados, possuímos as seguintes rotas.

- *new_obs*: Informando valores x e y e fator grupo válidos, acrescenta
  a informação em uma nova linha, indicando a data na qual foi
  acrescentada e a posição desta nova observação;
- *delete_obs*: Informando o número da observação, permite excluí-la do
  conjunto de dados; \[**ELETIVA**\]
- *switch_obs*: Informando o número da observação, valores x e y e fator
  grupo válidos, atualiza os dados de determinada observação específica.
  \[**ELETIVA**\]

Já no que se refere às rotas para inferência dos parâmetros, temos as
que seguem, todas relacionadas com obtenção de resultados (nenhuma
permite manipulação dos dados).

- *grafico*: Gera um gráfico com valores das variáveis numéricas x e y,
  assim como coloca cores para os pontos em relação a cada grupo;
  \[Observação: O gráfico é feito através de modelo de regressão
  considerando interação entre *x* e o *grupo*, que não é o estabelecido
  aqui. Não conseguimos alterar isto\]
- *parametros*: Retorna os parâmetros da regressão em formato .json: os
  coeficientes associados a cada variável preditora e o quadrado médio
  do erro (estimador não-viesado para variância do erro);
- *residuos*: Retorna, para cada observação, o resíduo associado, no
  formato .json; \[**ELETIVA**\]
- *grafico_residuos*: Retorna o gráfico que compara os quantis da
  distribuição amostral dos resíduos com os quantis de uma distribuição
  normal (QQ-Plot); \[**ELETIVA**\]
- *significancia*: Retorna a significância dos coeficientes de regressão
  em formato .json. \[**ELETIVA**\]

Por fim, destacam-se as rotas criadas para predição com base no modelo
de regressão estabelecido.

- *predicao_unica*: Inserindo valores para as variáveis preditoras *x* e
  *grupo*, retorna o valor predito para variável resposta numérica y,
  com base no modelo de regressão treinado com as observações.
- *predicao_multipla*: Inserindo múltiplos valores para *x* e *grupo* em
  formato JSON no corpo da requisição (req), retorna os valores preditos
  de y para cada um dos novos conjuntos de variáveis preditoras, com
  base no modelo de regressão treinado com as observações.
  \[**ELETIVA**\]

#### Exemplos

Seguem alguns exemplos para requisições utilizando as rotas construídas.
O valor *servidor:XXXX* nas URLs exemplificadas deverá ser trocado pelo
local e a porta em que o API está sendo executado. Em geral, se você
está rodando a API localmente, o valor para *servidor* será “localhost”,
que geralmente mapeia para o endereço IP “127.0.0.1”, reservado para uso
local, enquanto se você usar a API em um servidor na internet o valor de
*servidor* será, obviamente, o servidor de forma “meuservidor.com”. O
valor de *XXXX* será a porta em que o serviço está sendo processado.

- *new_obs*: Adicionamos uma nova observação em que *x* = 3.5, *grupo* =
  B e *y* = 2.125 com o seguinte URL:
  <a href="http://servidor:XXXX/new_obs?x=3.5&amp;grupo=B&amp;y=2.125"
  class="uri">http://servidor:XXXX/new_obs?x=3.5&amp;grupo=B&amp;y=2.125</a>

- *delete_obs*: Removemos a observação nova que fizemos, em que sua a
  posição da linha é *observacao* = 26 com o seguinte URL:
  <a href="http://servidor:XXXX/delete_obs?observacao=26"
  class="uri">http://servidor:XXXX/delete_obs?observacao=26</a>

- *switch_obs*: Alteramos a observação nova que fizemos, em que sua a
  posição da linha é *observacao* = 26, para os novos valores *x* = 2.6,
  *grupo* = A e *y* = 1, com o seguinte URL: <a
  href="http://servidor:XXXX/switch_obs?observacao=26&amp;x=2.6&amp;grupo=A&amp;y=1"
  class="uri">http://servidor:XXXX/switch_obs?observacao=26&amp;x=2.6&amp;grupo=A&amp;y=1</a>

- *grafico*: Criamos o gráfico das observações com a seguinte URL:
  <a href="http://servidor:XXXX/grafico"
  class="uri">http://servidor:XXXX/grafico</a>

- *parametros*: Estimamos os parâmetros da nossa regressão com a
  seguinte URL: <a href="http://servidor:XXXX/parametros"
  class="uri">http://servidor:XXXX/parametros</a>

- *residuos*: Observamos os resíduos do modelo ajustado com a seguinte
  URL: <a href="http://servidor:XXXX/residuos"
  class="uri">http://servidor:XXXX/residuos</a>

- *grafico_residuos*: Podemos ver o gráfico de resíduos do modelo com a
  seguinte URL: <a href="http://servidor:XXXX/grafico_residuos"
  class="uri">http://servidor:XXXX/grafico_residuos</a>

- *significancia*: Analisamos o nível de significância dos coeficientes
  com a seguinte URL: <a href="http://servidor:XXXX/significancia"
  class="uri">http://servidor:XXXX/significancia</a>

- *predicao_unica*: Podemos predizer um único novo *y* a partir das
  variáveis preditoras *x* = 3, *grupo* = C com a seguinte URL:
  <a href="http://servidor:XXXX/predicao_unica?new_x=3&amp;new_grupo=C"
  class="uri">http://servidor:XXXX/predicao_unica?new_x=3&amp;new_grupo=C</a>

- *predicao_multipla*: A predição de múltiplos valores deve ser feito a
  partir do corpo da requisição em formato JSON. Podemos predizer os
  valores de *y* para as seguintes variáveis preditoras *x*: \[1.2, 2.3,
  3.4\], *grupo*: \[“C”, “B”, “A”\] em uma ferramenta de envio de
  requisições a partir das entradas:

\$json = ‘{“x”: \[1.2, 2.3, 3.4\], “grupo”: \[“C”, “B”, “A”\]}’

Invoke-RestMethod -Uri “<a href="http://servidor:XXXX/predicao_multipla"
class="uri">http://servidor:XXXX/predicao_multipla</a>” -Method Post
-Body \$json -ContentType “application/json”
