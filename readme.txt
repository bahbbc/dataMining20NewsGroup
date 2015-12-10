Diretrizes para executar as rotinas implementadas em R para conjunto de dados 20NewsGroups. Após o download do conjunto de dados, siga esses passos para executar os algoritmos.

########
### PRE-PROCESSAMENTO
#######

Para rodar o pré-processamento é necessário ter os dados na seguinte estruturas de diretórios:

~/workspace/dataMining20NewsGroup/

O arquivo que faz o pré-processamento é o `main-pre-processsing.r`. É necessário ter o pacote Matrix e SnowballC.

Use as funções `install.packages('Matrix')` e `install.packages('SnowballC')` caso não possua os pacotes.

Caso queira variar o corte inicial e final dos termos é necessário alterar as linhas 29 e 30 para o corte de maior frequência e as linhas 32 e 33 para o corte de menor frequência.

A execução dessas funções podem demorar um pouco.

Essa rotina utiliza o código presente em `pre_processing.r` e `tf_idf_matrix.r`

Ao final da execução dessa etapa você terá 2 arquivos na raiz do seu computador. 'tf-idf.mtx' e 'tf-idf-norm.mtx'.

=======================================

########
### REDE NEURAL
#######

O arquivo que roda a MLP é o `main-neural.r`. Antes de rodar a MLP é necessário dividir os dados em teste, validação e treinamento primeiro. Isso é feito pela rotina `separate_data.r`.

A chamada da rede está na linha 48. A rede neural recebe 7 parâmetros. O primeiro é o número de épocas, depois os dados para treinamento, os dados desejados de saída da rede para o treinamento, os dados de validação e os dados esperados de saída da rede para a validação, o sexto parâmetro é a taxa de aprendizado inicial e o último o número de neurônios na camada escondida.

A inicialização dos pesos sempre é feita de forma aleatória, não sendo necessário passá-los durante a execução.

Ao final do treinamento da rede um gráfico com a curva do erro quadrático médio das épocas é apresentado.

O código da rede neural se encontra em `mlp_batelada.r` e ela faz uso das rotinas `calc_grad_batelada.r` que contém o método para saída da rede `first_phase` e o método para cálculo das derivadas: `dEt_dx_bat` e as funções auxiliares que ficam em `helper_functions.r` onde é estão implementados outros métodos para ativação da rede (como sigmoid ou gradient) e funções para a derivada, pesos aleatórios e normalização vetorial (função não nativa do R).

Para verificar os resultados obtidos pela rede é necessário rodar o conjunto de testes e  gerar a matrix de confusão utilizando a rotina `avalia-rede.r`

========================================

########
### K-MEANS
#######

O arquivo que roda o k-means é o main-kmeans.r. O primeiro parâmetro da função `my_kmeans` é o conjunto de dados, depois o número de k desejado; a função para o cálculo das distâncias e um erro mínimo tolerado. O erro mínimo não pode ser abaixo de zero, se não o algoritmo ficará em loop infinito.

O código do kmeans está na rotina `kmeans.r` e lá também estão implementadas as funções para cálculo de distância dos centróides.

As funções de avaliação internas e externas estão implementadas na rotina `avalia_kmeans.r`. Nesta rotina também se encontra o mapeamento feito das 20 classes para 6. É necessário mudar a váriavel `compare` deve ser modificada caso outros dados queiram ser analizados pelos índices.
