# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?
tmp <- merge(insta_products, products, by = 'product_id', all.y = TRUE) %>%
  mutate(n_v = ifelse(is.na(order_id), 0, 1)) %>%
  select(product_id, product_name, n_v) %>%
  group_by(product_id, product_name) %>%
  summarise(num_vendas = sum(n_v)) %>%
  filter(num_vendas == 0)
  
paste('Qtd. de produtos nunca comprados: ', nrow(tmp))

#------------------------------------------------------------------------
  
#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

df_2 <- 
  merge( merge(products, departments), aisles) %>%
  as.data.frame()

  
#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

df_3 <- df_2 %>%
  group_by(department_id, department, aisle_id, aisle) %>%
  summarise(num_products = n()) %>% 
  arrange(desc(num_products)) %>%
  head(10) %>%
  as.data.frame()

print.data.frame(df_3)

#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

df_corredor_depto <- merge(df_2, df_3)

cnt_pedidos <- merge( df_corredor_depto, insta_products, by = "product_id") %>%
  select(order_id) %>%
  distinct() %>%
  count()

total_pedidos <- nrow(insta_orders)

paste("percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior: ", round(cnt_pedidos * 100 / total_pedidos, digits = 2), "%")

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

prod_ped <- merge(insta_products, df_corredor_depto) %>%
  filter(aisle_id != 100 | department_id != 21) %>%
  select(order_id, product_id)

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   
full_df <- 
  merge(prod_ped, products)

full_df <- 
  merge(full_df, departments)

full_df <- 
  merge(full_df, aisles)

full_df <- 
  merge(full_df, insta_orders)
  
   # Transforme as variáveis user_id, department e aisle em factor
full_df$user_id <- factor(full_df$user_id)
full_df$department <- factor(full_df$department)
full_df$aisle <- factor(full_df$aisle)
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)
full_df$order_hour_of_day <- factor(full_df$order_hour_of_day, ordered = TRUE)
   # Este dataframe deverá ser utilizado em todas as atividades seguintes

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos


#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)


#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.


#13 # Identifique, por usuário, o tempo médio entre pedidos


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.


#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências


#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 


#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

