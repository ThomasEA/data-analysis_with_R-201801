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

prod_ped <- insta_products %>%
  left_join(products, by = "product_id") %>%
  left_join(departments, by="department_id") %>%
  left_join(aisles, by="aisle_id") %>%
  filter(department != "missing" | aisle != "missing")

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   
full_df <- 
  merge(prod_ped, insta_orders)


   # Transforme as variáveis user_id, department e aisle em factor
full_df$user_id <- factor(full_df$user_id)
full_df$department <- factor(full_df$department)
full_df$aisle <- factor(full_df$aisle)
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)
full_df$order_hour_of_day <- factor(full_df$order_hour_of_day, ordered = TRUE)
   # Este dataframe deverá ser utilizado em todas as atividades seguintes

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
hr_mais_ped <- full_df %>%
  select(order_id, user_id, order_hour_of_day) %>%
  distinct() %>%
  group_by(order_hour_of_day)

top_5_hr <- hr_mais_ped %>%
  group_by(order_hour_of_day) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(5)

top_5_hr %>% View()

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
top_15_prods <- full_df %>%
  filter(order_hour_of_day %in% as.factor(top_5_hr$order_hour_of_day)) %>%
  group_by(product_id, product_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15) 

top_15_prods %>%
  View()

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
full_df %>%
  filter(product_id %in% as.factor(top_15_prods$product_id))  %>%
  group_by(order_hour_of_day, product_name) %>%
  count() %>% 
  summarise(Mean = mean(n)) %>% View()


full_df %>%
  filter(product_id %in% top_15_prods$product_id) %>%
  group_by(product_id, product_name, order_hour_of_day) %>%
  count() %>% 
  group_by(product_id, product_name) %>%
  summarise(med_vendas_hora = mean(n)) %>%
  View()


full_df %>%
  filter(product_id %in% top_15_prods$product_id) %>%
  group_by(order_hour_of_day, product_name) %>%
  summarise(med = mean(n)) -> med_vendas_hora


   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos.

ggplot(med_vendas_hora, aes(x=order_hour_of_day, y=med)) +
  geom_line(aes(group=product_name, color=product_name))

   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
est_descr <- 
  full_df %>%
    group_by(order_hour_of_day) %>%
    summarise(mean = mean(order_id), sd = sd(order_id), max = max(order_id), min = min(order_id), cnt = n()) %>% ungroup()

full_df %>%
  group_by(order_hour_of_day) -> x

# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 
# R.: Acredito que não é uma distribuição gaussiana ou normal, como é possível verificar através do histograma

#{VALIDAR}

ggplot(x, aes(x=order_hour_of_day)) +
  geom_histogram(stat = "count" )


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

#{DEPENDE DA QUESTÃO 9}

#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

full_df %>%
  group_by(order_dow) %>%
  count(order_id) -> dow_group

ggplot(dow_group, aes(x=order_dow, group=order_dow)) +
  geom_boxplot(aes(y=n)) +
  scale_x_continuous( breaks = 0:6 ) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",")) +
  labs( x = "Dia Semana", y = "Qtde Pedidos")


#13 # Identifique, por usuário, o tempo médio entre pedidos
tempo_med_entre_pedidos <- full_df %>%
  group_by(user_id) %>%
  summarise(tempo = mean(days_since_prior_order))


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado
tempo_med_entre_pedidos %>% 
  group_by(tempo) %>%
  count() %>%
  View()

ggplot(tempo_med_entre_pedidos, aes(x=tempo)) +
  geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
  scale_x_continuous() +
  labs( x = "Tempo médio entre pedidos (em dias)"
        , y = "Qtde usuarios" )

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

#{TERMINAR - falta filtrar a quantidade mínima de pedidos}
full_df %>%
  group_by(user_id) %>%
  summarise(tempo = mean(days_since_prior_order))


ggplot(tempo_med_entre_pedidos, aes(x=tempo)) +
  geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
  scale_x_continuous() +
  labs( x = "Tempo médio entre pedidos (em dias)"
        , y = "Qtde usuarios" )


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

