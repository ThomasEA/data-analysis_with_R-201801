# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)
library(dplyr)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted_main <- read_csv("aula-05/data/ted_main.csv.gz")

# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

summary(ted_main)
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
# Não

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

ted_main %>%
  mutate(duration = as.duration(duration),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date)) -> ted_main


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted_main %>%
  mutate(event = as.factor(event),
         speaker_occupation = as.factor(speaker_occupation)) -> ted_main

# Retire do dataframe a variável name

ted_main %>%
  select(-name) -> ted_main


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
summary(ted_main)
  

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

ted_main %>% 
  filter(languages < 1) %>%
  count()
# Foram encontrados 86 registros sem línguas

ted_main %>%
  mutate(languages = if_else(as.integer(languages) <= 0, as.integer(1), languages)) -> ted_main

ted_main %>% View()

# Verifique os 15 registros com menor data de filmagem. 

ted_main %>%
  arrange(film_date) %>%
  head(15) %>%
  select(title, film_date) -> filmes_menor_data_filmagem

filmes_menor_data_filmagem

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

ted_main %>%
  group_by(year(film_date)) %>%
  summarise(cont = n()) %>%
  arrange(cont) %>%
  ungroup() -> apresentacoes_ano

print.data.frame(apresentacoes_ano)

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
quantile(apresentacoes_ano$cont, probs = seq(from=0, to=1, by=0.1))

quantil = c(quantile(apresentacoes_ano$cont, probs = seq(from=0, to=1, by=0.1)))[5]

ted_main %>%
  group_by(year(film_date)) %>%
  mutate(cont = n()) %>%
  ungroup() %>%
  filter(cont > quantil) -> ted_main_quartil

# Verifique novamente o resumo dos dados do dataframe

ted_main_quartil %>%
  View()

# Verifique os 10 registros com maior duração.

ted_main_quartil %>%
  arrange(desc(duration)) %>%
  head(10) %>%
  select(title, duration) -> maior_duracao

print.data.frame(maior_duracao)

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas

sd_duration_3 <- ted_main_quartil$duration %>%
  sd() * 3

mean_duration <- ted_main_quartil$duration %>%
  mean()

duration_sd <- as.duration(mean_duration + sd_duration_3)

ted_main_quartil %>%
  filter(duration > duration_sd) %>%
  select(title, 
         duration) %>%
  mutate(duration_sd_3 = duration_sd) -> apresentacoes_acima_dp_3

print.data.frame(apresentacoes_acima_dp_3)

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
quantile(ted_main_quartil$duration, probs = seq(0,1,.25)) -> qtl
iqr <- IQR(ted_main_quartil$duration)
ted_main_quartil %>%
  filter(duration > 1.5 * iqr + qtl[4]) %>%
  select(title, duration) -> apresentacoes_supera
print.data.frame(apresentacoes_supera)

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted_main_quartil$views, probs = seq(0,1,0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
summary(ted_main_quartil$views)
median_views <- median(ted_main_quartil$views)
mean_views <- mean(ted_main_quartil$views)
#   * Média e Mediana. Qual é maior?
#       R.: A média.
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#       R.: O desvio padrão
sd_views <- sd(ted_main_quartil$views)
dam_views <- median(abs(ted_main_quartil$views - median(ted_main_quartil$views)))
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#       R.: O IQR é 947532.75, e é aprox. 2.19 vezes maior que o Desvio Absoluto da Mediana
iqr_views <- IQR(ted_main_quartil$views)
iqr_n_vezes_maior = iqr_views / dam_views
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
#       R.: Acredito que a disposição dos valores de quantidades de visualização não está simétrica. 
#           Estes se encontram preferencialmente abaixo da média, já que o IQR (medida entre o 1º e 3º quartil) ficou abaixo da média.
#           O desvio absoluto da mediana para mais (acima da mediana) também ficou ligeiramente acima da média.


# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
ted_main_quartil %>%
  filter(views > quantile(ted_main_quartil$views, prob = 0.9)) %>%
  mutate(agrupamento = 'High_Views') %>%
  group_by(agrupamento) %>%
  summarise(mean = mean(languages),
            sd = sd(languages),
            median = median(languages),
            iqr = IQR(languages)) -> high_views

ted_main_quartil %>%
  filter(views < quantile(ted_main_quartil$views, prob = 0.1)) %>%
  mutate(agrupamento = 'Low_Views') %>%
  group_by(agrupamento) %>%
  summarise(mean = mean(languages),
            sd = sd(languages),
            median = median(languages),
            iqr = IQR(languages)) -> low_views


print('10% Maiores Visualizações')
print.data.frame(high_views)
print('10% Menores Visualizações')
print.data.frame(low_views)

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro

ted_main_quartil %>% 
  filter(str_detect(event, "TED*")) %>%
  group_by(event) %>%
  summarise(cnt_events = n()) -> ted_events

print(nrow(ted_events))

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
ted_main_quartil %>% 
  filter(str_detect(event, "TED") & views > median_views) -> ted_events_filt
#   * a quantidade de apresentações resultante do filtro, por evento
ted_events_filt %>% 
  group_by(event) %>%
  summarise(qtd = n()) %>% View()

#   * o ano do evento (utilizar o menor ano da data de publicação)
ted_events_filt %>%
  group_by(event) %>%
  summarise(ano_publicacao = min(year(published_date)), qtd_apres = n()) %>%
  View()

#   * a quantidade média de línguas das apresentações
ted_events_filt %>%
  summarise(media = mean(languages)) %>% 
  print(media)

#   * o desvio padrão da quantidade de línguas
ted_events_filt %>%
  summarise(desv_padrao = sd(languages)) %>% 
  print(desv_padrao)

#   * o coeficiente de variação da quantidade de línguas
ted_events_filt  %>%
  summarise(cf_var = sd(languages) / mean(languages)) %>%
  print(cf_var)
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
ted_events_filt %>%
  group_by(event) %>%
  summarise(qtd_apres = n()) %>%
  filter(qtd_apres > 10) %>%
  ungroup() -> ted_events_mais_dez_apresentacoes
  
# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas

ted_events_filt %>%
    summarise(corr = cor(views, languages)) %>%
    mutate(tipo = 'Views x Languages') -> c1

#     * Quantidade de visualizações e Duração
ted_events_filt %>%
  summarise(corr = cor(views, duration)) %>%
  mutate(tipo = 'Views x Duration') -> c2
#     * Quantidade de visualizações e Quantidade de Comentários
ted_events_filt %>%
  summarise(corr = cor(views, comments)) %>%
  mutate(tipo = 'Views x Comments') -> c3

#     * Quantidade de Comentários e Quantidade de línguas
ted_events_filt %>%
  summarise(corr = cor(comments, languages)) %>%
  mutate(tipo = 'Comments x Languages') -> c4

rbind(c1, c2, c3, c4) %>%
  mutate(classificacao = case_when(
    abs(corr) >= 0.9 ~ "MUITO FORTE",
    abs(corr) >= 0.7 ~ "FORTE",
    abs(corr) >= 0.5 ~ "MODERADO",
    abs(corr) >= 0.3 ~ "FRACO",
    abs(corr) >= 0.0 ~ "DESPREZÍVEL"
  )) %>% View()

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado


ted_main %>%
  group_by(year(film_date)) %>%
  summarise(med_duracao = median(duration)) -> df_ted_main

cor(x = df_ted_main$`year(film_date)`, y = df_ted_main$med_duracao)

#A correlão entre ano e mediana de duração é -0.24.85896.
#Apesar de ser NEGATIVA (ir na direção oposta), é considerada Desprezível
#Isso significa que há praticamente nenhuma relação entre a variável ano de filmagem e  mediana de duração

