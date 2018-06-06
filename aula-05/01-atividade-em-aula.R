# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)



# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted_main <- read.csv("aula-05/data/ted_main.csv.gz")



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
  mutate(duration = duration(duration, units = "seconds"),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date)) -> ted_main


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted_main %>%
  mutate(event = factor(x = event),
         speaker_occupation = factor(speaker_occupation)) -> ted_main


# Retire do dataframe a variável name

ted_main %>%
  select(-name) -> ted_main


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas


ted_main %>%
  summary()

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

ted_main %>%
  filter(languages < 1) %>%
  count()
# Foram encontrados 86 registros sem línguas

ted_main %>%
  filter(languages < 1) %>%
  mutate(languages = 1) -> ted_main


# Verifique os 15 registros com menor data de filmagem. 

ted_main %>%
  arrange(film_date) %>%
  head(15) %>%
  select(title, film_date) -> filmes_menor_data_filmagem

filmes_menor_data_filmagem

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

apresentacoes_ano <- ted_main %>%
  group_by(year(film_date)) %>%
  summarise(cont = n()) %>%
  arrange(cont) %>%
  ungroup()

print.data.frame(apresentacoes_ano)

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
quantil = c(quantile(apresentacoes_ano$cont, probs = seq(from=0.1, to=1, by=0.1)))[4]

ted_main %>%
  group_by(ano = year(film_date)) %>%
  summarise(cont = n()) %>%
  ungroup() %>%
  filter(cont > quantil) %>%
  select(ano) -> ted_main_quartil

ted_main <- ted_main %>%
  filter(year(film_date) %in% ted_main_quartil$ano)
  
# Verifique novamente o resumo dos dados do dataframe

ted_main %>%
  View()

# Verifique os 10 registros com maior duração.

ted_main %>%
  arrange(desc(duration)) %>%
  head(10) %>%
  select(title, duration) -> maior_duracao

print.data.frame(maior_duracao)

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas

sd_duration_3 <- ted_main$duration %>%
  sd() * 3

mean_duration <- ted_main$duration %>%
  mean()

duration_sd <- as.duration(mean_duration + sd_duration_3)

ted_main %>%
  filter(duration > duration_sd) %>%
  select(title, 
         duration) %>%
  mutate(duration_sd_3 = duration_sd) -> apresentacoes_acima_dp_3

print.data.frame(apresentacoes_acima_dp_3)

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
quantile(ted_main$duration, probs = seq(0,1,.25)) -> qtl
iqr <- IQR(ted_main$duration)
ted_main %>%
  filter(duration > 1.5 * iqr + qtl[4]) %>%
  select(title, duration) -> apresentacoes_supera
print.data.frame(apresentacoes_supera)

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted_main$views, probs = seq(0.1,1,0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#       R.: A média.
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#       R.: O desvio padrão
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#       R.: O IQR é 379168, e é aprox. 2.65 vezes maior que o Desvio Absoluto da Mediana
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
#       R.: Acredito que a disposição dos valores de quantidades de visualização não está simétrica. 
#           Estes se encontram preferencialmente abaixo da média, já que o IQR (medida entre o 1º e 3º quartil) ficou abaixo da média.
#           O desvio absoluto da mediana para mais (acima da mediana) também ficou ligeiramente acima da média.

summary(ted_main$views)
mean_views <- mean(ted_main$views)
sd_views <- sd(ted_main$views)
dam_views <- median(abs(ted_main$views - median(ted_main$views)))
iqr_views <- IQR(ted_main$views)

iqr_n_vezes_maior = iqr_views / dam_views

######### AQUI FALTA CONTINUAR   ##########


# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
high_views <- ted_main %>%
  filter(views > quantile(ted_main$views, prob = 0.9)) 

ted_main %>%
  filter(views < quantile(ted_main$views, prob = 0.1)) -> low_views

stats_high_views <- data.frame(mean = mean(high_views$languages),
                               sd = sd(high_views$languages),
                               median = median(high_views$languages),
                               iqr = IQR(high_views$languages))
stats_low_views <- data.frame(mean = mean(low_views$languages),
                               sd = sd(low_views$languages),
                               median = median(low_views$languages),
                               iqr = IQR(low_views$languages))
print('10% Maiores Visualizações')
print.data.frame(stats_high_views)
print('10% Menores Visualizações')
print.data.frame(stats_low_views)

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro

ted_main %>%
  group_by(event) %>%
  summarise(cnt_events = n()) -> ted_events
  
ted_events[str_detect(ted_events$event, "TED*"),] -> ted_events

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES

med_views <- median(ted_main$views)

ted_events <- ted_main[str_detect(ted_main$event, "TED*"),]
  
ted_events %>%
  group_by(event) %>%
  summarise(ano_evento = min(year(published_date)),
            qtd_med_linguas = mean(languages),
            sd_qtd_linguas = sd(languages),
            cv = sd_qtd_linguas / mean(languages),
            qtd_views = sum(views)) %>%
  filter(qtd_views > med_views) %>%
  ungroup()


# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas

ted_maior10Apresentacoes %>%
    summarise(correlacao = cor(views, languages)) %>%
    mutate(case_when(
        abs(correlacao) >= 0.9 ~ "MUITO FORTE",
        abs(correlacao) >= 0.7 ~ "FORTE",
        abs(correlacao) >= 0.5 ~ "MODERADO",
        abs(correlacao) >= 0.3 ~ "FRACO",
        abs(correlacao) >= 0.0 ~ "DESPREZÍVEL"
      ))

#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




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

