library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

salarios <- read_csv("aula-04/data/201802_dados_salarios_servidores.csv.gz")

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.


### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

d <- salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  summarise(qtd_serv = n(), 
            coef_corr = cor(year(Sys.Date()) - year(DATA_INGRESSO_ORGAO), year(Sys.Date()) - year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO))) %>%
  ungroup() %>%
  filter(qtd_serv >= 200) %>% #filtra os registros
  arrange(qtd_serv) %>% #ordena
  mutate(DIR_CORR = ifelse(coef_corr >= 0, 'POSITIVO', 'NEGATIVO')) %>%
  mutate(FORCA_CORR = ifelse(abs(coef_corr) >= 0.9, 'Muito Forte',
                             ifelse(abs(coef_corr) >= 0.7 & abs(coef_corr) < 0.9, 'Forte',
                                    ifelse(abs(coef_corr) >= 0.5 & abs(coef_corr) < 0.7, 'Moderada',
                                           ifelse(abs(coef_corr) >= 0.3 & abs(coef_corr) < 0.5, 'Fraca',
                                                  'Desprezível'))))) %>%
  select(DESCRICAO_CARGO, coef_corr, DIR_CORR, FORCA_CORR)


### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###
calcula_moda_lotacao <- function(c) {
    as.character(r <- salarios %>%
    filter(salarios$DESCRICAO_CARGO == as.character(c)) %>%
    count(ORGSUP_LOTACAO) %>%
    arrange(desc(n)) %>%
    head(1) %>%
    select(ORGSUP_LOTACAO))
}

calcula_moda_exercicio <- function(c) {
  as.character(r <- salarios %>%
                 filter(salarios$DESCRICAO_CARGO == as.character(c)) %>%
                 count(ORGSUP_EXERCICIO) %>%
                 arrange(desc(n)) %>%
                 head(1) %>%
                 select(ORGSUP_EXERCICIO))
}

dez_mais <- d %>%
  arrange(desc(abs(coef_corr))) %>%
  head(10)

dez_menos <- d %>%
  arrange(abs(coef_corr)) %>%
  head(10)

records <- rbind(dez_mais, dez_menos) #junta dois datasets

records %>% pull(DESCRICAO_CARGO) -> cargos

resultado_2 <- records %>%
  group_by(DESCRICAO_CARGO, FORCA_CORR) %>%
  summarise(MODA_ORGSUP_LOTACAO = calcula_moda_lotacao(DESCRICAO_CARGO),
            MODA_ORGSUP_EXERCICIO = calcula_moda_exercicio(DESCRICAO_CARGO))


#Resposta:
#Existe diferença entre as Modas. No geral, quando existe diferença a força da correlação tende a ser Desprezíl, para a amostra analisada

