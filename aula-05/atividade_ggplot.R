if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")
library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)

ted_main <- read.csv("aula-05/data/ted_main.csv.gz")

summary(ted_main$views)

ted_main %>%
  mutate(ano_publicacao = year(as_datetime(published_date))) %>%
  filter(ano_publicacao >= 2012 & ano_publicacao <= 2017) -> ted_main_transf
  #group_by(event, ano_publicacao) %>%
  #summarise(num_views = sum(views)) -> ted_main_t
ggplot(ted_main_transf, aes( x = views)) +
  geom_histogram(bins = 1000) +
  scale_x_continuous(labels = scales::comma_format()) +
  facet_wrap(~ ano_publicacao, ncol = 3) + 
  labs( y = 'Total de apresentações',
        x = 'Visualizações',
        title = 'Histograma de visualizações facetado por ano')

