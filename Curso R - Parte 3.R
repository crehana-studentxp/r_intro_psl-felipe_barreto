###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###
# CURSO INTRODUÇÃO A R - SCRIPT 3
###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###


# PROJETO: Construir a tabela de classificação final do Brasileirão 2018


# Base de dados para o projeto
# https://www.kaggle.com/adaoduque/campeonato-brasileiro-de-futebol?select=campeonato-brasileiro-full.csv

# Bibliotecas
library(tidyverse)
library(kableExtra)
library(plotly) # Gráficos interativos
library(PerformanceAnalytics) # correlação
library(reshape2) # melt

# Função de visualização
ver <- function(df, n=7, name_table = "Base de Dados"){
  df %>% head(n) %>% 
    kbl(caption = paste("Tabela:",name_table)) %>%
    kable_classic(full_width = F, html_font = "")%>% 
    scroll_box(width = "100%", height = "300px")
}

# Importando a base de dados
path <- '/Users/felipebarreto/Desktop/Projetos Pessoais/02. Curso R - Crehana/Base de dados/campeonato-brasileiro-full.csv'
brasileirao <- read_csv(path)

#veja que já aparecem as classes de cada variável

# Conhecendo a base de dados
brasileirao %>% ver()
brasileirao %>% dim()

brasileirao %>% View()
brasileirao %>% summary() %>% ver()

brasileirao %>% str()

# Observe que o R já reconheceu o formato de data para Date

# Time perdedor
brasileirao$Perdedor <- case_when (
  brasileirao$Vencedor == '-' ~ '-',
  brasileirao$Vencedor == brasileirao$Mandante ~ brasileirao$Visitante,
  brasileirao$Vencedor == brasileirao$Visitante ~ brasileirao$Mandante
  )

# Pontuação: venceu = 3, empatou = 1, perdeu = 0
brasileirao$Pontos_Mandante <- case_when(
  brasileirao$Vencedor == brasileirao$Mandante ~ 3,
  brasileirao$Vencedor == '-' ~ 1,
  brasileirao$Vencedor == brasileirao$Visitante ~ 0
)

brasileirao$Pontos_Visitante <- case_when(
  brasileirao$Pontos_Mandante == 3 ~ 0,
  brasileirao$Pontos_Mandante == 1 ~ 1,
  brasileirao$Pontos_Mandante == 0 ~ 3
)

brasileirao$venceu_mandante <- case_when(
  brasileirao$Pontos_Mandante == 3 ~ 1,
  brasileirao$Pontos_Mandante != 3 ~ 0
)

brasileirao$venceu_visitante <- case_when(
  brasileirao$Pontos_Visitante == 3 ~ 1,
  brasileirao$Pontos_Visitante != 3 ~ 0
)

brasileirao$perdeu_mandante <- case_when(
  brasileirao$Pontos_Mandante == 0 ~ 1,
  brasileirao$Pontos_Mandante != 0 ~ 0
)

brasileirao$perdeu_visitante <- case_when(
  brasileirao$Pontos_Visitante == 0 ~ 1,
  brasileirao$Pontos_Visitante != 0 ~ 0
)

brasileirao$empatou_mandante <- case_when(
  brasileirao$Pontos_Mandante == 1 ~ 1,
  brasileirao$Pontos_Mandante != 1 ~ 0
)

brasileirao$empatou_visitante <- case_when(
  brasileirao$Pontos_Visitante == 1 ~ 1,
  brasileirao$Pontos_Visitante != 1 ~ 0
)

View(brasileirao)

# Data do Brasileirão 2018: De 14/14/2018 até 02/12/2018
temp_2018 <- brasileirao %>% filter(Data >= '2018-04-14' & 
                                    Data <= '2018-12-02')

# Garantindo que e os dados estão completos
temp_2018 %>% group_by(Mandante) %>% count() %>% ver(n=20)
temp_2018 %>% dim() # 380 jogos (19*20)

### Agrupando os times da temporada
mandante <- temp_2018 %>% group_by(Mandante) %>% 
  summarise("Jogos M" = n(),
            "Pontos M" = sum(Pontos_Mandante),
            "Gols Feitos M" = sum(`Mandante Placar`),
            "Gols Levados M" = sum(`Visitante Placar`),
            "Vitorias M" = sum(venceu_mandante),
            "Derrotas M" = sum(perdeu_mandante),
            "Empates M" = sum(empatou_mandante)
              ) %>% rename(Clube=Mandante)

visitante <- temp_2018 %>% group_by(Visitante) %>% 
  summarise("Jogos V" = n(),
            "Pontos V" = sum(Pontos_Visitante),
            "Gols Feitos V" = sum(`Visitante Placar`),
            "Gols Levados V" = sum(`Mandante Placar`),
            "Vitorias V" = sum(venceu_visitante),
            "Derrotas V" = sum(perdeu_visitante),
            "Empates V" = sum(empatou_visitante)
            ) %>% rename(Clube=Visitante)

classificacao <- mandante %>% inner_join(visitante, by="Clube")
View(classificacao)

# Classificação com tabela final
classificacao_final <- classificacao %>% 
  mutate("PTS" = `Pontos M` + `Pontos V`,
         "PJ" = `Jogos M` + `Jogos V`,
         "VIT" = `Vitorias M` + `Vitorias V`,
         "E" = `Empates M` + `Empates V`,
         "DER" = `Derrotas M` + `Derrotas V`,
         "GP" = `Gols Feitos M` + `Gols Feitos V`,
         "GC" = `Gols Levados M` + `Gols Levados V`,
         "SG" = (`Gols Feitos M` + `Gols Feitos V`) - 
                (`Gols Levados M` + `Gols Levados V`)
         ) %>% 
  select(Clube,PTS,PJ,VIT,E,DER,GP,GC,SG) %>% 
  arrange(desc(VIT))

## -------------------------------------------------------------------------- ##
# PLOTAGENS

# Boxplot!
ggplotly(
ggplot(classificacao_final) +
  geom_boxplot(aes( y = PTS)) +
  labs(x = "Variáveis",
       y = "Valores") 
)

cor(classificacao_final[,2:9]) %>% melt()

# Correlação - Usando a biblioteca PerformanceAnalytics
chart.Correlation(classificacao_final[,2:9], histogram = T)


## -------------------------------------------------------------------------- ##

## DESAFIO ## 
# Criar uma função que tenha com output a classificação de alguma temporada, em
# que os parãmetros da função sejam as datas de início e fim para determinar a 
# temporada. Exemplo: funcao_temporada(dataset, data_inicio, data_fim).

