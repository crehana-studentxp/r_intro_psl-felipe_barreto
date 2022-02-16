###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###
# CURSO INTRODUÇÃO A R - SCRIPT 3
###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###


# PROJETO: Construir a tabela de classificação final do Brasileirão 2018


# Base de dados para o projeto
# https://www.kaggle.com/adaoduque/campeonato-brasileiro-de-futebol?select=campeonato-brasileiro-full.csv

# Bibliotecas
library(tidyverse)

# Importando a base de dados
path <- '/Users/felipebarreto/Desktop/Projetos Pessoais/02. Curso R - Crehana/Base de dados/campeonato-brasileiro-full.csv'
brasileirao <- read_csv(path)

df.temporada <- function(dataset, data_inicio, data_final){

  dataset$Perdedor <- case_when (
    dataset$Vencedor == '-' ~ '-',
    dataset$Vencedor == dataset$Mandante ~ dataset$Visitante,
    dataset$Vencedor == dataset$Visitante ~ dataset$Mandante
  )

  dataset$Pontos_Mandante <- case_when(
    dataset$Vencedor == dataset$Mandante ~ 3,
    dataset$Vencedor == '-' ~ 1,
    dataset$Vencedor == dataset$Visitante ~ 0
  )

  dataset$Pontos_Visitante <- case_when(
    dataset$Pontos_Mandante == 3 ~ 0,
    dataset$Pontos_Mandante == 1 ~ 1,
    dataset$Pontos_Mandante == 0 ~ 3
  )
  
  dataset$venceu_mandante <- case_when(
    dataset$Pontos_Mandante == 3 ~ 1,
    dataset$Pontos_Mandante != 3 ~ 0
  )
  
  dataset$venceu_visitante <- case_when(
    dataset$Pontos_Visitante == 3 ~ 1,
    dataset$Pontos_Visitante != 3 ~ 0
  )
  
  dataset$perdeu_mandante <- case_when(
    dataset$Pontos_Mandante == 0 ~ 1,
    dataset$Pontos_Mandante != 0 ~ 0
  )
  
  dataset$perdeu_visitante <- case_when(
    dataset$Pontos_Visitante == 0 ~ 1,
    dataset$Pontos_Visitante != 0 ~ 0
  )
  
  dataset$empatou_mandante <- case_when(
    dataset$Pontos_Mandante == 1 ~ 1,
    dataset$Pontos_Mandante != 1 ~ 0
  )
  
  dataset$empatou_visitante <- case_when(
    dataset$Pontos_Visitante == 1 ~ 1,
    dataset$Pontos_Visitante != 1 ~ 0
  )

  # Data do Brasileirão 2018: De 14/14/2018 até 02/12/2018
  temporada <- dataset %>% filter(Data >= data_inicio  &  Data <= data_final)
  
  ### Agrupando os times da temporada
  mandante <- temporada %>% group_by(Mandante) %>% 
    summarise("Jogos M" = n(),
              "Pontos M" = sum(Pontos_Mandante),
              "Gols Feitos M" = sum(`Mandante Placar`),
              "Gols Levados M" = sum(`Visitante Placar`),
              "Vitorias M" = sum(venceu_mandante),
              "Derrotas M" = sum(perdeu_mandante),
              "Empates M" = sum(empatou_mandante)
    ) %>% rename(Clube=Mandante)
  
  visitante <- temporada %>% group_by(Visitante) %>% 
    summarise("Jogos V" = n(),
              "Pontos V" = sum(Pontos_Visitante),
              "Gols Feitos V" = sum(`Visitante Placar`),
              "Gols Levados V" = sum(`Mandante Placar`),
              "Vitorias V" = sum(venceu_visitante),
              "Derrotas V" = sum(perdeu_visitante),
              "Empates V" = sum(empatou_visitante)
    ) %>% rename(Clube=Visitante)

  classificacao <- mandante %>% inner_join(visitante, by="Clube")

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
    
  return(classificacao_final)
}

x <- df.temporada(brasileirao, data_inicio = '2017-05-03', data_final = '2017-12-03')


