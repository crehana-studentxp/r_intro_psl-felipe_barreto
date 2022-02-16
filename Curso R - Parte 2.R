###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###
# CURSO INTRODUÇÃO A R - SCRIPT 2
###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###

# IMPORTANTE
# Há um conjunto de funções incríveis que não são nativas do R, mas foram desen-
# volvidas pela comunidade. Algumas até já tem no R puro, mas com o uso de outros.
# Um pacote muito importante, o qual carrega diversos outros importantes pacotes 
# é o tidyverse. Um dos principais pacotes importados é o dplyr

  
# Pacotes ----------------------------------------------------------------------
install.packages("tidyverse")   # análise, visualização e tratamento de dados
install.packages("kableExtra")  # visualização
library(tidyverse)
library(kableExtra)             

# Documentação
?tidyverse

# Pipe ( %>% ) -----------------------------------------------------------------
summary(iris)
iris %>% summary()
iris %>% head() %>% kbl()

ver <- function(df, n=7, name_table = "Base de Dados"){
  df %>% head(n) %>% 
    kbl(caption = paste("Tabela:",name_table)) %>%
    kable_classic(full_width = F, html_font = "") %>% 
    scroll_box(width = "100%", height = "300px")
}

args(ver)               # ver os argumentos da função
args(kable_classic)     # ver os argumentos da função
ver                     # ver a construção da função

ver(iris)
ver(iris, n=10, name_table = "Dataset Iris")
iris %>% summary() %>% ver()

# Filtros (Nativo do R x select() do pacote dplyr)
iris[iris$Species == 'versicolor' , ] %>% ver()
iris %>% filter(Species == 'versicolor') %>% ver()

# Condicionais ifelse / case_when ----------------------------------------------
classificacao <- ifelse(iris$Sepal.Width > 5, " > 5", " <= 5") # atribuindo a um vetor
iris$Classificacao <- classificacao   # criando uma nova variável a partir do vetor
iris %>% ver()

iris$Largura_Sepal <- case_when(
            iris$Sepal.Width > 5 ~ "L > 5",
            iris$Sepal.Width > 4 & iris$Sepal.Width <= 5 ~ "4 < L <= 5",
            iris$Sepal.Width > 3 & iris$Sepal.Width <= 4 ~ "3 < L <= 4",
            iris$Sepal.Width > 2 & iris$Sepal.Width <= 3 ~ "2 < L <= 3",
            iris$Sepal.Width > 1 & iris$Sepal.Width <= 2 ~ "1 < L <= 2",
            iris$Sepal.Width <= 1 ~ "L <= 1"
            )
iris %>% ver()

# Verificando/Alterando as classes do dataset ----------------------------------

class(iris)                # data.frame
class(iris$Species)        # factor (níveis, não somente textos aleatórios)
class(iris$Sepal.Length)   # numeric (float)
class(iris$Largura_Sepal)  # character

iris$Largura_Sepal  # observe que não consta os níveis relacionados
summary(iris)  # ver diferença entre as variáveis Species e Largura_Sepal

# supondo que eu queira classificar a variável Largura_Sepal
iris$Largura_Sepal <- factor(iris$Largura_Sepal)
class(iris$Largura_Sepal)
iris$Largura_Sepal

# veja que foram criados níveis. Como o R já entendeu a ordem, não precisei 
# especificar os níveis. Contudo, se eu quisesse/precisasse especificar, seria:
iris$Largura_Sepal <- factor(iris$Largura_Sepal,
                             levels = c("L <= 1",
                                        "1 < L <= 2",
                                        "2 < L <= 3",
                                        "3 < L <= 4",
                                        "4 < L <= 5",
                                        "L > 5")
                            )

# Obs: Deve ser na ordem desejada de classificação

iris %>% summary() %>% ver()  # veja diferença entre "Species" e "Largura_Sepal"

# Renomeando variáveis ---------------------------------------------------------
iris <- rename(.data = iris,
               "Comprimento Sepala" = Sepal.Length,
               "Largura Sepala" = Sepal.Width,
               "Comprimento Pétala" = Petal.Length,
               "Largura Pétala" = Petal.Width,
               "Espécies" = Species,
               "Class Largura Sepala" = Largura_Sepal)

iris %>% ver()

# criando um dataframe com as classes das variáveis
df_class <- data.frame("Variável" = iris %>% names(),
                       "Classe da Variável" = c(class(iris$`Comprimento Sepala`),
                                                class(iris$`Largura Sepala`),
                                                class(iris$`Comprimento Pétala`),
                                                class(iris$`Largura Pétala`),
                                                class(iris$`Espécies`),
                                                class(iris$`Classificacao`),
                                                class(iris$`Class Largura Sepala`)
                                                )
                       )

# Looping: for/while -----------------------------------------------------------
# vamos criar uma função para verificar todas as classes de cada variável

for (i in names(iris)){
  cat(paste(i, class(i), " - "))
}

# utilizando um for para a criação do dataframe. Exemplo
length(names(iris))                     # nº de variáveis
1:length(names(iris))                   # sequência 1,2,3,4,5,6
names(iris)[7]                          # nome da variável da posição 7
class(names(iris)[7])                   # classe da variável da posição 7
class(iris$`Comprimento Sepala`)        # classe da variável da posição 7
class(iris[["Comprimento Sepala"]])     # classe da variável da posição 7

classes_cariaveis_iris <- c()   # atribuindo um vetor vazio
for (i in 1:length(names(iris))){
  classes_cariaveis_iris[i] <- class(iris[[i]])
}
classes_cariaveis_iris

# aplicando em um looping
df_class <- data.frame("Variável" = iris %>% names(),
                       "Classe da Variável" = classes_cariaveis_iris)

df_class %>% ver(name_table = "Classes das Variáveis")

# Excluindo uma variável do dataset --------------------------------------------
# não é desejável a variável Classificacao (NULL)
iris$Classificacao <- NULL

iris %>% names()  # novos nomes das variáveis
iris %>% ver()

# Gráficos
plot(iris)
plot(iris$Espécies)
plot(iris$`Class Largura Sepala`)
plot(iris[, c("Espécies", "Class Largura Sepala")])
plot(iris)

#  histograma default do R
hist(iris$`Comprimento Pétala`, 
     main = "Histograma do dataset iris", 
     xlab = "Comprimento das Pétalas",
     ylab = "Frequência",
     col = "tomato", breaks = 30)

# histograma a partir do pacote ggplot2
iris %>% 
  ggplot() +
  geom_histogram(aes(x = `Comprimento Pétala`),
                 fill = "tomato",
                 color = "white") + # cor branca para as bordas
  scale_y_continuous(labels = scales::comma) + # para que os labels sejam separados por vígulas
  labs(x = "Comprimento das Pétalas",
       y = "Frequência") +
  theme_bw()  # background


################################################################################
# ---------------------------------- FIM ------------------------------------- #
################################################################################

