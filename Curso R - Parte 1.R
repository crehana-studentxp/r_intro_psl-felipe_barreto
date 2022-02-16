###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###
# CURSO INTRODUÇÃO A R - SCRIPT 1
###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###.###

# ---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.--
# INICIANDO COM ALGUMAS DICAS
# ---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.---.--

# No R, tudo é um objeto (na memória da máquina)
# Objeto: Novo comando, imagem, palavra, mapa, função, dataset, lista, etc.
# Cada objeto possui uma classe

# O R tem uma sintaxe/gramática própria (literal!)
# O R é case sensitive! (a != A)

# Como em qualquer linguagem de programação, todo comando deve ser passo a passo
# com objetivos racionais

# Como preferência, não utilize acentos nem caracteres especiais, para que não 
# haja problemas com encoding (codificação) nem com transformações dos dados

# Função é um direcionamento que eu dou à máquina.

# Formas de criar/atribuir um objeto: símbolos "=" ou "<-"
# É muito mais comum na comunidade a utilização do símbolo "<-", sendo o "=" re-
# servado para operações matemáticas

# Para casas decimais, o separador é o "."


# Operações Matemáticas --------------------------------------------------------

# Soma / Subtração
2 + 5
2.3 + 1.5
2 - 5
2.3 - 1.5

# Multiplicação / Divisão
2 * 5
2 / 5

soma <- 2 + 3
soma

# Pontos importantes
2**3          # Exponencial
2^3           # Exponencial 
log(2.71828)  # Logaritmo Natual (Euler)
log(10)       # Logartimo Natural de 10
log(5,3)      # Logaritmo de 5 na base 3
log10(10)     # Logaritmo de 10 na base 10

10 %%  3      # Resto da divisão
10 %/% 3      # Parte inteira da
2 == 2        # Comparação de igualdade
2 != 2        # Comparação de diferença
5 > 7         # Maior que
2 < 3         # Menos que
7 >= 6        # Maior ou igual
3 <= 2        # Menor ou igual
1:5           # Sequêsncia (Importante notar que no R começa e termina como dado)

logaritmo_base_3 <- log2(10)
logaritmo_base_3


# Atribuindo objetos -----------------------------------------------------------

objeto.1 <- Introdução a R # Erro!
objeto.1 <- Introdução_a_R # Erro!
objeto.1 <- "Introdução a R"
objeto.1

class(objeto.1)

objeto.1 + 2     # Erro!
objeto.1 + "2"   # Erro!

paste0(objeto.1, 2)
paste0(objeto.1, "2")

obejto.2 <- 5 ; objeto.3 <- 3  # posso dar mais de um comando em uma linha
novo.soma <- obejto.2 + objeto.3

novo.igual <- obejto.2 == objeto.3
novo.desigual <- obejto.2 != objeto.3

# Vetores
vetor.2 <- c(1,2,3,4,5)  # Função concatenate c()
vetor.3 <- c(1:5)
vetor.4 <- c("a","b","c","d","e")
vetor.5 <- c("alto",'baixo',"medio")
vetor.6 <- c(1:5, "a","b","c","d","e") # posso colocar mais de uma classe
vetor.7 <- c(2, NA, 'a', 3, TRUE)


# Operação com vetores
vetor.1 * vetor.2     # Cara posição faz a operação com sua similar
vetor.1 - 2*vetor.2
vetor.1+2 / vetor.2

# Tipos de objeto
summary(vetor.5)
factor(vetor.5, labels = c(2,3,1))  # especifiquei a ordem desejada
sort(vetor.5)                       # agora é possível ordenar

# Observação: NULL é diferente de NA
# NULL: anulando/apagando um registro
# NA: valor faltante (Não é zero!)


# Funções ----------------------------------------------------------------------

# O que é uma função:
# Como achar um manual para a função
?sum
?sqrt

# Linha de comandos para fazer uma ação específica pré-determinada
vetor.8 <- c(1,3,5,2,4,2,3,6,2,6,5,2)

sqrt(4)          # função de raiz quadrada
table(vetor.8)   # função que mostra a distribuição de frequência do vetor

# Supondo que queremos verificar se um número é par (divisão por 2 tem resto 0)
verifica.par <- function(x){
  sim.par <- x %% 2 == 0
  return(sim.par)
}

verifica.par(4)
verifica.par(7)

# Verificando a Moda de uma lista
moda <- function(x){
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
  
}

moda(vetor.8)

# Trabalhando com uma base de dados (Iris -> Nativa do R) ----------------------
iris
View(iris)  # Visualizar o dataset

length(iris)    # número de variáveis
head(iris)      # 6 primeiras linhas do dataset
tail(iris)      # 6 últimas linhas do dataset
nrow(iris)      # número de observações 
ncol(iris)      # número de colunas/varuáveis
names(iris)     # nomes das variáveis
str(iris)       # informações do dataset
summary(iris)   # resumo de algumas estatísticas descritivas

iris$Sepal.Length        # variável Sepal.Length  
iris[["Sepal.Length"]]   # variável Sepal.Length  
iris[1,2]                # dataset[linha, coluna]
iris[,2]                 # todas as linhas da coluna 2
iris[1,]                 # todas as colunas da linha 1

iris[iris$Species == 'setosa' , ]   # somente as linhas com setosa
iris[iris$Species == 'setosa' |     # operador & x |
       iris$Species == 'versicolor', ] # somente as linhas com setosa e versicolor
iris[, c("Species","Sepal.Length")]  # somente as colunas desejadas

#supondo que quero separar a base para estudo da especie setosa e sepalas com
#um tamanho entre 4 e 4.5
iris[iris$Sepal.Length >= 4 & 
       iris$Sepal.Length <= 4.5 &
       iris$Species == 'setosa', ]


################################################################################
# ---------------------------------- FIM ------------------------------------- #
################################################################################

