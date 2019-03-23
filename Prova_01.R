setwd('~/Documents/MIT/Bloco_A/Aula_Analytics_R/Prova_01/')

#----------------------
# 1) TITANIC
#----------------------

df <- read.csv("titanic_train.csv", stringsAsFactors = FALSE)

# Quantas variáveis e observações possui o arquivo?
dim(df)

# Quais são as classes das variáveis?
sapply(df, class)

# Qual é a média dos preços dos tickets?
mean(df$Fare)

# Faça um filtro na tabela e crie dois outros data frames.
# Um para o gênero masculino e o outro para o gênero feminino.
df_fem = df[df$Sex == 'female',]
df_mas = df[df$Sex == 'male',]

# Crie duas listas, uma para informações do dataframe do gênero feminino e outra para o gênero masculino.
# Cada lista deve ser composta de:
## - número total de passageiros
## - número de sobreviventes
## - número de passageiros na primeira classe
## - preço do ticket
## - número de parentes∖filhos
fem = c(
  dim(df_fem)[1],
  dim(df_fem[df_fem$Survived == 1,])[1],
  dim(df_fem[df_fem$Pclass == 1,])[1],
  mean(df_fem$Fare),
  mean(df_fem$SibSp + df_fem$Parch)
)

masc = c(
  dim(df_mas)[1],
  dim(df_mas[df_mas$Survived == 1,])[1],
  dim(df_mas[df_mas$Pclass == 1,])[1],
  mean(df_mas$Fare),
  mean(df_mas$SibSp + df_mas$Parch)
)

# Com base nas listas criadas, responda:
## - Qual gênero teve o maior número de pessoas embarcadas?
## - Qual gênero sobreviveu mais?
## - Qual gênero teve a maior média do número de parentes?
respostas <- c(
  a = ifelse(masc[1] > fem[1], 'male', 'female'),
  b = ifelse(masc[2] > fem[2], 'male', 'female'),
  c = ifelse(masc[5] > fem[5], 'male', 'female')
)


#----------------------
# 2) HUMAN DEVELOPMENT INDEX
#----------------------

df <- read.csv2("Human_development_index_HDI.csv", dec = ".", stringsAsFactors = FALSE, strip.white=TRUE)

# Crie uma função que classifique os países (em uma coluna extra) em 2014 de acordo com a tabela acima.
df$HDI.Class_2014 <- cut(df$Ano_2014,
                         breaks= c(0, 0.533, 0.709, 0.795, Inf),
                         labels=c("baixo", "medio", "alto", "muito alto") )

# Qual país cresceu mais em relação à 2013?
df$HDI_Growth <- with(df, (Ano_2014 - Ano_2013)/Ano_2013)
best <- df[which.max(df$HDI_Growth),c('Country','HDI_Growth')]

# Qual país caiu mais em relação à 2013?
worst <- df[which.min(df$HDI_Growth),c('Country','HDI_Growth')]

# Quantos países estão com classificação baixa?
sum(with(df, HDI.Class_2014 == 'baixo'))

# Qual é a posição do Brasil?
df[df$Country == 'Brazil',c('Country', 'HDI.Rank')]


#----------------------
# 3) DADOS ANP
#----------------------

df <- read.csv2("dados_anp2.csv", stringsAsFactors = FALSE, encoding = 'latin1', na.strings = 'NaN')
df$PRECO_COMPRA <- as.numeric(df$PRECO_COMPRA)

# Use as funções summary() e str() para entender a base
summary(df)
str(df)

# Quantos preços foram coletados?
resposta <- c(
  'compra' = NROW(na.omit(df[,'PRECO_COMPRA'])),
  'venda' = NROW(na.omit(df[,'PRECO_VENDA']))
)

# Crie uma tabela com a frequência de postos por combustível,
# atribua essa tabela à variável “quantidade_postos”.
library(plyr)
quantidade_postos = count(df[,'COMBUSTIVEL'])

# Qual combustível teve menos preços coletados?
quantidade_postos[which.min(quantidade_postos$freq),]

# Qual é o posto com menor preço de venda? É confiável essa fonte?
df[which.min(df$PRECO_VENDA),c('RAZAO_SOCIAL', 'FORNECEDOR', 'BANDEIRA', 'PRECO_VENDA', 'COMBUSTIVEL')]

# Crie o dataframe “dados_etanol”, como um filtro do dataframe anp.
# Apresente “dados_etanol” por UF e média dos preços de venda do etanol
dados_etanol <- df[df$COMBUSTIVEL == 'Etanol',]
precos_UF <- aggregate(dados_etanol$PRECO_VENDA, list(dados_etanol$UF), mean)

# Qual é o estado com a menor média de preços de venda do etanol. Isso faz sentido?
precos_UF[which.min(precos_UF$x),]

# Exporte este último dataframe no formato CSV
write.csv(dados_etanol, file = "dados_etanol.csv")


#----------------------
# 4) DIETAS DE GALINHAS
#----------------------

df <- chickwts

# Qual dieta apresentou maior média de peso?
diet <- aggregate(df$weight, list(df$feed), mean)
diet[which.max(diet$x),]

# Qual dieta apresentou maior homogeneidade dos pesos?
diet <- aggregate(df$weight, list(df$feed), var)
diet[which.min(diet$x),]

# Qual dieta seria escolhida para aumento de peso?
boxplot(weight~feed, data=df, main="Chicken weight distribution by Diets")


#----------------------
# 5) UM ESTUDO SOBRE SALÁRIOS DE JOGADORES DE BASEBALL
#----------------------
# Instalar o pacote ISLR
install.packages('ISLR')
library(ISLR)

# Chamar a base “Hitters”
df <- Hitters

# Histograma e boxplot dos salários
hist(df$Salary, xlab="Salaries ($k)", main="Histogram of baseball players' salaries")
boxplot(x=df$Salary, data=df, main="Boxplot of baseball players' salaries")

# Analise os valores faltantes
df[is.na(df$Salary),]

# Qual liga apresenta os maiores salários?
boxplot(Salary~NewLeague, data=df, main="Boxplot of salaries by League")

# Qual divisão apresenta os maiores salários?
boxplot(Salary~Division, data=df, main="Boxplot of salaries by Division")

# Quais variáveis quantitativas apresentam maior correlação com o salário?
library(corrplot)
library(reshape2)
library(ggplot2)
df_quant <- na.omit(df[,sapply(df,is.numeric)])
foo <- melt(df_quant, "Salary")

ggplot(foo) +  geom_point(aes(value, Salary)) + facet_wrap(~variable, ncol=4, scales="free_x", as.table = TRUE)
