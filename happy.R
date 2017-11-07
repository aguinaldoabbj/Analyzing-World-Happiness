
# Preparação do Ambiente

## Definindo diretorio atual como diretorio de trabalho
setwd(system("pwd", intern = T) )

## Carregando bibliotecas básicas(algumas tem que ser instaladas via install_github() )
library(devtools) # necessario para carregar bibliotecas de terceiros. Pacote exige instalacao de varias dependencias no sistema.
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
#library(tidyr)
#suppressMessages(library(dplyr))
#library(RColorBrewer)
#library(tm)
#library(wordcloud)
#library(xtable)
#library(VIM) # vizualização de NAs
#library(mice) # completar dados ausentes
#suppressPackageStartupMessages(library(DescTools))
#suppressMessages(library(psych))
#suppressMessages(library(PerformanceAnalytics))
#suppressMessages(library(plotly))
#library(FactoMineR)
#library(corrplot)
#library(factoextra)
options(warn = -1) # Nao exibir warnings de carregamento de pacotes

# Carregando dados brutos
happy.raw = read.csv("happy2016.csv",stringsAsFactors = FALSE, header = TRUE, na.strings=c("","-","NA"))
# criando DF e nomeando indivíduos com base na coluna Country
happy = data.frame(happy.raw, row.names = happy.raw$Country, check.names = FALSE) 


#Mostrando dataset original
head(happy)

# removendo a colunas que nao serao utilizadas
happy = subset(happy, select = -c(1,5,6)) 
#head(happy)
#renomeando colunas para nome mais "amigável"
names(happy)[4] = "Economy"
names(happy)[6] = "Life.Expectancy"
names(happy)[8] = "Gov.Trust"
names(happy)[10] = "Dystopia.Resid"
#head(happy)
# Reordenando colunas
happy = happy[,c(1,4,5,6,7,8,9,10,3,2)]

# Visualizando dataset de trabalho
head(happy)

#sumário do dataset de trabalho
library(DescTools)
Desc(happy, plotit=F)

## Boxplot usando plotly
library(plotly)
par(mfrow=c(3,1))
plot_ly(happy,x=~Region,
        y=~Happiness.Score,
        type="box",
        boxpoints="all",
        pointpos = -1.8,
        color=~Region)

plot_ly(happy,x=~Region,
        y=~Economy,
        type="box",
        boxpoints="all",
        pointpos = -1.8,
        color=~Region)

plot_ly(happy,x=~Region,
        y=~Happiness.Rank,
        type="box",
        boxpoints="all",
        pointpos = -1.8,
        color=~Region)

# Region e Happiness.Rank serão retirados da análise
# Scatterplot de todas as variáveis relevantes para ter visão geral da correlação entre as variáveis
pairs(happy[,2:9], cex=0.27)

# Gerando matriz de correlacao
happy_corr = corr.test(happy[,2:9])
happy_corr$r

# Plot da análise da correlação 
library(corrplot)
corrplot(happy_corr$r, method="pie", type="upper")

# Chart com o pacote PerformanceAnalytics
library(PerformanceAnalytics)
suppressWarnings(chart.Correlation(happy[,2:8], histogram=TRUE, pch=19))

## Análise PCA com a biblioteca FactoMineR
library("FactoMineR")
happy_PCA = PCA(
                happy,
                #happy[,2:9], 
                scale.unit=TRUE,
                quali.sup=1, 
                quanti.sup=10, 
                axes = c(1,2), 
                graph = F) # a coluna das regiões deve ser omitida da análise
happy_PCA

# Mostrando autovalores
happy_PCA$eig

# Barplot das variâncias
library(factoextra)
fviz_screeplot(happy_PCA)

# Contribuições das variáveis nas primeiras componentes
fviz_pca_contrib(happy_PCA, choice = "var", axes = 1:2)

fviz_pca_contrib(happy_PCA, choice = "ind", axes = 1:2, top=50)
#happy_PCA$ind$contrib[1:10,]

happy_PCA$var$coord

# Gráfico do Mapa de Fatores (váriáveis)
library(ggrepel)
fviz_pca_var(happy_PCA, col.var="contrib", repel = TRUE)+
scale_color_gradient2(low="white", mid="blue", 
                      high="red", midpoint=8)+theme_minimal()

# Gráfico do Mapa de Fatores (indivíduos)
fviz_pca_ind(happy_PCA, repel=TRUE, select.ind = list(contrib = 65), col.ind="contrib")+
scale_color_gradient2(low="white", mid="blue", 
    high="red", midpoint=0.50) + theme_minimal()

fviz_pca_ind(happy_PCA,
             label = "none", 
             habillage = as.factor(happy$Region), 
             #addEllipses = TRUE 
             )

fviz_pca_biplot(happy_PCA,  col.var="contrib", repel = TRUE, select.ind = list(contrib = 50))+
scale_color_gradient2(low="white", mid="blue", 
    high="red", midpoint=0.50) + theme_minimal()
