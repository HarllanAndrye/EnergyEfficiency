# --------------- Pacotes necessários --------------- #
#install.packages('cluster')
library(cluster)
library(fpc)
library(dtw)

# Medidas de similaridade entre séries temporais
#install.packages('TSdist')
library(TSdist)

#install.packages("reshape")
#library(reshape)

#install.packages('ggplot2')
require(ggplot2)
#install.packages('gridExtra')
require(gridExtra)

library(gtable)
library(grid)
# --------------------------------------------------- #


# Normaliza os dados das curvas de carga com valores entre 0 e 1
normDataPerfil <- function(dados){
  dados[,2] = (dados[,2] - min(dados[,2]))/(max(dados[,2]) - min(dados[,2]))
  return(dados)
}


# Quantidade de clusters (Elbow Method)
#     data: dados que seram agrupados
#     nc: número máximo de grupos
#     seed: um número aleatório
# (http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))

  for (i in 2:nc){
    set.seed(seed)
    # nstart = 25  (This approach is often recommended)
    # withinss: Soma dos quadrados dentro do cluster.
    # Procura-se minimizar a variação ou diferença dentro do mesmo grupo.
    wss[i] <- kmeans(data, centers=i, nstart = 25)$tot.withinss # sum? Poderia usar o tot.withinss?
    #wss[i] <- sum(kmeans(data, centers=i, nstart = 25)$withinss)
  }
  
  # Obter o joelho da curva
  elbow <- max(wss[wss <= mean(wss)])
  clusters <- which(wss == elbow)

  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main = "Elbow Method")
  abline(v=clusters, col=2, lty=2)
  return(clusters)
}


# ------------------------- #
# MAE (Mean Absolute Error) #
#
# Cálculo baseado no artigo: Methods for generating TLPs (typical load profiles) for smart grid-based energy programs.
# tlp = typical load profile (predicted), rlp = real load profile (actual)
#
# Exemplo para chamar a função:
#     maeSeries(series, k.res.cor$cluster, qtdCluster.cor)
#
maeSeries <- function(series,kmeansClusters,qtdCluster){
  
  qtdObs <- nrow(series)
  qtdTotalUsers <- length(series)
  soma3 <- 0
  
  for(k in 1:qtdCluster){ #k
    
    # Índice de cada elemento pertencente ao cluster k
    cluster = which(kmeansClusters == k)
    # Quantidade de elementos no cluster k
    qtdUsersCluster <- length(cluster)
    
    # Curva típica (centróide) do cluster k
    predicted <- c()
    for(p in 1:qtdObs){
      predicted <- rbind( predicted, mean(t(series[p,cluster])) )
    }
    
    soma2 <- 0
    for(y in 1:qtdUsersCluster){ #nCi
      
      actual <- series[,cluster[y]]
      
      error <- actual - predicted
      m <- mean(abs(error)) # MAE
      # OU mae(actual, predicted) # do pacote "library(Metrics)"
      
      soma2 <- soma2 + m
    }
    
    soma3 <- soma3 + soma2
  }
  maeS <- soma3/qtdTotalUsers
  
  return(maeS)
}


