# Observando a evolução das métricas de validação de cluster em 3 medidade de dissimilaridade:
#       Correlação, Distância euclidiana e DTW
#
# Quantidade de curvas de carga: 148
# Métricas: Dunn, Silhuette, MAE
# Amostra: 1º dia de Janeiro (24 observações)
#

source('functions.R')

# Perfis de consumo
pathData <- "../Database/"

# Caminho de todos arquivos (perfis), apenas o mês Janeiro
arquivos = list.files(pathData, pattern="*-01.csv", full.names=TRUE)


# Colocando todos os dados em uma tabela
# Apenas 1º dia de Janeiro
for(i in 1:(length(arquivos))){
  # Lendo 1 arquivo
  u <- read.csv(arquivos[i], header = TRUE, sep = ",")
  u[,1] <- as.POSIXct(u[,1], origin="1970-01-01")
  
  # Obtendo apenas 1 dia (1º de Janeiro)
  x <- c()
  for(j in 1:24){
    x <- rbind( x, which( u$date == as.POSIXct( paste("2014-01-01 ", (j-1),":00:00", sep = "") ) ) )
  }
  u <- u[x,]
  # Normalizando o consumo
  u <- normDataPerfil(u)
  
  # Adicionando na tabela
  if( i==1 ){ # No 1º passo recebe "date" e "consumption"
    series <- u
  } else { # Depois, apenas o "consumption"
    series <- cbind(series, u[,2])
  }
}

# Renomeando as colunas
collNames <- c("date")
for(i in 1:length(arquivos)){
  # Obtendo o id das curvas de carga
  id <- substr(arquivos[i], 35, nchar(arquivos[i])-12)
  column <- paste("consumption", id, sep = "")
  
  # Colocando o nome da coluna em um vetor
  collNames <- cbind(collNames, column)
}
colnames(series) <- collNames


# Distância Euclidiana
distEuclidean2 <- function(serie1, serie2, rowNumber){
  dist_euc <- 0
  for(i in 1:rowNumber){
    dist_euc <- dist_euc + ((serie1[i] - serie2[i])^2)
  }
  
  return(sqrt(dist_euc))
}

# Criando a matriz de similaridade
matriz <- function(tam){
  distMatriz <- matrix(nrow = tam, ncol = tam)
  for(i in 1:tam){ # linha
    for(j in 1:tam){ # coluna
      distMatriz[i,j] <- 0
    }
  }
  
  return(distMatriz)
}

# Função para calcular as métricas de validação em cada medida de dissimilaridade
calculandoMetricas <- function(tam, series, curvAleatorias){
  # -------------------------
  # Calculando a correlação
  matrizSim <- matriz(tam)
  dataUser <- c()
  for(i in 1:tam){
    p1 <- series[,colnames(series) == paste("consumption", curvAleatorias[i], sep = "") ]
    
    # Para o Mae
    dataUser <- rbind(dataUser, p1)
    
    for(j in 1:tam){
      p2 <- series[,colnames(series) == paste("consumption", curvAleatorias[j], sep = "") ]
      matrizSim[i,j] <- CorDistance(p1, p2)
    }
  }
  matrizSim <- as.data.frame(matrizSim)

  # Agrupamento
  qtdCluster <- wssplot(matrizSim, nc = nrow(matrizSim)-1)
  k.res <- kmeans(matrizSim, centers = qtdCluster, nstart = 25)
  matrizSim$cluster <- k.res$cluster
  # Validação
  # -(ncol(matrizSim)): para retirar a última coluna que foi acrescentada "cluster"
  Dist <- as.dist(matrizSim[,-(ncol(matrizSim))])
  dunnCorr <- cluster.stats(Dist, matrizSim$cluster)$dunn
  silhouetteCorr <- cluster.stats(Dist, matrizSim$cluster)$avg.silwidth
  # Calculando o MAE
  maeCorr <- maeSeries(t(dataUser), k.res$cluster, qtdCluster)
  
  
  # -----------------------------------
  # Calculando a distância euclidiana
  matrizSim <- matriz(tam)
  dataUser <- c()
  for(i in 1:tam){
    p1 <- series[,colnames(series) == paste("consumption", curvAleatorias[i], sep = "") ]
    
    dataUser <- rbind(dataUser, p1)
    
    for(j in 1:tam){
      p2 <- series[,colnames(series) == paste("consumption", curvAleatorias[j], sep = "") ]
      matrizSim[i,j] <- distEuclidean2(p1, p2, length(p1))
    }
  }
  matrizSim <- as.data.frame(matrizSim)
  
  # Agrupamento
  qtdCluster <- wssplot(matrizSim, nc = nrow(matrizSim)-1)
  k.res <- kmeans(matrizSim, centers = qtdCluster, nstart = 25)
  matrizSim$cluster <- k.res$cluster
  # Validação
  Dist <- dist(matrizSim[,-(ncol(matrizSim))])
  dunnEuclid <- cluster.stats(Dist, matrizSim$cluster)$dunn
  silhouetteEuclid <- cluster.stats(Dist, matrizSim$cluster)$avg.silwidth
  # Calculando o MAE
  maeEuclid <- maeSeries(t(dataUser), k.res$cluster, qtdCluster)
  
  
  # --------------------------
  # Calculando a métrica DTW
  matrizSim <- matriz(tam)
  dataUser <- c()
  for(i in 1:tam){
    p1 <- series[,colnames(series) == paste("consumption", curvAleatorias[i], sep = "") ]
    
    dataUser <- rbind(dataUser, p1)
    
    for(j in 1:tam){
      p2 <- series[,colnames(series) == paste("consumption", curvAleatorias[j], sep = "") ]
      matrizSim[i,j] <- dtw(p1, p2)$normalizedDistance
    }
  }
  matrizSim <- as.data.frame(matrizSim)
  
  # Agrupamento
  qtdCluster <- wssplot(matrizSim, nc = nrow(matrizSim)-1)
  k.res <- kmeans(matrizSim, centers = qtdCluster, nstart = 25)
  matrizSim$cluster <- k.res$cluster
  # Validação
  Dist <- dist(matrizSim[,-(ncol(matrizSim))])
  dunnDtw <- cluster.stats(Dist, matrizSim$cluster)$dunn
  silhouetteDtw <- cluster.stats(Dist, matrizSim$cluster)$avg.silwidth
  # Calculando o MAE
  maeDtw <- maeSeries(t(dataUser), k.res$cluster, qtdCluster)
  
  result <- c(dunnCorr, silhouetteCorr, dunnEuclid, silhouetteEuclid, dunnDtw, silhouetteDtw, 
              maeCorr, maeEuclid, maeDtw)
  return(result)
}


# Médias
mean_silhouetteCorr <- c()
mean_silhouetteEuclid <- c()
mean_silhouetteDtw <- c()
mean_maeCorr <- c()
mean_maeEuclid <- c()
mean_maeDtw <- c()
# Medianas
median_silhouetteCorr <- c()
median_silhouetteEuclid <- c()
median_silhouetteDtw <- c()
median_maeCorr <- c()
median_maeEuclid <- c()
median_maeDtw <- c()

seq_curvas <- c()
# Variando a quantidade de curvas de carga
qtdUsers <- c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,148)

# Quantidade de execuções
execucoes <- 30

# Sequência das curvas de carga em cada execução
curvasAleatorias <- c()
for(x in 1:execucoes){
  curvasAleatorias <- rbind(curvasAleatorias, sample(1:148, 148))
}

# ----------- Executar 30 vezes ----------- #
for(y in 1:execucoes){
  
  print(paste("Execução:", y))
  
  dunnCorr <- c()
  silhouetteCorr <- c()
  dunnEuclid <- c()
  silhouetteEuclid <- c()
  dunnDtw <- c()
  silhouetteDtw <- c()
  maeCorr <- c()
  maeEuclid <- c()
  maeDtw <- c()
  
  for(i in 1:length(qtdUsers)){
    result <- calculandoMetricas(qtdUsers[i], series, curvasAleatorias[y,])
    # Dunn Index
    dunnCorr <- rbind(dunnCorr, result[1])
    dunnEuclid <- rbind(dunnEuclid, result[3])
    dunnDtw <- rbind(dunnDtw, result[5])
    # Silhouette
    silhouetteCorr <- rbind(silhouetteCorr, result[2])
    silhouetteEuclid <- rbind(silhouetteEuclid, result[4])
    silhouetteDtw <- rbind(silhouetteDtw, result[6])
    # Mean Absolute Error
    maeCorr <- rbind(maeCorr, result[7])
    maeEuclid <- rbind(maeEuclid, result[8])
    maeDtw <- rbind(maeDtw, result[9])
  }
  
  # Médias
  mean_silhouetteCorr <- rbind(mean_silhouetteCorr, mean(silhouetteCorr))
  mean_silhouetteEuclid <- rbind(mean_silhouetteEuclid, mean(silhouetteEuclid))
  mean_silhouetteDtw <- rbind(mean_silhouetteDtw, mean(silhouetteDtw))
  mean_maeCorr <- rbind(mean_maeCorr, mean(maeCorr))
  mean_maeEuclid <- rbind(mean_maeEuclid, mean(maeEuclid))
  mean_maeDtw <- rbind(mean_maeDtw, mean(maeDtw))
  # Medianas
  median_silhouetteCorr <- rbind(median_silhouetteCorr, median(silhouetteCorr))
  median_silhouetteEuclid <- rbind(median_silhouetteEuclid, median(silhouetteEuclid))
  median_silhouetteDtw <- rbind(median_silhouetteDtw, median(silhouetteDtw))
  median_maeCorr <- rbind(median_maeCorr, median(maeCorr))
  median_maeEuclid <- rbind(median_maeEuclid, median(maeEuclid))
  median_maeDtw <- rbind(median_maeDtw, median(maeDtw))
  
}
# ----------------------------------------- #



# --------------------------------------- #
# -------- Gráficos - Resultados -------- #
# --------------------------------------- #

usersQtd <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

#dunnValues <- cbind(dunnCorr, dunnEuclid, dunnDtw)
#dunnValues <- as.data.frame(dunnValues)
#colnames(dunnValues) <- c("dunnCorr", "dunnEuclid", "dunnDtw")
#dunnValues$usersQtd <- usersQtd

silhouetteValues <- cbind(mean_silhouetteCorr, mean_silhouetteEuclid, mean_silhouetteDtw)
silhouetteValues <- as.data.frame(silhouetteValues)
colnames(silhouetteValues) <- c("silhouetteCorr", "silhouetteEuclid", "silhouetteDtw")
silhouetteValues$usersQtd <- usersQtd

maeValues <- cbind(mean_maeCorr, mean_maeEuclid, mean_maeDtw)
maeValues <- as.data.frame(maeValues)
colnames(maeValues) <- c("maeCorr", "maeEuclid", "maeDtw")
maeValues$userQtd <- usersQtd


# ----- Salvando os resultados de cada execução em arquivo ----- #
# Média
write.csv(silhouetteValues, file = "../Results/silhueta_24_observacoes(30_execucoes).csv")
write.csv(maeValues, file = "../Results/mae_24_observacoes(30_execucoes).csv")
# Mediana
silhouetteValues_median <- cbind(median_silhouetteCorr, median_silhouetteEuclid, median_silhouetteDtw)
silhouetteValues_median <- as.data.frame(silhouetteValues_median)
colnames(silhouetteValues_median) <- c("silhouetteCorr", "silhouetteEuclid", "silhouetteDtw")
silhouetteValues_median$usersQtd <- usersQtd

maeValues_median <- cbind(median_maeCorr, median_maeEuclid, median_maeDtw)
maeValues_median <- as.data.frame(maeValues_median)
colnames(maeValues_median) <- c("maeCorr", "maeEuclid", "maeDtw")
maeValues_median$userQtd <- usersQtd

write.csv(silhouetteValues, file = "../Results/silhueta_24_observacoes(30_execucoes - mediana).csv")
write.csv(maeValues, file = "../Results/mae_24_observacoes(30_execucoes - mediana).csv")
# -------------------------------------------------------------- #

# Dunn Index
gg1 <- ggplot() +
  geom_line(data = dunnValues, aes(x = usersQtd, y = dunnCorr, group = 1, colour = "dunnCorr")) +
  geom_line(data = dunnValues, aes(x = usersQtd, y = dunnEuclid, group = 1, colour = "dunnEuclid")) +
  geom_line(data = dunnValues, aes(x = usersQtd, y = dunnDtw, group = 1, colour = "dunnDtw")) +
  xlab('Qtd de usuários (1 dia)') + ylab('Dunn Index') +
  ggtitle('Comparação das medidas de dissimilaridade') + 
  scale_colour_discrete(name = "Métrica") +
  scale_x_discrete(limits = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150))

# Silhouette
gg2 <- ggplot() +
  geom_line(data = silhouetteValues, aes(x = usersQtd, y = silhouetteCorr, group = 1, colour = "silhouetteCorr"), size=1) +
  geom_line(data = silhouetteValues, aes(x = usersQtd, y = silhouetteEuclid, group = 1, colour = "silhouetteEuclid"), size=1) +
  geom_line(data = silhouetteValues, aes(x = usersQtd, y = silhouetteDtw, group = 1, colour = "silhouetteDtw"), size=1) +
  xlab('Quantidade de execuções') + ylab('Silhueta') +
  ggtitle(' ') + 
  scale_colour_discrete(name = element_blank(), breaks=c("silhouetteCorr", "silhouetteDtw", "silhouetteEuclid"), 
                        labels = c("Correlação", "DTW", "Euclidiana")) +
  guides(colour = guide_legend(override.aes = list(size=3))) + theme_bw()

summary(silhouetteValues)
# silhouetteCorr   silhouetteEuclid silhouetteDtw 
# Median :0.1311   Median :0.2150   Median :0.1382
# Mean   :0.1312   Mean   :0.2134   Mean   :0.1379

# MAE
gg3 <- ggplot() +
  geom_line(data = maeValues, aes(x = usersQtd, y = maeCorr, group = 1, colour = "maeCorr"), size=1) +
  geom_line(data = maeValues, aes(x = usersQtd, y = maeEuclid, group = 1, colour = "maeEuclid"), size=1) +
  geom_line(data = maeValues, aes(x = usersQtd, y = maeDtw, group = 1, colour = "maeDtw"), size=1) +
  xlab('Quantidade de execuções') + ylab('MAE') +
  ggtitle(' ') + 
  scale_colour_discrete(name = element_blank(), breaks=c("maeCorr", "maeDtw", "maeEuclid"), 
                        labels = c("Correlação", "DTW", "Euclidiana")) + 
  guides(colour = guide_legend(override.aes = list(size=3))) + theme_bw()

summary(maeValues)
# maeCorr            maeEuclid          maeDtw          
# Median :0.003316   Median :0.003068   Median :0.003488
# Mean   :0.003304   Mean   :0.003060   Mean   :0.003493


# Figura_24Obs_1_jan_10-148_curvas.png (845x500)
g2 <- ggplotGrob(gg2)
g4 <- ggplotGrob(gg3)
g <- rbind(g2, g4, size="first") # Para as imagens ficar com o mesmo tamanho (alinhadas)
g$widths <- unit.pmax(g2$widths, g4$widths)

png(file="../Results/Images/Figura_24Obs_1_jan_10-148_curvas.png", width = 845, height = 500)
grid.newpage()
grid.draw(g)
dev.off()



# ---------------------- #
# Intervalo de confiança #
# ---------------------- #

# Silhouette
t.corr <- t.test(silhouetteValues$silhouetteCorr) # 95 percent confidence interval: 0.1261648 0.1361934
t.dtw <- t.test(silhouetteValues$silhouetteDtw) # 95 percent confidence interval: 0.1343225 0.1414836
t.euclid <- t.test(silhouetteValues$silhouetteEuclid) # 95 percent confidence interval: 0.2094144 0.2173712

df <- data.frame(Medidas = c("Correlação", "DTW", "Euclidiana"),
                 Media = c(mean(silhouetteValues$silhouetteCorr), mean(silhouetteValues$silhouetteDtw),
                           mean(silhouetteValues$silhouetteEuclid)),
                 IC.Inferior = c(t.corr$conf.int[1], t.dtw$conf.int[1], t.euclid$conf.int[1]),
                 IC.Superior = c(t.corr$conf.int[2], t.dtw$conf.int[2], t.euclid$conf.int[2])
)

# Gráfico IC Silhueta
ggIC_sil <- ggplot(df, aes(x=Medidas, y=Media, group=1)) +
  xlab('Medidas') + ylab('Silhueta') +
  ggtitle('IC com 95% (Silhueta - 24 observações)') +
  geom_errorbar(width=.2, aes(ymin=IC.Inferior, ymax=IC.Superior)) +
  geom_point(shape=21, size=2, fill="white") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# MAE
t.mae.corr <- t.test(maeValues$maeCorr) # 95 percent confidence interval: 0.003266525 0.003341585
t.mae.dtw <- t.test(maeValues$maeDtw) # 95 percent confidence interval: 0.003452977 0.003532195
t.mae.euclid <- t.test(maeValues$maeEuclid) # 95 percent confidence interval: 0.003031720 0.003088767

df.mae <- data.frame(Medidas = c("Correlação", "DTW", "Euclidiana"),
                     Media = c(mean(maeValues$maeCorr), mean(maeValues$maeDtw), mean(maeValues$maeEuclid)),
                     IC.Inferior = c(t.mae.corr$conf.int[1], t.mae.dtw$conf.int[1], t.mae.euclid$conf.int[1]),
                     IC.Superior = c(t.mae.corr$conf.int[2], t.mae.dtw$conf.int[2], t.mae.euclid$conf.int[2])
)

# Gráfico IC MAE
ggIC_mae <- ggplot(df.mae, aes(x=Medidas, y=Media, group=1)) +
  labs(title="IC com 95% (MAE - 24 observações)", x="Medidas", y="MAE") +
  geom_errorbar(width=.2, aes(ymin=IC.Inferior, ymax=IC.Superior)) +
  geom_point(shape=21, size=2, fill="white") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# IC_Silhueta_MAE_24_Observacoes.png (850 x 460)
png(file="../Results/Images/IC_Silhueta_MAE_24_Observacoes.png", width = 850, height = 460)
grid.arrange(ggIC_sil, ggIC_mae, ncol = 2)
dev.off()



# -- Todos gráficos juntos (Experimentos e IC) -- #
g2 <- ggplotGrob(gg2)
g4 <- ggplotGrob(gg4)
comparacao <- rbind(g2, g4, size="first")
comparacao$widths <- unit.pmax(g2$widths, g4$widths)

g1 <- ggplotGrob(ggIC_sil)
g3 <- ggplotGrob(ggIC_mae)
ic <- rbind(g1, g3, size="first")
ic$widths <- unit.pmax(g1$widths, g3$widths)

# Figura_24Obs_1_jan_10-148_curvas(IC).png (845x500)
png(file="../Results/Images/Figura_24Obs_1_jan_10-148_curvas(IC).png", width = 845, height = 500)
grid.arrange(comparacao, ic, ncol = 2, widths=c(4/5,2/5),
             top=textGrob("Comparação das medidas de dissimilaridade (24 observações)", gp=gpar(cex=1.5)))
dev.off()


