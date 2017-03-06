# Script para agrupar as curvas de carga com observações a cada 1 hora (24 observações - 1 dia).

# Retorna 1 dia (24 obs.)
source("data_houses_1hour_month.R") # Dados NÃO normalizados
# --------------------------------------------------------------- #

# Normalizar os dados (potência)
dayHouse1 <- normDataPerfil(dayHouse1)
dayHouse2 <- normDataPerfil(dayHouse2)
dayHouse3 <- normDataPerfil(dayHouse3)
dayHouse4 <- normDataPerfil(dayHouse4)
dayHouse5 <- normDataPerfil(dayHouse5)
dayHouse6 <- normDataPerfil(dayHouse6)
dayHouse7 <- normDataPerfil(dayHouse7)
dayHouse8 <- normDataPerfil(dayHouse8)
dayHouse9 <- normDataPerfil(dayHouse9)
dayHouse10 <- normDataPerfil(dayHouse10)
dayHouse11 <- normDataPerfil(dayHouse11)
dayHouse12 <- normDataPerfil(dayHouse12)
dayHouse13 <- normDataPerfil(dayHouse13)
dayHouse15 <- normDataPerfil(dayHouse15)
dayHouse16 <- normDataPerfil(dayHouse16)
dayHouse17 <- normDataPerfil(dayHouse17)
dayHouse18 <- normDataPerfil(dayHouse18)
dayHouse19 <- normDataPerfil(dayHouse19)
dayHouse20 <- normDataPerfil(dayHouse20)
dayHouse21 <- normDataPerfil(dayHouse21)



# Unir os dados em um data frame
series <- dayHouse1
series <- cbind(series, dayHouse2$Aggregate, dayHouse3$Aggregate, dayHouse4$Aggregate, dayHouse5$Aggregate,
                dayHouse6$Aggregate, dayHouse7$Aggregate, dayHouse8$Aggregate, dayHouse9$Aggregate,
                dayHouse10$Aggregate, dayHouse11$Aggregate, dayHouse12$Aggregate, dayHouse13$Aggregate,
                dayHouse15$Aggregate, dayHouse16$Aggregate, dayHouse17$Aggregate, dayHouse18$Aggregate,
                dayHouse19$Aggregate, dayHouse20$Aggregate, dayHouse21$Aggregate)
series$Time <- NULL

# Agrupando os perfis de consumo
source("functions_cluster_profile(R).R") # Esse arquivo usa a correlação do R, do TSdist.

# Fixando a quantidade de clusters
qtdCluster <- 5

# ---------- #
# Correlação #
# ---------- #
simCorrelacao <- as.data.frame(correlacao_20U(dayHouse1,dayHouse2,dayHouse3,dayHouse4,dayHouse5,dayHouse6,dayHouse7,dayHouse8,
                                              dayHouse9,dayHouse10,dayHouse11,dayHouse12,dayHouse13,dayHouse15,dayHouse16,
                                              dayHouse17,dayHouse18,dayHouse19,dayHouse20,dayHouse21)
)
simCorrelacao <- data.frame(user1=simCorrelacao$V1, user2=simCorrelacao$V2, user3=simCorrelacao$V3, user4=simCorrelacao$V4, 
                            user5=simCorrelacao$V5, user6=simCorrelacao$V6, user7=simCorrelacao$V7, user8=simCorrelacao$V8, 
                            user9=simCorrelacao$V9, user10=simCorrelacao$V10, user11=simCorrelacao$V11, 
                            user12=simCorrelacao$V12, user13=simCorrelacao$V13, user14=simCorrelacao$V14,
                            user15=simCorrelacao$V15, user16=simCorrelacao$V16, user17=simCorrelacao$V17,
                            user18=simCorrelacao$V18, user19=simCorrelacao$V19, user20=simCorrelacao$V20)

# Agrupamento
#qtdCluster.cor <- qtdCluster <- wssplot(simCorrelacao, nc = nrow(simCorrelacao)-1)
hierarchical(simCorrelacao, qtdCluster) # Dendrogram
k.res.cor <- k.res <- kmeans(simCorrelacao, centers = qtdCluster, nstart = 25)
simCorrelacao$cluster <- k.res$cluster

# Validação
Dist <- as.dist(simCorrelacao[,-(ncol(simCorrelacao))])
cluster.stats(Dist, simCorrelacao$cluster)$dunn # 0.5084191
cluster.stats(Dist, simCorrelacao$cluster)$avg.silwidth # 0.1598314
plot(silhouette(simCorrelacao$cluster, Dist))
# Calculando o MAE
maeSeries(series, k.res$cluster, qtdCluster) # 0.1035672

# Grupos
for(i in 1:qtdCluster){
  print(which(simCorrelacao$cluster == i))
}
# Sequencia: (H = house)
# H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H15,H16,H17,H18,H19,H20,H21
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20

# 1,8
# 6,13
# 11
# 5,9,15,16,18,19,21
# 2,3,4,7,10,12,17,20


# Gráficos

centroids <- c()
for (i in 1:qtdCluster.cor) {
  cluster = which(k.res.cor$cluster == i)
  
  # Centroid de cada grupo
  centroid <- c()
  for(y in 1:24){
    centroid <- rbind( centroid, mean(t(series[y,cluster])) )
  }
  
  # Guardando os centroides
  centroids <- cbind(centroids, centroid)
}
centroids <- as.data.frame(centroids)
centroids$Time <- dayHouse1$Time

gg1 <- ggplot() +
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V1, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 1)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg2 <- ggplot() +
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V2, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 2)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg3 <- ggplot() +
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V3, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 3)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg4 <- ggplot() +
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V4, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 4)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg5 <- ggplot() +
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V5, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 5)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))


grid.arrange(gg1,gg2,gg3,gg4,gg5, nrow = 3, ncol = 2)


# ---------- #
# Euclidiana #
# ---------- #
simEuclid <- as.data.frame(distaciaEuclid_20U(dayHouse1,dayHouse2,dayHouse3,dayHouse4,dayHouse5,dayHouse6,dayHouse7,dayHouse8,
                                              dayHouse9,dayHouse10,dayHouse11,dayHouse12,dayHouse13,dayHouse15,dayHouse16,
                                              dayHouse17,dayHouse18,dayHouse19,dayHouse20,dayHouse21)
)
simEuclid <- data.frame(user1=simEuclid$V1, user2=simEuclid$V2, user3=simEuclid$V3, user4=simEuclid$V4, user5=simEuclid$V5, 
                        user6=simEuclid$V6, user7=simEuclid$V7, user8=simEuclid$V8, user9=simEuclid$V9, user10=simEuclid$V10,
                        user11=simEuclid$V11, user12=simEuclid$V12, user13=simEuclid$V13, user14=simEuclid$V14,
                        user15=simEuclid$V15, user16=simEuclid$V16, user17=simEuclid$V17, user18=simEuclid$V18, 
                        user19=simEuclid$V19, user20=simEuclid$V20)

# Agrupamento
#qtdCluster.euc <- qtdCluster <- wssplot(simEuclid, nc = nrow(simEuclid)-1)
hierarchical(simEuclid, qtdCluster) # Dendrogram
k.res.euc <- k.res <- kmeans(simEuclid, centers = qtdCluster, nstart = 25)
simEuclid$cluster <- k.res$cluster

# Validação
Dist <- dist(simEuclid[,-(ncol(simEuclid))])
cluster.stats(Dist, simEuclid$cluster)$dunn # 0.4757805
cluster.stats(Dist, simEuclid$cluster)$avg.silwidth # 0.1842477
plot(silhouette(simEuclid$cluster, Dist))
# Calculando o MAE
maeSeries(series, k.res$cluster, qtdCluster) # 0.1025181

# Grupos
for(i in 1:qtdCluster){
  print(which(simEuclid$cluster == i))
}
# Sequencia: (H = house)
# H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H15,H16,H17,H18,H19,H20,H21
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20

# 1,8
# 4,7,10,12,17,20
# 2,5,9,15,16,18,19,21
# 11
# 3,6,13


# Gráficos

centroids <- c()
for (i in 1:qtdCluster.euc) {
  cluster = which(k.res.euc$cluster == i)
  
  # Centroid de cada grupo
  centroid <- c()
  for(y in 1:24){
    centroid <- rbind( centroid, mean(t(series[y,cluster])) )
  }
  
  # Guardando os centroides
  centroids <- cbind(centroids, centroid)
}
centroids <- as.data.frame(centroids)
centroids$Time <- dayHouse1$Time


gg6 <- ggplot() +
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V1, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 1)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg7 <- ggplot() +
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V2, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 2)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg8 <- ggplot() +
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V3, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 3)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg9 <- ggplot() +
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V4, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 4)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg10 <- ggplot() +
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V5, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 5)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

grid.arrange(gg6,gg7,gg8,gg9,gg10, nrow = 3, ncol = 2)


# --- #
# DTW #
# --- #
simDtw <- as.data.frame(distanciaDTW_20U(dayHouse1,dayHouse2,dayHouse3,dayHouse4,dayHouse5,dayHouse6,dayHouse7,dayHouse8,
                                         dayHouse9,dayHouse10,dayHouse11,dayHouse12,dayHouse13,dayHouse15,dayHouse16,
                                         dayHouse17,dayHouse18,dayHouse19,dayHouse20,dayHouse21)
)
simDtw <- data.frame(user1=simDtw$V1, user2=simDtw$V2, user3=simDtw$V3, user4=simDtw$V4, user5=simDtw$V5, 
                     user6=simDtw$V6, user7=simDtw$V7, user8=simDtw$V8, user9=simDtw$V9, user10=simDtw$V10,
                     user11=simDtw$V11, user12=simDtw$V12, user13=simDtw$V13, user14=simDtw$V14, user15=simDtw$V15, 
                     user16=simDtw$V16, user17=simDtw$V17, user18=simDtw$V18, user19=simDtw$V19, user20=simDtw$V20)

# Agrupamento
#qtdCluster.dtw <- qtdCluster <- wssplot(simDtw, nc = nrow(simDtw)-1)
hierarchical(simDtw, qtdCluster) # Dendrogram
k.res.dtw <- k.res <- kmeans(simDtw, centers = qtdCluster, nstart = 25)
simDtw$cluster <- k.res$cluster

# Validação
Dist <- as.dist(simDtw[,-(ncol(simDtw))])
cluster.stats(Dist, simDtw$cluster)$dunn # 0.4573559
cluster.stats(Dist, simDtw$cluster)$avg.silwidth # 0.1301027
plot(silhouette(simDtw$cluster, Dist))
# Calculando o MAE
maeSeries(series, k.res$cluster, qtdCluster) # 0.1148038

# Grupos
for(i in 1:qtdCluster){
  print(which(simDtw$cluster == i))
}
# Sequencia: (H = house)
# H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H15,H16,H17,H18,H19,H20,H21
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20

# 2,4,5,15
# 9,16,19
# 6
# 3,7,10,11,12,13,17,18,20,21
# 1,8


# Gráficos

centroids <- c()
for (i in 1:qtdCluster.dtw) {
  cluster = which(k.res.dtw$cluster == i)

  # Centroid de cada grupo
  centroid <- c()
  for(y in 1:24){
    centroid <- rbind( centroid, mean(t(series[y,cluster])) )
  }
  
  # Guardando os centroides
  centroids <- cbind(centroids, centroid)
}
centroids <- as.data.frame(centroids)
centroids$Time <- dayHouse1$Time


gg11 <- ggplot() +
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V1, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 1)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg12 <- ggplot() +
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V2, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 2)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg13 <- ggplot() +
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V3, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 3)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg14 <- ggplot() +
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V4, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 4)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg15 <- ggplot() +
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  #geom_line(data = centroids, aes(x = Time, y = V5, group = 1), size = 1, color = "red") +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 5)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

grid.arrange(gg11,gg12,gg13,gg14,gg15, nrow = 3, ncol = 2)



# Todos agrupamentos
png(filename = "../Results/cluster_20users_mean_1hour_day.png", width = 1920, height = 1080)
grid.arrange(gg1,gg6,gg11, gg2,gg7,gg12, gg3,gg8,gg13, gg4,gg9,gg14, gg5,gg10,gg15, nrow = 5, ncol = 3)
dev.off()




