# Script para agrupar as curvas de carga com observações a cada 10 minutos, dentro de 1 dia (144 observações).

source("data_houses_10minutes.R") # Dados já normalizados

# Gráficos das 20 casas

gg1 <- ggplot() + 
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg2 <- ggplot() + 
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg3 <- ggplot() + 
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg4 <- ggplot() + 
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg5 <- ggplot() + 
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg6 <- ggplot() + 
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg7 <- ggplot() + 
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg8 <- ggplot() + 
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg9 <- ggplot() + 
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg10 <- ggplot() + 
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg11 <- ggplot() + 
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg12 <- ggplot() + 
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg13 <- ggplot() + 
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg14 <- ggplot() + 
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg15 <- ggplot() + 
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg16 <- ggplot() + 
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg17 <- ggplot() + 
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg18 <- ggplot() + 
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg19 <- ggplot() + 
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

gg20 <- ggplot() + 
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Date') + ylab('Power') + ggtitle('1 day of January')

grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, gg9, gg10, 
             gg11, gg12, gg13, gg14, gg15, gg16, gg17, gg18, gg19, gg20,
             nrow = 4, ncol = 5)




# Unir os dados em um data frame
series <- dayHouse1
series <- cbind(series, dayHouse2$Aggregate, dayHouse3$Aggregate, dayHouse4$Aggregate, dayHouse5$Aggregate,
                dayHouse6$Aggregate, dayHouse7$Aggregate, dayHouse8$Aggregate, dayHouse9$Aggregate,
                dayHouse10$Aggregate, dayHouse11$Aggregate, dayHouse12$Aggregate, dayHouse13$Aggregate,
                dayHouse15$Aggregate, dayHouse16$Aggregate, dayHouse17$Aggregate, dayHouse18$Aggregate,
                dayHouse19$Aggregate, dayHouse20$Aggregate, dayHouse21$Aggregate)
series$Time <- NULL

# Fixando a quantidade de clusters
qtdCluster <- 5

# Agrupando os perfis de consumo
source("functions_cluster_profile.R")

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

for(i in 1:nrow(simCorrelacao)){ # linha
  for(j in 1:ncol(simCorrelacao)){ # coluna
    simCorrelacao[i,j] <- sqrt( (1-simCorrelacao[i,j])/2 )
  }
}


# Agrupamento
#qtdCluster <- wssplot(simCorrelacao, nc = nrow(simCorrelacao)-1)
hierarchical(simCorrelacao, qtdCluster) # Dendrogram
k.res <- kmeans(simCorrelacao, centers = qtdCluster, nstart = 25)
simCorrelacao$cluster <- k.res$cluster

# Validação
Dist <- as.dist(simCorrelacao[,-(ncol(simCorrelacao))])
cluster.stats(Dist, simCorrelacao$cluster)$dunn # 0.4949157
cluster.stats(Dist, simCorrelacao$cluster)$avg.silwidth # 0.266
plot(silhouette(simCorrelacao$cluster, Dist))
# Calculando o MAE
maeSeries(series, k.res$cluster, qtdCluster) # 0.1103037

# Grupos
for(i in 1:qtdCluster){
  print(which(simCorrelacao$cluster == i))
}
# Sequencia: (H = house)
# H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H15,H16,H17,H18,H19,H20,H21
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20

# 1
# 15,16,19
# 5
# 4,6,11,21
# 2,3,7,8,9,10,12,13,17,18,20

# Gráficos
gg1 <- ggplot() +
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 1)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg2 <- ggplot() +
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 2)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg3 <- ggplot() +
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 3)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg4 <- ggplot() +
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Correlação (grupo 4)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg5 <- ggplot() +
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
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
#qtdCluster <- wssplot(simEuclid, nc = nrow(simEuclid)-1)
hierarchical(simEuclid, qtdCluster) # Dendrogram
k.res <- kmeans(simEuclid, centers = qtdCluster, nstart = 25)
simEuclid$cluster <- k.res$cluster

# Validação
Dist <- dist(simEuclid[,-(ncol(simEuclid))])
cluster.stats(Dist, simEuclid$cluster)$dunn # 0.3589234
cluster.stats(Dist, simEuclid$cluster)$avg.silwidth # 0.2978361
plot(silhouette(simEuclid$cluster, Dist))
# Calculando o MAE
maeSeries(series, k.res$cluster, qtdCluster) # 0.09944951

# Grupos
for(i in 1:qtdCluster){
  print(which(simEuclid$cluster == i))
}
# Sequencia: (H = house)
# H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H15,H16,H17,H18,H19,H20,H21
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20

# 4,5,8
# 2,3,11,12,18,20,21
# 1
# 6,7,9,10,13,17
# 15,16,19

# Gráficos
gg6 <- ggplot() +
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 1)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg7 <- ggplot() +
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 2)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg8 <- ggplot() +
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 3)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg9 <- ggplot() +
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 4)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg10 <- ggplot() +
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('Euclidiana (grupo 5)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

grid.arrange(gg5,gg6,gg7,gg8,gg9,gg10, nrow = 3, ncol = 2)


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
#qtdCluster <- wssplot(simDtw, nc = nrow(simDtw)-1)
hierarchical(simDtw, qtdCluster) # Dendrogram
k.res <- kmeans(simDtw, centers = qtdCluster, nstart = 25)
simDtw$cluster <- k.res$cluster

# Validação
Dist <- dist(simDtw[,-(ncol(simDtw))])
cluster.stats(Dist, simDtw$cluster)$dunn # 0.3128908
cluster.stats(Dist, simDtw$cluster)$avg.silwidth # 0.2319691
plot(silhouette(simDtw$cluster, Dist))
# Calculando o MAE
maeSeries(series, k.res$cluster, qtdCluster) # 0.1008105

# Grupos
for(i in 1:qtdCluster){
  print(which(simDtw$cluster == i))
}
# 1,4,5
# 16
# 6,7,9,10,13,17,18,21
# 15,19
# 2,3,8,11,12,20

# Gráficos
gg11 <- ggplot() +
  geom_line(data = dayHouse1, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse4, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse5, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 1)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg12 <- ggplot() +
  geom_line(data = dayHouse16, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 2)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg13 <- ggplot() +
  geom_line(data = dayHouse6, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse7, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse9, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse10, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse13, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse17, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse18, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse21, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 3)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg14 <- ggplot() +
  geom_line(data = dayHouse15, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse19, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 4)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

gg15 <- ggplot() +
  geom_line(data = dayHouse2, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse3, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse8, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse11, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse12, aes(x = Time, y = Aggregate, group = 1)) +
  geom_line(data = dayHouse20, aes(x = Time, y = Aggregate, group = 1)) +
  xlab('Hora (1 dia)') + ylab('Consumo normalizado') +
  ggtitle('DTW (grupo 5)') + theme_bw() +
  theme(title = element_text(size = 18), axis.title = element_text(size = 15))

grid.arrange(gg11,gg12,gg13,gg14,gg15, nrow = 3, ncol = 2)


# Todos os agrupamentos
png(filename = "../Results/cluster_20users_mean_10min_1day.png", width = 1920, height = 1080)
grid.arrange(gg1,gg6,gg11, gg2,gg7,gg12, gg3,gg8,gg13, gg4,gg9,gg14, gg5,gg10,gg15, nrow = 5, ncol = 3)
dev.off()




