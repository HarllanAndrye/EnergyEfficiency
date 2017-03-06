# ------------------------------------------------------- #
# --- Agrupando os perfis e calculando a curva típica --- #
# ------------------------------------------------------- #

# Para 20 usuários
# Retorna 1 dia (24 obs. - dados a cada 1 hora)
source("data_houses_1hour_month.R") # Dados NÃO normalizados

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

# Gráficos (example_typical_real_load.png)
dados <- c(4,7,10,12,17,20)

png(filename = "../Results/example_typical_real_load.png", width = 1920, height = 1080) # Salvando imagem

par(mar=c(6, 6, 3, 2)) # http://research.stowers-institute.org/efg/R/Graphics/Basics/mar-oma/index.htm

matplot(matrix(seq(1,24,1),ncol=1), series[,dados], type='l',
        ylab='Consumo normalizado', xlab='Hora', main='Curvas de carga', cex.main=3, cex.axis=1.5, las=1, cex.lab=3,
        lty=1, col=1)

centroid <- c()
for(y in 1:24){
  centroid <- rbind( centroid, mean(t(series[y,dados])) )
}
matlines(matrix(seq(1,24,1),ncol=1), centroid, type='l', col = 'red', lwd=4)

legend("topleft", inset=.05, lty = c(1,1), lwd = c(2.5,2.5), legend=c("Típica","Real"), col=c("Red", "Black"), cex = 3)

dev.off()




# ------------------------ Exemplo com curvas de carga do REFIT e sintéticas ------------------------ #
dados <- c(4,7,10,12,17,20)
colnames(series[,dados])
# [1] "dayHouse4$Aggregate"  "dayHouse7$Aggregate"  "dayHouse10$Aggregate" "dayHouse12$Aggregate" "dayHouse18$Aggregate"
# [6] "dayHouse21$Aggregate"

series$date <- c("00","01","02","03","04","05","06","07","08","09","10","11",
                 "12","13","14","15","16","17","18","19","20","21","22","23")

# Calculando o centróide
centroid <- c()
for(i in 1:24){
  centroid <- rbind( centroid, mean( t( series[i,dados] ) ) )
}
centroid <- as.data.frame(centroid)
centroid$date <- series$date

# Gráfico - example_typical_real_load.png (845x501)
ggRefit <- ggplot() + 
  geom_line(data = series, aes(x = date, y = dayHouse4$Aggregate, group = 1, colour = "series")) +
  geom_line(data = series, aes(x = date, y = dayHouse7$Aggregate, group = 1, colour = "series")) +
  geom_line(data = series, aes(x = date, y = dayHouse10$Aggregate, group = 1, colour = "series")) +
  geom_line(data = series, aes(x = date, y = dayHouse12$Aggregate, group = 1, colour = "series")) +
  geom_line(data = series, aes(x = date, y = dayHouse18$Aggregate, group = 1, colour = "series")) +
  geom_line(data = series, aes(x = date, y = dayHouse21$Aggregate, group = 1, colour = "series")) +
  geom_line(data = centroid, aes(x = date, y = V1, group = 1, colour = "centroid"), size = 1.4) +
  xlab('Hora') + ylab('Consumo normalizado') +
  ggtitle('Curvas de carga agrupadas') + 
  scale_color_manual(name = element_blank(), breaks=c("series", "centroid"), 
                     labels = c("Real", "Típica"), values = c('red', 'black')) +
  theme_bw() +
  theme(title = element_text(size = 10), legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) #legend.position="none"


# ----------------- #
# Perfis sintéticos #
# ----------------- #

pathData <- "../../Consumption_Profile/Database/"
# Caminho de todos arquivos (perfis), apenas mês Janeiro
arquivos = list.files(pathData, pattern="*-01.csv", full.names=TRUE)


# Colocando todos os dados em uma tabela
# 1 dia (média)
for(i in 1:(length(arquivos))){
  # Lendo 1 arquivo
  u <- read.csv(arquivos[i], header = TRUE, sep = ",")

  # Obtendo apenas 1 dia, calculando a média das horas
  u$date <- substr(u[ ,1],12,13) # Retirando o dia
  u <- aggregate(u[,2], list("date" = u[,1]), FUN = mean)
  u <- data.frame(date=u$date, consumption=u$x)
  u <- normDataPerfil(u) # Normalizando o consumo
  
  # colocando na tabela
  if( i==1 ){ # No 1º passo recebe "date" e "consumption"
    seriesA <- u
  } else { # Depois, apenas o "consumption"
    seriesA <- cbind(seriesA, u[,2])
  }
}

# Renomeando as colunas
collNames <- c("date")
for(i in 1:length(arquivos)){
  # Obtendo o id das curvas de carga
  id <- substr(arquivos[i], 81, nchar(arquivos[i])-12)
  column <- paste("consumption", id, sep = "")
  
  # Colocando o nome da coluna em um vetor
  collNames <- cbind(collNames, column)
}
colnames(seriesA) <- collNames


# Curvas: 6, 17 e 18
# Descobrindo os indices da curvas que serão utilizadas
which(colnames(seriesA) == "consumption6")
# [1] 106
which(colnames(seriesA) == "consumption18")
# [1] 60
which(colnames(seriesA) == "consumption17")
# [1] 59

# Calculando o centróide
dadosA <- c(106,59,60)
colnames(seriesA[,dadosA])
centroidA <- c()
for(i in 1:24){
  centroidA <- rbind( centroidA, mean( t( seriesA[i,dadosA] ) ) )
}
centroidA <- as.data.frame(centroidA)
centroidA$date <- seriesA$date


ggSintetico <- ggplot() +
  geom_line(data = seriesA, aes(x = date, y = consumption6, group = 1, colour = "seriesA")) +
  geom_line(data = seriesA, aes(x = date, y = consumption17, group = 1, colour = "seriesA")) +
  geom_line(data = seriesA, aes(x = date, y = consumption18, group = 1, colour = "seriesA")) +
  geom_line(data = centroidA, aes(x = date, y = V1, group = 1, colour = "centroidA"), size = 1.4) +
  xlab('Hora') + ylab('Consumo normalizado') +
  ggtitle('Curvas de carga agrupadas') + 
  scale_color_manual(name = element_blank(), breaks=c("seriesA", "centroidA"), 
                     labels = c("Real", "Típica"), values = c('red', 'black')) +
  theme_bw() +
  theme(title = element_text(size = 10), legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))



# exemplo_curvas_tipicas_cap2.png (1030x290)
png(filename = "../Results/exemplo_curvas_tipicas_cap2.png", width = 1030, height = 290) # Salvando imagem
grid.arrange(ggRefit, ggSintetico, ncol = 2)
dev.off()










