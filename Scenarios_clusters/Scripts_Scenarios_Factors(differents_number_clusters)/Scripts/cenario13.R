#
# Cenário 13: NIALM
#     -> Aparelhos[]
#

source("functions.R")

# Caminho para armazenar os resultados
resultsData <- "../Results/Data/cenario13/"
resultsImages <- "../Results/Images/"

# Quantidade de execuções
qtdExecucoes <- 30


for(numberClusters in 3:8){ # Quantidade de clusters
  print(paste("Qtd cluster = ", numberClusters, sep = ""))
  # Número da execução
  execucao = 1
  # Definição do nome dos arquivos que os resultados da execuções vão ser armazenados
  nomeArquivo2 = paste(resultsData, numberClusters, "/dados_exec", execucao, ".csv", sep="") # Usuários
  
  #---------------------------
  # 1) pre-processa os dados
  #---------------------------
  # Arquivo com os dados dos usuários
  dados = read.csv("../Database/Questionario(algoritmo_mapeado).csv",  header=T,sep=",")
  dados$X <- seq(1:nrow(dados))
  
  dados2 = dados
  #dados[1:5] <- dados$Perfil <- dados$X <- NULL
  dados[1] <- dados[2] <- dados[3] <- dados[4] <- dados[5] <- dados$Perfil <- dados$X <- NULL
  rownames(dados) <- dados2$X
  
  # Alterando as informações dos aparelhos: deixar com 0 (não possui) ou 1 (possui).
  for(i in 1:ncol(dados)){
    dados[which(dados[i] > 0), i] <- 1
  }
  
  dadosOrig <- dados2 # Dados iniciais do arquivo .csv
  
  
  #---------------------------
  # 2) agrupamento dos dados
  #---------------------------
  normDados = normData(dados)
  cl <- kmeans(normDados, numberClusters, nstart = 500)
  normDados$cluster = cl$cluster
  
  # Salvando os resultados em um arquivo .csv
  dados2 = dados
  dados2$cluster = cl$cluster
  write.csv(dados2, nomeArquivo2)
  
  Dist <- dist(normDados[,-(ncol(normDados))], method = "euclidean")
  # Cálculo do Dunn Index
  d <- c()
  d = rbind(d, dunn(Dist,cl$cluster))
  
  # Cálculo da Silhueta
  s <- c()
  s <- rbind(s, cluster.stats(Dist, cl$cluster)$avg.silwidth)
  
  
  #---------------------------
  # Demais execuções
  #---------------------------
  for(execucao in 2:qtdExecucoes){
    #print(execucao)
    nomeArquivo2 = paste(resultsData, numberClusters, "/dados_exec", execucao, ".csv", sep="")
  
    # 2) agrupamento dos dados
    normDados = normData(dados)
    cl <- kmeans(normDados, numberClusters, nstart = 500)
    normDados$cluster = cl$cluster
  
    # Salvando os resultados em um arquivo .csv
    dados2 = dados
    dados2$cluster = cl$cluster
    write.csv(dados2, nomeArquivo2)
    
    Dist <- dist(normDados[,-(ncol(normDados))], method = "euclidean")
    # Cálculo do Dunn Index
    d = rbind(d, dunn(Dist,cl$cluster))
  
    # Cálculo da Silhueta
    s <- rbind(s, cluster.stats(Dist, cl$cluster)$avg.silwidth)
  }
  
  # Dunn e Silhouette
  write.csv(d, file = paste(resultsData, numberClusters, "/dunn.csv", sep = ""))
  write.csv(s, file = paste(resultsData, numberClusters, "/silhouette.csv", sep = ""))
  
  # Silhueta de cada grupo
  cluster.stats(Dist, cl$cluster)$clus.avg.silwidths
  #(6 grupos) 5.4.4 = 0.2475685
  #(8 grupos)
  #   g1 = 0.33054992
  #   g2 = 0.03038161
  #   g3 = 1.00000000
  #   g4 = 0.46817288
  #   g5 = 0.14302640
  #   g6 = 0.41262013
  #   g7 = 0.19096343
  #   g8 = 0.57322330
  plot(silhouette(cl$cluster, Dist))
  
  png(file = paste(resultsImages, numberClusters, "/cenario13_metricas.png", sep = ""), width = 1110, height = 611)
  par(mfrow=c(1,2))
  plot(d, type = 'l', xlab = "Execuções", ylab = "Dunn Index", main = "Cenário 13", las=1)
  plot(s, type = 'l', xlab = "Execuções", ylab = "Silhouette Width", main = "Cenário 13", las=1)
  dev.off()
  
  write(paste(numberClusters, " clusters, Dunn = ", d[qtdExecucoes], sep = ""), paste(resultsData, "dunn_cenario13.txt", sep = ""), 
        append=TRUE)
  write(paste(numberClusters, " clusters, Silhouette = ", s[qtdExecucoes], sep = ""), paste(resultsData, "silhouette_cenario13.txt", sep = ""), 
        append=TRUE)
  
  
  # ------------------------------ Grupos formados ------------------------------ #
  exec1 <- read.csv(paste(resultsData, numberClusters, "/dados_exec1.csv", sep = ""))
  exec1 <- cbind(dadosOrig[2:3], exec1)
  gg1 <- ggplot(exec1, aes(Longitude, Latitude, col=factor(cluster))) + geom_point(size = 4) + 
    labs(title="Cenário 13 - exec. 1", x="Longitude", y="Latitude") + 
    scale_colour_discrete(name = "Grupos") + 
    geom_text(aes(Longitude, Latitude+0.003, label=X))
  
  exec30 <- read.csv(paste(resultsData, numberClusters, "/dados_exec30.csv", sep = ""))
  exec30 <- cbind(dadosOrig[2:3], exec30)
  gg2 <- ggplot(exec30, aes(Longitude, Latitude, col=factor(cluster))) + geom_point(size = 4) + 
    labs(title="Cenário 13 - exec. 30", x="Longitude", y="Latitude") + 
    scale_colour_discrete(name = "Grupos") + 
    geom_text(aes(Longitude, Latitude+0.003, label=X))
  
  png(file = paste(resultsImages, numberClusters, "/cenario13_grupos.png", sep = ""), width = 1350, height = 550)
  grid.arrange(gg1, gg2, ncol = 2)
  dev.off()
  
  
  write(paste("-- Quantidade = ", numberClusters, " --", sep = ""), paste(resultsData, "grupos_cenario13.txt", sep = ""), append=TRUE)
  # Grupos - Última execução
  qtdCluster <- max(exec30$cluster)
  for(i in 1:qtdCluster){
    # Escrevendo no arquivo...
    write(which(exec30$cluster == i), paste(resultsData, "grupos_cenario13.txt", sep = ""), append=TRUE, ncolumns = 50)
  }
  write("", paste(resultsData, "grupos_cenario13.txt", sep = ""), append=TRUE) # para pular linha
}




