#
# Cenário 1: Características
#     -> Latitude, Longitude, Consumo
#

#source("adaptative_cluster.R")
source("functions.R")

# Caminho para armazenar os resultados
resultsData <- "../Results/Data/cenario1/"
resultsImages <- "../Results/Images/"

# Quantidade de execuções
qtdExecucoes <- 30


for(numberClusters in 3:8){ # Quantidade de clusters

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
  dados = as.data.frame(cbind(dados$Latitude, dados$Longitude, dados$Consumo))
  rownames(dados) <- dados2$X
  
  dadosOrig <- dados2 # Dados iniciais do arquivo .csv
  
  
  #---------------------------
  # 2) agrupamento dos dados
  #---------------------------
  normDados = normData(dados)
  #numberClusters = wss(normDados, nc = nrow(dados)-1)
  cl <- kmeans(normDados, numberClusters, nstart = 200)
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
  # Silhueta de cada grupo
  cluster.stats(Dist, cl$cluster)$clus.avg.silwidths
  
  
  #---------------------------
  # Demais execuções
  #---------------------------
  for(execucao in 2:qtdExecucoes){
    print(execucao)
    
    nomeArquivo2 = paste(resultsData, numberClusters, "/dados_exec", execucao, ".csv", sep="")
  
    # 2) agrupamento dos dados
    normDados = normData(dados)
    #numberClusters = wss(normDados, nc = nrow(dados)-1)
    cl <- kmeans(normDados, numberClusters, nstart = 200)
    normDados$cluster = cl$cluster
  
    # Salvando os resultado em um arquivo .csv
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
  #(3 grupos) 1.1.2 = 0.1763989
  #(6 grupos)
  #   1.4.1 = 0.2336778
  #   1.4.2 = 0.3969678
  #   1.4.3 = 0.2837980
  #   1.4.4 = 0.4326894
  #   1.4.5 = 0.2333135
  #   1.4.6 = 0.3526744
  #(7 grupos) 1.5.5 = 0.2234566
  #(8 grupos)
  #   g1 = 0.4748628
  #   g2 = 0.3148608
  #   g3 = 0.2141983
  #   g4 = 0.2398110
  #   g5 = 0.5338568
  #   g6 = 0.2477181
  #   g7 = 0.3471855
  #   g8 = 0.4108869
  plot(silhouette(cl$cluster, Dist))
  
  png(file = paste(resultsImages, numberClusters, "/cenario1_metricas.png", sep = ""), width = 1110, height = 611)
  par(mfrow=c(1,2))
  plot(d, type = 'l', xlab = "Execuções", ylab = "Dunn Index", main = "Cenário 1", las=1)
  plot(s, type = 'l', xlab = "Execuções", ylab = "Silhouette Width", main = "Cenário 1", las=1)
  dev.off()
  
  write(paste(numberClusters, " clusters, Dunn = ", d[qtdExecucoes], sep = ""), paste(resultsData, "dunn_cenario1.txt", sep = ""), 
        append=TRUE)
  write(paste(numberClusters, " clusters, Silhouette = ", s[qtdExecucoes], sep = ""), paste(resultsData, "silhouette_cenario1.txt", sep = ""), 
        append=TRUE)
  
  
  # ------------------------------ Grupos formados ------------------------------ #
  exec1 <- read.csv(paste(resultsData, numberClusters, "/dados_exec1.csv", sep = ""))
  gg1 <- ggplot(exec1, aes(V2, V1, col=factor(cluster))) + geom_point(size = 4) + 
    labs(title="Cenário 1 - exec. 1", x="Longitude", y="Latitude") + 
    scale_colour_discrete(name = "Grupos") + 
    geom_text(aes(V2,V1+0.003, label=X))
  
  exec30 <- read.csv(paste(resultsData, numberClusters, "/dados_exec30.csv", sep = ""))
  gg2 <- ggplot(exec30, aes(V2, V1, col=factor(cluster))) + geom_point(size = 4) + 
    labs(title="Cenário 1 - exec. 30", x="Longitude", y="Latitude") + 
    scale_colour_discrete(name = "Grupos") + 
    geom_text(aes(V2,V1+0.003, label=X))
  
  png(file = paste(resultsImages, numberClusters, "/cenario1_grupos.png", sep = ""), width = 1350, height = 550)
  grid.arrange(gg1, gg2, ncol = 2)
  dev.off()
  
  
  write(paste("-- Quantidade = ", numberClusters, " --", sep = ""), paste(resultsData, "grupos_cenario1.txt", sep = ""), append=TRUE)
  # Grupos - Última execução
  qtdCluster <- max(exec30$cluster)
  for(i in 1:qtdCluster){
    #print(which(exec30$cluster == i))
    # Escrevendo no arquivo...
    write(which(exec30$cluster == i), paste(resultsData, "grupos_cenario1.txt", sep = ""), append=TRUE, ncolumns = 50)
  }
  write("", paste(resultsData, "grupos_cenario1.txt", sep = ""), append=TRUE) # para pular linha
}





