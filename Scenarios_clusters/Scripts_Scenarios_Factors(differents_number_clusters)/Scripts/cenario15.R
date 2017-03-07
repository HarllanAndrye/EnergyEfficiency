#
# Cenário 15: Perfil
#     -> Similaridade[]
#

source("functions.R")

# Caminho para armazenar os resultados
resultsData <- "../Results/Data/cenario15/"
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
  
  # ******************************************************************************** #
  # Pegando os perfis e calculando a similaridade entre eles
  # Usado a distãncia euclidiana como medida de similaridade, devido aos resultados anteriores, 
  #   comparando várias medidades,a euclidiana foi melhor.
  # ...
  
  # Caminho de todos arquivos (perfis)
  arquivos <- as.character(dados[,ncol(dados)])
  for(x in 1:length(arquivos)){
    arquivos[x] <- paste("../", arquivos[x], sep = "")
  }
  
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
      series <- u
    } else { # Depois, apenas o "consumption"
      series <- cbind(series, u[,2])
    }
  }
  # Renomeando as colunas
  collNames <- c("date")
  for(i in 1:length(arquivos)){
    column <- paste("user", i, sep = "")
    collNames <- cbind(collNames, column)
  }
  colnames(series) <- collNames
  
  # Similaridade (Euclidiana)
  tam <- ncol(series)-1 # -1 pq não conta com a coluna "date"
  matrizSim <- matrix(data = 0, nrow = tam, ncol = tam)
  for(i in 1:tam){
    p1 <- series[,colnames(series) == paste("user", i, sep = "") ]
    
    for(j in 1:tam){
      p2 <- series[,colnames(series) == paste("user", j, sep = "") ]
      matrizSim[i,j] <- EuclideanDistance(p1, p2)
    }
  }
  matrizSim <- as.data.frame(matrizSim)
  colnames(matrizSim) <- collNames[-1]
  # ******************************************************************************** #
  
  dadosOrig <- dados2 <- dados # Dados iniciais do arquivo .csv
  
  # Dados da similaridade entre as séries
  dados <- matrizSim
  
  
  
  #---------------------------
  # 2) agrupamento dos dados
  #---------------------------
  normDados = normData(dados)
  cl <- kmeans(normDados, numberClusters, nstart = 2000)
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
    cl <- kmeans(normDados, numberClusters, nstart = 2000)
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
  #(6 grupos) 3.4.1 = 0.3554580
  #(8 grupos)
  #   g1 = 0.1125818
  #   g2 = 0.4119086
  #   g3 = 0.1163778
  #   g4 = 0.1821055
  #   g5 = 0.0000000
  #   g6 = 0.1353321
  #   g7 = 0.2309179
  #   g8 = 0.4246999
  plot(silhouette(cl$cluster, Dist))
  
  png(file = paste(resultsImages, numberClusters, "/cenario15_metricas.png", sep = ""), width = 1110, height = 611)
  par(mfrow=c(1,2))
  plot(d, type = 'l', xlab = "Execuções", ylab = "Dunn Index", main = "Cenário 15", las=1)
  plot(s, type = 'l', xlab = "Execuções", ylab = "Silhouette Width", main = "Cenário 15", las=1)
  dev.off()
  
  write(paste(numberClusters, " clusters, Dunn = ", d[qtdExecucoes], sep = ""), paste(resultsData, "dunn_cenario15.txt", sep = ""), 
        append=TRUE)
  write(paste(numberClusters, " clusters, Silhouette = ", s[qtdExecucoes], sep = ""), paste(resultsData, "silhouette_cenario15.txt", sep = ""), 
        append=TRUE)
  
  
  # ------------------------------ Grupos formados ------------------------------ #
  series_ <- series[-1]
  
  exec30 <- read.csv(paste(resultsData, numberClusters, "/dados_exec30.csv", sep = ""))
  
  write(paste("-- Quantidade = ", numberClusters, " --", sep = ""), paste(resultsData, "grupos_cenario15.txt", sep = ""), append=TRUE)
  # Grupos - Última execução
  qtdCluster <- max(exec30$cluster)
  for(i in 1:qtdCluster){
    # Escrevendo no arquivo...
    write(which(exec30$cluster == i), paste(resultsData, "grupos_cenario15.txt", sep = ""), append=TRUE, ncolumns = 50)
  }
  write("", paste(resultsData, "grupos_cenario15.txt", sep = ""), append=TRUE) # para pular linha
  
  
  # Perfis agrupados
  png(file = paste(resultsImages, numberClusters, "/cenario15_grupos_perfis.png", sep = ""), width = 1350, height = 550)
  
  # mfrow=c(nr, nc) -- number row, number column
  if(numberClusters == 3){
    par(mfrow=c(1,3))
  } else if(numberClusters == 4){
    par(mfrow=c(2,2))
  } else if(numberClusters == 5 || numberClusters == 6){
    par(mfrow=c(2,3))
  } else if(numberClusters == 7 || numberClusters == 8){
    par(mfrow=c(2,4))
  }
  
  for (i in 1:qtdCluster) {
    cluster = which(exec30$cluster == i)
    matplot(matrix(seq(1,24,1),ncol=1),series_[,cluster],type='l',
            ylab='Consumo',xlab='Horas',main=paste('cluster',i),cex.main=1.5,cex.axis=1.5,
            col=1, lty=1)
  }
  dev.off()
  
  write(paste(numberClusters, " clusters, MAE = ", maeSeries(series_, exec30$cluster, qtdCluster), sep = ""), 
        paste(resultsData, "mae_cenario15.txt", sep = ""), append=TRUE)
}




