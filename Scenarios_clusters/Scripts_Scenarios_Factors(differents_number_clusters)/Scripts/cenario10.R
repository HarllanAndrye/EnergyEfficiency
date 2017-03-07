#
# Cenário 10: Feedback + NIALM
#     -> Aparelhos[], Feedback[]
#

source("adaptative_cluster.R")
source("functions.R")
source("fb.R")

# Caminho para armazenar os resultados
resultsData <- "../Results/Data/cenario10/"
resultsImages <- "../Results/Images/"

# Quantidade de execuções
qtdExecucoes <- 30


for(numberClusters in 3:8){ # Quantidade de clusters
  print(paste("Qtd cluster = ", numberClusters, sep = ""))
  # Número da execução
  execucao = 1
  # Definição do nome dos arquivos que os resultados da execuções vão ser armazenados
  nomeArquivo1 = paste(resultsData, numberClusters, "/rec_exec", execucao, ".csv", sep="") # Recomendações
  nomeArquivo2 = paste(resultsData, numberClusters, "/dados_exec", execucao, ".csv", sep="") # Usuários
  
  
  #---------------------------
  # 1) pre-processa os dados
  #---------------------------
  # Arquivo com os dados dos usuários
  dados = read.csv("../Database/Questionario(algoritmo_mapeado).csv",  header=T,sep=",")
  dados$X <- seq(1:nrow(dados))
  
  # Arquivo com as recomendações
  rec = read.csv("../Database/recomendacoes.csv",head = T)
  
  dados2 = dados
  dados$Bairro <- dados$Consumo <- dados$Valor_conta <- dados$X <- dados$Perfil <- NULL
  rownames(dados) <- dados2$X # Isso serve para o feedback automático, para mapear os ids dos usuários
  
  # Alterando as informações dos aparelhos: deixar com 0 (não possui) ou 1 (possui).
  for(i in 3:ncol(dados)){
    dados[which(dados[i] > 0), i] <- 1
  }
  
  
  # Acrescentando o vetor de recomendações (Feedback)
  ultimaColuna <- ncol(dados)
  for(i in 1:nrow(rec)){
    dados[,(ncol(dados)+1)] = 0
    colnames(dados)[i+ultimaColuna] = as.character(rec[i,1])
  }
  
  dadosOrig <- dados2 # Dados iniciais do arquivo .csv
  
  
  #---------------------------
  # 2) agrupamento dos dados
  #---------------------------
  # Primeira execução
  pesos = c()
  pesos[1:ncol(dados)] = 1/ncol(dados)
  normDados = normData(dados)
  normDados = changeWeigth(normDados,pesos)
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
  # 3) Recomenda
  #---------------------------
  g = c()
  g2 = c()
  for(i in 1:numberClusters){
    recomendacoes = getRecomendacoes(rec=rec)
    g = cbind(g,as.character((recomendacoes[,1])))
    g2 = rbind(g2,recomendacoes) 
  }
  write.csv(g2,nomeArquivo1)
  
  
  #---------------------------
  # 4) Feedback
  #---------------------------
  feedback = generate_feedback(execucao, paste(resultsData, numberClusters, "/", sep = "")) # Feedback automático com base nos gostos dos usuários
  feedback = as.data.frame(t(feedback))
  for(i in 1:nrow(dados)){
    recUser = g[,normDados[i, ]$cluster]
    dados[i, ] = changeFeedback(feedback[i,],recUser,dados[i,])
  }
  
  
  #---------------------------
  # 5) Atualiza os pesos
  #---------------------------
  erro_past = 0
  erro = c(0,0,0,0,0,0,0,0,0,0) # Erro em relação ao feedback
  
  for(i in 1:nrow(feedback)){
    erro[i] = sum(feedback[i, ]==1)/10
  }
  
  # Deixando apenas o feedback e nialm
  dados$Latitude <- dados$Longitude <- NULL
  normDados$Latitude <- normDados$Longitude <- NULL
  
  # Para a primeira execução 
  alfa = 0.5
  pesos = c()
  pesos[1:ncol(dados)] = 1/ncol(dados)
  
  alfa = setAlfa(mean(erro_past),mean(erro),alfa)
  pesos = attWeight(pesos,calcGradient(erro,pesos,normDados,cl$centers),alfa) 
  write.table(rbind(pesos), file= paste(resultsData, numberClusters, "/pesos.csv", sep = ""), sep=",")
  
  
  #---------------------------
  # Demais execuções
  #---------------------------
  for(execucao in 2:qtdExecucoes){
    #print(execucao)
    nomeArquivo1 = paste(resultsData, numberClusters, "/rec_exec", execucao, ".csv", sep="")
    nomeArquivo2 = paste(resultsData, numberClusters, "/dados_exec", execucao, ".csv", sep="")
  
    # 2) agrupamento dos dados
    normDados = normData(dados)
    normDados = changeWeigth(normDados,pesos)
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
    
    # 3) Recomenda
    g = c()
    g2 = c()
    for(i in 1:numberClusters){
      recomendacoes = getRecomendacoes(rec=rec)
      g = cbind(g,as.character((recomendacoes[,1])))
      g2 = rbind(g2,recomendacoes) 
    }
    write.csv(g2,nomeArquivo1)  
  
    # 4) Feedback
    feedback = generate_feedback(execucao, paste(resultsData, numberClusters, "/", sep = ""))
    feedback = as.data.frame(t(feedback))
    for(i in 1:nrow(dados)){
      recUser = g[,normDados[i, ]$cluster]
      dados[i, ] = changeFeedback(feedback[i,],recUser,dados[i,])
    }
  
    # 5) Atualiza os pesos
    erro_past = erro
    erro = c(0,0,0,0,0,0,0,0,0,0)
    
    for(i in 1:nrow(feedback)){
      erro[i] = sum(feedback[i, ]==1)/10
    }
    
    alfa = setAlfa(mean(erro_past),mean(erro),alfa)
    pesos = attWeight(pesos,calcGradient(erro,pesos,normDados,cl$centers),alfa) 
    write.table(rbind(pesos), file= paste(resultsData, numberClusters, "/pesos.csv", sep = ""), sep=",", append=TRUE, col.names=FALSE)
  }
  
  # Dunn e Silhouette
  write.csv(d, file = paste(resultsData, numberClusters, "/dunn.csv", sep = ""))
  write.csv(s, file = paste(resultsData, numberClusters, "/silhouette.csv", sep = ""))
  
  png(file = paste(resultsImages, numberClusters, "/cenario10_metricas.png", sep = ""), width = 1110, height = 611)
  par(mfrow=c(1,2))
  plot(d, type = 'l', xlab = "Execuções", ylab = "Dunn Index", main = "Cenário 10", las=1)
  plot(s, type = 'l', xlab = "Execuções", ylab = "Silhouette Width", main = "Cenário 10", las=1)
  dev.off()
  
  write(paste(numberClusters, " clusters, Dunn = ", d[qtdExecucoes], sep = ""), paste(resultsData, "dunn_cenario10.txt", sep = ""), 
        append=TRUE)
  write(paste(numberClusters, " clusters, Silhouette = ", s[qtdExecucoes], sep = ""), paste(resultsData, "silhouette_cenario10.txt", sep = ""), 
        append=TRUE)
  
  
  # ------------------------------ Grupos formados ------------------------------ #
  exec1 <- read.csv(paste(resultsData, numberClusters, "/dados_exec1.csv", sep = ""))
  gg1 <- ggplot(exec1, aes(Longitude, Latitude, col=factor(cluster))) + geom_point(size = 4) + 
    labs(title="Cenário 10 - exec. 1", x="Longitude", y="Latitude") + 
    scale_colour_discrete(name = "Grupos") + 
    geom_text(aes(Longitude, Latitude+0.003, label=X))
  
  exec30 <- read.csv(paste(resultsData, numberClusters, "/dados_exec30.csv", sep = ""))
  exec30 <- cbind(exec1[2:3], exec30)
  gg2 <- ggplot(exec30, aes(Longitude, Latitude, col=factor(cluster))) + geom_point(size = 4) + 
    labs(title="Cenário 10 - exec. 30", x="Longitude", y="Latitude") + 
    scale_colour_discrete(name = "Grupos") + 
    geom_text(aes(Longitude, Latitude+0.003, label=X))
  
  png(file = paste(resultsImages, numberClusters, "/cenario10_grupos.png", sep = ""), width = 1350, height = 550)
  grid.arrange(gg1, gg2, ncol = 2)
  dev.off()
  
  
  write(paste("-- Quantidade = ", numberClusters, " --", sep = ""), paste(resultsData, "grupos_cenario10.txt", sep = ""), append=TRUE)
  # Grupos - Última execução
  qtdCluster <- max(exec30$cluster)
  for(i in 1:qtdCluster){
    # Escrevendo no arquivo...
    write(which(exec30$cluster == i), paste(resultsData, "grupos_cenario10.txt", sep = ""), append=TRUE, ncolumns = 50)
  }
  write("", paste(resultsData, "grupos_cenario10.txt", sep = ""), append=TRUE) # para pular linha
}




