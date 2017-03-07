# Script com as funções usadas no processo de agrupamento e adaptação dos pesos ao feedback do consumidor
# Baseado em: https://github.com/Iararibeiro/RecomendadorConsumo


# Altera os pesos
changeWeigth <- function(dados, pesos=NULL){
  p = ncol(dados)
  if(missing(pesos)){
    px = 1/p
    for(i in 1:(ncol(dados))){
      dados[,i] = px*dados[,i]
    }
  }else{
    for(i in 1:ncol(dados)){
      dados[,i] = pesos[i]*dados[,i]
    }
  }
  return(dados)
}



# Funções para calcular os erros a partir do feedback

# ((dados[6,1] - c[1,1])*p1)/(sqrt(2)*p1) * ((e6-1)/2))
calcGradient <- function(erro,pesos,dados,centroides){
  gradient = c()
  
  for(j in 1:(ncol(dados)-1)){
    sum = 0
    for(i in 1:nrow(dados)){
      sum = sum + ((dados[i,j] - centroides[dados$cluster[i],j])*pesos[j])/(sqrt(2)*pesos[j]) * ((erro[i]-1)/2)
    }
    gradient = rbind(gradient,(sum/nrow(dados)))
  }
  return(gradient)
}

attWeight <- function(pesos, gradient,alfa){
  for(i in 1:length(pesos)){
    pesos[i] = pesos[i] - (gradient[i] *alfa)
  }
  
  return(pesos)
}

setAlfa <- function(erro_past,erro_present,alfa){
  if(erro_past > erro_present){
    if(alfa < 0.5) alfa = alfa + 0.1
  }else{
    if(alfa >= 0.2) alfa = alfa - 0.1
  }
  
  if(is.null(alfa))alfa=0.1
  
  return(alfa)
}



# Retorna as recomendações para os consumidores
getRecomendacoes <- function(rec, group=NULL){
  #Se os usuários do grupo não forem passados
  if(missing(group)){
    positions = sample(1:nrow(rec),10,replace = F)
    return(rec[positions,])
  }else{
    #Implementação que pega as 3 recomendações mais populares do grupo
    # POPULAR = as que tiveram mais scores positivos entre os usuários
    score = c()
    for(index in 5:(ncol(group)-1)){
      score <- rbind(score, sum(group[ ,index]))
    }
    score<- as.data.frame(score)
    score$names<- rec$V1
    score<- score[order(-score[,1]), ]
    score<- score[1:5, ]
    
    recUser <- rec[is.element(rec$V1, score$names), ]
    positions = sample(1:nrow(rec),5,replace = F)
    recUser <- rbind(recUser, rec[positions, ])
    return(recUser)
  }
}


# Altera o feedback do consumidor
changeFeedback <- function(feedback, recomendacoes,dados){
  for(i in 1:length(recomendacoes)){
    if(dados[ ,colnames(dados) == recomendacoes[i] ] == 0){
      dados[ ,colnames(dados) == recomendacoes[i] ] = feedback[i]
    }
  }
  return(dados)
}


