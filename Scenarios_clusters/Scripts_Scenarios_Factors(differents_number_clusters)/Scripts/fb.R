# Gerar o feedback automático de acordo com as ações que os consumidores (usuário) estão dispostos a fazer

# execucao: número da execução (1,2,3,...)
# caminhoDados: caminho onde estão os resultados das execuções
generate_feedback<-function(execucao, caminhoDados){
  users = read.csv(paste(caminhoDados, "dados_exec", execucao, ".csv", sep = ""), header = T)
  recExec = read.csv(paste(caminhoDados, "rec_exec", execucao, ".csv", sep = ""), header = T)
  pop = read.csv("../Database/Questionario(populacao_mapeado).csv", header=T)
  
  feedback = c()
  
  for(i in 1:nrow(users)){
    c = users[i, ]$cluster # Cluster do usuário i
    # Pega as 10 recomendações referentes aos clusters
    if(c > 1){
      rUser = recExec[(((c-1)*10)+1):(((c-1)*10)+10), ]
    }else{
      rUser = recExec[1:10, ]
    }
    
    fbu = c() # feedback do usuário
    
    # TA: Troca de aparelho
    # AF: Aprender funções
    # TL: Trocar lâmpadas
    # MK: Realizar mudanças na casa
    # MC: Mudanças de conforto
    # AL: Atividades de limpeza
    # MR: Mudar rotina
    # MH: Mudar horário atividades
    
    for(j in 1:nrow(rUser)){
      u = pop[pop$Usuario == users[i, ]$X, ] # Pega o usuário do arquivo "população": com os gostos do usuário.
      if(sum(rUser[j,4:ncol(rUser)] == u[, 15:ncol(u)]) == 0){ # Compara as colunas: TA AF TL MK MC AL MR MH
        fbu = rbind(fbu, -1)
      }else{
        categoria = substr(rUser[j,2],0,2) # Retorna: ac OU ch OU ge OU ...
        
        if( ((categoria == "ch") & (u$Chuveiro == 0)) | ((categoria == "ac") & (u$Ar.condicionado == 0)) |
            ((categoria == "mw") & (u$Microondas == 0)) | ((categoria == "fe") & (u$Ferro == 0)) |
            ((categoria == "wm") & (u$Lava_roupas == 0)) ){
          fbu = rbind(fbu, -1)
        }else{
          fbu = rbind(fbu, 1)
        }

      }
    }
    
    feedback = cbind(feedback,fbu)
  }
  
  feedback = as.data.frame(feedback)
  
  write.csv(feedback, paste(caminhoDados, "feedback", execucao, ".csv", sep = ""))
  return(feedback)
}


