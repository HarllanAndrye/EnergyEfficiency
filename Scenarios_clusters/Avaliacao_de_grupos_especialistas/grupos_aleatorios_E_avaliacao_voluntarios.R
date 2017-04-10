# ----------------- #
# Grupos aleatórios #
# ----------------- #

# Residências
res <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
         31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)

# verificando se há duplicados
verificaDupli <- function(res){
  numbers <- sample(res, 6)
  
  cont <- 0
  for (i in 1:length(numbers)) {
    for(j in 1:length(numbers)){
      if(numbers[i] == numbers[j]){
        cont <- cont + 1
      }
    }
  }
  result <- c(cont, numbers)
  return(result)
}

grupos <- c()

for(x in 1:7){
  
  cont <- verificaDupli(res)
  while(cont[1] > length(cont[-1])){ # se há duplicados, repete o processo de random
    cont <- verificaDupli(res)
  }
  
  grupos <- rbind(grupos, cont[-1])
  
  res <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
           31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)
  
  res <- res[-grupos]
  
}

grupos <- rbind(grupos, c(res,0)) # O zero é descartado, pois não existe residência com id zero.
                                  # É só para completar a tabela grupos (mesma qtd de colunas p/ cada linha).

print(grupos)
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]   11   27   40   46   33   25
# [2,]   39    1   45   38   36   28
# [3,]   19   14   18   29    6   23
# [4,]   41   10   22    7   34   15
# [5,]    3   44   13   31    9   47
# [6,]   42   26    5    8   30    4
# [7,]   37   43   12   35   16    2
# [8,]   17   20   21   24   32    0




# ---------------------------------------- #
# Escolhendo os grupos que serão avaliados #
# ---------------------------------------- #

# O que está entre parênteses é o id do cenário que será utilizado nas avaliações do voluntários.
# Cenários rotulados de 1 a 6.

# (1) Cenário 9 (Feedback)
# (2) Cenário 11 (Feedback + perfil)
# (3) Cenário aleatório
# (4) Cenário 13 (NIALM)
# (5) Cenário 15 (Perfil)
# (6) Cenário 1 (carc. diveras)


# Página 1
p1 <- sample(1:8, 6, replace = T)
# Página 2
p2 <- sample(1:8, 6, replace = T)
# Página 3
p3 <- sample(1:8, 6, replace = T)
# Página 4
p4 <- sample(1:8, 6, replace = T)

paginas <- rbind(p1,p2,p3,p4)
print(paginas) # cada coluna é de um cenário (X)

#    [,1] [,2] [,3] [,4] [,5] [,6]
# p1    1    8    5    8    6    7
# p2    2    4    8    6    2    8
# p3    3    8    8    6    6    6
# p4    1    5    1    2    4    6







