# Analisando os resultados da avaliação dos grupos feitos pelos especialistas.

# Lendo o arquivo com os resultados
results <- read.csv("Avaliação de grupos.csv", header = TRUE, sep = ",")

# Removendo o 1º resultado, pois está errado. O avaliador enviou só para poder registrar sua 1º resposta, mas
#   depois enviou outra resposta (formulário) com os dados das 2 páginas.
results <- results[-1,]

# Removendo a coluna da data do resultado
results$Timestamp <- NULL

# Alterando os nomes das colunas
colnames(results) <- c("Grupo1.1","Grupo2.1","Grupo3.1","Grupo4.1","Grupo5.1","Grupo6.1",
                       "Grupo1.2","Grupo2.2","Grupo3.2","Grupo4.2","Grupo5.2","Grupo6.2")

# Dividindo os resultados de cada página
grupos_p1 <- results[,c(1,2,3,4,5,6)]
grupos_p2 <- results[,c(7,8,9,10,11,12)]

# Retirando o último resultado, pois o avaliador só respondeu a 1º página #
grupos_p2 <- grupos_p2[-5,]


# ----------------------------------------------------------------- #
# ---------------------- Grupos da 1º página ---------------------- #
# ----------------------------------------------------------------- #

# Calculando a média/mediana das notas atribuídas a cada grupo
medias <- c(mean(grupos_p1$Grupo1.1), mean(grupos_p1$Grupo2.1), mean(grupos_p1$Grupo3.1),
            mean(grupos_p1$Grupo4.1), mean(grupos_p1$Grupo5.1), mean(grupos_p1$Grupo6.1))

mediana <- c(median(grupos_p1$Grupo1.1), median(grupos_p1$Grupo2.1), median(grupos_p1$Grupo3.1),
             median(grupos_p1$Grupo4.1), median(grupos_p1$Grupo5.1), median(grupos_p1$Grupo6.1))

# Invertendo a tabela
grupos_p1 <- t(grupos_p1)
grupos_p1 <- as.data.frame(grupos_p1)

# Adicionando as novas colunas de média e mediana
grupos_p1$media <- medias
grupos_p1$mediana <- mediana

# ----- Gráficos ----- #
# -c(6,7)
boxplot(t(grupos_p1[,-c(6,7)]), xlab = "Grupos", ylab = "Classificação", ylim = c(6,1), las = 1,
        main = "Classificação dos grupos")
# Classificação dos grupos (do 1º ao 6º colocado):
#   6,1,4,5,3,2 (Com os 5 resultados)


# 930 x 465 (classificacao_dos_grupos_p1.png)
plot(0, type = "n", main = "Classificação dos grupos", xlab = "Grupos", ylab = "Classificação", 
     xlim = c(1,6), ylim = c(6,1), las = 1)
grid() # add grid
points(grupos_p1[,1], pch = 16)
points(grupos_p1[,2], pch = 16)#, col = 2)
points(grupos_p1[,3], pch = 16)#, col = 3)
points(grupos_p1[,4], pch = 16)#, col = 4)
points(grupos_p1[,5], pch = 16)#, col = 5)
# Média
points(grupos_p1[,6], pch = 8, cex = 2, col = 2) # quando tiver 5 avaliadores, mudar o 5 por 6.
lines(grupos_p1[,6], col = 2)
# Mediana
points(grupos_p1[,7], pch = 10, cex = 2, col = 4) # quando tiver 5 avaliadores, mudar o 6 por 7.
lines(grupos_p1[,7], col = 4)
legend("topright", c("Média", "Mediana"), pch = c(8, 10), col = c(2,4), inset = c(.015,0.6))
# inset: localização da legenda


# ----------------------------------------------------------------- #
# ---------------------- Grupos da 2º página ---------------------- #
# ----------------------------------------------------------------- #

# Calculando a média/mediana das notas atribuídas a cada grupo
medias_p2 <- c(mean(grupos_p2$Grupo1.2), mean(grupos_p2$Grupo2.2), mean(grupos_p2$Grupo3.2),
               mean(grupos_p2$Grupo4.2), mean(grupos_p2$Grupo5.2), mean(grupos_p2$Grupo6.2))

mediana_p2 <- c(median(grupos_p2$Grupo1.2), median(grupos_p2$Grupo2.2), median(grupos_p2$Grupo3.2),
                median(grupos_p2$Grupo4.2), median(grupos_p2$Grupo5.2), median(grupos_p2$Grupo6.2))

# Invertendo a tabela
grupos_p2 <- t(grupos_p2)
grupos_p2 <- as.data.frame(grupos_p2)

# Adicionando as novas colunas de média e mediana
grupos_p2$media <- medias_p2
grupos_p2$mediana <- mediana_p2

# Gráficos
boxplot(t(grupos_p2[,-c(5,6)]), xlab = "Grupos", ylab = "Classificação", ylim = c(6,1), las = 1,
        main = "Classificação dos grupos") # -(media e mediana)
# Classificação dos grupos (do 1º ao 6º colocado):
#   1,5,6,3,2,4


# 930 x 465 (classificacao_dos_grupos_p2.png)
plot(0, type = "n", main = "Classificação dos grupos", xlab = "Grupos", ylab = "Classificação", 
     xlim = c(1,6), ylim = c(6,1), las = 1)
grid() # add grid
points(grupos_p2[,1], pch = 16)
points(grupos_p2[,2], pch = 16)
points(grupos_p2[,3], pch = 16)
points(grupos_p2[,4], pch = 16)
# Média
points(grupos_p2[,5], pch = 8, cex = 2, col = 2)
lines(grupos_p2[,5], col = 2)
# Mediana
points(grupos_p2[,6], pch = 10, cex = 2, col = 4)
lines(grupos_p2[,6], col = 4)
legend("topright", c("Média", "Mediana"), pch = c(8, 10), col = c(2,4), inset = c(.08,0.6))






