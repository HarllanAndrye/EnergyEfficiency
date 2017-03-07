# MDS - Multidimensional scaling


#install.packages('MASS')
library(MASS)
library(cluster)

source("functions.R")


# ----- Cenário 1 - 6 grupos ----- #
source("adaptative_cluster.R")
numberClusters = 6
resultsData <- paste("../Results/Data/cenario1/", numberClusters, "/dados_exec30.csv", sep = "")
exec30 = read.csv(resultsData)
tmp <- exec30
tmp$X <- tmp$cluster <- NULL
normDados = normData(tmp)
normDados$cluster <- exec30$cluster
rownames(normDados) <- exec30$X
Dist <- dist(normDados[,-(ncol(normDados))], method = "euclidean")


# ------------------------------------------------------ #
fit <- isoMDS(Dist, k=2) # k é o número de dimensões
# Plot
x <- fit$points[,1]
y <- fit$points[,2]
cols <- c("red", "blue", "black", "green", "orange", "yellow")

png(file = paste(resultsImages, numberClusters, "../Results/Images/agrupamento_6grupos.png", sep = ""), width = 855, height = 645)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Agrupamento - 6 grupos", pch = 'O', cex = 2, col = cols[normDados$cluster])
text(x, y, labels = row.names(normDados[,-(ncol(normDados))]), cex=.7)
dev.off()


# ------------------------------------------------------ #
# http://www.statmethods.net/advstats/cluster.html
# agrupameno_cen1_6grupos.png
png(file = paste(resultsImages, numberClusters, "../Results/Images/agrupameno_cen1_6grupos.png", sep = ""), width = 855, height = 645)
clusplot(normDados[,-(ncol(normDados))], normDados$cluster, color=TRUE, shade=F, labels=2, lines=0,
         main="Agrupamento do cenário 1", las = 1)
dev.off()


