# Script para pegar os dados da base (1 month) e deixar as observações a cada hora (hora em hora - 1 dia)

# Funções
source("functions.R")

# Caminho dos arquivos
pathFiles <- "../Database/month/"

# ------- #
# House 1 #
# ------- #
# Dados filtrados usando o script "filter_data_1month.py"
dayHouse1 <- read.csv(paste(pathFiles, "house_1_month.csv", sep = ""), header = T)

dayHouse1$Unix <- dayHouse1$Appliance1 <- dayHouse1$Appliance2 <- dayHouse1$Appliance3 <- dayHouse1$Appliance4 <- NULL
dayHouse1$Appliance5 <- dayHouse1$Appliance6 <- dayHouse1$Appliance7 <- dayHouse1$Appliance8 <- dayHouse1$Appliance9 <- NULL

dayHouse1$Time <- substr(dayHouse1[ ,1],12,13) # Retirando o dia
dayHouse1 <- aggregate(dayHouse1[,2], list("Time" = dayHouse1[,1]), FUN = mean) # Calculando a média
colnames(dayHouse1) <- c("Time", "Aggregate")


# ------- #
# House 2 #
# ------- #
dayHouse2 <- read.csv(paste(pathFiles, "house_2_month.csv", sep = ""), header = T)

dayHouse2$Unix <- dayHouse2$Appliance1 <- dayHouse2$Appliance2 <- dayHouse2$Appliance3 <- dayHouse2$Appliance4 <- NULL
dayHouse2$Appliance5 <- dayHouse2$Appliance6 <- dayHouse2$Appliance7 <- dayHouse2$Appliance8 <- dayHouse2$Appliance9 <- NULL

dayHouse2$Time <- substr(dayHouse2[ ,1],12,13) # Retirando o dia
dayHouse2 <- aggregate(dayHouse2[,2], list("Time" = dayHouse2[,1]), FUN = mean) # Calculando a média
colnames(dayHouse2) <- c("Time", "Aggregate")


# ------- #
# House 3 #
# ------- #
dayHouse3 <- read.csv(paste(pathFiles, "house_3_month.csv", sep = ""), header = T)

dayHouse3$Unix <- dayHouse3$Appliance1 <- dayHouse3$Appliance2 <- dayHouse3$Appliance3 <- dayHouse3$Appliance4 <- NULL
dayHouse3$Appliance5 <- dayHouse3$Appliance6 <- dayHouse3$Appliance7 <- dayHouse3$Appliance8 <- dayHouse3$Appliance9 <- NULL

dayHouse3$Time <- substr(dayHouse3[ ,1],12,13) # Retirando o dia
dayHouse3 <- aggregate(dayHouse3[,2], list("Time" = dayHouse3[,1]), FUN = mean) # Calculando a média
colnames(dayHouse3) <- c("Time", "Aggregate")


# ------- #
# House 4 #
# ------- #
dayHouse4 <- read.csv(paste(pathFiles, "house_4_month.csv", sep = ""), header = T)

dayHouse4$Unix <- dayHouse4$Appliance1 <- dayHouse4$Appliance2 <- dayHouse4$Appliance3 <- dayHouse4$Appliance4 <- NULL
dayHouse4$Appliance5 <- dayHouse4$Appliance6 <- dayHouse4$Appliance7 <- dayHouse4$Appliance8 <- dayHouse4$Appliance9 <- NULL

dayHouse4$Time <- substr(dayHouse4[ ,1],12,13) # Retirando o dia
dayHouse4 <- aggregate(dayHouse4[,2], list("Time" = dayHouse4[,1]), FUN = mean) # Calculando a média
colnames(dayHouse4) <- c("Time", "Aggregate")


# ------- #
# House 5 #
# ------- #
dayHouse5 <- read.csv(paste(pathFiles, "house_5_month.csv", sep = ""), header = T)

dayHouse5$Unix <- dayHouse5$Appliance1 <- dayHouse5$Appliance2 <- dayHouse5$Appliance3 <- dayHouse5$Appliance4 <- NULL
dayHouse5$Appliance5 <- dayHouse5$Appliance6 <- dayHouse5$Appliance7 <- dayHouse5$Appliance8 <- dayHouse5$Appliance9 <- NULL

dayHouse5$Time <- substr(dayHouse5[ ,1],12,13) # Retirando o dia
dayHouse5 <- aggregate(dayHouse5[,2], list("Time" = dayHouse5[,1]), FUN = mean) # Calculando a média
colnames(dayHouse5) <- c("Time", "Aggregate")


# ------- #
# House 6 #
# ------- #
dayHouse6 <- read.csv(paste(pathFiles, "house_6_month.csv", sep = ""), header = T)

dayHouse6$Unix <- dayHouse6$Appliance1 <- dayHouse6$Appliance2 <- dayHouse6$Appliance3 <- dayHouse6$Appliance4 <- NULL
dayHouse6$Appliance5 <- dayHouse6$Appliance6 <- dayHouse6$Appliance7 <- dayHouse6$Appliance8 <- dayHouse6$Appliance9 <- NULL

dayHouse6$Time <- substr(dayHouse6[ ,1],12,13) # Retirando o dia
dayHouse6 <- aggregate(dayHouse6[,2], list("Time" = dayHouse6[,1]), FUN = mean) # Calculando a média
colnames(dayHouse6) <- c("Time", "Aggregate")


# ------- #
# House 7 #
# ------- #
dayHouse7 <- read.csv(paste(pathFiles, "house_7_month.csv", sep = ""), header = T)

dayHouse7$Unix <- dayHouse7$Appliance1 <- dayHouse7$Appliance2 <- dayHouse7$Appliance3 <- dayHouse7$Appliance4 <- NULL
dayHouse7$Appliance5 <- dayHouse7$Appliance6 <- dayHouse7$Appliance7 <- dayHouse7$Appliance8 <- dayHouse7$Appliance9 <- NULL

dayHouse7$Time <- substr(dayHouse7[ ,1],12,13) # Retirando o dia
dayHouse7 <- aggregate(dayHouse7[,2], list("Time" = dayHouse7[,1]), FUN = mean) # Calculando a média
colnames(dayHouse7) <- c("Time", "Aggregate")


# ------- #
# House 8 #
# ------- #
dayHouse8 <- read.csv(paste(pathFiles, "house_8_month.csv", sep = ""), header = T)

dayHouse8$Unix <- dayHouse8$Appliance1 <- dayHouse8$Appliance2 <- dayHouse8$Appliance3 <- dayHouse8$Appliance4 <- NULL
dayHouse8$Appliance5 <- dayHouse8$Appliance6 <- dayHouse8$Appliance7 <- dayHouse8$Appliance8 <- dayHouse8$Appliance9 <- NULL

dayHouse8$Time <- substr(dayHouse8[ ,1],12,13) # Retirando o dia
dayHouse8 <- aggregate(dayHouse8[,2], list("Time" = dayHouse8[,1]), FUN = mean) # Calculando a média
colnames(dayHouse8) <- c("Time", "Aggregate")


# ------- #
# House 9 #
# ------- #
dayHouse9 <- read.csv(paste(pathFiles, "house_9_month.csv", sep = ""), header = T)

dayHouse9$Unix <- dayHouse9$Appliance1 <- dayHouse9$Appliance2 <- dayHouse9$Appliance3 <- dayHouse9$Appliance4 <- NULL
dayHouse9$Appliance5 <- dayHouse9$Appliance6 <- dayHouse9$Appliance7 <- dayHouse9$Appliance8 <- dayHouse9$Appliance9 <- NULL

dayHouse9$Time <- substr(dayHouse9[ ,1],12,13) # Retirando o dia
dayHouse9 <- aggregate(dayHouse9[,2], list("Time" = dayHouse9[,1]), FUN = mean) # Calculando a média
colnames(dayHouse9) <- c("Time", "Aggregate")


# ------- #
# House 10 #
# ------- #
dayHouse10 <- read.csv(paste(pathFiles, "house_10_month.csv", sep = ""), header = T)

dayHouse10$Unix <- dayHouse10$Appliance1 <- dayHouse10$Appliance2 <- dayHouse10$Appliance3 <- dayHouse10$Appliance4 <- NULL
dayHouse10$Appliance5 <- dayHouse10$Appliance6 <- dayHouse10$Appliance7 <- dayHouse10$Appliance8 <- dayHouse10$Appliance9 <- NULL

dayHouse10$Time <- substr(dayHouse10[ ,1],12,13) # Retirando o dia
dayHouse10 <- aggregate(dayHouse10[,2], list("Time" = dayHouse10[,1]), FUN = mean) # Calculando a média
colnames(dayHouse10) <- c("Time", "Aggregate")


# ------- #
# House 11 #
# ------- #
dayHouse11 <- read.csv(paste(pathFiles, "house_11_month.csv", sep = ""), header = T)

dayHouse11$Unix <- dayHouse11$Appliance1 <- dayHouse11$Appliance2 <- dayHouse11$Appliance3 <- dayHouse11$Appliance4 <- NULL
dayHouse11$Appliance5 <- dayHouse11$Appliance6 <- dayHouse11$Appliance7 <- dayHouse11$Appliance8 <- dayHouse11$Appliance9 <- NULL

dayHouse11$Time <- substr(dayHouse11[ ,1],12,13) # Retirando o dia
dayHouse11 <- aggregate(dayHouse11[,2], list("Time" = dayHouse11[,1]), FUN = mean) # Calculando a média
colnames(dayHouse11) <- c("Time", "Aggregate")


# ------- #
# House 12 #
# ------- #
dayHouse12 <- read.csv(paste(pathFiles, "house_12_month.csv", sep = ""), header = T)

dayHouse12$Unix <- dayHouse12$Appliance1 <- dayHouse12$Appliance2 <- dayHouse12$Appliance3 <- dayHouse12$Appliance4 <- NULL
dayHouse12$Appliance5 <- dayHouse12$Appliance6 <- dayHouse12$Appliance7 <- dayHouse12$Appliance8 <- dayHouse12$Appliance9 <- NULL

dayHouse12$Time <- substr(dayHouse12[ ,1],12,13) # Retirando o dia
dayHouse12 <- aggregate(dayHouse12[,2], list("Time" = dayHouse12[,1]), FUN = mean) # Calculando a média
colnames(dayHouse12) <- c("Time", "Aggregate")


# ------- #
# House 13 #
# ------- #
dayHouse13 <- read.csv(paste(pathFiles, "house_13_month.csv", sep = ""), header = T)

dayHouse13$Unix <- dayHouse13$Appliance1 <- dayHouse13$Appliance2 <- dayHouse13$Appliance3 <- dayHouse13$Appliance4 <- NULL
dayHouse13$Appliance5 <- dayHouse13$Appliance6 <- dayHouse13$Appliance7 <- dayHouse13$Appliance8 <- dayHouse13$Appliance9 <- NULL

dayHouse13$Time <- substr(dayHouse13[ ,1],12,13) # Retirando o dia
dayHouse13 <- aggregate(dayHouse13[,2], list("Time" = dayHouse13[,1]), FUN = mean) # Calculando a média
colnames(dayHouse13) <- c("Time", "Aggregate")


# ------- #
# House 15 #
# ------- #
dayHouse15 <- read.csv(paste(pathFiles, "house_15_month.csv", sep = ""), header = T)

dayHouse15$Unix <- dayHouse15$Appliance1 <- dayHouse15$Appliance2 <- dayHouse15$Appliance3 <- dayHouse15$Appliance4 <- NULL
dayHouse15$Appliance5 <- dayHouse15$Appliance6 <- dayHouse15$Appliance7 <- dayHouse15$Appliance8 <- dayHouse15$Appliance9 <- NULL

dayHouse15$Time <- substr(dayHouse15[ ,1],12,13) # Retirando o dia
dayHouse15 <- aggregate(dayHouse15[,2], list("Time" = dayHouse15[,1]), FUN = mean) # Calculando a média
colnames(dayHouse15) <- c("Time", "Aggregate")


# -------- #
# House 16 #
# -------- #
dayHouse16 <- read.csv(paste(pathFiles, "house_16_month.csv", sep = ""), header = T)

dayHouse16$Unix <- dayHouse16$Appliance1 <- dayHouse16$Appliance2 <- dayHouse16$Appliance3 <- dayHouse16$Appliance4 <- NULL
dayHouse16$Appliance5 <- dayHouse16$Appliance6 <- dayHouse16$Appliance7 <- dayHouse16$Appliance8 <- dayHouse16$Appliance9 <- NULL

dayHouse16$Time <- substr(dayHouse16[ ,1],12,13) # Retirando o dia
dayHouse16 <- aggregate(dayHouse16[,2], list("Time" = dayHouse16[,1]), FUN = mean) # Calculando a média
colnames(dayHouse16) <- c("Time", "Aggregate")


# -------- #
# House 17 #
# -------- #
dayHouse17 <- read.csv(paste(pathFiles, "house_17_month.csv", sep = ""), header = T)

dayHouse17$Unix <- dayHouse17$Appliance1 <- dayHouse17$Appliance2 <- dayHouse17$Appliance3 <- dayHouse17$Appliance4 <- NULL
dayHouse17$Appliance5 <- dayHouse17$Appliance6 <- dayHouse17$Appliance7 <- dayHouse17$Appliance8 <- dayHouse17$Appliance9 <- NULL

dayHouse17$Time <- substr(dayHouse17[ ,1],12,13) # Retirando o dia
dayHouse17 <- aggregate(dayHouse17[,2], list("Time" = dayHouse17[,1]), FUN = mean) # Calculando a média
colnames(dayHouse17) <- c("Time", "Aggregate")


# -------- #
# House 18 #
# -------- #
dayHouse18 <- read.csv(paste(pathFiles, "house_18_month.csv", sep = ""), header = T)

dayHouse18$Unix <- dayHouse18$Appliance1 <- dayHouse18$Appliance2 <- dayHouse18$Appliance3 <- dayHouse18$Appliance4 <- NULL
dayHouse18$Appliance5 <- dayHouse18$Appliance6 <- dayHouse18$Appliance7 <- dayHouse18$Appliance8 <- dayHouse18$Appliance9 <- NULL

dayHouse18$Time <- substr(dayHouse18[ ,1],12,13) # Retirando o dia
dayHouse18 <- aggregate(dayHouse18[,2], list("Time" = dayHouse18[,1]), FUN = mean) # Calculando a média
colnames(dayHouse18) <- c("Time", "Aggregate")


# -------- #
# House 19 #
# -------- #
dayHouse19 <- read.csv(paste(pathFiles, "house_19_month.csv", sep = ""), header = T)

dayHouse19$Unix <- dayHouse19$Appliance1 <- dayHouse19$Appliance2 <- dayHouse19$Appliance3 <- dayHouse19$Appliance4 <- NULL
dayHouse19$Appliance5 <- dayHouse19$Appliance6 <- dayHouse19$Appliance7 <- dayHouse19$Appliance8 <- dayHouse19$Appliance9 <- NULL

dayHouse19$Time <- substr(dayHouse19[ ,1],12,13) # Retirando o dia
dayHouse19 <- aggregate(dayHouse19[,2], list("Time" = dayHouse19[,1]), FUN = mean) # Calculando a média
colnames(dayHouse19) <- c("Time", "Aggregate")


# -------- #
# House 20 #
# -------- #
dayHouse20 <- read.csv(paste(pathFiles, "house_20_month.csv", sep = ""), header = T)

dayHouse20$Unix <- dayHouse20$Appliance1 <- dayHouse20$Appliance2 <- dayHouse20$Appliance3 <- dayHouse20$Appliance4 <- NULL
dayHouse20$Appliance5 <- dayHouse20$Appliance6 <- dayHouse20$Appliance7 <- dayHouse20$Appliance8 <- dayHouse20$Appliance9 <- NULL

dayHouse20$Time <- substr(dayHouse20[ ,1],12,13) # Retirando o dia
dayHouse20 <- aggregate(dayHouse20[,2], list("Time" = dayHouse20[,1]), FUN = mean) # Calculando a média
colnames(dayHouse20) <- c("Time", "Aggregate")


# -------- #
# House 21 #
# -------- #
dayHouse21 <- read.csv(paste(pathFiles, "house_21_month.csv", sep = ""), header = T)

dayHouse21$Unix <- dayHouse21$Appliance1 <- dayHouse21$Appliance2 <- dayHouse21$Appliance3 <- dayHouse21$Appliance4 <- NULL
dayHouse21$Appliance5 <- dayHouse21$Appliance6 <- dayHouse21$Appliance7 <- dayHouse21$Appliance8 <- dayHouse21$Appliance9 <- NULL

dayHouse21$Time <- substr(dayHouse21[ ,1],12,13) # Retirando o dia
dayHouse21 <- aggregate(dayHouse21[,2], list("Time" = dayHouse21[,1]), FUN = mean) # Calculando a média
colnames(dayHouse21) <- c("Time", "Aggregate")



