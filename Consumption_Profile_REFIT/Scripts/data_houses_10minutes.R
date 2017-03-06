# Script para pegar os dados da base (1 dia) e deixar as observações a cada 10 minutos (10 em 10 minutos)

# Dados
source("data_houses_1minute.R")


# ------- #
# House 1 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse1$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse1[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse1 <- tmp


# ------- #
# House 2 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse2$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse2[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse2 <- tmp


# ------- #
# House 3 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse3$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse3[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse3 <- tmp


# ------- #
# House 4 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse4$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse4[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse4 <- tmp


# ------- #
# House 5 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse5$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse5[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse5 <- tmp


# ------- #
# House 6 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse6$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse6[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse6 <- tmp


# ------- #
# House 7 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse7$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse7[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse7 <- tmp


# ------- #
# House 8 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse8$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse8[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse8 <- tmp


# ------- #
# House 9 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse9$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse9[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse9 <- tmp


# ------- #
# House 10 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse10$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse10[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse10 <- tmp


# ------- #
# House 11 #
# ------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse11$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse11[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse11 <- tmp


# -------- #
# House 12 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse12$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse12[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse12 <- tmp


# -------- #
# House 13 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse13$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse13[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse13 <- tmp


# -------- #
# House 15 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse15$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse15[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse15 <- tmp


# -------- #
# House 16 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse16$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse16[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse16 <- tmp


# -------- #
# House 17 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse17$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse17[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse17 <- tmp


# -------- #
# House 18 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse18$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse18[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse18 <- tmp


# -------- #
# House 19 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse19$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse19[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse19 <- tmp


# -------- #
# House 20 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse20$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse20[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse20 <- tmp


# -------- #
# House 21 #
# -------- #
x <- c()
tmp <- c()
for(i in 1:24){ # horas
  for(j in 1:60){ # minutos
    # Pega o 'index' do dado e adiciona em 'x'
    x <- rbind( x, which( dayHouse21$Time == as.POSIXct( paste("2015-01-08 ", (i-1),":", (j-1), ":00", sep = "") ) ) )
    
    # de 10 em 10, calcula a média/mediana da potência
    if( j == 10 || j == 20 || j == 30 || j == 40 || j == 50 || j == 60 ){
      # day[linha, coluna]
      tmp <- rbind( tmp, c( paste("2015-01-08 ", (i-1),":", (j-10), ":00", sep = ""), mean(dayHouse21[x,2]) ) )
    }
  }
}
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("Time", "Aggregate")
tmp$Time <- as.POSIXct(tmp$Time, origin="1970-01-01")
tmp$Aggregate <- as.numeric(as.character(tmp$Aggregate))
# Normalizar os dados (potência)
tmp <- normDataPerfil(tmp)

dayHouse21 <- tmp







