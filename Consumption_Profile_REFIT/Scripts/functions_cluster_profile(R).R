# Funções para agrupamento de perfis de consumo

library(TSdist)

# Construindo uma matriz de similaridade com correlação
correlacao_20U <- function(user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,
                       user11,user12,user13,user14,user15,user16,user17,user18,user19,user20){
  
  # Criando a matriz de correlação
  corr <- matrix(data = 0, nrow = 20, ncol = 20)
  
  # User 1
  corr[1,2] <- CorDistance(user1[,2], user2[,2])
  corr[1,3] <- CorDistance(user1[,2], user3[,2])
  corr[1,4] <- CorDistance(user1[,2], user4[,2])
  corr[1,5] <- CorDistance(user1[,2], user5[,2])
  corr[1,6] <- CorDistance(user1[,2], user6[,2])
  corr[1,7] <- CorDistance(user1[,2], user7[,2])
  corr[1,8] <- CorDistance(user1[,2], user8[,2])
  corr[1,9] <- CorDistance(user1[,2], user9[,2])
  corr[1,10] <- CorDistance(user1[,2], user10[,2])
  corr[1,11] <- CorDistance(user1[,2], user11[,2])
  corr[1,12] <- CorDistance(user1[,2], user12[,2])
  corr[1,13] <- CorDistance(user1[,2], user13[,2])
  corr[1,14] <- CorDistance(user1[,2], user14[,2])
  corr[1,15] <- CorDistance(user1[,2], user15[,2])
  corr[1,16] <- CorDistance(user1[,2], user16[,2])
  corr[1,17] <- CorDistance(user1[,2], user17[,2])
  corr[1,18] <- CorDistance(user1[,2], user18[,2])
  corr[1,19] <- CorDistance(user1[,2], user19[,2])
  corr[1,20] <- CorDistance(user1[,2], user20[,2])
  
  # User 2
  corr[2,3] <- CorDistance(user2[,2], user3[,2])
  corr[2,4] <- CorDistance(user2[,2], user4[,2])
  corr[2,5] <- CorDistance(user2[,2], user5[,2])
  corr[2,6] <- CorDistance(user2[,2], user6[,2])
  corr[2,7] <- CorDistance(user2[,2], user7[,2])
  corr[2,8] <- CorDistance(user2[,2], user8[,2])
  corr[2,9] <- CorDistance(user2[,2], user9[,2])
  corr[2,10] <- CorDistance(user2[,2], user10[,2])
  corr[2,11] <- CorDistance(user2[,2], user11[,2])
  corr[2,12] <- CorDistance(user2[,2], user12[,2])
  corr[2,13] <- CorDistance(user2[,2], user13[,2])
  corr[2,14] <- CorDistance(user2[,2], user14[,2])
  corr[2,15] <- CorDistance(user2[,2], user15[,2])
  corr[2,16] <- CorDistance(user2[,2], user16[,2])
  corr[2,17] <- CorDistance(user2[,2], user17[,2])
  corr[2,18] <- CorDistance(user2[,2], user18[,2])
  corr[2,19] <- CorDistance(user2[,2], user19[,2])
  corr[2,20] <- CorDistance(user2[,2], user20[,2])
  
  # User 3
  corr[3,4] <- CorDistance(user3[,2], user4[,2])
  corr[3,5] <- CorDistance(user3[,2], user5[,2])
  corr[3,6] <- CorDistance(user3[,2], user6[,2])
  corr[3,7] <- CorDistance(user3[,2], user7[,2])
  corr[3,8]  <- CorDistance(user3[,2], user8[,2])
  corr[3,9] <- CorDistance(user3[,2], user9[,2])
  corr[3,10] <- CorDistance(user3[,2], user10[,2])
  corr[3,11] <- CorDistance(user3[,2], user11[,2])
  corr[3,12] <- CorDistance(user3[,2], user12[,2])
  corr[3,13] <- CorDistance(user3[,2], user13[,2])
  corr[3,14] <- CorDistance(user3[,2], user14[,2])
  corr[3,15] <- CorDistance(user3[,2], user15[,2])
  corr[3,16] <- CorDistance(user3[,2], user16[,2])
  corr[3,17] <- CorDistance(user3[,2], user17[,2])
  corr[3,18] <- CorDistance(user3[,2], user18[,2])
  corr[3,19] <- CorDistance(user3[,2], user19[,2])
  corr[3,20] <- CorDistance(user3[,2], user20[,2])
  
  # User 4
  corr[4,5] <- CorDistance(user4[,2], user5[,2])
  corr[4,6] <- CorDistance(user4[,2], user6[,2])
  corr[4,7] <- CorDistance(user4[,2], user7[,2])
  corr[4,8] <- CorDistance(user4[,2], user8[,2])
  corr[4,9] <- CorDistance(user4[,2], user9[,2])
  corr[4,10] <- CorDistance(user4[,2], user10[,2])
  corr[4,11] <- CorDistance(user4[,2], user11[,2])
  corr[4,12] <- CorDistance(user4[,2], user12[,2])
  corr[4,13] <- CorDistance(user4[,2], user13[,2])
  corr[4,14] <- CorDistance(user4[,2], user14[,2])
  corr[4,15] <- CorDistance(user4[,2], user15[,2])
  corr[4,16] <- CorDistance(user4[,2], user16[,2])
  corr[4,17] <- CorDistance(user4[,2], user17[,2])
  corr[4,18] <- CorDistance(user4[,2], user18[,2])
  corr[4,19] <- CorDistance(user4[,2], user19[,2])
  corr[4,20] <- CorDistance(user4[,2], user20[,2])
  
  # User 5
  corr[5,6] <- CorDistance(user5[,2], user6[,2])
  corr[5,7] <- CorDistance(user5[,2], user7[,2])
  corr[5,8] <- CorDistance(user5[,2], user8[,2])
  corr[5,9] <- CorDistance(user5[,2], user9[,2])
  corr[5,10] <- CorDistance(user5[,2], user10[,2])
  corr[5,11] <- CorDistance(user5[,2], user11[,2])
  corr[5,12] <- CorDistance(user5[,2], user12[,2])
  corr[5,13] <- CorDistance(user5[,2], user13[,2])
  corr[5,14] <- CorDistance(user5[,2], user14[,2])
  corr[5,15] <- CorDistance(user5[,2], user15[,2])
  corr[5,16] <- CorDistance(user5[,2], user16[,2])
  corr[5,17] <- CorDistance(user5[,2], user17[,2])
  corr[5,18] <- CorDistance(user5[,2], user18[,2])
  corr[5,19] <- CorDistance(user5[,2], user19[,2])
  corr[5,20] <- CorDistance(user5[,2], user20[,2])
  
  # User 6
  corr[6,7] <- CorDistance(user6[,2], user7[,2])
  corr[6,8] <- CorDistance(user6[,2], user8[,2])
  corr[6,9] <- CorDistance(user6[,2], user9[,2])
  corr[6,10] <- CorDistance(user6[,2], user10[,2])
  corr[6,11] <- CorDistance(user6[,2], user11[,2])
  corr[6,12] <- CorDistance(user6[,2], user12[,2])
  corr[6,13] <- CorDistance(user6[,2], user13[,2])
  corr[6,14] <- CorDistance(user6[,2], user14[,2])
  corr[6,15] <- CorDistance(user6[,2], user15[,2])
  corr[6,16] <- CorDistance(user6[,2], user16[,2])
  corr[6,17] <- CorDistance(user6[,2], user17[,2])
  corr[6,18] <- CorDistance(user6[,2], user18[,2])
  corr[6,19] <- CorDistance(user6[,2], user19[,2])
  corr[6,20] <- CorDistance(user6[,2], user20[,2])
  
  # User 7
  corr[7,8] <- CorDistance(user7[,2], user8[,2])
  corr[7,9] <- CorDistance(user7[,2], user9[,2])
  corr[7,10] <- CorDistance(user7[,2], user10[,2])
  corr[7,11] <- CorDistance(user7[,2], user11[,2])
  corr[7,12] <- CorDistance(user7[,2], user12[,2])
  corr[7,13] <- CorDistance(user7[,2], user13[,2])
  corr[7,14] <- CorDistance(user7[,2], user14[,2])
  corr[7,15] <- CorDistance(user7[,2], user15[,2])
  corr[7,16] <- CorDistance(user7[,2], user16[,2])
  corr[7,17] <- CorDistance(user7[,2], user17[,2])
  corr[7,18] <- CorDistance(user7[,2], user18[,2])
  corr[7,19] <- CorDistance(user7[,2], user19[,2])
  corr[7,20] <- CorDistance(user7[,2], user20[,2])
  
  # User 8
  corr[8,9] <- CorDistance(user8[,2], user9[,2])
  corr[8,10] <- CorDistance(user8[,2], user10[,2])
  corr[8,11] <- CorDistance(user8[,2], user11[,2])
  corr[8,12] <- CorDistance(user8[,2], user12[,2])
  corr[8,13] <- CorDistance(user8[,2], user13[,2])
  corr[8,14] <- CorDistance(user8[,2], user14[,2])
  corr[8,15] <- CorDistance(user8[,2], user15[,2])
  corr[8,16] <- CorDistance(user8[,2], user16[,2])
  corr[8,17] <- CorDistance(user8[,2], user17[,2])
  corr[8,18] <- CorDistance(user8[,2], user18[,2])
  corr[8,19] <- CorDistance(user8[,2], user19[,2])
  corr[8,20] <- CorDistance(user8[,2], user20[,2])
  
  # User 9
  corr[9,10] <- CorDistance(user9[,2], user10[,2])
  corr[9,11] <- CorDistance(user9[,2], user11[,2])
  corr[9,12] <- CorDistance(user9[,2], user12[,2])
  corr[9,13] <- CorDistance(user9[,2], user13[,2])
  corr[9,14] <- CorDistance(user9[,2], user14[,2])
  corr[9,15] <- CorDistance(user9[,2], user15[,2])
  corr[9,16] <- CorDistance(user9[,2], user16[,2])
  corr[9,17] <- CorDistance(user9[,2], user17[,2])
  corr[9,18] <- CorDistance(user9[,2], user18[,2])
  corr[9,19] <- CorDistance(user9[,2], user19[,2])
  corr[9,20] <- CorDistance(user9[,2], user20[,2])
  
  # User 10
  corr[10,11] <- CorDistance(user10[,2], user11[,2])
  corr[10,12] <- CorDistance(user10[,2], user12[,2])
  corr[10,13] <- CorDistance(user10[,2], user13[,2])
  corr[10,14] <- CorDistance(user10[,2], user14[,2])
  corr[10,15] <- CorDistance(user10[,2], user15[,2])
  corr[10,16] <- CorDistance(user10[,2], user16[,2])
  corr[10,17] <- CorDistance(user10[,2], user17[,2])
  corr[10,18] <- CorDistance(user10[,2], user18[,2])
  corr[10,19] <- CorDistance(user10[,2], user19[,2])
  corr[10,20] <- CorDistance(user10[,2], user20[,2])
  
  # User 11
  corr[11,12] <- CorDistance(user11[,2], user12[,2])
  corr[11,13] <- CorDistance(user11[,2], user13[,2])
  corr[11,14] <- CorDistance(user11[,2], user14[,2])
  corr[11,15] <- CorDistance(user11[,2], user15[,2])
  corr[11,16] <- CorDistance(user11[,2], user16[,2])
  corr[11,17] <- CorDistance(user11[,2], user17[,2])
  corr[11,18] <- CorDistance(user11[,2], user18[,2])
  corr[11,19] <- CorDistance(user11[,2], user19[,2])
  corr[11,20] <- CorDistance(user11[,2], user20[,2])
  
  # User 12
  corr[12,13] <- CorDistance(user12[,2], user13[,2])
  corr[12,14] <- CorDistance(user12[,2], user14[,2])
  corr[12,15] <- CorDistance(user12[,2], user15[,2])
  corr[12,16] <- CorDistance(user12[,2], user16[,2])
  corr[12,17] <- CorDistance(user12[,2], user17[,2])
  corr[12,18] <- CorDistance(user12[,2], user18[,2])
  corr[12,19] <- CorDistance(user12[,2], user19[,2])
  corr[12,20] <- CorDistance(user12[,2], user20[,2])
  
  # User 13
  corr[13,14] <- CorDistance(user13[,2], user14[,2])
  corr[13,15] <- CorDistance(user13[,2], user15[,2])
  corr[13,16] <- CorDistance(user13[,2], user16[,2])
  corr[13,17] <- CorDistance(user13[,2], user17[,2])
  corr[13,18] <- CorDistance(user13[,2], user18[,2])
  corr[13,19] <- CorDistance(user13[,2], user19[,2])
  corr[13,20] <- CorDistance(user13[,2], user20[,2])
  
  # User 14
  corr[14,15] <- CorDistance(user14[,2], user15[,2])
  corr[14,16] <- CorDistance(user14[,2], user16[,2])
  corr[14,17] <- CorDistance(user14[,2], user17[,2])
  corr[14,18] <- CorDistance(user14[,2], user18[,2])
  corr[14,19] <- CorDistance(user14[,2], user19[,2])
  corr[14,20] <- CorDistance(user14[,2], user20[,2])
  
  # User 15
  corr[15,16] <- CorDistance(user15[,2], user16[,2])
  corr[15,17] <- CorDistance(user15[,2], user17[,2])
  corr[15,18] <- CorDistance(user15[,2], user18[,2])
  corr[15,19] <- CorDistance(user15[,2], user19[,2])
  corr[15,20] <- CorDistance(user15[,2], user20[,2])
  
  # User 16
  corr[16,17] <- CorDistance(user16[,2], user17[,2])
  corr[16,18] <- CorDistance(user16[,2], user18[,2])
  corr[16,19] <- CorDistance(user16[,2], user19[,2])
  corr[16,20] <- CorDistance(user16[,2], user20[,2])
  
  # User 17
  corr[17,18] <- CorDistance(user17[,2], user18[,2])
  corr[17,19] <- CorDistance(user17[,2], user19[,2])
  corr[17,20] <- CorDistance(user17[,2], user20[,2])
  
  # User 18
  corr[18,19] <- CorDistance(user18[,2], user19[,2])
  corr[18,20] <- CorDistance(user18[,2], user20[,2])
  
  # User 19
  corr[19,20] <- CorDistance(user19[,2], user20[,2])
  
  # Preenchendo o restante da matriz
  for(i in 1:20){ # linha
    for(j in 1:20){ # coluna
      if(i == j){
        corr[i,j] <- 0
      } else if(corr[i,j] != 0){
        corr[j,i] <- corr[i,j]
      }
    }
  }
  
  return(corr)
}


# Construindo uma matriz de similaridade com euclidiana
distaciaEuclid_20U <- function(user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,
                           user11,user12,user13,user14,user15,user16,user17,user18,user19,user20){
  # Criando a matriz
  corr <- matrix(data = 0, nrow = 20, ncol = 20)
  
  # User 1
  corr[1,2] <- distEuclidean(user1, user2)
  corr[1,3] <- distEuclidean(user1, user3)
  corr[1,4] <- distEuclidean(user1, user4)
  corr[1,5] <- distEuclidean(user1, user5)
  corr[1,6] <- distEuclidean(user1, user6)
  corr[1,7] <- distEuclidean(user1, user7)
  corr[1,8] <- distEuclidean(user1, user8)
  corr[1,9] <- distEuclidean(user1, user9)
  corr[1,10] <- distEuclidean(user1, user10)
  corr[1,11] <- distEuclidean(user1, user11)
  corr[1,12] <- distEuclidean(user1, user12)
  corr[1,13] <- distEuclidean(user1, user13)
  corr[1,14] <- distEuclidean(user1, user14)
  corr[1,15] <- distEuclidean(user1, user15)
  corr[1,16] <- distEuclidean(user1, user16)
  corr[1,17] <- distEuclidean(user1, user17)
  corr[1,18] <- distEuclidean(user1, user18)
  corr[1,19] <- distEuclidean(user1, user19)
  corr[1,20] <- distEuclidean(user1, user20)
  
  # User 2
  corr[2,3] <- distEuclidean(user2, user3)
  corr[2,4] <- distEuclidean(user2, user4)
  corr[2,5] <- distEuclidean(user2, user5)
  corr[2,6] <- distEuclidean(user2, user6)
  corr[2,7] <- distEuclidean(user2, user7)
  corr[2,8] <- distEuclidean(user2, user8)
  corr[2,9] <- distEuclidean(user2, user9)
  corr[2,10] <- distEuclidean(user2, user10)
  corr[2,11] <- distEuclidean(user2, user11)
  corr[2,12] <- distEuclidean(user2, user12)
  corr[2,13] <- distEuclidean(user2, user13)
  corr[2,14] <- distEuclidean(user2, user14)
  corr[2,15] <- distEuclidean(user2, user15)
  corr[2,16] <- distEuclidean(user2, user16)
  corr[2,17] <- distEuclidean(user2, user17)
  corr[2,18] <- distEuclidean(user2, user18)
  corr[2,19] <- distEuclidean(user2, user19)
  corr[2,20] <- distEuclidean(user2, user20)
  
  # User 3
  corr[3,4] <- distEuclidean(user3, user4)
  corr[3,5] <- distEuclidean(user3, user5)
  corr[3,6] <- distEuclidean(user3, user6)
  corr[3,7] <- distEuclidean(user3, user7)
  corr[3,8]  <- distEuclidean(user3, user8)
  corr[3,9] <- distEuclidean(user3, user9)
  corr[3,10] <- distEuclidean(user3, user10)
  corr[3,11] <- distEuclidean(user3, user11)
  corr[3,12] <- distEuclidean(user3, user12)
  corr[3,13] <- distEuclidean(user3, user13)
  corr[3,14] <- distEuclidean(user3, user14)
  corr[3,15] <- distEuclidean(user3, user15)
  corr[3,16] <- distEuclidean(user3, user16)
  corr[3,17] <- distEuclidean(user3, user17)
  corr[3,18] <- distEuclidean(user3, user18)
  corr[3,19] <- distEuclidean(user3, user19)
  corr[3,20] <- distEuclidean(user3, user20)
  
  # User 4
  corr[4,5] <- distEuclidean(user4, user5)
  corr[4,6] <- distEuclidean(user4, user6)
  corr[4,7] <- distEuclidean(user4, user7)
  corr[4,8] <- distEuclidean(user4, user8)
  corr[4,9] <- distEuclidean(user4, user9)
  corr[4,10] <- distEuclidean(user4, user10)
  corr[4,11] <- distEuclidean(user4, user11)
  corr[4,12] <- distEuclidean(user4, user12)
  corr[4,13] <- distEuclidean(user4, user13)
  corr[4,14] <- distEuclidean(user4, user14)
  corr[4,15] <- distEuclidean(user4, user15)
  corr[4,16] <- distEuclidean(user4, user16)
  corr[4,17] <- distEuclidean(user4, user17)
  corr[4,18] <- distEuclidean(user4, user18)
  corr[4,19] <- distEuclidean(user4, user19)
  corr[4,20] <- distEuclidean(user4, user20)
  
  # User 5
  corr[5,6] <- distEuclidean(user5, user6)
  corr[5,7] <- distEuclidean(user5, user7)
  corr[5,8] <- distEuclidean(user5, user8)
  corr[5,9] <- distEuclidean(user5, user9)
  corr[5,10] <- distEuclidean(user5, user10)
  corr[5,11] <- distEuclidean(user5, user11)
  corr[5,12] <- distEuclidean(user5, user12)
  corr[5,13] <- distEuclidean(user5, user13)
  corr[5,14] <- distEuclidean(user5, user14)
  corr[5,15] <- distEuclidean(user5, user15)
  corr[5,16] <- distEuclidean(user5, user16)
  corr[5,17] <- distEuclidean(user5, user17)
  corr[5,18] <- distEuclidean(user5, user18)
  corr[5,19] <- distEuclidean(user5, user19)
  corr[5,20] <- distEuclidean(user5, user20)
  
  # User 6
  corr[6,7] <- distEuclidean(user6, user7)
  corr[6,8] <- distEuclidean(user6, user8)
  corr[6,9] <- distEuclidean(user6, user9)
  corr[6,10] <- distEuclidean(user6, user10)
  corr[6,11] <- distEuclidean(user6, user11)
  corr[6,12] <- distEuclidean(user6, user12)
  corr[6,13] <- distEuclidean(user6, user13)
  corr[6,14] <- distEuclidean(user6, user14)
  corr[6,15] <- distEuclidean(user6, user15)
  corr[6,16] <- distEuclidean(user6, user16)
  corr[6,17] <- distEuclidean(user6, user17)
  corr[6,18] <- distEuclidean(user6, user18)
  corr[6,19] <- distEuclidean(user6, user19)
  corr[6,20] <- distEuclidean(user6, user20)
  
  # User 7
  corr[7,8] <- distEuclidean(user7, user8)
  corr[7,9] <- distEuclidean(user7, user9)
  corr[7,10] <- distEuclidean(user7, user10)
  corr[7,11] <- distEuclidean(user7, user11)
  corr[7,12] <- distEuclidean(user7, user12)
  corr[7,13] <- distEuclidean(user7, user13)
  corr[7,14] <- distEuclidean(user7, user14)
  corr[7,15] <- distEuclidean(user7, user15)
  corr[7,16] <- distEuclidean(user7, user16)
  corr[7,17] <- distEuclidean(user7, user17)
  corr[7,18] <- distEuclidean(user7, user18)
  corr[7,19] <- distEuclidean(user7, user19)
  corr[7,20] <- distEuclidean(user7, user20)
  
  # User 8
  corr[8,9] <- distEuclidean(user8, user9)
  corr[8,10] <- distEuclidean(user8, user10)
  corr[8,11] <- distEuclidean(user8, user11)
  corr[8,12] <- distEuclidean(user8, user12)
  corr[8,13] <- distEuclidean(user8, user13)
  corr[8,14] <- distEuclidean(user8, user14)
  corr[8,15] <- distEuclidean(user8, user15)
  corr[8,16] <- distEuclidean(user8, user16)
  corr[8,17] <- distEuclidean(user8, user17)
  corr[8,18] <- distEuclidean(user8, user18)
  corr[8,19] <- distEuclidean(user8, user19)
  corr[8,20] <- distEuclidean(user8, user20)
  
  # User 9
  corr[9,10] <- distEuclidean(user9, user10)
  corr[9,11] <- distEuclidean(user9, user11)
  corr[9,12] <- distEuclidean(user9, user12)
  corr[9,13] <- distEuclidean(user9, user13)
  corr[9,14] <- distEuclidean(user9, user14)
  corr[9,15] <- distEuclidean(user9, user15)
  corr[9,16] <- distEuclidean(user9, user16)
  corr[9,17] <- distEuclidean(user9, user17)
  corr[9,18] <- distEuclidean(user9, user18)
  corr[9,19] <- distEuclidean(user9, user19)
  corr[9,20] <- distEuclidean(user9, user20)
  
  # User 10
  corr[10,11] <- distEuclidean(user10, user11)
  corr[10,12] <- distEuclidean(user10, user12)
  corr[10,13] <- distEuclidean(user10, user13)
  corr[10,14] <- distEuclidean(user10, user14)
  corr[10,15] <- distEuclidean(user10, user15)
  corr[10,16] <- distEuclidean(user10, user16)
  corr[10,17] <- distEuclidean(user10, user17)
  corr[10,18] <- distEuclidean(user10, user18)
  corr[10,19] <- distEuclidean(user10, user19)
  corr[10,20] <- distEuclidean(user10, user20)
  
  # User 11
  corr[11,12] <- distEuclidean(user11, user12)
  corr[11,13] <- distEuclidean(user11, user13)
  corr[11,14] <- distEuclidean(user11, user14)
  corr[11,15] <- distEuclidean(user11, user15)
  corr[11,16] <- distEuclidean(user11, user16)
  corr[11,17] <- distEuclidean(user11, user17)
  corr[11,18] <- distEuclidean(user11, user18)
  corr[11,19] <- distEuclidean(user11, user19)
  corr[11,20] <- distEuclidean(user11, user20)
  
  # User 12
  corr[12,13] <- distEuclidean(user12, user13)
  corr[12,14] <- distEuclidean(user12, user14)
  corr[12,15] <- distEuclidean(user12, user15)
  corr[12,16] <- distEuclidean(user12, user16)
  corr[12,17] <- distEuclidean(user12, user17)
  corr[12,18] <- distEuclidean(user12, user18)
  corr[12,19] <- distEuclidean(user12, user19)
  corr[12,20] <- distEuclidean(user12, user20)
  
  # User 13
  corr[13,14] <- distEuclidean(user13, user14)
  corr[13,15] <- distEuclidean(user13, user15)
  corr[13,16] <- distEuclidean(user13, user16)
  corr[13,17] <- distEuclidean(user13, user17)
  corr[13,18] <- distEuclidean(user13, user18)
  corr[13,19] <- distEuclidean(user13, user19)
  corr[13,20] <- distEuclidean(user13, user20)
  
  # User 14
  corr[14,15] <- distEuclidean(user14, user15)
  corr[14,16] <- distEuclidean(user14, user16)
  corr[14,17] <- distEuclidean(user14, user17)
  corr[14,18] <- distEuclidean(user14, user18)
  corr[14,19] <- distEuclidean(user14, user19)
  corr[14,20] <- distEuclidean(user14, user20)
  
  # User 15
  corr[15,16] <- distEuclidean(user15, user16)
  corr[15,17] <- distEuclidean(user15, user17)
  corr[15,18] <- distEuclidean(user15, user18)
  corr[15,19] <- distEuclidean(user15, user19)
  corr[15,20] <- distEuclidean(user15, user20)
  
  # User 16
  corr[16,17] <- distEuclidean(user16, user17)
  corr[16,18] <- distEuclidean(user16, user18)
  corr[16,19] <- distEuclidean(user16, user19)
  corr[16,20] <- distEuclidean(user16, user20)
  
  # User 17
  corr[17,18] <- distEuclidean(user17, user18)
  corr[17,19] <- distEuclidean(user17, user19)
  corr[17,20] <- distEuclidean(user17, user20)
  
  # User 18
  corr[18,19] <- distEuclidean(user18, user19)
  corr[18,20] <- distEuclidean(user18, user20)
  
  # User 19
  corr[19,20] <- distEuclidean(user19, user20)
  
  # Preenchendo o restante da matriz
  for(i in 1:20){ # linha
    for(j in 1:20){ # coluna
      if(i == j){
        corr[i,j] <- 0
      } else if(corr[i,j] != 0){
        corr[j,i] <- corr[i,j]
      }
    }
  }
  
  return(corr)
}



# Construindo uma matriz de similaridade com DTW
distanciaDTW_20U <- function(user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,
                            user11,user12,user13,user14,user15,user16,user17,user18,user19,user20){
  # Criando a matriz
  corr <- matrix(data = 0, nrow = 20, ncol = 20)

  # User 1
  corr[1,2] <- dtw(user1[,2], user2[,2])$normalizedDistance
  corr[1,3] <- dtw(user1[,2], user3[,2])$normalizedDistance
  corr[1,4] <- dtw(user1[,2], user4[,2])$normalizedDistance
  corr[1,5] <- dtw(user1[,2], user5[,2])$normalizedDistance
  corr[1,6] <- dtw(user1[,2], user6[,2])$normalizedDistance
  corr[1,7] <- dtw(user1[,2], user7[,2])$normalizedDistance
  corr[1,8] <- dtw(user1[,2], user8[,2])$normalizedDistance
  corr[1,9] <- dtw(user1[,2], user9[,2])$normalizedDistance
  corr[1,10] <- dtw(user1[,2], user10[,2])$normalizedDistance
  corr[1,11] <- dtw(user1[,2], user11[,2])$normalizedDistance
  corr[1,12] <- dtw(user1[,2], user12[,2])$normalizedDistance
  corr[1,13] <- dtw(user1[,2], user13[,2])$normalizedDistance
  corr[1,14] <- dtw(user1[,2], user14[,2])$normalizedDistance
  corr[1,15] <- dtw(user1[,2], user15[,2])$normalizedDistance
  corr[1,16] <- dtw(user1[,2], user16[,2])$normalizedDistance
  corr[1,17] <- dtw(user1[,2], user17[,2])$normalizedDistance
  corr[1,18] <- dtw(user1[,2], user18[,2])$normalizedDistance
  corr[1,19] <- dtw(user1[,2], user19[,2])$normalizedDistance
  corr[1,20] <- dtw(user1[,2], user20[,2])$normalizedDistance
  
  # User 2
  corr[2,3] <- dtw(user2[,2], user3[,2])$normalizedDistance
  corr[2,4] <- dtw(user2[,2], user4[,2])$normalizedDistance
  corr[2,5] <- dtw(user2[,2], user5[,2])$normalizedDistance
  corr[2,6] <- dtw(user2[,2], user6[,2])$normalizedDistance
  corr[2,7] <- dtw(user2[,2], user7[,2])$normalizedDistance
  corr[2,8] <- dtw(user2[,2], user8[,2])$normalizedDistance
  corr[2,9] <- dtw(user2[,2], user9[,2])$normalizedDistance
  corr[2,10] <- dtw(user2[,2], user10[,2])$normalizedDistance
  corr[2,11] <- dtw(user2[,2], user11[,2])$normalizedDistance
  corr[2,12] <- dtw(user2[,2], user12[,2])$normalizedDistance
  corr[2,13] <- dtw(user2[,2], user13[,2])$normalizedDistance
  corr[2,14] <- dtw(user2[,2], user14[,2])$normalizedDistance
  corr[2,15] <- dtw(user2[,2], user15[,2])$normalizedDistance
  corr[2,16] <- dtw(user2[,2], user16[,2])$normalizedDistance
  corr[2,17] <- dtw(user2[,2], user17[,2])$normalizedDistance
  corr[2,18] <- dtw(user2[,2], user18[,2])$normalizedDistance
  corr[2,19] <- dtw(user2[,2], user19[,2])$normalizedDistance
  corr[2,20] <- dtw(user2[,2], user20[,2])$normalizedDistance
  
  # User 3
  corr[3,4] <- dtw(user3[,2], user4[,2])$normalizedDistance
  corr[3,5] <- dtw(user3[,2], user5[,2])$normalizedDistance
  corr[3,6] <- dtw(user3[,2], user6[,2])$normalizedDistance
  corr[3,7] <- dtw(user3[,2], user7[,2])$normalizedDistance
  corr[3,8]  <- dtw(user3[,2], user8[,2])$normalizedDistance
  corr[3,9] <- dtw(user3[,2], user9[,2])$normalizedDistance
  corr[3,10] <- dtw(user3[,2], user10[,2])$normalizedDistance
  corr[3,11] <- dtw(user3[,2], user11[,2])$normalizedDistance
  corr[3,12] <- dtw(user3[,2], user12[,2])$normalizedDistance
  corr[3,13] <- dtw(user3[,2], user13[,2])$normalizedDistance
  corr[3,14] <- dtw(user3[,2], user14[,2])$normalizedDistance
  corr[3,15] <- dtw(user3[,2], user15[,2])$normalizedDistance
  corr[3,16] <- dtw(user3[,2], user16[,2])$normalizedDistance
  corr[3,17] <- dtw(user3[,2], user17[,2])$normalizedDistance
  corr[3,18] <- dtw(user3[,2], user18[,2])$normalizedDistance
  corr[3,19] <- dtw(user3[,2], user19[,2])$normalizedDistance
  corr[3,20] <- dtw(user3[,2], user20[,2])$normalizedDistance
  
  # User 4
  corr[4,5] <- dtw(user4[,2], user5[,2])$normalizedDistance
  corr[4,6] <- dtw(user4[,2], user6[,2])$normalizedDistance
  corr[4,7] <- dtw(user4[,2], user7[,2])$normalizedDistance
  corr[4,8] <- dtw(user4[,2], user8[,2])$normalizedDistance
  corr[4,9] <- dtw(user4[,2], user9[,2])$normalizedDistance
  corr[4,10] <- dtw(user4[,2], user10[,2])$normalizedDistance
  corr[4,11] <- dtw(user4[,2], user11[,2])$normalizedDistance
  corr[4,12] <- dtw(user4[,2], user12[,2])$normalizedDistance
  corr[4,13] <- dtw(user4[,2], user13[,2])$normalizedDistance
  corr[4,14] <- dtw(user4[,2], user14[,2])$normalizedDistance
  corr[4,15] <- dtw(user4[,2], user15[,2])$normalizedDistance
  corr[4,16] <- dtw(user4[,2], user16[,2])$normalizedDistance
  corr[4,17] <- dtw(user4[,2], user17[,2])$normalizedDistance
  corr[4,18] <- dtw(user4[,2], user18[,2])$normalizedDistance
  corr[4,19] <- dtw(user4[,2], user19[,2])$normalizedDistance
  corr[4,20] <- dtw(user4[,2], user20[,2])$normalizedDistance
  
  # User 5
  corr[5,6] <- dtw(user5[,2], user6[,2])$normalizedDistance
  corr[5,7] <- dtw(user5[,2], user7[,2])$normalizedDistance
  corr[5,8] <- dtw(user5[,2], user8[,2])$normalizedDistance
  corr[5,9] <- dtw(user5[,2], user9[,2])$normalizedDistance
  corr[5,10] <- dtw(user5[,2], user10[,2])$normalizedDistance
  corr[5,11] <- dtw(user5[,2], user11[,2])$normalizedDistance
  corr[5,12] <- dtw(user5[,2], user12[,2])$normalizedDistance
  corr[5,13] <- dtw(user5[,2], user13[,2])$normalizedDistance
  corr[5,14] <- dtw(user5[,2], user14[,2])$normalizedDistance
  corr[5,15] <- dtw(user5[,2], user15[,2])$normalizedDistance
  corr[5,16] <- dtw(user5[,2], user16[,2])$normalizedDistance
  corr[5,17] <- dtw(user5[,2], user17[,2])$normalizedDistance
  corr[5,18] <- dtw(user5[,2], user18[,2])$normalizedDistance
  corr[5,19] <- dtw(user5[,2], user19[,2])$normalizedDistance
  corr[5,20] <- dtw(user5[,2], user20[,2])$normalizedDistance
  
  # User 6
  corr[6,7] <- dtw(user6[,2], user7[,2])$normalizedDistance
  corr[6,8] <- dtw(user6[,2], user8[,2])$normalizedDistance
  corr[6,9] <- dtw(user6[,2], user9[,2])$normalizedDistance
  corr[6,10] <- dtw(user6[,2], user10[,2])$normalizedDistance
  corr[6,11] <- dtw(user6[,2], user11[,2])$normalizedDistance
  corr[6,12] <- dtw(user6[,2], user12[,2])$normalizedDistance
  corr[6,13] <- dtw(user6[,2], user13[,2])$normalizedDistance
  corr[6,14] <- dtw(user6[,2], user14[,2])$normalizedDistance
  corr[6,15] <- dtw(user6[,2], user15[,2])$normalizedDistance
  corr[6,16] <- dtw(user6[,2], user16[,2])$normalizedDistance
  corr[6,17] <- dtw(user6[,2], user17[,2])$normalizedDistance
  corr[6,18] <- dtw(user6[,2], user18[,2])$normalizedDistance
  corr[6,19] <- dtw(user6[,2], user19[,2])$normalizedDistance
  corr[6,20] <- dtw(user6[,2], user20[,2])$normalizedDistance
  
  # User 7
  corr[7,8] <- dtw(user7[,2], user8[,2])$normalizedDistance
  corr[7,9] <- dtw(user7[,2], user9[,2])$normalizedDistance
  corr[7,10] <- dtw(user7[,2], user10[,2])$normalizedDistance
  corr[7,11] <- dtw(user7[,2], user11[,2])$normalizedDistance
  corr[7,12] <- dtw(user7[,2], user12[,2])$normalizedDistance
  corr[7,13] <- dtw(user7[,2], user13[,2])$normalizedDistance
  corr[7,14] <- dtw(user7[,2], user14[,2])$normalizedDistance
  corr[7,15] <- dtw(user7[,2], user15[,2])$normalizedDistance
  corr[7,16] <- dtw(user7[,2], user16[,2])$normalizedDistance
  corr[7,17] <- dtw(user7[,2], user17[,2])$normalizedDistance
  corr[7,18] <- dtw(user7[,2], user18[,2])$normalizedDistance
  corr[7,19] <- dtw(user7[,2], user19[,2])$normalizedDistance
  corr[7,20] <- dtw(user7[,2], user20[,2])$normalizedDistance
  
  # User 8
  corr[8,9] <- dtw(user8[,2], user9[,2])$normalizedDistance
  corr[8,10] <- dtw(user8[,2], user10[,2])$normalizedDistance
  corr[8,11] <- dtw(user8[,2], user11[,2])$normalizedDistance
  corr[8,12] <- dtw(user8[,2], user12[,2])$normalizedDistance
  corr[8,13] <- dtw(user8[,2], user13[,2])$normalizedDistance
  corr[8,14] <- dtw(user8[,2], user14[,2])$normalizedDistance
  corr[8,15] <- dtw(user8[,2], user15[,2])$normalizedDistance
  corr[8,16] <- dtw(user8[,2], user16[,2])$normalizedDistance
  corr[8,17] <- dtw(user8[,2], user17[,2])$normalizedDistance
  corr[8,18] <- dtw(user8[,2], user18[,2])$normalizedDistance
  corr[8,19] <- dtw(user8[,2], user19[,2])$normalizedDistance
  corr[8,20] <- dtw(user8[,2], user20[,2])$normalizedDistance
  
  # User 9
  corr[9,10] <- dtw(user9[,2], user10[,2])$normalizedDistance
  corr[9,11] <- dtw(user9[,2], user11[,2])$normalizedDistance
  corr[9,12] <- dtw(user9[,2], user12[,2])$normalizedDistance
  corr[9,13] <- dtw(user9[,2], user13[,2])$normalizedDistance
  corr[9,14] <- dtw(user9[,2], user14[,2])$normalizedDistance
  corr[9,15] <- dtw(user9[,2], user15[,2])$normalizedDistance
  corr[9,16] <- dtw(user9[,2], user16[,2])$normalizedDistance
  corr[9,17] <- dtw(user9[,2], user17[,2])$normalizedDistance
  corr[9,18] <- dtw(user9[,2], user18[,2])$normalizedDistance
  corr[9,19] <- dtw(user9[,2], user19[,2])$normalizedDistance
  corr[9,20] <- dtw(user9[,2], user20[,2])$normalizedDistance
  
  # User 10
  corr[10,11] <- dtw(user10[,2], user11[,2])$normalizedDistance
  corr[10,12] <- dtw(user10[,2], user12[,2])$normalizedDistance
  corr[10,13] <- dtw(user10[,2], user13[,2])$normalizedDistance
  corr[10,14] <- dtw(user10[,2], user14[,2])$normalizedDistance
  corr[10,15] <- dtw(user10[,2], user15[,2])$normalizedDistance
  corr[10,16] <- dtw(user10[,2], user16[,2])$normalizedDistance
  corr[10,17] <- dtw(user10[,2], user17[,2])$normalizedDistance
  corr[10,18] <- dtw(user10[,2], user18[,2])$normalizedDistance
  corr[10,19] <- dtw(user10[,2], user19[,2])$normalizedDistance
  corr[10,20] <- dtw(user10[,2], user20[,2])$normalizedDistance
  
  # User 11
  corr[11,12] <- dtw(user11[,2], user12[,2])$normalizedDistance
  corr[11,13] <- dtw(user11[,2], user13[,2])$normalizedDistance
  corr[11,14] <- dtw(user11[,2], user14[,2])$normalizedDistance
  corr[11,15] <- dtw(user11[,2], user15[,2])$normalizedDistance
  corr[11,16] <- dtw(user11[,2], user16[,2])$normalizedDistance
  corr[11,17] <- dtw(user11[,2], user17[,2])$normalizedDistance
  corr[11,18] <- dtw(user11[,2], user18[,2])$normalizedDistance
  corr[11,19] <- dtw(user11[,2], user19[,2])$normalizedDistance
  corr[11,20] <- dtw(user11[,2], user20[,2])$normalizedDistance
  
  # User 12
  corr[12,13] <- dtw(user12[,2], user13[,2])$normalizedDistance
  corr[12,14] <- dtw(user12[,2], user14[,2])$normalizedDistance
  corr[12,15] <- dtw(user12[,2], user15[,2])$normalizedDistance
  corr[12,16] <- dtw(user12[,2], user16[,2])$normalizedDistance
  corr[12,17] <- dtw(user12[,2], user17[,2])$normalizedDistance
  corr[12,18] <- dtw(user12[,2], user18[,2])$normalizedDistance
  corr[12,19] <- dtw(user12[,2], user19[,2])$normalizedDistance
  corr[12,20] <- dtw(user12[,2], user20[,2])$normalizedDistance
  
  # User 13
  corr[13,14] <- dtw(user13[,2], user14[,2])$normalizedDistance
  corr[13,15] <- dtw(user13[,2], user15[,2])$normalizedDistance
  corr[13,16] <- dtw(user13[,2], user16[,2])$normalizedDistance
  corr[13,17] <- dtw(user13[,2], user17[,2])$normalizedDistance
  corr[13,18] <- dtw(user13[,2], user18[,2])$normalizedDistance
  corr[13,19] <- dtw(user13[,2], user19[,2])$normalizedDistance
  corr[13,20] <- dtw(user13[,2], user20[,2])$normalizedDistance
  
  # User 14
  corr[14,15] <- dtw(user14[,2], user15[,2])$normalizedDistance
  corr[14,16] <- dtw(user14[,2], user16[,2])$normalizedDistance
  corr[14,17] <- dtw(user14[,2], user17[,2])$normalizedDistance
  corr[14,18] <- dtw(user14[,2], user18[,2])$normalizedDistance
  corr[14,19] <- dtw(user14[,2], user19[,2])$normalizedDistance
  corr[14,20] <- dtw(user14[,2], user20[,2])$normalizedDistance
  
  # User 15
  corr[15,16] <- dtw(user15[,2], user16[,2])$normalizedDistance
  corr[15,17] <- dtw(user15[,2], user17[,2])$normalizedDistance
  corr[15,18] <- dtw(user15[,2], user18[,2])$normalizedDistance
  corr[15,19] <- dtw(user15[,2], user19[,2])$normalizedDistance
  corr[15,20] <- dtw(user15[,2], user20[,2])$normalizedDistance
  
  # User 16
  corr[16,17] <- dtw(user16[,2], user17[,2])$normalizedDistance
  corr[16,18] <- dtw(user16[,2], user18[,2])$normalizedDistance
  corr[16,19] <- dtw(user16[,2], user19[,2])$normalizedDistance
  corr[16,20] <- dtw(user16[,2], user20[,2])$normalizedDistance
  
  # User 17
  corr[17,18] <- dtw(user17[,2], user18[,2])$normalizedDistance
  corr[17,19] <- dtw(user17[,2], user19[,2])$normalizedDistance
  corr[17,20] <- dtw(user17[,2], user20[,2])$normalizedDistance
  
  # User 18
  corr[18,19] <- dtw(user18[,2], user19[,2])$normalizedDistance
  corr[18,20] <- dtw(user18[,2], user20[,2])$normalizedDistance
  
  # User 19
  corr[19,20] <- dtw(user19[,2], user20[,2])$normalizedDistance

  # Preenchendo o restante da matriz
  for(i in 1:20){ # linha
    for(j in 1:20){ # coluna
      if(i == j){
        corr[i,j] <- 0
      } else if(corr[i,j] != 0){
        corr[j,i] <- corr[i,j]
      }
    }
  }
  
  return(corr)
}




