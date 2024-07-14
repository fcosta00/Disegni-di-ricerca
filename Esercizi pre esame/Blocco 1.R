{
  library(RcmdrMisc)
  
  head( alpha1 )
  bkp <- alpha1
  
  reliability( cov(alpha1) )
  
  #inverto Domanda13
  alpha1$Domanda13inv <- (1 + 4) - alpha1$Domanda13
  reliability( cov(alpha1[, c(1:12, 14:15)]) )
  
  #inverto Domanda5
  alpha1$Domanda5inv <- (1 + 4) - alpha1$Domanda5
  reliability( cov(alpha1[, c(1:4, 6:12, 14:16)]) )
  
  #inverto Domanda10
  alpha1$Domanda10inv <- (1 + 4) - alpha1$Domanda10
  reliability( cov(alpha1[, c(1:4, 6:9, 11:12, 14:17)]) )
  
  #inverto Domanda4
  alpha1$Domanda4inv <- (1 + 4) - alpha1$Domanda4
  reliability( cov(alpha1[, c(1:3, 6:9, 11:12, 14:18)]) )
  
  #inverto Domanda3
  alpha1$Domanda3inv <- (1 + 4) - alpha1$Domanda3
  reliability( cov(alpha1[, c(1:2, 6:9, 11:12, 14:19)]) )
  
  #inverto Domanda9
  alpha1$Domanda9inv <- (1 + 4) - alpha1$Domanda9
  reliability( cov(alpha1[, c(1:2, 6:9, 11:12, 14:19)]) )
  
  #rimetto il 10
  reliability( cov(alpha1[, c(1:2, 6:8, 10:12, 14:16, 18:20)]) )
  
  #inverto Domanda14
  alpha1$Domanda14inv <- (1 + 4) - alpha1$Domanda14
  reliability( cov(alpha1[, c(1:2, 6:8, 10:12, 15:16, 18:21)]) )
  
  #procedo con l'eliminazione in base all'alpha sotto a 0.1
  #elimino domanda 14inv
  reliability( cov(alpha1[, c(1:2, 6:8, 10:12, 15:16, 18:20)]) )
  
  #elimino domanda 10
  reliability( cov(alpha1[, c(1:2, 6:8, 11:12, 15:16, 18:20)]) )
  
  #elimino domanda 3 inv
  reliability( cov(alpha1[, c(1:2, 6:8, 11:12, 15:16, 18, 20)]) )
  
  #elimino domanda 9 inv
  reliability( cov(alpha1[, c(1:2, 6:8, 11:12, 15:16, 18)]) )
  
  #provo a eliminare sotto 0.2 perchè alpha della scala ancora troppo basso
  #elimino domanda 8
  reliability( cov(alpha1[, c(1:2, 6:7, 11:12, 15:16, 18)]) )
  
  #elimino domanda 4 inv
  reliability( cov(alpha1[, c(1:2, 6:7, 11:12, 15:16)]) )
  
  #elimino domanda 12
  reliability( cov(alpha1[, c(1:2, 6:7, 11, 15:16)]) )
  
  #elimino domanda 11
  reliability( cov(alpha1[, c(1:2, 6:7, 15:16)]) )
  
  #elimino domanda 11
  reliability( cov(alpha1[, c(1:2, 6:7, 16)]) )
  
  
}# alpha 1

{
  bkp2 <- alpha2
  
  head(alpha2)
  
  library(RcmdrMisc)
  reliability( cov(alpha2))
  
  # inverto item PRE11
  alpha2$PRE11inv <- 5 - alpha2$PRE11
  reliability( cov(alpha2[, c(1:10, 12:16)]))
  
  # inverto item PRE10
  alpha2$PRE10inv <- 5 - alpha2$PRE10
  reliability( cov(alpha2[, c(1:9, 12:17)]))
  
  # inverto item PRE13
  alpha2$PRE13inv <- 5 - alpha2$PRE13
  reliability( cov(alpha2[, c(1:9, 12, 14:18)]))
  
  #finita inversione
  #alpha è gia sufficiente (>0.7) ma ci sono item che non rispecchiano bene il costrutto 
  #elimino item con r < 0.1
  
  #elimino Pre6
  reliability( cov(alpha2[, c(1:5, 7:9, 12, 14:18)]))
  
  #elimino Pre13inv
  reliability( cov(alpha2[, c(1:5, 7:9, 12, 14:17)]))
  
  #tutti gli item rispecchiano bene il costrutto totale. La reliability interna è buona con un valore standardizzato di 0.82
  
  
  
}# alpha 2

{
  library(RcmdrMisc)
  
  bkp3 <- alpha3
  
  head(alpha3)
  
  reliability( cov(alpha3) )
  
  #ci sono degli item invertiti
  
  #inverto item_13
  alpha3$Item_13inv <- 3 - alpha3$Item_13
  reliability( cov(alpha3[, -c(13)]) )

  #inverto item_10
  alpha3$Item_10inv <- 3 - alpha3$Item_10
  reliability( cov(alpha3[, -c(13, 10)]) )  
  
  #inverto item_16
  alpha3$Item_16inv <- 3 - alpha3$Item_16
  reliability( cov(alpha3[, -c(13, 10, 16)]) ) 
  
  #inverto item_1
  alpha3$Item_1inv <- 3 - alpha3$Item_1
  reliability( cov(alpha3[, -c(13, 10, 16, 1)]) ) 
  
  #il risultato dell'alpha è già buono (>0.84) ma vado ad eliminare gli item sotto 0.1 perchè non riflettono bene la scala
  
  #elimino item 5
  reliability( cov(alpha3[, -c(13, 10, 16, 1, 5)]) ) 
  
}# alpha 3