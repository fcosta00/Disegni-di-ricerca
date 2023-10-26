# A L P H A    1 ----

# Importazione dataset Esame.csv e chiamata librerie necessarie
library(RcmdrMisc)

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
esame <- read.csv("Dataset/Esame.csv", sep=";")

# Si calcoli l’α di Cronbach (fase PRE) del questionario, si commenti il risultato ottenuto
head(esame)

esame$PRE10_inv <- (1+4) - esame$PRE10    # inverto item pre10
reliability( cov( esame[,c(3:11, 33, 13:17)] ) )
# l'alpha è sotto il livello di sufficienza, ma diversi item si presentano inversi. 
# Si effettuano le opportune inversioni una alla volta, partendo da quelle con |r| più alto, sinchè tutte le correlazioni non sono positive

esame$PRE11_inv <- (1+4) - esame$PRE11    # inverto item pre11
reliability( cov( esame[,c(3:11, 33, 34, 14:17)] ) )

esame$PRE13_inv <- (1+4) - esame$PRE13    # inverto item pre11
reliability( cov( esame[,c(3:11, 33, 34, 14, 35, 16, 17)] ) )
# tutte gli item hanno correlazione positiva. 
# L'alpha ha un valore accettabile ma procedo con l'eliminazione degli item con correlazione bassa per aumentare il valore e rendere la scala più coerente

reliability( cov( esame[,c(3:7, 9:11, 33, 34, 14, 35, 16, 17)] ) ) # elimino PRE6
# l'alpha inizia ad avere un buon valore ma proseguo con l'eliminazione di pre_13_inv vista la cor di 0.06

reliability( cov( esame[,c(3:7, 9:11, 33, 34, 14, 16, 17)] ) ) # elimino PRE13
# l'alpha ha un buon valore e tutti gli item hanno un impatto positivo sul totale mostrando una coerenza della scala




# A L P H A    2 ----

# Importazione dataset benessere.csv e chiamata librerie necessarie
library(RcmdrMisc)

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
benessere <- read.csv("Dataset/Benessere.csv", sep=";")

# Si calcoli l’α di Cronbach (fase PRE) del questionario, si commenti il risultato ottenuto
head(benessere)

benessere$PRE4_inv <- (1+4) - benessere$PRE4    # inverto item pre4
reliability( cov( benessere[,c(3:5, 33, 7:17)] ) )
# l'alpha sopra la soglia di sufficienza, ma un altro item è inverso. 
# Si effettuano le opportune inversioni una alla volta, partendo da quelle con |r| più alto, sinchè tutte le correlazioni non sono positive

benessere$PRE12_inv <- (1+4) - benessere$PRE12    # inverto item pre12
reliability( cov( benessere[,c(3:5, 33, 7:13, 34, 15:17)] ) )
# tutti gli item hanno correlazione positiva
# l'alpha raggiunge un buon valore ma si procede con l'eliminaizione dell'item pre4 per rendere la scala più coerente

reliability( cov( benessere[,c(3:5, 7:13, 34, 15:17)] ) )
# l'alpha ha un buon valore e tutti gli item hanno un impatto positivo sul totale mostrando una coerenza della scala



# A L P H A    3 -----

# Importazione dataset benessere.csv e chiamata librerie necessarie

library(RcmdrMisc)

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
gioia <- read.csv("Dataset/gioia.csv", sep=";")

# Si calcoli l’α di Cronbach (fase POST) del questionario, si commenti il risultato ottenuto
head(gioia)

reliability( cov( gioia[,11:19] ) )
# procedo con l'inversione degli item negativi partendo da quelli con l' |r| maggiore

gioia$POST_1_inv <- (1+4) - gioia$POST_1    # inverto item post1
reliability( cov( gioia[,c(20, 12:19)] ) )

gioia$POST_7_inv <- (1+4) - gioia$POST_7    # inverto item post1
reliability( cov( gioia[,c(20, 12:16, 21, 18, 19)] ) )
# gli item sono tutti con r postivo ma l'alpha è insufficiente
# si procede con l'eliminazione degli item con r < 0.2

reliability( cov( gioia[,c(20, 12:16, 18, 19)] ) ) # elm post 7

reliability( cov( gioia[,c(20, 12, 14, 15, 16, 18, 19)] ) ) # elm post 3

reliability( cov( gioia[,c(12, 14, 15, 16, 18, 19)] ) ) # elm post 1

reliability( cov( gioia[,c(12, 14, 15, 16, 18)] ) ) # elm post 9
# l'alpha rimane insufficiente e apportare ulteriori riduzioni di scala la renderebbe con insufficienti item
# possiamo dire he questo questionario non ha coerenza interna
