
# Calcolo della k di cohen passaggio per passaggio, seguendo le slide

tabella <-  matrix( c(18, 2, 3, 26), 2, 2, byrow = T)
tabella

rowSums(tabella)
colSums(tabella)

AO = 18 + 26

N =  (18 + 2) + (3 + 26)

AA0 = (20*21) / N
AA1 = (28*29) / N

K = (AO - (AA0+AA1) ) / (N - (AA0+AA1))


# importo dataset cohen
head(cohen)

cTable <- table(cohen) # reazione tabella doppia entrata frequenza di dicotomiche

library( psych )
cohen.kappa( cTable ) # funzione per calcolare K in modo automatico


# soluzione alternativa con pacchetto irr
library(irr)
kappa2(cohen) # in questo caso caricare solo dataset, o meglio le colonne di cui calcolare
# ci fornisce anche l'indice statistico associato al k ma non l'intervallo di confidenza




