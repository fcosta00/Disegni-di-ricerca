# Pratica alpha di chronbach

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
ansia <- read.csv("Dataset/Ansia.csv", sep=";")

str(ansia)

# calcolare l'alpha di chroncìbach con matrice di covarianza
library(RcmdrMisc)
reliability( cov(ansia) ) # puoi ridurre il dataset specificando le colonne ansia[2:5,]
# ultima colonna è la correlazione item-totale -> cioè la correlazione tra ogni item e tutta la scala

# in generale il valore soglia sopra il quale possiamo dire che lo strumento è adeguatamente affidabile è 0.70
# 0.7 dice che si è sulla linea di galleggiamento, bisonga in ogni caso controllare gli item singoli per verificare che non generino rumore o dati sballati

# item 7 e 11 devono essere invertiti
# (il minimo valore + il massimo valore) - il valore che vogliamo invertire

range(ansia)

# invertiamo prima l'item con la correlazione più elevata in correlazione assoluto
#i passaggi di inversione vanno eseguiti uno alla volta

ansia$PRE_7_inv <- (1+4) - ansia$PRE_7
head(ansia$PRE_7)
head(ansia$PRE_7_inv)
reliability( cov(ansia[,c(1:6,12,8:11)]) )
#l'alpha e l'std.alpha è aumentato

#visto che item 11 ancora negativo 

ansia$PRE_11_inv <- (1+4) - ansia$PRE_11 
head(ansia$PRE_11)
head(ansia$PRE_11_inv)
reliability( cov(ansia[,c(1:6,12,8:10,13)]) )
#l'alhpa è aumentato ulteriormente e non ci sono più item negativi
# SOGLIA PER ELIMINARE ITEM -> due approcci generali, basta dichiarare o 0.1 <= si elimina, o 0.2 <= si elimina

#L'ordine per le operazioni da effettuare è sempre l'inversione, e poi l'eliminazione. Sempre con un passaggio singolo alla volta

reliability( cov(ansia[,c(1:6,12,8:10)]) )
