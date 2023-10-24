# Pratica alpha di chronbach

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
ansia <- read.csv("Dataset/Ansia.csv", sep=";")

str(ansia)

# calcolare l'alpha di chroncìbach con matrice di covarianza
library(RcmdrMisc)
reliability( cov(ansia) ) # puoi ridurre il dataset specificando le colonne ansia[2:5,]

