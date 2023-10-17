# E S E R C I Z I O   1 3   O T T O B R E

# esercizi R base
# usare il dataset "prova.csv"

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
prova <- read_delim("Dataset/prova.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


# 1. Quante righe e quante colonne ci sono in prova? Trovare la funzione specifica per questa domanda.

NCOL(prova) # dim(prova)
NROW(prova)


# 2. Di che tipo sono le variabili che contiene?

str(prova)


# Calcolare la deviazione standard di Ore_studio. RISULTATO UTILE PER LA CASSAFORTE! (ARROTONDATO ALLA SECONDA CIFRA DECIMALE)

sd_OreStudio <- round( sd(prova$Ore_studio), 2 )


# In media, quanti spritz si bevono nel bar C? RISULTATO UTILE PER LA CASSAFORTE! (ARROTONDATO ALLA SECONDA CIFRA DECIMALE)

mean_spritzC <- round( mean( prova[prova$Bar == 'C',]$Spritz ), 2 ) 
#aggregate(prova[,2], list(prova[,1]), mean)
#tapply(prova$Spritz, prova$Bar, mean)


# Creare una variabile "corso" che contenga il  corso di studio.
# Nelle prime 13 righe inserire "psicologia",
# nelle seconde 13 inserire "ingegneria",
# nelle 13 successive inserire "biologia",
# nelle ultime 19, inserire "statistica".

# In quale corso si bevono più spritz in media? LA MEDIA MAGGIORE SARà RISULTATO UTILE PER LA CASSAFORTE! (ARROTONDATO ALLA SECONDA CIFRA DECIMALE)

prova$corso <- c( rep('psicologia', 13), rep('ingegneria', 13), rep('biologia', 13), rep('statistica', 19) )

round( tapply(prova$Spritz, prova$corso, mean), 2)
#mean_psi <- round( mean( prova[prova$corso == 'psicologia',]$Spritz ), 2 )
#mean_ing <- round( mean( prova[prova$corso == 'ingegneria',]$Spritz ), 2 )
#mean_bio <- round( mean( prova[prova$corso == 'biologia',]$Spritz ), 2 )
#mean_sta <- round( mean( prova[prova$corso == 'statistica',]$Spritz ), 2 )

## USARE IL PUNTO PER I DECIMALI