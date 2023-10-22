# E S E R C I Z I O   1 3   O T T O B R E


### Per svolgere i seguenti esercizi, si faccia affidamento al database "Ottobre.csv".

setwd("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi")
ottobre <- read_delim("Dataset/Ottobre.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


## Esercizio 0
## Creare una nuova colonna, che sia il prodotto delle prime due. Di questa colonna, Si inserisca il primo quartile come valore nella cassafoRte (arrotondato alla seconda cifra decimale, se bisogna).
ottobre$prodotto <- ottobre$Age * ottobre$Income
round( quantile(ottobre$prodotto), 2 )

## Esercizio 1
## Per la colonna Category, si calcolino le frequenze e si inserisca nella cassafoRte il conteggio dei soggetti appartenenti alla categoria "B".
table( ottobre$Category )

## Esercizio 2
## Creare una nuova colonna secondo questa logica:
# - se il valore della cella nella prima colonna è inferiore o uguale a 42, il corrispondente valore della cella della nuova colonna dovrà essere 1
# - se il valore della cella nella prima colonna è maggiore di 42, il corrispondente valore della cella della nuova colonna dovrà essere 2.
# Una volta creata la colonna, si sommino tutti i sui valori all interno e si inserisca nella cassaforte la radice quadrata ti tale valore, arrotondato al secondo decimale.

ottobre$controllo <- (ottobre$Age > 42) +1
round( sqrt( sum(ottobre$contrllo) ), 2 )
