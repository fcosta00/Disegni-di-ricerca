## Carichiamo il dataframe "Altezza.csv"

dati<-read.csv(file.choose(),sep = ";",dec=",",header=T)
str(dati)
# Calcoliamo la media del predittore
media_altezza<-mean(dati$Altezza)

# Calcoliamo la media della variabile dipendente
media_agilita<-mean(dati$Agilita)

# Calcoliamo gli scarti dalla media del predittore
dati$x_xmedio<-dati$Altezza-media_altezza

# Calcoliamo gli scarti dalla media del predittore, al quadrato
dati$x_xmedio_quadrato<-dati$x_xmedio^2

# Calcoliamo gli scarti dalla media della variabile dipendente
dati$y_ymedio<-dati$Agilita-media_agilita

# Calcoliamo il prodotto degli scarti dalle medie
dati$x_xmedio_y_ymedio<-dati$x_xmedio*dati$y_ymedio

# A questo punto siamo pronti per per calcolare b (coefficiente angolare

numeratore_b<-sum(dati$x_xmedio_y_ymedio)
denominatore_b<-sum(dati$x_xmedio_quadrato)

b<-numeratore_b/denominatore_b
b<-round(b,2)

# Ora possiamo calcolare a (intercetta)
a<-media_agilita-(b*media_altezza)
a<-round(a,2)

# Verifichiamo questi calcoli con la funzione lm() di R.
prima_regressione<-lm(Agilita~Altezza,data=dati)
prima_regressione
# Possiamo estrarre solo i coefficienti
coef(prima_regressione)

# Questi risultati, tengono da un punto di vista statistico?
summary(prima_regressione)
residuals(prima_regressione)
resid(prima_regressione)
## Impariamo a leggere il summary del nostro modello:

## Call:
# Si tratta semplicemente della formula del nostro modello,
# in cui viene ribadito come Agilità siala nostra variabile dipendente
# e Altezza la nostra variabile indipendente/ predittore.

## Residuals:
# Sono i residui del nostro modello, la e che abbiamo visto nella teoria.
# Sono tanti, dal momento che per ogni soggetto esiste una misura di errore.
# La loro media:la nostra e media rispetto all'intera regressione
# R preferisce riportare la mediana, insieme ai valore minimo, massimo
# primo e terzo quartile.
# Sui residui potremo andare a testare i nostri assunti del modello lineare.

## Coefficients:
# In questa tabella sono riportati i parametri a e b, e le statistiche associate:

# Estimate
#  Il primo valore riguarda l'intercetta,
# il secondo, invece, quello del coefficiente angolare.
# In questo caso possiamo quindi dire che il valore di agilita medio quando 
# l'altezza è uguale a zero è circa -70.
# Invece, per ogni aumento di un unità in altezza, avremo un aumento di 0,85
# in agilità

# Std.Error
# Si riferisce all'errore standardi di misura del nostro coefficiente.
# In altre parole, misura quanto, in media, il nostro coefficiente varia
# dalla media attuale della nostra varaibile di risposta.
# In merito all errore standard del nostro predittore, quanto più piccolo ,è
# meglio è. E' una misura utile perchè ci dice di quanto
# varierebbe, al massimo, il nosotro parametro qualora facessimo rigirare
# il modello ancora e ancora.
# Nel nostro caso, qualora facessimo girare il modello 1000 volte,
# il nostro cambiamento in agilità potrebbe essere al massimo di 0.1448.

# t value
# Si tratta della statistica associata ai nostri parametri di regressione.
# Tecnicamente, è la misura di quante deviazioni standard il nostro coefficiente
# è diverso da zero. Perch? è importante?
# Se il nostro coefficiente è uguale a zero, vuol dire che la relazione tra
# Agilità ed altezza sarebbe nulla. Questa affermazione coincide con
# l'ipotesi nulla del modello che stiamo testando:
# Ho: non c'è relazione tra agilità e altezza 
# oppure
# H0 L'altezza non può predire l'agilità
# QUesta ipotesi la vogliamo confrontare con quella alternativa,
# secondo cui
# H1 c'è una relazione tra agilità e altezza 
# oppure
# H1 L'altezza può predire l'agilità.
# Vien da sé che quanto più il nostro t si allontana a zero,
# tanto più possiamo rigettare, con un certo grado di probabilità,
# la nostra ipotesi nulla.
# Da dove lo otteniamo il t? In pratica dividendo il nostro coefficiente
# con il suo valore di errore standard

0.8528/.1448

# Un valore decisamente lontano da 0!!

## Pr(>|t|)
# indica il nostro p value.


# Residual standard error: 
# è la deviazione standard dei residui del nostro modello
sd(prima_regressione$residuals)
# è una misura di qualità del nostro modello. Indica di quanto il nostro
# modello (i punti osservati) deviano dalla regressione di retta vera.
# Si tratta di una misura che quanifica l'errore del nostro modello.
# Ne consegue che abbiamo bisogno di un valore molto piccolo.
# Tramite questo dato, possiamo quantificare l'errore nel nostro modello, dividendolo
# per la media della nostra VD.

sigma(prima_regressione)/mean(dati$Agilita)
# Nel nostro modello, e=0.13
# ossia, abbiamo un 13% di errore.

# Multiple R-squared:
# è conosciuto anche come il coefficiente di determinazione.
# E' un modo per capire quanto il nostro modello sia adeguato per i nostri
# dati. Varia da 0 a 1.
# Esprime quanta proporzione di varianza nei dati sia spiegata dal nostro modello.
# la sua variante Adjusted, è una correzione in base al numero di variabili che ci sono.
0.7891*100
# il nostro modello spiega i nostri dati al 78.91%

# F-statistic:
# Si tratta di un indice che ci dice quanto il nostro modello di regressione,
# in generale, perfrorma meglio del caso.
# La statistica F testa l'ipotesi secondo cui nessun predittore ha una relazione 
# statisticamente significativa con la variabile dipendente.
# Per converso, l'ipotesi alternativa affermerebbe che almeno un predittore ha
# una relazione statisticamente significativa.
# Nel nostro caso, possiamo rigettare l'ipotesi nulla
# (p-value: 0.0003662).

## Verifichiamo che alcune assunzioni della regressione lineare

hist(prima_regressione$residuals)

shapiro.test(prima_regressione$residuals)

qqnorm(prima_regressione$residuals)