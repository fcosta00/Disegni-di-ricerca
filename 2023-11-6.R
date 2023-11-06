
# Lezione scorsa volta ---------

dati<-read.csv(file.choose(),sep = ";",dec=",",header=T)
str(dati)

media_altezza<-mean(dati$Altezza)
media_agilita<-mean(dati$Agilita)
dati$x_xmedio<-dati$Altezza-media_altezza
dati$x_xmedio_quadrato<-dati$x_xmedio^2
dati$y_ymedio<-dati$Agilita-media_agilita
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

# Lezione odierna -------

summary(prima_regressione)
# i residui sono la e, cioè la distanza tra il valore ideale e il valore reale
# gradi di libertà -> grado in cui ha possibilità di variare, grado di variabilità
# residual standard errpr -> circa deviazione standard dei residui. Il circa deriva dall'approssimazione.
# (possiamo vederlo come misura di qualità del modello, più piccolo è meglio è)
# sigma() estrae la deviazione standard dei residui

sigma( prima_regressione ) # l'output del summary (quanto varia l'errore del modello) 
sigma( prima_regressione ) / mean(dati$Agilita) # circa la percentuale di errore del modello, la famosa e del nostro modello di regressione

# R^2 quanto il mio modello riesce a spiegare la variazione dei dati
# ogni tanto lo si legge anche come indice di fit, quanto il nostro modello si adatta ai dati
# la versione Adjusted è corretta per il rumore

# F statistica che ci permettere di creare la distribuzione sulla quale calcolare il p-value
# p-value la probabilità di trovare un F più estremo di questo qual ora h0 fosse vera

# std.error coefficients -> la deviazione standard delle statistiche calcolate sul campione


{
  boxplot( summary(prima_regressione)$residuals )
  hist( summary(prima_regressione)$residuals )
  shapiro.test( summary(prima_regressione)$residuals ) # è normale
  
  library(easystats)
  report(prima_regressione)
} # comandi non fatti a lezione 



