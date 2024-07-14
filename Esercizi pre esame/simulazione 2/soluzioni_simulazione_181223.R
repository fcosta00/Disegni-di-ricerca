library(RcmdrMisc)

# Importo i dati
mydf=read.table(file.choose(),sep=";",header = T)

# Calcolo lìalpha per la prima volta
reliability(cov(mydf))

# Alpha reliability =  0.7571 
# Standardized alpha =  0.7468 
# 
# Reliability deleting each item in turn:
#          Alpha Std.Alpha r(item, total)
# item_1  0.7283    0.7125         0.5325
# item_2  0.7315    0.7213         0.4720
# item_3  0.7165    0.7042         0.6099
# item_4  0.7080    0.6944         0.7021
# item_5  0.7130    0.6974         0.6612
# item_6  0.7785    0.7681        -0.0072
# item_7  0.7821    0.7700        -0.0218
# item_8  0.7050    0.6923         0.7354
# item_9  0.7310    0.7187         0.5035
# item_10 0.7787    0.7665         0.0211
# item_11 0.7238    0.7062         0.6042
# item_12 0.7985    0.8064        -0.4267
# item_13 0.7354    0.7250         0.4409
# item_14 0.7355    0.7239         0.4388

# Si può osservare come lo strumento abbia un affidabilità di aprtenza adeguata (alpha stand=0.75), dal momento che il valore di alpha è superiore alla soglia di 0.7.
# Tuttavia, si nota anche che alcuni item sono correlati negativamente con il resto della scala. In particolare, l'item 12 presenta
# la correlazione negativa maggiore. Inverto dunque tale item, dopo aver compreso quali siano i valori minimi e massimi per ogni item.
min(mydf) # 0
max(mydf) # 4

# Inverto l'item 12
mydf$item_12_inv=4-mydf$item_12

# Ricalcolo l'alpha.
reliability(cov(mydf[,c(1:11,13:15)]))

# Alpha reliability =  0.8088 
# Standardized alpha =  0.8171 
# 
# Reliability deleting each item in turn:
#              Alpha Std.Alpha r(item, total)
# item_1      0.7917    0.8006         0.5102
# item_2      0.7930    0.8033         0.4804
# item_3      0.7852    0.7963         0.5729
# item_4      0.7776    0.7889         0.6741
# item_5      0.7805    0.7901         0.6424
# item_6      0.8268    0.8342         0.0185
# item_7      0.8344    0.8410        -0.0605
# item_8      0.7674    0.7774         0.8022
# item_9      0.7869    0.7950         0.5761
# item_10     0.8225    0.8274         0.1098
# item_11     0.7846    0.7912         0.6299
# item_13     0.7930    0.8028         0.4805
# item_14     0.8000    0.8097         0.3992
# item_12_inv 0.7985    0.8064         0.4267

# Il valore di alpha standardizzato è incrementato (0.82). Vi è ancora un item con correlazione negativa (item 7).
# Procedo dunque ad invertirlo

mydf$item_7_inv=4-mydf$item_7


reliability(cov(mydf[,c(1:6,8:11,13:16)]))

# Alpha reliability =  0.8176 
# Standardized alpha =  0.8265 
# 
# Reliability deleting each item in turn:
#              Alpha Std.Alpha r(item, total)
# item_1      0.8094    0.8192         0.3970
# item_2      0.8110    0.8200         0.3911
# item_3      0.8037    0.8141         0.4763
# item_4      0.7896    0.8016         0.6630
# item_5      0.7951    0.8053         0.5939
# item_6      0.8275    0.8364         0.1380
# item_8      0.7793    0.7902         0.7998
# item_9      0.7949    0.8040         0.6142
# item_10     0.8176    0.8257         0.2964
# item_11     0.7926    0.8001         0.6719
# item_13     0.8015    0.8124         0.5045
# item_14     0.8165    0.8254         0.3186
# item_12_inv 0.8054    0.8136         0.4811
# item_7_inv  0.8344    0.8410         0.0605

# Il valore di alpha è nuovamente incrementato (0.83), seppur di poco rispetto alla analisi precedente.
# Si può notare che a questo punto le correlazioni item-totale sono tutte positive. Un item, tuttavia, presenta una correlazione inferiore a 
# a 0.10 (item 7 inv). Ricalcolo dunque il valore di alpha dopo averlo rimosso.

reliability(cov(mydf[,c(1:6,8:11,13:15)]))

# Alpha reliability =  0.8344 
# Standardized alpha =  0.841 
# 
# Reliability deleting each item in turn:
#              Alpha Std.Alpha r(item, total)
# item_1      0.8246    0.8320         0.4577
# item_2      0.8267    0.8336         0.4400
# item_3      0.8192    0.8275         0.5299
# item_4      0.8084    0.8176         0.6765
# item_5      0.8124    0.8201         0.6251
# item_6      0.8501    0.8562         0.0791
# item_8      0.7984    0.8066         0.8110
# item_9      0.8151    0.8217         0.6022
# item_10     0.8436    0.8478         0.2048
# item_11     0.8127    0.8180         0.6585
# item_13     0.8217    0.8296         0.4983
# item_14     0.8326    0.8393         0.3623
# item_12_inv 0.8253    0.8319         0.4591

# Il valore di alpha è nuovamente incrementato (0.84), seppur di poco rispetto alla analisi precedente.
# Vi è ancora Un item che presenta una correlazione inferiore a 
# a 0.10 (item 6). Ricalcolo dunque il valore di alpha dopo averlo rimosso.

reliability(cov(mydf[,c(1:5,8:11,13:15)]))

# Alpha reliability =  0.8501 
# Standardized alpha =  0.8562 
# 
# Reliability deleting each item in turn:
#              Alpha Std.Alpha r(item, total)
# item_1      0.8418    0.8487         0.4738
# item_2      0.8421    0.8485         0.4843
# item_3      0.8368    0.8442         0.5456
# item_4      0.8270    0.8355         0.6782
# item_5      0.8298    0.8367         0.6430
# item_8      0.8175    0.8248         0.8076
# item_9      0.8341    0.8403         0.5921
# item_10     0.8638    0.8670         0.1781
# item_11     0.8317    0.8369         0.6459
# item_13     0.8426    0.8492         0.4709
# item_14     0.8486    0.8548         0.3975
# item_12_inv 0.8437    0.8502         0.4504

# Il valore finale di alpha, dopo due eliminazioni, è di 0.86 (standardizzato), risultato che suggerisce per avere un buon livello di
# affidabilità per indagare l'ansia di stato si possono usare 12 item , previa inversione di due di essi.

####### ESERCIZIO KAPPA
library(irr)

ka=read.table(file.choose(),sep=";",header = T)

kappa2(cbind(ka$Opa,ka$Opb))$value # 0.3820225

## Dopo aver calcolato il kappa di Cohen con la funzione kappa2 del pacchetto irr, è possibile notare come
## tale coefficiente si possa definire scarso DUnque è possibile affermare che nella valuytazione i due giudici,
## potrebbero valutato la tendenza a commettere falli in maniera non indipendente. 
## É anche verosimile supporre che gli istruttori non abbiano spiegato in maniera sufficientemente chiara come valutare una buona compliance.
## Inoltre, non si può assumere che sia stato correttamente compreso il modo con cui valutare la compliance.


####### REGRESSIONE 1
# Importo i dati
dati=read.table(file.choose(),sep=";",dec=",",header = T)

# Per svolgere questo esercizio, occorre usare i contrasti sliding difference, dop aver cambiato l'ordine della variabile Gruppo.
dati$Gruppo=factor(dati$Gruppo,levels = c("Adolescenti","Giovani_Adulti","Adulti","Anziani"))
contrasts(dati$Gruppo)=MASS::contr.sdif(4)
contrasts(dati$Gruppo)

#                  2-1  3-2   4-3
# Adolescenti    -0.75 -0.5 -0.25
# Giovani_Adulti  0.25 -0.5 -0.25
# Adulti          0.25  0.5 -0.25
# Anziani         0.25  0.5  0.75 


modello=lm(Punteggio~Gruppo,dati)
summary(modello)

# Call:
#   lm(formula = Punteggio ~ Gruppo, data = dati)
# 
# Residuals:
#    Min     1Q Median     3Q    Max 
# -5.150 -2.400 -0.150  2.375  5.850 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  74.3125     0.3309  224.61  < 2e-16 ***
#   Gruppo2-1    15.2500     0.9358   16.30  < 2e-16 ***
#   Gruppo3-2    -9.7000     0.9358  -10.37 3.39e-16 ***
#   Gruppo4-3   -17.7000     0.9358  -18.91  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.959 on 76 degrees of freedom
# Multiple R-squared:  0.9218,	Adjusted R-squared:  0.9187 
# F-statistic: 298.6 on 3 and 76 DF,  p-value: < 2.2e-16

plot(effect("Gruppo",modello))

## Dalle analisi emerge come il modello, in generale, sia statisticamente significativo (F(3,76)=298.6,p<.001).
## Dunque è possibile affermare che esiste una relazione tra il gruppo di età ed il livello abilità attentive.
## Inoltre, il modello (inclusi il predittore ed i confronti sceltti) permette di spiegare il 92% della varianza. Tale valore
## è molto alto, nonostante il predittore sia solo uno.
## Esaminando i singoli coefficienti, possiamo osservare come la media di abilità attentive nel gruppo di
## giovani adulti sia maggiore di quella degli adolescenti (beta= 15.25, t=16.30, p < .001). Al contrario,  gli adulti hanno riportato,
## in media, punteggi inferiori rispetto ai giovani adulti (beta= -9.7, t=-10.37, p < .001) Infine, gli anziani hanno riportato,
## in media, punteggi inferiori rispetto agli adulti (beta= -17.7, t=-18.91, p < .001). Pertanto, si può dunque rigettare l'ipotesi nulla
## secondo cui non vi sono differenze in termini di abilità attentive tra le singole coppie di gruppo confrontati. Infatti, ogni confronto
## è emerso come statisticamente significativo.

## Interpretazione. Le diferenze riscontrate sono spiegabili sia da un punto di vista psicobiologico che cognitivo.
## In giovane età adulta, il cervello è nella sua forma più performante, quindi è verosimile che funzioni in modo più efficace ed efficiente.
## Con l'avanzare dell'età invece si sa che le funzioni cognitive calino naturalmente. Inoltre, con un'età crescente, crescono anche 
## le fonti su cui dividere l'attenzione, portanto ad un affaticamente cognitivo generale maggiore per adulti e anziani.
## Sarebbe inetressante testare se esistono differenze in termni di attenzione selettiva o sostenuta.


####### REGRESSIONE 2

dati$PC=factor(dati$PC)
contrasts(dati$PC)=contr.sum(2)
contrasts(dati$PC)

#    [,1]
# NO    1
# SI   -1

modello_int=lm(Punteggio~Gruppo*PC,dati)
summary(modello_int)

plot(effect("Gruppo",modello_int))
plot(effect("PC",modello_int))


# Call:
#   lm(formula = Punteggio ~ Gruppo * PC, data = dati)
# 
# Residuals:
#    Min     1Q Median     3Q    Max 
# -4.900 -2.425 -0.350  2.500  6.100 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    74.3125     0.3381 219.817  < 2e-16 ***
#   Gruppo2-1      15.2500     0.9562  15.949  < 2e-16 ***
#   Gruppo3-2      -9.7000     0.9562 -10.144 1.61e-15 ***
#   Gruppo4-3     -17.7000     0.9562 -18.511  < 2e-16 ***
#   PC1            -0.0375     0.3381  -0.111    0.912    
#   Gruppo2-1:PC1  -0.3500     0.9562  -0.366    0.715    
#   Gruppo3-2:PC1   0.3000     0.9562   0.314    0.755    
#   Gruppo4-3:PC1  -0.7000     0.9562  -0.732    0.467    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.024 on 72 degrees of freedom
# Multiple R-squared:  0.9227,	Adjusted R-squared:  0.9151 
# F-statistic: 122.7 on 7 and 72 DF,  p-value: < 2.2e-16

## Risultati. 
## Complessivamente, è possibile rigettare l'ipotesi nulla secondo la quale 
## non esiste alcuna relazione tra le abilità attentive e almeno uno dei predittori, 
## inclusa la loro interazione (F(7,72) = 127.7, p < .001). 
## Il modello spiega il 91% della varianza (considerando il coefficiente R2 aggiustato), suggerendo un ottimo adattamento ai dati.
## Considerando gli effetti singoli, le differenze e le significatività riscontrate nell'analisi precende permangono.
## D'altra parte, non si osservano differenze statisticamente significative tra coloro che svolgono i cruciverba e coloro che non
## li svolgono (beta = -0.037, t= -0.11, p = .91). Tale mancanza di differenza si osserva anche considerando l'effetto di interazione,
## in cui nessuna differenza delle differenze è statisticamente significativa.
## Tale risultato è corroborato anche osservando il grafico, in cui si evidenza il parallelismo tra i due gruppi.

plot(effect("Gruppo*PC",modello_int),multiline=T)


m1=lm(Punteggio~1,dati)
m2=lm(Punteggio~Gruppo,dati)
m3=lm(Punteggio~PC,dati)
m4=lm(Punteggio~Gruppo+PC,dati)
m5=modello_int

AIC(m1,m2,m3,m4,m5)

#    df      AIC
# m1  2 604.3990
# m2  5 406.5171 !!!!
# m3  3 606.3979
# m4  6 408.5036
# m5  9 413.6409

## Dal confronto tra modelli emerge che quello con minore errore è il secondo, che coincide con il modello dell'esercizio precedente.
## Il fatto che l'aggiunta della variabile PC non abbi a portato ne ad un miglioramento del modello ne ad effetti singoli e/o di interazione
## statisticamente significativi si può spiegare consierando la natura della variabile aggiuntiva. Svlgere crciverba implica avera capacità
## cognitive come l'intelligenza cristallizzata (considerando le parole crociate), non quelle attentive. Si dovrebbe prendere in considerazione
## la paossibilità di inserire variabili maggiormmente correlata con l'attenzione.