# es 1

datax <- alpha2

str(datax)

library(RcmdrMisc)

reliability(cov(datax))
#inverto item con alpha negativo

#inverto item_12
datax$item_12inv <- 4- datax$item_12
reliability(cov(datax[, -c(12)]))

#inverto item_7
datax$item_7inv <- 4- datax$item_7
reliability(cov(datax[, -c(12, 7)]))

#il valore totale di alpha è buono ma non eccellente (0.827)
# elimino tutti gli item sotto il valore sogli di <0.1 per amunetare la coerenza della scala e vedere se sale il valore di alpha

#elimino item_7inv
reliability(cov(datax[, -c(12, 7, 16)]))

#elimino item_6
reliability(cov(datax[, -c(12, 7, 16, 6)]))


#il valore di alpha è ottimo >0.85. 


#es 2

library(irr)

kappa2(kappa2)

#valore di k accettabile, non ottimo. probabilmente la definizione di compliance non è stata ottima.
# possiamo comunque dire che esiste un allineamento dei risultati dato il valore (0.38) reliability(cov(datax[, -c(12, 7, 16)]))
# e il pv value negativo che ci ci permette di rifiutare l'ipotesi che laccordo sia causuale



# es 3

library(MASS)

datax <- sim2

str(sim2)

datax$Gruppo <- factor( datax$Gruppo, levels = c('Adolescenti', 'Giovani_Adulti', 'Adulti', 'Anziani'))

contrasts(datax$Gruppo) <- contr.sdif(4)
contrasts(datax$Gruppo)

mod = lm( Punteggio ~ Gruppo, datax )

summary(mod)
plot( effects::effect('Gruppo', mod))

datax$PC <- factor( datax$PC)
contrasts(datax$PC) <- contr.sum(2)

mod = lm( Punteggio ~ Gruppo*PC , datax )
summary(mod)
plot( effects::effect('Gruppo:PC', mod), multiline = T)
plot( effects::effect('PC', mod), multiline = T)


