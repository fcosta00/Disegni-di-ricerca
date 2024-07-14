library(MASS)

datax <- anorexia

str(datax)

datax$Treat <- as.factor( datax$Treat )

contrasts(  datax$Treat )

datax$Treat <- factor( datax$Treat, levels = c('Cont', 'CBT', 'FT'))
# cambiamo l'ordine dei livelli dei fattori

contrasts(  datax$Treat )

mod_cat<-lm( Prewt ~ Treat, data=datax)
summary( mod_cat )

library(easystats) 
report(mod_cat)

mean( datax[ datax$Treat == 'CBT', ]$Prewt)

mod_cat

with( datax, tapply(Prewt, Treat, mean) )

library(effects)
plot( effect('Treat', mod_cat) )

plot(mod_cat)

