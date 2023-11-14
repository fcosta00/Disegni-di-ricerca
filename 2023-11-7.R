# caricamento database
library(MASS)
data <- anorexia
head( data )

str( data )

contrasts( data$Treat) # per vedere la tabella di contrasto

data$Treat <- factor( data$Treat, levels = c('Cont', 'CBT', 'FT'))

mod_cat<-lm( Postwt-Prewt ~ Treat, data=data)

summary(mod_cat)
# rteoricamnete l'intercetta è la media del gruppo di controllo

with(data, tapply(Postwt-Prewt, Treat, mean) )

library(effects)
plot( effect('Treat', mod_cat) )
