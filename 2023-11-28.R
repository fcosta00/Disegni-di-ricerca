str(Regressioni)

datax <- Regressioni

mod_0 <- lm( Paura_Covid19 ~ Impulsivita + Genere, datax )
summary(mod_0)

plot( effects::effect('Genere', mod_0) )

datax$Genere <- factor( datax$Genere )
contrasts(datax$Genere) <- contr.sum(2)

mod_1 <- lm( Paura_Covid19 ~ Impulsivita + Genere, datax )
summary(mod_1, fit.measures = TRUE, standardized=TRUE )



mod_1 <- lm( Paura_Covid19 ~ Impulsivita + Genere + Impulsivita:Genere, datax )
summary(mod_1)

plot( effects::effect('Impulsivita:Genere', mod_1) )
plot( effects::effect('Impulsivita:Genere', mod_1), multiline = T )


# analisi adattamento modello in base all'anova

m1 <- lm( Paura_Covid19 ~ 1, datax )
m2 <- lm( Paura_Covid19 ~ Impulsivita, datax )
m3 <- lm( Paura_Covid19 ~ Genere, datax )
m4 <- lm( Paura_Covid19 ~ Impulsivita + Genere, datax )
m5 <- lm( Paura_Covid19 ~ Impulsivita + Genere + Impulsivita:Genere, datax )

anova(m1, m2, m3, m4, m5) # se il modello ha lo stesso numero di variabili non li considera confrontabili, per questo ci sono gli indici
# se il modello è sstatisticamente significativo vuol dire che è migliore rispetto il primo


AIC(m1, m2, m3, m4, m5) # AIC indica quantità di errore nel modello
# più piccolo è il numero meglio è

BIC(m1, m2, m3, m4, m5)
