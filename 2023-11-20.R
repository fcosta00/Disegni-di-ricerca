# esercizio sui contrasti sum

library( MASS )

anorexia$Treat <- factor(anorexia$Treat)

contrasts(anorexia$Treat)
anorexia$Treat <- factor(anorexia$Treat, levels= c( 'CBT', 'FT','Cont'))

contrasts(anorexia$Treat) <- contr.sum(3)

mod = lm( Postwt ~ Treat, data= anorexia)

summary(mod)

library(effects)
plot( effect('Treat', mod) )

mean(   mean( anorexia[ anorexia$Treat == 'CBT', ]$Postwt), mean( anorexia[ anorexia$Treat == 'FT', ]$Postwt), mean( anorexia[ anorexia$Treat == 'Cont', ]$Postwt)   )

contrasts(anorexia$Treat)


# esercizio sui sliding contrast -----

datax <- esercizio_pp

datax$Esercizio <- factor(datax$Esercizio , levels = c('Basso', 'Medio', 'Alto') )

contrasts(datax$Esercizio) <- contr.sdif(3)
contrasts(datax$Esercizio)

str(datax)

mod = lm( Perdita_peso ~ Esercizio, data= datax )

summary(mod)

plot( effect('Esercizio', mod) )

mean(  datax$Perdita_peso   )



# esercizio sui polinomiali -----

datax <- esercizio_pp

datax$Esercizio <- factor(datax$Esercizio , levels = c('Basso', 'Medio', 'Alto') )

contrasts(datax$Esercizio) <- contr.poly(3)
contrasts(datax$Esercizio)

str(datax)

mod = lm( Perdita_peso ~ Esercizio, data= datax )

summary(mod)

plot( effect('Esercizio', mod) )

mean(   datax$Perdita_peso   )



#verificare ortogonalità

cor( contr.poly(3))



# esercizio sui helmet Invertiti -----

library(MASS)

datax <- anorexia
str(datax)

datax$Treat <- factor(datax$Treat, levels= c( 'CBT', 'FT', 'Cont'))
contrasts(datax$Treat) <- contr.helmert(3) 

mod = lm( Postwt ~ Treat, data= datax )

mod = lm( Postwt - Prewt ~ Treat, data= datax )

summary(mod)

plot( effect('Treat', mod) )



# esercizio sui helmet Invertiti -----

library(MASS)

datax <- anorexia
str(datax)

datax$Treat <- factor(datax$Treat, levels= c( 'Cont', 'CBT', 'FT' ))
contrasts(datax$Treat) 

# modo complicato per creare matrice contrasti
H <- matrix( c(1/3, -1/6, -1/6,
               0, 1/2, -1/2), 2, 3,byrow = T)
mat = round( ginv(H), 2)

#modo easy
mat <- matrix( c(2, -1, -1,
                 0,  1, -1), 3, 2)

contrasts(datax$Treat) <- mat




mod = lm( Postwt ~ Treat, data= datax )

summary(mod)
# interpretazione: 

mean( datax[ datax$Treat == 'Cont', ]$Postwt) 

mean(   mean( datax[ datax$Treat == 'CBT', ]$Postwt), mean( datax[ datax$Treat == 'FT', ]$Postwt)   )
mean(  c( datax[ datax$Treat == 'CBT', ]$Postwt, datax[ datax$Treat == 'FT', ]$Postwt)   )


plot( effect('Treat', mod) )








datax <- esercizio_pp
str(datax)

datax$Esercizio <- factor(datax$Esercizio, levels= c('Basso', 'Medio', 'Alto'))
contrasts(datax$Esercizio) <- contr.helmert(3) 

mod = lm( Perdita_peso ~ Esercizio, data= datax )

summary(mod)

plot( effect('Esercizio', mod) )




# esercizio per presentazione
dieta <- read.csv("C:/Users/franc/Desktop/dieta_estesa.csv")

datax <- dieta
str(datax)

datax$dieta <- factor(datax$dieta, levels= c('vegana', 'ecologica', 'vegetariana', 'mediterranea'))

datax[ datax$dieta == 'vegetariana', ]$co2 <- datax[ datax$dieta == 'vegetariana', ]$co2 - 2

contrasts(datax$dieta)

#modo easy
mat <- matrix( c(1, -1, 0, 0,
                 0, 0, 1, -1,
                 1/2, 1/2, -1/2, -1/2), 4, 3)


#modo easy
H <- matrix( c(1/2, 1/2, -1/2, -1/2,
               1, -1, 0, 0,
               0, 0, 1, -1), 3, 4, byrow = T)
mat = round( ginv(H), 2)


contrasts(datax$dieta) <- mat

mod = lm( co2 ~ dieta, data= datax )

summary(mod)

plot( effects::effect('dieta', mod) )

mean <- (21 + 24.5) / 2
std <- (24.5 - 21) / 4

# Genera un numero maggiore di dati
round( rnorm(10, mean, std), 1 )
