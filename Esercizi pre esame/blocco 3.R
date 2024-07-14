{
  library(MASS)
  datax <- esercizio1
  
  head(datax)
  summary(datax)
  str(datax)
  
  datax$Istruzione <- factor(datax$Istruzione, levels = c( 'Elementare' , 'Media' , 'Laurea' ))
  
  contrasts(datax$Istruzione) <- contr.sdif(3)
  contrasts(datax$Istruzione)
  
  datax$Eta <- scale(datax$Eta, center = T, scale = F)
  
  mod = lm( Performance ~ Istruzione*Eta, data = datax)
  summary( mod )
} #esercizio 1

{
  datax <- esercizio2
  
  str(datax)
  datax$Diversita <- factor(datax$Diversita)
  
  contrasts(datax$Diversita)
  
  mod= lm( Efficacia ~ Diversita, datax)
  summary(mod)
  
  plot( effects::effect('Diversita', mod))
  
} # esercizio 2

{
  datax <- esercizio3
  
  str(datax)
  
  mod = lm( Benessere_Psicologico ~ Durata_Sonno + Stress, datax)
  summary(mod)
  plot( effects::effect('Stress', mod) )
  plot( effects::effect('Durata_Sonno', mod) )
  
} # esercizio 3

{
  datax <- esercizio4
  
  str(datax)
  summary(datax)
  
  datax$Terapia <- factor( datax$Terapia)
  datax$Genere <- factor( datax$Genere)
  
  mod = lm( Ansia ~ Terapia*Genere, datax)
  summary(mod)
  
  anova(mod)
  
  contramodcontrasts()
  
  
  library(easystats)
  report(mod)
  
} # esercizio 4

{
  datax <- esercizio5
  
  str(datax)
  datax$Pratica <- factor(datax$Pratica, levels = c("Bassa ", "Alta " ))
  
  datax$Eta <- scale(datax$Eta, scale = F, center = T)
  contrasts(datax$Pratica) <- contr.sum(2)
  
  mod = lm( ML ~ Eta*Pratica, datax )
  summary(mod)
  
  plot(effects::effect("Pratica",mod),multiline=T)
  
  plot(effects::effect("Eta*Pratica",mod),multiline=T)
  
  
} # esercizio 5

{
  datax <- esercizio6
  
  str(datax)
  datax$Relazione_Sociale <- factor(datax$Relazione_Sociale)
  datax$Genere<- factor(datax$Genere)
  
  mod0 = lm( Benessere_Psicologico ~ Genere, datax )
  mod1 = lm( Benessere_Psicologico ~ Supporto_Sociale, datax )
  mod2 = lm( Benessere_Psicologico ~ Relazione_Sociale, datax )
  mod3 = lm( Benessere_Psicologico ~ Genere + Supporto_Sociale, datax )
  mod4 = lm( Benessere_Psicologico ~ Genere + Relazione_Sociale, datax )
  mod5 = lm( Benessere_Psicologico ~ Supporto_Sociale + Relazione_Sociale, datax )
  mod6 = lm( Benessere_Psicologico ~ Genere + Supporto_Sociale + Relazione_Sociale, datax )
  
  anova(mod0, mod1, mod2, mod3, mod4, mod5, mod6)
  
  library(stats)
  AIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6)
  BIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6)
  
  
} # esercizio 6

{
  datax <- esercizio7
  
  str(datax)
  summary(datax)
  
  datax$Intervention_Type <- factor( datax$Intervention_Type, levels = c( 'CG', 'CT', 'CST', 'MRT') )
  
  mod = lm( Memory_Capacity ~ Intervention_Type, datax  )
  summary(mod)
  
  plot( effects::effect( 'Intervention_Type', mod))
  
} # esercizio 7

{
  library(stats)
  datax <- esercizio8
  
  str(datax)
  summary(datax)
  
  datax$Leadership <- factor( datax$Leadership, levels = c( ' Trasformazionale', ' Democratica', ' Autocratica') )
  
  contrasts(datax$Leadership) <- contr.helmert(3) 
  contrasts(datax$Leadership) 
  
  mod = lm( Soddisfazione ~ Leadership, datax)
  summary(mod)
  
  plot( effects::effect( 'Leadership', mod ))
  
} # esercizio 8

{
  datax <- esercizio9
  
  str(datax)
  summary(datax)
  
  datax$Coaching <- factor(datax$Coaching)
  
  levels(datax$Coaching)
  datax$Coaching <- factor(datax$Coaching, levels = c( 'Nessuna', 'Motivazionale', 'Diretto', 'Supporto'))
  
  mod = lm( Performance ~ Coaching, datax)
  summary(mod)
  
  plot( effects::effect('Coaching', mod))
  
  
} # esercizio 9

{
  datax <- esercizio10
  
  str(datax)
  summary(datax)
  
  mod = lm( Performance ~ All_men, datax )
  summary( mod )
  
  plot( effects::effect( 'All_men', mod))
}



