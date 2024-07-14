
#ES 1 ----

# Il metodo dello split-half è un metodo utilizzato per verificare l'affidabilità interna e la coerenza interna del dataset di riferimento.
# Questo consiste nel dividere in due gli item del test e vedere quando i due risultati sono correlati.
# Attualmente il metodo dello split half viene poco utilizzato e si è preferito utilizzare alternative come l'alpha di chronbach,
# in quanto, esegue multiple volte questo tipo di operazione andanda a stimare il risultato finale di affidabilità in modo più stabile e preciso.
# Con il metodo dello split half, a prescindere dalla metodologia in cui si dividono gli item, è possibile incappare in stime imprecise e impreviste,
# data magari, per esempio, l'inclusione di due item simili in entrambe le divisioni del daaset che però non rispecchiano bene il costrutto finale. 
# Una casistica come questa produrrebbe probabilmente un buon valore di splithalf anche se probabilmente il valore realistico di affidabilità non lo è.
# In generale possiamo comunque dire che, a prescindere dal metodo di stima, il calcolo dell'affidabilità di coerenza interna del test risulta fondamentale in quanto permette il calcolo di una percentuale di quanto quel test è scevro da errore.
# Nonostante ciò i valori ideali displit half dovrebbero superare il 0.7 ma non avvicinarsi mai perricolosamente all'1 in quanto potrebbe essere indice di item pressochè identici del nel test e di un'analisi del costrutto non adeguatamente ampia.




# ES 2 ----

satisfaction <- read.csv("C:/Users/franc/Desktop/esame 16 gennaiio/satisfaction.csv", sep=";")

datax <- satisfaction[, c(3:13)]
str(datax)

summary(datax)

min<- 0
max<- 4

library(RcmdrMisc)
reliability(cov(datax))

# l'indice di affidabilità calcolato tramite l'alpha di chronbahc risulta estremamente basst,
# (0.11). Prima di poter interpretare questa valore dobbiamo però invertire gli item con a negativo

# Per l'inversione partiamo sempre dall'item con magnitudine maggiore e ne invertiamo uno alla volta

# Invertiamo pre8 (r= -0.55)
datax$pre8inv <- (min + max) - datax$pre8
reliability(cov(datax[, -c(8)]))
# successivamente a questa invesione notiamo già un alfa molto migliorato(0.51)
# rimangono però ancora item negativi

# Invertiamo pre4(r= -0.44)
datax$pre4inv <- (min + max) - datax$pre4
reliability(cov(datax[, -c(8, 4)]))
# nuovamente il valore di alpha è migliorato notevolmente arrivando quasi al valore soglia (0.69)
# rimane però ancora un item negativo

# Invertiamo pre3(r= -0.02)
datax$pre3inv <- (min + max) - datax$pre3
reliability(cov(datax[, -c(8, 4, 3)]))
# l'alpha è salito nuovamente ma in modo marginale (0.7), 
# questo è probabilmente dovuto alla bassa magnitudine di partenza dell'item stesso.
# possiamo per cui dedurre che l'item3 risulti poco coerente con il costrutto rilevato dagli item della stessa scala

# Nonostante la scala abbia raggiunto il valore soglia di 0.7,
# eliminiamo gli item con valore di r < 0.1 per aumentare il valore di alpha e rendere la scala più coerente

reliability(cov(datax[, -c(8, 4, 3, 14)]))
# l'alpha totale ha raggiunto il valore di 0.73. Questo è un valore discreto che possiamo accettare per il nostro test.
# rimane però l'item7 con r <0.1 che andremo ad eliminare

reliability(cov(datax[, -c(8, 4, 3, 14,)]))
# il valore di alpha è cresciuto nuovamente raggiungendo il valore di 0.75.
# questo è un valore discreto che di permette dire che la scala è coerente.
# si vede però anche che ci sono item, come primo e l'ottavo, che hanno una correlazione molto più forte degli altri con la scala.
# possiamo per cui pensare che siano gli item che più rispecchiano il costrutto finale,
# cioè che sono più vicini al costrutto che cerca il questionario, la soddisfazione sul luogo di lavoro



# ES 3 ----

kval <- read.csv("C:/Users/franc/Desktop/esame 16 gennaiio/kval.csv", sep=";")
datax <- kval

str(datax)
summary(datax)

library(irr)
kappa2(datax)

# l'indice di concordanza inter-rater calcolato tramite k di cohen risulta 0.325
# nonotante il p-value significativo ci permetta di rifiutare l'ipotesi che la concordanza sia casuale, il valore dell'indice è basso.
# Questo ci indica come non ci sia una forte concordanza tra i valori del giudice A e del giudice B,
# cioè i due  tirocinanti-osservatori non hanno interpretato allo stesso modo la presenza o meno del comportamente in molte delle loro valutazioni.
# Il risultato potrebbe essere facilmente dovuto in primis dal loro livello di formazione.
# Essendo tirocinanti commetterranno maggiormente errori rispetto a un professionista esperto del settore, e, questi errori, sommandosi,
# han portato a molte discordanze. IN qggiunta a ciò potrebbe essere anche dovuto ad errori a una spiegazione inaccurata, o poco chiara,
# del loro supervisore che ha provocato una differente interpretazione tra i due giudici.



# ES 4 ----

income <- read.csv2("C:/Users/franc/Desktop/esame 16 gennaiio/income.csv")
datax <- income

str(datax)
summary(datax)

mod <- lm( qol ~ income, datax)
summary(mod)

plot(effects::effect('income', mod))

# Il modello in primis non risulta significatico, i predittori non sono adatti a stimare la y,
# cioè il reddito non va ad essere impattante per la qualità di vita,
# o per lo meno nel nostro modello e con il nostro dataset non risulta un buon indice per la previsione della qualità di vita.
# Ad avvalorare ciò vediamo anche l'r^2, cioè la varianza spiegata dal modello che risulta 0.005, quasi nulla. 
# Questo significa che il reddito non spiega il cambiamento dei dati sulla qualità di vita
# Guardando poi i residui vediamo come siano abbastanza centrati, questo potrebbe indicare una possibile normalità di essi.
# Ciò è però da valutare con test specifici e/o con l'osservazione di grafici specifici come il qqplot
# Spostandoci poi ai coefficienti guardiamo l'intercetta.
# il pvalue associato risulta significativo, cioè ponendo il valore di income a 0, possiamo rifiutare l'ipotesi che sia uguale a zero.
# Come vediamo infatti il valore del coefficiente è 53.8, questo significa che con reddito a zero, secondo il nostro modello,
# il risultato del test sulla qualità di vita  è di circa 54.
# Guardando income vediamo innanzitutto che non è significativo (pvalue 0.16).
# Questo significa che non probabilmente non impatta sul valore di qol.
# Cioè il reddito non ci spiega il cambiaento del valore della qualità di vita.
# Questa ipotesi è avvalorata dal valore stimata del coefficiente ( -0.003) che risulta pressochè nullo.
# Ciò significa che per ogni punto in più sul valore di qol il reddito varia di circa -0.003.
# Come Interpretazione generape possiamo dire che il rreddito percepito da una persona non vada a influenzare la qualià di vita della stessa, nè in positivo nè in negativo.
# Essendo che il qol considera aspetti anche psicologici sarebbe forse più opportuno utilizzare altri predittori che hanno più valenza per questo aspetto.
# Probabilmente, visto che la qualità di vita a livello economico è crescita negli ultimi decenni,
# ora altri aspetti diventano più importanti. Degli esempi possono essere il numero di relazioni tra pari, la stabilità di queste, la soddifazione nelle relazioni interpersonali, l'ambito di lavoro e molti altri



# ES 5 ----

library(MASS)

auto <- read.csv("C:/Users/franc/Desktop/esame 16 gennaiio/auto.csv", sep=";")
datax <- auto

str(datax)
summary(datax)

# ordino per livello di autonomia visto che è una variabile categoriale ordinale
datax$Autonomia <- factor(datax$Autonomia, levels = c('Basso', 'Medio', 'Alto'))

# decido di utilizzare i contrasti sliding difference perchè voglio confrontare il livello con il precente
# in modo vi vedere come cambia il percepito con il crescendo di autonomia del veicolo.
# questo mi permette di capire se c'è un effetto tetto per il livello di autonomia e, 
# in caso di ampliamento della ricerca con ulteriori livelli, di inserirli confrontandoli con l'autonomia massima precedente
contrasts(datax$Autonomia) <- contr.sdif(3)

mod <- lm( Gravita_Decisioni ~ Velocita*Autonomia, datax)
summary(mod)

plot(effects::effect('Velocita:Autonomia', mod), multiline = T)
plot(effects::effect('Velocita', mod))
plot(effects::effect('Autonomia', mod))

# In primis il modello risulta significativo, ciò vuol dire che in generale, 
# i predittori scelti sembrano essere adatti alla stima della variabile dipendente
# L' r^2 presenta un ottimo valore (0.79). Questo mostra come, avalorando la tesi precendente,
# il modello riesce a spiegare bene l'oscillazione di Gravita_decisioni. Questo significa che,
# Vecita e autonomia sono buoni indicatori per capire la gravita percepita sulle decisioni.
# I residui sembrano un po' sbilanciati, questo ci porta a pensare che la loro curba abbia una skew positiva,
# sarebbe per cui necessario verificare la normalità di essi tramite un test specifico e/o l'osservazione di grafici adatti
# #guardando i coefficienti vediamo come:
# Intercetta - non risulta significativa, per cui, ponendo le variabili indipendenti a 0,
# non possiamo rifiutare l'ipotesi nulla che sia uguale a zero. 
# Velocità - questo coefficiente guarda l'impatto di velocità sul valore di Gravità decisioni.
# Questo è significativo, per cui possiamo rifiutare l'ipotesi nulla che la velocità non sia influente sul valore di gravita
# A livello pratico, dato il valore el coefficiente di 0.1 possiamo, per ogni valore km/h in più il punteggio di gravità sale di circa 0.1.
# Coè, possiamo dire che la velocità sia impattante sul livello di gravità, più si va veloci più la gravità delle decisioni sale
# Autonomia 2-1 - questo coefficiente guarda la differenza tra i valori di medio-basso.
# Questo non è significativo, per cui non possiamo rifutare l'ipotesi nulla che non ci sia differenza tra i due livelli di autonomia per il valore di gravita.
# A livello pratico, dato il valore del coefficiente di 1.63, ponendo il valore di velocità a 0, il valore di gravità saraà più alto.
# non essendo però significativo non possiamo assumere che si sia una vera differenza tra i due valori
# Stesse conclusioni possiamo anche trarre dai coefficienti di Autonomia 3-2 che vanno a confrontare la differenza tra Alto e medio
# Velocita:Autonomia2-1 - Questo va a guardare la differenza dell'impattato della velocità tra le categorie Medio e Basso.
# il pvalue non significativo ci mostra come non ci sia una differenza sostanziale. 
# seppur il coefficiente sia leggermente negativo -0.55 non possiamo assumere che un aumento di autonomia faccia percepire meno la gravità con laumento di velocità
# Velocita:autonomia3-2 - uguali conclusioni possiamo dire del precedente. Anche qui il pvalue  non è significativo per cui non possiamo dire ci sia una differenza sostanziale.
# Qui il coefficiente è comunque praticamente nullo per cui non esiste in alcun modo una differenza di impatto della velocità salendo di autonomia
# Questi risultati potrebbero essere dovuti in primis dal forte senso di responsabilità personale. 
# Seppur con una macchina molto autonoma le persone si sento molto responsabili di possibili situazioni gravose dato che si considerano comunque conduttori dell'auto.
# perobabilmente l'autonomia dell'auto non è ancora un concetto integrato visto che esiste sempre la possibilità di intervento e, per cui, la possibilità personale di intervento.
# Sicuramente questo modello da anche un ultieriore conferma sul sentirsi maggiormente responsabili se si tiene una guida veloce


mod0 <- lm( Gravita_Decisioni ~ 1, datax)
mod1 <- lm( Gravita_Decisioni ~ Velocita, datax)
mod2 <- lm( Gravita_Decisioni ~ Autonomia, datax)
mod3 <- lm( Gravita_Decisioni ~ Velocita+Autonomia, datax)
mod4 <- lm( Gravita_Decisioni ~ Velocita + Autonomia + Velocita*Autonomia, datax)

AIC(mod0, mod1, mod2, mod3, mod4)

# Guardando il confronto tra modelli tramite l'indice AIC, vediamo come il migliore, per cui quello con il valore più basso sia il mod3 (241)
# questo modello, per cui quello senza interazione, è presumibilmente il mogliore visto che non c'è differenza tra il quanto impatta la velocità per i diversi gradi di autonimia del veicolo.
# viene invece mantenuto il valore di autonomia probabilmente perchè, seppur prima non risultassero significative le differenze,
# esiste una differenza di base tra i valori di gravità in base al grado di autonomia, che però non è correlato con la velocità
# questo potrebbe suggerirci l'idea di ricercare maggiormente questa differenza di base, magari con un campione più ampio o con altri tipi di contrasti
