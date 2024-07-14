# es 1

alpha1 <- read.csv("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi pre esame/Simulazione 1/alpha1.csv", sep=";")

datax <- alpha1

library(RcmdrMisc)
reliability( cov(datax))

#inverto gli item con segno negativo partendo da quello con maggior magnitudine

datax$item_7inv <- 4- datax$item_7
reliability(cov(datax[, -c(7)]))

datax$item_2inv <- 4- datax$item_2
reliability(cov(datax[, -c(7,2)]))

#il valore di alpha è già buono ma procedo comunque ad eliminare gli item con a<0.2 per renderla più coerente e nella
reliability(cov(datax[, -c(7, 2, 15)]))

#il valore di alpha è molto buono 0.84, questo significa che tutti gli item rispecchiano bene il costrutto e che la scala ha una buona affidabilità interna


# es 2

kappa1 <- read.csv("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi pre esame/Simulazione 1/kappa1.csv", sep=";")

datax <- kappa1

library(irr)
kappa2(datax)
# il grado di accordo è eccellente (0.75), questo significa ce i due arbitri hanno dato quasi sempre la stessa valutazione


# es 3

regressioni1 <- read.csv2("C:/Users/franc/OneDrive - Università degli Studi di Padova/Università-PC-senzaMilza/Magistrale/Disegni di ricerca/Esercizi pre esame/Simulazione 1/regressioni1.csv")

datax <- regressioni1

str(datax)
summary(datax)

datax$Corso <- factor(datax$Corso)
datax$Sport <- factor(datax$Sport)

mod = lm(Punteggio ~ Sport, datax)
summary(mod)
plot( effects::effect('Sport', mod))

# innanzitutto il modello risulta significativo, per cui i predittori spiegano almeno in parte la nostra y, ma r^2 totale è di solo 0.22.
# questo significa che il modello spiega solo il 0.2 della varianza totale, per cui, presumibilmente ci sono preditorri più adatti da inserire o sostituire con gli attuali
# a livello pratico significa che lo sport non spiega coì bene il variare dei dati e che ci potrebbero essere ulteriori caratteristiche che ci portano a capire meglio le differenze di aggressività
# I residui del modello sembrano un po' sbilanciati guardo il summary, ma bisognerebbe verificarlo con un test specifico di normalità o con la visualizzazione di grafici adeguati( es. qqplot)
# Guardando i coefficienti si vede come sia nuoto che tennis abbiano un livello di aggressività decisamente inferiore. Il nuoto in primis risulta quello con il livello minore con un punteggio di -4.9 medio rispetto ai calciatori
# La prima ipotesi per cui la rifiutiamo. Il livello di aggressività dei calciatori rispetto ai nuotatori non è uguale, come dimostra la significatività del pvalue.
# Questo potrebbe essere dovuto sia dall'ambiente molto mascolino del calcio sia all'essere uno sport di contatto rispetto al nuoto
# La seconda ipotesi è confermata. il livello di aggressività medio di un tennista è inferiore rispetto a quello di un calciatore, come conferma il pvalue.
# anche qui la motivazione del risultato potrebbe essere la medesima

mod = lm(Punteggio ~ Motivazione*Corso, datax)
summary(mod)
plot( effects::effect('Motivazione:Corso', mod), multiline = T)
plot( effects::effect('Motivazione:Corso', mod) )
plot( effects::effect('Corso', mod) )
plot( effects::effect('Motivazione', mod) )

# innanzitutto il modello risulta significativo, per cui i predittori spiegano almeno in parte la nostra y, e l' r^2 totale risulta 0.33, che in ambito psicologico è un valore discreto, ma bisognerebbe conoscere meglio la letteratura di riferimento
# a prescindere da ciò i coefficienti e l'r^2 ci possono far intuire che, presumibilmente, esistono caratteristiche più incisive per questo costrutto che si potrebbero aggiungere o con le quali creare nuove combinazioni
# I residui del modello sembrano un po' sbilanciati guardo il summary, ma bisognerebbe verificarlo con un test specifico di normalità o con la visualizzazione di grafici adeguati( es. qqplot)
# Osservando i coefficienti vediamo come ci sia un unico valore con forte magnitudine, CorsiSI. Questo è anche l'unico ad essere significativo p-value 0.02.
# Questo significa che ponendo motivazione a 0, i la frequentazione o meno del corso è incisiva sul punteggio di aggressività. nello specifico, in media c'è una differenza di -12 per chi ha frequentato il corso (a parità di punteggio di motivazione)
# la motivazione invece presenta un coefficiente quasi nullo (-0.06) non significicativo. Questo ci mostra come la motivazione sulla partecipazione allo studio non sia impattante per il punteggio di aggressività
# Molto simile è il risultato dell'interazione tra corso e motivazione, solo con un coefficiente leggermente positivo, seppur 

mod0 = lm(Punteggio ~ 1, datax)
mod1 = lm(Punteggio ~ Motivazione, datax)
mod2 = lm(Punteggio ~ Corso, datax)
mod3 = lm(Punteggio ~ Motivazione + Corso, datax)
mod4 = lm(Punteggio ~ Motivazione*Corso, datax)

anova( mod0, mod1, mod2, mod3, mod4)
BIC( mod0, mod1, mod2, mod3, mod4 )
AIC( mod0, mod1, mod2, mod3, mod4 )

summary(mod2)

# Domanda 1

# La metanalisi è una tipologia di disegno di ricerca non sperimentale, non effettuando direttamente la raccolta dei dati e la manipolazione della variabile indipendente.
# Questa si basa sulla raccolta dei dati dalla letteratura di riferimerimento e, tramite tecniche statistiche e la conoscenza approfondita dell'ambito, 
# l'unione di questi dati da diverse fonti per creare un quadro rissuntivo dell'andamento di un fenomeno o per analizzare aspetti specifici (per esempio andamenti cross-culturali).
# La realizzazione di una metanalisi è però meticolosa in quanto deve seguire precise teniche in modo che sia fortemente sistematica e più robusta contro bias e artefatti.
# una tra queste è la PICA??

# Domanda 2

# La reliability risulta un aspetto fondamentale per un questionario in quanto permette di valutare quanto affidabile è il punteggio effettuato su quella scala.
# Con affidabile si intende quanto gli item sono coerenti col costrutto ricercato, cioè la 'precisione' degli item nella valutazione. 
# Per fallo si controlla quanto il punteggio dell'item è correlato con il punteggio totale senza quell'item. Questo permette di valutare se l'item è efficace nella valutazione della scala stessa e se è coerente con essa.
# qquesto permette di verificare la presenza di item non adatti alla scala e, in caso, lo snellimento della scala o la modifica della stessa




