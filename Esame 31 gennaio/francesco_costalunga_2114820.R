# Domanda Teorica -----

#' La matrice dei contrasti è una matrice che va a definire quali sono i confronti da fare nei modelli di regressione lineare o in altre tipologie di analisi statistiche,
#' come per esempio l'ANOVA.
#' Questa è specifica di quei modelli che utilizzano un fattore (variabile categoriale), cioè una variabile che ha più livelli.
#' La sua specificità deriva dalla necessità di definire comme effetturare i test statistici e le stime tra i livelli di queste variabili. 
#' Questa deriva direttamente da quelle che sono le ipotesi di ricerca della ricerca su cui si è basata la raccolta dati e permette di creare una analisi che sia personalizzata sulle ipotesi senza dover includere tutte le possibile combinazioni dei livelli delle varibili coinvolte.
#' Infatti, nel calcolo matematico della stessa, viene creata la cosidetta matrice delle ipotesi ( matrice più facilmente interpretabile che descrive matematicamente le ipotesi di ricerca ) e poi,
#' tramite l'operazione della generalizzata inversa, si ricava la matrice dei contrasti.
#' Tale vantaggio porta a una maggiore significatività dei test statistici che interessano alla nostra ricerca, in quanto, una inclusione maggiore di test porterebbe a dover effettuare la correzzione dei valori dei pvalue,
#' andando ad aumentare il loro lavore. Questa tipologia di matrici possono essere di varie tipologie e presentare diverse caratteristiche.
#' Innanzitutto, tutte le tipologie di contrasti, tranne i più comuni treatment, cono centrati e normalizzati, questo vuol dire che la media di ogni contrasto (la colonna della matrice) viene sottratta al valore assegnato a ciascun livello.
#' Ciò viene fatto per motivazioni interpretative e per avvicinarsi al concetto di ortogonalità. Questo aggiunge al concetto di ortogonalità la impossibilità di ricavare un contrasto basandosi sugli altri, cioè che i contrasti non siano correlati tra loro.
#' Tale specifica porta alla possibilità di interpretare il contrasto singolo, ignorando gli altri. L'ortogonalià però non è mantenuta da tutte le tipologie, ma solo dagli Helmert, dagli Helmert invertiti, dai polinomiali e dovrebbe essere mantenuta quando si creano contrasti personalizzati.
#' Un'altra caratteristica comune dei contrasti è la numerosità, infatti essa è sempre il numero dei livelli della variabile -1. Questo serve a consumare i gradi di libertà andando a migliorare la stima stessa del modello.
#' La conoscenza della matrice dei contrasti utilizzata risulta fondamentale qaundo si va ad interpretare un modello di regressione lineare.
#' I valori della matrice andranno infatti ad ingluenzare sia i valori di signifactività (perchè cambiano i confronti che di fanno), 
#' sia i valori dei coefficienti stessi. Infatti, sepper contrasti differenti potrebbero portare le medesime significatività, avrebbero un significatto differente e valori che potrebbero essere, invertiti, dimezzati o stravolti completamente, in base ai valori assegnati ai livelli nella matrice dei contrasti.
#' Le tipologie di queste matrici possono essere tra le più varie come i polinomiali, che ricercano la curva dell'andamento dei dati, o i sum che vanno a confrontare il livello con quello generale.
#' La matrice dei contrasti per cui andare a stravolgere quello che è il significato del medesimo modello di regressione con la stessa formula applicata

# ES 1 -----

ses <- read.csv("C:/Users/franc/Desktop/Esame 31 gennaio/ses.csv", sep=";")

datax <- ses
str(datax)
summary(datax)

min<- 1
max<- 5

library(RcmdrMisc)
reliability(cov(datax))

#' l'alpha totale calcolata risulta insufficiente (sotto soglia 0.7) e sono presenti degli item con r negativo.
#' Questo vuol dire che la scala non ha una buona consistenza interna e gli item,
#' così come è strutturato attualmente, non rispecchiano bene il costrutto di base.
#' Questo potrebbe essere dovuto all'inversione degli item che presentano attualmente r negativo,
#' per questo procedo con l'inversione di tali item partendo da quello con magnitudine maggiore


datax$SES7inv <- (min + max) - datax$SES7
reliability(cov(datax[,-c(7)]))
#' sono presenti ulteriori item invertiti,
#' inverto per l'item 6, quello con magnitudine maggiore

datax$SES6inv <- (min + max) - datax$SES6
reliability(cov(datax[,-c(7,6)]))
#' l'alpha non ha ancora raggiunto sogli e sono presenti ulteriori item invertiti,
#' inverto per l'item 4, quello con magnitudine maggiore

datax$SES4inv <- (min + max) - datax$SES4
reliability(cov(datax[,-c(7,6,4)]))
#' l'alpha non ha ancora raggiunto sogli e sono presenti ulteriori item invertiti,
#' inverto per l'item 2, quello con magnitudine maggiore

datax$SES2inv <- (min + max) - datax$SES2
reliability(cov(datax[,-c(7,6,4,2)]))
#' l'alpha ha raggiunto la soglia, ma sono presenti ulteriori item invertiti,
#' inverto per l'item 1, quello con magnitudine maggiore

datax$SES1inv <- (min + max) - datax$SES1
reliability(cov(datax[,-c(7,6,4,2,1)]))
#' Non sono èiù presenti item invertiti e l'alpha risulta sopra soglia (0.828). 
#' Inoltre nessun item ha una correlazione item-totale inferiore di 0.2 ciò ci permette di considerare la scala nella sua completezza,
#' senza operare esclusioni di item per una maggiore coerenza interna.

#' Dato il risultato dell'alpha possiamo dire che la scala risulta coerente e tutti gli item rispecchiano il costrutto di ricerca.
#' é pur vero che si potrebbero apportare piccole modifiche per raggiungere un ottimo puntunteggio nell'alpha (>0.85),
#' magari cambiando di poco la formulazione di alcune domande o cambiando un item non eccessivamente rappresentativo.
#' In generale possiamo però definirla una buona scala, ogni tem ha un ottimo valore di alpha e un buon valore di r.
#' Inoltre, nessuno presenta valori estremi, quindi non sembrano essere presenti item ridondandi o troppo predittivi del costrutto.
#' possiamo per cui dire che tutte le domande poste erano erano coerenti tra loro ricercando tutti l'autostima in modo adeguato.




# ES 2 -----

var <- read.csv("C:/Users/franc/Desktop/Esame 31 gennaio/var.csv", sep=";")

datax <- var

str(datax)
summary(datax)

library(irr)
kappa2(datax)

#' L'indice di concordanza tra giudici valutato con la K di Cohen, 
#' ha mostrato un pvalue significativo e un valore di 0.72
#' Questo ci mostra innanzitutto di eascludere che la concordanza tra i due sia dovuta dal caso, 
#' inoltre un valore 0.72 risulta un ottimo dato, e indica come i giusti hanno dato la maggior parte delle volte la stessa valutazione
#' Possiamo per cui pensare che l'anno di addestramento sia stato particolarmente efficace nella preparazione degli arbitri,
#' inoltre che abbia permesso di chiarire in maniera univoca per tutti l'oggetto di valutazione e come la valutazione dovesse venire effettuata.
#' Dopo questi risultati si potrebbe anche pensare di rivisionare le azioni non concordi per poter capire l'oggetto dubbio e ampliare il training per riuscire a raggiungere una concordanza pressochè perfetta
#' 
#' 
#' 

# ES 3 -----

dati_esame_310124 <- read.csv("C:/Users/franc/Desktop/Esame 31 gennaio/dati_esame_310124.csv", sep=";")

datax <- dati_esame_310124

str(datax)
summary(datax)

datax$tipo <- factor( datax$tipo )
levels(datax$tipo)

contrasts(datax$tipo) <- contr.helmert(3)
library(MASS)
ginv(contrasts(datax$tipo))

mod <- lm( eai_tot ~ tipo, datax)
summary(mod)

plot(effects::effect( 'tipo' , mod))

#' La codifica dei contrasti scelta per verificare le ipotesi di ricerca è l'Helmert
#' Questo perchè come primo contrasto permette di confrontare i primi due livelli (aerobico vs anaerobico),
#' e come secondo contrasto valuta il terzo contro la media dei primi due (entrambi vs ( aerobico+ anaerobico )/2)
#' Osservando il summary del modello vediamo innanzitutto che risulta significativo (F-statistic: 5.535 on 2 and 87 DF,  p-value: 0.00546).
#' Questo mostra come possiamo rifiutare l'ipotesi nulla che il modello non predica in alcun modo la nostra y.
#' L'indice di R^2 risulta 0.113, mostrando come il modello spieghi però solo in minima parte quella che è la varianza della Y  e come siano probabilmente necessaria l'inclusione di altri preditorri per screare un modello che si adatta maggiormente ai dati.
#' Ciò significa che i nostri predittori non riescono a spiegare bene il variare di y, 
#' cioè il tipo di attività fisica non spiega bene cambi il valore del punteggio del questionario sulle dipendenze.
#' L'indice R^2 aggiustato risulta 0.0925 e ha il medesimo significato del precedente. 
#' Il valore risulta però leggermente minore perchè viene corretto sulla struttura stessa del modello, 
#' in linea generale andrà a diminuire maggiore sarà la complessità stessa del modello.
#' Osservando i residui vediamo come sembrino abbastanza bilanciati, ma con una probabile skew positiva nella curva di distribuzione.
#' Per verificare che la distribuzione sia normale e che, per cui, gli assunti siano rispettati, sarebbe necessario fare un test specifico o osservare i grafici adatti come il qqplot
#' Analizzando i coefficienti:
#' Intercetta -> risulta significativa per cui è possiamo rigettare H0 che sia uguale a 0. Il valore della stimata si aggiara attorno a 23, che corrisponde alla grand mean,
#' ma, per le nostre ipotesi non risulta necessario analizzarla ulteriormente dato che non ci fornisce un risultato interessante per la conferma o meno delle ipotesi stesse.
#' Tipo1 -> questo è il risultato del contrasto mettendo a confronto aerobico contro anaerobico. 
#' Risulta significativo per cui possiamo rigettare H0 che non sia differenti. 
#' Dato poi il valore della stimata (3.95) possiamo vedere come la tipologia anaerobico porti un aumento del test medio di 4 punti rispetto alla media di aerobico.
#' Questo suggerisce come lo sport anaerobico possa portare una maggiore tendenza alla dipendenza da attività fisica
#' Tipo2 -> Questo non risulta significativo. Dobbiamo per cui accettare l'ipotesi nulla che non esista una differenza significativa tra il punteggio medio di chi pratica uno sport con qntrambe le attività e tra la media di chi pratica solo una delle due.
#' Questo risultato è corroborato dalla stimata (-0.2) che risulta sì negativa, ma non abbastanza forte da suggerire una reale differenza.
#' Sembrerebbe che eseguire uno sport con entrambe le tipologie di attività fisica possa portare a un livello medio di poropensione alla dipendenza da attività fisica.
#' Questo è forse dovuto a un effetto 'positivo' sulla dipendenza da parte della attività aerobica che va a ontrobilanciare quello negativo della attività anaerobica.
#' Interessante sarebbe capise se questa tipologia di dipendenza sarebbe dovuta a fattori di personalità che possano a indurre a praticare una attività rispetto all'altra o analizzare la possibile presenza di altri mediatori.
#' Inoltre, l'inclusione magari di un mediatore o di altri fattori di personalità che possano influenzare la propensione,
#' andrebbero ad aumentare l'R^2 del modello e riuscendo a prevedere meglio quelli che sono i valori al test sulla dipendenza da attività fisica, in qunato, il valore attuale suggerisce come manchino dei predittori probabilmente fondamentali per questo modello.
#' Oltre a quelli di personalità si potrebbe poi includere anche patologie o tratti che hanno riscontro di comorbidità in letteratura,
#' un esempio può essere la presenza di altre dipendenze o ol dismorfismo corporeo, in modo di creare un modello che sia maggiormente adattato alla variabile ricercata e non considerando più come unica possibile influenza la tipologia di attività fisica

# ES 4 -----

dati_esame_310124 <- read.csv("C:/Users/franc/Desktop/Esame 31 gennaio/dati_esame_310124.csv", sep=";")

datax <- dati_esame_310124

str(datax)
summary(datax)

datax$tipo <- factor( datax$tipo )
levels(datax$tipo)

mod <- lm( scoff ~ eai_tot + oci_tot, datax)
summary(mod)

plot(effects::effect( 'eai_tot' , mod))
plot(effects::effect( 'oci_tot' , mod))


#' Osservando il summary del modello vediamo innanzitutto che risulta significativo (F-statistic: 8.583 on 2 and 87 DF,  p-value: 0.0003964).
#' Questo mostra come possiamo rifiutare l'ipotesi nulla che il modello non predica in alcun modo la nostra y.
#' L'indice di R^2 risulta 0.1648, mostrando come il modello spieghi però solo in minima parte quella che è la varianza della Y e come siano probabilmente necessaria l'inclusione di altri preditorri per screare un modello che si adatta maggiormente ai dati.
#' Ciò significa che i nostri predittori non riescono a spiegare bene il variare di y, 
#' cioè la dipendenza da attività fisica e la propensione alla DOC non spiegano bene come il cambiamento sul punteggio relativo alla propensione ai disturbi alimentari.
#' L'indice R^2 aggiustato risulta 0.1456 e ha il medesimo significato del precedente. 
#' Il valore risulta però leggermente minore perchè viene corretto sulla struttura stessa del modello, 
#' in linea generale andrà a diminuire maggiore sarà la complessità stessa del modello.
#' Osservando i residui non sembrano ben bilanciati, con una mediana spostata verso valori negativi e un valore Max sufficiente differente dal min da suggerire la presenza di outlier. 
#' Sarebbe per cui necessario verificare la presenza di outlier e che la distribuzione sia normale e che, per cui, gli assunti siano rispettati.
#' Analizzando i coefficienti:
#' Intercetta -> risulta significativa per cui è possiamo rigettare H0 che sia uguale a 0. Il valore della stimata è 0.33 e rappresenta il valore di y quando settiamo tutte le variabili indipendenti a 0.
#' Per le nostre ipotesi non risulta necessario analizzarla ulteriormente dato che non ci fornisce un risultato interessante per la conferma o meno delle ipotesi stesse.
#' eai_tot -> Non risulta significativo ( p-value 0.812413) per cui dobbiamo accettarre l'ipotesi nulla che non sia influente sulla nostra y.
#' Ciò signifa che il punteggio sul questionario relativo all'attività fisica non risulta predittivo per la propensione ai disturbi alimentari. 
#' Questo è corroborato anche da valore pressochè nullo che assume il coefficiente stimato.
#' oci_tot -> Risulta significativo (pvalue  0.000139) per cui possiamo rifiutare l'ipotesi nulla che non sia influente sulla nostra y.
#' Ciò significa che il punteggio riguardante il DOC risulta predittivo per la propensione ai disturbi alimentari. 
#' Il valore della stimata (0.047580) mostra poi una influenza leggermente positivi, cioè ad ogni scatto di 1 del valore su oci tot il valore dell y aumenta leggermente. 
#' A livello pratico mostra per cui come la propensione per la doc porti a una maggiore propensione ai disturbi alimentari.
#' In generale possiamo dire come la propensione alla dipendenza da attività fisica non sia assolutamente predittiva per i disturbi alimentari,
#' mentre le sintomatiche ossessive si. La possibile sostituzione dal modello dell'attività fisica andrebbe per cui a portare un probabile miglioramento dell'addattamento.
#' Sarebbe interessante una inclusione di differenziazione dei disturbi alimentari per andare a ricerca se differisce l'impatto della doc sulle diverse tipologie.
#' Alternativamente l'inclusione di fattori di personalità come aspetti depressivi o aspetti di ansia, che sappiamo essere a volte in comorbità con i disturbi alimentari,
#' potrebbero essere dei buoni indicatori per la propensione a tali disturbi.
#' 
#