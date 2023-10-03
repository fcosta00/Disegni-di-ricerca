# Sheldon con OCD

sheldon <- c('knock','knock','knock','Penny')

for (i in 1:3) {
  for (i in 1:4) {
      print(sheldon[i])
  }
}

# repetion function ( oggetto, numero ripetizioni )
sheldon <- rep( c('knock', 'penny'), 3 )

# repetion function ( oggetto, numero ripetizioni ) -> con each ripete ognuno singolarmente 3 volte
sheldon <- rep( c('knock', 'penny'), each=3 )



# primo piccolo esercizio di creazione dataframe: creare tre vettori e un dataframe che li contenfa

id <- 1:5
alfabeto <- c('a', 'b', 'c', 'd', 'e')
x <- rep('R', 5)

dat1 <- data.frame(
  ID = id,
  alfa = alfabeto,
  x = x
)

#prova ad esportare il dataset in csv. (row.names serve per eliminare i numeri di riga)
write.table( dat1, file = 'dat1.csv', sep = ',', row.names = FALSE )
