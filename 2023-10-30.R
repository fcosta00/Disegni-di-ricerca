
# Calcolo della k di Khoen passaggio per passaggio, seguendo le slide

tabella <-  matrix( c(18, 2, 3, 26), 2, 2, byrow = T)
tabella

rowSums(tabella)
colSums(tabella)

AO = 18 + 26

N =  (18 + 2) + (3 + 26)

AA0 = (20*21)  / N
AA1 = (28*29) / N

K = (AO - (AA0+AA1) ) / (N - (AA0+AA1))


