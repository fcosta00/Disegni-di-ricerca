y=c(82,49,53,112,47,69,77,71,62,78)
x=c(176,154,138,196,132,176,181,169,150,175)

meanx=mean(x)
meany=mean(y)

scartix=x-meanx
scartiy=y-meany


scartix_quadrato=scartix^2
scartiy_quadrato=scartiy^2

prodotto=scartix*scartiy

somma_prodotti=sum(prodotto)

somma_scartix=sum(scartix_quadrato)
b=somma_prodotti/somma_scartix
round(b,digits = 2)

a=meany-(b*meanx)
round(a, digits = 2)

modello=lm(y~x)

summary(modello)
coef(modello)
