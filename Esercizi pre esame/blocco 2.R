{
  
  bkp1 <- kappa1
  
  library(irr)
  
  kappa2(kappa1)
  
  #kappa molto basso. 
  #non si può dire che l'accordo tra giudici non sia causale dato il valore del k (quasi 0.2) e 
  #un pvalue non significativo
  
}# kappa 1

{
  library(irr)
  
  bkp <- kappa2
  
  kappa2( kappa2)
  
  #controprova
  library(psych)
  cohen.kappa( table(kappa2) )
  
}# kappa 2


{
  library(irr)
  
  kappa2(kappa3)
}# kappa 3