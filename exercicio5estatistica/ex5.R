# QUESTAO 01

domicilios = c(4,5,2,9,1,4,4,6,7,2,2,4,4,7,4,5,6,8,1,2,6,4,2,3,2,3,2,4,5,6,8,5,2,3,4,1,6,3,2,3,5,4,8,5,4,2,4,3,2,4,5,9,5,6,4,3,4,5,4,2,9,8,18,8,7,9,6,14,8,9,22,8,9,14,9,9,8,8,15,7,7,9,9,8,7,12,8,9,8,8)
qtdDomicilios = length(domicilios)

# letra a

acumMaioresQueCinco = 0
acumComodos = 0

for(i in 1:qtdDomicilios){
  acumComodos = acumComodos + domicilios[i]
  if(domicilios[i] > 5) {
    acumMaioresQueCinco = acumMaioresQueCinco + 1
  }
  
}

proporcaoMaiorQueCinco = (acumMaioresQueCinco / qtdDomicilios) * 100
mediaComodos = acumComodos / qtdDomicilios

