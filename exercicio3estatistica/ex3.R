#QUESTAO 01

questao01 <- function(n, replaceValue){      

x <- c(1,2,3,3,5)
      vector <- c()
      
      for (j in n) {
        for (i in 1:1000){
          mediaDaAmostra <- mean(sample(x, j, replace=replaceValue))
          vector <- c(vector,mediaDaAmostra)
        }
        hist(vector, main = paste('Histograma para o valor',j))
        print(paste('Extraindo 1000 amostras de tamanho', j, sep=' '))
        print(paste('Media dos valores:', mean(vector), sep= ' '))
        print(paste('A media deve se aproximar de', mean(x)))
        print(paste('Variancia:', var(vector), sep=' '))
        varDeX <- var(x)/j
        print(paste('Variancia da amostra deve se aproximar de:',varDeX , sep=' '))
        vector <- c()
        print('----------------------------------')
    }
      
}

#Chamando a funcao da questao 01 referente a letra A

questao01(c(2,3,5,10), TRUE)
      
#Chamando a funcao da questao 01 referente a letra B

questao01(c(2,3), FALSE)

# -------------------------------

# QUESTAO 02

acumMediasNormal <- c()
acumVarNormal <- c()

for(k in 1:1000){
  normal <- rnorm(100, mean=100, sd=5)
  media <- mean(normal)
  variancia <- var(normal)
  acumMediasNormal <- c(acumMediasNormal, media)
  acumVarNormal <- c(acumVarNormal, variancia)
}

hist(acumMediasNormal)
hist(acumVarNormal)

#Comparando os valores gerados com os fornecidos


mediaTotal <- mean(acumMediasNormal)
varianciaTotal <- mean(acumVarNormal)

print(paste('A media e a variancia obtida foi, respectivamente:', mediaTotal, varianciaTotal))
print(paste('A media e a variancia do resultado teorico eh: 100 e 25'))

# ------------------------------



# QUESTAO 03
# Gerando 1000 amostras de tamanho 100 da distribuicao exp com alpha igual a 10

alpha = 10
expMediaAcum = c()
for (i in 1:1000){
  exp = rexp(100, rate=alpha)
  expMediaAcum = c(expMediaAcum, mean(exp))
  
}
T = 1/expMeanDist

print(paste('Media do parametro T:',mean(T)))
print(paste('Variancia do parametro T',var(T)))
hist(T, main="Histograma de T")