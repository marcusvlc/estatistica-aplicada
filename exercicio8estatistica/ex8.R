########## QUESTAO 01 ############

dados <- read.csv("dados.csv", sep = ";", dec = ",")
attach(dados)

## LETRA A ##

plot(dados$idade,dados$salario, xlab = "Idade", ylab = "Salario")

## LETRA C ##

cor(dados$idade, dados$salario)

## LETRA D ##

model <- lm(dados$idade ~ dados$salario)

## LETRA E ##

X< dados$idade
Y<-dados$salario
lm(Y~X)
plot(X,Y)
abline(lm(Y ~ X))

########## QUESTAO 02 ############

## LETRA A ##

cor(dados$idade, dados$salario)

a <- lm(dados$idade ~ dados$salario)
summary(a)

## LETRA B ##

cor(dados$idade**2, dados$salario)

xb <- dados$idade**2
yb <- dados$salario
  
plot(xb, yb)

abline(lm(yb ~ xb))

b <- lm(dados$idade**2 ~ dados$salario)
summary(b)


## LETRA C ##

cor(log(dados$idade), dados$salario)

c <- lm(log(dados$idade) ~ dados$salario)
summary(c)


xc <- log(dados$idade)
yc <- dados$salario

plot(xc, yc)

abline(lm(yc ~ xc))


## LETRA D ##

cor(sqrt(dados$idade), dados$salario)

d <- lm(sqrt(dados$idade) ~ dados$salario)
summary(d)

xd <- sqrt(dados$idade)
yd <- dados$salario

plot(xd, yd)

abline(lm(yd ~ xd))


## LETRA E ##

cor(1/dados$idade, dados$salario)

e <- lm(1/dados$idade ~ dados$salario)
summary(e)

xe <- 1/(dados$idade)
ye <- dados$salario

plot(xe, ye)

abline(lm(ye ~ xe))
