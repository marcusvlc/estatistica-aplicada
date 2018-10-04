dados <- read.csv("salarios.csv", header = TRUE, sep = ",")
attach(dados)
hist(Mecânico,col="green",main="Histograma da variável Mecânico")
hist(Prof..Sec.,col="blue",main="Histograma da variável Professor Secundarista")
hist(Administrador,col="red",main="Histograma da variável Admininstrador")
hist(Eng..Eletr.,col="purple",main="Histograma da variável Engenheiro Elétrico")

summary(Mecânico)
var(Mecânico)
sd(Mecânico)

summary(Prof..Sec.)
var(Prof..Sec.)
sd(Prof..Sec.)

summary(Administrador)
var(Administrador)
sd(Administrador)

summary(Eng..Eletr.)
var(Eng..Eletr.)
sd(Eng..Eletr.)

boxplot(Mecânico, col="green", main="BoxPlot Salários dos Mecânicos")
data=c("Mecânico", "Prof..Sec.", "Administrador", "Eng..Eletr.")
boxplot(Mecânico,Prof..Sec., Administrador, Eng..Eletr., ylab = "salários", xlab="profissões", names= data)
  