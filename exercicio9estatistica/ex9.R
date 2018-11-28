# QUESTAO 01

dados <- scan("vendas.dat",what=list(telhados=0, gastos=0, clientes=0, marcas=0, potencial=0))
attach(dados)

# Letra A
 
plot(x = dados$gastos , y = dados$telhados)

plot(x = dados$clientes , y = dados$telhados)

plot(x = dados$marcas , y = dados$telhados)

plot(x = dados$potencial , y = dados$telhados)

# Letra B

cor(matrix(unlist(dados), nrow=26))

# Letra C

lm1 <- lm(telhados ~ marcas)
lm2 <- lm(telhados ~ marcas+clientes)
lm3 <- lm(telhados ~ marcas+clientes+potencial)
lm4 <- lm(telhados ~ gastos+clientes+marcas+potencial)

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)


# QUESTAO 2

# Letra A



data2 <- matrix(c(37.310, 37.380,34.135,36.985,38.715,40.620,39.200,40.320,10,5,3,6,8,20,8,14,2,6,1,5,8,0,4,6,16,16,12,14,16,12,18,17), nrow = 8 , ncol =4 , byrow = FALSE)


vetor_x = matrix(c(1,1,1,1,1,1,1,1,10,5,3,6,8,20,8,14,2,6,1,5,8,0,4,6,16,16,12,14,16,12,18,17), nrow = 8 , ncol =4 , byrow = FALSE)
  
vetor_y = c(37.310, 37.380,34.135,36.985,38.715,40.620,39.200,40.320)

vetor_x
beta <- solve(t(vetor_x) %*% vetor_x) %*% (t(vetor_x) %*% vetor_y)

modelo_matriz = function(beta1,x1,x2,x3){beta[1,] + beta[2,]*x1 + beta[3,]*x2 + beta[4,]*x3}

y_chapeu = c(modelo_matriz(beta = beta[1,],vetor_x[1,2],vetor_x[1,3],vetor_x[1,4]),
          modelo_matriz(beta = beta[1,],vetor_x[2,2],vetor_x[2,3],vetor_x[2,4]),
          modelo_matriz(beta = beta[1,],vetor_x[3,2],vetor_x[3,3],vetor_x[3,4]),
          modelo_matriz(beta = beta[1,],vetor_x[4,2],vetor_x[4,3],vetor_x[4,4]),
          modelo_matriz(beta = beta[1,],vetor_x[5,2],vetor_x[5,3],vetor_x[5,4]),
          modelo_matriz(beta = beta[1,],vetor_x[6,2],vetor_x[6,3],vetor_x[6,4]),
          modelo_matriz(beta = beta[1,],vetor_x[7,2],vetor_x[7,3],vetor_x[7,4]),
          modelo_matriz(beta = beta[1,],vetor_x[7,2],vetor_x[8,3],vetor_x[8,4]))
