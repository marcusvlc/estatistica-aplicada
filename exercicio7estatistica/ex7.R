########## QUESTAO 01 ##########

# hipotese h0: Testar se a moeda favorece cara

# hipotese h1: Testar se a moeda nao favorece cara

prob_cara = 2/5

prob_zero_caras = dbinom(0, 4, prob_cara)

prob_uma_cara = dbinom(1, 4, prob_cara)
  
prob_duas_caras = dbinom(2, 4, prob_cara)
  
prob_tres_caras = dbinom(3, 4, prob_cara)
  
prob_quatro_caras = dbinom(4, 4, prob_cara)

valores_observados = c(72,204,228,101,20)
valores_esperados = c(625*prob_zero_caras, 625*prob_uma_cara, 625*prob_duas_caras, 625*prob_tres_caras, 625*prob_quatro_caras)


qui_quadrado = sum( ((valores_observados - valores_esperados) **2) / valores_esperados) 

pchisq(qui_quadrado, df = 4, lower.tail = FALSE)

# Como P = 0.4637673 > 0.05, entao, nao rejeitamos h0, entao ha evidencias que a moeda favorece cara 

########## QUESTAO 02 ##########

dados = dados_cia = read.csv('DadosCiaMB.csv', sep = ";")

attach(dados)

# Letra A, criando a tabela de distribuicao conjunta para as variaveis grau de instrucao e regiao de procedencia

tabela = table(Instrucao, procedencia)

# Letra B

linha_um <- c(tabela[1,1:3])
linha_dois <- c(tabela[2,1:3])
linha_tres <- c(tabela[3,1:3])

col_um <-c(tabela[1:3,1])
col_dois <-c(tabela[1:3,2])
col_tres <-c(tabela[1:3, 3])

sum_linhas = c( sum(linha_um), sum(linha_dois), sum(linha_tres) )
sum_colunas = c( sum(col_um), sum(col_dois), sum(col_tres) )

quiQ2 = 0

calculaEsperado <- function (i, j){
  return ((sum_linhas[i] * sum_colunas[j]) / sum(sum_linhas))
}

(sum_linhas[1] * sum_colunas[1]) / sum(sum_linhas)
calculaEsperado(1,1)
sum(sum_linhas)

for(i in 1:3) {
  for(j in 1:3) {
    quiQ2 = (quiQ2 + ((tabela[i,j] - calculaEsperado(i, j))**2) / calculaEsperado(i,j))
  }
}

pchisq(quiQ2, df = 4, lower.tail = TRUE)

# H0: Existe dependencia entre as variaveis
# H1: Nao existe dependencia entre as variaveis

# Como P = 0.04400023 < 0.05, entao, rejeitamos H0, logo, existem evidencias que nao ha dependencia entre as variaveis
