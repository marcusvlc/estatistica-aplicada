##########  QUESTAO 01 ######### 

domicilios = c(4,5,2,9,1,4,4,6,7,2,2,4,4,7,4,5,6,8,1,2,6,4,2,3,2,3,2,4,5,6,8,5,2,3,4,1,6,3,2,3,5,4,8,5,4,2,4,3,2,4,5,9,5,6,4,3,4,5,4,2,9,8,18,8,7,9,6,14,8,9,22,8,9,14,9,9,8,8,15,7,7,9,9,8,7,12,8,9,8,8)
qtdDomicilios = length(domicilios)

######### letra a ######### 

proporcaoMaiorQueCinco = function(populacao, qtdPopulacao) {
  contador = 0
  for(i in 1:qtdPopulacao) {
    if(populacao[i] > 5){
      contador = contador + 1
    }
  }
  
  return (contador/qtdPopulacao)
}

# Variavel que armazena a proporcao referente aos
# domicilios com mais de 5 comodos (populacional)
parametroPopP = proporcaoMaiorQueCinco(domicilios, qtdDomicilios)
# Variavel que armazena a media de comodos dos domicilios (populacional)
parametroPopMi = mean(domicilios)

print(parametroPopMi)
print(parametroPopP)


######### letra b e d ######### 

qtdRepeticoes = 10000
qtdColetaAmostra = 20
alfa = 0.05
qtdRejeicoesH0 = 0
qtdRejeicoesH0_d = 0

for(j in 1:qtdRepeticoes) {
  amostraDomicilios = sample(domicilios, qtdColetaAmostra, replace = FALSE)
  teste = t.test(amostraDomicilios, y = NULL,  mu = 7, alternative = "two.sided")
  
  teste_d = prop.test(x = sum(amostraDomicilios>5), n = qtdColetaAmostra, p = 0.5,
                      alternative = "two.sided",
                      conf.level = 0.95, correct = TRUE)
  
  
  parametroP = teste$p.value
  
  parametroP_d = teste_d$p.value
  
  if(parametroP <= alfa) {
    qtdRejeicoesH0 = qtdRejeicoesH0 + 1
  }
  
  if(parametroP_d <= alfa) {
    qtdRejeicoesH0_d = qtdRejeicoesH0_d + 1
  }
}

proporcaoRejeicao = qtdRejeicoesH0 / qtdRepeticoes
proporcaoRejeicao_d = qtdRejeicoesH0_d / qtdRepeticoes

# Aumentar o alfa pra regiao critica aumentar e rejeitar mais, 
# ou aumentar o tamanho das amostras  que estao sendo coletadas

# Aumentando o tamanho da amostra de 20 para 40, temos:

count = 0

for(k in 1:10000) {
  amostra = sample(domicilios, 40, replace = FALSE)
  teste_c = t.test(amostra, y = NULL,  mu = 7, alternative = "two.sided")
  
  p_amostra = teste_c$p.value
  
  if(p_amostra <= alfa) {
    count = count + 1
  }
}

proporcaoRejeicao_c = count / 10000

