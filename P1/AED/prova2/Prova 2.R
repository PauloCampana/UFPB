#' ---
#' title: "Prova 2 - An�lise explorat�ria de dados"
#' author: "Paulo Ricardo Seganfredo Campana"
#' date: "23 de Outubro de 2021"
#' ---

#' Quest�o 01 - Letra A

q1x<-c(2,1,8,4,5,9,3,3,4,9)

#' Quest�o 01 - Letra B

q1e<-as.factor(c(2,1,2,1,2,1,2,2,1,2))

#' Quest�o 01 - Letra C

q1x[3]

#' Quest�o 01 - Letra D

q1e[10]

#' Quest�o 01 - Letra E

q1banco=data.frame(X=q1x,E=q1e)
q1banco

#' Quest�o 01 - Letra F

q1banco[5,]

#' Quest�o 01 - Letra G

q1banco[,1]

#' Quest�o 01 - Letra H

tapply(q1x,q1e,mean)

#' Quest�o 01 - Letra I

sum(q1x^2)

#' Quest�o 01 - Letra J

mean(q1x)

#' Quest�o 01 - Letra K
#'
#' q1e<-as.factor(q1e), j� feito na letra B
#'
#' Quest�o 2

aleatorio<-function(obj){
  v<-ifelse(obj>4,1,0)
  soma<-sum(v)
  p<-sum(v)/length(obj)
  return(p)}

#' Quest�o 2 - Letra A
#'
#' A fun��o atribui valor 1 para elementos de um vetor que s�o maiores que 4, e 0 para 
#' menores que 4, � ent�o feito a m�dia desses valores e o resultado � direcionado ao console.
#' A fun��o pode ser interpretada como a porcentagem de valores maiores que 4 de um vetor.
#'
#' Quest�o 2 - Letra B

#' Os �nicos valores de X maiores que 4 s�o: 8, 5, 9 e 9, � atribuido 1 a estes 4 valores e 0
#' aos demais 6, a m�dia resultar� em "0.4".

aleatorio(q1x)

#' Quest�o 3

q3y<-c(3.9,2.2,3.7,2.5,2.4,2.3,2.5,2,2,1.4)
q3x<-c(50,38,48,36,34,34,36,30,31,29)

#' Quest�o 3 - Letra A

data.frame("Vari�vel"=c("Peso","Tamanho"),
           "M�dia"=c(mean(q3y),mean(q3x)))

#' Quest�o 3 - Letra B

data.frame("Vari�vel"=c("Peso","Tamanho"),
           "Vari�ncia"=c(var(q3y),var(q3x)),
           "Desvio Padr�o"=c(sd(q3y),sd(q3x)))

#' Quest�o 3 - Letra C
#'
#' Os valores de Y e X est�o em magnitudes diferentes, usa-se a o coeficiente de varia��o.
#' O valor X poss�i menor varia��o.

cv<-function(v){sd(v)/mean(v)}
cv(q3y)
cv(q3x)

#' Quest�o 3 - Letras D e E

par(mfrow=c(2,1))
boxplot(q3x,horizontal=TRUE,main="Tamanho")
boxplot(q3y,horizontal=TRUE,main="Peso")
par(mfrow=c(1,1))

#' Quest�o 3 - Letra F

plot(q3x,q3y,main="Gr�fico de disper��o entre tamanho e peso",
     xlab="Tamanho",ylab="Peso")

#' Quest�o 3 - Letra G
#'
#' Correla��o = 96%, forte ind�cio de correla��o entre as vari�veis.

cor(q3x,q3y)

#' Quest�o 3 - Letra H

asymmetry=function(v,numerador){
  (sqrt(length(v)/(length(v)-1)))*(1/(length(v)-1))*(numerador/sd(v)^3)}

data.frame("Vari�vel"=c("Tamanho","Peso"),
           "Assimetria"=c(asymmetry(q3x,2952.72),asymmetry(q3y,3.013)))

#' Quest�o 3 - Letra I

kurtosis=function(v,numerador){
  (length(v)/(length(v)-1)^2)*(numerador/var(v)^2)-3}

data.frame("Vari�vel"=c("Tamanho","Peso"),
           "Curtose"=c(kurtosis(q3x,55444.03),kurtosis(q3y,7.63)))

#' Quest�o 3 - Letra J
#'
#' a = -1.2834
#' b = 0.1031

lm(q3y~q3x)$coefficients

#' Quest�o 3 - Letra K
#'
#' Sim, porque 92.8% da varia��o de Y pode ser explicada por X (Multiple R-squared:  0.928)

summary(lm(q3y~q3x))$r.squared

#' Quest�o 3 - Letra L
#'
#' Se estes valores s�o de magnitude baixa, fornece evid�ncia que o modelo linear representa
#' bem os dados.

0.001+0.189+0.001+0.005+0.032+0.006+0.005+0.0036+0.008+0.094

#' Quest�o 3 - Letra M
#'
#' Usando os coeficientes da regress�o linear, podemos criar uma fun��o de peso estimado em
#' rela��o ao tamanho.

q3<-function(x){
  lm(q3y~q3x)$coefficients[1]+lm(q3y~q3x)$coefficients[2]*x}

data.frame("Vari�vel"=c("Tamanho","Peso estimado"),
           "Beb� 1"=c(46,q3(46)),
           "Beb� 2"=c(35,q3(35)),
           "Beb� 3"=c(49,q3(49)))

#' Quest�o 3 - Letra N
#'
#' Y = X*b

Y = matrix(lm(q3y~q3x)$fitted.values,nrow=10,ncol=1)
Y
X = matrix(c(1,1,1,1,1,1,1,1,1,1,q3x),nrow=10,ncol=2)
X
b = matrix(lm(q3y~q3x)$coefficients,nrow=2,ncol=1)
b

#' X*b - Y = 0

X %*% b - Y

#' Quest�o 4
#'
#' Objetivo: "Estimar a propor��o de pessoas que est�o infectadas com COVID-19 na cidade de Jo�o
#' Pessoa para o planejamento de a��es sociais referentes �s fam�lias."
#'
#' Usar a PNAD COVID19, uma pesquisa domiciliar j� feita como base, pode ser acessada em:
#' https://biblioteca.ibge.gov.br/visualizacao/instrumentos_de_coleta/doc5592.pdf
#'
#' Algumas quest�es se referem-se a detec��o do COVID, por�m a quest�o 4 assume que os moradores j�
#' tenham feito o teste em domic�lio.
#'
#' Outros tipos de perguntas s�o as referentes a atividades escolares, trabalho, recebimento de
#' pens�o, empr�stimos e alugel, que embora importantes, n�o est�o no escopo atual e deixariam o
#' question�rio muito longo.
#'
#' Nos resta quest�es do tipo: idade, sexo, cor/ra�a, escolaridade, situa��o econ�mica e quest�es
#' referentes a presen�a de coronavirus no domic�lio.
#'
#' Devido a pesquisa ser feita a mais de um ano atr�s, algumas perguntas novas devem ser
#' adicionadascomo as referentes sobre ter tido o v�rus no passado por�m recuperado, se teve
#' parentes que morreram de covid, se j� foram vacinados e quantas doses, se pretendem vacinar.
#'
#' Ficam ent�o com as seguintes vari�veis a ser consideradas:
#'
#' * Moradores do domic�lio                (Valor inteiro positivo)
#' * Idade                                 (Valor em anos)
#' * Sexo                                  (Homem/Mulher/Outro)
#' * Cor/Ra�a                              (Branca/Preta/Amarela/Parda/Ind�gena)
#' * Escolaridade                          (Sem instru��o/Fundamental/M�dio/Superior/P�s-gradua��o)
#' * Renda do domic�lio                    (Valor em Reais)
#' * Ter covid atualmente                  (Sim/N�o/N�o sei)
#' * Ter tido covid e recuperado           (Sim/N�o)
#' * Mortes por covid no domic�lio         (Valor inteiro positivo)
#' * Estado de vacina��o                   (Ainda n�o vacinado/1� dose/2� dose/N�o irei tomar)