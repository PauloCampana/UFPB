#' ---
#' title: "Prova 2 - Análise exploratória de dados"
#' author: "Paulo Ricardo Seganfredo Campana"
#' date: "23 de Outubro de 2021"
#' ---

#' Questão 01 - Letra A

q1x<-c(2,1,8,4,5,9,3,3,4,9)

#' Questão 01 - Letra B

q1e<-as.factor(c(2,1,2,1,2,1,2,2,1,2))

#' Questão 01 - Letra C

q1x[3]

#' Questão 01 - Letra D

q1e[10]

#' Questão 01 - Letra E

q1banco=data.frame(X=q1x,E=q1e)
q1banco

#' Questão 01 - Letra F

q1banco[5,]

#' Questão 01 - Letra G

q1banco[,1]

#' Questão 01 - Letra H

tapply(q1x,q1e,mean)

#' Questão 01 - Letra I

sum(q1x^2)

#' Questão 01 - Letra J

mean(q1x)

#' Questão 01 - Letra K
#'
#' q1e<-as.factor(q1e), já feito na letra B
#'
#' Questão 2

aleatorio<-function(obj){
  v<-ifelse(obj>4,1,0)
  soma<-sum(v)
  p<-sum(v)/length(obj)
  return(p)}

#' Questão 2 - Letra A
#'
#' A função atribui valor 1 para elementos de um vetor que são maiores que 4, e 0 para 
#' menores que 4, é então feito a média desses valores e o resultado é direcionado ao console.
#' A função pode ser interpretada como a porcentagem de valores maiores que 4 de um vetor.
#'
#' Questão 2 - Letra B

#' Os únicos valores de X maiores que 4 são: 8, 5, 9 e 9, é atribuido 1 a estes 4 valores e 0
#' aos demais 6, a média resultará em "0.4".

aleatorio(q1x)

#' Questão 3

q3y<-c(3.9,2.2,3.7,2.5,2.4,2.3,2.5,2,2,1.4)
q3x<-c(50,38,48,36,34,34,36,30,31,29)

#' Questão 3 - Letra A

data.frame("Variável"=c("Peso","Tamanho"),
           "Média"=c(mean(q3y),mean(q3x)))

#' Questão 3 - Letra B

data.frame("Variável"=c("Peso","Tamanho"),
           "Variância"=c(var(q3y),var(q3x)),
           "Desvio Padrão"=c(sd(q3y),sd(q3x)))

#' Questão 3 - Letra C
#'
#' Os valores de Y e X estão em magnitudes diferentes, usa-se a o coeficiente de variação.
#' O valor X possúi menor variação.

cv<-function(v){sd(v)/mean(v)}
cv(q3y)
cv(q3x)

#' Questão 3 - Letras D e E

par(mfrow=c(2,1))
boxplot(q3x,horizontal=TRUE,main="Tamanho")
boxplot(q3y,horizontal=TRUE,main="Peso")
par(mfrow=c(1,1))

#' Questão 3 - Letra F

plot(q3x,q3y,main="Gráfico de disperção entre tamanho e peso",
     xlab="Tamanho",ylab="Peso")

#' Questão 3 - Letra G
#'
#' Correlação = 96%, forte indício de correlação entre as variáveis.

cor(q3x,q3y)

#' Questão 3 - Letra H

asymmetry=function(v,numerador){
  (sqrt(length(v)/(length(v)-1)))*(1/(length(v)-1))*(numerador/sd(v)^3)}

data.frame("Variável"=c("Tamanho","Peso"),
           "Assimetria"=c(asymmetry(q3x,2952.72),asymmetry(q3y,3.013)))

#' Questão 3 - Letra I

kurtosis=function(v,numerador){
  (length(v)/(length(v)-1)^2)*(numerador/var(v)^2)-3}

data.frame("Variável"=c("Tamanho","Peso"),
           "Curtose"=c(kurtosis(q3x,55444.03),kurtosis(q3y,7.63)))

#' Questão 3 - Letra J
#'
#' a = -1.2834
#' b = 0.1031

lm(q3y~q3x)$coefficients

#' Questão 3 - Letra K
#'
#' Sim, porque 92.8% da variação de Y pode ser explicada por X (Multiple R-squared:  0.928)

summary(lm(q3y~q3x))$r.squared

#' Questão 3 - Letra L
#'
#' Se estes valores são de magnitude baixa, fornece evidência que o modelo linear representa
#' bem os dados.

0.001+0.189+0.001+0.005+0.032+0.006+0.005+0.0036+0.008+0.094

#' Questão 3 - Letra M
#'
#' Usando os coeficientes da regressão linear, podemos criar uma função de peso estimado em
#' relação ao tamanho.

q3<-function(x){
  lm(q3y~q3x)$coefficients[1]+lm(q3y~q3x)$coefficients[2]*x}

data.frame("Variável"=c("Tamanho","Peso estimado"),
           "Bebê 1"=c(46,q3(46)),
           "Bebê 2"=c(35,q3(35)),
           "Bebê 3"=c(49,q3(49)))

#' Questão 3 - Letra N
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

#' Questão 4
#'
#' Objetivo: "Estimar a proporção de pessoas que estão infectadas com COVID-19 na cidade de João
#' Pessoa para o planejamento de ações sociais referentes às famílias."
#'
#' Usar a PNAD COVID19, uma pesquisa domiciliar já feita como base, pode ser acessada em:
#' https://biblioteca.ibge.gov.br/visualizacao/instrumentos_de_coleta/doc5592.pdf
#'
#' Algumas questões se referem-se a detecção do COVID, porém a questão 4 assume que os moradores já
#' tenham feito o teste em domicílio.
#'
#' Outros tipos de perguntas são as referentes a atividades escolares, trabalho, recebimento de
#' pensão, empréstimos e alugel, que embora importantes, não estão no escopo atual e deixariam o
#' questionário muito longo.
#'
#' Nos resta questões do tipo: idade, sexo, cor/raça, escolaridade, situação econômica e questões
#' referentes a presença de coronavirus no domicílio.
#'
#' Devido a pesquisa ser feita a mais de um ano atrás, algumas perguntas novas devem ser
#' adicionadascomo as referentes sobre ter tido o vírus no passado porém recuperado, se teve
#' parentes que morreram de covid, se já foram vacinados e quantas doses, se pretendem vacinar.
#'
#' Ficam então com as seguintes variáveis a ser consideradas:
#'
#' * Moradores do domicílio                (Valor inteiro positivo)
#' * Idade                                 (Valor em anos)
#' * Sexo                                  (Homem/Mulher/Outro)
#' * Cor/Raça                              (Branca/Preta/Amarela/Parda/Indígena)
#' * Escolaridade                          (Sem instrução/Fundamental/Médio/Superior/Pós-graduação)
#' * Renda do domicílio                    (Valor em Reais)
#' * Ter covid atualmente                  (Sim/Não/Não sei)
#' * Ter tido covid e recuperado           (Sim/Não)
#' * Mortes por covid no domicílio         (Valor inteiro positivo)
#' * Estado de vacinação                   (Ainda não vacinado/1ª dose/2ª dose/Não irei tomar)