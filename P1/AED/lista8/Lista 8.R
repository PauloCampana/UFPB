###############################################################################################

# 1) q1(250) = 4049.892

q1b=function(q1lengthx,q1sumxy,q1sumx,q1sumy,q1sumx2){
  (q1sumxy-(q1sumx*q1sumy/q1lengthx))/(q1sumx2-(q1sumx^2/q1lengthx))}

q1a=function(q1lengthx,q1sumx,q1sumy){
  (q1sumy/q1lengthx)-(q1b(q1lengthx,q1sumxy,q1sumx,q1sumy,q1sumx2))*(q1sumx/q1lengthx)}

q1lengthx=10
q1sumxy=10.025
q1sumx=2.08
q1sumy=29.6
q1sumx2=0.6714

q1b(q1lengthx,q1sumxy,q1sumx,q1sumy,q1sumx2)
q1a(q1lengthx,q1sumx,q1sumy)

q1=function(q1x){
  q1a(q1lengthx,q1sumx,q1sumy)+(q1b(q1lengthx,q1sumxy,q1sumx,q1sumy,q1sumx2))*q1x}

q1(250)

###############################################################################################

# 2)

###############################################################################################

# 3)

###############################################################################################

# 4)

# a) 

q4ano=seq(2007,2015)
q4prod=c(3.5,4.5,6,6.8,5.2,7,NaN,NaN,NaN)

plot(q4ano,q4prod)

# b) cor = 80%

cor(q4ano,q4prod)

# c)

lm(q4prod~q4ano)

# d) N?o muito, pois r^2 = 55%

summary(lm(q4prod~q4ano))

# e) q4(2013) = 66198 R$

q4=function(ano){
  -1165.7514+0.5824*ano}

q4(2013)

# f) Varia de forma linear, que n?o representa a realidade.

plot(q4ano,q4prod,
     xlim=c(2007,2015),
     ylim=c(3.5,8))
lines(q4ano,q4(q4ano),
     xlim=c(2007,2015),
     ylim=c(3.5,8))

###############################################################################################

# 5)

numero=seq(1,32)
peso=c(115.1,90.4,97.0,102.5,108.1,118.0,110.5,103.7,115.9,96.1,95.0,120.1,98.0,99.5,111.1,
         105.0,90.1,117.1,115.6,97.1,114.9,96.0,95.0,117.5,96.7,117.1,117.9,95.4,116.9,116.8,
         95.9,97.5)
comprimento=c(1.15,1.17,1.61,1.57,1.56,1.65,1.45,1.38,1.45,1.43,1.45,1.60,1.49,1.39,1.38,
                1.47,1.56,1.65,1.45,1.43,1.45,1.60,1.49,1.17,1.61,1.57,1.56,1.65,1.15,1.17,
                1.61,1.57)
idade=c(5,4,6,7,6,5,5,4,8,6,4,7,7,8,5,6,7,8,4,5,6,7,7,7,5,5,7,7,6,8,8,5)
gordura=c(26.7,17.4,16.5,28.9,26.0,25.9,25.7,20.9,21.9,18.5,20.1,28.9,19.0,17.0,25.0,22.8,
            10.6,23.8,25.6,18.2,29.0,11.7,10.8,22.9,14.1,20.8,20.9,16.7,25.3,25.1,14.0,13.8)

# a)

data.frame(Dado=c("Peso","Comprimento","Idade","Gordura"),
           M?dia=c(mean(peso),mean(comprimento),mean(idade),mean(gordura)))

# b)

data.frame(Dado=c("Peso","Comprimento","Idade","Gordura"),
           Vari?ncia=c(var(peso),var(comprimento),var(idade),var(gordura)),
           Desvios_Padr?es=c(sd(peso),sd(comprimento),sd(idade),sd(gordura)))

# c), d), e)

plot(peso,comprimento)
plot(peso,idade)
plot(peso,gordura)

# f), g), h)

cor(peso,comprimento)
cor(peso,idade)
cor(peso,gordura)

# i) Apenas o graf?co e correla??o dos valores peso e gordura mostram uma correla??o adequada.

# j), k), l), m), n), o), p)

summary(lm(peso~comprimento))
summary(lm(peso~idade))
summary(lm(peso~gordura))
summary(lm(peso~comprimento+idade))
summary(lm(peso~comprimento+gordura))
summary(lm(peso~idade+gordura))
summary(lm(peso~comprimento+idade+gordura))

# q), r), s), t), u), v), w)

summary(lm(log(peso)~comprimento))
summary(lm(log(peso)~idade))
summary(lm(log(peso)~gordura))
summary(lm(log(peso)~comprimento+idade))
summary(lm(log(peso)~comprimento+gordura))
summary(lm(log(peso)~idade+gordura))
summary(lm(log(peso)~comprimento+idade+gordura))

# x) O R j? faz isso internamente, sim, o resultado ? refletido no valor R^2 ajustado,
#    o modelo 13 apresentou melhor resultado com R^2 ajustado = 63.28%.

# y) 121, 106, 89, 119, 55, 119 respectivamente.

novoporco=(data.frame(q5idade=c(7,4,5,7,6),q5gordura=c(29.9,23.8,10.1,28.9,30)))
novoporco
q5y=function(q5idade,q5gordura){
  exp(1)^(4.283158+0.012911*novoporco$q5idade+0.014194*novoporco$q5gordura)}
q5y(novoporco)

# z) Sim, o modelo 13.

###############################################################################################

# 6)

q6meanx=0.94
q6meany=6.69
q6nvarx=166
q6nvary=4029
q6nvarxy=792

# a) cor = 96%

q6cor=function(q6nvarx,q6nvary,q6nvarxy){
  q6nvarxy/(sqrt(q6nvarx*q6nvary))}

q6cor(q6nvarx,q6nvary,q6nvarxy)

# b)

q6b=function(q6nvarxy,q6nvarx){
  q6nvarxy/(q6nvarx)}

q6a=function(q6meanx,q6meany,q6b){
  q6meany-q6b(q6nvarxy,q6nvarx)*q6meanx}

q6=function(q6x){q6a(q6meanx,q6meany,q6b)+q6b(q6nvarxy,q6nvarx)*q6x}

# c) 40,37 e 73,77 respectivamente

q6(8)
q6(15)

# d)

plot(q6)

# e) N?o mudaria, pois n?o ? utilizado na f?rmula.

# f) No gr?fico 2, sim.

###############################################################################################

# 7)

q7x=c(5,-4,8,7,5,1,7,7,8)
q7y=c(3,5,7,9,11,13,24,8,1)

# a) R^2 = 0.7%

summary(lm(q7y~q7x))

# b) R^2 = 45%

summary(lm(q7y~q7x-1))

# c) O segundo modelo, a adi??o de um intercepto prejudica no aumento da correla??o deste modelo.


###############################################################################################

# 8)

attach(q8)

# a) Correlação negativa, indica que o rendimento diminui quanto maior o número de horas no
# facebook.

cor(rendimento,horas)

# b) Não, correlação favorável

# c)

summary(lm(rendimento~horas))

# d) e e)

cor(rendimento[sexo=="M"],horas[sexo=="M"])
cor(rendimento[sexo=="F"],horas[sexo=="F"])

summary(lm(rendimento[sexo=="M"]~horas[sexo=="M"]))
summary(lm(rendimento[sexo=="F"]~horas[sexo=="F"]))

# f) O grupo Feminino

# g)

sexo=c(1,1,0,0,1,0,1,1,1,0,0,1,0,1,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,0)

summary(lm(rendimento~horas+sexo))

# h)

lm(rendimento~horas+sexo)$coefficients
q8coef=lm(rendimento~horas+sexo)$coefficients


q8=function(horas,sexo){
  q8coef[1]+q8coef[2]*horas+q8coef[3]*sexo}

novoaluno=data.frame(aluno=31:38,
                     horas=c(9,2,8,11,2,3,5,10),
                     sexo=c(0,0,1,0,1,1,0,1))
novoaluno

data.frame(aluno=31:38,
           horas=c(9,2,8,11,2,3,5,10),
           sexo=c(0,0,1,0,1,1,0,1),
           "Rendimento previsto"=q8(novoaluno$horas,novoaluno$sexo))

###############################################################################################

