numero=seq(1,32)
dieta=c("B","A","A","B","A","B","B","A","B","A","A","B","A","A","B","B",
        "A","B","B","A","B","A","A","B","A","B","B","A","B","B","A","A")
peso=c(115.10,90.40,97,102.50,108.10,118,110.50,103.70,115.90,
       96.10,95,120.10,98,99.50,111.10,105,90.10,117.10,115.60,
       97.10,114.90,96,95,117.50,96.70,117.10,117.90,95.40,116.90,
       116.80,95.90,97.50)
tamanho=c(1.15,1.17,1.61,1.57,1.56,1.65,1.45,1.38,1.45,1.43,1.45,1.60,
          1.49,1.39,1.38,1.47,1.56,1.65,1.45,1.43,1.45,1.60,1.49,1.17,
          1.61,1.57,1.56,1.65,1.15,1.17,1.61,1.57)
raca=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,4,4,4,4,3,3,3,3,2,2,2,2,1,1,1,1)
sexo=c("Macho","F?mea","F?mea","Macho","F?mea","Macho","Macho","F?mea",
       "F?mea","Macho","Macho","F?mea","Macho","Macho","F?mea","F?mea",
       "F?mea","Macho","Macho","F?mea","Macho","F?mea","F?mea","Macho",
       "Macho","F?mea","F?mea","Macho","F?mea","F?mea","Macho","Macho")

getmode=function(v){
  uniqv=unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]}

cv=function(x){
  sd(x)/mean(x)}

asymmetry=function(x){
  (sqrt(length(x)/(length(x)-1)))*(1/(length(x)-1))*
    ((sum((x-mean(x))^3))/sd(x)^3)}

kurtosis=function(x){
  ((length(x)/(length(x)-1)^2)*((sum((x-mean(x))^4))/sd(x)^4))-3}

########################################################################

# a) numero:  qualitativa ordinal
#    dieta:   qualitativa nominal
#    peso:    quantitativa cont?nua
#    tamanho: quantitativa cont?nua
#    raca:    qualitativa nominal
#    sexo:    qualitativa nominal

########################################################################

# b) mean(peso) = 105.7344

mean(peso)

########################################################################

# c) tapply(peso,raca,mean) =   1         2        3        4 
#                               104.0125  108.4250 106.3125 104.1875

tapply(peso,raca,mean)

# d) tapply(peso,sexo,mean) =   F?mea     Macho 
#                               106.1375  105.3312

tapply(peso,sexo,mean)

# e) tapply(peso,dieta,mean) =  A         B 
#                               96.96875  114.50000 

tapply(peso,dieta,mean)

########################################################################

# f) tapply(peso,raca,median) =   1       2       3       4 
#                                 100.00  109.30  105.50  102.25

tapply(peso,raca,median)

# g) tapply(peso,sexo,median) =   F?mea   Macho 
#                                 106.55  101.00 

tapply(peso,sexo,median)

# h) tapply(peso,dieta,median) =  A       B
#                                 96.40   116.35 

tapply(peso,dieta,median)

########################################################################

# i) tapply(peso,raca,getmode) =  1     2     3     4 
#                                 115.1 108.1 95.0  98.0

tapply(peso,raca,getmode)

# j) tapply(peso,sexo,getmode) =  F?mea Macho 
#                                 90.4  115.1 

tapply(peso,sexo,getmode)

# k) tapply(peso,dieta,getmode) = A     B 
#                                 95.0  117.1

tapply(peso,dieta,getmode)

########################################################################

# l) tapply(peso,raca,sd) =   1         2       3         4
#                             10.67392  9.18519 11.63436  9.66857 

tapply(peso,raca,sd)

# m) tapply(peso,sexo,sd) =   F?mea     Macho 
#                             10.67981  9.58246

tapply(peso,sexo,sd)

# n) tapply(peso,dieta,sd) =  A         B 
#                             4.32384   4.85963

tapply(peso,dieta,sd)

########################################################################

# o) Ra?a 3, pois poss?i menor diverg?ncia de sua m?dia em
#    compara??o com a m?dia global.

mean(peso)
tapply(peso,raca,mean)
tapply(peso,raca,mean) - mean(peso)
abs(tapply(peso,raca,mean) - mean(peso))
min(abs(tapply(peso,raca,mean) - mean(peso)))
which.min(abs(tapply(peso,raca,mean) - mean(peso)))

########################################################################

# p) Sim, pois s?o duas vari?veis diferentes e de baixa correla??o.

cv(peso)
cv(tamanho)
cor(peso,tamanho)

########################################################################

# q) tapply(peso,raca,cv) =     1       2       3       4
#                               0.10262 0.08471 0.10943 0.09279
#
#    tapply(tamanho,raca,cv) =  1       2       3       4
#                               0.16761 0.06105 0.09191 0.06104

tapply(peso,raca,cv)
tapply(tamanho,raca,cv)

# r) tapply(peso,sexo,cv) =     F?mea   Macho 
#                               0.10062 0.09097
#
#    tapply(tamanho,sexo,cv) =  F?mea   Macho 
#                               0.11002 0.10379

tapply(peso,sexo,cv)
tapply(tamanho,sexo,cv)

# s) tapply(peso,dieta,cv) =    A       B 
#                               0.04459 0.04244
#    tapply(tamanho,dieta,cv) = A       B 
#                               0.08233 0.12497

tapply(peso,dieta,cv)
tapply(tamanho,dieta,cv)

########################################################################

# t) A Ra?a 1 e a dieta B oferecem maior varia??o de tamanho.

########################################################################

# u) asymmetry(peso) =     0.02218387
#    asymmetry(tamanho) = -0.9014667
#
#    kurtosis(peso) =     -1.61463
#    kurtosis(tamanho) =  -0.2003604

asymmetry(peso)
asymmetry(tamanho)

kurtosis(peso)
kurtosis(tamanho)

########################################################################

# v) Os valores do tamanho se concentram a direita comparados com
#    o peso.

par(mfrow=c(2,1))
invisible(boxplot(peso,horizontal=TRUE))
invisible(boxplot(tamanho,horizontal=TRUE))

# w) Em rela??o ao peso, ra?a 3 poss?i peso um pouco inferiores e a 
#    ra?a 4 poss?i menor varia??o comparado com as demais.
#    Em rela??o ao tamanho, a ra?a 1 tem a maior vari?ncia, cobrindo 
#    todo o espectro, enquanto as ra?as 2 e 3 tem tamanhos maiores
#    que as da ra?a 4.

par(mfrow=c(1,4))
invisible(tapply(peso,raca,boxplot))
invisible(tapply(tamanho,raca,boxplot))

# x) Os porcos machos um outlier baixo de tamanho.

par(mfrow=c(1,2))
invisible(tapply(peso,sexo,boxplot))
invisible(tapply(tamanho,sexo,boxplot))

# y) A dieta B oferece maior peso, enquanto a dieta A menor vari?ncia
#    do tamanho.

par(mfrow=c(1,2))
invisible(tapply(peso,dieta,boxplot))
invisible(tapply(tamanho,dieta,boxplot))
par(mfrow=c(1,1))

########################################################################

# z) N?o pois os valores est?o muito espalhados.

plot(peso,tamanho)

# aa) Baix?ssima correla??o, cor(peso,tamanho) = -0.2018518

cor(peso,tamanho)

# bb) 

lm(peso~tamanho)

# cc) reg(novoporco) = 105.4149 101.6478 101.2581 108.2727 106.7139

reg=function(x){
  124.77-12.99*x}

novoporco=c(1.49,1.78,1.81,1.27,1.39)

reg(novoporco)

# dd)

cor(peso[sexo=="Macho"],tamanho[sexo=="Macho"])
cor(peso[sexo=="F?mea"],tamanho[sexo=="F?mea"])

lm(peso[sexo=="Macho"]~tamanho[sexo=="Macho"])
lm(peso[sexo=="F?mea"]~tamanho[sexo=="F?mea"])

# ee)

lm(peso[sexo=="Macho"]~tamanho[sexo=="Macho"])$coefficients
lm(peso[sexo=="F?mea"]~tamanho[sexo=="F?mea"])$coefficients

regm=function(x){
  136.04626-20.70093*x}
regf=function(x){
  114.127034-5.521924*x}

regm(c(1.49,1.78))
regf(c(1.81,1.27,1.39))