v<-c(1,8,7,5,4,3,11,9,12,8)
u<-c(1,8,5,3,8,0,4,6,8,1)
sumstudy=function(u,v)
{study<-c(sum(u), sum(v), sum(u^2), sum(v^2), 
          sum(u)^2, sum(v)^2, sum(u*v))
nome<-c("Soma de u","Soma de v","Soma de quadrados de u",
        "Soma de quadrados de v","Quadrado da soma de u",
        "Quadrado da soma de v","Soma dos produtos cruzados")
return(data.frame(nome,study))}
sumstudy(u,v)


varstudy=function(x)
{vstudy<-c(mean(x),median(x),(3*median(x)-2*mean(x)),var(x),sd(x),
           (sd(x)/mean(x)),((3*mean(x)-3*median(x))/sd(x)),
           ((sqrt(length(x))/(length(x)-1))*(1/(length(x)-1))*
              (1/(sd(x)^3))*(sum((x-mean(x))^3))),0,
           (((length(x))/(length(x)-1)^2)*(1/(var(x)^2))*
              (sum((x-mean(x))^4))))
vnome<-c("Média","Mediana","Moda","Variância","Desvio Pardão",
         "Coeficiente de variação","Assimetria 1","Assimetria 2",
         "Curtose 1","Curtose 2")
return(data.frame(vnome,vstudy))}
varstudy(v)


binome=function(a,b)
{return(ifelse(a<b, "Impossível calcular (a<b)",
        factorial(a)/(factorial(b)*factorial(a-b))))}
binome(6,5)
binome(5,6)


idade<-c(31,32,33,45,49,51,43,22,19,95)
sexo<-as.factor(c(rep("Masc",5),rep("Fem",5)))
tapply(idade,sexo,mean)
cv=function(x){sd(x)/mean(x)}
tapply(idade,sexo,cv)


sexo1=as.factor(c(rep("Masc",11),rep("Fem",22)))
tab1=table(sexo1)
barplot(tab1,
        main="Distribuição dos intrevistados segundo o sexo",
        xlab="Sexo",ylab="Frequência",
        col=c("pink","cyan"),
        ylim=c(0,30))
bebe=as.factor(c(rep("Sim",25),rep("Não",8)))
tab2=table(bebe)
barplot(tab2)
crosstab(sexo1,bebe)

altura=seq(150,210,1)
hist(altura,
     main="Distribuição dos intrevistados segundo altura",
     ylim=c(0,15),
     col="steelblue4",
     cex.main=1,cex.axis=1,cex.lab=1,
     xlab="Altura",ylab="Frequência")

pie(tab1,
    col=c("pink","cyan"))


help.search("mean")
?mean
mean(altura)
mean(altura,0.1)

y=c(4.0,8.0,15.0,22.6,36.4,45.3,60.0)
x=c(0,0.6,1.2,1.5,1.8,2.1,2.4)
banco=data.frame(Y=y,X=x,LOG10Y=log10(y))

plot(x,y)
plot(x,log10(y))
cor(x,y)
cor(x,log10(y))

reg1=lm(y~x)
reg2=lm(log10(y)~x)

reg1$fitted.values
reg2$fitted.values
10^reg2$fitted.values

banco1=cbind(y,reg1$fitted.values,reg2$fitted.values,
             10^(reg2$fitted.values))
banco1


x2=seq(1:7)+3
banco2=data.frame(y,x,x2)

reg3=lm(y~x+x2)
summary(reg3)

reg4=lm(log10(y)~x+x2)
summary(reg4)

attach(prob)
tapply(nota,turma,mean)
tapply(nota,turma,var)
