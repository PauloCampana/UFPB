paciente <- c(1:10)
ureia <- c(90,105,99,100,95,94,88,245,110,94)
ureia
sort(ureia)
sort(ureia, decreasing = TRUE)
rank(ureia)

ureia.cre <- sort(ureia)
ureia.dec <- sort(ureia, decreasing = TRUE)

sqrt(ureia)

efofeks <- function(x) {
  exp(log((x+1)))/(length(x)^(1/3))
}
efofeks(ureia)

col.c <- c(340, 460, 280, 320, 220)
col.t <- c(180, 220, 200, 240, 260)
col.total <- c(col.c, col.t)

mean(col.c)
mean(col.t)
mean(col.total)

col.tab <- data.frame("Medida" = c("Mínimo", "Média", "Máximo"),
                      "Grupo C" = c(min(col.c), mean(col.c), max(col.c)),
                      "Grupo T" = c(min(col.t), mean(col.t), max(col.t)),
                      "Global" = c(min(col.total), mean(col.total), max(col.total)))
col.tab

bil <- c(0.4, 0.5, 0.5, 0.7, 0.2, 0.5, 0.6, 0.3, 0.8, 0.4, 0.3, 0.4, 0.7, 0.9, 0.5,
         0.9, 0.8, 1.0, 1.0, 0.9)

sort(bil)
range(bil)
max(bil) - min(bil)
bil[7]
bil[3] <- 0.2
is.numeric(bil)

A <- matrix(c(3, -1, 2, -2, 3, 1, 1, 4, 1, 4, 0, 3, 0, 4, 0, 3), nrow=4,ncol=4, byrow=TRUE)
B <- matrix(c(4, 0, 3, 0, 4, 1, 3, 1, 2, 4, 0, 3, 6, 4, 0, 3), nrow=4,ncol=4, byrow=TRUE)
t(B)
det(B)
solve(B)
c <- A+B
C
D <- A%*%B
D
A
colSums(A[,2:3])
rowSums(A[1:4,])
A[4,1]
A[3,3] - A[1,3] - A[c(1:4),c(2,3)] 
A[1,1] <- 99
A
A[-1]
A[-1]
A[-1,]

cbind(paciente, ureia)
rbind(paciente, ureia)
sexo <- c(rep("M",5), rep("F",5))
sexo
cbind(paciente, ureia, sexo)
rbind(paciente, ureia, sexo)

adf <- read.delim2("~/R/paae/a.txt")
names(adf)
adf
fix(adf)
adf[1,1]
head(adf,5)
tail(adf,5)
adf$Q_3
adf$Q_2
adf$Q_4
mean(adf$Q_2)
mean(adf$Q_3)
mean(adf$Q_4)
mean(adf$Q_4, na.rm = TRUE)
summary(adf$Q_4)

names(adf) <- c("nq", "idade", "sexo", "cre", "trabalha", "filhos",
                   "em", "ef","horas", "n_livros", "biblio", "inglesa")
adf
indice <- adf$cre/adf$horas
adf <- cbind.data.frame(adf, indice)
adf
adf <- rbind.data.frame(adf, c(24,31,"m",7.77, "s","n",2,2,12,2,"c","b",7.77/12))
adf
adf[-24,]
adf2 <- adf[order(adf$cre, decreasing = TRUE),]
adf2

write.table(adf, file = "adf.txt", sep = " ", na = "NA", dec = ".",
            col.names = TRUE, quote=FALSE)

x<-c(1,2,9,4,5)
y<-c(1,2,6,7,8)
x>y
x>=y
x<y
x==y
x!=y

adf$idade > 25
adf$horas <= 10
adf$cre >= 8.3
1 * (adf$cre >= 8.3)

which(adf$idade > 25)
adf$sexo[c(which(adf$idade > 25))]
ifelse(adf$cre>=7,"Aprovado", "Reprovado")

conceito <- ifelse(adf2$cre>=7, "Excelente", "Insuficiente")
conceito
adf2 <- cbind.data.frame(adf2, conceito)
names(adf2)
write.table(adf2, file = "adf2.txt", sep = " ", na = "NA", dec = ".",
            col.names = TRUE, quote=FALSE)

adf[adf$trabalha == "s", c("idade", "cre", "horas")]
adf[adf$trabalha == "s" & adf$idade > 25, 1:4]
adf[adf$horas > 25 | adf$cre > 9,]
adf[adf$trabalha == "s" & adf$cre < 7 & adf$filhos == "s",]
horas.cat <- ifelse(adf$horas >= 10, "Desejavel", "Insuficiente")
horas.cat
adf

library(haven)
educ <- read_sav("Educ.sav")
educ
educ$Teacher <- as_factor(educ$Teacher)
table(educ$Teacher)
educ$Gender <- as_factor(educ$Gender)
table(educ$Gender)
educ$Ethnic<- as_factor(educ$Ethnic)
table(educ$Ethnic)
educ$Freeredu <- as_factor(educ$Freeredu)
table(educ$Freeredu)

which(educ$Score > 70)

educ$Teacher[which(educ$Score > 70)]

table(educ$Teacher[which(educ$Score > 70)])

adf2$sexo <- factor(adf2$sexo, levels = c("m", "f"), labels = c("Masculino", "Feminino"))
adf2$trabalha <- factor(adf2$trabalha, levels = c("n", "s"), labels = c("Não", "Sim"))
adf2$filhos <- factor(adf2$filhos, levels = c("n", "s"), labels = c("Não", "Sim"))
adf2$em <- factor(adf2$em, levels = c("1", "2"), labels = c("Não", "Sim"))
adf2$ef <- factor(adf2$ef, levels = c("1", "2"), labels = c("Não", "Sim"))
adf2$inglesa <- factor(adf2$inglesa, levels = c("a", "b", "c", "d", "e"),
                       labels = c("Excelente", "Bom", "Razoável", "Ruim", "Péssimo"))
adf2$biblio <- factor(adf2$biblio, levels = c("a", "b", "c", "d", "e"),
                       labels = c("Excelente", "Bom", "Razoável", "Ruim", "Péssimo"))


mode(adf2$cre)
adf2$cre <- as.numeric(adf2$cre)
adf2$horas <- as.numeric(adf2$horas)
adf2$indice <- adf2$cre/adf2$horas

pie(table(adf2$sexo), col = c(4:5), main = "Titulo", clockwise = TRUE, radius = 1)
barplot(table(adf2$biblio), col = (1:5))

levels(adf2$biblio)[median(as.numeric(adf2$biblio))]

table(adf2$n_livros)
barplot(table(adf2$n_livros), col = 2)
plot(table(adf2$n_livros), col = 2)
plot(cumsum(prop.table(table(adf2$n_livros))), type = "S")

quantile(adf2$horas, seq(0, 1, 0.1), na.rm = TRUE)

cv <- function(x){
  sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
}

cv(adf2$cre)

table(cut(adf2$cre, seq(min(adf2$cre, na.rm = T),
                        max(adf2$cre, na.rm = T),
                        nclass.Sturges(adf2$cre))))

hist(adf2$cre, col = "blue")    

par(mfrow = c(1,2))
tapply(adf2$cre, adf2$sexo, hist)
par(mfrow = c(1,1))

x <- rchisq(100000, df = 4)

hist(x, freq = FALSE, col = "grey", ylim = c(0, 0.2))
curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)

y <- rnorm(1000, 200, 30)
qqnorm(y)
qqline(y, col = 2)
ks.test(y, mean(y), sd(y))


t(prop.table(table(adf2$sexo, adf2$trabalha)))
barplot(t(prop.table(table(adf2$sexo, adf2$trabalha))))

library(oii)
association.measures(adf2$sexo, adf2$trabalha)

adf2$biblio
as.numeric(adf2$biblio)

library(irr)
medico1 <- c(0,0,0,1,1,0,1,2,2,2,2,1,0,0,0,1,1,2,1,1)
medico2 <- c(0,0,1,0,1,0,2,2,1,2,2,1,0,0,1,1,1,2,2,1)
dadoskappa <- cbind(medico1, medico2)
dadoskappa
kappa2(dadoskappa)

medico3 <- c(0,0,0,0,1,0,2,2,1,2,2,1,0,0,2,1,1,2,2,1)
kappam.fleiss(cbind(medico1, medico2, medico3))
table(medico3, medico2)

fono1 <- c(0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,5,5)
fono2 <- c(0,1,0,2,0,1,1,2,1,2,2,2,2,3,3,3,3,4,4,4,4,5)
fono3 <- c(0,0,0,1,1,1,1,1,2,2,2,4,2,3,2,3,3,4,5,4,5,5)
kappam.fleiss(cbind(fono1, fono2, fono3))

avaliador1 <- c("Bom","Bom","Bom","Bom","Bom","Bom","Regular","Regular","Regular","Ruim")
avaliador2 <- c("Bom","Bom","Bom","Bom","Bom","Regular","Bom","Regular","Regular","Regular")
kappa2(cbind(avaliador1, avaliador2))
kappam.fleiss(cbind(avaliador1, avaliador2))

cre.cat <- cut(adf2$cre, quantile(adf2$cre, na.rm = TRUE), include.lowest = TRUE)
cre.cat
table(cre.cat)
table(cre.cat, adf2$sexo)

tapply(adf2$cre, adf2$sexo, mean, na.rm = TRUE)
tapply(adf2$cre, adf2$sexo, median, na.rm = TRUE)
tapply(adf2$cre, adf2$sexo, sd, na.rm = TRUE)

idade.cat <- cut(as.numeric(adf2$idade), quantile(as.numeric(adf2$idade), na.rm = TRUE),
                 include.lowest = TRUE)
table(cre.cat, idade.cat)

write.table(adf, file = "adf2.txt", sep = " ", na = "NA", dec = ".",
            col.names = TRUE, quote=FALSE)

adf2$idade <- as.numeric(adf2$idade)

cor(adf2$cre, adf2$idade)
cor.test(adf2$cre, adf2$idade)

plot(adf2$horas, adf2$cre, pch = 24)
cor.test(adf2$horas, adf2$cre)

dbinom(8:10, 10, 0.2)
sum(dbinom(8:10, 10, 0.2))

prob <- dbinom(0:10, 10, 0.2)
prob
plot(0:10, prob, type = "h")
plot(0:10, dbinom(0:10, 10, 0.5), type = "h")

sum(dpois(0:2, 1.5))
ppois(2, 1.5)


