# Amostragem

# população objeto
#   
# característica populacional
# unidade amostral
# amostra
# erro amostral
# censo

# Amostragem não probabilística
#   por acessibilidade ou conveniência
#   intencional
#   por cotas

# Amostragem probabilística
#   aleatória simples
#     sorteio aleatório
#   sistemática
#     4, 14, 24, 34...
#   estratificada
#     divisão da população por grupos de característica comum
#     uniforme, proporcional, ótima
#   por conglomerados
#     divisão de grupos heterogêneos

# amostra piloto

# Fatores que determinam o tamanho da amostra
#   nível de confiança (1-a)
#   erro
#   variabilidade
# n = (Z_(1-a/2) * sigma/E)^2

# grau ed confiança
# a
# valor crítico

# ex1 
# E = 500
# sigma = 6250
# z = 1.96
#   n = (1.96 * 6250/500)^2 = 600.25 ~ 601

# ex2
# E = 50
# sigma = 400
# z = 1.96
#   n = (1.96 * 400/50)^2 =~ 246
#   n = (1.96 * 400/25)^2 =~ 984

# estimativa da proporção populacionaç
# n = (z^2_1-a/2 pq/E^2)

# ex3
# z = 1.64
# E = 0.05
# p = ? (assume 0.5 worst case)
#   n = (1.64^2 * 0.5^2/0.05^2) = 269

# z = qnorm(1-alpha/2)

# para populações finitas

# N-n/N-1
# 1000-269/999

# n = (N * sigma^2 * (Z_a/2)^2)/((N-1) * E^2 + sigma^2 * (Z_a/2)^2)
# n = (1000 * 0.5^2 * 1.64^2)/(999 * 0.05^2 + 0.5^2 * 1.64^2) = 212


# Parâmetro - para população
# Estatística - para amostra
#   proporção, média, variância
# são VA e possuem distribuição

# se N >= 30, distribuição de X_ é normal

y <- matrix(rnorm(20000, 100, 30), ncol = 2000)
ybar <- apply(y, MARGIN = 2, mean)
mean(ybar)
sd(ybar)
30/sqrt(10)
hist(ybar)

z <- matrix(rchisq(60000, 1, ncp = 100), ncol = 2000)
zbar <- apply(z, MARGIN = 2, mean)
mean(zbar)
sd(zbar)
100/sqrt(30)
hist(zbar, prob = T)

# Binomial

# ^p ~ N(p, p(1-p)/n)
# z = (^p - p)/(sqrt(r(1-p)/n)) ~ N(0, 1)
# E(^p) = p
# var(^p) = p(1-p)/n

# se np e n(1-p) >= 5, pode ser aproximado pela normal

