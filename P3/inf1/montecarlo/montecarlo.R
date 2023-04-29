set.seed(2022)

# Simulação de Monte Carlo com 1000 Iterações para encontrar Estimador para Lambda
# de uma Amostra aleatória de diferentes Tamanhos com distribuição Poisson.

# Inicialmente, analisar para n = 50

Iterações <- 1000                                       # Iterações da Simulação
Lambda <- 15                                            # Pârametro Lambda da população
Tamanho <- 50                                           # Tamanho da Amostra

# Função de log verossimilhança, negativa pois o optimize() precisa achar ponto de mínimo
LogVerPoi <- function(lambda){
    fun <- -length(Amostra) * lambda + sum(Amostra) * log(lambda) - sum(log(factorial(Amostra)))
    return(-fun)
}

# Vetor com os resultados da simulação
Estimador <- NULL

# Loop da Simulação
for(i in 1:Iterações){
    Amostra <- rpois(n = Tamanho, lambda = Lambda)      # Amostra aleatória Poisson
    Modelo <- optimize(                                 # Achar ponto de mínimo
        f = LogVerPoi,                                  # da função LogVerPoi
        interval = c(0, 100)                            # para lambda entre (0, 100)
    )
    Estimador[i] <- Modelo$minimum                      # extrair o ponto de mínimo
}

# Analisando o Estimador
Esperança = mean(Estimador)                             # E(θ^)   = E(X)
Viés = Esperança - Lambda                               # B(θ^)   = E(X) - λ
Variância = var(Estimador) / Tamanho                    # Var(θ^) = Var(X)/n
EQM = Variância + Viés^2                                # EQM(θ^) = Var(X)/n + B^2

data.frame(Tamanho, Esperança, Viés, Variância, EQM)