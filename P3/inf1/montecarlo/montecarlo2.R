Iterações <- 1000
Lambda <- 15

LogVerPoi <- function(lambda){
    fun <- -length(Amostra) * lambda + sum(Amostra) * log(lambda) - sum(log(factorial(Amostra)))
    return(-fun)
}

Analise <- function(Tamanho){
    Estimador <- NULL
    for(i in 1:Iterações){
        Amostra <<- rpois(n = Tamanho, lambda = Lambda)
        Modelo <- optimize(
            f = LogVerPoi,
            interval = c(0, 100)
        )
        Estimador[i] <- Modelo$minimum
    }

    Esperança = mean(Estimador)
    Viés = Esperança - Lambda
    Variância = var(Estimador) / Tamanho
    EQM = Variância + Viés^2

    data.frame(Tamanho, Esperança, Viés, Variância, EQM)
}

Analise(50)
Analise(100)
Analise(500)