library(dplyr)

testar_var <- function(S, n, alpha = 0.05) {
    stopifnot(
        is.list(S),
        is.matrix(S[[1]]),
        is.numeric(n[[1]]),
        is.numeric(alpha),
        length(S) == length(n),
        length(alpha) == 1
    )
    ntotal <- sum(unlist(n))
    g <- length(n)
    p <- ncol(S[[1]])
    tmp <- sum(sapply(
        seq_along(n),
        \(i) 1 / (n[[i]] - 1) - 1 / (ntotal - g)
    ))
    u <- tmp * (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (g - 1))
    Sc <- lapply(
        seq_along(n),
        \(i) (n[[i]] - 1) * S[[i]] / (ntotal - g)
    ) |> Reduce(f = "+")
    tmp <- sum(sapply(
        seq_along(n),
        \(i) (n[[i]] - 1) * log(det(S[[i]]))
    ))
    M <- (ntotal - g) * log(det(Sc)) - tmp
    stat <- (1 - u) * M
    df <- p * (p + 1) * (g - 1) / 2
    quantil <- qchisq(1 - alpha, df)
    pvalor <- 1 - pchisq(stat, df)
    decisao <- ifelse(
        stat > quantil,
        "Rejeita a hipótese nula",
        "Não rejeita a hipótese nula"
    )
    cat("Teste para variâncias iguais\n")
    cat("H0: Todas as matrizes de covariâncias são iguais\n")
    cat(decisao, "\n")
    tibble(
        Estatística = stat,
        Quantil = quantil,
        p.valor = pvalor
    )
}
testar_var_dados <- function(X, alpha = 0.05) {
    stopifnot(
        is.list(X),
        is.matrix(X[[1]])
    )
    n <- sapply(X, nrow)
    S <- lapply(X, cov)
    testar_var(S, n, alpha)
}

testar_med_emparelhado <- function(dbar, Sd, n, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(Sd), is.numeric(dbar), is.numeric(n),
        is.numeric(alpha), is.character(tipo),
        ncol(Sd) == length(dbar),
        length(n) == 1, length(alpha) == 1,
        tipo %in% c("F", "f", "chi", "x")
    )
    p <- ncol(Sd)
    tmp <- t(dbar) %*% solve(Sd) %*% dbar
    if (tipo %in% c("F", "f")) {
        stat <- n * (n - p) / (p * (n - 1)) * as.numeric(tmp)
        quantil <- qf(1 - alpha, p, n - p)
        pvalor <- 1 - pf(stat, p, n - p)
    } else {
        stat <- n * as.numeric(tmp)
        quantil <- qchisq(1 - alpha, p)
        pvalor <- 1 - pchisq(stat, p)
    }
    decisao <- ifelse(
        stat > quantil,
        "Rejeita a hipótese nula",
        "Não rejeita a hipótese nula"
    )
    cat("Teste para média de amostras emparelhadas\n")
    cat("H0: mu_1 = mu_2\n")
    cat(decisao, "\n")
    tibble(
        Estatística = stat,
        Quantil = quantil,
        p.valor = pvalor
    )
}
testar_med_emparelhado_dados <- function(X1, X2, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(X1), is.matrix(X2),
        all(dim(X1) == dim(X2))
    )
    X <- X1 - X2
    dbar <- colMeans(X)
    Sd <- cov(X)
    n <- nrow(X)
    testar_med_emparelhado(dbar, Sd, n, tipo, alpha)
}

ics_med_emparelhado <- function(dbar, Sd, n, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(Sd), is.numeric(dbar), is.numeric(n),
        is.numeric(alpha), is.character(tipo),
        ncol(Sd) == length(dbar),
        length(n) == 1, length(alpha) == 1,
        tipo %in% c("F", "f", "chi", "x")
    )
    p <- ncol(Sd)
    if (tipo %in% c("F", "f")) {
        quantil <- qf(1 - alpha, p, n - p) / (n * (n - p) / (p * (n - 1)))
    } else {
        quantil <- qchisq(1 - alpha, p) / n
    }
    ics <- sapply(
        1:length(dbar),
        \(i) {
            err <- sqrt(quantil * Sd[i,i])
            dbar[i] + c(-err, err)
        }
    )
    tibble(
        Variável = 1:p,
        ICS_inferior = ics[1, ],
        ICS_superior = ics[2, ]
    )
}
ics_med_emparelhado_dados <- function(X1, X2, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(X1), is.matrix(X2),
        all(dim(X1) == dim(X2))
    )
    X <- X1 - X2
    dbar <- colMeans(X)
    Sd <- cov(X)
    n <- nrow(X)
    ics_med_emparelhado(dbar, Sd, n, tipo, alpha)
}

testar_med_var_igual <- function(xbar1, xbar2, S1, S2, n1, n2, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(S1), is.matrix(S2),
        is.numeric(xbar1), is.numeric(xbar2),
        is.numeric(n1), is.numeric(n2),
        is.numeric(alpha), is.character(tipo),
        ncol(S1) == length(xbar1),
        ncol(S2) == length(xbar2),
        length(n1) == 1, length(n2) == 1,
        length(alpha) == 1,
        tipo %in% c("F", "f", "chi", "x")
    )
    p <- ncol(S1)
    Sc <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
    tmp <- t(xbar1 - xbar2) %*% solve((1/n1 + 1/n2) * Sc) %*% (xbar1 - xbar2)
    if (tipo %in% c("F", "f")) {
        stat <- (n1 + n2 - p - 1) / (p * (n1 + n2 - 2)) * as.numeric(tmp)
        quantil <- qf(1 - alpha, p, n1 + n2 - p - 1)
        pvalor <- 1 - pf(stat, p, n1 + n2 - p - 1)
    } else {
        stat <- as.numeric(tmp)
        quantil <- qchisq(1 - alpha, p)
        pvalor <- 1 - pchisq(stat, p)
    }
    decisao <- ifelse(
        stat > quantil,
        "Rejeita a hipótese nula",
        "Não rejeita a hipótese nula"
    )
    cat("Teste para média de dois grupos com covariâncias iguais\n")
    cat("H0: mu_1 = mu_2\n")
    cat(decisao, "\n")
    tibble(
        Estatística = stat,
        Quantil = quantil,
        p.valor = pvalor
    )
}
testar_med_var_igual_dados <- function(X1, X2, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(X1), is.matrix(X2),
        ncol(X1) == ncol(X2)
    )
    xbar1 <- colMeans(X1)
    xbar2 <- colMeans(X2)
    S1 <- cov(X1)
    S2 <- cov(X2)
    n1 <- nrow(X1)
    n2 <- nrow(X2)
    testar_med_var_igual(xbar1, xbar2, S1, S2, n1, n2, tipo, alpha)
}

ics_med_var_igual <- function(xbar1, xbar2, S1, S2, n1, n2, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(S1), is.matrix(S2),
        is.numeric(xbar1), is.numeric(xbar2),
        is.numeric(n1), is.numeric(n2),
        is.numeric(alpha), is.character(tipo),
        ncol(S1) == length(xbar1),
        ncol(S2) == length(xbar2),
        length(n1) == 1, length(n2) == 1,
        length(alpha) == 1,
        tipo %in% c("F", "f", "chi", "x")
    )
    p <- ncol(S1)
    Sc <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
    if (tipo %in% c("F", "f")) {
        tmp <- (n1 + n2 - p - 1) / (p * (n1 + n2 - 2))
        quantil <- qf(1 - alpha, p, n1 + n2 - p - 1) / tmp * (1/n1 + 1/n2)
    } else {
        quantil <- qchisq(1 - alpha, p) * (1/n1 + 1/n2)
    }
    ics <- sapply(
        1:length(xbar1),
        \(i) {
            err <- sqrt(quantil * Sc[i,i])
            xbar1[i] - xbar2[i] + c(-err, err)
        }
    )
    tibble(
        Variável = 1:p,
        ICS_inferior = ics[1, ],
        ICS_superior = ics[2, ]
    )
}
ics_med_var_igual_dados <- function(X1, X2, tipo = "F", alpha = 0.05) {
    stopifnot(
        is.matrix(X1), is.matrix(X2),
        ncol(X1) == ncol(X2)
    )
    xbar1 <- colMeans(X1)
    xbar2 <- colMeans(X2)
    S1 <- cov(X1)
    S2 <- cov(X2)
    n1 <- nrow(X1)
    n2 <- nrow(X2)
    ics_med_var_igual(xbar1, xbar2, S1, S2, n1, n2, tipo, alpha)
}

testar_med_var_diff <- function(xbar1, xbar2, S1, S2, n1, n2, alpha = 0.05) {
    stopifnot(
        is.matrix(S1), is.matrix(S2),
        is.numeric(xbar1), is.numeric(xbar2),
        is.numeric(n1), is.numeric(n2),
        is.numeric(alpha),
        ncol(S1) == length(xbar1),
        ncol(S2) == length(xbar2),
        length(n1) == 1, length(n2) == 1,
        length(alpha) == 1
    )
    p <- ncol(S1)
    tmp <- t(xbar1 - xbar2) %*% solve(S1 / n1 + S2 / n2) %*% (xbar1 - xbar2)
    stat <- as.numeric(tmp)
    quantil <- qchisq(1 - alpha, p)
    pvalor <- 1 - pchisq(stat, p)
    decisao <- ifelse(
        stat > quantil,
        "Rejeita a hipótese nula",
        "Não rejeita a hipótese nula"
    )
    cat("Teste para média de dois grupos com covariâncias diferentes\n")
    cat("H0: mu_1 = mu_2\n")
    cat(decisao, "\n")
    tibble(
        Estatística = stat,
        Quantil = quantil,
        p.valor = pvalor
    )
}
testar_med_var_diff_dados <- function(X1, X2, alpha = 0.05) {
    stopifnot(
        is.matrix(X1), is.matrix(X2),
        ncol(X1) == ncol(X2)
    )
    xbar1 <- colMeans(X1)
    xbar2 <- colMeans(X2)
    S1 <- cov(X1)
    S2 <- cov(X2)
    n1 <- nrow(X1)
    n2 <- nrow(X2)
    testar_med_var_diff(xbar1, xbar2, S1, S2, n1, n2, alpha)
}

ics_med_var_diff <- function(xbar1, xbar2, S1, S2, n1, n2, alpha = 0.05) {
    stopifnot(
        is.matrix(S1), is.matrix(S2),
        is.numeric(xbar1), is.numeric(xbar2),
        is.numeric(n1), is.numeric(n2),
        is.numeric(alpha),
        ncol(S1) == length(xbar1),
        ncol(S2) == length(xbar2),
        length(n1) == 1, length(n2) == 1,
        length(alpha) == 1
    )
    p <- ncol(S1)
    quantil <- qchisq(1 - alpha, p)
    ics <- sapply(
        1:length(xbar1),
        \(i) {
            err <- sqrt(quantil * (S1[i,i] / n1 + S2[i,i] / n2))
            xbar1[i] - xbar2[i] + c(-err, err)
        }
    )
    tibble(
        Variável = 1:p,
        ICS_inferior = ics[1, ],
        ICS_superior = ics[2, ]
    )
}
ics_med_var_diff_dados <- function(X1, X2, alpha = 0.05) {
    stopifnot(
        is.matrix(X1), is.matrix(X2),
        ncol(X1) == ncol(X2)
    )
    xbar1 <- colMeans(X1)
    xbar2 <- colMeans(X2)
    S1 <- cov(X1)
    S2 <- cov(X2)
    n1 <- nrow(X1)
    n2 <- nrow(X2)
    ics_med_var_diff(xbar1, xbar2, S1, S2, n1, n2, alpha)
}


X1 <- matrix(c(8,8,8,7,8,9,8,8, 4,3,3,3,4,3,3,3), nrow = 8)
X2 <- matrix(c(8,8,7,6,7,8,8,7, 4,3,3,3,3,3,3,4), nrow = 8)
X <- X1 - X2

testar_var(list(cov(X1), cov(X2)), c(nrow(X1), nrow(X2)))
testar_var_dados(list(X1, X2))

testar_med_emparelhado(colMeans(X), cov(X), nrow(X))
testar_med_emparelhado_dados(X1, X2)

ics_med_emparelhado(colMeans(X), cov(X), nrow(X))
ics_med_emparelhado_dados(X1, X2)

testar_med_var_igual(colMeans(X1), colMeans(X2), cov(X1), cov(X2), nrow(X1), nrow(X2))
testar_med_var_igual_dados(X1, X2)

ics_med_var_igual(colMeans(X1), colMeans(X2), cov(X1), cov(X2), nrow(X1), nrow(X2))
ics_med_var_igual_dados(X1, X2)

testar_med_var_diff(colMeans(X1), colMeans(X2), cov(X1), cov(X2), nrow(X1), nrow(X2))
testar_med_var_diff_dados(X1, X2)

ics_med_var_diff(colMeans(X1), colMeans(X2), cov(X1), cov(X2), nrow(X1), nrow(X2))
ics_med_var_diff_dados(X1, X2)
