testar <- function(
        x, mu0 = 0, alpha = 0.05,
        região_crítica = "bilateral", tipo = "z"
) {
    x_crítico <- x_crítico(x, mu0, alpha, região_crítica, tipo)
    tibble(
        estatística = estatística(x, mu0, tipo),
        `p-valor`   = p_valor(x, mu0, região_crítica, tipo),
        `x-crítico` = paste0(
            "[",
            round(x_crítico[1], 3),
            ", ",
            round(x_crítico[2], 3),
            "]"
        ),
        `região crítica` = região_crítica,
        tipo
    ) |> mutate(across(where(is.numeric), \(x) round(x, 3)))
}

função_p <- function(e, x, tipo) {
    case_when(
        tipo == "z" ~ pnorm(e),
        tipo == "t" ~ pt(e, df = x["n"] - 1)
    )
}
função_q <- function(e, x, tipo) {
    case_when(
        tipo == "z" ~ qnorm(e),
        tipo == "t" ~ qt(e, df = x["n"] - 1)
    )
}

estatística <- function(x, mu0, tipo) {
    case_when(
        tipo == "z" ~ (x["m"] - mu0) / sqrt(x["v"] / x["n"]),
        tipo == "t" ~ (x["m"] - mu0) / sqrt(x["v"] / x["n"])
    )
}

p_valor <- function(x, mu0, região_crítica, tipo) {
    est <- estatística(x, mu0, tipo)
    p <- function(e) função_p(e, x, tipo)
    case_when(
        região_crítica == "bilateral" ~ 1 - abs(p(est) - p(-est)),
        região_crítica == "maior"     ~ 1 - p(est),
        região_crítica == "menor"     ~     p(est),
    )
}

quantil <- function(x, alpha, região_crítica, tipo) {
    q <- function(e) função_q(e, x, tipo)
    inf <- case_when(
        região_crítica == "bilateral" ~ q(alpha/2),
        região_crítica == "maior"     ~ q(alpha),
        região_crítica == "menor"     ~ -Inf
    )
    sup <- case_when(
        região_crítica == "bilateral" ~ q(1 - alpha/2),
        região_crítica == "maior"     ~ Inf,
        região_crítica == "menor"     ~ q(1 - alpha)
    )
    c(inf, sup)
}

x_crítico <- function(x, mu0, alpha, região_crítica, tipo) {
    quantil <- quantil(x, alpha, região_crítica, tipo)
    case_when(
        tipo == "z" ~ mu0 + sqrt(x["v"] / x["n"]) * -quantil[c(2,1)],
        tipo == "t" ~ mu0 + sqrt(x["v"] / x["n"]) * -quantil[c(2,1)]
    )
}