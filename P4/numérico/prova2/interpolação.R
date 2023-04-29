Lagrange <- function(data) {
    n <- nrow(data)
    P <- function(x) {
        L <- function(x, i) {
            prod(sapply(
                1:n,
                function(n) {
                    if (n == i) 1
                    else (x - data[[1]][n]) / (data[[1]][i] - data[[1]][n])
                }
            ))
        }
        sum(sapply(
            1:n,
            function(n) data[[2]][n] * L(x, n)
        ))
    }
    Vectorize(P)
}

diff_div <- function(v, data) {
    n <- length(v)
    if (n == 1) data[[2]][which(data[[1]] %in% v)]
    else (diff_div(v[-1], data) - diff_div(v[-n], data)) / (v[n] - v[1])
}

Newton <- function(data) {
    n <- nrow(data)
    a <- sapply(
        1:n,
        function(n) diff_div(data[[1]][1:n], data)
    )
    P <- function(x) {
        poli <- function(x, i) {
            prod(sapply(
                1:i,
                function(n) {
                    if (n == 1) 1
                    else x - data[[1]][n-1]
                }
            ))
        }
        sum(sapply(
            1:n,
            function(n) a[n] * poli(x, n)
        ))
    }
    Vectorize(P)
}

plot_interp <- function(data, método) {
    ggplot(data, aes(pull(data, 1), pull(data, 2))) +
        geom_function(
            fun = método(data),
            color = "#00c060", n = 1000
        ) +
        geom_point(size = 3, color = "#008040")
}