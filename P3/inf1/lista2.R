# 8.2

q82 <- c(152, 115, 109, 94, 88, 137, 152, 77, 160, 165,
         125, 40, 128, 123, 136, 101, 62, 153, 83, 69)
q82mean <- mean(q82)
q82sqmean <- sum(q82^2) / length(q82)
q82beta <- q82sqmean / q82mean - q82mean
q82alpha <- mean(q82) / q82beta

# Exercício

# 1.

q1 <- c(9.9, 13.19, 7.1, 7.54, 11.01, 8.81, 10.63, 7.83,
        8.68, 6.27, 8.20, 11.12, 9.62, 11.58, 9.64, 8.44,
        7.1, 6.76, 9.2, 9.53, 11.67, 7.59, 9.3, 6.25,
        6.01, 12.44, 12.13, 11.29, 11.57, 6.83)
mean(q1)

vnorm <- function(mu, dados){
    dnorm(mean(dados), mean = mu, sd = 2, log = TRUE)
}
mu <- seq(min(q1), max(q1), l = 100)
logvero <- sapply(mu, vnorm, dados = q1)
plot(mu, logvero, type = "l")
abline(v = mean(q1))

