# 1. --------------------------------------------
qbinom(0.95, 16, 0.1)
pbinom(6, 16, 0.1, lower.tail = FALSE)
1 - pbinom(6 - 1, 16, 0.1)

# região crítica: x > 4
# pvalor: 0.003296
# rejeita p = 0.1

# 2. --------------------------------------------
pbinom(1, 20, 0.2) + 1 - pbinom(8 - 1, 20, 0.2)
2 * (1 - pbinom(9 - 1, 20, 0.2))

# alpha: 0.1013
# pvalor: 0.01996
# rejeita p = 0.2

# 3. --------------------------------------------
q3 <- c(
    1.80, 2.25, 2.50, 2.70, 2.75,
    3.00, 3.10, 3.25, 3.30, 3.55, 5.65
)
q3_d <- q3 - 3.5
q3_r <- rank(abs(q3_d)) * (2 * (q3_d >= 0) - 1)
q3_r[which(q3_r >= 0)] |> sum()
q3_r[which(q3_r < 0)] |> sum()
wilcox.test(q3, mu = 3.5)

# estatística: 12
# quantil: 11
# não rejeita mediana = 3.5

# 4. --------------------------------------------
q4 <- c(
    99, 100, 90, 94, 135, 108, 107, 111,
    119, 104, 127, 109, 117, 105, 125
)
q4_d <- q4 - 107
q4_r <- rank(abs(q4_d)) * (2 * (q4_d >= 0) - 1)
q4_r[which(q4_r >= 0)] |> sum()
q4_r[which(q4_r < 0)] |> sum()
wilcox.test(q4, mu = 107)

# quantil: 46.5
# estatística: 25
# não rejeita mediana = 107

# 5. --------------------------------------------
chisq.test(
    c(58, 44, 34, 38, 26)
)
qchisq(0.99, 4)
pchisq(14.4, 4, lower.tail = FALSE)

# estatística: 14.4
# quantil: 13.27
# pvalor: 0.006122
# rejeita p1 = p2 = p3 = p4

# 6. --------------------------------------------
chisq.test(
    c(214, 231, 182, 154, 219)
)
qchisq(0.99, 4)
pchisq(19.79, 4, lower.tail = FALSE)

# estatística: 19.79
# quantil: 13.27
# pvalor: 0.0005494
# rejeita p1 = p2 = p3 = p4

# 7. --------------------------------------------
q7 <- c(7, 14, 18, 23, 22, 9, 3, 4)
fn <- Reduce(`+`, q7, accumulate = TRUE) / sum(q7)
fs <- ppois(0:7, 3)
max(abs(fn - fs))
ks.test(
    q7,
    ppois, lambda = 3
)

# estatística: 0.03319
# quantil: 0.136
# não rejeita x ~ pois

# 8. --------------------------------------------
q8 <- sort(c(
    12.0, 10.5, 13.1, 11.9, 10.4,
    12.8, 11.2, 11.4, 12.2, 11.9
))
fn <- Reduce(`+`, q8, accumulate = TRUE) / sum(q8)
fs <- pnorm(q8, mean(q8), sd(q8))
max(abs(fn - fs))

max(abs(dplyr::lag(fn) - fs), na.rm = TRUE)
max(abs(fn - dplyr::lag(fs)), na.rm = TRUE)

# estatística: 0.09986
# quantil: 0.41
# não rejeita x ~ norm

# 9. --------------------------------------------
q9 <- c(
    12.4, 14.2, 11.7, 14.0, 12.7, 15.7, 12.8,
    14.1, 17.9, 18.4, 17.5, 20.2, 20.8, 20.3
)
tseries::runs.test(as.factor(q9 > median(q9)))

# estatística: 5
# quantil: [3,13]
# não rejeita aleatoriedade

# 10. -------------------------------------------

# estatística: 8
# quantil: [6,17]
# não rejeita aleatoriedade
