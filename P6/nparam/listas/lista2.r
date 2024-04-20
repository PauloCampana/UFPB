# 1. ---------------------------------------------
qchisq(0.95, 1)
pchisq(0.4444, 1, lower.tail = FALSE)

# estatística: 0.4444
# quantil: 3.841
# pvalor: 0.505
# não rejeita o não-efeito da campanha

# 2. ---------------------------------------------
q2 <- data.frame(
    radial = c(
        26.5, 14.3, 12.7, 20.2,
        15.1, 16.9, 23.4, 16.4
    ),
    common = c(
        25.8, 14.5, 12.1, 19.9,
        15.1, 15.8, 23.0, 16.0
    )
)
q2_d <- q2$radial - q2$common
q2_d <- q2_d[which(q2_d != 0)]
2 * (q2_d <= 0) - 1
2 * pbinom(1, length(q2_d), 0.5)

# estatística: 1
# pvalor: 0.125
# não rejeita mediana_diff = 0

# 3. ---------------------------------------------
q3 <- data.frame(
    old = c(18, 15, 19, 23, 12, 16, 28, 18),
    new = c(24, 14, 22, 28, 16, 20, 20, 18)
)
q3_d <- q3$old - q3$new
q3_d <- q3_d[which(q3_d != 0)]
2 * (q3_d <= 0) - 1
2 * pbinom(2, length(q3_d), 0.5)

# estatística: 2
# pvalor: 0.4531
# não rejeita mediana_diff = 0

# 4. ---------------------------------------------
q4 <- data.frame(
    before = c(
        3, 4, 2, 1, 3, 6, 4, 5, 2, 0, 2, 5, 3, 3
    ),
    after = c(
        2, 1, 0, 1, 1, 3, 3, 2, 2, 2, 3, 4, 3, 2
    )
)
q4_d <- q4$before - q4$after
q4_d <- q4_d[which(q4_d != 0)]
2 * (q4_d <= 0) - 1
pbinom(2, length(q4_d), 0.5)

# estatística: 2
# pvalor: 0.03271
# rejeita mediana_diff = 0

# 5. ---------------------------------------------
q5 <- data.frame(
    trad = c(6, 7, 14, 10, 5, 13, 11, 12, 15, 18),
    piaget = c(5, 11, 13, 19, 15, 12, 17, 13, 16, 9)
)
q5_d <- q5$trad - q5$piaget
q5_d <- q5_d[which(q5_d != 0)]
2 * (q5_d <= 0) - 1
pbinom(6, length(q4_d), 0.5)

# estatística: 6
# pvalor: 0.03271
# rejeita mediana_diff = 0

# 6. ---------------------------------------------
q5 <- matrix(c(37, 20, 48, 40, 16, 39), 2)
q5_e <- apply(q5, 1, sum) %*% t(apply(q5, 2, sum)) / sum(q5)
sum((q5_e - q5)^2 / q5_e)
qchisq(0.95, 2)
pchisq(15.39, 2, lower.tail = FALSE)

# estatística: 15.39
# quantil: 5.991
# pvalor: 0.000455
# rejeita independência

# 7. ---------------------------------------------
q6 <- matrix(c(40, 15, 10, 85), 2)
q6_e <- apply(q6, 1, sum) %*% t(apply(q6, 2, sum)) / sum(q6)
sum((q6_e - q6)^2 / q6_e)
qchisq(0.99, 1)
pchisq(60.65, 2, lower.tail = FALSE)

# estatística: 60.65
# quantil: 6.635
# pvalor: 1e-13
# rejeita independência

# 8. ---------------------------------------------
ph <- function(a, b, c, d) {
    choose(a + b, a) * choose(c + d, c) /
    choose(a + b + c + d, a + c)
}
ph(6,1,0,3)

# pvalor: 0.03333
# rejeita independência

# 9. ---------------------------------------------
2 * (ph(6,8,12,2) + ph(5,9,13,1) + ph(4,10,14,0))

# pvalor: 0.04607
# rejeita independência

# 10. ---------------------------------------------
q10 <- cbind(
    c(51,58,48,26),
    c(33,29,42,38),
    c(16,13,30,16)
)
q10_e <- apply(q10, 1, sum) %*% t(apply(q10, 2, sum)) / sum(q10)
sum((q10_e - q10)^2 / q10_e)
qchisq(0.95, 6)
pchisq(17.17, 6, lower.tail = FALSE)

# estatística: 17.17
# quantil: 12.59
# pvalor: 0.008678
# rejeita independência

# 11. ---------------------------------------------
q11 <- data.frame(
    class1 = c(
        9,10,6,8,4,5,9,6,4,6,5,7,7,9,6
    ),
    class2 = c(
        4,5,9,9,6,6,8,3,7,5,9,6,10,8,8
    )
)
q11_m <- median(c(q11$class1, q11$class2))
a <- sum(q11$class1 >= q11_m)
c <- sum(q11$class1 < q11_m)
b <- sum(q11$class2 >= q11_m)
d <- sum(q11$class2 < q11_m)
n <- nrow(q11) * ncol(q11)
n * (abs(a*d - b*c) - n/2)^2 /
((a+b) * (c+d) * (a+c) * (b+d))
qchisq(0.95, 1)
pchisq(0.01339, 1)

# estatística: 0.01339
# quantil: 3.841
# pvalor: 0.0921
# não rejeita independência

# 12. ---------------------------------------------
q12 <- list(
    reg1 = c(
        17,19,22,25,24,25,30,29,31,28,37,15,40,17,38,39
    ),
    reg2 = c(
        18,21,20,23,17,26,28,28,29,32,16,35,19,27
    )
)
q12_m <- median(c(q12$reg1, q12$reg2))
a <- sum(q12$reg1 >= q12_m)
c <- sum(q12$reg1 < q12_m)
b <- sum(q12$reg2 >= q12_m)
d <- sum(q12$reg2 < q12_m)
n <- length(q12$reg1) + length(q12$reg2)
n * (abs(a*d - b*c) - n/2)^2 /
((a+b) * (c+d) * (a+c) * (b+d))
qchisq(0.95, 1)
pchisq(0.01339, 1)

# estatística: 0.01339
# quantil: 3.841
# pvalor: 0.0921
# não rejeita independência

# 13. ---------------------------------------------
q13 <- list(
    homens = c(68, 63, 75, 65, 73),
    mulheres = c(83, 78, 70, 93, 88, 80, 85)
)
wilcox.test(q13$homens, q13$mulheres)

# estatística: 2
# pvalor: 0.0101
# rejeita igualdade

# 14. ---------------------------------------------
q14 <- list(
    x = c(22, 26, 16, 27, 19, 38, 37),
    y = c(35, 32, 31, 23, 28, 18, 25, 21)
)
wilcox.test(q14$x, q14$y, alternative = "less")

# estatística: 27
# pvalor: 0.4775
# não rejeita igualdade

# 15. ---------------------------------------------
q15 <- list(
    x = c(28.4, 34.2, 29.5, 32.2, 30.1),
    y = c(32.2, 34.3, 36.2, 35.6, 32.5)
)
wilcox.test(q15$x, q15$y)

# estatística: 2.5
# pvalor: 0.04653
# rejeita igualdade
