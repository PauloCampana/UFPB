# Questão 1. ------------------------------------
q1 <- data.frame(
    A = c(2, 1, 2, 2, 1, 1, 2, 1),
    B = c(3, 3, 3, 3, 2, 3, 3, 3),
    C = c(1, 2, 1, 1, 3, 2, 1, 2)
)

q1 |>
    as.matrix() |>
    friedman.test()

# estatística: 9.25
# pvalor: 0.008
# rejeita medianas iguais nos grupos

# Questão 2. ------------------------------------
q2 <- data.frame(
    A = c(4000, 1600, 1600, 1200, 840, 352, 224, 200, 184),
    B = c(3210, 1040, 647, 570, 445, 156, 155, 99, 70),
    C = c(6120, 2410, 2210, 2060, 1400, 249, 224, 208, 227)
)

q2 |>
    as.matrix() |>
    friedman.test()

# estatística: 15.943
# pvalor: 0.00034
# rejeita medianas iguais nos grupos

# Questão 3. ------------------------------------
q3 <- data.frame(
    I = c(1, 1, 1, 1, 1, 0, 1),
    II = c(1, 1, 0, 1, 0, 1, 1),
    III = c(0, 0, 0, 0, 0, 1, 0),
    IV = c(0, 0, 0, 0, 1, 0, 0)
)

q3 |>
    as.matrix() |>
    friedman.test()

# estatística: 9.222
# pvalor: 0.02648
# rejeita medianas iguais nos grupos

# Questão 4. ------------------------------------
q4 <- data.frame(
    name = c(rep("I", 6), rep("II", 5), rep("III", 7)),
    value = c(
        19, 21, 29, 22, 37, 43,
        30, 38, 35, 24, 29,
        39, 32, 41, 44, 30, 27, 33
    )
)

kruskal.test(q4$value, q4$name)

# estatística: 2.7276
# pvalor: 0.2557
# não rejeita medianas iguais nos grupos

# Questão 5. ------------------------------------
q5 <- data.frame( #nolint
    name = rep(c("I", "II", "III", "IV"), each = 5),
    value = c(
        24.5, 23.5, 26.4, 27.1, 29.9,
        28.4, 34.2, 29.5, 32.2, 30.1,
        26.1, 28.3, 24.3, 26.2, 27.8,
        32.2, 34.3, 36.2, 35.6, 32.5
    )
)

kruskal.test(q5$value, q5$name)

# estatística: 14.562
# pvalor: 0.002232
# rejeita medianas iguais nos grupos

# Questão 6. ------------------------------------
q6 <- data.frame(
    leitura = c(5, 2, 1, 4, 3, 6),
    status = c(4, 1, 2, 3, 5, 6)
)

cor(q6, method = "kendall")

# correlação: 0.6

# Questão 7. ------------------------------------
q7 <- data.frame(
    autori = c(3, 4, 2, 1, 8, 11, 10, 6, 7, 12, 5, 9),
    aspi = c(3, 6, 5, 1, 10, 9, 8, 3, 4, 12, 7, 11)
)

cor(q7, method = "kendall")

# correlação: 0.67

# Questão 8. ------------------------------------
q8 <- data.frame(
    x = c(65, 57, 55, 38, 29, 43, 49),
    y = c(58, 61, 58, 23, 34, 38, 37)
)

cor.test(q8$x, q8$y, method = "spearman")
cor.test(q8$x, q8$y, method = "spearman", alternative = "greater")

# correlação: 0.8649
# a) sim
# b) sim

# Questão 9. ------------------------------------
q9 <- data.frame(
    x = c(30, 17, 35, 28, 42, 25, 19, 29),
    y = c(35, 31, 43, 46, 50, 32, 33, 42)
)

cor.test(q9$x, q9$y, method = "spearman")

# correlação significante
