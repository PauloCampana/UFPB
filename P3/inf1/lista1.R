# Questão 1.
pnorm(10, 7.5, sqrt(20/10)) - pnorm(5, 7.5, sqrt(20/10))
pnorm(2.5/sqrt(20/10)) - pnorm(-2.5/sqrt(20/10))

# Questão 2.
pnorm(0.5, 0.4, sqrt(0.24/30))
pnorm(0.1/sqrt(0.24/30))

# Questão 3.
pnorm(55, 52.5, 12.1/sqrt(10)) - pnorm(45, 52.5, 12.1/sqrt(10))
pnorm(2.5 * sqrt(10)/12.1) - pnorm(-7.5 * sqrt(10)/12.1)

# Questão 4.
pnorm(51, 50, 4) - pnorm(49, 50, 4)
pnorm(1/4) - pnorm(-1/4)

pnorm(51, 50, sqrt(16/64)) - pnorm(49, 50, sqrt(16/64))
pnorm(2) - pnorm(-2)

# Questão 5.
pnorm(52, 54, 6/sqrt(50))
pnorm(-5 * sqrt(2)/3)

# Questão 6.
pnorm(55, 50, sqrt(25), lower.tail = FALSE)
1 - pnorm(1)

# Questão 7.
pnorm(1.5, 0, sqrt(9)) - pnorm(-0.3, 0, sqrt(9))
pnorm(0.5) - pnorm(-0.1)

# Questão 8.
pnorm(71.97, 71.43, sqrt(56.25/25)) - pnorm(68.91, 71.43, sqrt(56.25/25))
