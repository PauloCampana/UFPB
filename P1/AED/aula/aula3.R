m.violence <- matrix(c(611, 34, 16, 18, 308, 10, 17, 10),
                     nrow = 2, byrow = TRUE)
chisq.test(m.violence)

m.violence.w <- matrix(c(356, 20, 3, 9, 226, 11, 10 ,9),
                       nrow = 2, byrow = TRUE)
chisq.test(m.violence.w)

m.blood <- matrix(c(72, 230, 54, 192, 16, 63, 8, 15),
                  nrow = 4, byrow = TRUE)
chisq.test(m.blood)

m.headache <- matrix(c(6, 30, 22, 11, 35, 17, 4, 19, 14, 5, 25, 12),
            nrow = 4, byrow = TRUE)
chisq.test(m.headache)

m.hemo <- matrix(c(80, 100, 20, 99, 190, 96, 70, 30, 10),
                 nrow = 3, byrow = TRUE)
chisq.test(m.hemo)

m.joint <- matrix(c(111, 102, 59, 73, 80, 125),
                  nrow = 3, byrow = TRUE)
chisq.test(m.joint)

m.drug.a <- matrix(c(5, 9, 6, 9, 9, 2),
                   nrow = 2, byrow = TRUE)
m.drug.b <- matrix(c(2, 10, 8, 2, 8, 10),
                   nrow = 2, byrow = TRUE)
m.drug.c <- matrix(c(5, 9, 6, 7, 8, 5),
                   nrow = 2, byrow = TRUE)
chisq.test(m.drug.a)
chisq.test(m.drug.b)
chisq.test(m.drug.c)
