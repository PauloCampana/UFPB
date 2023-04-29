notas <- list(
    aluno_1 = c(7.1, 3.2, NA),
    aluno_2 = c(2.7, 8.8, 10.0),
    aluno_3 = c(0.0, NA, NA),
    aluno_4 = c(7.7, 8.4, 6.3),
    aluno_5 = c(3.6, 6.6, 8.1),
    aluno_6 = c(NA, NA, NA),
    aluno_7 = c(7.4, 7.1, 7.3),
    aluno_8 = c(10.0, NA, 7.0),
    aluno_9 = c(1.6, 3.2, 5.3),
    aluno_10 = c(8.8, 9.2, 8.0)
)

funstatus <- function(notas){
    if(all(is.na(notas))) return("REP") else
    if(mean(notas, na.rm = TRUE) >= 7) return("A") else
    if(mean(notas, na.rm = TRUE) >= 4) return("F") else
    return("REP")
}

status <- sapply(notas, funstatus)
alunos <- paste0("Aluno_", 1:10)
notas2 <- sapply(notas, c) |> t()

histórico <- data.frame(nomes = alunos,
                        notas = notas2,
                        status = status)

aprovados <- histórico[histórico$status == "A", ]
bons_alunos <- histórico[!histórico$status == "REP", ]
rownames(histórico) <- paste0("id_", 1:10)

selectna <- NULL
for(i in 1:10){
    selectna[i] <- histórico[ ,2:4][i, ] |> is.na() |> any()
}

histórico_na <- histórico[selectna, ]

média <- apply(histórico[, 2:4], MARGIN = 1, FUN = mean)
histórico$média <- média
