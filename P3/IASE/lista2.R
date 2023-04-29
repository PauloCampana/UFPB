dados <- state.x77 |> as.data.frame()
dados1 <- dados[dados$Population > 4246, ]
dados2 <- dados1[dados1$Population < 8000, ]

estadosc <- rownames(dados2)

média <- dados$Population |> mean()
dados3 <- dados[dados$Population > 1.5 * média, ]
estadose <- rownames(dados3)

mediana <- dados$Population |> median()
dados4 <- dados[dados$Population > 2 * mediana & dados$`Life Exp` > 71.84, ]

médiarenda <- dados$Income |> mean()
dados5 <- dados[dados$Income > médiarenda & dados$`Life Exp` > 72, ]

dados[51, ] <- apply(dados, MARGIN = 2, FUN = mean)
dados[52, ] <- apply(dados, MARGIN = 2, FUN = var)
