sui <- read.csv("master.csv")
sui <- sui[,-c(8,10,12)]

names(sui) <- c("País", "Ano", "Sexo", "Idade", "Suicidios", "População",
                "Suicidios por 100 mil habitantes", "IDH", "PIB per capita")

sui$Sexo[sui$Sexo == "male"] <- "Masculino"
sui$Sexo[sui$Sexo == "female"] <- "Feminino"

sui$Idade[sui$Idade == "5-14 years"] <- "5 à 14 anos"
sui$Idade[sui$Idade == "15-24 years"] <- "15 à 24 anos"
sui$Idade[sui$Idade == "25-34 years"] <- "25 à 34 anos"
sui$Idade[sui$Idade == "35-54 years"] <- "35 à 54 anos"
sui$Idade[sui$Idade == "55-74 years"] <- "55 à 74 anos"
sui$Idade[sui$Idade == "75+ years"] <- "75 anos ou mais"

write.csv(sui, file = "sui.csv")
