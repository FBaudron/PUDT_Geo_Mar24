
library(openxlsx)

setwd('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\')

loc_o_n = read.xlsx('3. Localites\\Sélection localités\\Localités sélectionnées oui non végétation 6 Mars 2024.xlsx', sheet = 1)

table(loc_o_n$Départeme)

loc_o_n = subset(loc_o_n, subset = Départeme == "Sangha") 

names(loc_o_n)

loc_o_n = loc_o_n[, c(3, 2, 1, 12:13)]

loc_o_n = loc_o_n[order(loc_o_n$Selection, decreasing = TRUE),]  

write.xlsx(loc_o_n, file = "4. Departements//Sangha.xlsx")

