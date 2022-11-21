
################################
## 23-tablas-cruzadas.R       ##
################################
source(here::here("script_asequibilidad/", "22-proporcion-gasto.R"))

# tabla: deciles de ingresos vs tamaño del hogar
table_1 = as.data.frame(table(dataset_2_na$deciles, dataset_2_na$P6008))
colnames(table_1) = c("Decil", "Tamaño_hogar", "Freq")

table_1_unmelt = dcast(data = table_1, Tamaño_hogar ~ Decil, value.var = "Freq")

table_1_unmelt$Total = rowSums(table_1_unmelt[,2:ncol(table_1_unmelt)])

total_2 = as.data.frame(matrix(c("Total", as.numeric(colSums(table_1_unmelt[, 2:ncol(table_1_unmelt)]))), nrow = 1))

colnames(total_2) = colnames(table_1_unmelt)
table_1_unmelt = rbind(table_1_unmelt, total_2)

# análisis para hogares n-personales (n =2,3,4)
Grupo = c("[1,4)", "[4,9)", "[9,14)", "[14,19)", "[19,31)", "[31,51)",  "[51,70)",  "[70,Inf)")
table_2_def = data.frame(Grupo)

for (k in 2:4) {
  dataset_bi = filter(dataset, P6008 == k)
  dataset_bi = dataset_bi[c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P3271", "P6040", "P6008")]
  colnames(dataset_bi) = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "Sexo", "Edad", "Size")
  dataset_bi = dataset_bi %>% mutate(Int = cut(Edad, c(c(1, 4, 9, 14, 19, 31, 51, 70), Inf), right = FALSE))
  
  table_2 = as.data.frame(table(dataset_bi$Int, dataset_bi$Sexo))
  colnames(table_2) = c("Grupo", "Sexo", "Freq")
  table_2$Sexo = factor(table_2$Sexo, levels = c(1,2), labels = c("Male", "Female"))
  table_2_unmelt = dcast(data = table_2, Grupo ~ Sexo, value.var = "Freq")
  colnames(table_2_unmelt) = c("Grupo", paste0("Male (n = ",k,")"), paste0("Female (n = ",k,")"))
  table_2_def = merge(table_2_def, table_2_unmelt, by = "Grupo")
}
