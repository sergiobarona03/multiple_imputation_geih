
# Población diferenciada por sexo
dataset_prueba = dataset[c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P3271", "P6040", "P6008", "FEX_C18")]
colnames(dataset_prueba) = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "Sexo", "Edad", "Size", "expr")
dataset_prueba$Sexo = factor(dataset_prueba$Sexo, levels = c(1,2), labels = c("Male", "Female"))

tabla_sexo = dataset_prueba %>%  group_by(Sexo) %>% summarise(sum(expr))

colnames(tabla_sexo) = c("sexo", "freq")

tabla_sexo$share = tabla_sexo$freq/sum(tabla_sexo$freq)*100
 
# Población diferenciada por muestra
dataset_prueba = dataset[c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P3271", "P6040", "P6008", "FEX_C18")]
colnames(dataset_prueba) = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "Sexo", "Edad", "Size", "expr")
dataset_prueba$Sexo = factor(dataset_prueba$Sexo, levels = c(1,2), labels = c("Male", "Female"))

tabla_sexo_muestra = count(dataset_prueba, Sexo)

tabla_sexo_muestra$share = tabla_sexo_muestra$n/sum(tabla_sexo_muestra$n)*100


# Expansión de la población en general

tabla_prueba = dataset_prueba %>% summarise(sum(expr))



# tabla: deciles de ingresos vs tamaño del hogar

dataset_2_na$deciles = as.factor(dataset_2_na$deciles)
dataset_2_na$P6008 = as.factor(dataset_2_na$P6008)

table_1 = dataset_2_na %>% group_by(deciles) %>% summarise(sum(FEX_C18))

# pregunta de investigacion: ¿es simplemente una expansion?

df_deciles = as.data.frame(matrix(ncol = 2, nrow = 10))
colnames(df_deciles) = c("decil", "n")
df_deciles$decil = table_1$deciles

for (k in 1:10) {
  df_x = dataset_2_na %>% filter(deciles %in% df_deciles$decil[k])
  df_deciles$n[k] = sum(df_x$FEX_C18)
}

  







