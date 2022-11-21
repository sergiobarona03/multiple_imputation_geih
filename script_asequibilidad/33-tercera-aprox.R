
########################
## 33-tercera-aprox.R ##
########################

library(here)
source(here::here("script_asequibilidad/", "322-resultados-segunda-individuos.R"))


dataset_aprox_3 = dataset[c("id","P6040", "P3271")]
colnames(dataset_aprox_3) = c("id", "edad", "sexo")

dataset_aprox_3 = dataset_aprox_3 %>% mutate(grupo_demo = cut(edad, c(c(1, 4, 9, 14, 19, 31, 51, 70), Inf), right = FALSE))

dataset_aprox_3$sexo = factor(dataset_aprox_3$sexo, levels = c(1,2), labels = c(0, 1))


# recodificar (este paso es provisional)
dataset_aprox_3 = dataset_aprox_3[c("id", "sexo", "grupo_demo")]
dataset_aprox_3$grupo_demo = factor(as.factor(dataset_aprox_3$grupo_demo),
                             levels = c("[1,4)", "[4,9)", "[9,14)", "[14,19)",
                                        "[19,31)", "[31,51)", "[51,70)", "[70,Inf)"),
                             labels = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)",
                                        "[19, 31)", "[31, 50)", "[51, 70)", ">70"))
# id para ordenar
dataset_aprox_3$id_aux = c(1:nrow(dataset_aprox_3))


###########################
## Dieta de subsistencia ##
###########################

# calcular el costo diario de la dieta de subsistencia para cada familia
dieta_1_aprox_3 = merge(dataset_aprox_3, dieta_1[c("grupo_demo", "sexo", "costo_dia")],
                        by = c("grupo_demo", "sexo"), all.x = TRUE)

#ordenar segun el id
dieta_1_aprox_3 = dieta_1_aprox_3[order(dieta_1_aprox_3$id_aux),]

# seleccionar las variables de interes
dieta_1_aprox_3 = dieta_1_aprox_3[c("id", "grupo_demo", "sexo", "costo_dia")]

# calcular el costo de la dieta para cada hogar
dieta_1_aprox_3_hogar = data.frame(levels(as.factor(dieta_1_aprox_3$id)))
colnames(dieta_1_aprox_3_hogar) = "id"
dieta_1_aprox_3_hogar$costo_diario = NA

for (k in 1:nrow(dieta_1_aprox_3)) {
  df = data.frame()
  df = dieta_1_aprox_3 %>% filter(id %in% dieta_1_aprox_3_hogar$id[k])
  id_x = which(dieta_1_aprox_3_hogar$id == dieta_1_aprox_3_hogar$id[k])
  dieta_1_aprox_3_hogar$costo_diario[id_x] = sum(df$costo_dia)
}

# datos sobre gasto per capita en alimentacion

dieta_1_aprox_3_hogar_gasto = merge(dieta_1_aprox_3_hogar, 
                                    dataset_2_na[c("id", "per_capita")],
                                    by = "id", all.y = FALSE, all.x = FALSE)

# provisionalmente: eliminar hogares con recien nacidos
dieta_1_aprox_3_hogar_gasto = dieta_1_aprox_3_hogar_gasto[!is.na(dieta_1_aprox_3_hogar_gasto$costo_diario),]

# construir per capita diario
dieta_1_aprox_3_hogar_gasto$per_capita_dia = dieta_1_aprox_3_hogar_gasto$per_capita/30

# crear la dummy para construir la proporción
dieta_1_aprox_3_hogar_gasto$dummy = NA

for (k in 1:nrow(dieta_1_aprox_3_hogar_gasto)) {
  if (dieta_1_aprox_3_hogar_gasto$per_capita_dia[k] < dieta_1_aprox_3_hogar_gasto$costo_diario[k]) {
    dieta_1_aprox_3_hogar_gasto$dummy[k] = 1
  } else {
    dieta_1_aprox_3_hogar_gasto$dummy[k] = 0
  }
}

# calcular proporcion de hogares por debajo de la linea
schneider_outcome_1 = as.data.frame(matrix(nrow = 1, ncol=2))
colnames(schneider_outcome_1) = c("Tipo", "Tasa")
schneider_outcome_1$Tipo = "Dieta de subsistencia"
schneider_outcome_1$Tasa = sum(dieta_1_aprox_3_hogar_gasto$dummy)/nrow(dieta_1_aprox_3_hogar_gasto)


#####################################
## Dieta nutricionalmente adecuada ##
#####################################

# calcular el costo diario de la dieta nutritiva para cada familia
dieta_2_aprox_3 = merge(dataset_aprox_3, dieta_2[c("grupo_demo", "sexo", "costo_dia")],
                        by = c("grupo_demo", "sexo"), all.x = TRUE)

#ordenar segun el id
dieta_2_aprox_3 = dieta_2_aprox_3[order(dieta_2_aprox_3$id_aux),]

# seleccionar las variables de interes
dieta_2_aprox_3 = dieta_2_aprox_3[c("id", "grupo_demo", "sexo", "costo_dia")]

# calcular el costo de la dieta para cada hogar
dieta_2_aprox_3_hogar = data.frame(levels(as.factor(dieta_2_aprox_3$id)))
colnames(dieta_2_aprox_3_hogar) = "id"
dieta_2_aprox_3_hogar$costo_diario = NA

for (k in 1:nrow(dieta_2_aprox_3)) {
  df = data.frame()
  df = dieta_2_aprox_3 %>% filter(id %in% dieta_2_aprox_3_hogar$id[k])
  id_x = which(dieta_2_aprox_3_hogar$id == dieta_2_aprox_3_hogar$id[k])
  dieta_2_aprox_3_hogar$costo_diario[id_x] = sum(df$costo_dia)
}

# datos sobre gasto per capita en alimentacion

dieta_2_aprox_3_hogar_gasto = merge(dieta_2_aprox_3_hogar, 
                                    dataset_2_na[c("id", "per_capita")],
                                    by = "id", all.y = FALSE, all.x = FALSE)

# provisionalmente: eliminar hogares con recien nacidos
dieta_2_aprox_3_hogar_gasto = dieta_2_aprox_3_hogar_gasto[!is.na(dieta_2_aprox_3_hogar_gasto$costo_diario),]

# construir per capita diario
dieta_2_aprox_3_hogar_gasto$per_capita_dia = dieta_2_aprox_3_hogar_gasto$per_capita/30

# crear la dummy para construir la proporción
dieta_2_aprox_3_hogar_gasto$dummy = NA

for (k in 1:nrow(dieta_2_aprox_3_hogar_gasto)) {
  if (dieta_2_aprox_3_hogar_gasto$per_capita_dia[k] < dieta_2_aprox_3_hogar_gasto$costo_diario[k]) {
    dieta_2_aprox_3_hogar_gasto$dummy[k] = 1
  } else {
    dieta_2_aprox_3_hogar_gasto$dummy[k] = 0
  }
}

# calcular proporcion de hogares por debajo de la linea
schneider_outcome_2 = as.data.frame(matrix(nrow = 1, ncol=2))
colnames(schneider_outcome_2) = c("Tipo", "Tasa")
schneider_outcome_2$Tipo = "Dieta nutritiva"
schneider_outcome_2$Tasa = sum(dieta_2_aprox_3_hogar_gasto$dummy)/nrow(dieta_2_aprox_3_hogar_gasto)

#####################################
## Dieta saludable o recomendada   ##
#####################################

# calcular el costo diario de la dieta saludable para cada familia
dieta_3_aprox_3 = merge(dataset_aprox_3, dieta_3[c("grupo_demo", "sexo", "costo_dia")],
                        by = c("grupo_demo", "sexo"), all.x = TRUE)

#ordenar segun el id
dieta_3_aprox_3 = dieta_3_aprox_3[order(dieta_3_aprox_3$id_aux),]

# seleccionar las variables de interes
dieta_3_aprox_3 = dieta_3_aprox_3[c("id", "grupo_demo", "sexo", "costo_dia")]

# calcular el costo de la dieta para cada hogar
dieta_3_aprox_3_hogar = data.frame(levels(as.factor(dieta_3_aprox_3$id)))
colnames(dieta_3_aprox_3_hogar) = "id"
dieta_3_aprox_3_hogar$costo_diario = NA

for (k in 1:nrow(dieta_3_aprox_3)) {
  df = data.frame()
  df = dieta_3_aprox_3 %>% filter(id %in% dieta_3_aprox_3_hogar$id[k])
  id_x = which(dieta_3_aprox_3_hogar$id == dieta_3_aprox_3_hogar$id[k])
  dieta_3_aprox_3_hogar$costo_diario[id_x] = sum(df$costo_dia)
}

# datos sobre gasto per capita en alimentacion

dieta_3_aprox_3_hogar_gasto = merge(dieta_3_aprox_3_hogar, 
                                    dataset_2_na[c("id", "per_capita")],
                                    by = "id", all.y = FALSE, all.x = FALSE)

# provisionalmente: eliminar hogares con recien nacidos
dieta_3_aprox_3_hogar_gasto = dieta_3_aprox_3_hogar_gasto[!is.na(dieta_3_aprox_3_hogar_gasto$costo_diario),]

# construir per capita diario
dieta_3_aprox_3_hogar_gasto$per_capita_dia = dieta_3_aprox_3_hogar_gasto$per_capita/30

# crear la dummy para construir la proporción
dieta_3_aprox_3_hogar_gasto$dummy = NA

for (k in 1:nrow(dieta_3_aprox_3_hogar_gasto)) {
  if (dieta_3_aprox_3_hogar_gasto$per_capita_dia[k] < dieta_3_aprox_3_hogar_gasto$costo_diario[k]) {
    dieta_3_aprox_3_hogar_gasto$dummy[k] = 1
  } else {
    dieta_3_aprox_3_hogar_gasto$dummy[k] = 0
  }
}

# calcular proporcion de hogares por debajo de la linea
schneider_outcome_3 = as.data.frame(matrix(nrow = 1, ncol=2))
colnames(schneider_outcome_3) = c("Tipo", "Tasa")
schneider_outcome_3$Tipo = "Dieta saludable"
schneider_outcome_3$Tasa = sum(dieta_3_aprox_3_hogar_gasto$dummy)/nrow(dieta_3_aprox_3_hogar_gasto)


##########################
## Resultados generales ##
##########################

household_schneider_outcome = rbind(schneider_outcome_1, schneider_outcome_2)
household_schneider_outcome = rbind(household_schneider_outcome, schneider_outcome_3)





