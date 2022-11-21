
#######################################
## 321-resultados-segunda-hogares.R  ##
#######################################

library(here)
source(here::here("script_asequibilidad/", "32-segunda-aprox.R"))


#Calcular gasto per capita en alimentacion por hogar
dataset_poverty = dataset_2_na

dataset_poverty$per_capita = dataset_poverty$ingreso_alimentos/dataset_poverty$P6008
dataset_poverty$per_capita_dia = dataset_poverty$per_capita/30


##############################
## Dieta de subsistencia    ##
##############################

# construcción de la variable indicadora
poverty_dieta_1 = dataset_poverty
poverty_dieta_1$dummy = NA

for (k in 1:nrow(poverty_dieta_1)) {
  if (poverty_dieta_1$per_capita_dia[k] < mean(model_dieta_1$per_capita)) {
    poverty_dieta_1$dummy[k] = 1
  } else {
    poverty_dieta_1$dummy[k] = 0
  }
}

# calcular brecha relativa para cada hogar
poverty_dieta_1$brecha_rel =  ((mean(model_dieta_1$per_capita) - poverty_dieta_1$per_capita_dia)/mean(model_dieta_1$per_capita))*poverty_dieta_1$dummy

# calculo de indices
poverty_1_outcome = as.data.frame(matrix(ncol=4))
colnames(poverty_1_outcome) = c("tipo", "tasa", "brecha", "severidad")

poverty_1_outcome$tipo = "Dieta de subsistencia"
poverty_1_outcome$tasa = (sum(poverty_dieta_1$dummy)/nrow(poverty_dieta_1))*100
poverty_1_outcome$brecha = sum(poverty_dieta_1$brecha_rel)/nrow(poverty_dieta_1)
poverty_1_outcome$severidad = sum(poverty_dieta_1$brecha_rel*poverty_dieta_1$brecha_rel)/nrow(poverty_dieta_1)


######################################
## Dieta nutricionalmente adecuada  ##
######################################
poverty_dieta_2 = dataset_poverty
poverty_dieta_2$dummy = NA

for (k in 1:nrow(poverty_dieta_2)) {
  if (poverty_dieta_2$per_capita_dia[k] < mean(model_dieta_2$per_capita)) {
    poverty_dieta_2$dummy[k] = 1
  } else {
    poverty_dieta_2$dummy[k] = 0
  }
}

# calcular brecha relativa para cada hogar
poverty_dieta_2$brecha_rel =  ((mean(model_dieta_2$per_capita) - poverty_dieta_2$per_capita_dia)/mean(model_dieta_2$per_capita))*poverty_dieta_2$dummy

# calculo de indices
poverty_2_outcome = as.data.frame(matrix(ncol=4))
colnames(poverty_2_outcome) = c("tipo", "tasa", "brecha", "severidad")

poverty_2_outcome$tipo = "Dieta nutritiva"
poverty_2_outcome$tasa = (sum(poverty_dieta_2$dummy)/nrow(poverty_dieta_2))*100
poverty_2_outcome$brecha = sum(poverty_dieta_2$brecha_rel)/nrow(poverty_dieta_2)
poverty_2_outcome$severidad = sum(poverty_dieta_2$brecha_rel*poverty_dieta_2$brecha_rel)/nrow(poverty_dieta_2)


######################################
## Dieta saludable o recomendada    ##
######################################

poverty_dieta_3 = dataset_poverty
poverty_dieta_3$dummy = NA

for (k in 1:nrow(poverty_dieta_3)) {
  if (poverty_dieta_3$per_capita_dia[k] < mean(model_dieta_3$per_capita)) {
    poverty_dieta_3$dummy[k] = 1
  } else {
    poverty_dieta_3$dummy[k] = 0
  }
}

# calcular brecha relativa para cada hogar
poverty_dieta_3$brecha_rel =  ((mean(model_dieta_3$per_capita) - poverty_dieta_3$per_capita_dia)/mean(model_dieta_3$per_capita))*poverty_dieta_3$dummy

# calculo de indices
poverty_3_outcome = as.data.frame(matrix(ncol=4))
colnames(poverty_3_outcome) = c("tipo", "tasa", "brecha", "severidad")

poverty_3_outcome$tipo = "Dieta saludable"
poverty_3_outcome$tasa = (sum(poverty_dieta_3$dummy)/nrow(poverty_dieta_3))*100
poverty_3_outcome$brecha = sum(poverty_dieta_3$brecha_rel)/nrow(poverty_dieta_3)
poverty_3_outcome$severidad = sum(poverty_dieta_3$brecha_rel*poverty_dieta_3$brecha_rel)/nrow(poverty_dieta_3)


############################
## Resultados generales   ##
############################

# Tabla de índices
household_poverty_outcome = rbind(poverty_1_outcome, poverty_2_outcome)
household_poverty_outcome = rbind(household_poverty_outcome, poverty_3_outcome)




