
########################
## 32-segunda-aprox.R ##
########################

library(here)
source(here::here("script_asequibilidad/", "311-resultados-primera.R"))

model_household = read_excel("script_asequibilidad/model_household.xlsx")

###########################
## dieta de subsistencia ##
###########################

# calcular el costo para cada miembro
model_dieta_1 = merge(model_household, dieta_1[c("grupo_demo", "sexo", "costo_dia")], 
                             by = c("grupo_demo", "sexo"), 
                             all.x = TRUE, all.y = FALSE)

model_dieta_1$hogar_total = sum(model_dieta_1$costo_dia)
model_dieta_1$per_capita = model_dieta_1$hogar_total/nrow(model_dieta_1)

#####################################
## dieta nutricionalmente adecuada ##
#####################################

# calcular el costo para cada miembro
model_dieta_2  = merge(model_household, dieta_2[c("grupo_demo", "sexo", "costo_dia")], 
                             by = c("grupo_demo", "sexo"), 
                             all.x = TRUE, all.y = FALSE)

model_dieta_2$hogar_total = sum(model_dieta_2$costo_dia)
model_dieta_2$per_capita = model_dieta_2$hogar_total/nrow(model_dieta_2)

#####################################
## dieta nutricionalmente adecuada ##
#####################################

# calcular el costo para cada miembro
model_dieta_3  = merge(model_household, dieta_3[c("grupo_demo", "sexo", "costo_dia")], 
                             by = c("grupo_demo", "sexo"), 
                             all.x = TRUE, all.y = FALSE)

model_dieta_3$hogar_total = sum(model_dieta_3$costo_dia)
model_dieta_3$per_capita = model_dieta_3$hogar_total/nrow(model_dieta_3)




