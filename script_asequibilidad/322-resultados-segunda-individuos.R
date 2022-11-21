

##########################################
## 322-resultados-segunda-individuos.R  ##
##########################################

library(here)
source(here::here("script_asequibilidad/", "321-resultados-segunda-hogares.R"))

##############################
## Dieta de subsistencia    ##
##############################
N = sum(poverty_dieta_1$P6008*poverty_dieta_1$FEX_C18)
z = mean(model_dieta_1$per_capita)
poverty_dieta_1_ind = poverty_dieta_1

# construcción de la variable indicadora
poverty_dieta_1_ind$dummy_ind = poverty_dieta_1_ind$dummy*poverty_dieta_1$P6008

# calcular brecha relativa 
poverty_dieta_1_ind$brecha_rel =  ((z - poverty_dieta_1_ind$per_capita_dia)/z)*poverty_dieta_1_ind$dummy_ind

# calcular el cuadrado de la brecha relativa 
poverty_dieta_1_ind$brecha_rel_sqr =  (((z - poverty_dieta_1_ind$per_capita_dia)/z)^2)*poverty_dieta_1_ind$dummy_ind

# calculo de indices
poverty_1_outcome_ind = as.data.frame(matrix(ncol=4))
colnames(poverty_1_outcome_ind) = c("tipo", "tasa", "brecha", "severidad")

poverty_1_outcome_ind$tipo = "Dieta de subsistencia"

poverty_1_outcome_ind$tasa = (sum(poverty_dieta_1_ind$dummy_ind*poverty_dieta_1_ind$FEX_C18)/N)*100
poverty_1_outcome_ind$brecha = sum(poverty_dieta_1_ind$brecha_rel*poverty_dieta_1_ind$FEX_C18)/N
poverty_1_outcome_ind$severidad = sum(poverty_dieta_1_ind$brecha_rel_sqr*poverty_dieta_1_ind$FEX_C18)/N



######################################
## Dieta nutricionalmente adecuada  ##
######################################
N = sum(poverty_dieta_2$P6008*poverty_dieta_2$FEX_C18)
z = mean(model_dieta_2$per_capita)
poverty_dieta_2_ind = poverty_dieta_2

# construcción de la variable indicadora
poverty_dieta_2_ind$dummy_ind = poverty_dieta_2_ind$dummy*poverty_dieta_2$P6008

# calcular brecha relativa 
poverty_dieta_2_ind$brecha_rel =  ((z - poverty_dieta_2_ind$per_capita_dia)/z)*poverty_dieta_2_ind$dummy_ind

# calcular el cuadrado de la brecha relativa 
poverty_dieta_2_ind$brecha_rel_sqr =  (((z - poverty_dieta_2_ind$per_capita_dia)/z)^2)*poverty_dieta_2_ind$dummy_ind

# calculo de indices
poverty_2_outcome_ind = as.data.frame(matrix(ncol=4))
colnames(poverty_2_outcome_ind) = c("tipo", "tasa", "brecha", "severidad")

poverty_2_outcome_ind$tipo = "Dieta nutritiva"

poverty_2_outcome_ind$tasa = (sum(poverty_dieta_2_ind$dummy_ind*poverty_dieta_2_ind$FEX_C18)/N)*100
poverty_2_outcome_ind$brecha = sum(poverty_dieta_2_ind$brecha_rel*poverty_dieta_2_ind$FEX_C18)/N
poverty_2_outcome_ind$severidad = sum(poverty_dieta_2_ind$brecha_rel_sqr*poverty_dieta_2_ind$FEX_C18)/N


######################################
## Dieta saludable o recomendada    ##
######################################
N = sum(poverty_dieta_3$P6008*poverty_dieta_3$FEX_C18)
z = mean(model_dieta_3$per_capita)
poverty_dieta_3_ind = poverty_dieta_3

# construcción de la variable indicadora
poverty_dieta_3_ind$dummy_ind = poverty_dieta_3_ind$dummy*poverty_dieta_3$P6008

# calcular brecha relativa 
poverty_dieta_3_ind$brecha_rel =  ((z - poverty_dieta_3_ind$per_capita_dia)/z)*poverty_dieta_3_ind$dummy_ind

# calcular el cuadrado de la brecha relativa 
poverty_dieta_3_ind$brecha_rel_sqr =  (((z - poverty_dieta_3_ind$per_capita_dia)/z)^2)*poverty_dieta_3_ind$dummy_ind

# calculo de indices
poverty_3_outcome_ind = as.data.frame(matrix(ncol=4))
colnames(poverty_3_outcome_ind) = c("tipo", "tasa", "brecha", "severidad")

poverty_3_outcome_ind$tipo = "Dieta saludable"

poverty_3_outcome_ind$tasa = (sum(poverty_dieta_3_ind$dummy_ind*poverty_dieta_3_ind$FEX_C18)/N)*100
poverty_3_outcome_ind$brecha = sum(poverty_dieta_3_ind$brecha_rel*poverty_dieta_3_ind$FEX_C18)/N
poverty_3_outcome_ind$severidad = sum(poverty_dieta_3_ind$brecha_rel_sqr*poverty_dieta_3_ind$FEX_C18)/N



############################
## Resultados generales   ##
############################

# Tabla de índices
ind_poverty_outcome = rbind(poverty_1_outcome_ind, poverty_2_outcome_ind)
ind_poverty_outcome = rbind(ind_poverty_outcome, poverty_3_outcome_ind)



