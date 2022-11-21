
###############################
## 311-resultados-primera.R  ##
###############################

# Base de datos para el gasto en alimentación (la base de datos es general)
deciles_gasto_tipo = cbind(deciles_gasto[c("deciles", "gasto_alimentacion")], 
                           replicate((nrow(dieta_1_hogar)-1), 
                                     deciles_gasto$gasto_alimentacion))

deciles_gasto_tipo = deciles_gasto_tipo  %>% as.matrix() %>% t() %>% as.data.frame()

deciles_gasto_tipo = deciles_gasto_tipo[2:(nrow(dieta_1_hogar)+1),]

deciles_gasto_tipo = cbind(c(1:nrow(dieta_1_hogar)), deciles_gasto_tipo)

colnames(deciles_gasto_tipo) = c("Tipo", "decil 1", "decil 2", "decil 3", "decil 4",
                                 "decil 5", "decil 6", "decil 7", "decil 8", 
                                 "decil 9", "decil 10")


################################
## 311-resultados-primera.R  ##
###############################

library(here)
source(here::here("script_asequibilidad/", "31-primera-aprox.R"))


##########################################
## Asequibilidad: dieta de subsistencia ##
##########################################



aprox_1_hogar_dieta_1 = data.frame(dieta_1_hogar,  
                                   replicate(9,dieta_1_hogar$`Costo diario estimado ($)`))

colnames(aprox_1_hogar_dieta_1) = c("Tipo", "decil 1", "decil 2", "decil 3", "decil 4",
                                    "decil 5", "decil 6", "decil 7", "decil 8", 
                                    "decil 9", "decil 10")

aprox_1_dieta_1 = aprox_1_hogar_dieta_1

for (i in 1:nrow(aprox_1_dieta_1)) {
  for (j in 2:ncol(aprox_1_dieta_1)) {
    if (aprox_1_hogar_dieta_1[i,j] <= (as.numeric(deciles_gasto_tipo[i,j])/30)) {
      aprox_1_dieta_1[i,j] = "YES"
    } else {
      aprox_1_dieta_1[i,j] = "NO"
    }
  }
}

####################################################
## Asequibilidad: dieta nutricionalmente adecuada ##
####################################################
aprox_1_hogar_dieta_2 = data.frame(dieta_2_hogar,  
                                   replicate(9,dieta_2_hogar$`Costo diario estimado ($)`))

colnames(aprox_1_hogar_dieta_2) = c("Tipo", "decil 1", "decil 2", "decil 3", "decil 4",
                                    "decil 5", "decil 6", "decil 7", "decil 8", 
                                    "decil 9", "decil 10")

aprox_1_dieta_2 = aprox_1_hogar_dieta_2

for (i in 1:nrow(aprox_1_dieta_2)) {
  for (j in 2:ncol(aprox_1_dieta_2)) {
    if (aprox_1_hogar_dieta_2[i,j] <= (as.numeric(deciles_gasto_tipo[i,j])/30)) {
      aprox_1_dieta_2[i,j] = "YES"
    } else {
      aprox_1_dieta_2[i,j] = "NO"
    }
  }
}

####################################################
## Asequibilidad: dieta saludable o recomendada   ##
####################################################
aprox_1_hogar_dieta_3 = data.frame(dieta_3_hogar,  
                                   replicate(9,dieta_3_hogar$`Costo diario estimado ($)`))

colnames(aprox_1_hogar_dieta_3) = c("Tipo", "decil 1", "decil 2", "decil 3", "decil 4",
                                    "decil 5", "decil 6", "decil 7", "decil 8", 
                                    "decil 9", "decil 10")

aprox_1_dieta_3 = aprox_1_hogar_dieta_3

for (i in 1:nrow(aprox_1_dieta_3)) {
  for (j in 2:ncol(aprox_1_dieta_3)) {
    if (aprox_1_hogar_dieta_3[i,j] <= (as.numeric(deciles_gasto_tipo[i,j])/30)) {
      aprox_1_dieta_3[i,j] = "YES"
    } else {
      aprox_1_dieta_3[i,j] = "NO"
    }
  }
}










