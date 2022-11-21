
#########################
## 31-primera-aprox.R  ##
#########################
library(here)
source(here::here("script_asequibilidad/", "30-asequibilidad-dietas.R"))
###########################
## Cargar tipos de hogar ##
###########################

tipo_hogares = read_excel("script_asequibilidad/tipo_hogares.xlsx")
colnames(tipo_hogares) = c("hogar", "persona", "sexo", "grupo_demo")

####################################################
####################################################
## Calcular el costo mínimo de cada tipo de dieta ##
##      para cada tipo de hogar definido          ##
####################################################
####################################################

tipo_hogares$id = c(1:nrow(tipo_hogares))

###########################
## dieta de subsistencia ##
###########################

# calcular el costo para cada miembro
tipo_hogares_dieta_1 = merge(tipo_hogares, dieta_1[c("grupo_demo", "sexo", "costo_dia")], 
                             by = c("grupo_demo", "sexo"), 
                             all.x = TRUE, all.y = FALSE)

tipo_hogares_dieta_1 = tipo_hogares_dieta_1[order(tipo_hogares_dieta_1$id),]
tipo_hogares_dieta_1 = tipo_hogares_dieta_1[c("hogar", "persona", "sexo", "costo_dia")]

# calcular el costo para el tipo de hogar
dieta_1_hogar = as.data.frame(matrix(1:9))
colnames(dieta_1_hogar) = "hogar"

for (k in 1:9) {
  df = data.frame()
  df = tipo_hogares_dieta_1 %>% filter(hogar %in% k)
  dieta_1_hogar[which(dieta_1_hogar$hogar == k),2] = sum(df$costo_dia)
}

colnames(dieta_1_hogar) = c("Tipo de hogar", "Costo diario estimado ($)")


#####################################
## dieta nutricionalmente adecuada ##
#####################################

# calcular el costo para cada miembro
tipo_hogares_dieta_2 = merge(tipo_hogares, dieta_2[c("grupo_demo", "sexo", "costo_dia")], 
                             by = c("grupo_demo", "sexo"), 
                             all.x = TRUE, all.y = FALSE)

tipo_hogares_dieta_2 = tipo_hogares_dieta_2[order(tipo_hogares_dieta_2$id),]
tipo_hogares_dieta_2 = tipo_hogares_dieta_2[c("hogar", "persona", "sexo", "costo_dia")]

# calcular el costo para el tipo de hogar
dieta_2_hogar = as.data.frame(matrix(1:9))
colnames(dieta_2_hogar) = "hogar"

for (k in 1:9) {
  df = data.frame()
  df = tipo_hogares_dieta_2 %>% filter(hogar %in% k)
  dieta_2_hogar[which(dieta_2_hogar$hogar == k),2] = sum(df$costo_dia)
}

colnames(dieta_2_hogar) = c("Tipo de hogar", "Costo diario estimado ($)")

###########################
## dieta de subsistencia ##
###########################

# calcular el costo para cada miembro
tipo_hogares_dieta_3 = merge(tipo_hogares, dieta_3[c("grupo_demo", "sexo", "costo_dia")], 
                             by = c("grupo_demo", "sexo"), 
                             all.x = TRUE, all.y = FALSE)

tipo_hogares_dieta_3 = tipo_hogares_dieta_3[order(tipo_hogares_dieta_3$id),]
tipo_hogares_dieta_3 = tipo_hogares_dieta_3[c("hogar", "persona", "sexo", "costo_dia")]

# calcular el costo para el tipo de hogar
dieta_3_hogar = as.data.frame(matrix(1:9))
colnames(dieta_3_hogar) = "hogar"

for (k in 1:9) {
  df = data.frame()
  df = tipo_hogares_dieta_3 %>% filter(hogar %in% k)
  dieta_3_hogar[which(dieta_3_hogar$hogar == k),2] = sum(df$costo_dia)
}

colnames(dieta_3_hogar) = c("Tipo de hogar", "Costo diario estimado ($)")



