
#########################################
## 221- Proporcion del gasto desde ECV ##
#########################################

library(dplyr)
library(tidyverse)

# Fuente: ECV, 2021

gastos_hogares = read.csv("prueba_gastos_ecv/Gastos de los hogares (Gastos por Item).csv",
                          sep = ",") 

hogares = read.csv("prueba_gastos_ecv/Caracteristicas y composicion del hogar.csv",
                   sep = ";")

vivienda = read.csv("prueba_gastos_ecv/Datos de la vivienda.csv", sep = ";")
  
servicios = read.csv("prueba_gastos_ecv/Servicios del hogar.csv", sep = ",")

# Restricción de la información a Cali (tamaño de muestra n = 3338)
vivienda_cali = vivienda %>% filter(P1_DEPARTAMENTO %in% 76)
vivienda_cali = vivienda_cali %>% filter(CLASE %in% 1)
gastos_hogares_cali = gastos_hogares %>% filter(DIRECTORIO %in% vivienda_cali$DIRECTORIO)
servicios_cali = servicios %>% filter(DIRECTORIO %in% vivienda_cali$DIRECTORIO)

# Selección de las variables de interés
vivienda_cali_1 = vivienda_cali[c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P",
                                  "ORDEN", "P1_DEPARTAMENTO", "CLASE", "FEX_C",
                                  "CANT_HOG_COMPLETOS", "CANT_HOGARES_VIVIENDA")]

gastos_hogares_cali_1 = gastos_hogares_cali[c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P",
                                              "ORDEN", "FEX_C", "P3204", "P3204S1", "P3204S2")]

servicios_cali_1 = servicios_cali[c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN",
                                    "I_HOGAR", "I_UGASTO", "PERCAPITA", "I_OU")]

###############################
## Cálculo del ingreso para  ##
##       cada hogar          ##
###############################
# construir id
servicios_cali_1$id = paste0(servicios_cali$DIRECTORIO,
                             "-",servicios_cali$ORDEN)

# ingresos hogares
hogar_ingresos = servicios_cali_1[c("id", "I_HOGAR")]

###########################################
## Cálculo del gasto total y  gasto      ##
##   en alimentación para cada hogar     ##
###########################################

# construir id
gastos_hogares_cali_1$id = paste0(gastos_hogares_cali_1$DIRECTORIO,
                                  "-",gastos_hogares_cali_1$SECUENCIA_P)

# reemplazar NA por 0 en las variables de gasto
gastos_hogares_cali_1$P3204S1[is.na(gastos_hogares_cali_1$P3204S1)] = 0
gastos_hogares_cali_1$P3204S2[is.na(gastos_hogares_cali_1$P3204S2)] = 0

# construir base de datos de recepción
hogar_gastos = data.frame(levels(as.factor(gastos_hogares_cali_1$id)))
hogar_gastos$gasto_total = NA
hogar_gastos$gasto_alimentos = NA
colnames(hogar_gastos) = c("id", "gasto_total", "gasto_alimentos")

for (k in 1:nrow(hogar_gastos)) {
  # bucle para gasto total
  df_1 = data.frame()
  df_1 = gastos_hogares_cali_1 %>% filter(id %in% hogar_gastos$id[k])
  hogar_gastos$gasto_total[k] = sum(df_1$P3204S1) + sum(df_1$P3204S2)
  #bucle para gasto en alimentos
  df_2 = df_1 %>% filter(P3204 %in% c(1:26,32))
  hogar_gastos$gasto_alimentos[k] = sum(df_2$P3204S1) + sum(df_2$P3204S2)
}

###################################
## Cálculo de las proporciones   ##
## (desde el ingreso y el gasto) ##
###################################

# recuperar el factor de expansión
hogar_gastos_dep = merge(hogar_gastos, gastos_hogares_cali_1[c("id","FEX_C")], by = "id")
hogar_gastos_dep = hogar_gastos_dep[!duplicated(hogar_gastos_dep),]

# eliminar valores nulos en alimentación
hogar_gastos_dep = hogar_gastos_dep %>% filter(gasto_alimentos != 0)

# merge gastos-ingresos
gastos_ingresos = merge(hogar_gastos_dep, hogar_ingresos, by = "id")

# implementación del factor de expansión 

gastos_ingresos_exp = as.data.frame(matrix(ncol = ncol(gastos_ingresos)))
colnames(gastos_ingresos_exp) = colnames(gastos_ingresos)

  
for (k in 1:nrow(gastos_ingresos)) {
  print(k)
  df = gastos_ingresos[k,]
  times = df$FEX_C
  df_x = df[rep(1, times),]
  
  gastos_ingresos_exp = rbind(gastos_ingresos_exp,
                              df_x)
  gastos_ingresos_exp = na.omit(gastos_ingresos_exp)
  rm(df,df_x)
}

# guardar base de datos expandida
saveRDS(gastos_ingresos_exp, here::here("prueba_gastos_ecv/gastos_ingresos_exp.RDS"))

# leer la base de datos expandida
gastos_ingresos_exp = readRDS(here::here("prueba_gastos_ecv/gastos_ingresos_exp.RDS"))

# proporción del gasto
gastos_ingresos_exp$share_gasto = gastos_ingresos_exp$gasto_alimentos/gastos_ingresos_exp$gasto_total
summary(gastos_ingresos_exp$share_gasto)

# Nota: los resultados para la proporción del ingresos no tiene resultados realistas
  
############
## Ad hoc ##
############

# eliminar gastos e ingresos nulos
gastos_ingresos_exp = gastos_ingresos_exp %>% filter(I_HOGAR != 0)
gastos_ingresos_exp = gastos_ingresos_exp %>% filter(gasto_total != 0)

############
## Ad hoc ##
############

# organización por deciles
deciles_ingresos = quantile(gastos_ingresos_exp$I_HOGAR, 
                            probs = seq(0.1,1, by = 0.1))

# clasificación por deciles
gastos_ingresos_exp = gastos_ingresos_exp %>% mutate(deciles = cut(I_HOGAR,
                                                      c(min(gastos_ingresos_exp$I_HOGAR)
                                                        , as.numeric(deciles_ingresos)),
                                                      c("decil 1", "decil 2", "decil 3",
                                                        "decil 4", "decil 5", "decil 6",
                                                        "decil 7", "decil 8", "decil 9",
                                                        "decil 10")))

# calculo de proporciones medias del gasto en alimentacion

mean_share = data.frame(c("decil 1", "decil 2", "decil 3",
                          "decil 4", "decil 5", "decil 6",
                          "decil 7", "decil 8", "decil 9",
                          "decil 10"))
colnames(mean_share) = "decil"
mean_share$share = NA

for (i in 1:nrow(mean_share)) {
  df = gastos_ingresos_exp %>% filter(deciles %in% mean_share$decil[i])
  mean_share$share[i] = mean(df$share_gasto)
}

# presentación de proporción media
mean_share$share = mean_share$share*100


#####################################
## ¿Y si lo tomamos por quintiles? ##
#####################################


# organización por quintiles
quintile = quantile(gastos_ingresos_exp$I_HOGAR, 
                            probs = seq(0,1, by = 0.2))

# clasificación por deciles
gastos_ingresos_exp = gastos_ingresos_exp %>% mutate(quintil = ntile(I_HOGAR, 5))

# calculo de proporciones medias del gasto en alimentacion

mean_share_2 = data.frame(c("Q1", "Q2", 
                          "Q3", "Q4", "Q5"))

colnames(mean_share_2) = "quintil"
mean_share_2$share = NA

for (i in 1:nrow(mean_share_2)) {
  df = gastos_ingresos_exp %>% filter(quintil %in% i)
  mean_share_2$share[i] = mean(df$share_gasto)
}

# presentación de proporción media
mean_share_2$share = mean_share_2$share*100


###################################
## ¿Y si analizamos unicamente   ##
##  el rango intercuartilico?    ##
###################################

# definir los valores atípicos del ingreso 
quant = quantile(gastos_ingresos_exp$I_HOGAR, probs = c(0.25, 0.75), na.rm = F)
iqr = IQR(gastos_ingresos_exp$I_HOGAR)
lower = quant[1] - 1.5*iqr
upper = quant[2] + 1.5*iqr

no_outliers = subset(gastos_ingresos_exp$I_HOGAR,
                     gastos_ingresos_exp$I_HOGAR > lower &
                       gastos_ingresos_exp$I_HOGAR < upper)

# eliminar valores atípicos de ingresos
gastos_ingresos_exp_out = gastos_ingresos_exp %>% filter(I_HOGAR %in% no_outliers)

# organización por quintiles
quintile = quantile(gastos_ingresos_exp_out$I_HOGAR, 
                    probs = seq(0,1, by = 0.2))

# clasificación por quintiles
gastos_ingresos_exp_out = gastos_ingresos_exp_out %>% mutate(quintil = ntile(I_HOGAR, 5))

# calculo de proporciones medias del gasto en alimentacion

mean_share_3 = data.frame(c("Q1", "Q2", 
                            "Q3", "Q4", "Q5"))

colnames(mean_share_3) = "quintil"
mean_share_3$share = NA

for (i in 1:nrow(mean_share_3)) {
  df = gastos_ingresos_exp_out %>% filter(quintil %in% i)
  mean_share_3$share[i] = mean(df$share_gasto)
}

# presentación de proporción media
mean_share_3$share = mean_share_3$share*100


#############################################
## Calcular deciles segun la clasificacion ##
##        derivada de la GEIH (2022)       ##
#############################################

# diferenciar los ingresos del hogar segun la clasificacion por 
# deciles y quintiles de la GEIH
deciles = quantile(dataset_2$ingresos, probs = seq(0, 1, by = .1))
quintiles = quantile(dataset_2$ingresos, probs = seq(0, 1, by = .2))

gastos_ingresos_exp = gastos_ingresos_exp %>% mutate(deciles = cut(I_HOGAR, deciles, c("decil 1", "decil 2", "decil 3", "decil 4", "decil 5",
                                                                               "decil 6", "decil 7", "decil 8", "decil 9", "decil 10")))

gastos_ingresos_exp =  gastos_ingresos_exp %>% mutate(quintiles = cut(I_HOGAR, quintiles, c("Q1", "Q2", 
                                                                                        "Q3", "Q4", "Q5")))

#eliminar valores NA porque no se ajustan a los intervalos (ni quintiles ni deciles)
gastos_ingresos_exp = na.omit(gastos_ingresos_exp)


# calculo de proporciones medias del gasto en alimentacion (quintiles)
mean_share_3 = data.frame(c("Q1", "Q2", 
                            "Q3", "Q4", "Q5"))

colnames(mean_share_3) = "quintil"
mean_share_3$share = NA

for (i in 1:nrow(mean_share_3)) {
  df = gastos_ingresos_exp %>% filter(quintiles %in% paste0("Q",i))
  mean_share_3$share[i] = mean(df$share_gasto)
}

mean_share_3$share = mean_share_3$share*100

# calculo de proporciones medias del gasto en alimentacion (deciles)
mean_share = data.frame(c("decil 1", "decil 2", "decil 3",
                          "decil 4", "decil 5", "decil 6",
                          "decil 7", "decil 8", "decil 9",
                          "decil 10"))
colnames(mean_share) = "decil"
mean_share$share = NA

for (i in 1:nrow(mean_share)) {
  df = gastos_ingresos_exp %>% filter(deciles %in% mean_share$decil[i])
  mean_share$share[i] = mean(df$share_gasto)
}

mean_share$share = mean_share$share*100


