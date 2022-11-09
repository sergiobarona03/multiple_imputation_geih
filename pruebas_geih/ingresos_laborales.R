
###################
## Análisis GEIH ##
###################

library(readxl)
library(tidyverse)

# directorio hace referencia al número de vivienda
# secuencia hace referencia al número del hogar
# orden hace referencia a la persona dentro del hogar

ocupados = read.csv2("datos_geih/Ocupados.CSV", sep = ";")
ocupados_cali = ocupados %>% filter(AREA %in% 76)

ocupados_cali = ocupados_cali[!is.na(ocupados_cali$AREA),]

ocupados_ingresos = ocupados_cali[c("DIRECTORIO", "SECUENCIA_P",
                               "ORDEN", "HOGAR", "AREA", "INGLABO")]

na_ingresos = ocupados_ingresos %>% filter(is.na(ocupados_ingresos$INGLABO))
ingresos_nulos = ocupados_ingresos %>% filter(ocupados_ingresos$INGLABO %in% 0)


# valores NA en la muestra total

ocupados_geih = ocupados[c("DIRECTORIO", "SECUENCIA_P",
                                "ORDEN", "HOGAR", "AREA", "INGLABO")]

na_ingresos_geih = ocupados_geih %>% filter(is.na(ocupados_geih$INGLABO))
ingresos_nulos_geih= ocupados_geih %>% filter(ocupados_geih$INGLABO %in% 0)
