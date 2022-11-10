
###################
## An?lisis GEIH ##
###################

library(readxl)
library(tidyverse)

# directorio hace referencia al n?mero de vivienda
# secuencia hace referencia al n?mero del hogar
# orden hace referencia a la persona dentro del hogar

# valores NA en la muestra total

ocupados_geih = ocupados[c("DIRECTORIO", "SECUENCIA_P",
                           "ORDEN", "HOGAR", "AREA", "INGLABO")]

na_ingresos_geih = ocupados_geih %>% filter(is.na(ocupados_geih$INGLABO))
ingresos_nulos_geih= ocupados_geih %>% filter(ocupados_geih$INGLABO %in% 0)

#####################
## Muestra de Cali ##
#####################

# valores en NA en la muestra de Cali

ocupados = read.csv2("datos_geih/Ocupados.CSV", sep = ";")
ocupados_cali = ocupados %>% filter(AREA %in% 76)

ocupados_cali = ocupados_cali[!is.na(ocupados_cali$AREA),]

ocupados_ingresos = ocupados_cali[c("DIRECTORIO", "SECUENCIA_P",
                               "ORDEN", "HOGAR", "AREA", "INGLABO")]

na_ingresos = ocupados_ingresos %>% filter(is.na(ocupados_ingresos$INGLABO))
ingresos_nulos = ocupados_ingresos %>% filter(ocupados_ingresos$INGLABO %in% 0)

# se considera el siguiente conjunto de variables:
# P6500: Antes de descuentos, cuánto ganó el mes pasado?
# P6510S2: El mes pasado recibió ingresos por horas extras?
# P6510S1: Cuánto recibió?

keep = c("DIRECTORIO", "SECUENCIA_P",
         "ORDEN", "HOGAR", "AREA", "INGLABO",
         "P6500", "P6510S2", "P6510S1")

ocupados_cali = ocupados %>% filter(AREA %in% 76)
ocupados_cali = ocupados_cali[keep]


plot(ocupados_cali$INGLABO, type = "l", col = "red") + lines(ocupados_cali$P6500, col = "green")

ocupados_cali$diff = ocupados_cali$INGLABO - ocupados_cali$P6500

plot(ocupados_cali$diff, type = "l")
