
##########################
## Análisis de factores ##
##  de expansión        ##
##########################

#########################
## Bases de datos GEIH ##
##   según módulos     ##
#########################
setwd("D:/Desktop/multiple_imputation_geih")

# Módulo de características generales de los hogares
Caracteristicas_generales <- read.csv("datos_geih/Características generales, seguridad social en salud y educación.CSV", sep = ";")
# Módulo de ocupados
Ocupados = read.csv("datos_geih/Ocupados.CSV", sep = ";")
# Módulo de datos del hogar y vivienda
Datos_del_hogar_y_la_vivienda = read.csv("datos_geih/Datos del hogar y la vivienda.CSV", sep = ";")
# Módulo de no-ocupados
No_ocupados = read.csv("datos_geih/No ocupados.CSV", sep = ";")
# Módulo de otros ingresos
Otros_ingresos_e_impuestos = read.csv("datos_geih/Otros ingresos e impuestos.CSV", sep = ";")

##############################
## Merge y recuperación de  ##
##  factores de expansión   ##
##############################

# restricción para Cali
OcupadosF <- filter(Ocupados,AREA == 76)
Datos_del_hogar_y_la_viviendaF <- filter(Datos_del_hogar_y_la_vivienda,AREA == 76)
No_ocupadosF <- filter(No_ocupados,AREA == 76)
Otros_ingresos_e_impuestosF <- filter(Otros_ingresos_e_impuestos,AREA == 76)
Caracteristicas_generalesF <- filter(Caracteristicas_generales,AREA == 76)

# Variables de interés
ocup <- OcupadosF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P6800", "INGLABO")]


Datos_vivi <- Datos_del_hogar_y_la_viviendaF[c("DIRECTORIO","SECUENCIA_P","P4030S1A1","P6008", "FEX_C18")]


Noocup <- No_ocupadosF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P7422S1")]

Ot_ing <- Otros_ingresos_e_impuestosF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P7500S1A1","P7500S2A1","P7500S3A1")]

Car_gen <- Caracteristicas_generalesF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P3271","P6050","P6040", "P3042")]


# merge total 
OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)

OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)

OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)

dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))
View(dataset)

# Análisis de factores de expansión
# Los factores de expansión aparecen por vivienda










