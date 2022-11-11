install.packages("xlsx")
library(xlsx)
library(readxl)
library(tidyverse)
#SUBIENDO LAS BASES

Ocupados <- read_excel("E:\\Escritorio\\NUTRICIÓN\\GEIH_Agosto_marco_2018\\GEIH_Agosto_marco_2018\\CSV2\\CSV\\utilizar_bases_GEIH\\Ocupados.xlsx")
Datos_del_hogar_y_la_vivienda <- read_excel("E:\\Escritorio\\NUTRICIÓN\\GEIH_Agosto_marco_2018\\GEIH_Agosto_marco_2018\\CSV2\\CSV\\utilizar_bases_GEIH\\Datos del hogar y la vivienda.xlsx")
No_ocupados <- read_excel("E:\\Escritorio\\NUTRICIÓN\\GEIH_Agosto_marco_2018\\GEIH_Agosto_marco_2018\\CSV2\\CSV\\utilizar_bases_GEIH\\No ocupados.xlsx")
Otros_ingresos_e_impuestos <- read_excel("E:\\Escritorio\\NUTRICIÓN\\GEIH_Agosto_marco_2018\\GEIH_Agosto_marco_2018\\CSV2\\CSV\\utilizar_bases_GEIH\\Otros ingresos e impuestos.xlsx")
Caracteristicas_generales <- read_excel("E:\\Escritorio\\NUTRICIÓN\\GEIH_Agosto_marco_2018\\GEIH_Agosto_marco_2018\\CSV2\\CSV\\utilizar_bases_GEIH\\Caracteristicas_generales.xlsx")

#Filtros para Cali
OcupadosF <- filter(Ocupados,AREA == 76)

Datos_del_hogar_y_la_viviendaF <- filter(Datos_del_hogar_y_la_vivienda,AREA == 76)

No_ocupadosF <- filter(No_ocupados,AREA == 76)

Otros_ingresos_e_impuestosF <- filter(Otros_ingresos_e_impuestos,AREA == 76)

Caracteristicas_generalesF <- filter(Caracteristicas_generales,AREA == 76)


#SELECCIONAR LAS VARIABLES QUE ME INTERESAN

ocup <- select(OcupadosF,"DIRECTORIO","SECUENCIA_P","ORDEN","INGLABO") #ocupados
dim(ocup)
Datos_vivi <- select(Datos_del_hogar_y_la_viviendaF,"DIRECTORIO","SECUENCIA_P","P4030S1A1","P6008")
Noocup <- select(No_ocupadosF,"DIRECTORIO","SECUENCIA_P","ORDEN","P7422S1")
dim(Noocup)
Ot_ing <- select(Otros_ingresos_e_impuestosF,"DIRECTORIO","SECUENCIA_P","ORDEN","P7500S1A1","P7500S2A1","P7500S3A1")
Car_gen <- select(Caracteristicas_generalesF,"DIRECTORIO","SECUENCIA_P","ORDEN","P3271","P6050","P6040")


#Merge full
OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
dim(OCUP_Noocup)
dim(Datos_vivi_NOOCUP)

OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)

OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)

Basefinal <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))

view(Basefinal)


#Crear columna con suma de ingresos

Basefinal1 <- mutate(Basefinal, Ingresos = rowSums(Basefinal[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))
view(Basefinal1)

write.xlsx(Basefinal1,"Basefinal_GEIH1.xlsx")

quantile(Basefinal1$Ingresos, probs = seq(0.1,0.9, by = 0.1))


Basefinal1$id = paste0(Basefinal1$DIRECTORIO,"-",Basefinal1$SECUENCIA_P)

base_final_2 = data.frame(levels(as.factor(Basefinal1$id)))
colnames(base_final_2) = "id_hogar"
base_final_2$ingresos = NA


hogares_id = levels(as.factor(Basefinal1$id))

for (k in 1:levels(as.factor(Basefinal1$id))) {
  df = data.frame()
  df = Basefinal1 %>% filter(id %in% hogares_id[k])
  base_final_2[which(base_final_2$id_hogar == hogares_id[k]), 2] = sum(df$df.Ingresos)
}

