

###################################################
###################################################
## Módulo 3: distribución de ingresos corrientes ##
###################################################
###################################################

#source(here::here("script_asequibilidad/", "20-multiple-imputation.R"))
source(here::here("script_asequibilidad/", "10-cargar-datos.R"))

# filtro para Cali, Valle del Cauca (código 76)
ocup <-  filter(Ocupados[c("DIRECTORIO","SECUENCIA_P", "ORDEN", "AREA","P6800", "INGLABO")], AREA == 76)

Datos_vivi <- filter(Datos_del_hogar_y_la_vivienda[c("DIRECTORIO","SECUENCIA_P", "AREA","P4030S1A1","P6008")],
                     AREA == 76)

Noocup <- filter(No_ocupados[c("DIRECTORIO","SECUENCIA_P","ORDEN","AREA","P7422S1")],
                 AREA == 76)

Ot_ing <- filter(Otros_ingresos_e_impuestos[c("DIRECTORIO","SECUENCIA_P","ORDEN","AREA","P7500S1A1","P7500S2A1","P7500S3A1")],
                 AREA == 76)

Car_gen <- filter(Caracteristicas_generales[c("DIRECTORIO","SECUENCIA_P","ORDEN","AREA","P3271","P6050","P6040", "P3042", "FEX_C18")],
                  AREA == 76)

ocup = ocup[setdiff(colnames(ocup), "AREA")] 
Datos_vivi = Datos_vivi[setdiff(colnames(Datos_vivi), "AREA")] 
Noocup = Noocup[setdiff(colnames(Noocup), "AREA")] 
Ot_ing = Ot_ing[setdiff(colnames(Ot_ing), "AREA")] 
Car_gen = Car_gen[setdiff(colnames(Car_gen), "AREA")] 



# merge completo
OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))

# calcular de ingresos corrientes totales por persona
dataset <- mutate(dataset, 
                  Ingresos = rowSums(dataset[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")],
                                     na.rm=TRUE))

# construccion de una variable de identificación para los hogares
dataset$id = paste0(dataset$DIRECTORIO,"-",dataset$SECUENCIA_P)

# construccion de base de datos de recepción vacía
dataset_2 = data.frame(levels(as.factor(dataset$id)))
colnames(dataset_2) = "id"
dataset_2$ingresos = NA

hogares_id = levels(as.factor(dataset_2$id))

# bucle para el cálculo de los ingresos para los hogares
for (k in 1:length(hogares_id)) {
  df = data.frame()
  df = dataset %>% filter(id %in% hogares_id[k])
  dataset_2[which(dataset_2$id == hogares_id[k]), 2] = sum(df$Ingresos)
}

# incluir la variable de tamaño de los hogares
dataset_2 = merge(dataset_2, dataset[c("id", "P6008", "FEX_C18")], by ="id")
dataset_2 = dataset_2[!duplicated(dataset_2),]
