
######################################
## 21-ingresos-corrientes-hogares.R ##
######################################

# merge completo
OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))

# calcular de ingresos corrientes totales por persona
dataset <- mutate(dataset, Ingresos = rowSums(dataset[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))

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
dataset_2 = merge(dataset_2, dataset[c("id", "P6008")], by ="id")
dataset_2 = dataset_2[!duplicated(dataset_2),]
