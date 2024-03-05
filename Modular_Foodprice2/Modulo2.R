

#########################################################
#########################################################
## Módulo 2: imputación múltiple de ingresos laborales ##
#########################################################
#########################################################

source(here::here("script_asequibilidad/", "10-cargar-datos.R"))

#-----------------------------------#
# Preparación de las bases de datos #----------------------------------------------------------------------------------------------------------------
#-----------------------------------#
# filtro para Cali, Valle del Cauca (código 76) y variables de interés
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

#-----------------------------------------#
# Selección de base de datos para imputar #
#-----------------------------------------#
dataset_imputar = merge(ocup, Car_gen, all.x = TRUE)
dataset_imputar = dataset_imputar[c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "INGLABO", "P6040", "P3271", "P3042", "P6800")]

dataset_imputar$id = paste0(dataset_imputar$DIRECTORIO,"-",dataset_imputar$SECUENCIA_P,"-",dataset_imputar$ORDEN)
dataset_imputar = dataset_imputar[c("id","INGLABO", "P6040", "P3271", "P3042", "P6800")]
colnames(dataset_imputar) = c("id_hogar", "ing", "edad", "sexo", "estudio", "horas")
dataset_imputar$edad_sqr = dataset_imputar$edad*dataset_imputar$edad

# determinar la clase de cada variable
dataset_imputar$id_hogar = as.character(dataset_imputar$id_hogar)
dataset_imputar$ing = as.numeric(dataset_imputar$ing)
dataset_imputar$edad = as.numeric(dataset_imputar$edad)
dataset_imputar$sexo = as.factor(dataset_imputar$sexo)
dataset_imputar$estudio = as.factor(dataset_imputar$estudio)
dataset_imputar$horas = as.numeric(dataset_imputar$horas)

# Guardar la base de datos input para la imputación múltiple
saveRDS(dataset_imputar, here("resultados_generales/dataset_imputar.RDS"))

#--------------------------------------#
#        Imputación múltiple           #
#  (Coincidencia de media predictiva)  #----------------------------------------------------------------------------------------------------------------
#--------------------------------------#
imputed_ocupados = mice(dataset_imputar, m=5, maxit = 50, method = 'pmm')

# Creación de la base de datos de recepción
lista_datasets = list()
length(lista_datasets) = 5
lista_datasets[[1]] = complete(imputed_ocupados,1)
lista_datasets[[2]] = complete(imputed_ocupados,2)
lista_datasets[[3]] = complete(imputed_ocupados,3)
lista_datasets[[4]] = complete(imputed_ocupados,4)
lista_datasets[[5]] = complete(imputed_ocupados,5)

OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))

# Cálculo de los ingresos corrientes por personas
dataset = mutate(dataset,
                 Ingresos = rowSums(dataset[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))

# Variable de identificación para los hogares
dataset$id = paste0(dataset$DIRECTORIO,"-",dataset$SECUENCIA_P)

# Base de datos de recepción vacía
dataset_2 = data.frame(levels(as.factor(dataset$id)))
colnames(dataset_2) = "id"
dataset_2$ingresos = NA
hogares_id = levels(as.factor(dataset_2$id))

# Bucle para el cálculo de los ingresos para los hogares
for (k in 1:length(hogares_id)) {
  df = data.frame()
  df = dataset %>% filter(id %in% hogares_id[k])
  dataset_2[which(dataset_2$id == hogares_id[k]), 2] = sum(df$Ingresos)
}

# Cálculo de deciles
deciles = quantile(dataset_2$ingresos, probs = seq(.1, .9, by = .1))

# Construcción de bases de datos de recepción:
deciles_imputacion = data.frame(names(deciles), deciles)
colnames(deciles_imputacion) = c("Decil", "Dataset_0")

# Merge entre base de datos imputados y no-imputados
for (l in 1:length(lista_datasets)) {
  # merge completo
  df_1 = ocup
  df_1$id_hogar = paste0(df_1$DIRECTORIO,"-",df_1$SECUENCIA_P,"-",df_1$ORDEN)
  df_2 = lista_datasets[[l]]
  df_2 = df_2[c("id_hogar", "ing")]
  
  df_1 = merge(df_1, df_2, by = "id_hogar", all.x = TRUE)
  df_1$INGLABO = df_1$ing
  df_1 = df_1[c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6800", "INGLABO")]
  
  OCUP_Noocup <- merge(df_1,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))
  
  # cálculo de ingresos corrientes totales por persona
  dataset <- mutate(dataset, Ingresos = rowSums(dataset[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))
  
  #construcción de una variable de identificación para los hogares
  dataset$id = paste0(dataset$DIRECTORIO,"-",dataset$SECUENCIA_P)
  
  #construcción de base de datos de recepción vacía
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
  
  # cálculo de deciles
  deciles_2 = quantile(dataset_2$ingresos, probs = seq(.1, .9, by = .1))
  
  # construcción de bases de datos para conjugar:
  df_3 = data.frame(names(deciles_2), deciles_2)
  colnames(df_3) = c("Decil", paste0("Dataset_",l))
  
  deciles_imputacion = merge(deciles_imputacion, df_3, by = "Decil")
}

#############################################
#############################################
##   Contraste: base de datos original     ##
##   vs bases de datos imputadas a partir  ##
##        de diagramas de caja             ##
#############################################
#############################################

# merge total
OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))

# cálculo de ingresos corrientes totales por persona
dataset <- mutate(dataset, Ingresos = rowSums(dataset[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))

#construcción de una variable de identificación para los hogares
dataset$id = paste0(dataset$DIRECTORIO,"-",dataset$SECUENCIA_P)

#construcción de base de datos de recepción vacía
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

dataset_boxplot = dataset_2
colnames(dataset_boxplot) = c("id", paste0("Dataset_",0))


for (l in 1:length(lista_datasets)) {
  # merge completo
  df_1 = ocup
  df_1$id_hogar = paste0(df_1$DIRECTORIO,"-",df_1$SECUENCIA_P,"-",df_1$ORDEN)
  df_2 = lista_datasets[[l]]
  df_2 = df_2[c("id_hogar", "ing")]
  
  df_1 = merge(df_1, df_2, by = "id_hogar", all.x = TRUE)
  df_1$INGLABO = df_1$ing
  df_1 = df_1[c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6800", "INGLABO")]
  
  OCUP_Noocup <- merge(df_1,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))
  
  # cálculo de ingresos corrientes totales por persona
  dataset <- mutate(dataset, Ingresos = rowSums(dataset[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))
  
  #construcción de una variable de identificación para los hogares
  dataset$id = paste0(dataset$DIRECTORIO,"-",dataset$SECUENCIA_P)
  
  #construcción de base de datos de recepción vacía
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
  
  colnames(dataset_2) = c("id", paste0("Dataset_",l))
  dataset_boxplot = merge(dataset_boxplot, dataset_2, by = "id")
}






