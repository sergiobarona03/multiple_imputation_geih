######################################################
######################################################
## Módulo 5: distribución del gasto en alimentación ##
######################################################
######################################################

source(here::here("script_asequibilidad/", "21-ingresos-corrientes-hogares.R"))

# recodificar los ingresos corrientes de los hogares según deciles
deciles = quantile(dataset_2$ingresos, probs = seq(.1, .9, by = .1))
dataset_2 = dataset_2 %>% mutate(deciles = cut(ingresos, c(0, deciles, Inf),
                                               c("decil 1", "decil 2", "decil 3", "decil 4", "decil 5",
                                                 "decil 6", "decil 7", "decil 8", "decil 9", "decil 10")))

# trabajar con la base de datos sin valores NA (vease el analisis de imputacion)
dataset_2_na = dataset_2[!is.na(dataset_2$deciles),]

# Determinar la proporción del gasto
deciles_gasto = data.frame(levels(as.factor(dataset_2_na$deciles)))
colnames(deciles_gasto) = "deciles"
deciles_gasto$share = c( 0.31478, 
                         0.16449, 0.14856, 
                         0.144, 0.1322, 
                         0.11439, 0.11522, 
                         0.10748,  0.09165,  
                         0.08167)

#####################################################
# Determinar el ingreso dedicado a la alimentación ##
#####################################################

# asignar la proporción a cada hogar
dataset_2_na$id_aux = c(1:nrow(dataset_2_na))
dataset_2_na = merge(dataset_2_na, deciles_gasto, by = "deciles", all.x = TRUE)
dataset_2_na = dataset_2_na[order(dataset_2_na$id_aux),]

dataset_2_na = dataset_2_na[setdiff(colnames(dataset_2_na), "id_aux")]

# calcular ingreso dedicado a alimentacion
dataset_2_na$ingreso_alimentos = dataset_2_na$share*dataset_2_na$ingresos

###################################################
## Calcular la nueva distribución por deciles    ##
## para el gasto en alimentación de los hogares  ##
###################################################
deciles_gasto$ingresos_corrientes = quantile(dataset_2$ingresos, probs = seq(0.1,1,by = 0.1))
deciles_gasto$gasto_alimentacion = deciles_gasto$ingresos_corrientes*deciles_gasto$share


#####################################################
## Determinar la proporción del gasto en quintiles ##
#####################################################







