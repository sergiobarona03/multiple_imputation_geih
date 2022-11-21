
################################
## 30-asequibilidad-dietas.R  ##
################################
source(here::here("script_asequibilidad/", "23-tablas-cruzadas.R"))


###################################
## Cargar los resultados para el ##
##  costo de cada tipo de dieta  ##
###################################

# dieta de subsistencia
dieta_1 = read_excel("script_asequibilidad/costo_dietas/costo_dieta_subsistencia.xlsx")

# dieta adecuada en nutrientes
dieta_2 = read_excel("script_asequibilidad/costo_dietas/costo_dieta_nutritiva.xlsx")

# dieta recomendada
dieta_3 = read_excel("script_asequibilidad/costo_dietas/costo_dieta_saludable.xlsx")
