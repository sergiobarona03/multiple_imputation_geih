##############################
## 10-cargar-bases-de-datos ##
##############################
source(here::here("script_asequibilidad/", "00-cargar-librerias.R"))

Ocupados = read.csv("datos_geih/Ocupados.CSV", sep = ";")

Datos_del_hogar_y_la_vivienda = read.csv("datos_geih/Datos del hogar y la vivienda.CSV", sep = ";")

No_ocupados = read.csv("datos_geih/No ocupados.CSV", sep = ";")

Otros_ingresos_e_impuestos = read.csv("datos_geih/Otros ingresos e impuestos.CSV", sep = ";")

Caracteristicas_generales <- read.csv("datos_geih/Caracteristicas generales, seguridad social en salud y educacion.CSV", sep = ";")


