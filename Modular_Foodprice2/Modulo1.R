
#################################################
#################################################
## Módulo 1: cargar librerías y bases de datos ##
#################################################
#################################################

#------------------#
# Cargar librerías #-----------------------------------------------------------------------------------------------------------------------
#------------------#
library(here)
library(readxl)
library(tidyverse)
library(knitr)
library(moments)
library(maditr)
library(mice)
library(VIM)
library(dplyr)
library(finalfit)

#-----------------------#
# Cargar bases de datos #-----------------------------------------------------------------------------------------------------------------------
#-----------------------#
Ocupados = read.csv("datos_geih/Ocupados.CSV", sep = ";")

Datos_del_hogar_y_la_vivienda = read.csv("datos_geih/Datos del hogar y la vivienda.CSV", sep = ";")

No_ocupados = read.csv("datos_geih/No ocupados.CSV", sep = ";")

Otros_ingresos_e_impuestos = read.csv("datos_geih/Otros ingresos e impuestos.CSV", sep = ";")

Caracteristicas_generales <- read.csv("datos_geih/Caracteristicas generales, seguridad social en salud y educacion.CSV", sep = ";")











