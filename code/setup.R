# Librerias
pacman::p_load(
  rmdformats,
  tidyverse,
  dplyr,
  readxl,
  readr,
  ggplot2,
  plotly,
  lubridate,
  xtable,
  expm
)

# Valores iniciales
interes <- 10.88/100

# R scripts
source('code/lectura_de_datos.R')

source('code/funciones.R')

source('code/funciones_actuariales.R')