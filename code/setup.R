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
interes <- 0.05

# R scripts
source('code/lectura_de_datos.R')

source('code/funciones.R')

source('code/funciones_actuariales.R')