tablas_probabilidad_transiciones <- read_excel("data/tablas_probabilidad_transiciones.xlsx", 
                                               range = "A1:F29")
colnames(tablas_probabilidad_transiciones) <- c('Estado inicial', 'Edad', '0', '1', '2', '3')

tabla_1 <- tablas_probabilidad_transiciones[1:7,2:6] %>%
  group_by(Edad) %>%
  do({
    # Para cada grupo de edad, generar todas las edades intermedias
    edad_inicial <- .$Edad[1]
    edad_rango <- seq(edad_inicial, edad_inicial + 9)
    datos_expandido <- .[rep(1, length(edad_rango)), ] 
    datos_expandido$Edad <- edad_rango                 
    datos_expandido
  }) %>%
  ungroup()

tabla_2 <- tablas_probabilidad_transiciones[8:14,2:6] %>%
  group_by(Edad) %>%
  do({
    # Para cada grupo de edad, generar todas las edades intermedias
    edad_inicial <- .$Edad[1]
    edad_rango <- seq(edad_inicial, edad_inicial + 9)
    datos_expandido <- .[rep(1, length(edad_rango)), ] 
    datos_expandido$Edad <- edad_rango                 
    datos_expandido
  }) %>%
  ungroup()

tabla_3 <- tablas_probabilidad_transiciones[15:21,2:6] %>%
  group_by(Edad) %>%
  do({
    # Para cada grupo de edad, generar todas las edades intermedias
    edad_inicial <- .$Edad[1]
    edad_rango <- seq(edad_inicial, edad_inicial + 9)
    datos_expandido <- .[rep(1, length(edad_rango)), ] # 
    datos_expandido$Edad <- edad_rango                 
    datos_expandido
  }) %>%
  ungroup()

tabla_4 <- tablas_probabilidad_transiciones[22:28,2:6] %>%
  group_by(Edad) %>%
  do({
    # Para cada grupo de edad, generar todas las edades intermedias
    edad_inicial <- .$Edad[1]
    edad_rango <- seq(edad_inicial, edad_inicial + 9)
    datos_expandido <- .[rep(1, length(edad_rango)), ] 
    datos_expandido$Edad <- edad_rango                 
    datos_expandido
  }) %>%
  ungroup()

tablas_estados <- list(tabla_1, tabla_2, tabla_3, tabla_4)


rm(tabla_1)
rm(tabla_2)
rm(tabla_3)
rm(tabla_4)


lista_tablas <- split(tablas_probabilidad_transiciones[,3:6], 
                      rep(1:4, each = 7))

