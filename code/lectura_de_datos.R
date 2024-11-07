tablas_probabilidad_transiciones <- read_excel("data/tablas_probabilidad_transiciones.xlsx", 
                                               range = "A1:F29")
colnames(tablas_probabilidad_transiciones) <- c('Estado inicial', 'Edad', '0', '1', '2', '3')

tablas_probabilidad_transiciones <- tablas_probabilidad_transiciones[order(tablas_probabilidad_transiciones$Edad), ]



lista_tablas_masc <- split(tablas_probabilidad_transiciones[,3:6], 
                      rep(1:7, each = 4))

tablas_probabilidad_transiciones <- read_excel("data/tablas_probabilidad_transiciones.xlsx", 
                                               sheet = "Table2_female", range = "A1:F29")
colnames(tablas_probabilidad_transiciones) <- c('Estado inicial', 'Edad', '0', '1', '2', '3')
tablas_probabilidad_transiciones <- tablas_probabilidad_transiciones[order(tablas_probabilidad_transiciones$Edad), ]


lista_tablas_fem <- split(tablas_probabilidad_transiciones[,3:6], 
                           rep(1:7, each = 4))
