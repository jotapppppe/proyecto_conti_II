# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

tablas_probabilidad_transiciones <- tablas_probabilidad_transiciones %>%
  mutate(across(c(`0`, `1`, `2`, `3`), as.numeric))  # Si todas deben ser numéricas

tablas_probabilidad_transiciones <- tablas_probabilidad_transiciones %>%
  mutate(
    `Estado inicial` = as.character(`Estado inicial`),
    Edad = as.numeric(Edad)
  )


tabla_larga <- tablas_probabilidad_transiciones %>%
  pivot_longer(
    cols = c(`0`, `1`, `2`, `3`),  # Incluye nombres de columnas válidos
    names_to = "Estado_Destino",
    values_to = "Probabilidad"
  )


datos_able <- tabla_larga %>% filter(`Estado inicial` == "Able" & Estado_Destino != "0")

# Crear gráfico para el estado inicial Able
able <- ggplot(datos_able, aes(x = Edad, y = Probabilidad, color = Estado_Destino)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Estado Inicial: Sano",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Sano a Enfermo", "Sano a Enfermo Grave", "Sano a Fallecimiento")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(datos_able$Edad), max(datos_able$Edad), 5)) +
  scale_y_continuous(
    limits = c(0, max(datos_able$Probabilidad) * 1.1), 
    labels = scales::percent_format(accuracy = 1)
  )













datos_mild_moderate <- tabla_larga %>% filter(`Estado inicial` == "Mild_Moderate" & Estado_Destino != "1")

# Crear gráfico para el estado inicial 1
mild_moderate <- ggplot(datos_mild_moderate, aes(x = Edad, y = Probabilidad, color = Estado_Destino)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Estado Inicial: Enfermo",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Enfermo a Sano", "Enfermo a Enfermo Grave", "Enfermo a Fallecimiento")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(datos_mild_moderate$Edad), max(datos_mild_moderate$Edad), 5)) +
  scale_y_continuous(
    limits = c(0, max(datos_mild_moderate$Probabilidad) * 1.1), 
    labels = scales::percent_format(accuracy = 1)
  )













datos_severe_profound <- tabla_larga %>% filter(`Estado inicial` == "Severe_Profound" & Estado_Destino != "2")

# Crear gráfico para el estado inicial 2
severe_profound <- ggplot(datos_severe_profound, aes(x = Edad, y = Probabilidad, color = Estado_Destino)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Estado Inicial: Enfermo Grave",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Enfermo Grave a Sano", "Enfermo Grave a Enfermo", "Enfermo Grave a Fallecimiento")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(datos_severe_profound$Edad), max(datos_severe_profound$Edad), 5)) +
  scale_y_continuous(
    limits = c(0, max(datos_severe_profound$Probabilidad) * 1.1), 
    labels = scales::percent_format(accuracy = 1)
  )








datos_dead<- tabla_larga %>% filter(`Estado inicial` == "Dead" & Estado_Destino != "3")

# Crear gráfico para el estado inicial 3
dead <- ggplot(datos_dead, aes(x = Edad, y = Probabilidad, color = Estado_Destino)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Probabilidades de Transición - Estado Inicial: Fallecimiento",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Fallecimiento a Sano", "Fallecimiento a Sano", "Fallecimiento a Enfermo Grave")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(datos_dead$Edad), max(datos_dead$Edad), 5)) +
  scale_y_continuous(
    limits = c(0, max(datos_dead$Probabilidad) * 1.1), 
    labels = scales::percent_format(accuracy = 1)
  )