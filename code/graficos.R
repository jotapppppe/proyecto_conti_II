# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)


datos_able <- tabla_larga %>% filter(`Estado inicial` == "Able" & Estado_Destino != "0")

# Crear gráfico para el estado inicial Able
able <- ggplot(datos_able, aes(x = Edad, y = Probabilidad, color = Estado_Destino)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Probabilidades de Transición - Estado Inicial: Able",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Able to Mild_Moderate", "Able to Severe_Profound", "Able to Dead")
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
    title = "Probabilidades de Transición - Estado Inicial: Mild_Moderate",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Mild_Moderate to Able", "Mild_Moderate to Severe_Profound", "Mild_Moderate to Dead")
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
    title = "Probabilidades de Transición - Estado Inicial: Severe_Profound",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Severe_Profound to Able", "Severe_Profound to Mild_Moderate", "Severe_Profound to Dead")
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
    title = "Probabilidades de Transición - Estado Inicial: Dead",
    x = "Edad",
    y = "Probabilidad",
    color = "Estado Destino"
  ) +
  scale_color_manual(
    values = c("blue", "purple", "brown"),
    labels = c( "Dead to Able", "Dead to Mild_Moderate", "Dead to Severe_Profound")
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
