# Cargar librerías necesarias
library(tidyverse)
library(ggplot2)

# Importar datos
titanic <- read.csv("Titanicv2.csv")

# 1. Análisis inicial de la estructura de datos
str(titanic)

# 2. Análisis estadístico básico por clase y supervivencia
titanic %>%
  group_by(Pclass, Survived) %>%
  summarise(
    count = n(),
    avg_age = mean(Age, na.rm = TRUE),
    median_fare = median(Fare),
    females = sum(Sex == "female", na.rm = TRUE),
    males = sum(Sex == "male", na.rm = TRUE)
  ) %>%
  ungroup()

# 3. Tasa de supervivencia por género
titanic %>%
  group_by(Sex) %>%
  summarise(
    survival_rate = mean(Survived, na.rm = TRUE),
    total_passengers = n()
  )

# 4. Visualizaciones

# Histograma de edades
ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Edades en el Titanic",
       x = "Edad",
       y = "Frecuencia")

# Boxplot de tarifa por clase y supervivencia
ggplot(titanic, aes(x = factor(Pclass), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(title = "Distribución de Tarifas por Clase y Supervivencia",
       x = "Clase",
       y = "Tarifa",
       fill = "Sobrevivió") +
  scale_fill_discrete(labels = c("No", "Sí"))

# Gráfico de barras de supervivencia por clase y género
ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  facet_wrap(~Sex) +
  labs(title = "Supervivencia por Clase y Género",
       x = "Clase",
       y = "Cantidad de Pasajeros",
       fill = "Sobrevivió") +
  scale_fill_discrete(labels = c("No", "Sí"))

# 5. Análisis de correlación entre variables numéricas
numeric_vars <- titanic %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")
print(numeric_vars)