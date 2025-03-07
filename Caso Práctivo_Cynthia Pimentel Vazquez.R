# Leer el archivo Online Sales Data.csv
install.packages("tidyverse")  # Solo si no lo tienes instalado
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")  # Instalar si no lo tienes

library(tidyr)             # Cargar el paquete
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)

setwd("C:/Users/cynth/OneDrive/Escritorio/2. DIPLOMADO CIENCIA DE DATOS/6. R/Material práctico/Caso Práctico R")

df <- read.csv("Titanicv2.csv")

# Resumen los de los datos

str(df)
summary(df)
names(df)
head(df)       # Primeras filas
#Revisar valores faltantes
colSums(is.na(df))


#Vamos a explorar sobrevivientes por edad
df%>% ggplot(aes(x=Age, fill=Survived))+geom_histogram(alpha=0.7)

df%>% ggplot(aes(x=Survived, y=Age))+geom_point(position = "jitter")


df %>% count(Survived)

#Unicamente sobrevivieron las personas del sexo femenino

df %>% count(Survived,Sex)


df%>% ggplot(aes(x=Sex, y=Survived))+geom_point(position = "jitter")

#Vamos a exploarar Pclass
#No aporta a la clasificación de supervivientes
df%>% ggplot(aes(x=Pclass, y=Survived))+geom_point(position = "jitter")

df %>% count(Survived,Pclass)

#Vamos a explorar embarked
#En general Cherbourg y Queenstow no muestran ingun patron
#pero southampton muestra basatantes no sobrevivientes
df%>% ggplot(aes(x=Embarked, y=Survived))+geom_point(position = "jitter")

df %>% count(Survived,Embarked)


#Vamos a explorar fare
# no se observa ningun patron por el costo del boleto
df%>% ggplot(aes(x=Fare, fill=Survived))+geom_histogram(alpha=0.7)

df %>% 
  count(Survived, Embarked)

        
df %>% 
  group_by(Pclass, Survived) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x=Pclass, y=total, fill=Survived))+
  geom_bar(stat = "identity", position ="dodge")+
  labs(x="Clase Social ", y="Cantidad", title= "Cantidad de muertes por clase social")

df %>% 
  count(Survived, Pclass)      

df %>% 
  group_by(Sex, Survived) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x=Sex, y=total, fill=Survived))+
  geom_bar(stat = "identity", position ="dodge")+
  labs(x="Sexo ", y="Cantidad", title= "Cantidad de muertes por sexo")        
#----------------------------------------------------------------------------------

#Tasa de Supervivencia General
df %>%
  group_by(Survived) %>%
  summarise(Total = n(), 
            Porcentaje = round((n() / nrow(df)) * 100, 2))

# Supervivencia por Clase Social
df %>%
  group_by(Pclass, Survived) %>%
  summarise(Total = n()) %>%
  mutate(Porcentaje = round((Total / sum(Total)) * 100, 2))
#Supervivencia por Género
df %>%
  group_by(Sex, Survived) %>%
  summarise(Total = n()) %>%
  mutate(Porcentaje = round((Total / sum(Total)) * 100, 2))


#Edad Promedio y Mediana por Supervivencia
df %>%
  group_by(Survived) %>%
  summarise(Edad_Promedio = mean(Age, na.rm = TRUE),
            Edad_Mediana = median(Age, na.rm = TRUE),
            Edad_Minima = min(Age, na.rm = TRUE),
            Edad_Maxima = max(Age, na.rm = TRUE))


#Costo del Ticket por Supervivencia
df %>%
  group_by(Survived) %>%
  summarise(Fare_Promedio = mean(Fare, na.rm = TRUE),
            Fare_Mediana = median(Fare, na.rm = TRUE),
            Fare_Maximo = max(Fare, na.rm = TRUE))

#Supervivencia según el Puerto de Embarque
df %>%
  group_by(Embarked, Survived) %>%
  summarise(Total = n()) %>%
  mutate(Porcentaje = round((Total / sum(Total)) * 100, 2))



#Distribución de Edad por Supervivencia
ggplot(df, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de Edad por Supervivencia", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Distribución del Costo del Ticket por Supervivencia
ggplot(df, aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Costo del Ticket por Supervivencia", x = "Supervivencia", y = "Costo del Ticket") +
  theme_minimal()

#Supervivencia según el Puerto de Embarque
df %>%
  group_by(Embarked, Survived) %>%
  summarise(Total = n()) %>%
  ggplot(aes(x = Embarked, y = Total, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Supervivencia por Puerto de Embarque", x = "Puerto", y = "Cantidad de Pasajeros") +
  theme_minimal()

##########################################


df %>% 
  group_by(Pclass) %>% 
  summarise(Total_Pasajeros = n())



#Promedio de edad por clase socioeconómica
df %>% 
  group_by(Pclass) %>% 
  summarise(Promedio_Edad = mean(Age, na.rm = TRUE))

#Mediana de tarifa (Fare) por clase

df %>% 
  group_by(Pclass) %>% 
  summarise(Mediana_Fare = median(Fare, na.rm = TRUE))
#Cantidad de sobrevivientes por género
df %>% 
  group_by(Sex, Survived) %>% 
  summarise(Total = n())


#Edad máxima y mínima de los pasajeros
df %>% 
  summarise(Edad_Min = min(Age, na.rm = TRUE), Edad_Max = max(Age, na.rm = TRUE))
#----------------------------------------------------------------------------------------

#Otras Gráficas


df %>% 
  ggplot(aes(x = Pclass, fill = Pclass)) +
  geom_bar() +
  labs(title = "Cantidad de pasajeros por clase", x = "Clase", y = "Cantidad") +
  theme_minimal()
df %>% 
  ggplot(aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Supervivencia por género", x = "Género", y = "Cantidad") +
  theme_minimal()
df %>% 
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribución de edades de los pasajeros", x = "Edad", y = "Frecuencia") +
  theme_minimal()
df %>% 
  ggplot(aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot() +
  labs(title = "Distribución de tarifas por clase", x = "Clase", y = "Tarifa") +
  theme_minimal()
df %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de edades por supervivencia", x = "Edad", y = "Densidad") +
  theme_minimal()
df %>% 
  ggplot(aes(x = Embarked, fill = Embarked)) +
  geom_bar() +
  labs(title = "Cantidad de pasajeros por puerto de embarque", x = "Puerto", y = "Cantidad") +
  theme_minimal()

