# Cálculo de indicadores financieros

install.packages("openxlsx")
install.packages("missForest")
library(missForest)
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(palmerpenguins)
library(missForest)

# Importacion de datos ----
balances_2014 <- read_excel("Data/balances_2014.xlsx",sheet = "v2014_activo1")
names(balances_2014)

# Liquidez Corriente ----

liquidez_corriente <- balances_2014 %>%
  mutate(v345 = coalesce(v345, 0),
         v539 = coalesce(v539, 0)) %>%
  mutate(liquidez_corriente = ifelse(is.finite(v345 / v539), round(v345 / v539, 5), NA))


# Endeudamiento del activo ----

endeudamiento_activo <- 
  balances_2014 %>% 
    mutate(v599 = coalesce(v599, 0),
           v499 = coalesce(v499, 0)) %>%
    mutate(endeudamiento_activo1 = ifelse(v499 == 0, NA, round(v599 / v499, 5)))

# Endeudamiento Patrimonial ----

patrimonio <- balances_2014 %>%
  transmute(patrimonio = round(coalesce(v499 - v599, NA), 5))

endeudamiento_patrimonial <- balances_2014 %>% 
  mutate(endeudamiento_patrimonial = round(v599 / v499, 5)) %>%
  na.omit() %>%
  filter(!is.infinite(endeudamiento_patrimonial))


# Endeudamiento Activo Fijo ----

endeudamiento_activof <- data.frame(balances_2014 %>% 
  mutate(endeudamiento_activof = patrimonio/v498))

# Apalancamiento ----
apal <- balances_2014 %>%
  mutate(apalancamiento = v499 / patrimonio) %>%
  mutate(apalancamiento = replace(apalancamiento, is.na(apalancamiento), 0)) %>%
 mutate(apalancamiento = round(apalancamiento, 5))

# Parte 1----

dim(balances_2014)
# El data frame balances_2014 contiene 47033 observaciones y 347 variables.
sapply(balances_2014, class)
# Las variables contenidas en balances_2014 son tipo numéricas y caracteres.
str(balances_2014)
# A continuación se describira las variables que a nuestro criterio son las más relevantes:
# La variable expediente es una variable numérica es el identificador.
# RUC es una variable categórica representa al Registro Único de Contribuyente.
# nombre_cia es una variable categórica contiene el nombre de cada empresa.
# situación es una variable categórica indica si la empresa esta activa o no.
# tipo es una variable categórica idica el tipo de entidad 
# ciiu4_nivel1 y ciiu4_nivel 6 son variables categóricas representan la clasificación de la actividad económica de la empresa.
names(balances_2014)
# Las variables contenidas son: expediente,ruc,nombre_cia,situacion,tipo,fecha_const,pais,provincia,canton,ciudad,ciiu4_nivel1,ciiu4_nivel6,
#trab_direc,trab_admin,trab_produc,trab_otros, tamanio, anio. Seguidas de las variables de valor para los estados fincieros de van desde la v311 a v7593.

# Parte 2 ----
print(balances_2014)
names(endeudamiento_activo)
#¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?

unique(endeudamiento_activo$tamanio)

resultado <- endeudamiento_activo %>%
  transmute(
    nuevo_endeudamiento = ifelse(tamanio == "PEQUEÑA", endeudamiento_activo1, NA),
    PEQUEÑA = nuevo_endeudamiento,
    nuevo_endeudamiento = ifelse(tamanio == "GRANDE", endeudamiento_activo1, NA),
    GRANDE = nuevo_endeudamiento,
    nuevo_endeudamiento = ifelse(tamanio == "MICRO", endeudamiento_activo1, NA),
    MICRO = nuevo_endeudamiento
  ) %>%
  select(-nuevo_endeudamiento)


resultado <- resultado %>%
  replace_na(list(MICRO = 0, PEQUEÑA = 0, GRANDE =0)) %>%
  mutate(PYMES = MICRO + PEQUEÑA)%>%
  group_by(dummy = 1) %>%
  summarise(E.ACTIVO_PYME = sum(PYMES), E.ACTIVO_GRANDE = sum(GRANDE))

#RESPUESTA: El endeudamiento del activo es mayor en las micro más pequeñas empresas que en las grandes.


#¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de 
#60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

unique(liquidez_corriente$trab_direc)
unique(liquidez_corriente$tipo)

datos_filtrados <- liquidez_corriente %>%
  filter(trab_direc > 60, trab_admin >= 100, trab_admin <= 800)

liquidez<- datos_filtrados %>%
  group_by(tipo) %>%
  summarise(liquidez_tipo = sum(liquidez_corriente, na.rm = TRUE))


#Describe el top 10 de empresas con mayor apalancamiento.
top10_apalancamiento <- apal %>% 
  arrange(desc(apalancamiento)) %>% 
  head(10) %>% 
  select(nombre_cia, apalancamiento)


# Parte 3 ----
