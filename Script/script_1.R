# Cálculo de indicadores financieros
#Instalar paquetes----
install.packages("openxlsx")
install.packages("missForest")
install.packages("ggplot2")

#cargar paquetes----
library(missForest)
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(palmerpenguins)
library(purrr)
library(ggplot2)


# Importacion de datos ----
balances_2014 <- read_excel("Data/balances_2014.xlsx",sheet = "v2014_activo1")
names(balances_2014)

#Declaracion de variables
# Liquidez Corriente ----
liquidez_corriente <- balances_2014 %>%
  mutate(v345 = coalesce(v345, 0),
         v539 = coalesce(v539, 0)) %>%
  mutate(liquidez_corriente = ifelse(v539 != 0 & is.finite(v345 / v539), round(v345 / v539, 5), NA))

# Endeudamiento del activo ----
endeudamiento_activo <- balances_2014 %>% 
    mutate(v599 = coalesce(v599, 0),
           v499 = coalesce(v499, 0)) %>%
    mutate(endeudamiento_activo_1 = ifelse(v499 == 0, NA, round(v599 / v499, 5)))


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
# ciiu4_nivel1 y ciiu4_nivel6 son variables categóricas representan la clasificación de la actividad económica de la empresa.

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
  summarise(liquidez_tipo = sum(liquidez_corriente, na.rm = TRUE)) %>% 
  view("Liquidez")


#Describe el top 10 de empresas con mayor apalancamiento.
top10_apalancamiento <- apal %>% 
  arrange(desc(apalancamiento)) %>% 
  head(10) %>% 
  select(nombre_cia, apalancamiento) %>% 
  view("Top_10_mayor_apalancamiento")


# Parte 3 ----
#1.Utilizando los datos en el archivo balance_2014.xlxs generar un tibble
#denonimado empresas con las siguientes variables:
#empresas,status,tipo_de_empresa,pais,provincia,canton,ciudad,
#actividad_economica,subactividad,liquidez_corriente, endeudamiento_del_activo,
#endeudamiento_patrimonial,endeudamiento_activo_fijo,apalancamiento.

#codigo para punto 1 parte 3
#Cambio de nombre de columnas 
balances_2014_v1<-balances_2014 %>% 
  rename("Empresas"="nombre_cia","Status"="situacion","Tipo_de_empresa"="tipo",
         "Actividad_economica"="ciiu4_nivel1","Subactividad"="ciiu4_nivel6"
         ) %>%  view("Tabla_1")

#seleccionar solo columnas de interes de frame balances_2014
select_balances_v1<-select(balances_2014_v1,"Empresas","Status","Tipo_de_empresa",
                        "pais","provincia","canton","ciudad","Actividad_economica",
                        "Subactividad") %>% 
                          view("Tabla_columnas_seleccionadas")


#################esto para mejorar###################
# seleccionar columna de liquidez corriente
select_liqu_1<- liquidez_corriente[,c(3,348)] %>% 
  view('liquidez_corriente')
# seleccionar columna de endeudamiento activo
select_act_1<- endeudamiento_activo[,c(3,348)] %>% 
  view('endeudamiento_activo')
# seleccionar columna de endeudamiento patrimonial
select_pat_1<- endeudamiento_patrimonial[,c(3,348)] %>% 
  view('endeudamiento_activo')
# seleccionar columna de endeudamiento activo fijo 
select_actfij_1<- endeudamiento_activof[,c(3,348)] %>% 
  view('endeudamiento_activo_fijo')
# seleccionar columna de apalancamiento
select_apa_1<- apal[,c(3,348)] %>% 
  view('apalancamiento')

#unimos las tablas en 1 sola tabla nueva tabla_2_calculos
# List of data frames to join
dfs_list <- list(
  select_liqu_1,
  select_act_1,
  select_pat_1,
  select_actfij_1,
  select_apa_1)

# left joins usando reduce
tabla_2_calculos <- dfs_list %>%
  reduce(left_join, 
         by = c("nombre_cia")) %>%
  view("tabla_completa_prueba")

#join entre tablas: select_balances_v2 y tabla_2_calculos
join_empresas<-select_balances_v1 %>% 
  left_join(tabla_2_calculos,by=c("Empresas"="nombre_cia")) %>% 
  view("tabla_compilada_Empresas")

###########       tibble 
Empresas<-as.tibble(join_empresas) %>% 
  view("Empresas")
head(Empresas)


#2.Crea una tabla resumiendo el numero total de empresas por actividad economica
# y por cada canton. la tabla simplemente debe aparecer 
#como una data frame o tibble en tu script.

summary_empresas <- Empresas %>%
  group_by(Actividad_economica, canton) %>%
  summarize(Total_empresas = n()) %>% 
  view("summary_empresas")

#3.Graficamente muestra el comparartivo de los indicadoes financieros de liquidez
#y solvencia por status y provincia.

indicador_financiero_1<-Empresas %>% 
  group_by(provincia, Status)%>% 
  summarise_at(vars(liquidez_corriente,endeudamiento_patrimonial,
                   endeudamiento_activo1),
                   list( Promedio=~mean(.,na.rm =T))) %>% view("Resumen_1")
#falta el grafico

#Graficamente muestra el comparativo de los indicadores financieros de liquidez
# y solvencia por tipo de empresa.
indicador_financiero_2<-Empresas %>% 
group_by(Tipo_de_empresa)%>% 
 summarise_at(vars(liquidez_corriente,endeudamiento_patrimonial,
                    endeudamiento_activo1),
                    list( Promedio=~mean(.,na.rm =T))) %>% view("Resumen_2")
#falta el grafico

