# Análisis Financiero de Empresas - Proyecto

## Data

Este proyecto tiene como objetivo analizar los datos financieros de un conjunto de empresas en el año 2014. Se realizará una exploración y cálculo de varios indicadores financieros importantes para comprender el desempeño y la salud financiera de estas empresas. A continuación, se detalla la estructura del proyecto y el análisis realizado paso a paso:

-   Importación de Datos: En esta sección, se cargan los datos de los balances financieros de las empresas desde un archivo Excel llamado "balances_2014.xlsx". Se utilizan las librerías dplyr y DT.

-   Declaración de Variables: En esta parte, se calculan y transforman diferentes indicadores financieros como la liquidez corriente, el endeudamiento del activo, el endeudamiento patrimonial, el endeudamiento del activo fijo y el apalancamiento. Estos cálculos se basan en diferentes columnas del dataset de balances financieros.

-   Análisis Exploratorio de Datos: Se realiza un análisis exploratorio para conocer la estructura del dataset. Se muestran el número de observaciones y variables, los tipos de datos de las variables y una descripción de las variables más relevantes.

-   Análisis del Endeudamiento del Activo por Tamaño de Empresa: Se realiza un análisis para determinar si el endeudamiento del activo es mayor en empresas micro + pequeñas en comparación con las grandes empresas.

-   Análisis de la Liquidez por Tipo de Compañía: Se analiza si la liquidez es diferente entre las empresas que tienen más de 60 trabajadores directos y que cuentan con 100 a 800 trabajadores administrativos.

-   Análisis del Top 10 de Empresas con Mayor Apalancamiento: Se identifican las 10 empresas con el mayor apalancamiento financiero.

-   Creación de una Tabla con Datos Relevantes: Se crea un tibble llamado "Empresas" con las variables más relevantes, incluyendo información de empresas, estado, tipo de empresa, país, provincia, cantón, ciudad, actividad económica, subactividad y los indicadores financieros calculados anteriormente.

-   Resumen del Número Total de Empresas por Actividad Económica y Cantón: Se resume el número total de empresas por actividad económica y por cada cantón en una tabla.

-   Gráficos Comparativos de Indicadores Financieros por Estado y Provincia: Se realiza un análisis gráfico para comparar los indicadores financieros de liquidez y solvencia por estado y provincia.

## **Análisis y tareas específicas**

1.  Importación de Datos: En esta sección, se cargan los datos de los balances financieros de las empresas desde un archivo Excel llamado "balances_2014.xlsx". Se utilizan las librerías dplyr y DT.

2.  Declaración de Variables: En esta parte, se calculan y transforman diferentes indicadores financieros como la liquidez corriente, el endeudamiento del activo, el endeudamiento patrimonial, el endeudamiento del activo fijo y el apalancamiento. Estos cálculos se basan en diferentes columnas del dataset de balances financieros.

3.  Análisis Exploratorio de Datos: Se realiza un análisis exploratorio para conocer la estructura del dataset. Se muestran el número de observaciones y variables, los tipos de datos de las variables y una descripción de las variables más relevantes.

4.  Análisis del Endeudamiento del Activo por Tamaño de Empresa: Se realiza un análisis para determinar si el endeudamiento del activo es mayor en empresas micro + pequeñas en comparación con las grandes empresas.

5.  Análisis de la Liquidez por Tipo de Compañía: Se analiza si la liquidez es diferente entre las empresas que tienen más de 60 trabajadores directos y que cuentan con 100 a 800 trabajadores administrativos.

6.  Análisis del Top 10 de Empresas con Mayor Apalancamiento: Se identifican las 10 empresas con el mayor apalancamiento financiero.

7.  Creación de una Tabla con Datos Relevantes: Se crea un tibble llamado "Empresas" con las variables más relevantes, incluyendo información de empresas, estado, tipo de empresa, país, provincia, cantón, ciudad, actividad económica, subactividad y los indicadores financieros calculados anteriormente.

8.  Resumen del Número Total de Empresas por Actividad Económica y Cantón: Se resume el número total de empresas por actividad económica y por cada cantón en una tabla.

9.  Gráficos Comparativos de Indicadores Financieros por Estado y Provincia: Se realiza un análisis gráfico para comparar los indicadores financieros de liquidez y solvencia por estado y provincia.
