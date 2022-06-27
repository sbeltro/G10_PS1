################################################################################
# Problem Set 1: Predicting Income
# Autores: Natalia Capacho, Yurani Gonzalez, Sebastian Beltran
# Big Data - MECA 
################################################################################

# Limpiar el espacio de trabajo ----
rm(list=ls())
# Instalar Paquetes y cargar librerias --------
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rio,        # import/export data
       tidyverse,  # tidy-data
       skimr,      # summary data
       caret,      # classification and regression training
       rvest,      # read html
       xml2,       # xml_child
       data.table, # extension de data frame
       stargazer,  # exportar datos 
       ggplot2,    # graficas
       boot,
       devtools,
       Rtools,
       gridExtra,
       dplyr,
       tableone,
       xlsx)
# 1. Adquirir los datos ----
Links = list()
base =  list()
for (i in 1:10){
  Links[[i]] = paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i, ".html")
  base[[i]] = Links[[i]] %>% read_html() %>% html_table()
  colnames(base[[i]][[1]])[1] = "n"
  base[[i]] = base[[i]][[1]] %>% as_tibble()
}

GEIH = rbindlist(l=base , use.names=T , fill=T)

# Reestructurar la forma de ver y construir la base de datos "GEIH" con Tibble
vignette("tibble") 
GEIH <- as_tibble(GEIH)

# Inspeccionar la base de datos
skim(GEIH)
saveRDS(GEIH, "stores/GEIH.rds")
# 2. Limpiar y explorar los datos ----
# 3. Perfil edad-ingresos ----
# 4. La brecha de ingresos ----
#    * Estimar modelo MCO de brecha de ingresos incondicional ----
#    * Estimar modelo MCO de perfil edad-ingreso por genero ----
#    * Salario igual para trabajos iguales ----
#      a. Estimar modelos MCO de brecha de ingresos condicional----
#      b. Usar FWL para hacer la estimacion ----
# 5. Prediccion de ingresos ----
#    a. Dividir la muestra en dos: entrenamiento(70%) y prueba(30%) y estimar modelos ----
#       i.  Estimar modelo sin covariables solo constante ----
#       ii. Estimar los modelos previos ----
#       iii.Otros modelos ----
#       iv. Comparar error de prediccion promedio de todos los modelos ----
#       v.  Leverage ----
#    b. Hacer validacion cruzada en K-iteraciones (K-fold Cross-Validation) ----
#       * Estimar modelo sin covariables solo constante ----
#       * Estimar Modelo de perfil edad-ingresos ----
#       * Estimar modelo MCO de brecha de ingresos incondicional ----
#       * Estimar modelo MCO de brecha de ingresos condicional ----
#       * Estimar otros modelos ----
#        ** Estimar modelo 4 ----
#        ** Estimar modelo 5 ----
#        ** Estimar modelo 6 ----
#        ** Estimar modelo 7 ----
#        ** Estimar modelo 8 ----
#       * Comparar error de prediccion promedio de todos los modelos ----
#    c. Hacer validacion cruzada dejando uno afuera (Leave-One-Out Cross-validation [LOOCV]) ----
#       i.  Escribir lopp ----
#       ii. Comparar resultados ----