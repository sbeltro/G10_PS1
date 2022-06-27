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

GEIH = readRDS("stores/GEIH.rds")

# Filtrar la base de datos para considerar unicamente empleados, mayores de 18 y que trabajen en Bogota
BASE_I <- subset(GEIH,subset = age>18 & dsi==0)

# Manipular variables de interes 

## Correccion de los tipos de variable 
variables_categoricas <- c(
  "cclasnr11", "cclasnr2", "cclasnr3", "cclasnr4", "cclasnr5",
  "cclasnr6", "cclasnr7", "cclasnr8", "clase", "college",
  "cotPension", "cuentaPropia", "depto","dsi", "formal", "ina", "inac", 
  "informal", "microEmpresa", "ocu", "p6090", "p6100", "p6210", 
  "p6240", "p6510", "p6510s2", "p6545", "p6545s2", "p6580", "p6580s2", 
  "p6585s1",  "p6585s1a2", "p6585s2",   "p6585s2a2", "p6585s3", 
  "p6585s3a2", "p6585s4", "p6585s4a2", "p6590", "p6600", "p6610", 
  "p6620", "p6630s1", "p6630s2", "p6630s3", "p6630s4", "p6630s6", 
  "p6920", "p7040", "p7050", "p7090", "p7110", "p7120", "p7140s1", 
  "p7140s2", "p7150",  "p7160", "p7310", "p7350", "p7422", "p7472", "p7495", 
  "p7500s1", "p7500s2", "p7500s3", "p7505", "p7510s1", 
  "p7510s2", "p7510s3", "p7510s5", "p7510s6", "p7510s7", 
  "pea", "pet", "regSalud", "relab", "sex", "wap")

BASE_I <- BASE_I %>%
  mutate(p6050 = factor(p6050, levels=c(1,2,3,4,5,6,9)))

BASE_I <- BASE_I %>%
  mutate(maxEducLevel = factor(maxEducLevel, levels=c(1,2,3,4,5,6,7,9)))

BASE_I <- BASE_I %>%
  mutate(sizeFirm = factor(sizeFirm, levels=c(1,2,3,4,5)))

BASE_I <- BASE_I %>%
  mutate(oficio = factor(oficio, levels=c(1:99)))

BASE_I <- BASE_I %>%
  mutate(p6090 = factor(p6090, levels=c(1,2,9)))

BASE_I <- BASE_I %>% 
  mutate(sex = case_when(sex == 1 ~ 0, 
                         sex == 0 ~ 1))

# Transformar variables en logicas
BASE_I <- as.data.frame(BASE_I)
for (i in variables_categoricas){
  BASE_I[,i] = as.numeric(BASE_I[,i])
  BASE_I[,i] = as.logical(BASE_I[,i])
}

# Reemplazar los missings de las variables
recode_vars = c("oficio", "y_horasExtras_m", "y_bonificaciones_m", "y_auxilioAliment_m",
                "y_auxilioTransp_m", "y_subFamiliar_m", "y_subEducativo_m", "y_primaServicios_m",
                "y_primaNavidad_m", "y_primaVacaciones_m", "y_primas_m", "y_viaticos_m",
                "y_accidentes_m", "y_total_m")

BASE_I <- as.data.frame(BASE_I)
for (i in recode_vars){
  BASE_I[,i] = ifelse(test = is.na(BASE_I[,i]) == T , yes = 0 , BASE_I[,i])
}

BASE_I <- as_tibble(BASE_I)

## Recodificar variable de seguridad social
BASE_I$p6090 <- with(BASE_I, ifelse(p6090 == 1,
                                    yes = 1, 
                                    no = 0))

## Recodificar variable formal
BASE_I <- BASE_I %>%
  mutate(formal = ifelse(test = is.na(formal) == T,
                         yes = p6090,
                         no = formal))

## Recodificar variable ingreso intereses o dividendos
BASE_I <- BASE_I %>%
  mutate(iof1es = ifelse(test = is.na(iof1es) == T,
                         yes = iof1,
                         no = iof1es))

## Recodificar variable ingreso jubilaciones y pensiones
BASE_I <- BASE_I %>%
  mutate(iof2es = ifelse(test = is.na(iof2es) == T,
                         yes = iof2,
                         no = iof2es))

## Recodificar ingreso arriendos 
BASE_I <- BASE_I %>%
  mutate(y_vivienda_m = ifelse(test = is.na(y_vivienda_m) == T,
                               yes = p7500s1a1,
                               no = y_vivienda_m))

# Construccion de otras variables de interes

## Construir variable de microempresa
BASE_I <- BASE_I %>% 
  mutate(Micro_empresa = ifelse(sizeFirm == 1 | sizeFirm == 2 | sizeFirm == 3,
                                yes = 1,
                                no = 0))

## Borrar observaciones para las que microempresa tiene missings
BASE <- BASE_I %>% subset(is.na(Micro_empresa)==F)

## Construir variable anos de educacion 

## Recodificar variable de maximo nivel educacion
BASE <- BASE %>%
  mutate(maxEducLevel = ifelse(test = is.na(maxEducLevel)==T,
                               yes = 9,
                               no = maxEducLevel))

BASE <- BASE %>% 
  mutate(educ = case_when(maxEducLevel == 1 | maxEducLevel == 9 ~ 0, 
                          maxEducLevel == 2 ~ 3, 
                          maxEducLevel == 3 ~ 7, 
                          maxEducLevel == 4 ~ 8,
                          maxEducLevel == 5 ~ 13,
                          maxEducLevel == 6 ~ 14,
                          maxEducLevel == 7 ~ 19))

## Construir variable experiencia laboral (experiencia potencial)
BASE <- BASE %>% 
  mutate(experiencia = case_when(educ == 0 & age > 18 ~ age - 18,
                                 educ > 0 & age >= 18 & age <= 22 ~ age - 18, 
                                 educ > 0 & age > 22 ~ age - educ - 6),
         experiencia2 = experiencia^2)

BASE <- BASE %>%
  mutate(experiencia = ifelse(test = experiencia < 0,
                              yes = 0,
                              no = experiencia),
         experiencia2 = ifelse(test = experiencia2 < 0,
                               yes = 0,
                               no = experiencia2))

## Construir variable jefe de hogar 
BASE = BASE %>% 
  mutate(jefeHogar = case_when(p6050 == 1 ~ 1, 
                               p6050 == 2 ~ 0, 
                               p6050 == 3 ~ 0, 
                               p6050 == 4 ~ 0,
                               p6050 == 5 ~ 0,
                               p6050 == 6 ~ 0,
                               p6050 == 9 ~ 0))

## Construir variable de ayuda de hogares
BASE <- BASE %>% 
  mutate(y_ayudaHogares = p7510s1a1 + p7510s2a1)

## Construir variable de primas 
BASE <- BASE %>% 
  mutate(primas = y_primaServicios_m + y_primaNavidad_m + y_primaVacaciones_m)

## Recodificar ingreso por primas construida
BASE <- BASE %>%
  mutate(primas = ifelse(primas == 0,
                         yes = y_primas_m,
                         no = primas))

## Construir edad al cuadrado 
BASE <- BASE %>% 
  mutate(edad2 = age^2)

# Renombrar variables 
colnames(BASE)[which(colnames(BASE)=="age")] = "edad"
colnames(BASE)[which(colnames(BASE)=="sex")] = "mujer"
colnames(BASE)[which(colnames(BASE)=="clase")] = "urbano"
colnames(BASE)[which(colnames(BASE)=="iof1es")] = "y_interesDividendo"
colnames(BASE)[which(colnames(BASE)=="iof2es")] = "y_jubilacionPension"
colnames(BASE)[which(colnames(BASE)=="p7510s3a1")] = "y_ayudaInstint"
colnames(BASE)[which(colnames(BASE)=="p7500s3a1")] = "y_pensionAlimentaria"
colnames(BASE)[which(colnames(BASE)=="p7510s7a1")] = "y_otrasFuentes"

## Construir variable de otros ingresos 
BASE <- BASE %>% 
  mutate(y_otrosTotal = y_interesDividendo + y_jubilacionPension + y_ayudaHogares
         + y_ayudaInstint + y_vivienda_m + y_horasExtras_m + y_bonificaciones_m
         + y_auxilioAliment_m + y_auxilioTransp_m + y_subFamiliar_m + y_subEducativo_m  
         + primas + y_pensionAlimentaria + y_otrasFuentes + y_viaticos_m + y_accidentes_m)

# Construir variable de ingreso
BASE <- BASE %>% 
  mutate(Ingreso = y_total_m + y_interesDividendo + y_jubilacionPension + y_ayudaHogares 
         + y_ayudaInstint + y_vivienda_m + y_horasExtras_m + y_bonificaciones_m
         + y_auxilioAliment_m + y_auxilioTransp_m + y_subFamiliar_m
         + y_subEducativo_m + primas + y_viaticos_m + y_pensionAlimentaria 
         + y_otrasFuentes + y_accidentes_m)

BASE <- BASE %>%
  mutate(Ingreso = ifelse(test = is.na(Ingreso)==T | Ingreso==0,
                          yes = ingtot,
                          no = Ingreso))

## Correccion de los tipos de variable a las variables creadas 
variables_categoricas_2 <- c("jefeHogar", "Micro_empresa", "formal")

BASE <- as.data.frame(BASE)
for (i in variables_categoricas_2){
  BASE[,i] = as.numeric(BASE[,i])
  BASE[,i] = as.logical(BASE[,i])
}

# Seleccionar las variables de interes 
BASE_PS <- BASE %>% 
  select(n, directorio, secuencia_p, orden, dominio, edad, mujer, educ, experiencia, experiencia2, 
         cuentaPropia, formal, oficio, jefeHogar, sizeFirm, Micro_empresa, urbano, depto, 
         maxEducLevel, dsi, estrato1, regSalud, y_total_m, y_interesDividendo, y_jubilacionPension, 
         y_ayudaHogares, y_ayudaInstint, y_vivienda_m, y_horasExtras_m, y_bonificaciones_m, y_auxilioAliment_m, 
         y_auxilioTransp_m, y_subFamiliar_m, y_subEducativo_m, y_primaServicios_m, y_primaNavidad_m, y_otrosTotal,
         y_primaVacaciones_m, primas, y_pensionAlimentaria, y_otrasFuentes, y_viaticos_m, y_accidentes_m,
         p6090, iof1, iof2, p7500s1a1, y_primas_m, p6050, p7510s1a1, p7510s2a1, ingtot, edad2, Ingreso)

# Codificar algunas variables para un proceso mas adelante
BASE_PS <- BASE_PS %>%
  mutate(mujer2 = ifelse(test = mujer == T,yes = 1,no  = 0),
         cuentaPropia2 = ifelse(test = cuentaPropia == T,
                                yes = 1,no  = 0),
         formal2 = ifelse(test = formal == T,yes = 1,no  = 0),
         jefeHogar2 = ifelse(test = jefeHogar == T,yes = 1,no  = 0),
         Micro_empresa2 = ifelse(test = Micro_empresa == T,yes = 1,no  = 0),
         educ_mujer2 = mujer2 * educ,
         edad_cuentaPropia2 = edad * cuentaPropia2,
         edad2_cuentaPropia2 = edad2 * cuentaPropia2,
         educ_cuentaPropia2 = educ * cuentaPropia2)

# Estadisticas descriptivas de las variables
TABLA <- BASE %>% 
  select(Ingreso, edad, mujer, educ, experiencia, cuentaPropia, formal, oficio,
         jefeHogar, Micro_empresa, estrato1, y_total_m, y_otrosTotal)
stargazer(as.data.frame(TABLA), type = "text", out = "stores/TABLA1.txt", digits = 0)

vars_tabla = c("Ingreso", "edad", "mujer", "educ", "experiencia", "cuentaPropia", "formal", "oficio",
               "jefeHogar", "Micro_empresa", "estrato1", "y_total_m", "y_otrosTotal")
CreateTableOne(data = BASE_PS,
               vars = vars_tabla,
               argsApprox = list(correct = TRUE))

## Grafica de ingreso vs edad categorizada por genero
gra1 <- ggplot(data = BASE_PS,mapping = aes(x = edad,y = Ingreso)) +
  geom_col(aes(colour = mujer),fill = "white") +
  facet_grid(~mujer) +
  labs(colour = "Genero",x = "Edad (anos)", y = "Ingreso (COP)", title = "Ingresos vs. edad por genero") +
  scale_color_manual(values = c("FALSE"="royalblue","TRUE"="violetred"),label = c("FALSE"="Hombre","TRUE"="Mujer")) +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

## Grafica de ingreso vs experiencia 
gra2 <- ggplot(data = BASE_PS, mapping = aes(x = experiencia , y = Ingreso)) +
  geom_col(col = "navyblue" , size = 0.5) + 
  labs(x = "Experiencia laboral (anos)", y = "Ingreso (COP)", title = "Ingresos vs. experiencia laboral") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

png("views/G1.png", width=800, height=291)
grid.arrange(gra1, gra2, ncol = 2)
dev.off()

## Grafica de ingreso por estrato
gra3 <- ggplot(data = BASE_PS,mapping = aes(as.factor(estrato1),Ingreso)) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,30000000)) +
  labs(x = "Estrato", y = "Ingreso (COP)", title = "Ingresos por estrato") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

## Grafica de ingreso por tipo de formalidad del trabajador
gra4<-ggplot(data = BASE_PS, mapping = aes(formal,Ingreso)) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,5000000)) +
  scale_x_discrete(breaks = c(0,1),labels = c("FALSE"="Informal","TRUE"="Formal")) +
  labs(x = "Formalidad", y = "Ingreso (COP)", title = "Ingresos por formalidad") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

## Grafica de ingreso por tipo de trabajador
gra5 <- ggplot(data = BASE_PS,mapping = aes(cuentaPropia, Ingreso)) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,4500000)) +
  scale_x_discrete(breaks = c(0,1),labels = c("FALSE"="Dependiente","TRUE"="Independiente")) +
  labs(x = "Tipo de trabajador", y = "Ingreso (COP)", title = "Ingresos por tipo trabajador") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

png("views/G2.png", width=620, height=291)
grid.arrange(gra4, gra5, ncol = 2)
dev.off()

## Grafica de ingreso por edad y estrato
gra6 <- ggplot(data = BASE_PS, 
               mapping = aes(x = edad , y = Ingreso , group=as.factor(estrato1) , color=as.factor(estrato1))) +
  geom_point() + 
  labs(colour = "Estrato", x = "Edad (anos)", y = "Ingreso (COP)", title = "Ingresos vs edad por estrato") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

png("views/G3.png", width=800, height=291)
grid.arrange(gra3, gra6, ncol = 2)
dev.off()

# 3. Perfil edad-ingresos ----
# Estimar modelo Minimos Cuadrados Ordinario (MCO) de perfil edad-ingresos 
modelo <- lm(Ingreso ~ edad + edad2, data = BASE_PS)
texreg::htmlreg(modelo, file='modelo.doc')

# Crear datos con los valores a graficar
m1_datos_g1_e <- data.frame(m1_valores_predichos = predict(modelo),  
                            edad = BASE_PS$edad)

# Graficar edad e ingresos 
png("views/G4.png", width=350, height=291)
g1_e <- ggplot(m1_datos_g1_e, aes(x = edad, y = m1_valores_predichos)) +
  geom_point(color="navyblue") +
  geom_vline(xintercept = 47, linetype = 2, color = "4") + 
  labs(x = "Edad (a?os)", y = "Ingreso estimado (COP)") +
  theme_test() 
dev.off()

# Bootstrap 
edad_fn<-function(data,index){
  f<-lm(Ingreso ~ edad + edad2,data, subset = index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]
  edad <- -b2/(2*b3)
  return(edad)
}

m1_resultado <- boot(data=BASE_PS, edad_fn, R=500)
m1_resultado

# Construir intervalo de confianza 
Lim_inf1 <- 47.29889-(1.96*0.9538228)
Lim_sup1 <- 47.29889+(1.96*0.9538228)

Lim_inf1  
Lim_sup1

# Crear datos con los valores a graficar
m_datos_g <- data.frame(m_valores_predichos = predict(modelo),  
                        m_valores_observados = BASE_PS$Ingreso)

# Graficar valores predichos del perfil edad-ingreso 
g <- ggplot(m_datos_g, aes(x = m_valores_predichos, y = m_valores_observados)) +
  geom_point(color="gray") +
  geom_abline(intercept = 0, slope = 1, color = "navyblue") + 
  scale_y_continuous(limits = c(0,50000000)) + 
  #scale_x_continuous(limits = c(12,14.3)) + 
  labs(x = "Ingreso estimado (COP)", y = "Ingreso observado (COP)") +
  theme_test()

# Crear logaritmo de ingreso 
BASE_PS <- BASE_PS %>% 
  mutate(L_Ingreso = ifelse(Ingreso==0, 
                            yes = 0,
                            no = log(Ingreso)))

# Estimar modelo Minimos Cuadrados Ordinario (MCO) de perfil edad-ingresos 
modelo_1 <- lm(L_Ingreso ~ edad + edad2, data = BASE_PS)

# Calcular ajuste del modelo
BASE_PS$ajuste_1<-predict(modelo_1)
MSE_1 <- with(BASE_PS,mean((L_Ingreso-ajuste_1)^2))

# Crear datos con los valores a graficar
m1_datos_g1 <- data.frame(m1_valores(y_predichos = predict(modelo_1),  
                                     m1_valores_observados = BASE_PS$L_Ingreso))

# Graficar valores predichos del perfil edad-ingreso 
g1 <- ggplot(m1_datos_g1, aes(x = m1_valores_predichos, y = m1_valores_observados)) +
  geom_point(color="gray") +
  geom_abline(intercept = 0, slope = 1, color = "navyblue") + 
  scale_y_continuous(limits = c(5,20)) + scale_x_continuous(limits = c(12,14.3)) + 
  labs(x = "Ingreso estimado (log)", y = "Ingreso observado (log)") +
  theme_test()

png("views/Anexo1.png", width=700, height=291)
grid.arrange(g,g1, ncol=2)
dev.off()
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


