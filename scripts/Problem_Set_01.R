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
stargazer(as.data.frame(TABLA), type = "text", out = "views/TABLA1.txt", digits = 0)

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
texreg::htmlreg(modelo, file = 'views/modelo.doc')

# Crear datos con los valores a graficar
m1_datos_g1_e <- data.frame(m1_valores_predichos = predict(modelo),  
                            edad = BASE_PS$edad)

# Graficar edad e ingresos 
png("views/G4.png", width=350, height=291)
g1_e <- ggplot(m1_datos_g1_e, aes(x = edad, y = m1_valores_predichos)) +
  geom_point(color = "navyblue") +
  geom_vline(xintercept = 47, linetype = 2, color = "4") + 
  labs(x = "Edad (anos)", y = "Ingreso estimado (COP)") +
  theme_test() 
dev.off()

# Bootstrap 
edad_fn<-function(data,index){
  f<-lm(Ingreso ~ edad + edad2,data, subset = index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]
  edad <- -b2 / (2 * b3)
  return(edad)
}

m1_resultado <- boot(data = BASE_PS, edad_fn, R = 500)
m1_resultado

# Construir intervalo de confianza 
Lim_inf1 <- 47.29889 - (1.96 * 0.9538228)
Lim_sup1 <- 47.29889 + (1.96 * 0.9538228)

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
m1_datos_g1 <- data.frame(m1_valores_predichos = predict(modelo_1),  
                                     m1_valores_observados = BASE_PS$L_Ingreso)

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

modelo_2 <- lm(L_Ingreso ~ mujer, data = BASE_PS)
texreg::htmlreg(modelo_2, file = 'views/modelo_2.doc')

# Calcular ajuste del modelo
BASE_PS$ajuste_2<-predict(modelo_2)
MSE_2 <- with(BASE_PS, mean((L_Ingreso - ajuste_2)^2))

#    * Estimar modelo MCO de perfil edad-ingreso por genero ----
# Estimar modelos
modelo_2_m <- lm(Ingreso ~ edad + edad2, data = subset(BASE_PS, subset = mujer==1))
modelo_2_h <- lm(Ingreso ~ edad + edad2, data = subset(BASE_PS, subset = mujer==0))
texreg::htmlreg(list(modelo_2_m, modelo_2_h), file = 'views/modelo_2_m_h.doc')

#Grafico edad-ingreso por genero

# Crear datos con los valores a graficar
m2_m_datos_g2 <- data.frame(m2_m_valores_predichos = predict(modelo_2_m),  
                            edad = subset(BASE_PS, subset = mujer==1)$edad)
m2_h_datos_g2 <- data.frame(m2_h_valores_predichos = predict(modelo_2_h),  
                            edad = subset(BASE_PS, subset = mujer==0)$edad)

# Graficar valores predichos del perfil edad-ingreso 
g2 <- ggplot(m2_m_datos_g2, aes(x = edad, y = m2_m_valores_predichos)) +
  geom_point(color="navyblue") +
  geom_vline(xintercept = 43, linetype = 2, color = "4") + 
  labs(x = "Edad (anos)", y = "Ingreso estimado (COP)", title = "Mujeres") +
  theme_test() + theme(plot.title=element_text(hjust=0.5)) 

g3 <- ggplot(m2_h_datos_g2, aes(x = edad, y = m2_h_valores_predichos)) +
  geom_point(color="navyblue") +
  geom_vline(xintercept = 50, linetype = 2, color = "4") +  
  labs(x = "Edad (anos)", y = "Ingreso estimado (COP)", title = "Hombres") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

png("views/G6.png", width = 700, height = 291)
grid.arrange(g2,g3, ncol = 2)
dev.off()

# Grafico ingreso estimado de los modelos

# Crear datos con los valores a graficar
m2_m_g2 <- data.frame(m2_valores_predichos = predict(modelo_2_m),  
                      m2_valores_observados = subset(BASE_PS, subset = mujer==1)$Ingreso)
m2_h_g2 <- data.frame(m2h_valores_predichos = predict(modelo_2_h),  
                      m2h_valores_observados = subset(BASE_PS, subset = mujer==0)$Ingreso)

# Graficar valores predichos del perfil edad-ingreso 
g2m <- ggplot(m2_h_g2, aes(x = m2h_valores_predichos, y = m2h_valores_observados)) +
  geom_point(color="gray") +
  geom_abline(intercept = 0, slope = 1, color = "navyblue") + 
  scale_y_continuous(limits = c(-1830000,50000000)) + 
  labs(x = "Ingreso estimado (COP)", y = "Ingreso observado (COP)", title = "Mujeres") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

g2h <- ggplot(m2_m_g2, aes(x = m2_valores_predichos, y = m2_valores_observados)) +
  geom_point(color="gray") +
  geom_abline(intercept = 0, slope = 1, color = "navyblue") + 
  scale_y_continuous(limits = c(-1830000,50000000)) + 
  labs(x = "Ingreso estimado (COP)", y = "Ingreso observado (COP)", title = "Hombres") +
  theme_test() + theme(plot.title=element_text(hjust=0.5))

png("views/G5.png", width = 700, height = 291)
grid.arrange(g2m, g2h, ncol = 2)
dev.off()

# Bootstrap

# Mujeres
m2_m_results <- boot(data = subset(BASE_PS, subset = mujer==1), edad_fn, R = 500)
m2_m_results

# Construir intervalo de confianza 
Lim_inf2m <- 42.92666 - (1.96 * 0.8671444)
Lim_sup2m <- 42.92666 + (1.96 * 0.8671444)
Lim_inf2m  
Lim_sup2m

#Hombres
m2_h_results <- boot(data = subset(BASE_PS, subset = mujer==0), edad_fn, R = 500)
m2_h_results

# Construir intervalo de confianza 
Lim_inf2h <- 50.3818 - (1.96 * 1.911106)
Lim_sup2h <- 50.3818 + (1.96 * 1.911106)
Lim_inf2h  
Lim_sup2h

#    * Salario igual para trabajos iguales ----
#      a. Estimar modelos MCO de brecha de ingresos condicional----
modelo_3_1 <- lm(L_Ingreso ~ mujer + cuentaPropia + formal + Micro_empresa, data = BASE_PS)
modelo_3_2 <- lm(L_Ingreso ~ mujer + cuentaPropia + formal + Micro_empresa + oficio, data = BASE_PS)

BASE_PS <- BASE_PS %>% 
  mutate(L_salario = ifelse(y_total_m==0, 
                            yes = 0,
                            no = log(y_total_m)))

modelo_3_1s <- lm(L_salario ~ mujer + cuentaPropia + formal + Micro_empresa, data = BASE_PS)
modelo_3_2s <- lm(L_salario ~ mujer + cuentaPropia + formal + Micro_empresa + oficio, data = BASE_PS)

texreg::htmlreg(list(modelo_3_1, modelo_3_2,modelo_3_1s, modelo_3_2s), file = 'views/modelo_3_n.doc')

# Calcular ajuste de los modelos
BASE_PS$ajuste_3_1 <- predict(modelo_3_1)
MSE_3_1 <- with(BASE_PS,mean((L_Ingreso - ajuste_3_1)^2))

BASE_PS$ajuste_3_2 <- predict(modelo_3_2)
MSE_3_2 <- with(BASE_PS,mean((L_Ingreso - ajuste_3_2)^2))

#      b. Usar FWL para hacer la estimacion ----
# Explorar datos atipicos
quantile(x=BASE_PS$Ingreso ,na.rm = T)
iqr = IQR(x=BASE_PS$Ingreso, na.rm = T)

# Hacer estimacion usando el teorema de FWL
BASE_PS = BASE_PS %>% 
  mutate(Ingreso_out = ifelse(test = Ingreso > 4*iqr, 
                              yes = 1, 
                              no = 0))

# Modelo 3_1
modelo_3_1_Out <- lm(L_Ingreso ~ mujer + Ingreso_out + cuentaPropia + formal + Micro_empresa, data = BASE_PS)

BASE_PS <- BASE_PS %>% 
  mutate(residuo_L_Ingreso = lm(L_Ingreso ~ Ingreso_out + cuentaPropia + formal + Micro_empresa,data = BASE_PS)$residuals,
         residuo_mujer = lm(mujer ~ Ingreso_out + cuentaPropia + formal + Micro_empresa,data = BASE_PS)$residuals,
  )

modelo_3_1_FWL<-lm(residuo_L_Ingreso~residuo_mujer, data = BASE_PS)

# Calcular ajuste del modelo
BASE_PS$ajuste_3_1_FWL <- predict(modelo_3_1_FWL)
MSE_3_1_FWL<- with(BASE_PS, mean((residuo_L_Ingreso - ajuste_3_1_FWL)^2))

# Modelo 3_2
modelo_3_2_Out <- lm(L_Ingreso ~ mujer + cuentaPropia + formal + oficio + Micro_empresa + Ingreso_out, data = BASE_PS)

BASE_PS <- BASE_PS %>% 
  mutate(residuo_L_Ingreso2 = lm(L_Ingreso ~ Ingreso_out + cuentaPropia + formal + oficio + Micro_empresa,data = BASE_PS)$residuals,
         residuo_mujer2 = lm(mujer ~ Ingreso_out + cuentaPropia + formal + oficio + Micro_empresa,data = BASE_PS)$residuals,
  )

modelo_3_2_FWL <- lm(residuo_L_Ingreso2~residuo_mujer2, data = BASE_PS)

# Calcular ajuste del modelo
BASE_PS$ajuste_3_2_FWL <- predict(modelo_3_2_FWL)
MSE_3_2_FWL <- with(BASE_PS,mean((residuo_L_Ingreso - ajuste_3_2_FWL)^2))

texreg::htmlreg(list(modelo_3_1_Out, modelo_3_1_FWL, modelo_3_2_Out, modelo_3_2_FWL), file = 'views/modelo_3_FWL.doc')

# Estimacion para salario 
quantile(x=BASE_PS$y_total_m, na.rm = T)
iqr_s = IQR(x=BASE_PS$y_total_m, na.rm=T)

# Hacer estimacion usando el teorema de FWL
BASE_PS = BASE_PS %>% 
  mutate(y_total_m_out = ifelse(test = y_total_m > 4*iqr_s, 
                                yes = 1, 
                                no = 0))

# Modelo 3_1
modelo_3_1_Out_S <- lm(L_salario ~ mujer + y_total_m_out + cuentaPropia + formal + Micro_empresa, data = BASE_PS)

BASE_PS <- BASE_PS %>% 
  mutate(residuo_L_Salario = lm(L_salario ~ y_total_m_out + cuentaPropia + formal + Micro_empresa,data = BASE_PS)$residuals,
         residuo_mujer_S = lm(mujer ~ y_total_m_out + cuentaPropia + formal + Micro_empresa,data = BASE_PS)$residuals,
  )

modelo_3_1_FWL_S <- lm(residuo_L_Salario~residuo_mujer_S,data = BASE_PS)

# Modelo 3_2
modelo_3_2_Out_S <- lm(L_salario ~ mujer + cuentaPropia + formal + oficio + Micro_empresa + y_total_m_out, data = BASE_PS)

BASE_PS <- BASE_PS %>% 
  mutate(residuo_L_Salario2 = lm(L_salario ~ y_total_m_out + cuentaPropia + formal + oficio + Micro_empresa,data = BASE_PS)$residuals,
         residuo_mujer_S2 = lm(mujer ~ y_total_m_out + cuentaPropia + formal + oficio + Micro_empresa,data = BASE_PS)$residuals,
  )

modelo_3_2_FWL_S <- lm(residuo_L_Salario2~residuo_mujer_S2, data = BASE_PS)

texreg::htmlreg(list(modelo_3_1_Out_S, modelo_3_1_FWL_S, modelo_3_2_Out_S, modelo_3_2_FWL_S), file = 'views/modelo_3_FWL_s.doc')

# 5. Prediccion de ingresos ----
#    a. Dividir la muestra en dos: entrenamiento(70%) y prueba(30%) y estimar modelos ----
# Establecer semilla para reproducibilidad
set.seed(10101)

# Generar un indicador logico para dividir los conjuntos de datos
BASE_PS <- BASE_PS %>%
  mutate(holdout= as.logical(1:nrow(BASE_PS) %in%
                               sample(nrow(BASE_PS), nrow(BASE_PS)*.3))
  )

prueba <- BASE_PS[BASE_PS$holdout==T,]
entrenamiento <- BASE_PS[BASE_PS$holdout==F,]

#       i.  Estimar modelo sin covariables solo constante ----
modelo_referencia <- lm(L_Ingreso ~ 1, data = entrenamiento)
coef(modelo_referencia)
mean(entrenamiento$L_Ingreso)

## Estimar modelo con la muestra 
prueba$modelo_referencia <- predict(modelo_referencia,newdata = prueba)
MSE_mref <- with(prueba, mean((L_Ingreso - modelo_referencia)^2))

#       ii. Estimar los modelos previos ----
## Estimar Modelo de perfil edad-ingresos 
modelo_1_4 <- lm(L_Ingreso ~ edad + edad2, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_1_4<-predict(modelo_1_4, newdata = prueba)
MSE_m1_4 <- with(prueba,mean((L_Ingreso - modelo_1_4)^2))

## Estimar modelo MCO de brecha de ingresos incondicional 
modelo_2_4 <- lm(L_Ingreso ~ mujer, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_2_4 <- predict(modelo_2_4, newdata = prueba)
MSE_m2_4 <- with(prueba, mean((L_Ingreso - modelo_2_4)^2))

## Estimar modelo MCO de brecha de ingresos condicional 1
modelo_3_1_4 <- lm(L_Ingreso ~ mujer + cuentaPropia + formal + Micro_empresa, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_3_1_4 <- predict(modelo_3_1_4, newdata = prueba)
MSE_m3_1_4 <- with(prueba,mean((L_Ingreso - modelo_3_1_4)^2))


## Estimar modelo MCO de brecha de ingresos condicional 2 
modelo_3_2_4 <- lm(L_Ingreso ~ mujer + cuentaPropia + formal + oficio + Micro_empresa, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_3_2_4 <- predict(modelo_3_2_4, newdata = prueba)
MSE_m3_2_4 <- with(prueba,mean((L_Ingreso - modelo_3_2_4)^2))

#       iii.Otros modelos ----

## Estimar modelo 4
modelo_4 <- lm(L_Ingreso ~ edad + edad2 + educ, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_4 <- predict(modelo_4, newdata = prueba)
MSE_m4 <- with(prueba,mean((L_Ingreso - modelo_4)^2))

## Estimar modelo 5
modelo_5 <- lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_5 <- predict(modelo_5, newdata = prueba)
MSE_m5 <- with(prueba,mean((L_Ingreso - modelo_5)^2))

## Estimar modelo 6
modelo_6 <- lm(L_Ingreso ~ edad + edad2 + educ + cuentaPropia, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_6 <- predict(modelo_6, newdata = prueba)
MSE_m6 <- with(prueba,mean((L_Ingreso - modelo_6)^2))

## Estimar modelo 7
modelo_7 <- lm(L_Ingreso ~ edad + edad2 + educ + cuentaPropia + cuentaPropia:edad + 
                 cuentaPropia:edad2 + cuentaPropia:educ, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_7 <- predict(modelo_7, newdata = prueba)
MSE_m7 <- with(prueba,mean((L_Ingreso - modelo_7)^2))

## Estimar modelo 8
modelo_8 <- lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                 cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                 oficio + Micro_empresa, data = entrenamiento)

## Estimar modelo con la muestra 
prueba$modelo_8 <- predict(modelo_8, newdata = prueba)
MSE_m8 <- with(prueba,mean((L_Ingreso - modelo_8)^2))

texreg::htmlreg(modelo_8, type="text", file = 'views/modelo_8.doc')

#       iv. Comparar error de prediccion promedio de todos los modelos ----------------------
MSE_modelos <- data.frame(matrix(NA, 10, 2))
colnames(MSE_modelos) <- c("Modelo", "MSE")

MSE_modelos[1,1] = "M.0"
MSE_modelos[2,1] = "M.1"
MSE_modelos[3,1] = "M.2"
MSE_modelos[4,1] = "M.3.1"
MSE_modelos[5,1] = "M.3.2"
MSE_modelos[6,1] = "M.4"
MSE_modelos[7,1] = "M.5"
MSE_modelos[8,1] = "M.6"
MSE_modelos[9,1] = "M.7"
MSE_modelos[10,1] = "M.8"

MSE_modelos[1,2] = MSE_mref
MSE_modelos[2,2] = MSE_m1_4
MSE_modelos[3,2] = MSE_m2_4
MSE_modelos[4,2] = MSE_m3_1_4
MSE_modelos[5,2] = MSE_m3_2_4
MSE_modelos[6,2] = MSE_m4
MSE_modelos[7,2] = MSE_m5
MSE_modelos[8,2] = MSE_m6
MSE_modelos[9,2] = MSE_m7
MSE_modelos[10,2] = MSE_m8

write.xlsx(MSE_modelos, "views/MSE_modelos.xlsx")

png("views/G7.png", width = 450, height = 291)
MSE_m_grafico2 <- ggplot(MSE_modelos, aes(x = Modelo, y = MSE, group = 1)) +
  geom_line(color="navyblue") +
  geom_point() +
  scale_y_continuous(limits = c(3,4))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Modelo", y = "MSE") +
  theme_test()
dev.off()

#       v.  Leverage ----
# Explorar datos atipicos
quantile(x=prueba$Ingreso, na.rm = T)
iqr_p = IQR(x=prueba$Ingreso, na.rm=T)

prueba = prueba %>% 
  mutate(Ingreso_out_p = ifelse(test = Ingreso > 4*iqr_p, 
                                yes = 1, 
                                no = 0))

modelo_8_Out <- lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                     cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                     oficio + Micro_empresa + Ingreso_out_p, data = prueba)

#Crea una lista para guardar los alphas del modelo_8_out y corre un loop para guadar ese valor para cada observación
alphas <- c() 

for (j in 1:nrow(prueba)) { 
  uj <- modelo_8_Out$residual[j] 
  hj <- lm.influence(modelo_8_Out)$hat[j] 
  alpha <- uj / (1 - hj) 
  alphas <- c(alphas, alpha) 
} 

# Agregar las variables estimado, Ing_estimado, Leverage, Residuales y Hj pertenecientes al modelo_8_out
prueba <- prueba %>%
          mutate(estimado = predict(modelo_8_Out),
                 Ing_Estimado = exp(estimado),
                 Leverage = alphas,
                 Residuales_8 = modelo_8_Out$residuals,
                 Hj_8 = Hj_mod8)

# Ver un resumen de la lista de los alphas del modelo_8_out para determinar cual es el valor del percentil 3 (el mas alto) 
summary(alphas)

# Crea una base donde solo se tenga encuenta los ingresos y alphas más altos 
prueba_ing <- subset(prueba, subset = alphas > 1.77 & Ingreso_out == 1)

# Crea una base donde este el ingreso original reportadado y los demás valores estimados del modelo_8_out para revisar los outliers
tabla_lev <- prueba_ing %>% 
             select(Ingreso, Ing_Estimado, Leverage, Residuales_8, Hj_8)
view(tabla_lev)

# Crea una gráfica para ver los outliers de los ingreso más altos vs el Leverage
leverage <- data.frame(alphas, prueba$Ingreso_out) 

png("views/G8.png", width = 466, length = 291)
leverage <- data.frame(alphas, prueba$Ingreso_out) 
gra_lev <- ggplot(leverage,aes(alphas,prueba$Ingreso_out))+
  geom_point(color="navyblue", size=1)+
  xlab("Alpha") + ylab("Outliers (ingreso)")+
  theme_test() 
dev.off()

prueba <- prueba %>% mutate(res_y_x=lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                                         cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                                         oficio + Micro_empresa, data = prueba)$residuals,
                            res_e_x=lm(Ingreso_out_p ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                                         cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                                         oficio + Micro_empresa, data = prueba)$residuals)
modelo_5_L <- lm(res_y_x ~ res_e_x, data = prueba)
stargazer(modelo_8_Out, modelo_5_L,type="text")

#    b. Hacer validacion cruzada en K-iteraciones (K-fold Cross-Validation) ----
#       * Estimar modelo sin covariables solo constante ----
modelo_5_KV_m <- train(Ingreso ~ .,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "null")

# Ajuste del modelo
RMSE_5_KV <- modelo_5_1_KV[["results"]][["RMSE"]]

#       * Estimar Modelo de perfil edad-ingresos ----
modelo_5_1_KV <- train(L_Ingreso ~ edad + edad2,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")

# Ajuste del modelo
RMSE_5_1_KV <- modelo_5_1_KV[["results"]][["RMSE"]]

#       * Estimar modelo MCO de brecha de ingresos incondicional ----
modelo_5_2_KV <- train(L_Ingreso ~ mujer2,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")

# Ajuste del modelo
RMSE_5_2_KV <- modelo_5_2_KV[["results"]][["RMSE"]]

#       * Estimar modelo MCO de brecha de ingresos condicional ----
# Modelo 3.1
modelo_5_3_1_KV <- train(L_Ingreso ~ mujer2 + cuentaPropia2 + formal2 + Micro_empresa2,
                         data = BASE_PS,
                         trControl = trainControl(method = "cv", number = 5), 
                         method = "lm")

# Ajuste del modelo
RMSE_5_3_1_KV <- modelo_5_3_1_KV[["results"]][["RMSE"]]

# Modelo 3.2
modelo_5_3_2_KV <- train(L_Ingreso ~ mujer2 + cuentaPropia2 + formal2 + oficio + Micro_empresa2,
                         data = BASE_PS,
                         trControl = trainControl(method = "cv", number = 5), 
                         method = "lm")

# Ajuste del modelo
RMSE_5_3_2_KV <- modelo_5_3_2_KV[["results"]][["RMSE"]]

#       * Estimar otros modelos ----
#        ** Estimar modelo 4 ----
modelo_5_4_KV <- train(L_Ingreso ~ edad + edad2 + educ,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")
# Calcular ajuste del modelo
RMSE_5_4_KV <- modelo_5_4_KV[["results"]][["RMSE"]]

#        ** Estimar modelo 5 ----
modelo_5_5_KV <- train(L_Ingreso ~ edad + edad2 + educ + mujer2 + educ_mujer2,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")

# Calcular ajuste del modelo
RMSE_5_5_KV <- modelo_5_5_KV[["results"]][["RMSE"]]
#        ** Estimar modelo 6 ----
modelo_5_6_KV <- train(L_Ingreso ~ edad + edad2 + educ + cuentaPropia2,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")

# Calcular ajuste del modelo
RMSE_5_6_KV <- modelo_5_6_KV[["results"]][["RMSE"]]
#        ** Estimar modelo 7 ----
modelo_5_7_KV <- train(L_Ingreso ~ edad + edad2 + educ + cuentaPropia2 + edad_cuentaPropia2 + 
                         edad2_cuentaPropia2 + educ_cuentaPropia2,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")

# Calcular ajuste del modelo
RMSE_5_7_KV <- modelo_5_6_KV[["results"]][["RMSE"]]
#        ** Estimar modelo 8 ----

modelo_5_8_KV <- train(L_Ingreso ~ edad + edad2 + educ + mujer2 + educ_mujer2 + cuentaPropia2 + 
                         edad_cuentaPropia2 + edad2_cuentaPropia2 + cuentaPropia2 + formal2 + 
                         oficio + Micro_empresa2,
                       data = BASE_PS,
                       trControl = trainControl(method = "cv", number = 5), 
                       method = "lm")

# Calcular ajuste del modelo
RMSE_5_8_KV <- modelo_5_8_KV[["results"]][["RMSE"]]

#       * Comparar error de prediccion promedio de todos los modelos ----

MSE_modelos_KV <- data.frame(matrix(NA, 9, 2))
colnames(MSE_modelos_KV) <- c("Modelo", "Root_MSE")

MSE_modelos_KV[1,1] = "M.1(KV)"
MSE_modelos_KV[2,1] = "M.2(KV)"
MSE_modelos_KV[3,1] = "M.3.1(KV)"
MSE_modelos_KV[4,1] = "M.3.2(KV)"
MSE_modelos_KV[5,1] = "M.4(KV)"
MSE_modelos_KV[6,1] = "M.5(KV)"
MSE_modelos_KV[7,1] = "M.6(KV)"
MSE_modelos_KV[8,1] = "M.7(KV)"
MSE_modelos_KV[9,1] = "M.8(KV)"

MSE_modelos_KV[1,2] = RMSE_5_1_KV
MSE_modelos_KV[2,2] = RMSE_5_2_KV
MSE_modelos_KV[3,2] = RMSE_5_3_1_KV
MSE_modelos_KV[4,2] = RMSE_5_3_2_KV
MSE_modelos_KV[5,2] = RMSE_5_4_KV
MSE_modelos_KV[6,2] = RMSE_5_5_KV
MSE_modelos_KV[7,2] = RMSE_5_6_KV
MSE_modelos_KV[8,2] = RMSE_5_7_KV
MSE_modelos_KV[9,2]=RMSE_5_8_KV

write.xlsx(MSE_modelos_KV, "views/MSE_modelos_KV.xlsx")

p <- ggplot(MSE_modelos_KV, aes(x = Modelo, y = Root_MSE, group = 1)) +
  geom_line(color = "navyblue") +
  geom_point() +
  scale_y_continuous(limits = c(1.8,2.05))+
  labs(x = "Modelo", y = "RMSE") +
  theme_test()
p2 <- p + theme(axis.text = element_text(angle = 90)) 
p3 <- MSE_m_grafico2 + theme(axis.text = element_text(angle = 90))

png("views/G9.png", width = 430, height = 300)
grid.arrange(p3, p2, ncol = 2)
dev.off()

#    c. Hacer validacion cruzada dejando uno afuera (Leave-One-Out Cross-validation [LOOCV]) ----
#       i.  Escribir lopp ----
MSEs     <- list()
for (i in 1:nrow(BASE_PS)) {
  MSEs[[i]]      <- (BASE_PS[i,]$L_Ingreso - predict(lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia +
                                                          cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal +
                                                          oficio + Micro_empresa, data = BASE_PS[-i,]), newdata = BASE_PS[i,]))^2
  LOOCV_estad    <- Reduce("+", MSEs)/length(MSEs)
}

LOOCV_estad

#       ii. Comparar resultados ----
iqr_c = IQR(x=BASE_PS$Ingreso, na.rm=T)

BASE_PS = BASE_PS %>% 
  mutate(Ingreso_out_c = ifelse(test = Ingreso > 4*iqr_c, 
                                yes = 1, 
                                no = 0))

modelo_8_Outc <- lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                      cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                      oficio + Micro_empresa + Ingreso_out_c, data = BASE_PS)

BASE_PS <- BASE_PS %>% mutate(res_y_xc=lm(L_Ingreso ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                                            cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                                            oficio + Micro_empresa, data = BASE_PS)$residuals,
                              res_e_xc=lm(Ingreso_out_c ~ edad + edad2 + educ + mujer + educ:mujer + cuentaPropia + 
                                            cuentaPropia:edad + cuentaPropia:edad2 + cuentaPropia:educ + formal + 
                                            oficio + Micro_empresa, data = BASE_PS)$residuals)

modelo_5_Lc <- lm(res_y_xc ~ res_e_xc, data = BASE_PS)
stargazer(modelo_8_Outc, modelo_5_Lc, type = "text")

Uhat2 <- modelo_8_Outc$residuals 
Hj2 <- lm.influence(modelo_8_Outc)$hat 
alpha2 <- Uhat2 /(1 - Hj2) 

modelo_8_Outc$coefficients[10] > LOOCV_estad

mean(Hj2) < (2 * 12) / 16397



