#Directorio de trabajo
setwd("/Users/Camila/Documents/Curso periodismo datos/2017/Clases/Analisis_R")

#librerías
install.packages(c("GGally","corrplot", "PerformanceAnalytics", "gmodels", "Hmisc", "ggthemes"))

library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(GGally)
library(corrplot)
library(PerformanceAnalytics)

#Eliminar notación cientifica
options(scipen = 999)

#Importar archivo y convertir a tibble
encuesta <- read_excel("encuesta.xlsx")
encuesta <- tbl_df(encuesta)

#Explorar las variables
glimpse(encuesta)

#Convertir a factor
encuesta$REGION<-as.factor(encuesta$REGION)
#Convertimos todas las variables a factor
cols<- c("REGION","ZONA", "Tipo_vivienda", "M2", "Sexo", "Estado_civil", "NivInst", "CondAct", "np", "quintil")
encuesta[cols] <- lapply(encuesta[cols], factor)

##VARIABLES CATEGORICAS

  #Tablas de frecuencia
table(encuesta$ZONA)
table(encuesta$Sexo)
table(encuesta$quintil)

  #Tablas de contingencia
table(encuesta$ZONA, encuesta$Sexo)
table(encuesta$Tipo_vivienda, encuesta$ZONA)
  
  #Tablas con proporciones
tabla1<-table(encuesta$Tipo_vivienda, encuesta$ZONA)
prop.table(tabla1)
prop.table(table(encuesta$Tipo_vivienda, encuesta$ZONA))
  #Porcentaje por filas
prop.table(tabla1, 1)
prop.table(table(encuesta$Tipo_vivienda, encuesta$ZONA),1)
  #Porcentaje por columnas
prop.table(tabla1, 2)
prop.table(table(encuesta$Tipo_vivienda, encuesta$ZONA),2)

  #Crosstable
CrossTable(encuesta$Tipo_vivienda, encuesta$ZONA)
CrossTable(encuesta$Tipo_vivienda, encuesta$ZONA, prop.t=F, prop.chisq = F)
CrossTable(encuesta$ZONA)

##VARIABLES NUMÉRICAS
summary(encuesta)
describe(encuesta)

#histograma
hist(encuesta$ingreso)

##histograma con ggplot
histograma <- ggplot(encuesta, aes(x=ingreso)) +
  ggtitle("Ingreso neto de hogares unipersonales") +
  theme_fivethirtyeight() +
  geom_histogram(color="#28324a", fill="#3c78d8")
histograma

  #Dplyr
#Group_by
encuesta <- encuesta %>%
  group_by(REGION) %>%
  mutate(ingresoprom=mean(ingreso))

#Summarise
encuesta %>%
  group_by(REGION) %>%
  summarise(ingresoprom=mean(ingreso),
            edadprom=mean(Edad), 
            cantidad=n())


#CORRELACIONES
#importar base de datos
base <- read_excel("correlaciones.xlsx")

#Coeficiente de correlación
cor(base$porcentajepac, base$ips)

#pvalue
cor.test(base$porcentajepac, base$ips)

#Matriz de correlacion
cor(base)
round(cor(base),2)  

#matriz con pvalue
rcorr(as.matrix(base))

#Matriz gráfica
correlacion<-round(cor(base), 1)

corrplot(correlacion, method="number", type="upper")

#Gráficos de dispersión
chart.Correlation(base, histogram = F, pch = 19)
