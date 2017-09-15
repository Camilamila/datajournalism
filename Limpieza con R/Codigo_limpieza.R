#Clase 6 - Limpieza de datos con R

##En esta clase vamos a aprender a usar algunos paquetes para limpiar datos y estructurar información en R.  


#Establecer directorio de trabajo (cambien esto a su directorio)
#Recordar que en ese folder deben estar todos los archivos de trabajo
setwd("/Users/Camila/Documents/Curso periodismo datos/2017/Clases/Limpieza_R")

#Instalar paquetes y cargarlos a la sesion
install.packages("dplyr") 
install.packages("tidyr") 
install.packages("readxl")

library(dplyr)
library(tidyr) 
library(readxl)

###PRIMERA PARTE
#Cargar los archivos
morosidad16 <- read_xlsx("morosidad16.xlsx")
morosidad17<- read_xlsx("morosidad17.xlsx")

#Unir bases de datos
morosidad<-merge(morosidad17, morosidad16, by= "id", all=TRUE)

#Convertimos la base a una tibble para facilitar la lectura
morosidad <-tbl_df(morosidad)
#Explorar la base
###Explorar los datos
morosidad
dim(morosidad)
head(morosidad)
tail(morosidad)
str(morosidad)


## DPLYR

# SELECT
#seleccionar únicamente las columnas de id y deuda
select(morosidad, id, deuda17, deuda16)

#Si queremos todas las columnas menos Estado:
select(morosidad, -Estado)

#Si queremos seleccionar un rango de columnas por ejemplo de `id` a `situacion` usamos `:`.
select(morosidad, id:situacion.x)

#Si queremos guardar el resultado de esa función en un nuevo objeto, por ejemplo nos interesa 
#quedarnos únicamente con columnas que no estén repetidas
morosidad1 <- select(morosidad, -nombre.y, -situacion.y, -lugar.pago.y)

# FILTER

#si queremos filtrar solamente las deudas superiores a un millón:
filter(morosidad1, deuda17>1000000)

#las deudas que crecieron entre 2016 y 2017:
filter(morosidad1, deuda17>deuda16)

#las deudas mayores a un millón y de díficil cobro:
filter(morosidad1, deuda17>1000000 & situacion.x=="DIFICIL COBRO")

#Solo los casos de las empresas o personas que han estado morosas por los dos años
morosidad1 <- filter(morosidad1, !is.na(deuda17), !is.na(deuda16))

# MUTATE
#Podemos crear una variable que me diga cuánto cambió la deuda, que es la diferencia entre `deuda17` y `deuda16`:
morosidad1 <- mutate(morosidad1, cambio.deuda=deuda17-deuda16)

#variable que me categorice el cambio en la deuda en si aumentó o no.  
#Esto podemos hacerlo con la función `if_else()` o `ifelse` (funcionar igual).  
#La sintaxis es: `ifelse(condición, valor cierto, valor falso)`.  (Es similar a la función if en Excel).
morosidad1 <- mutate(morosidad1, tipo.cambio=ifelse(cambio.deuda<0,"disminuyó", "aumentó"))

#Con mutate podemos crear multiples variables a la vez, separando cada una por coma, por ejemplo:
morosidad1 <- mutate(morosidad1, cambio.deuda=deuda17-deuda16,
                     tipo.cambio=ifelse(cambio.deuda<0,"disminuyó", "aumentó"))

# ARRANGE

#Ordena los datos
morosidad1 <- arrange(morosidad1, deuda17, cambio.deuda)

#Si lo queremos en orden descendente usamos `desc()`
morosidad1 <- arrange(morosidad1, desc(deuda17), desc(cambio.deuda))


## %>%

#Este es el codigo que se corrio para lograr llegar a morosidad1
morosidad1 <- select(morosidad, -nombre.y, -situacion.y, -lugar.pago.y)
morosidad1 <- filter(morosidad1, !is.na(deuda17), !is.na(deuda16))
morosidad1 <- mutate(morosidad1, cambio.deuda=deuda17-deuda16)
morosidad1 <- mutate(morosidad1, tipo.cambio=ifelse(cambio.deuda<0,"disminuyó", "aumentó"))
morosidad1 <- arrange(morosidad1, desc(deuda17), desc(cambio.deuda))

#Codigo simplificado con el %>%
morosidad2 <- morosidad %>%
  select(-nombre.y, -situacion.y, -lugar.pago.y) %>%
  filter(!is.na(deuda17), !is.na(deuda16)) %>%
  mutate(cambio.deuda=deuda17-deuda16,
         tipo.cambio=ifelse(cambio.deuda<0,"disminuyó", "aumentó")) %>%
  arrange(desc(deuda17), desc(cambio.deuda))

#Exportar la base
#En csv
write.csv(morosidad2, "baselimpia.csv")



#### PARTE 2: TIDYR

#Cargar archivos
#Ponemos tbl_df() para convertirlo de una vez a una tibble
estudiantes <- tbl_df(read.csv("students.csv", header = T, sep= ","))
estudiantes2<- tbl_df(read.csv("students2.csv", header = T, sep= ","))

#ver base
estudiantes

#función gather
estudiantes_long <- gather(estudiantes, sexo, frecuencia, -grade)
estudiantes_long

#función spread
estudiantes_wide <- spread(estudiantes_long, sexo, frecuencia)
estudiantes_wide

#Estudiantes 2
estudiantes2

#limpieza con gather y separate
estudiantes2_long <- gather(estudiantes2, sexo_clase, frecuencia, -grade)
estudiantes2_long
estudiantes2_long2 <- separate(estudiantes2_long, sexo_clase, c("sexo", "clase"))
estudiantes2_long2

#todo en un mismo paso con %>%
estudiantes2_long <- estudiantes2 %>%
  gather(sexo_clase, frecuencia, -grade) %>%
  separate(sexo_clase, c("sexo", "clase"))  %>%
  print

#unite
estudiantes2_unida <- estudiantes2_long %>%
  unite(sexo_clase, sexo, clase, sep="-") %>%
  print
