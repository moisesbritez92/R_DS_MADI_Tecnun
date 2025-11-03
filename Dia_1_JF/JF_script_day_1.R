library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(jsonlite)
library(nycflights13)

# Introduccion inferencia estadistica ########

# Data
media = 3
varianza = 2

x <- seq(media-4*sqrt(varianza),media+4*sqrt(varianza),0.01)
y <- dnorm(x,mean=media,sd=sqrt(varianza))
plot(x,y,type="l",ylim = c(0,2))

#media es centrado y consistente.

A <- matrix(rnorm(10000*10,mean=media,sd=sqrt(varianza)),nrow=10000)
medias_n10 <- rowMeans(A)
lines(density(medias_n10),col="red")

B <- matrix(rnorm(10000*20,mean=media,sd=sqrt(varianza)),nrow=10000)
medias_n20 <- rowMeans(B)
lines(density(medias_n20),col="blue")
abline(v=media) #se ve que estan centrados

#que pasa con la varianza??
library(matrixStats)
vars_n10 <- rowVars(A)
plot(density(vars_n10),col="red",ylim=c(0,2))
mean(vars_n10)

vars_n20 <- rowVars(B)
lines(density(vars_n20),col="blue")
mean(vars_n20)

D <- matrix(rnorm(10000*100,mean=media,sd=sqrt(varianza)),nrow=10000)
vars_n100 <- rowVars(D)
lines(density(vars_n100),col="green")
abline(v=varianza) #se ve que estan centrados
mean(vars_n100)


####taks: codigo para mostrar que el estimador sesgado de la varianza
# es asintoticamente consitente 


#


#son robsutos
random <- rnorm(100,mean=media,sd=sqrt(varianza))
mean(random)
var(random)
#que pasa si metemos un valor atípico (outlier)
random[1] <- 1000
mean(random)
var(random)


# mad, mediana

random <- rnorm(100,mean=media,sd=sqrt(varianza))
mean(random)
var(random)
#que pasa si metemos un valor atípico (outlier)
random[1] <- 1000
mean(random)
median(random)
sd(random)
mad(random)
sqrt(varianza) #el valor que queremos estimar

#cual es mas eficiente, la media o la mediana??

medianas_n20 <- rowMedians(B)
plot(density(medias_n20),col="blue")
lines(density(medianas_n20),col="red")


#intervalo de confianza de la media

x <- rnorm(100)
t.test(x)


#intervalo de confianza de la varianza
## no existe una funcion en R que lo haga

n <- length(x)
alpha <- 0.05
s2 <- var(x)

# Cuantiles de la chi-cuadrado
chi2_lower <- qchisq(1 - alpha/2, df = n - 1)
chi2_upper <- qchisq(alpha/2, df = n - 1)

# Intervalo de confianza para la varianza
lower_bound <- (n - 1) * s2 / chi2_lower
upper_bound <- (n - 1) * s2 / chi2_upper

cat("Intervalo de confianza del 95% para la varianza:\n")
cat("(", lower_bound, ",", upper_bound, ")\n")

#





# 0. set working directory #######

# setwd("")


# 1.- cargar los datos #######

# Número de casos por provincia, fecha y tipo de detección
data1 <- read.csv('data/casos_tecnica_provincia.csv', 
                  header = T, sep = ',', stringsAsFactors = F,
                  check.names = F)

system.time(data1 <- read.csv('data/casos_tecnica_provincia.csv', 
                              header = T, sep = ',', stringsAsFactors = F,
                              check.names = F))

class(data1)
dim(data1)


#veamos que fread es mas rapido (con estos datos no se nota, pero 
#con data.frames mas grandes si que se nota)
system.time(data1_copia <- data.table::fread("./data/casos_tecnica_provincia.csv"))
class(data1_copia)
dim(data1_copia)
rm(data1_copia)


#cargar el resto de datos:


# Número de casos por comunidad autónoma, fecha y tipo de detección
data2 <- read.csv('data/casos_tecnica_ccaa.csv',
                  header = T, sep = ',', stringsAsFactors = F,
                  check.names = F)

# Número de casos, hospitalizaciones y defunciones por fecha, edad y sexo
data3 <- data.table::fread('data/casos_hosp_uci_def_sexo_edad_provres.csv', 
                           header = T, sep = ',',
                           stringsAsFactors = F, check.names = F)
data3 <- as.data.frame(data3)


# Read JSON files: aeropuertos:
myData1 <- fromJSON('./data/aeropuertos_2020.json')
output_dataframe1 <- as.data.frame(myData1$features)
data4 <- output_dataframe1$properties

myData2 <- fromJSON('data/airlines_delays.json',flatten = TRUE)
data5 <- as.data.frame(myData2)
dim(data5)
#flatten = TRUE se pone para unir todas las columnas en un unico data.frame
#con flatten = FALSE (valor por defecto) data5 tendria 3 columnas y cada
#columnas seria un data.frame.


#datos estudiantes

data6 = read.csv(file="./data/student-mat.csv",sep = ";")
View(data6)


## 1.1 explorar los datos #######

#data1 = Número de casos por comunidad autónoma, fecha y tipo de detección
#data2 = Número de casos por comunidad autónoma, fecha y tipo de detección
#data3 = Número de casos, hospitalizaciones y defunciones por fecha, edad y sexo
#data4 = aeropuertos
#data5 = retrasos aeropuerto
#data6 = datos de estudiantes


dim(data1)
head(data1)

dim(data2)
head(data2)

dim(data3)
head(data3)

dim(data4)
head(data4)
class(data4$geocode)


dim(data5)
class(data5)
colnames(data5)

dim(data6)
head(data6)


#2.- operacion basica con dplyr #########


# ejemplo, queremos saber para cada comunidad autónoma cuantos casos
# hay en total.

#bucle
system.time({
  casos_por_provincia = data.frame(provincia_iso  = unique(data3$provincia_iso),
                                   total_casos = 0)
  for(iix in 1:nrow(casos_por_provincia)){
    prov <- casos_por_provincia$provincia_iso[iix]
    iid <- which(data3$provincia_iso == prov)
    casos_por_provincia$total_casos[iix] <- sum(data3$num_casos[iid])
  }
  casos_por_provincia <- casos_por_provincia[order(casos_por_provincia$total_casos,decreasing = TRUE),]
  head(casos_por_provincia)
})



system.time({casos_por_provincia <- data3 %>% 
  group_by(provincia_iso) %>% 
  summarise(
    total_casos = sum(num_casos),
    .groups = 'drop'
  ) %>% 
  arrange(desc(total_casos)) 

head(casos_por_provincia)
})

#este código con un bucle es muy lento.
#poner %>% hace que sea más fácil de entender


#el mismo código sin usar %>%
arrange(summarise(group_by(data3,provincia_iso),total_casos=sum(num_casos),.groups = 'drop'),desc(total_casos))


#en el examen y en el trabajo se va a evaluar eficiencia del código.
#por lo general se pueden evitar los bucles.
#y con dplyr se puede hacer el código más fácil de entender.


# 3.- tidy ########

#en nuestros datos ya tenemos el formato de observaciones x variables.
#vamos a trabajar con la base de datos billboard (está cargada en la libreria tidyverse)
#es decir, no hace falta hacer nada especial para cargar esta base de datos
dim(billboard)
head(billboard)

#en este caso, hay dos variables que son:
# semana
# valor
#estos dos valores no están en el formato que queremos

billboard = billboard %>%
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )
head(billboard)
#





# 4.- transform rows #########
## 4.1.- duplicados (distinict y count)  ########

duplicated(data1) #un valor para cada fila.
sum(duplicated(data1))

sum(duplicated(data2))
sum(duplicated(data3)) #lleva tiempo
sum(duplicated(data4))
sum(duplicated(data5))
#para eliminar los duplicados, deberiamos quedarnos solo con los FALSE.

#todos
data4 <- data4 %>%
  distinct()

#quiero unicos
geocode <- data4 %>% 
  distinct(geocode)

table(duplicated(geocode)) #es equivalente a hacer un unique

geocode_gcd_icao <- data4 %>% 
  distinct(geocode,gcd_icao)
head(geocode_gcd_icao)

geocode_gcd_icao_all_columns <- data4 %>% 
  distinct(geocode,gcd_icao,.keep_all = TRUE)
head(geocode_gcd_icao_all_columns)


#¿Numero de vuelos en cada aeropuerto por año

data5 %>%
  count(Airport.Code,Time.Year) %>%
  head()



## 4.2.- Eliminar NA values (filter) ########

any(is.na(data1))
head(which(is.na(data1),arr.ind = TRUE))
data1[32,]

dim(data1)
any(is.na(data1$fecha))
data1 = na.omit(data1)
dim(data1)

#na.omit elimina las filas donde hay NA
## en ML veréis que algunos casos se pueden inferir los NA para no perder datos.

data2 = na.omit(data2)
data3 = na.omit(data3)

#para data4 queremos eliminar filas con NA en una columan específica.
colnames(data4)[c(4,6,7,8)]
#supongamos que solo nos interesa eliminar la fila si tiene NA en la columna "gcd_iata"

data4 <- data4 %>%
  filter(!is.na(gcd_iata))

any(is.na(data4$gcd_iata)) #hemos conseguido eliminar las filas donde habia NA en esta columna


## 4.3.- cambiar orden filas (arrange) ##########

#queremos que ordene las filas en funcion de un valor concreto

head(data4)
data4_ordenado <- data4 %>%
  arrange(longitud)
head(data4_ordenado)

data4_ordenado <- data4 %>%
  arrange(desc(longitud))
head(data4_ordenado)

#equivalente:
data4_ordenado <- data4[order(data4$longitud,decreasing = TRUE),]


#vamos a comparar los tiempos. Como es tan rapido no se puede medir con system.time
#para medir esto se puede utilizar microbenchmark

microbenchmark::microbenchmark(
  base = {
    data4_ordenado <- data4[order(data4$longitud,decreasing = TRUE),]
  },
  dplyr = {
    data4_ordenado <- data4 %>%
      arrange(desc(longitud))
  }
)
dim(data4)



#si hacemos lo mismo con data3
dim(data3)

microbenchmark::microbenchmark(
  base = {
    data3_ordenado <- data3[order(data3$grupo_edad,decreasing = TRUE),]
  },
  dplyr = {
    data3_ordenado <- data3 %>%
      arrange(desc(grupo_edad))
    
  },times = 10
)








## 4.4.- incorrect values ########

dim(data3)
unique(data3$sexo)
unique(data3$grupo_edad)
data3 <- data3[-which(data3$sexo =='NC'),]
data3 <- data3[-which(data3$grupo_edad =='NC'),]
dim(data3)

# 5.- transform columns #########
## 5.1.- add new columns (mutate) ########

dim(data2)
head(data2)
head(data3)

#añadir año, mes y dia en data2 y data3

data2$fecha[1]
year(data2$fecha[1])
day(data2$fecha[1])
month(data2$fecha[1])

data2_n <- data2 %>%
  mutate(year = year(fecha),
         day = day(fecha),
         month = month(fecha))

head(data2_n)

#con base:
data2_n <- data2
data2_n$year <- year(data2$fecha)
data2_n$dat <- day(data2$fecha)
data2_n$month <- month(data2$fecha)






## 5.2.- select ########

colnames(data3)
data3_new <- data3 %>%
  select(provincia_iso,fecha,grupo_edad)

head(data3_new)

## 5.3.- change column names (rename) ######

head(data3_new)
data3_new <- data3_new %>%
  rename(year_month = fecha)
head(data3_new)

## 5.4.- cambiar orden (relocate) #######

head(data3)
data3_new <- data3 %>%
  relocate(fecha:num_casos,.after = num_uci)
head(data3_new)

# 5.- groups ########
## 5.1.- incorrect values string patterns (group_by, summarise) ########

#son casos de datos que no estan completos
## estan todos los meses completos??

tabla_mes_dia <- data1 %>%
  mutate(
    year = year(fecha),
    month = month(fecha),
    day = day(fecha)
  ) %>%
  group_by(year,month) %>%
  summarise(
    max_day = max(day),
    .groups = 'drop'
  )

print(tabla_mes_dia,n=Inf) #queremos ver todo


#en data1 se puede observar que septiembre del 2021 no está completo
#comprobar esto para data1 y data2

# eliminar los meses que no estén completos en data1

data1_new <- data1[-!str_detect(string = data1$fecha,pattern = "2021-09"),]
#en data1_new hemos quitado año 2021 y mes 09.


# 6.- factors ########

head(data6)

class(data6$age)
#por lo general las variables numericas no dan problemas
#por defecto son numericas

class(data6$sex)
#las categoricas dan problemas. Por defecto son tipo "character"

data6$sex <-  as.factor(data6$sex)
head(data6$sex)

#otra forma de hacerlo:
data6$sex <- factor(data6$sex,levels = c("M","F"))
head(data6$sex) #podemos elegir el orden de los levels.

#problema viene si los levels son numeros
class(data6$freetime)
head(data6$freetime)
#esto es una categoria ordenada
data6$freetime <- as.factor(data6$freetime)
head(data6$freetime)
# ejercicios ########


#datos de la libreria nycflights13::flights
dim(flights)
head(flights)

## ejercicio 1 ######
class(flights)
flights %>%
  count(dep_delay >=2)

flights %>%
  filter(dep_delay >=2) %>%
  count()





