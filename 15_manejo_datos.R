library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Poniendo los datos en forma

# Cuando se trata de hacer gráficos, la mitad de la batalla ocurre antes de llamar
# a cualquier comando de gráficos. Antes de pasar sus datos a las funciones gráficas,
# primero debe leerlos en tablas y dada la estructura correcta. Los conjuntos de datos 
# proporcionados con R están listos para usar, pero cuando se trata de datos del 
# mundo real, este no suele ser el caso: tendrá que limpiar y reestructurar los datos
# antes de poder visualizarlos. Los conjuntos de datos en R se almacenan con mayor 
# frecuencia en marcos de datos. Por lo general, se utilizan como estructuras de datos
# bidimensionales, en las que cada fila representa un caso y cada columna
# representando una variable. Los marcos de datos son esencialmente listas de vectores
# y factores, todos de la misma longitud, donde cada vector o factor representa 
# una columna.

View(heightweight)
str(heightweight)
head(heightweight, 2)
tail(heightweight, 2)


#15.1. Creating a Data Frame
#15.1. Creación de un marco de datos

#two starting vectors
g <- c("A", "B", "C")
x <- 1:3

df <- data.frame(g, x)
df
typeof(df)
# Un marco de datos es esencialmente una lista de vectores y factores. Cada vector
# o factor se puede considerar como una columna en el marco de datos.
# Si sus vectores están en una lista, puede convertir la lista en un marco de datos
# con as.data Función .frame()

lst <- list(group = g, value = x) # a list of vectors

dat <- as.data.frame(lst)
dat
typeof(dat)




#15.2. Getting Information About a Data Structure
#15.2. Obtener información sobre una estructura de datos

str(ToothGrowth)
tg <- ToothGrowth
tg$supp <- as.character(tg$supp)

str(tg)

#from old data frame factor
ToothGrowth$supp
#from new data frame character
tg$supp


#15.3. Adding a Column to a Data Frame
#15.3. Agregar una columna a un marco de datos


data$newcol <- NA

data$newcol <- vec


#Cada "columna" de un marco de datos es un vector o factor. R los maneja de 
# manera ligeramente diferente a los vectores independientes, porque todas las 
# columnas en un marco de datos tienen la misma longitud.



#15.4. Deleting a Column from a Data Frame
#15.4. Eliminación de una columna de un marco de datos


data$badcol <- NULL

#Return data without badcol
data <- subset(data, select = -badcol)

#Exclude badcol and othercol
data <- subset(data, select = c(-badcol, -othercol))



#15.5. Renaming Columns in a Data Frame
#15.5. Cambio de nombre de columnas en un marco de datos


names(dat) <- c("name1", "name2", "name3")
dat


view(anthoming)
str(anthoming)
names(anthoming) #print the names of the columns

names(anthoming)[names(anthoming) == "ctrl"] <- c("Control")
names(anthoming)[names(anthoming) == "expt"] <- c("Experimental")
names(anthoming)


names(anthoming)[1] <-"Angulo"

names(anthoming)


#15.6. Reordering Columns in a Data Frame
#15.6. Reordenación de columnas en un marco de datos

dat

#Para reordenar las columnas por su posición numérica:
dat <- dat[c(1,3,2)]

#Para reordenar las columnas por su nombre
dat <- dat[c("col1", "col3", "col2")]


head(anthoming, 2)

anthoming[c(1,3,2)] #list-style indexing


# No poner nada antes de la coma significa seleccionar todas las filas
anthoming[, c(1,3,2)] #matrix-style indexing


anthoming[3] #list-style indexing

anthoming[, 3] #matrix-style indexing

anthoming[, 3, drop = FALSE] #matrix-style indexing with drop = FALSE



#15.7. Getting a Subset of a Data Frame
#15.7. Obtener un subconjunto de un marco de datos

view(climate)
str(climate)
head(climate, 2)
tail(climate, 2)


subset(climate, 
       Source == "Berkeley",
       select = c(Year, Anomaly10y))


subset(climate,
       Source == "Berkeley" & Year >= 1900 & Year <= 2000,
       select = c(Year, Anomaly10y))


climate[climate$Source=="Berkeley" & climate$Year >= 1900 & climate$Year <= 2000,
        c("Year", "Anomaly10y")]

climate[climate$Source=="Berkeley" & climate$Year >= 1900 & climate$Year <= 2000,
        c("Year", "Anomaly10y"),
        drop = FALSE]

climate[1:100, c(2,5)]


#15.8. Changing the Order of Factor Levels
#15.8. Cambiar el orden de los niveles de los factores

#by default, levels are order alphabetically
sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes


#change the order of levels
sizes = factor(sizes, levels = c("small", "medium", "large"))
sizes

factor(sizes, levels  =rev(levels(sizes)))
#El orden también se puede especificar con niveles cuando se crea el factor por
#primera vez.



#15.9. Changing the Order of Factor Levels Based on Data Values
#15.9. Cambiar el orden de los niveles de los factores en función de los valores de los datos


view(InsectSprays)
str(InsectSprays)
head(InsectSprays, 2)
tail(InsectSprays, 2)

## Make a copy since we'll modify it
iss <- InsectSprays
iss$spray

iss$spray <-reorder(iss$spray, iss$count, FUN = mean)
iss$spray



#15.10. Changing the Names of Factor Levels
#15.10. Cambiar los nombres de los niveles de los factores


sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes

levels(sizes)

#with revalue(), pass it named vector with the mappings
sizes1 <- revalue(sizes, c(small = "S", medium = "M", large = "L"))
sizes1


sizes1 <- revalue(sizes, c("small" = "S", "medium" = "M", "large" = "L"))
sizes1


## mapvalues() le permite usar dos vectores separados en lugar de un vector con nombre
mapvalues(sizes, c("small", "medium", "large"), c("s", "m", "l"))



sizes <- factor(c("small", "large", "large", "small", "medium"))

#index into the levels and rename each one
levels(sizes)[levels(sizes)=="large"] <- "Lg"
levels(sizes)[levels(sizes)=="medium"] <- "Md"
levels(sizes)[levels(sizes)=="small"] <- "Sm"
sizes



sizes <- factor(c("small", "large", "large", "small", "medium"))
levels(sizes) <- list(S="small", M="medium", L="large")
sizes


#by default, levels are ordered alphabetically
sizes <- factor(c("small", "large", "large", "small", "medium"))
levels(sizes)[1] <- "L"
sizes


#rename all levels at once
levels(sizes) <- c("Lg", "Me", "Sl")
sizes



#15.11. Removing Unused Levels from a Factor
#15.11. Eliminación de niveles no utilizados de un factor


sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes <- sizes[1:3]
sizes

# The droplevels() function preserves the order of factor levels.
# You can use the except argument to keep particular levels.
sizes <- droplevels(sizes)
sizes



#15.12. Changing the Names of Items in a Character Vector
#15.12. Cambiar los nombres de los elementos en un vector de caracteres


sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes

#with revalue(), pass it a named vector with the mappings
sizes1 <- revalue(sizes, c(small = "S", medium = "M", large = "L"))
sizes1


revalue(sizes, c("small" = "S", "medium" = "M", "large" = "L"))


# mapvalues() lets you use two separate vectors instead of a named vector
mapvalues(sizes, c("small", "medium", "large"), c("Sm", "Md", "Lg"))



#_____________DESCONTINUADO_______________________#
sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes
sizes[sizes == "small"] <- "S"
sizes[sizes == "medium"] <- "M"
sizes[sizes == "large"] <- "L"

sizes
#_____________END DESCONTINUADO_______________________#


#15.13. Recoding a Categorical Variable to Another Categorical Variable
#15.13. Recodificación de una variable categórica a otra Variable categórica


#work on a subset of the PlantGrowth data set 
str(PlantGrowth)
pg <- PlantGrowth[c(1,2,11,21,30),]
pg


pg <- PlantGrowth

oldvals <- c("ctrl", "trt1", "trt2")
newvals <- factor(c("No", "Yes", "Yes"))


pg$treatment <- newvals[match(pg$group, oldvals)]

pg

#_____________DESCONTINUADO_______________________#
pg$treatment[pg$group == "ctrl"] <- "no"
pg$treatment[pg$group == "trt1"] <- "yes"
pg$treatment[pg$group == "trt2"] <- "yes"

pg

#convert to a factor
pg$treatment <- factor(pg$treatment)
pg

#_____________END DESCONTINUADO_______________________#

pg <- PlantGrowth

pg$newcol[pg$group == "ctrl" & pg$weight < 5] <- "no_small"
pg$newcol[pg$group == "ctrl" & pg$weight >= 5] <- "no_large"
pg$newcol[pg$group == "trt1"] <- "yes"
pg$newcol[pg$group == "trt2"] <- "not"

pg$newcol<- factor(pg$newcol)
pg
str(pg)

pg$weighttrt <- interaction(pg$weight, pg$group)
pg



#15.14. Recoding a Continuous Variable to a Categorical Variable
#15.14. Recodificación de una variable continua a una Variable categórica

?cut
pg <- PlantGrowth[c(1,2,11,21,22),]
pg

pg$wtclass <- cut(pg$weight, breaks = c(0,5,6,Inf))
pg


pg$wtclass <- cut(pg$weight, 
                  breaks = c(0,5,6,Inf),
                  labels = c("Sm", "Md", "Lg"))
pg

cut(pg$weight, breaks = c(0,5,6,Inf), right = FALSE)



#15.15. Transforming Variables
#15.15. Transformación de variables

View(heightweight)
str(heightweight)
head(heightweight,2)
tail(heightweight,2)

hw <- heightweight
hw$hightCm <- hw$heightIn * 2.54
hw


# Para un código un poco más fácil de leer, puede usar transform() o mutate() del
# paquete plyr. Solo necesita especificar el marco de datos una vez, como primer 
# argumento de la función, lo que significa que proporciona una sintaxis más limpia,
# especialmente si está transformando múltiples variables:

hw <- transform(hw, 
                heightCm = heightIn * 2.54,
                weighKg = weightLb / 2.204)
hw

hw <- mutate(hw,
             heightCm = heightIn * 2.54,
             weighKg = weightLb / 2.204)
hw

## These all have the same effect:

hw <- transform(hw,
                bmi = weighKg / (heightCm / 100)^2)
hw

hw <- mutate(hw,
             bmi = weighKg  / (heightCm / 100)^2)
hw


hw$bmi <- hw$weighKg / (hw$heightCm/100)^2
hw


#Sin embargo, con mutate(), podemos calcularlos todos de una sola vez. El 
#siguiente código tiene el mismo efecto que los bloques separados anteriores:

hw <- heightweight
hw <- mutate(hw, 
             heightCm = heightIn * 2.54,
             weightKg = weightLb / 2.204,
             bmi = weightKg / (heightCm /100)^2)
hw



#15.16. Transforming Variables by Group
#15.16. Transformar variables por grupo

library(MASS)

view(cabbages)
str(cabbages)

cb <- ddply(cabbages, "Cult", transform, DevWt = HeadWt - mean(HeadWt))
cb

transform(cabbages, DevWt = HeadWt - mean(HeadWt))



cb <- ddply(cabbages, "Cult", transform, DevWt = HeadWt - mean(HeadWt))
head(cb, 3)

#the data before normalizing
cb %>% 
  ggplot(aes(
    x = Cult,
    y = HeadWt
  ))+
  geom_boxplot()

#after normalizing
cb %>% 
  ggplot(aes(
    x = Cult,
    y = DevWt
  ))+
  geom_boxplot()

#También puede dividir el marco de datos en múltiples variables y realizar 
# operaciones en múltiples variables. Esto se dividirá por Cult y Data, formando
# un grupo para cada combinación única de las dos variables, y luego calculará la
# desviación de la media de HeadWt y VitC dentro de cada grupo:


ddply(cabbages,
      c("Cult", "Date"),
      transform,
      devWt = HeadWt - mean(HeadWt),
      DevVitc = VitC - mean(VitC))


#15.17. Summarizing Data by Groups
#15.17. Resumen de datos por grupos

ddply(cabbages, 
      c("Cult", "Date"),
      summarise,
      Weight = mean(HeadWt),
      VitC = mean(VitC))

head(cabbages, 3)


summarise(cabbages, Weight = mean(HeadWt))


ddply(cabbages,
      "Cult",
      summarise,
      Weight = mean(HeadWt))

ddply(cabbages,
      c("Cult", "Date"),
      summarise,
      Weight = mean(HeadWt),
      VitC = mean(VitC))


ddply(cabbages,
      c("Cult", "Date"),
      summarise,
      Weight = mean(HeadWt),
      sd = sd(HeadWt),
      n = length(HeadWt))


#Dealing with NAs
#tratar con NA's

x <- cabbages
x$HeadWt[c(1,2,45)] <- NA #set some values to NA

ddply(x,
      c("Cult", "Date"),
      summarise,
      Weight = mean(HeadWt),
      sd = sd(HeadWt),
      n = length(HeadWt))

#para no mostrar resultados que tiene NA's
x

x1 <- ddply(x,
      c("Cult", "Date"),
      summarise,
      Weight = mean(HeadWt, na.rm = TRUE),
      sd = sd(HeadWt, na.rm = TRUE),
      n = sum(!is.na(HeadWt)))


# Copy cabbages and remove all rows with both c52 and d21
c2 <- subset(x, !( Cult=="c52" & Date=="d21" ) )
c2a <- ddply(c2, c("Cult", "Date"), summarise,
             Weight = mean(HeadWt, na.rm=TRUE),
             sd = sd(HeadWt, na.rm=TRUE),
             n = sum(!is.na(HeadWt)))


c2a %>% 
ggplot(aes(
  x = Date,
  fill = Cult,
  y = Weight
))+
  geom_bar(position = "dodge",
           stat = "identity")


c2b <- ddply(c2,
             c("Cult", "Date"),
             .drop = FALSE,
             summarise,
             Weight = mean(HeadWt, na.rm = TRUE),
             sd = sd(HeadWt, na.rm = TRUE),
             n = sum(!is.na(HeadWt)))
c2b %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cult
  ))+
  geom_bar(position = "dodge",
           stat = "identity")


#15.18. Summarizing Data with Standard Errors and Confidence Intervals
#15.18. Resumen de datos con errores estándar y Intervalos de confianza

ca <- ddply(cabbages,
             c("Cult", "Date"),
             summarise,
             Weight = mean(HeadWt, na.rm = TRUE),
             sd = sd(HeadWt, na.rm = TRUE),
             n = sum(!is.na(HeadWt)),
             se = sd/sqrt(n))
ca


ddply(cabbages,
      c("Cult", "Date"), 
      summarise,
      Weight = mean(HeadWt, na.rm=TRUE),
      sd = sd(HeadWt, na.rm=TRUE),
      n = sum(!is.na(HeadWt)),
      se = sd / sqrt(n))


# Intervalos de confianza
# Los intervalos de confianza se calculan utilizando el error estándar de la media
# y los grados de libertad. Para calcular un intervalo de confianza, use la función
# qt() para obtener el cuantil, luego multiplíquelo por el error estándar. La 
# función qt() dará cuantiles de la distribución t cuando se le dé un nivel de 
# probabilidad y grados de libertad. Para un intervalo de confianza del 95%, 
# use un nivel de probabilidad de .975; para la distribución t en forma de campana, 
# esto en esencia cortará el 2,5% del área bajo la curva en cada extremo. Los grados
# de libertad son iguales al tamaño de la muestra menos uno.

ciMult <- qt(.975, ca$n - 1)
ciMult

ca$ci <- ca$se * ciMult

ca


#Podríamos haber hecho todo esto en una sola línea, así:
ca$ci95 <- ca$se * qt(.975, ca$n)


# Las barras de error que representan el error estándar de la media y los intervalos 
# de confianza tienen el mismo propósito general: dar al espectador una idea de qué
# tan buena es la estimación de la la media poblacional es. El error estándar es la
# desviación estándar de la distribución muestral. Los intervalos de confianza son
# más fáciles de interpretar. Muy aproximadamente, un 95% de confianza intervalo 
# significa que hay un 95 % de posibilidades de que la verdadera media de la población
# esté dentro del intervalo (en realidad, no significa esto en absoluto, pero este 
# tema aparentemente simple es demasiado complicado de cubrir aquí; si quiere saber más,
# lea sobre estadísticas bayesianas). Esta función realizará todos los pasos para calcular
# la desviación estándar, el conteo, el error estándar y los intervalos de confianza.
# También puede manejar NA y combinaciones faltantes, con las opciones na.rm y .drop.
# De forma predeterminada, proporciona un intervalo de confianza del 95 %, pero se puede
# configurar con el argumento conf.interval:


summarySE <- function(data=NULL, measurevar, groupvars=NULL,
                      conf.interval=.95, na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  # New version of length that can handle NAs: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  # This does the summary
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, na.rm) {
                   c( n = length2(xx[,col], na.rm=na.rm),
                      mean = mean (xx[,col], na.rm=na.rm),
                      sd = sd (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$n) # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use
  # df=n-1, or if n==0, use df=0
  ciMult <- qt(conf.interval/2 + .5, datac$n-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

# Remove all rows with both c52 and d21
c2 <- subset(cabbages, !( Cult=="c52" & Date=="d21" ) )
# Set some values to NA
c2$HeadWt[c(1,20,45)] <- NA
summarySE(c2, "HeadWt", c("Cult", "Date"), conf.interval=.99,
          na.rm=TRUE, .drop=FALSE)



#15.19. Converting Data from Wide to Long
#15.19. Conversión de datos de ancho a largo

view(anthoming)
str(anthoming)
head(anthoming,2)
tail(anthoming,2)

library(reshape2)
melt(anthoming, id.vars="Angulo", variable.name="condition", value.name="count")


view(drunk)
str(drunk)
melt(drunk,
     id.vars = "sex",
     measure.vars = c("0-29", "30-39"),
     variable.name = "age",
     value.name = "count")



view(plum_wide)
str(plum_wide)

melt(plum_wide,
     id.vars = c("length", "time"),
     variable.name = "survival", 
     value.name = "count")



view(corneas)
str(corneas)

co <- corneas
co$id <- 1:nrow(co)

melt(co, 
     id.vars = "id",
     variable.name = "eye",
     value.name = "thickness")



#15.20. Converting Data from Long to Wide
#15.20. Conversión de datos de largo a ancho

view(plum)
dcast(plum,
      length + time ~ survival,
      value.var = "count")

dcast(plum,
      time ~ length + survival,
      value.var = "count")


#15.21. Converting a Time Series Object to Times and Values
#15.21. Conversión de un objeto de serie temporal en tiempos y Valores


view(nhtemp)
str(nhtemp)
length(nhtemp)


# Get times for each observation
x <- as.numeric(time(nhtemp))

# Get value of each observation
y <- as.numeric(nhtemp)

#put them in a data frame
df <- data.frame(x, y)
df




view(presidents)
str(presidents)

pres_rating <- data.frame(
  year = round(as.numeric(time(presidents),0)),
  rating = as.numeric(presidents))

pres_rating


pres_rating_2 <- data.frame(
  year = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents)),
  rating = as.numeric(presidents)
)  
pres_rating_2
