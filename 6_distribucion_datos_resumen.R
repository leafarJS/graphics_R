library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

##################################
#6.1. Making a Basic Histogram
#6.1. Hacer un Histograma Basico
##################################

view(faithful)
str(faithful)

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_histogram()

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_histogram(binwidth = 5,
                 fill = "#f768a1", 
                 colour = "#49006a")

# Divide the x range into 15 bins
range(faithful$waiting)
diff(range(faithful$waiting))
bin.size <- diff(range(faithful$waiting))/15

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_histogram(binwidth = bin.size,
                 fill = "#969696",
                 colour = "#252525")

x <- faithful %>% 
  ggplot(aes(
    x = waiting
  ))

x + geom_histogram(binwidth = 8,
                   fill = "tomato",
                   colour = "black",
                   origin = 31)

x + geom_histogram(binwidth = 8,
                   fill = "tomato",
                   colour = "black",
                   origin = 35)

##############################################################
#6.2. Making Multiple Histograms from Grouped Data
#6.2. Hacer mĂșltiples histogramas a partir de datos agrupados
##############################################################

library(MASS)
#Factores de riesgo asociados con el bajo peso al nacer del lactante
view(birthwt)
str(birthwt)
?birthwt

birthwt %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill = "#fe9929", 
                 colour = "#993404")+
  facet_grid((smoke ~ .))

x <- birthwt
x$smoke <- factor(x$smoke)
levels(x$smoke)

x$smoke <- revalue(x$smoke, c("0" = "No Smoke", "1"="Smoke"))

x %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill="#aa11bb",
                 colour = "black")+
  facet_grid(smoke ~ .)


x %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill="#aa11bb",
                 colour = "black")+
  facet_grid(race ~ .)

x %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill="#aa11bb",
                 colour = "black")+
  facet_grid(race ~ ., scales = "free")


str(x)
x %>% 
  ggplot(aes(
    x = bwt,
    fill = smoke
  ))+
  geom_histogram(position = "identity", 
                 alpha = 0.4)

##################################
#6.3. Making a Density Curve
#6.3. Hacer una Curva de Densidad
##################################

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_density()

# El expand_limits() aumenta el rango y para incluir el valor 0

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_line(stat = "density")


faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_line(stat = "density")+
  expand_limits(y=0)


x <- faithful$waiting
ggplot(NULL, aes(
  x = x
))+
  geom_density()

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_line(stat = "density",
            adjust = .25,
            colour = "red")


faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_line(stat = "density",
            adjust = .25,
            colour = "red")+
  geom_line(stat = "density")


faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_line(stat = "density",
            adjust = .25,
            colour = "red",
            size = 1.5)+
  geom_line(stat = "density")+
  geom_line(stat = "density",
            adjust = 2,
            colour = "blue")

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_density(fill = "blue",
               alpha = .2)

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_density(fill = "blue",
               alpha = .2)+
  xlim(35,105)

# Esto dibuja un polĂ­gono azul con geom_density(), luego agrega una lĂ­nea en la parte superior
faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_density(fill = "blue",
               colour = NA,
               alpha = .2)+
  geom_line(stat = "density")+
  xlim(35,105)


faithful %>% 
  ggplot(aes(
    x = waiting,
    y = ..density..
  ))+
  geom_histogram()


faithful %>% 
  ggplot(aes(
    x = waiting,
    y = ..density..
  ))+
  geom_histogram(fill = "#fed976",
                 colour = "#fd8d3c",
                 size = .2)+
  geom_density()+
  xlim(35,105)


faithful %>% 
  ggplot(aes(
    x = waiting,
    y = ..density..
  ))+
  geom_histogram(fill = "#fed976",
                 colour = "#fd8d3c",
                 size = .2)+
  geom_density()+
  xlim(35,105)


faithful %>% 
  ggplot(aes(
    x = waiting,
    y = ..density..
  ))+
  geom_histogram(fill = "#fed976",
                 colour = "#fd8d3c",
                 size = .2)+
  geom_density(fill = "blue",
               alpha = .2)+
  xlim(35,105)

#####################################################################
#6.4. Making Multiple Density Curves from Grouped Data
#6.4. Hacer mĂșltiples curvas de densidad a partir de datos agrupados
#####################################################################

x <- birthwt
x$smoke <- factor(x$smoke)

x %>% 
  ggplot(aes(
    x = bwt,
    colour = smoke
  ))+
  geom_density()

x %>% 
  ggplot(aes(
    x = bwt,
    fill = smoke
  ))+
  geom_density()

x %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_density()+
  facet_grid(smoke ~ .)

x %>% 
  ggplot(aes(
    x = bwt,
    fill = smoke
  ))+
  geom_density()+
  facet_grid(smoke ~ .)

x %>% 
  ggplot(aes(
    x = bwt,
    fill = smoke
  ))+
  geom_density(alpha = 0.2)+
  facet_grid(smoke ~ .)

x$smoke <- revalue(x$smoke, c("0" = "No Fumador", "1" = "Fumador"))

x %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_density()+
  facet_grid(smoke ~ .)

# Si desea ver los histogramas junto con las curvas de densidad, la mejor opciĂłn
# es utilizar facetas, ya que se pueden utilizar otros mĂ©todos para visualizar 
# ambos histogramas en un solo grĂĄfico. difĂ­cil de interpretar. Para hacer esto,
# mapee y=..density.., para que el histograma se escale hasta la altura de las 
# curvas de densidad.

x %>% 
  ggplot(aes(
    x = bwt, 
    y = ..density..
  ))+
  geom_histogram(binwidth = 200,
                 fill = "cornsilk",
                 colour = "gray60",
                 size = .2)

x %>% 
  ggplot(aes(
    x = bwt, 
    y = ..density..
  ))+
  geom_histogram(binwidth = 200,
                 fill = "cornsilk",
                 colour = "gray60",
                 size = .2)+
  geom_density()

x %>% 
  ggplot(aes(
    x = bwt, 
    y = ..density..
  ))+
  geom_histogram(binwidth = 200,
                 fill = "cornsilk",
                 colour = "gray60",
                 size = .2)+
  geom_density()+
  facet_grid(smoke ~ .)

x %>% 
  ggplot(aes(
    x = bwt, 
    y = ..density..
  ))+
  geom_histogram(binwidth = 200,
                 fill = "cornsilk",
                 colour = "gray60",
                 size = .2)+
  geom_density(fill = "blue",
               alpha = 0.3)+
  facet_grid(smoke ~ .)

######################################
#6.5. Making a Frequency Polygon
#6.5. Hacer un Poligono de Frecuencia
######################################

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_freqpoly()

# Un polĂ­gono de frecuencia parece similar a una curva de estimaciĂłn de densidad
# kernel, pero muestra la misma informaciĂłn que un histograma. Es decir, como un
# histograma, muestra lo que hay en los datos, mientras que una estimaciĂłn de la
# densidad del kernel es solo eso, una estimaciĂłn, y requiere que elija algĂșn 
# valor para el ancho de banda

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_freqpoly(binwidth = 4)

range(faithful$waiting)
diff(range(faithful$waiting))
bin.size = diff(range(faithful$waiting)) /15

faithful %>% 
  ggplot(aes(
    x = waiting
  ))+
  geom_freqpoly(binwidth = bin.size)

#######################################
#6.6. Making a Basic Box Plot
#6.6. Hacer un diagrama de caja bĂĄsico
#######################################

str(birthwt)
birthwt %>% 
  ggplot(aes(
    #usar factor() para convertir variables numĂ©ricas a discretas
    x = factor(race),
    y = bwt
  ))+
  geom_boxplot()


birthwt %>% 
  ggplot(aes(
    x = factor(race),
    y = bwt
  ))+
  geom_boxplot(width = .5)

# Un diagrama de caja consta de una caja y âbigotesâ. La caja va del percentil 25
# al el percentil 75 de los datos, tambiĂ©n conocido como rango intercuartil (IQR).
# Hay una lĂ­nea que indica la mediana o percentil 50 de los datos. Los bigotes
# comienzan desde el borde de la caja y extiĂ©ndase hasta el punto de datos mĂĄs 
# alejado que estĂ© dentro de 1,5 veces el IQR. Si hay puntos de datos que estĂĄn 
# mĂĄs allĂĄ de los extremos de los bigotes, se consideran valores atĂ­picos y se
# muestran con puntos.

birthwt %>% 
  ggplot(aes(
    x = factor(race),
    y = bwt
  ))+
  geom_boxplot(outlier.size = 1.5,
               outlier.shape = 21)

birthwt %>% 
  ggplot(aes(
    x = 1,
    y = bwt
  ))+
  geom_boxplot()+
  scale_x_continuous(breaks = NULL)+
  theme(axis.text.x = element_blank())

############################################
#6.7. Adding Notches to a Box Plot
#6.7. AĂ±adir muestras a un diagrama de caja
############################################

birthwt %>% 
  ggplot(aes(
    x = factor(race),
    y = bwt
  ))+
  geom_boxplot(notch = TRUE)

# Las muescas se utilizan en los diagramas de caja para ayudar a evaluar visualmente 
# si las medianas de las distribuciones difieren. Si las muescas no se superponen,
# esto es evidencia de que las medianas son diferentes. Con este conjunto de datos
# en particular, verĂĄ el siguiente mensaje:   Notch saliĂł de las bisagras. Intente
# configurar muesca = FALSO.


##############################################
#6.8. Adding Means to a Box Plot
#6.8. AdiciĂłn de medias a un diagrama de caja
##############################################

birthwt %>% 
  ggplot(aes(
    x = factor(race),
    y = bwt
  ))+
  geom_boxplot()+
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 23,
               size = 3,
               fill = "yellow")

# La lĂ­nea horizontal en el medio de un diagrama de caja muestra la mediana, no la
# media. Para datos que se distribuyen normalmente, la mediana y la media serĂĄn
# aproximadamente iguales, pero para datos sesgados estos valores diferirĂĄn

################################
#6.9. Making a Violin Plot
#6.9. Hacer un trama de violin
################################

view(heightweight)
str(heightweight)

x <- heightweight %>% 
  ggplot(aes(
    x = sex,
    y = heightIn
  ))

x + geom_violin()

x + geom_violin()+
  geom_boxplot(width = .1,
               fill = "black",
               outlier.color = NA)+
  stat_summary(fun.y = median,
               geom = "point",
               fill = "white",
               shape = 21,
               size = 2.5)

x + geom_violin(scale = "count")

x + geom_violin(adjust = 2)

x + geom_violin(adjust = .5)

###################################
#6.10. Making a Dot Plot
#6.10. Hacer un diagrama de puntos
###################################

view(countries)
str(countries)
unique(countries$Name)

x <- subset(countries, Year == 2009 & healthexp > 2000)

y <- x %>% 
  ggplot(aes(
    x = infmortality
  ))

y + geom_dotplot()

y + geom_dotplot(binwidth = .25)

y + geom_dotplot(binwidth = .25)+
  geom_rug()

y + geom_dotplot(binwidth = .25)+
  geom_rug()+
  scale_y_continuous(breaks = NULL) #Elimina marcadores de ticks

y + geom_dotplot(binwidth = .25)+
  geom_rug()+
  scale_y_continuous(breaks = NULL)+
  theme(axis.title.y = element_blank())

y + geom_dotplot(method = "histodot",
                 binwidth = .25)+
  geom_rug()+
  scale_y_continuous(breaks = NULL)+
  theme(axis.title.y = element_blank())

y + geom_dotplot(binwidth = .25,
                 stackdir = "center")+
  scale_y_continuous(breaks = NULL)+
  theme(axis.title.y = element_blank())

y + geom_dotplot(binwidth = .25,
                 stackdir = "centerwhole")+
  scale_y_continuous(breaks = NULL)+
  theme(axis.title.y = element_blank())

###############################################################
#6.11. Making Multiple Dot Plots for Grouped Data
#6.11. Hacer diagramas de puntos mĂșltiples para datos agrupados
###############################################################

heightweight %>% 
  ggplot(aes(
    x = sex,
    y = heightIn
  ))+
  geom_dotplot(binaxis = "y",
               binwidth = .5,
               stackdir = "center")

heightweight %>% 
  ggplot(aes(
    x = sex,
    y  =heightIn
  ))+
  geom_boxplot(outlier.colour = NA,
               width = .4)+
  geom_dotplot(binaxis = "y",
               binwidth = .5,
               stackdir = "center",
               fill = NA)

heightweight %>% 
  ggplot(aes(
    x = sex,
    y = heightIn
  ))+
  geom_boxplot(aes(
    x = as.numeric(sex) + .2,
    group = sex
  ), width = .25)

heightweight %>% 
  ggplot(aes(
    x = sex,
    y = heightIn
  ))+
  geom_boxplot(aes(
    x = as.numeric(sex) + .2,
    group = sex
  ), width = .25)+
  geom_dotplot(aes(
    x = as.numeric(sex) - .2,
    group = sex
  ), binaxis = "y",
  binwidth = .5,
  stackdir = "center")

heightweight %>% 
  ggplot(aes(
    x = sex,
    y = heightIn
  ))+
  geom_boxplot(aes(
    x = as.numeric(sex) + .2,
    group = sex
  ), width = .25)+
  geom_dotplot(aes(
    x = as.numeric(sex) - .2,
    group = sex
  ), binaxis = "y",
  binwidth = .5,
  stackdir = "center")+
  scale_x_continuous(breaks = 1:nlevels(heightweight$sex),
                     labels = levels(heightweight$sex))

#############################################################
#6.12. Making a Density Plot of Two-Dimensional Data
#6.12. Hacer un grĂĄfico de densidad de datos bidimensionales
#############################################################

view(faithful)
str(faithful)

x <- faithful %>% 
  ggplot(aes(
    x = eruptions,
    y = waiting
  ))

x + geom_point()+
  stat_density2d()

x + stat_density2d(aes(
  colour = ..level..
))

x + stat_density2d(aes(
  fill = ..density..
), geom = "raster",
contour = FALSE)

x + geom_point()+
  stat_density2d(aes(
    alpha = ..density..
  ), geom = "tile",
  contour = FALSE)

x + stat_density2d(aes(
  fill = ..density..
), geom = "raster", 
contour = FALSE,
h = c(.5, 5))
