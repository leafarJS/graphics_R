library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Ejes de simetria
# Los ejes x e y proporcionan contexto para interpretar los datos mostrados. 
# ggplot2 mostrará los ejes con valores predeterminados que se ven bien en la
# mayoría de los casos, pero es posible que desee controlar, por ejemplo, las 
# etiquetas de los ejes, el número y la ubicación de las marcas de graduación, 
# la marca de graduación etiquetas, y así sucesivamente.

################################################
#8.1. Swapping X- and Y-Axes
#8.1. Intercambio de los ejes de X y ejes de Y
################################################

view(PlantGrowth)
str(PlantGrowth)

PlantGrowth %>% 
  ggplot(aes(
    x = group, 
    y = weight
  ))+
  geom_boxplot()

PlantGrowth %>% 
  ggplot(aes(
    x = group, 
    y = weight
  ))+
  geom_boxplot()+
  coord_flip()

PlantGrowth %>% 
  ggplot(aes(
    x = group, 
    y = weight
  ))+
  geom_boxplot()+
  coord_flip()+
  scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

# A veces, cuando se intercambian los ejes, el orden de los elementos será el 
# inverso al usted quiere. En un gráfico con ejes x e y estándar, los elementos x
# comienzan a la izquierda y van a la derecha, que corresponde a la forma normal 
# de lectura, de izquierda a derecha. Cuando usted intercambiar los ejes, los 
# artículos aún van desde el origen hacia afuera, que en este caso será desde
# de abajo hacia arriba, pero esto entra en conflicto con la forma normal de 
# lectura, de arriba hacia abajo. A veces esto es un problema, ya veces no lo es. 
# Si la variable x es un factor, el orden se puede revertir usando scale_x_discrete()
# con limites=rev(niveles(...))

#################################################
#8.2. Setting the Range of a Continuous Axis
#8.2. Configuración del rango de un eje continuo
#################################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()

x

x + ylim(0, max(PlantGrowth$weight))

x + ylim(0,10)+
  scale_y_continuous(limits = c(0,10))

x + ylim(0,10)+
  scale_y_continuous(breaks = NULL)

x + scale_y_continuous(breaks = NULL)+
  ylim(0,10)

x + scale_y_continuous(limits = c(0,10), 
                       breaks = NULL)

x + scale_y_continuous(limits = c(5, 6.5))

x + coord_cartesian(ylim = c(4, 6.5))

x + expand_limits(y = 0)

###################################
#8.3. Reversing a Continuous Axis
#8.3. Inversión de un eje continuo
###################################

PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()+
  scale_y_reverse()

# Similar effect by specifying limits in reversed order

PlantGrowth %>% 
  ggplot(aes(
    x = group, 
    y = weight
  ))+
  geom_boxplot()+
  ylim(6.5, 3.5)

PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()+
  scale_y_reverse(limits = c(7 , 3))

#############################################################
#8.4. Changing the Order of Items on a Categorical Axis
#8.4. Cambiar el orden de los elementos en un eje categórico
#############################################################

x <- PlantGrowth %>% 
  ggplot(aes(
  x = group,
  y = weight
))+
  geom_boxplot()

x + scale_x_discrete(limits = c("trt1", "ctrl", "trt2"))

x + scale_x_discrete(limits = c("ctrl", "trt1"))

x + scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

###############################################################
#8.5. Setting the Scaling Ratio of the X- and Y-Axes
#8.5. Configuración de la relación de escala de los ejes X e Y
###############################################################

view(marathon)
str(marathon)

x <- marathon %>% 
  ggplot(aes(
    x = Half,
    y = Full
  ))+
  geom_point()

x

x + coord_fixed()

x + coord_fixed()+
  scale_y_continuous(breaks = seq(0, 420, 30))+
  scale_x_continuous(breaks = seq(0, 420, 30))

x + coord_fixed(ratio = 1/2)

x + coord_fixed(ratio = 1/2)+
  scale_y_continuous(breaks = seq(0, 420, 30))+
  scale_x_continuous(breaks = seq(0, 420, 15))

##################################################################
#8.6. Setting the Positions of Tick Marks
#8.6. Configuración de las posiciones de las marcas de graduación
##################################################################

PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()


PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()+
  scale_y_continuous(breaks = c(4,4.25,4.5,5,6,8))

# La ubicación de las marcas de verificación define dónde se dibujan las principales 
# líneas de cuadrícula. Si el eje representa una variable continua, las líneas de 
# las cuadrículas menores, que son más tenues y sin etiqueta, entoncespor defecto 
# se dibujará a mitad de camino entre cada línea principal de la cuadrícula.



# Establecer rupturas y etiquetas para un eje discreto
PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()+
  scale_x_discrete(limits = c("trt2", "ctrl"),
                     breaks = "ctrl")

#########################################
#8.7. Removing Tick Marks and Labels
#8.7. Eliminación de marcas y etiquetas
#########################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_boxplot()

x

x + theme(axis.text.y = element_blank()) 

x + theme(axis.ticks = element_blank())

x + theme(axis.ticks = element_blank(),
          axis.text.y = element_blank())

x + scale_y_continuous(breaks = NULL)

######################################################
#8.8. Changing the Text of Tick Labels
#8.8. Cambiar el texto de las etiquetas de las marcas
######################################################

view(heightweight)
str(heightweight)

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()

x

x + scale_y_continuous(breaks = c(50,56,60,66,72),
                       labels = c("Tiny", "Really\nshort", "Short", "Medium", "Tallish"))

# En lugar de establecer etiquetas completamente arbitrarias, es más común tener
# sus datos almacenados en un formato, mientras que desea que las etiquetas se 
# muestren en otro. Podríamos, por ej. Por ejemplo, desea que las alturas se 
# muestren en pies y pulgadas (como 5'6 ") en lugar de solo pulgadas. Para hacer
# esto, podemos definir una función formateadora, que toma un valor y devuelve el
# cadena correspondiente.

footinch_formatter <- function(x) {
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot, "'", inch, "\"", sep=""))
}

footinch_formatter(56:64)

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()

x + scale_y_continuous(breaks = seq(48,72,4),
                       labels = footinch_formatter)

timeHMS_formatter <- function(x){
  h <- floor(x/60)
  m <- floor(x %% 60)
  s <- round(60*(x %% 1)) # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- gsub("^00:", "", lab) # Remove leading 00: if present
  lab <- gsub("^0", "", lab) # Remove leading 0 if present
  return(lab)
}

timeHMS_formatter(c(.33, 50, 51.25, 59.32, 60, 60.1, 130.23))

x + scale_alpha_continuous(breaks = seq(48,72,4),
                           labels = timeHMS_formatter)


# • comma() agrega comas a los números, en los lugares de mil, millones, billones, etc.
# • dollar() agrega un signo de dólar y redondea al centavo más cercano.
# • percent() multiplica por 100, redondea al entero más cercano y agrega un signo de porcentaje.
# • Scientific() da números en notación científica, como 3.30e+05, para números grandes y
# números pequeños
# Si desea utilizar estas funciones, primero debe cargar el paquete scales, con li
# brary (escalas).

###########################################################
#8.9. Changing the Appearance of Tick Labels
#8.9. Cambiar la apariencia de las etiquetas de las marcas
###########################################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group, 
    y = weight
  ))+
  geom_boxplot()+
  scale_x_discrete(breaks= c("ctrl", "trt1", "trt2"),
                   labels = c("Control", "Treatment 1", "Treatment 2"))
x

x + theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = .5))

x + theme(axis.text.x = element_text(angle=30,
                                     hjust = 1,
                                     vjust = 1))

x + theme(axis.text.x = element_text(family = "Times", 
                                     face = "italic", 
                                     colour = "darkred", 
                                     size = rel(0.9)))

####################################################
#8.10. Changing the Text of Axis Labels
#8.10. Cambiar el texto de las etiquetas de los ejes
####################################################

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear, 
    y = heightIn, 
    colour = sex
  ))+
  geom_point()
# With default axis labels
x

# Set the axis labels
x + xlab("Age in years")+
  ylab("Height in inches")+
  theme(legend.position = "NONE")

x + labs(x = "Age in years", 
         y = "Height in inches")

x + scale_x_continuous(name="Age in years")

x + scale_x_continuous(name = "Age\n(years)")

x + scale_x_continuous(name = "Age\n(years)")+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())


#######################################
#8.11. Removing Axis Labels
#8.11. Eliminación de etiquetas de eje
#######################################

x + theme(axis.text.x = element_blank())

x + xlab("")

#########################################################
#8.12. Changing the Appearance of Axis Labels
#8.12. Cambiar la apariencia de las etiquetas de los ejes
#########################################################

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()

x

x + theme(axis.text.x = element_text(face = "italic",
                                     colour = "red",
                                     size = 14))+
  theme(axis.title.x = element_text(face = "italic",
                                    colour = "red",
                                    size = 14))

x + ylab("Height\n(inches)")+
  theme(axis.title.y = element_text(angle = 0,
                                    face = "italic",
                                    size = 14))

x + ylab("Height\n(inches)")+
  theme(axis.title.y = element_text(angle = 90,
                                    face = "italic",
                                    size = 14, 
                                    colour = "green"))

############################################
#8.13. Showing Lines Along the Axes
#8.13. Mostrar líneas a lo largo de los ejes
############################################

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()

x + theme(axis.line = element_line(colour = "red"))

x + theme_bw()

x + theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "tomato"))

x + theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "green", 
                                 size = 4))

x + theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black", 
                                 size = 4))

x + theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black", 
                                 size = 4,
                                 lineend = "square"))

#################################        
#8.14. Using a Logarithmic Axis
#8.14. Usando un eje logarítmico
#################################

#install.packages("MASS")
library(MASS)

view(Animals)
str(Animals)

x <- Animals %>% 
  ggplot(aes(
    x = body,
    y = brain,
    label = rownames(Animals)
  ))+
  geom_text(size = 2)
x

x + scale_x_log10()+
  scale_y_log10()

# Con un eje logarítmico, una distancia visual dada representa un cambio proporcional
# constante; por ejemplo, cada centímetro en el eje y podría representar una 
# multiplicación de la cantidad por 10. En contraste, con un eje lineal, una 
# distancia visual dada representa una cantidad constante cambio; cada centímetro 
# podría representar agregar 10 a la cantidad

a = 10^(0:3)
b = 10^(-1:5)

x + scale_x_log10(breaks = b)+
  scale_y_log10(breaks = a)


#install.packages("scales")
library(scales)

x + scale_x_log10(breaks = b,
                  labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = a,
                labels = trans_format("log10", math_format(10^.x)))


Animals %>% 
  ggplot(aes(
    x = log10(body),
    y = log10(brain),
    label = rownames(Animals)
  ))+
  geom_text(size = 3)


x + scale_x_continuous(trans = log_trans(),
                       breaks = trans_breaks("log", function(x) exp(x)),
                       labels = trans_format("log", math_format(e^.x)))+
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))



view(aapl)
str(aapl)

x <- aapl %>% 
  ggplot(aes(
    x = date,
    y = adj_price
  ))+
  geom_line()

x

x + scale_y_log10(breaks = c(2,10,50,250))

# Es posible usar un eje logarítmico para un solo eje. Suele ser útil para 
# representar datos financieros de esta manera, porque representa mejor el cambio
# proporcional.

#################################################
#8.15. Adding Ticks for a Logarithmic Axis
#8.15. Adición de marcas para un eje logarítmico
#################################################

Animals %>% 
  ggplot(aes(
    x = body, 
    y = brain,
    label = rownames(Animals)
  ))+
  geom_text(size = 3)

Animals %>% 
  ggplot(aes(
    x = body, 
    y = brain,
    label = rownames(Animals)
  ))+
  geom_text(size = 3)+
  annotation_logticks()

Animals %>% 
  ggplot(aes(
    x = body, 
    y = brain,
    label = rownames(Animals)
  ))+
  geom_text(size = 3)+
  annotation_logticks()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

# Las marcas de verificación creadas por annotation_logticks() son en realidad 
# geoms dentro de la trama.área de tintura. Hay una marca larga en cada potencia
# de 10 y una marca mediana en cada 5 Para que los colores de las marcas de 
# verificación y las líneas de la cuadrícula coincidan un poco mejor, puede
# usar tema_bw()

Animals %>% 
  ggplot(aes(
    x = body, 
    y = brain,
    label = rownames(Animals)
  ))+
  geom_text(size = 3)+
  annotation_logticks()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -2.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -1.3)

#################################
#8.16. Making a Circular Graph
#8.16. Hacer un grafico circular
#################################

view(wind)
str(wind)

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram()
  

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5)

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5)+
  coord_polar()

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5)+
  coord_polar()+
  scale_x_continuous(limits = c(0,360))

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5)+
  coord_polar()+
  scale_x_continuous(limits = c(0,360))+
  theme_minimal()



wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5,
                 colour = "black", 
                 size = .25)

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5,
                 colour = "black", 
                 size = .25)+
  guides(fill = guide_legend(reverse = TRUE))

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5,
                 colour = "black", 
                 size = .25)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_polar()


wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5,
                 colour = "black", 
                 size = .25)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_polar()+
  scale_x_continuous(limits = c(0,360),
                     breaks = seq(0,360, by=45),
                     minor_breaks = seq(0,360, by=15))

wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5,
                 colour = "black", 
                 size = .25)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_polar()+
  scale_x_continuous(limits = c(0,360),
                     breaks = seq(0,360, by=45),
                     minor_breaks = seq(0,360, by=15))+
  scale_fill_brewer()


wind %>% 
  ggplot(aes(
    x = DirCat,
    fill = SpeedCat
  ))+
  geom_histogram(binwidth = 15,
                 origin = -7.5,
                 colour = "black", 
                 size = .25)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_polar(start = -45 * pi /180)


view(deaths)
str(deaths)

x <- data.frame(deaths = as.numeric(mdeaths),
                month = as.numeric(cycle(mdeaths)))

x

y <- ddply(x, "month", summarise, deaths = mean(deaths))

y

head(y, 3)


y %>% 
  ggplot(aes(
    x = month,
    y = deaths
  ))+
  geom_line()

y %>% 
  ggplot(aes(
    x = month,
    y = deaths
  ))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)

y %>% 
  ggplot(aes(
    x = month,
    y = deaths
  ))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  coord_polar()


y %>% 
  ggplot(aes(
    x = month,
    y = deaths
  ))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  coord_polar()+
  ylim(0, max(y$deaths))

y %>% 
  ggplot(aes(
    x = month,
    y = deaths
  ))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  coord_polar()+
  ylim(0, max(y$deaths))+
  xlim(0,12)

mdx <- y[y$month==12,]
mdx$month <- 0
mdx

mdnew <- rbind(mdx, y)
mdnew

circle <- y %>% 
  ggplot(aes(
    x = month,
    y = deaths
  ))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)

circle %+% +
  mdnew+
  coord_polar()+
  ylim(0, max(y$deaths))

# Observe el uso del operador %+%. Cuando agrega un marco de datos a un
# objeto ggplot con %+%, reemplaza el marco de datos predeterminado en ggplot
# objeto.

###############################
#8.17. Using Dates on an Axis
#8.17. Uso de fechas en un eje
###############################

view(economics)
str(economics)
head(economics, 3)


economics %>% 
  ggplot(aes(
    x = date,
    y = psavert
  ))+
  geom_line()

# Take a subset of economics
econ <- subset(economics, date >= as.Date("1992-05-01") &
                 date < as.Date("1993-06-01"))

# Base plot - without specifying breaks
p <- ggplot(econ, aes(x=date, y=psavert)) + geom_line()

p

datebreaks <- seq(as.Date("1992-06-01"),
                  as.Date("1993-06-01"), 
                  by = "2 month")

p + scale_x_date(breaks = datebreaks)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


p + scale_x_date(breaks = datebreaks,
                 labels = date_format("%Y %b"))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# %Y Year with century (2012)
# %y Year without century (12)
# %m Month as a decimal number (08)
# %b Abbreviated month name in current locale (Aug)
# %B Full month name in current locale (August)
# %d Day of month as a decimal number (04)
# %U Week of the year as a decimal number, with Sunday as the first day of the week (00–53)
# %W Week of the year as a decimal number, with Monday as the first day of the week (00–53)
# %w Day of week (0–6, Sunday is 0)
# %a Abbreviated weekday name (Thu)
# %A Full weekday name (Thursday)

Sys.setlocale("LC_TIME", "italian")

# Consulte ?Sys.setlocale para obtener más información sobre cómo configurar la 
# configuración regional. Consulte ?strptime para obtener información sobre cómo 
# convertir cadenas en fechas y para obtener información sobre cómo formatear la
# salida de fecha.

view(WWWusage)
str(WWWusage)

www <- data.frame(minute = as.numeric(time(WWWusage)),
                  users = as.numeric(WWWusage))
www

timeHM_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  lab <- sprintf("%d:%02d", h, m) # Format the strings as HH:MM
  return(lab)
}

# Default x axis
www %>% 
  ggplot(aes(
    x = minute,
    y = users
  ))+
  geom_line()

#with formatted times
www %>% 
  ggplot(aes(
    x = minute, 
    y = users
  ))+ 
  geom_line()+
  scale_x_continuous(name = "time",
                     breaks = seq(0,100, by = 10),
                     labels = timeHM_formatter)

www %>% 
  ggplot(aes(
    x = minute, 
    y = users
  ))+ 
  geom_line()+
  scale_x_continuous(breaks=c(0, 20, 40, 60, 80, 100),
                     labels=c("0:00", "0:20", "0:40", "1:00", "1:20", "1:40"))


timeHM_formatter(c(0, 50, 51, 59, 60, 130, 604))

timeHMS_formatter <- function(x) {
  h <- floor(x/3600)
  m <- floor((x/60) %% 60)
  s <- round(x %% 60) # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- sub("^00:", "", lab) # Remove leading 00: if present
  lab <- sub("^0", "", lab) # Remove leading 0 if present
  return(lab)
}

timeHMS_formatter(c(20, 3000, 3075, 3559.2, 3600, 3606, 7813.8))
