library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Los diagramas de dispersión se utilizan para mostrar la relación entre dos variables continuas.
# En un diagrama de dispersión, cada observación en un conjunto de datos está representada por
# un punto. A menudo, una dispersión la gráfica también tendrá una línea que muestra los valores
# pronosticados basados en algún modelo estadístico.
# Esto es fácil de hacer con R y ggplot2, y puede ayudar a dar sentido a los datos cuando el
# las tendencias no son inmediatamente obvias con solo mirarlas.
# Con grandes conjuntos de datos, puede ser problemático graficar cada observación individual
# porque los puntos se superpondrán, oscureciéndose unos a otros. Cuando esto suceda, probablemente
# desea resumir los datos antes de mostrarlos. 

#############################################
#5.1. Making a Basic Scatter Plot
#5.1. Hacer un diagrama de dispersión básico
#############################################

view(heightweight)
x <- heightweight[, c("ageYear", "heightIn")]
str(x)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point(shape = 8) #default 16

##########################################################################
#5.2. Grouping Data Points by a Variable Using Shape or Color
#5.2. Agrupación de puntos de datos por una variable usando Forma o Color
##########################################################################

x <- heightweight[, c("sex", "ageYear", "heightIn")]
head(x)
str(x)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex
  ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    shape = sex
  ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex,
    shape = sex
  ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    shape = sex,
    colour = sex
  ))+
  geom_point()+
  scale_shape_manual(values = c(1,2))+
  scale_color_brewer(palette = "Set2")

###########################################
#5.3. Using Different Point Shapes
#5.3. uso de diferentes formas de puntos
##########################################

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point(shape = 3)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn, 
    shape = sex
  ))+
  geom_point(size = 3) +
  scale_shape_manual(values = c(1,11))

# Es posible hacer que la forma represente una variable y el relleno (vacío o sólido) 
# represente reenviar otra variable. Esto se hace un poco indirectamente, eligiendo 
# formas que tienen tanto color y relleno, y una paleta de colores que incluye NA y
# otro color (NA resultar en una forma hueca). Por ejemplo, tomaremos el conjunto de 
# datos de altura y peso y agregaremos otra columna que indica si el niño pesó 100 
# libras o más

#hacer una copia de la data
x <- heightweight
#categorizar en grupos de <100 y >=100
x$weightGroup <- cut(x$weightLb, 
                     breaks = c(-Inf, 100, Inf),
                     labels = c("< 100", ">= 100"))
str(x)
view(x)
#Use formas con relleno y color, y use colores vacíos (NA) y rellenos
x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn, 
    shape = sex,
    fill = weightGroup
  ))+
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(21,24))+
  scale_fill_manual(values = c(NA, "red"), 
                    guide = guide_legend(override.aes = list(shape = 21)))

#############################################################
#5.4. Mapping a Continuous Variable to Color or Size
#5.4. Asignación de una variable continua a color o tamaño
###########################################################

x <- heightweight[, c("sex", "ageYear", "heightIn", "weightLb")]

head(x)
str(x)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = weightLb
  ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = weightLb
  ))+
  geom_point(size = 2.5)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = weightLb,
    shape = sex
  ))+
  geom_point(size = 2.5)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    size = weightLb
  ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    size = weightLb
  ))+
  geom_point(aes(shape = sex))

x %>% 
  ggplot(aes(
    x = weightLb,
    y = heightIn, 
    fill = ageYear
  ))+
  geom_point(shape = 21, size = 2.5)

x %>% 
  ggplot(aes(
    x = weightLb,
    y = heightIn, 
    fill = ageYear
  ))+
  geom_point(shape = 21, size = 2.5)+
  scale_fill_gradient(low = "black", high = "white")

#El uso de guide_legend() dará como resultado una leyenda discreta en lugar
#de una barra de colores

x %>% 
  ggplot(aes(
    x = weightLb, 
    y = heightIn,
    fill = ageYear
  ))+
  geom_point(shape = 21, size = 2.5)+
  scale_fill_gradient(low = "black", 
                      high ="white",
                      breaks = 12:14,
                      guide = guide_legend())

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    size = weightLb,
    colour = sex
  ))+
  geom_point(alpha = .5)

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    size = weightLb,
    colour = sex
  ))+
  geom_point(alpha = .5)+
  scale_size_area() # hace el area proporcional al valor numerico

x %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    size = weightLb,
    colour = sex
  ))+
  geom_point(alpha = .5)+
  scale_size_area()+
  scale_color_brewer(palette = "Set1")

####################################
#5.5. Dealing with Overplotting
#5.5. lidiando con la superposición
####################################


view(diamonds)
x <- diamonds %>% 
  ggplot(aes(
    x = carat,
    y = price
  ))

x + geom_point()
x + geom_point(alpha = 0.1)
x + geom_point(alpha = .01)


# Por defecto, stat_bin_2d() divide el espacio en 30 grupos en las direcciones x e y,
# para un total de 900 contenedores. En la segunda versión, aumentamos el número de 
# contenedores con contenedores = 50.

x + stat_bin2d()
x + stat_bin_2d(bins = 50)

x + stat_bin_2d(bins = 50)+
  scale_fill_gradient(low = "lightblue",
                      high ="red",
                      limits = c(0, 6000))


#Otra alternativa es agrupar los datos en hexágonos en lugar de rectángulos, con stat_bin hex()

#install.packages("hexbin")
library(hexbin)
x + stat_binhex()

x + stat_binhex()+
  scale_fill_gradient(low = "lightblue",
                      high = "red", 
                      limits = c(0,8000))

x + stat_binhex()+
  scale_fill_gradient(low = "#feb24c", 
                      high = "red",
                      breaks = c(2,250,500,1000,2000,4000,6000),
                      limits = c(0,6000))


view(ChickWeight)
str(ChickWeight)

x <- ChickWeight %>% 
  ggplot(aes(
    x = Time,
    y = weight
  ))

x + geom_point()
x + geom_point(position = "jitter")

#También podría usar geom_jitter(), que es equivalente
x + geom_point(position = position_jitter(width = .5, 
                                          height = 0))

#########################################################
#5.6. Adding Fitted Regression Model Lines
#5.6. Adición de líneas de modelo de regresión ajustadas
#########################################################

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))

x + geom_point()

x + geom_point()+
  stat_smooth(method = lm)

# De forma predeterminada, stat_smooth() también agrega una región de confianza 
# del 95 % para el ajuste de regresión. los el intervalo de confianza se puede 
# cambiar configurando el nivel, o se puede deshabilitar con se=FALSE

# región de confianza del 99%
x + geom_point()+
  stat_smooth(method = lm, 
              level = 0.99)

#Sin región de confianza
x + geom_point()+
  stat_smooth(method = lm,
              se = FALSE)

# La línea de regresión lineal no es la única forma de ajustar un modelo a los 
# datos; de hecho, es ni siquiera el predeterminado. Si agrega stat_smooth() 
# sin especificar el método, utilice una curva de loess (polinomio ponderado 
# localmente).Ambos tendrá el mismo resultado:

x + geom_point(colour = "grey60")+
  stat_smooth()

x + geom_point(colour = "grey40")+
  stat_smooth(method = loess)


#install.packages("MASS")
library(MASS)
View(biopsy)
str(biopsy)

x <- biopsy

x$classn[x$class == "benign"] <- 0
x$classn[x$class == "malignant"] <- 1

str(x)
head(x, 3)
tail(x, 3)

ggplot(data = x, 
       aes(
         x = V1,
         y = classn
       ))+
  geom_point()

x %>% 
  ggplot(aes(
    x = V1,
    y = classn
  ))+
  geom_point(position = position_jitter(width = 0.3,
                                        height = 0.06),
             alpha = 0.4,
             shape = 21,
             size = 1.5)+
  stat_smooth(method = lm, 
              family = binomial)

x %>% 
  ggplot(aes(
    x = V1,
    y = classn
  ))+
  geom_point(position = position_jitter(width = 0.3,
                                        height = 0.06),
             alpha = 0.4,
             shape = 21,
             size = 1.5)+
  stat_smooth(method = glm, 
              family = binomial)


x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex
  ))+
  geom_point()+
  scale_color_brewer(palette = "Set1")

x + geom_smooth()

x + geom_smooth(method = lm,
                se = FALSE,
                fullrange = TRUE)

##########################################################
#5.7. Adding Fitted Lines from an Existing Mode
#5.7. Adición de líneas ajustadas desde un modo existente
##########################################################

#construiremos un modelo cuadrático usando lm() con ageYear como predictor
#de heightIn.

x <- lm(heightIn ~ ageYear + I(ageYear ^2), data = heightweight)

#Cree un marco de datos con la columna ageYear, interpolando a través del rango
xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)
predicted <- data.frame(ageYear = seq(xmin, xmax, length.out = 100))

#Calcular los valores predichos de heightIn
predicted$heightIn <- predict(x, predicted)
predicted

heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point(colour = "grey50")+
  geom_line(data = predicted, size = 2)


# Se puede usar cualquier objeto modelo, siempre que tenga un método predict() 
# correspondiente. Por ejemplo, lm tiene predict.lm(), loess tiene predict.loess(),
# y así sucesivamente. Agregar líneas de un modelo se puede simplificar usando la
# función predictvals(), definido a continuación. Si simplemente pasa un modelo,
# hará el trabajo de encontrar la variable nombres y rango del predictor, 
# y devolverá un marco de datos con predictor y pre‐valores dictados. Ese marco de
# datos se puede pasar a geom_line() para dibujar el ajuste línea, como hicimos antes


predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  # If xrange isn't passed in, determine xrange from the models.
  # Different ways of extracting the x range, depending on model type
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm")))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange <- range(model$x)
  }
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}

modlinear <- lm(heightIn ~ ageYear, heightweight)
modloess <- loess(heightIn ~ ageYear, heightweight)

lm_predicted <- predictvals(modlinear, "ageYear", "heightIn")
loess_predicted <- predictvals(modloess, "ageYear", "heightIn")

heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point(colour = "grey50")+
  geom_line(data = lm_predicted, 
            colour = "red", 
            size = 0.8)+
  geom_line(data = loess_predicted, 
            colour = "blue", 
            size = 1)


x <- biopsy
x$classn[x$class == "beningn"] <- 0
x$classn[x$class == "malignant"] <- 1

fitlogistic <- glm(classn ~ V1, 
                   data = x,
                   family = binomial)

glm_predicted <- predictvals(fitlogistic, "V1", "classn", type="response")

x %>% 
  ggplot(aes(
    x = V1,
    y = classn
  ))+
  geom_point(position = position_jitter(width = .3,
                                        height = .08),
             alpha = 0.4,
             shape = 21,
             size = 1.5)+
  geom_line(data = glm_predicted, 
            colour = "#fc9272", 
            size = 1)

#####################################################################
#5.8. Adding Fitted Lines from Multiple Existing Models
#5.8. Adición de líneas ajustadas desde múltiples modelos existentes
#####################################################################

#Use la función predictvals() de la receta anterior junto con dlply() y
#ldply() del paquete plyr.

# Con el conjunto de datos de altura y peso, crearemos un modelo lineal con lm() 
# para cada uno de los niveles de sexo, y poner esos objetos modelo en una lista.
# La construcción del modelo se realiza con un función, make_model(), definida 
# aquí. Si le pasa un marco de datos, simplemente devuelve un objeto de película. 
# El modelo se puede personalizar para sus datos:

make_model <- function(data){
  lm(heightIn ~ ageYear, data)
}

models <- dlply(heightweight, "sex", .fun = make_model)

#Imprima la lista de dos objetos lm, f y m
models


#Ahora que tenemos la lista de objetos modelo, podemos ejecutar predictvals() 
#para predecir valores de cada modelo, usando la función ldply()

predvals <- ldply(models, .fun = predictvals, xvar = "ageYear", yvar = "heightIn")
predvals

#Finalmente, podemos graficar los datos con los valores predichos
heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn, 
    colour = sex
  ))+
  geom_point()+
  geom_line(data = predvals)

# Las llamadas dlply() y ldply() se utilizan para dividir los datos en partes, 
# ejecutando funciones ciones en esas partes, y luego volver a montar la salida.
# Con el código anterior, el rango x de los valores pronosticados para cada grupo
# abarca el x rango de cada grupo, y no más allá; para los machos, la línea de 
# predicción se detiene en el mayor macho, mientras que para las hembras, la línea
# de predicción continúa más a la derecha, hasta la hembra de mayor edad. Para 
# formar líneas de predicción que tengan el mismo rango x en todos los grupos, 
# simplemente podemos pase en xrange, así:

predvals <- ldply(models, 
                  .fun = predictvals, 
                  xvar = "ageYear",
                  yvar = "heightIn",
                  xrange = range(heightweight$ageYear))

heightweight %>% 
  ggplot(aes(
    x =  ageYear,
    y =  heightIn,
    colour = sex
  ))+
  geom_point(size = 1.2)+
  geom_line(data = predvals)

############################################################
#5.9. Adding Annotations with Model Coefficients
#5.9. Adición de anotaciones con coeficientes de modelo
###########################################################

model <- lm(heightIn ~ ageYear, data = heightweight)
summary(model)

#Esto muestra que el valor de r2 es 0.4249. Crearemos un gráfico y agregaremos 
#manualmente el texto usando annotate()


# Primero generar datos de predicción
pred <- predictvals(model, "ageYear", "heightIn")

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()+
  geom_line(data = pred)

x + annotate("text", 
             label = "r^2=0.4249", 
             x = 16.5,
             y=52)

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()+
  geom_line(data = pred)

x + annotate("text", 
             label = "r^2==0.4249", 
             parse = TRUE,
             x = 16.5,
             y=52)


eqn <- as.character(as.expression(
  substitute(italic(y) == a + b * italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
             list(a = format(coef(model)[1], digits=3),
                  b = format(coef(model)[2], digits=3),
                  r2 = format(summary(model)$r.squared, digits=2)
             ))))
eqn
parse(text = eqn)

x + annotate("text", 
             label = eqn,
             parse=TRUE,
             x = Inf,
             y = -Inf,
             hjust = 1.1,
             vjust = -.5)

###################################################################
#5.10. Adding Marginal Rugs to a Scatter Plot
#5.10. Adición de alfombras marginales a un diagrama de dispersión
###################################################################

View(faithful)
str(faithful)

faithful %>% 
  ggplot(aes(
    x = eruptions,
    y = waiting
  ))+
  geom_point()+
  geom_rug()

# Un diagrama de alfombra marginal es esencialmente un diagrama de dispersión 
# unidimensional que se puede usar para visualizar la distribución de datos
# en cada eje.
faithful %>% 
  ggplot(aes(
    x = eruptions,
    y = waiting
  ))+
  geom_point()+
  geom_rug(position = "jitter", size = 0.2)

########################################################
#5.11. Labeling Points in a Scatter Plot
#5.11. Etiquetado de puntos en un gráfico de dispersión
########################################################

view(countries)
str(countries)

x <-subset(countries, Year == 2009 & healthexp > 2000)
str(x)
View(x)

y <- x %>% 
  ggplot(aes(
    x = healthexp,
    y = infmortality
  ))+
  geom_point()

y + annotate("text", 
             x = 4350,
             y = 5.4,
             label = "Canada")+
  annotate("text", 
           x = 7400,
           y = 6.8, 
           label = "USA")

y + geom_text(aes(
  label = Name
),
size = 4)


y + geom_text(aes(
  label = Name
),
size = 4,
vjust = 0)

y + geom_text(aes(
  y = infmortality+.1,
  label =Name
),
size = 4,
vjust = 0)


y + geom_text(aes(
  label = Name
), 
size = 4,
vjust = 0)

y + geom_text(aes(
  x = healthexp+100,
  label = Name
),
size = 4,
hjust = 0)


# Si desea etiquetar solo algunos de los puntos pero quiere que la ubicación se 
# maneje automáticamente, puede agregar una nueva columna a su marco de datos que
# contenga solo las etiquetas que desea. Esta es una forma de hacerlo: primero 
# haremos una copia de los datos que estamos usando, luego duplique la columna 
# Nombre en xxx:

x <- subset(countries, Year == 2009 & healthexp > 2000)
x$myName <- x$Name
str(x)

idx <- x$myName %in% c("Canada", "Ireland", "United Kingdom", "United States",
                       "New Zealand", "Iceland", "Japan", "Luxembourg",
                       "Netherlands", "Switzerland") 
idx

x$myName[!idx] <-NA

x %>% 
  ggplot(aes(
   x = healthexp,
   y = infmortality
  ))+
  geom_point()+
  geom_text(aes(
    x = healthexp+100,
    label = myName
  ), size = 4,
  hjust = 0)+
  xlim(2000,10000)

########################################
#5.12. Creating a Balloon Plot
#5.12. Creación de un diagrama de globo
#######################################

x <- subset(countries, Year==2009 &
                      Name %in% c("Canada", "Ireland", "United Kingdom", "United States",
                                  "New Zealand", "Iceland", "Japan", "Luxembourg",
                                  "Netherlands", "Switzerland"))
str(x)
head(x, 7)

y <- x %>% 
  ggplot(aes(
    x = healthexp,
    y = infmortality,
    size = GDP
  ))+
  geom_point(shape = 21,
             colour = "black", 
             fill = "cornsilk")

y

y + scale_size_area(max_size = 15)



view(HairEyeColor)
hec <- HairEyeColor[,,"Male"]+HairEyeColor[,,"Female"]
view(hec)

install.packages("reshape2")
library(reshape2)
hec <- melt(hec, value.name = "count")

hec %>% 
  ggplot(aes(
    x=Eye,
    y=Hair
    ))+
  geom_point(aes(
    size=count
    ),
    shape=21,
    colour="black",
    fill="cornsilk"
    )+
  scale_size_area(max_size=20,
                  guide=FALSE) +
  geom_text(aes(
    y = as.numeric(Hair)-sqrt(count)/22,
    label = count),
    vjust=1,
    colour="grey60",
    size=4)

##################################################
#5.13. Making a Scatter Plot Matrix
#5.13. Hacer una matriz de diagrama de dispersión
##################################################


# Una matriz de diagramas de dispersión es una forma excelente de visualizar las
# relaciones por pares entre varias variables Para hacer uno, use la función 
# pairs() de los gráficos base de R. Para este ejemplo, usaremos un subconjunto 
# del conjunto de datos de países. Extraeremos los datos de el año 2009, y 
# mantenga solo las columnas que son relevantes:

x <- subset(countries, Year == 2009,
            select = c(Name, GDP, laborrate, healthexp, infmortality))

str(x)
pairs(x[, 2:5])

#No usamos ggplot2 aquí porque no hace matrices de diagramas de dispersión 
#(al menos, no bien).

# También puede utilizar funciones personalizadas para los paneles. Para mostrar
# el coeficiente de correlación de cada par de variables en lugar de un diagrama 
# de dispersión, definiremos la función panel.cor. Esto también mostrará 
# correlaciones más altas en una fuente más grande. No te preocupes por los 
# detalles por ahora

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "black", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}
pairs(x[,2:5], upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.smooth)

pairs(x[,2:5], pch=".",
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.lm)


#git push -u origin main
