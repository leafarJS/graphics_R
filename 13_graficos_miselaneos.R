library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Gráficos misceláneos
# Hay muchas, muchas formas de visualizar datos y, a veces, las cosas no encajan 
# en categorías agradables y ordenadas. Este capítulo muestra cómo hacer algunas 
# de estas otras visualizaciones.

########################################
# 13.1. Making a Correlation Matrix
# 13.1. Hacer una matriz de correlación
########################################

view(mtcars)
str(mtcars)
head(mtcars)

# Primero, genere la matriz de correlación numérica usando cor. Esto generará 
# coeficientes de correlación para cada par de columnas:

mtcor <- round(cor(mtcars), digits = 2)

view(mtcor)

#install.packages("corrplot")
library(corrplot)

corrplot(mtcor)

mtcor %>% 
  corrplot( 
         method = "shade",
         shade.col = NA,
         tl.col = "black",
         tl.srt = 45)

#También puede ser útil mostrar etiquetas que representen el coeficiente de 
# correlación en cada cuadrado de la matriz. En este ejemplo, crearemos una paleta
# más clara para que el texto sea legible y eliminaremos la leyenda de color, 
# ya que es redundante. También ordenaremos los elementos para que los elementos 
# correlacionados estén más juntos, usando la opción order="AOE" (orden angular 
# de vectores propios)


# Generate a lighter palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

mtcor %>% 
  corrplot(
    method = "shade",
    shade.col = NA,
    tl.col = "black",
    tl.srt = 45, 
    col = col(200),
    addCoef.col = "black",
    addcolorlabel = "no",
    order = "AOE"
  )

# Option                            | Description
# type={"lower"| "upper"}           | Only use the lower or upper triangle
# diag=FALSE                        | Don’t show values on the diagonal
# addshade="all"                    | Add lines indicating the direction of the correlation
# shade.col=NA                      | Hide correlation direction lines
# method="shade"                    | Use colored squares
# method="ellipse"                  | Use ellipses
# addCoef.col="color"               | Add correlation coefficients, in color
# tl.srt="number"                   | Specify the rotation angle for top labels
# tl.col="color"                    | Specify the label color
# order={"AOE" | "FPC" | "hclust"}  | Sort labels using angular order of eigenvectors, first principle component, or hierarchical clustering

############################
#13.2. Plotting a Function
#13.2. Trazar una función
###########################

# Usa stat_function(). También es necesario darle a ggplot() un marco de datos
# ficticio para que obtenga el rango x adecuado. En este ejemplo usaremos dnorm(),
# que da la densidad de la distribución normal

# The data frame is only used for setting the range
x <- ggplot(data.frame(x = c(-3,3)), aes(x = x))

x + stat_function(fun = dnorm)


# Algunas funciones toman argumentos adicionales. Por ejemplo, dt(), la función 
# para la densidad de la distribución t, toma un parámetro para grados de libertad.
# Estos argumentos adicionales se pueden pasar a la función colocándolos en una 
# lista y dando la lista a args:

x + stat_function(fun = dt, args = list(df = 2 ))

myfun <- function(param){
  1/(1 + exp(-param + 10))
}
ggplot(data.frame(x = c(0,20)),
                  aes(x = x))+
  stat_function(fun = myfun)


# De forma predeterminada, la función se calcula en 101 puntos a lo largo del 
# rango x. Si tiene una función que fluctúa rápidamente, es posible que pueda ver
# los segmentos individuales. Para suavizar la curva, pase un valor mayor de n a
# stat_function(), como en stat_function(fun=myfun, n = 200)
               

ggplot(data.frame(x = c(0,20)),
       aes(x = x))+
  stat_function(fun = myfun,
                n = 200)                                                                                                                                                                                                                                                                   

#############################################################
#13.3. Shading a Subregion Under a Function Curve
#13.3. Sombreado de una subregión bajo una curva de función
############################################################

# Return dnorm(x) for 0 < x < 2, and NA for all other x
dnorm_limit <- function(x){
   y <- dnorm(x)
   y[x < 0 | x > 2] <- NA
   return(y)
}

# ggplot() with dummy data
# ggplot() con datos ficticios
x <- ggplot(data.frame(x = c(-3,3)),
            aes(x = x))

x +  stat_function(fun = dnorm_limit)


x +  stat_function(fun = dnorm_limit,
                   geom = "area")

# nota: Recuerda que lo que se pasa a esta función es un vector, no valores 
# individuales. Si esta función operara en elementos individuales a la vez, 
# podría tener sentido usar una instrucción if/else para decidir qué devolver,
# condicional al valor de x. Pero eso no funcionará aquí, ya que x es un vector
# con muchos valores.


x +  stat_function(fun = dnorm_limit,
                   geom = "area",
                   fill = "red")

x +  stat_function(fun = dnorm_limit,
                   geom = "area",
                   fill = "red",
                   alpha = 0.2)

x +  stat_function(fun = dnorm_limit,
                   geom = "area",
                   fill = "red",
                   alpha = 0.2)+
  stat_function(fun = dnorm)


# R tiene funciones de primera clase y podemos escribir una función que devuelva
# un cierre, es decir, podemos programar una función para programar otra función.
# Esta función le permitirá pasar una función, un valor mínimo y un valor máximo.
# Los valores fuera del rango se devolverán nuevamente con NA:

limitRange <- function(fun, min, max){
  function(x){
    y = fun(x)
    y[x < min | x > max] <- NA
    return(y)
  }
}

#this returns a function
dlimit <- limitRange(dnorm, 0, 2)

dlimit(-2,4)

x + stat_function(fun = dnorm) +
  stat_function(fun = limitRange(dnorm, 0, 2),
                geom="area", fill="yellow", alpha=0.2)

# La función limitRange() se puede usar con cualquier función, no solo con dnorm(),
# para crear una versión de rango limitado de esa función. El resultado de todo esto 
# es que en lugar de tener que escribir funciones con diferentes valores codificados
# para cada situación que se presente, podemos escribir una función y simplemente 
# pasarle diferentes argumentos según la situación.

################################
#13.4. Creating a Network Graph
#13.4. Crear un grafico de red
################################

#install.packages("igraph")
library(igraph)

# Specify edges for a directed graph
# Especificar bordes para un gráfico dirigido

gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)

str(gd)

#for an undirection graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed=FALSE)
#No labels
plot(gu, vertex.label = NA)

str(gu)




view(madmen2)
str(madmen2)
head(madmen2)

# Create a graph object from the data set
g <- graph.data.frame(madmen2, directed = TRUE)

# Remove unnecessary margins
par(mar=c(0,0,0,0))

g %>% 
  plot(layout = layout.fruchterman.reingold,
       vertex.size = 8,
       edge.arrow.size = 0.5,
       vertex.label = NA)



view(madmen)
str(madmen)
head(madmen,3)

g <- graph.data.frame(madmen, directed = TRUE)
par(mar = c(0,0,0,0)) # remove unnecessary margins

g %>% 
  plot(layout = layout.circle,
       vertex.size = 8,
       vertex.label = NA)

######################################################
#13.5. Using Text Labels in a Network Graph
#13.5. Uso de etiquetas de texto en un gráfico de red
#####################################################

## Copy madmen and drop every other row
x <- madmen[1:nrow(madmen) %% 2 == 1, ]

x

y <- graph.data.frame(x, directed = FALSE)

y

# Print out the names of each vertex
V(y)$name

y %>% 
  plot(
    layout = layout.fruchterman.reingold,
    vertex.size = 4,           #smaller nodel
    vertex.label = V(y)$name,  #set the labels
    vertex.label.cex = 0.8,    #slightly smaller font
    vertex.label.dist = 0.4,   #offset the labels 
    vertex.label.color = "grey"
  )


# Otra forma de lograr el mismo efecto es modificar el objeto de trazado, en lugar
# de pasar los valores como argumentos a plot(). Para hacer esto, use V()$xxx <- 
# en lugar de pasar un valor a un argumento vertex.xxx. Por ejemplo, esto resultará
# en la misma salida que el código anterior:

# This is equivalent to the preceding code
V(y)$size <- 8
V(y)$label <- V(y)$name
V(y)$label.cex <- 0.8
V(y)$label.dist <- 0.4
V(y)$label.color <- "black"

#set a property of the entire graph
y$layout <- layout.fruchterman.reingold

y %>% 
  plot()


# Las propiedades de los bordes también se pueden establecer, ya sea con la 
# función E() o pasando valores a los argumentos edge.xxx

#view the edges
E(y)

#set some of the lables to "M"
E(y)[c(2,11,19)]$label <- "M"

#set color of all to grey, and then color a few red
E(y)$color <- "grey70"
E(y)[c(2,11,19)]$color <- "red"

plot(y)


#################################
#13.6. Creating a Heat Map
#13.6. Creación de mapa de color 
#################################

# Utilice geom_tile() o geom_raster() y asigne una variable continua para rellenar.
# Usaremos el conjunto de datos de presidentes, que es un objeto de serie temporal
# en lugar de un marco de datos:

view(presidents)
class(presidents)
str(presidents)

pres_rating <- data.frame(
  rating = as.numeric(presidents),
  year = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents))
)
pres_rating

#base plot

x <- pres_rating %>% 
  ggplot(aes(
    x = year,
    y = quarter,
    fill = rating
  ))
x

#using geom_tile()

x + geom_tile()

#using geom_raster() -looks the some, but a little mor efficient

x + geom_raster()


#Para transmitir mejor la información útil, es posible que desee personalizar 
# la apariencia del mapa de calor. Con este ejemplo, invertiremos el eje y para 
# que progrese de arriba hacia abajo, y agregaremos marcas cada cuatro años a lo
# largo del eje x, para corresponder con cada período presidencial. También cambiaremos
# la escala de color usando scale_fill_gradient2(), que le permite especificar un 
# color de punto medio y los dos colores en el punto bajo y alto termina.

x + geom_tile()+
  scale_x_continuous(breaks = seq(1940,1976, by = 4))+
  scale_y_reverse()+
  scale_fill_gradient2(midpoint = 50,
                       mid = "grey70",
                       limits = c(0,100))

############################################################
#13.7. Creating a Three-Dimensional Scatter Plot
#13.7. Creación de un diagrama de dispersión tridimensional
############################################################

#install.packages("rgl")
library(rgl)

plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, 
       type = "s", 
       size = 0.75, 
       lit = FALSE)

#De forma predeterminada, plot3d() usa puntos cuadrados, que no aparecen 
# correctamente al guardar en un PDF. Para mejorar la apariencia, usamos type="s"
# para los puntos esféricos, los hicimos más pequeños con size=0.75 y apagamos la
# iluminación 3D con lit=FALSE (de lo contrario, se ven como esferas brillantes).

# Function to interleave the elements of two vectors
interleave <- function(v1, v2) as.vector(rbind(v1,v2))
# Plot the points
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab="Weight", ylab="Displacement", zlab="MPG",
       size=.75, type="s", lit=FALSE)
# Add the segments
segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha=0.4, col="blue")


# Make plot without axis ticks or labels
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,
       size=.75, type="s", lit=FALSE)
segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha = 0.4, col = "blue")
# Draw the box.
rgl.bbox(color="grey50", # grey60 surface and black text
         emission="grey50", # emission color is grey50
         xlen=0, ylen=0, zlen=0) # Don't add tick marks
# Set default color of future objects to black
rgl.material(color="black")+
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges=c("x--", "y+-", "z--"),
       ntick=6, # Attempt 6 tick marks on each side
       cex=.75) # Smaller font
# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Weight", edge="x--", line=2)
mtext3d("Displacement", edge="y+-", line=3)
mtext3d("MPG", edge="z--", line=3)

########################################################################
#13.8. Adding a Prediction Surface to a Three-Dimensional Plot
#13.8. Adición de una superficie de predicción a un Gráfico dimensional
########################################################################

# Given a model, predict zvar from xvar and yvar
# Defaults to range of x and y variables, and a 16x16 grid
predictgrid <- function(model, xvar, yvar, zvar, res = 16, type = NULL) {
  # Find the range of the predictor variable. This works for lm and glm
  # and some others, but may require customization for others.
  xrange <- range(model$model[[xvar]])
  yrange <- range(model$model[[yvar]])
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res))
  names(newdata) <- c(xvar, yvar)
  newdata[[zvar]] <- predict(model, newdata = newdata, type = type)
  newdata
}

# Convert long-style data frame with x, y, and z vars into a list
# with x and y as row/column values, and z as a matrix.
df2mat <- function(p, xvar = NULL, yvar = NULL, zvar = NULL) {
  if (is.null(xvar)) xvar <- names(p)[1]
  if (is.null(yvar)) yvar <- names(p)[2]
  if (is.null(zvar)) zvar <- names(p)[3]
  x <- unique(p[[xvar]])
  y <- unique(p[[yvar]])
  z <- matrix(p[[zvar]], nrow = length(y), ncol = length(x))
  m <- list(x, y, z)
  names(m) <- c(xvar, yvar, zvar)
  m
}

# Make a copy of the data set
m <- mtcars
# Generate a linear model
mod <- lm(mpg ~ wt + disp + wt:disp, data = m)
# Get predicted values of mpg from wt and disp
m$pred_mpg <- predict(mod)
# Get predicted mpg from a grid of wt and disp
mpgrid_df <- predictgrid(mod, "wt", "disp", "mpg")
mpgrid_list <- df2mat(mpgrid_df)
# Make the plot with the data points
plot3d(m$wt, m$disp, m$mpg, type="s", size=0.5, lit=FALSE)+
# Add the corresponding predicted points (smaller)
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type="s", size=0.5, lit=FALSE)+
# Add line segments showing the error
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha=0.4, col="red")+
# Add the mesh of predicted values
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha=0.4, front="lines", back="lines")

plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,
       size=.5, type="s", lit=FALSE)
# Add the corresponding predicted points (smaller)
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type="s", size=0.5, lit=FALSE)+
# Add line segments showing the error
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha=0.4, col="red")+
# Add the mesh of predicted values
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha=0.4, front="lines", back="lines")
# Draw the box
rgl.bbox(color="grey50", # grey60 surface and black text
         emission="grey50", # emission color is grey50
         xlen=0, ylen=0, zlen=0) # Don't add tick marks
# Set default color of future objects to black
rgl.material(color="black")
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges=c("x--", "y+-", "z--"),
       ntick=6, # Attempt 6 tick marks on each side
       cex=.75)+ # Smaller font
# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Weight", edge="x--", line=2)
mtext3d("Displacement", edge="y+-", line=3)
mtext3d("MPG", edge="z--", line=3)


#########################################
#13.9. Saving a Three-Dimensional Plot
#13.9. Guardar un grafico tridimensional 
#########################################

plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type="s", size=0.75, lit=FALSE)
rgl.snapshot('3dplot.png', fmt='png')

rgl.postscript('figs/miscgraph/3dplot.pdf', fmt='pdf')
rgl.postscript('figs/miscgraph/3dplot.ps', fmt='ps')


# Save the current viewpoint
view <- par3d("userMatrix")
# Restore the saved viewpoint
par3d(userMatrix = view)


dput(view)

view <- structure(c(0.907931625843048, 0.267511069774628, -0.322642296552658,
                    0, -0.410978674888611, 0.417272746562958, -0.810543060302734,
                    0, -0.0821993798017502, 0.868516683578491, 0.488796472549438,
                    0, 0, 0, 0, 1), .Dim = c(4L, 4L))
par3d(userMatrix = view)


dput(view)

###############################################
#13.10. Animating a Three-Dimensional Plot
#13.10. Animación de un grafico tridimensional 
###############################################

plot3d(mtcars$wt,
       mtcars$disp,
       mtcars$mpg,
       type = "s",
       size = 0.75, 
       lit = FALSE)

play3d(spin3d())

# De forma predeterminada, el gráfico girará sobre el eje z (vertical), hasta que
# envíe un comando de ruptura a R. Puede cambiar el eje de rotación, la velocidad 
# de rotación y la duración:

# Spin on x-axis, at 4 rpm, for 20 seconds
play3d(spin3d(axis=c(1,0,0), rpm=4), duration=20)

# Spin on z axis, at 4 rpm, for 15 seconds
movie3d(spin3d(axis=c(0,0,1), rpm=4), duration=15, fps=50)

####################################
#13.11. Creating a Dendrogram
#13.11. Creación de un dendograma
###################################

# Get data from year 2009
x <- subset(countries, Year == 2009)

# Drop rows that have any NA values
x <- x[complete.cases(x), ]

x <- x[sample(1:nrow(x), 25), ]

x

x <- x[, 4:7]

x

y <- scale(x)

y

dendrogram <- hclust(dist(y))

#make the dendrogram
plot(dendrogram)

#with tex aligned
plot(dendrogram, hang = -1)


# Un análisis de conglomerados es simplemente una forma de asignar puntos a grupos
# en un espacio n-dimensional (cuatro dimensiones, en este ejemplo). Un análisis de 
# conglomerados jerárquico divide cada grupo en dos grupos más pequeños y se puede 
# representar con los dendogramas en esta receta. Hay muchos parámetros diferentes 
# que puede controlar en el proceso de análisis de conglomerados jerárquicos, y es
# posible que no haya una sola forma "correcta" de hacerlo para sus datos.
# Primero, normalizamos los datos usando scale() con su configuración predeterminada.
# Puede escalar sus datos de manera diferente, o no escalar en absoluto. (Con este 
# conjunto de datos, no escalar los datos hará que el PIB supere a las otras 
# variables, como se muestra en la Figura 13-20). Para el cálculo de la distancia, 
# usamos el método predeterminado, "euclidiano", que calcula la distancia euclidiana 
# entre los puntos. Los otros métodos posibles son "máximo", "manhattan", "canberra",
# "binario" y "minkowski".

#######################################
#13.12. Creating a Vector Field
#13.12. Creación de un campo vectorial
#######################################

view(isabel)
str(isabel)
head(isabel, 3)
tail(isabel, 3)

islice <- subset(isabel, z == min(z))

islice %>% 
  ggplot(aes(
    x = x,
    y = y
  ))+
  geom_segment(aes(
    xend = x + vx/50,
    yend = y + vy/50
    
  ),
  size = 0.25)

# Take a slice where z is equal to the minimum value of z
islice <- subset(isabel, z == min(z))
# Keep 1 out of every 'by' values in vector x
every_n <- function(x, by = 2) {
  x <- sort(x)
  x[seq(1, length(x), by = by)]
}
# Keep 1 of every 4 values in x and y
keepx <- every_n(unique(isabel$x), by=4)
keepy <- every_n(unique(isabel$y), by=4)
# Keep only those rows where x value is in keepx and y value is in keepy
islicesub <- subset(islice, x %in% keepx & y %in% keepy)

library(grid)

islicesub %>% 
  ggplot(aes(
    x = x,
    y = y
  ))+
  geom_segment(aes(
    xend = x + vx/50,
    yend = y + vy/50
  ),
  arrow = arrow(length = unit(0.1, "cm")),
  size = 0.25)


# The existing 'speed' column includes the z component. We'll calculate
# speedxy, the horizontal speed.
islicesub$speedxy <- sqrt(islicesub$vx^2 + islicesub$vy^2)
# Map speed to alpha
ggplot(islicesub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50, alpha = speed),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.6)



#installed.packages("maps")
library(maps)

# Get USA map data
usa <- map_data("usa")
# Map speed to colour, and set go from "grey80" to "darkred"
ggplot(islicesub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50, colour = speed),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.6) +
  scale_colour_continuous(low="grey80", high="darkred") +
  geom_path(aes(x=long, y=lat, group=group), data=usa) +
  coord_cartesian(xlim = range(islicesub$x), ylim = range(islicesub$y))


# Keep 1 out of every 5 values in x and y, and 1 in 2 values in z
keepx <- every_n(unique(isabel$x), by=5)
keepy <- every_n(unique(isabel$y), by=5)
keepz <- every_n(unique(isabel$z), by=2)
isub <- subset(isabel, x %in% keepx & y %in% keepy & z %in% keepz)
ggplot(isub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50, colour = speed),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.5) +
  scale_colour_continuous(low="grey80", high="darkred") +
  facet_wrap( ~ z)

##################################
#13.13. Creating a QQ Plot
#13.13. Creación de un gráfico QQ
#################################

# QQ = quantile-quantile

#QQ pot height
qqnorm(heightweight$weightLb)
qqline(heightweight$weightLb)


# QQ plot of age
qqnorm(heightweight$ageYear)
qqline(heightweight$ageYear)

###########################################################################
#13.14. Creating a Graph of an Empirical Cumulative Distribution Function
#13.14. Crear un gráfico de un acumulativo empírico Función de distribución
###########################################################################

heightweight %>% 
  ggplot(aes(
    x = heightIn
  ))+
  stat_ecdf()

heightweight %>% 
  ggplot(aes(
    x = ageYear
  ))+
  stat_ecdf()

# El ECDF muestra qué proporción de observaciones están en o por debajo del valor
# x dado. Debido a que es empírico, la línea da un paso hacia arriba en cada valor
# de x donde hay uno o más observaciones.

####################################
#13.15. Creating a Mosaic Plot
#13.15. Crear un grafico de mosaico 
####################################

view(UCBAdmissions)
str(UCBAdmissions)
head(UCBAdmissions, 3)
tail(UCBAdmissions, 3)
ftable(UCBAdmissions)
dimnames(UCBAdmissions)
#Use la función mosaic() del paquete vcd. Para este ejemplo, usaremos el conjunto 
#de datos USBAdmissions, que es una tabla de contingencia con tres dimensiones. 

#install.packages("vcd")
library(vcd)

# Split by Admit, then Gender, then Dept
mosaic( ~ Admit + Gender + Dept,
        data = UCBAdmissions)

#Observe que mosaic() divide los datos en el orden en que se proporcionan las variables:
# primero en el estado de admisión, luego en el género, luego en el departamento. 
# El orden de trama resultante deja muy claro que se rechazaron más solicitantes 
# de los que se admitieron. También es claro que dentro del grupo de admitidos 
# había muchos más hombres que mujeres, mientras que en el grupo de rechazados 
# había aproximadamente el mismo número de hombres y mujeres. Sin embargo, es 
# difícil hacer comparaciones dentro de cada departamento. Un orden de división 
# de variables diferente puede revelar otra información interesante

mosaic( ~ Dept + Gender + Admit, data=UCBAdmissions,
        highlighting="Admit", highlighting_fill=c("lightblue", "pink"),
        direction=c("v","h","v"))

# En el ejemplo anterior también especificamos la dirección en la que se dividirá
# cada variable. La primera variable, Departamento, se divide verticalmente; la 
# segunda variable, Género, se divide horizontalmente; y la tercera variable, 
# Admitir, se divide verticalmente. La razón por la que elegimos estas direcciones
# es que, en este ejemplo en particular, facilita la comparación de los grupos de 
# hombres y mujeres dentro de cada departamento.

# Another possible set of splitting directions
mosaic( ~ Dept + Gender + Admit, data=UCBAdmissions,
        highlighting="Admit", highlighting_fill=c("lightblue", "pink"),
        direction=c("v", "v", "h"))

# This order makes it difficult to compare male and female
mosaic( ~ Dept + Gender + Admit, data=UCBAdmissions,
        highlighting="Admit", highlighting_fill=c("lightblue", "pink"),
        direction=c("v", "h", "h"))

##################################
#13.16. Creating a Pie Chart
#13.16. Crear un grafico circular
##################################

# Get a table of how many cases are in each level of fold
library(MASS)

str(survey)

x <- table(survey$Fold)

x

# Make the pie chart
pie(x)

pie(c(99, 18, 120), labels=c("L on R", "Neither", "R on L"))

##############################
#13.17. Creating a Map
#13.17. Creación de un mapa
##############################

states_map <- map_data("state")

states_map %>% 
  ggplot(aes(
    x = long,
    y = lat, 
    group = group
  ))+
  geom_polygon(fill = "white",
               colour = "black")

states_map %>% 
  ggplot(aes(
    x = long,
    y = lat,
    group = group
  ))+
  geom_path()

#install.packages("mapproj")
library(mapproj)

states_map %>% 
  ggplot(aes(
    x = long,
    y = lat,
    group = group
  ))+
  geom_path()+
  coord_map("mercator")


# Get map data for world
world_map <- map_data("world")
world_map

sort(unique(world_map$region))

south_american <- map_data("world",
                           region = c("Bolivia", "Chile", "Argentina", "Peru"))

south_american %>% 
  ggplot(aes(
    x = long,
    y = lat,
    group = group,
    fill = region
  ))+
  geom_polygon(colour = "black")+
  scale_fill_brewer(palette = "Set2")


bo <- map_data("world",
               region = "Bolivia")
bo <- subset(bo,
             long > 0 & lat > -48)
bo %>% 
  ggplot(aes(
    x = long,
    y = lat,
    group = group
  ))+
  geom_path()




