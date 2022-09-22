library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# facetas
# Una de las técnicas más útiles en la visualización de datos es representar grupos
# de datos uno al lado del otro, lo que facilita la comparación de los grupos. 
# Con ggplot2, una forma de hacer esto es asignando una variable discreta a una 
# estética, como la posición x, el color o la forma. Otra forma de hacer esto es 
# crear una subparcela para cada grupo y dibujar las subparcelas una al lado de 
# la otra. Este tipo de gráficos se conocen como pantallas Trellis. Se implementan
# en el paquete Lattice y en el paquete ggplot2. En ggplot2, se llaman facetas. 

###################################################
#11.1. Splitting Data into Subplots with Facets
#11.1. División de datos en subparcelas con facetas
####################################################

View(mpg)
str(mpg)
head(mpg)
tail(mpg)

#the base plot
x <- mpg %>% 
  ggplot(aes(
    x = displ,
    y = hwy
  ))+
  geom_point()

x

# Faceted by drv, in vertically arranged subpanels
# Facetado por drv, en subpaneles dispuestos verticalmente

x + facet_grid(drv ~ .)

# Faceted by cyl, in horizontally arranged subpanels
# Facetado por cilindro, en subpaneles dispuestos horizontalmente

x + facet_grid(. ~ cyl)

# Split by drv (vertical) and cyl (horizontal)
# Dividido por drv (vertical) y cil (horizontal)

x + facet_grid(drv ~ cyl)

# Facet on class
# Note there is nothing before the tilde

x + facet_wrap( ~ class)

# These will have the same result: 2 rows and 4 cols

x + facet_wrap( ~ class, nrow = 2)
x + facet_wrap( ~ class, ncol = 4)

###########################################
#11.2. Using Facets with Different Axes
#11.2. Uso de facetas con diferentes ejes
###########################################

# whit free y scales
# Con escalas y libres

x + facet_grid(drv ~ cyl, scales = "free_y")

# With free x and y scales
# Con escalas libres en x y y

x + facet_grid(drv ~ cyl, scales = "free")

###################################################
#11.3. Changing the Text of Facet Labels
#11.3. Cambiar el texto de las etiquetas de faceta
###################################################

var <- mpg

levels(var$drv)[levels(var$drv) == "4"] <- "4WD"
levels(var$drv)[levels(var$drv) == "f"] <- "FRONT"
levels(var$drv)[levels(var$drv) == "r"] <- "REAR"

view(var)

var %>% 
  ggplot(aes(
    x = displ,
    y = hwy
  ))+
  geom_point() +
  facet_grid(drv ~ .)

var %>% 
 ggplot(aes(
   x=displ,
   y=hwy
   )) + 
  geom_point() +
  facet_grid(drv ~ ., labeller = label_both)

var %>% 
 ggplot(aes(
   x=displ, 
   y=hwy
   )) + 
  geom_point()+
  facet_grid(drv ~ .)


mpg3 <- mpg

levels(mpg3$drv)[levels(mpg3$drv)=="4"] <- "4^{wd}"
levels(mpg3$drv)[levels(mpg3$drv)=="f"] <- "- Front %.% e^{pi * i}"
levels(mpg3$drv)[levels(mpg3$drv)=="r"] <- "4^{wd} - Front"

mpg3 %>% 
 ggplot(aes(
   x=displ, 
   y=hwy
   )) +
  geom_point() +
  facet_grid(drv ~ ., labeller = label_parsed)

#########################################################################
#11.4. Changing the Appearance of Facet Labels and Headers
#11.4. Cambio de la apariencia de las etiquetas de facetas y encabezados
########################################################################

view(cabbage_exp)
str(cabbage_exp)
head(cabbage_exp, 2)

cabbage_exp %>% 
  ggplot(aes(
    x = Cultivar,
    y = Weight
  )) +
  geom_bar(stat = "identity")+
  facet_grid(. ~ Date)

bbage_exp %>% 
  ggplot(aes(
    x = Cultivar,
    y = Weight
  )) +
  geom_bar(stat = "identity")+
  facet_grid(. ~ Date)+
  theme(strip.text = element_text(face = "bold",
                                  size = rel(1.5)),
        strip.background = element_rect(fill = "lightblue",
                                        colour = "black",
                                        size = 1))

cabbage_exp %>% 
  ggplot(aes(
    x = Cultivar,
    y = Weight,
    fill = Date
  )) +
  geom_bar(stat = "identity")+
  facet_grid(. ~ Date)+
  theme(strip.text = element_text(face = "bold",
                                  size = rel(1.5)),
        strip.background = element_rect(fill = "lightblue",
                                        colour = "black",
                                        size = 1))+
  theme(legend.position = "none")

