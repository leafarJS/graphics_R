library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Uso de colores en parcelas
# En la implementación de la gramática de gráficos de ggplot2, el color es una 
# estética, al igual que la posición x, la posición y y el tamaño. Si el color es
# una estética más, ¿por qué merece un capítulo propio? La razón es que el color 
# es una estética más complicada que las demás. En lugar de simplemente mover 
# geoms de izquierda a derecha o hacerlos más grandes y más pequeños, cuando usa 
# color, hay muchos grados de libertad y muchas más opciones para hacer. 
# ¿Qué paleta debería usar para valores discretos? ¿Deberías usar un degradado 
# con varios tonos diferentes? ¿Cómo elige colores que puedan ser interpretados 
# con precisión por personas con deficiencias en la visión del color? En este 
# capítulo, abordaré estos temas

###################################################
#12.1. Setting the Colors of Objects
#12.1. Configuración de los colores de los objetos
###################################################

view(mtcars)
str(mtcars)
head(mtcars, 2)

mtcars %>% 
  ggplot(aes(
    x = wt,
    y = mpg
  ))+
  geom_point(colour = "#fe9929")

#install.packages("MASS")
library(MASS)

view(birthwt)
str(birthwt)
head(birthwt,3)

birthwt %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill = "#1d91c0",
                 colour = "#081d58")

birthwt %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill = "#1d91c0",
                 colour = "#081d58")

birthwt %>% 
  ggplot(aes(
    x = bwt
  ))+
  geom_histogram(fill = "#1d91c0",
                 colour = "#081d58",
                 binwidth = 8)

#########################################
#12.2. Mapping Variables to Colors
#12.2. Asignación de variables a colores
########################################

view(cabbage_exp)
str(cabbage_exp)
head(cabbage_exp, 2)

# These both have the same effect
# Ambos tienen el mismo efecto

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           position = "dodge",
           colour = "black")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight
  ))+
  geom_bar(aes(
    fill = Cultivar
  ),
  stat = "identity",
  colour = "#49006a",
  position = "dodge")

# These both have the same effect
# Ambos tienen el mismo efecto

view(mtcars)
str(mtcars)
head(mtcars, 2)
tail(mtcars, 2)

mtcars %>% 
  ggplot(aes(
    x = wt,
    y = mpg,
    colour = cyl
  ))+
  geom_point()

mtcars %>% 
  ggplot(aes(
    x = wt,
    y = mpg
  ))+
  geom_point(aes(colour = cyl))

# Convert to factor in call to ggplot()
# Convertir a factor en la llamada a ggplot()

mtcars %>% 
  ggplot(aes(
    x = wt,
    y = mpg,
    colour = factor(cyl)
  ))+
  geom_point()+
  theme(legend.position = "top")

# Another method: Convert to factor in the data
# otro metodo: convertir el factor en el marco de datos

x <- mtcars # hacer una copia de mtcars
x$cyl <- factor(x$cyl) #convertir cyl en un factor 

x %>% 
  ggplot(aes(
    x = wt,
    y = mpg,
    colour  = cyl
  ))+
  geom_point()+
  theme(legend.position = "bottom")

##############################################################
#12.3. Using a Different Palette for a Discrete Variable
#12.3. Uso de una paleta diferente para una variable discreta
##############################################################


# Fill                  | scale Color             | scale Description
# scale_fill_discrete() | scale_colour_discrete() | Colors evenly spaced around the color wheel (same as                                              hue)
# scale_fill_hue()      | scale_colour_hue()      | Colors evenly spaced around the color wheel (same as discrete)
# scale_fill_grey()     | scale_colour_grey()     | Greyscale palette
# scale_fill_brewer()   | scale_colour_brewer()   | ColorBrewer palettes
# scale_fill_manual()   | scale_colour_manual()   | Manually specified colors

view(uspopage)
str(uspopage)
head(uspopage, 3)
tail(uspopage, 3)

#base plot
x <- uspopage %>% 
  ggplot(aes(
    x = Year,
    y = Thousands,
    fill = AgeGroup
  ))+
  geom_area()+
  theme(legend.position = "top")

x

#these three have the same effect

x + scale_fill_discrete()
x + scale_fill_hue()

#colorBrewer palette

x + scale_fill_brewer()


view(heightweight)
str(heightweight)
head(heightweight, 3)
tail(heightweight, 3)

# Basic scatter plot
x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex
  ))+
  geom_point()+
  theme(legend.position = "top")

#default lightness = 65

x

#slightly darker

x + scale_color_hue(l = 45)



#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

View(uspopage)

x <- uspopage %>% 
  ggplot(aes(
    x = Year,
    y = Thousands,
    fill = AgeGroup
  ))+
  geom_area()+
  theme(legend.position = "top")

x + scale_fill_brewer(palette = "Oranges")


x + scale_fill_grey()

########################################################################
#12.4. Using a Manually Defined Palette for a Discrete Variable
#12.4. Uso de una paleta definida manualmente para un Variable discreta
########################################################################

#base plot
x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex
  ))+
  geom_point()+
  theme(legend.position = "top")

x

#using color names

x + scale_color_manual(values = c("violet", "yellow"))

#using RGB values
x + scale_color_manual(values = c("#fdd49e", "#238443"))


# El orden de los elementos en el vector de valores coincide con el orden de los
# niveles de los factores para la escala discreta. En el ejemplo anterior, el 
# orden del sexo es f, luego m, por lo que el primer elemento en valores va con f
# y el segundo va con m. Aquí se explica cómo ver el orden de los niveles de los
# factores:

levels(heightweight$sex)
# "f" "m"

x + scale_color_manual(values = c(f="#fed976", m="#bd0026"))
  
###########################################
#12.5. Using a Colorblind-Friendly Palette
#12.5. Usar una paleta para daltónicos
##########################################

#base plot
x <- uspopage %>% 
  ggplot(aes(
    x = Year,
    y = Thousands,
    fill = AgeGroup
  ))+
  geom_area()+
  theme(legend.position = "top")

x

#the palette with grey
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

cb_palette_1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                  "#CC79A7")
#add it to the plot

x + scale_fill_manual(values = cb_palette)

x + scale_fill_manual(values = cb_palette_1)

########################################################################
#12.6. Using a Manually Defined Palette for a Continuous Variable
#12.6. Uso de una paleta definida manualmente para un Variable continua
#######################################################################

str(heightweight)

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = weightLb
  ))+
  geom_point(size = 3)+
  theme(legend.position = "top")

x

#with a gradient between two colors

x + scale_color_gradient(low = "black", high = "white")

#a gradient with a white midpoint

#install.packages("scales")
library(scales)

x + scale_color_gradient2(low=muted("red"), 
                          mid="white",
                          high=muted("blue"),
                          midpoint=110)

# a gradient of n colors

x + scale_color_gradientn(colours = c("darkred", "orange", "yellow", "white"))

x + scale_colour_gradientn(colours = c("#00441b", "#4d004b", "#084081", "#7f0000"))


# Fill                    | scale Color             | scale Description
# scale_fill_gradient()   | scale_colour_gradient() | Two-color gradient
# scale_fill_gradient2()  | scale_colour_gradient2()| Gradient with a middle color and two colors that diverge from it
# scale_fill_gradientn()  | scale_colour_gradientn()| Gradient with n colors, equally spaced

#####################################################
#12.7. Coloring a Shaded Region Based on Value
#12.7. Colorear una región sombreada según el valor
####################################################

view(climate)
str(climate)
head(climate, 3)
tail(climate, 3)

ssc <- subset(climate, Source == "Berkeley")

ssc$balance[ssc$Anomaly10y >= 0] <- "POS"
ssc$balance[ssc$Anomaly10y < 0] <- "NEG"

ssc

ssc %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y
  ))+
  geom_area(aes(fill = balance))+
  geom_line(size = 1)+
  geom_hline(yintercept = 0, size = 1)+
  theme(legend.position = "none")

# Si observa detenidamente la figura, notará que hay algunas áreas sombreadas 
# cerca de la línea cero. Esto se debe a que cada una de las dos áreas coloreadas
# es un solo polígono delimitado por los puntos de datos, y los puntos de datos
# no están realmente en cero. Para resolver este problema, podemos interpolar 
# los datos a 1,000 puntos usando approx():  
  
# approx() returns a list with x and y vectors
interp <- approx(ssc$Year, ssc$Anomaly10y, n=1000)
interp

# Put in a data frame and recalculate valence
ssc1 <- data.frame(Year = interp$x, Anomaly10y = interp$y)

ssc1$estado[ssc1$Anomaly10y >= 0] <- "Positivo"
ssc1$estado[ssc1$Anomaly10y < 0] <- "Negativa"

ssc1

ssc1 %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y
  ))+
  geom_area(aes(
    fill = estado),
    alpha = .4)+
  geom_line()+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("#cceeff", "#ffdddd"),
                    guide = FALSE)+
  scale_x_continuous(expand = c(0,0))+
  theme_minimal()
