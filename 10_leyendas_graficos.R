library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Leyendas
# Al igual que el eje x o y, una leyenda es una guía: muestra a las personas cómo
# asignar propiedades visuales (estéticas) a valores de datos.

############################
#10.1. Removing the Legend
#10.1. Remover la Leyenda
###########################

View(PlantGrowth)
str(PlantGrowth)

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

x + guides(fill = FALSE)

x + scale_fill_discrete(guide = FALSE)

x + scale_fill_hue(guide = "none")

x + theme(legend.position = "none")

##########################################
#10.2. Changing the Position of a Legend
#10.2. Cambiar la posición de una leyenda
##########################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Pastel2")

x + theme(legend.position = "top")


x + theme(legend.position = c(1,0),
          legend.justification =  c(1,0))

x + theme(legend.position = c(1,1),
          legend.justification = c(1,1))

x + theme(legend.position = c(.85, .2))+
  theme(legend.background = element_rect(fill = "white",
                                         colour = "black"))

x + theme(legend.position = c(.85,.2))+
  theme(legend.background = element_blank())+
  theme(legend.key = element_blank())

########################################################
#10.3. Changing the Order of Items in a Legend
#10.3. Cambiar el orden de los elementos en una leyenda
########################################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

x + scale_fill_discrete(limits = c("trt1", "trt2", "ctrl"))

x + scale_fill_grey(start = .5,
                    end = 1,
                    limits = c("trt1", "trt2", "ctrl"))

x + scale_fill_brewer(palette = "Pastel2",
                      limits = c("trt1", "trt2", "ctrl"))

# Todos los ejemplos anteriores fueron para relleno. Si usa escalas para otras 
# estéticas, como color (para líneas y puntos) o forma (para puntos), debe usar 
# la escala adecuada.Las escalas comúnmente utilizadas incluyen:
  
# • scale_fill_discrete()
# • scale_fill_hue()
# • scale_fill_manual()
# • scale_fill_grey()
# • scale_fill_brewer()
# • scale_colour_discrete()
# • scale_colour_hue()
# • scale_colour_manual()
# • scale_colour_grey()
# • scale_colour_brewer()
# • scale_shape_manual()
# • scale_linetype()

# Por defecto, usar scale_fill_discrete() es equivalente a usar scale_fill_hue();
# lo mismo es cierto para las escalas de color.

#########################################################
#10.4. Reversing the Order of Items in a Legend
#10.4. Invertir el orden de los elementos en una leyenda
#########################################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

#reverse the legend order
x + guides(fill = guide_legend(reverse = TRUE))

x + scale_fill_hue(guide = guide_legend(reverse = TRUE))

####################################
#10.5. Changing a Legend Title
#10.5. Cambiar un título de leyenda
####################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

#set the legend title to "Condicion"
x + labs(fill = "Condición")

x + scale_fill_discrete(name = "Estado")

view(heightweight)
str(heightweight)

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex
  ))+
  geom_point(aes(size = weightLb))+
  scale_size_continuous(range = c(1,4))

x

x + labs(colour = "Male/Female",
         size = "Weight\n(pounds)")

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    shape = sex, 
    colour = sex
  ))+
  geom_point()

x
# Change just shape
x + labs(shape = "Male/Female")

#change both shape and color
x + labs(shape = "Male/Female", colour = "Male/Female")

x <- PlantGrowth %>% 
  ggplot(aes(
    x  = group,
    y = weight,
    fill =  group
  ))+
  geom_boxplot()+
  guides(fill = guide_legend(title = "Genero"))

x

x + guides(fill = guide_legend(title = "Sexo"))

#####################################################
#10.6. Changing the Appearance of a Legend Title
#10.6. Cambiar la apariencia de un título de leyenda
####################################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

x + theme(legend.title = element_text(face = "italic",
                                      family = "Times",
                                      colour = "red",
                                      size = 14))

x + guides(fill = guide_legend(title.theme = element_text(face = "italic",
                                                          family = "times",
                                                          colour = "blue", 
                                                          size = 14)))
###########################################
#10.7. Removing a Legend Title
#10.7. Eliminación de un título de leyenda
###########################################

PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()+
  guides(fill = guide_legend(title = NULL))

PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()+
  scale_fill_hue(guide = guide_legend(title = NULL))

############################################
#10.8. Changing the Labels in a Legend
#10.8. Cambiar las etiquetas en una leyenda
############################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

x + scale_fill_discrete(labels = c("control", "test 1", "test 2"))

x + scale_fill_grey(start = .5,
                    end = 1,
                    labels = c("control", "texto 1", "texto 2"))

x + scale_fill_discrete(limits = c("trt1", "trt2", "ctrl"),
                        labels = c("Treat 1", "Treat 2", "control"))


#the base plot
x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    shape = sex,
    colour = sex
  ))+
  geom_point()

x

#change the labels for one scale 
x + scale_shape_discrete(labels = c("Female", "Male"))


#change the labels for both scales
x + scale_shape_discrete(labels = c("Mujer", "Varon"))+
  scale_colour_discrete(labels = c("Mujer", "Varon"))

# Other commonly used scales with legends include:
# • scale_fill_discrete()
# • scale_fill_hue()
# • scale_fill_manual()
# • scale_fill_grey()
# • scale_fill_brewer()
# • scale_colour_discrete()
# • scale_colour_hue()
# • scale_colour_manual()
# • scale_colour_grey()
# • scale_colour_brewer()
# • scale_shape_manual()
# • scale_linetype()
# By default, using scale_fill_discrete() is equivalent to using scale_fill_hue();
# the same is true for color scales.


#########################################################
#10.9. Changing the Appearance of Legend Labels
#10.9. Cambiar la apariencia de las etiquetas de leyenda
########################################################

x <- PlantGrowth %>% 
  ggplot(aes(
   x = group,
   y = weight, 
   fill = group
  ))+
  geom_boxplot()

x

x + theme(legend.text = element_text(face = "italic",
                                     family = "Arial Narrow",
                                     colour = "green",
                                     size = 14))

x + guides(fill = guide_legend(label.theme = element_text(face = "italic",
                                                          family = "Verdana",
                                                          colour = "red",
                                                          size = 14)))
####################################################
#10.10. Using Labels with Multiple Lines of Text
#10.10. Uso de etiquetas con varias líneas de texto
###################################################

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()

x

# Labels that have more than one line
x + scale_fill_discrete(labels = c("Control", "Type 1\nTreatment", "Type 2\nTreatment"))

x + scale_fill_discrete(labels = c("Control", "Type 1\nTreatment", "Type 2\nTreatment"))+
  theme(legend.text = element_text(lineheight = .8),
        legend.key.heigh = unit(1, "cm"))
