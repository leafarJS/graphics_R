library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

#Los gráficos de líneas se utilizan típicamente para visualizar cómo una 
#variable continua, en el eje y tiene cambios en relación a otra variable 
#continua, en el eje x. A menudo la x variable representa el tiempo, pero
#también puede representar alguna otra cantidad continua, como la cantidad 
#de un fármaco administrado a sujetos experimentales.
#Al igual que con los gráficos de barras, hay excepciones. Los gráficos de 
#líneas también se pueden utilizar con un discreto variable en el eje x. 
#Esto es apropiado cuando la variable está ordenada (por ejemplo, "pequeño",
#“mediano”, “grande”), pero no cuando la variable no está ordenada 
#(p. ej., “vaca”, “ganso”, “cerdo”). La mayoría de los ejemplos en este 
#capítulo usan una variable x continua, pero veremos una ejemplo en el que 
#la variable se convierte en un factor y, por lo tanto, se trata como 
#una variable discreta.

########################################
#4.1. Making a Basic Line Graph
#4.1. Hacer un gráfico lineal básico
########################################

view(BOD)
str(BOD)

BOD %>% 
ggplot(aes(
  x = Time,
  y = demand
))+
  geom_line()

x <- BOD
x$Time <- factor(x$Time)

str(x)

x %>% 
  ggplot(aes(
    x = Time,
    y = demand, 
    group = 1
  ))+
  geom_line()


BOD %>% 
  ggplot(aes(
    x = Time,
    y = demand
  ))+
  geom_line()+
  ylim(0, max(BOD$demand))

BOD %>% 
  ggplot(aes(
    x = Time,
    y = demand
  ))+
  geom_line()+
  expand_limits(y = 0)

##########################################
#4.2. Adding Points to a Line Graph
#4.2. Añadir puntos a un grafico lineal
##########################################

BOD %>% 
  ggplot(aes(
    x = Time, 
    y = demand
  ))+
  geom_line()+
  geom_point()


view(worldpop)
str(worldpop)

worldpop %>% 
  ggplot(aes(
    x = Year, 
    y = Population
  ))+
  geom_line()+
  geom_point()

#La misma con un eje log y
worldpop %>% 
  ggplot(aes(
    x = Year, 
    y = Population
  ))+
  geom_line()+
  geom_point()+
  scale_y_log10()

################################################
#4.3. Making a Line Graph with Multiple Lines
#4.3. Hacer un grafico lineal con varias lineas
################################################

view(ToothGrowth)
str(ToothGrowth)

x <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))

x %>% 
  ggplot(aes(
    x = dose,
    y = length, 
    colour = supp
  ))+
  geom_line()

x %>% 
  ggplot(aes(
    x = dose,
    y = length, 
    linetype = supp
  ))+
  geom_line()

x %>% 
  ggplot(aes(
    x = factor(dose),
    y = length,
    colour = supp,
    group = supp
  ))+
  geom_line()

#Observe el uso de group=supp. Sin esta declaración, ggplot() no sabrá cómo
#agrupe los datos para dibujar las líneas, y dará un error:

x %>% 
  ggplot(aes(
    x = dose, 
    y = length
  ))+
  geom_line()
#gráfico incorrecto con un patrón de diente de sierra dentado

x %>% 
  ggplot(aes(
    x = dose,
    y = length, 
    shape = supp
  ))+ geom_line()+
  geom_point(size = 4)

x %>% 
  ggplot(aes(
    x = dose, 
    y = length,
    fill = supp
  ))+ geom_line()+
  geom_point(size = 4, 
             shape = 21)

#evitar super posiciones
x %>% 
  ggplot(aes(
    x = dose, 
    y = length,
    shape = supp
  ))+
  geom_line(position = position_dodge(0.2))+
  geom_point(position = position_dodge(0.2), size = 3)

##########################################
#4.4. Changing the Appearance of Lines
#4.4. Cambiar la apariencia de las lineas
##########################################

BOD %>% 
  ggplot(aes(
    x = Time,
    y = demand
  ))+
  geom_line(linetype = "dashed",
            size = 2, 
            colour = "red")

x %>% 
  ggplot(aes(
    x = dose, 
    y = length, 
    colour = supp
  ))+
  geom_line()+
  scale_color_brewer(palette = "Set1")

x %>% 
  ggplot(aes(
    x = dose,
    y = length, 
    group = supp
  ))+
  geom_line(colour = "darkgreen",
            size = 1.5)

x %>% 
  ggplot(aes(
    x = dose, 
    y = length,
    colour = supp
  ))+
  geom_line(linetype = "dashed")+
  geom_point(shape = 22,
             size = 3, 
             fill = "white")

##########################################
#4.5. Changing the Appearance of Points
#4.5. Cambiar la aparienica de los puntos
##########################################

BOD %>% 
  ggplot(aes(
    x = Time,
    y = demand
  ))+
  geom_line()+
  geom_point(size = 4,
             shape = 22,
             colour = "darkred",
             fill = "pink")

BOD %>% 
  ggplot(aes(
    x = Time,
    y = demand
  ))+
  geom_line()+
  geom_point(size = 5,
             shape = 21, 
             fill = "white")

x %>% 
  ggplot(aes(
    x = dose, 
    y = length, 
    fill = supp
  ))+
  geom_line(position = position_dodge(0.2))+
  geom_point(shape = 21,
             size = 4, 
             position = position_dodge(0.2))+
  scale_fill_manual(values = c("#8c96c6", "#810f7c"))

##############################################
#4.6. Making a Graph with a Shaded Area
#4.6. Hacer un gráfico con un área sombreada
##############################################

x = data.frame(
  year = as.numeric(time(sunspot.year)),
  sunspots = as.numeric(sunspot.year)
)
str(x)
view(x)

x %>% 
  ggplot(aes(
    x = year, 
    y = sunspots
  ))+
  geom_area()

x %>% 
  ggplot(aes(
    x = year,
    y = sunspots
  ))+
  geom_area(colour = "red", 
            fill = "darkred", 
            alpha = 0.2)

x %>% 
  ggplot(aes(
    x = year, 
    y = sunspots
  ))+
  geom_area(fill = "blue", 
            alpha = 0.2)+
  geom_line()

#########################################
#4.7. Making a Stacked Area Graph
#4.7. Hacer un grafico de areas apiladas
#########################################

view(uspopage)
str(uspopage)

uspopage %>% 
  ggplot(aes(
    x = Year,
    y = Thousands, 
    fill = AgeGroup
  ))+
  geom_area()

uspopage %>% 
  ggplot(aes(
    x = Year,
    y = Thousands,
    fill = AgeGroup
  ))+
  geom_area(colour = "black", 
            size = 0.2, 
            alpha = 0.4)+
  scale_fill_brewer(palette = "Blues", 
                    breaks = rev(levels(uspopage$AgeGroup)))+
theme_minimal()

uspopage %>% 
  ggplot(aes(
    x = Year,
    y = Thousands,
    fill = AgeGroup,
    order = desc(AgeGroup)
  ))+
  geom_area(colour = "black", 
            size = 0.2, 
            alpha = 0.4)+
  scale_fill_brewer(palette = "Blues")

ggplot(data = uspopage, 
       aes(
         x = Year, 
         y = Thousands, 
         fill = AgeGroup,
         order = desc(AgeGroup)
       ))+
  geom_area(colour = NA, alpha = 0.4)+
  scale_fill_brewer(palette = "Blues")+
  geom_line(position = "stack", size = 0.2)

####################################################
#4.8. Making a Proportional Stacked Area Graph
#4.8. Hacer un gráfico de área apilada proporcional 
####################################################

view(uspopage)
x <- ddply(uspopage,
           "Year",
           transform, 
           percent = Thousands / sum(Thousands) * 100)

str(x)
view(x)

x %>% 
  ggplot(aes(
    x  = Year, 
    y = percent, 
    fill = AgeGroup
  ))+
  geom_area()

x %>% 
  ggplot(aes(
    x  = Year, 
    y = percent, 
    fill = AgeGroup
  ))+
  geom_area(colour = "black",
            size = 0.2,
            alpha = 0.4)


x %>% 
  ggplot(aes(
    x  = Year, 
    y = percent, 
    fill = AgeGroup
  ))+
  geom_area(colour = "black",
            size = 0.2,
            alpha = 0.4)+
  scale_fill_brewer(palette = "Greens", 
                    breaks = rev(levels(uspopage$AgeGroup)))

########################################
#4.9. Adding a Confidence Region
#4.9. Adición de una región de confianza
########################################

view(climate)
x <- subset(climate, 
            Source == "Berkeley",
            select = c("Year", "Anomaly10y", "Unc10y")
            )
str(x)

x %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y
  ))+
  geom_ribbon(aes(
    ymin = Anomaly10y - Unc10y,
    ymax = Anomaly10y + Unc10y
  ),
  alpha = 0.2)+
  geom_line()


x %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y
  ))+
  geom_line(aes(
    y = Anomaly10y-Unc10y
  ), colour = "green",
  linetype = "dotted",
  size = 1)+
  geom_line(aes(
    y = Anomaly10y+Unc10y
  ), colour = "red",
  linetype = "dotted",
  size = 1)+
  geom_line()
