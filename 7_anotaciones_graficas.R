library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

# Por lo general, mostrar solo sus datos no es suficiente: hay todo tipo de otra
# información que puede ayudar al espectador a interpretar los datos. Además del
# repertorio estándar de axis etiquetas, marcas de verificación y leyendas, 
# también puede agregar elementos gráficos o de texto individuales a tu parcela.
# Estos se pueden usar para agregar información contextual adicional, resaltar 
# un área de la gráfica, o agregue algún texto descriptivo sobre los datos.

######################################
#7.1. Adding Text Annotations
#7.1. Adición de anotaciones de texto
######################################

x <- faithful %>% 
  ggplot(aes(
    x = eruptions,
    y = waiting
  ))+
  geom_point()

x + annotate("text", 
             x = 3,
             y = 48,
             label = "Group 1")+
  annotate("text",
           x = 4.5,
           y = 66, 
           label = "Group 2")

x + annotate("text",
             x = 3,
             y = 48,
             label = "Grupo 1",
             family = "serif")+
  annotate("text", 
           x = 4.5,
           y = 66, 
           label = "Grupo 2", 
           family = "serif",
           fontface = "italic",
           colour = "darkred", 
           size = 3)

x + annotate("text",
             x = 3,
             y = 48, label = "Área 1",
             alpha = 0.2)+
  geom_text(x = 4.5,
            y = 66,
            label = "Área 2", 
            alpha = 0.2)

x + annotate("text",
             x  = -Inf,
             y = Inf,
             label = "Upper Left",
             hjust = -.2,
             vjust = 2)+
  annotate("text",
           x = mean(range(faithful$eruptions)),
           y = -Inf,
           vjust = -0.4,
           label = "Botton middle")

####################################################
# 7.2. Using Mathematical Expressions in Annotations
# 7.2. Uso de expresiones matemáticas en anotaciones
####################################################

x <- data.frame(x = c(-3,3)) %>% 
  ggplot(aes(
    x = x
  ))+
  stat_function(fun = dnorm)

x + annotate("text",
             x = 2,
             y = 0.3,
             parse =TRUE,
             label = "frac(1, sqrt(2*pi)) * e ^ {-x^2 / 2}")

x + annotate("text",
             x = 0,
             y = 0.05, 
             parse = TRUE,
             size = 4,
             label = "'función: ' * y ==frac(1, sqrt(2*pi)) * e ^{-x^2/2}")

########################
#7.3. Adding Lines
#7.3. Adición de lineas
########################

view(heightweight)
str(heightweight)

x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn,
    colour = sex
  ))+
  geom_point()

#adicionar lineas verticales y horizontales
x + geom_hline(yintercept = 60)+
  geom_vline(xintercept = 14)

#adicionar una linea en angulo
x + geom_abline(intercept = 37.4,
                slope = 1.75)

# for the ddplyr() function 
sex.mean <- ddply(heightweight, "sex", summarise, heightIn = mean(heightIn))

x + geom_hline(aes(
  yintercept = heightIn,
  colour = sex
), data = sex.mean,
linetype = "dashed", size = 1)


view(PlantGrowth)
str(PlantGrowth)

x <- PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_point()

x + geom_vline(xintercept = 2)

x + geom_vline(xintercept = which(levels(PlantGrowth$group)== "ctrl"))

# Es posible que haya notado que agregar líneas difiere de agregar otras
# anotaciones En lugar de usar la función anootate(), hemos usado geom_hline().
# Esto se debe a que las versiones anteriores de ggplot2 no tenía la función annotate().
# La línea geoms tenía código para manejar los casos especiales en los que se 
# usaron para agregar una sola línea, y cambiarlo rompería la compatibilidad con 
# versiones anteriores. En una versión futura sion de ggplot2, esto cambiará, 
# y annotate() funcionará con la línea geoms


##############################################
#7.4. Adding Line Segments and Arrows
#7.4. Adición de segmentos de línea y flechas
##############################################

view(climate)

x <- subset(climate, Source == "Berkeley") %>% 
  ggplot(aes(
    x = Year, 
    y = Anomaly10y
  ))+
  geom_line()

x + annotate("segment",
             x = 1950,
             xend = 1980,
             y = -.25,
             yend = -.25)

#install.packages("grid")
library(grid)

x + annotate("segment",
             x = 1850,
             xend = 1820,
             y = -.8,
             yend = -.95,
             colour = "red",
             size = 1.5,
             arrow = arrow())

x + annotate("segment",
             x = 1850,
             xend = 1820,
             y = -.8,
             yend = -.95,
             colour = "red",
             size = 1.5,
             arrow = arrow())+
  annotate("segment",
           x = 1950, 
           xend = 2000,
           y = -0.25,
           yend = -0.25,
           arrow = arrow(ends = "both",
                         angle = 90,
                         length = unit(.2, "cm")))

######################################
#7.5. Adding a Shaded Rectangle
#7.5. Agregar un rectángulo sombreado
######################################

x <- subset(climate, Source == "Berkeley") %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y
  ))+
  geom_line()

x + annotate("rect",
             xmin = 1950,
             xmax = 2000,
             ymin = -1,
             ymax = 1,
             alpha = .2,
             fill = "red")

# Cada capa se dibuja en el orden en que se agrega al objeto ggplot, por lo que en
# el anterior ejemplo, el rectángulo se dibuja encima de la línea. No es un 
# problema en ese caso, pero si le gustaría tener la línea sobre el rectángulo, 
# agregue primero el rectángulo y luego la línea. Cualquier geom se puede usar con
# annotate(), siempre que pase los parámetros adecuados. 

###########################
#7.6. Highlighting an Item
#7.6. Resaltar un elemento
###########################

view(PlantGrowth)
str(PlantGrowth)

x <- PlantGrowth
x$hl <- "No"

str(x)

x$hl[x$group == "trt2"] <- "Si"

str(x)

x$hl <- as.factor(x$hl)

str(x)

x %>%
  ggplot(aes(
    x = group,
    y = weight,
    fill = hl
  ))+
  geom_boxplot()


x %>%
  ggplot(aes(
    x = group,
    y = weight,
    fill = hl
  ))+
  geom_boxplot()+
  scale_fill_manual(values = c("#a1d99b", "#fec44f"),
                    guide = FALSE)

# Si tiene una pequeña cantidad de elementos, como en este ejemplo, en lugar de 
# crear una nueva columna podría usar el original y especificar los colores para 
# cada nivel de esa variable. Para ejemplo, el siguiente código usará la columna
# de grupo de PlantGrowth y manualmente establecer los colores para cada uno de 
# los tres niveles. El resultado será el mismo que con el código anterior:

PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight,
    fill = group
  ))+
  geom_boxplot()+
  scale_fill_manual(values = c("#fa9fb5", "#78c679", "#41b6c4"),
                    guide = FALSE)

#################################
#7.7. Adding Error Bars
#7.7. Adición de barras de error 
#################################

# En este ejemplo, los datos ya tienen valores para el error estándar de la media
# (se), que usaremos para las barras de error (también tiene valores para la 
# desviación estándar, sd, pero no estamos usando eso aquí)

view(cabbage_exp)
str(cabbage_exp)

x <- subset(cabbage_exp, Cultivar == "c39")
x

x %>% 
  ggplot(aes(
    x  = Date,
    y = Weight
  ))+
  geom_bar(stat = "identity",
           fill = c("#ffeda0", "#fd8d3c", "#807dba"),
           colour = "black")


x %>% 
  ggplot(aes(
    x  = Date,
    y = Weight
  ))+
  geom_bar(stat = "identity",
           fill = c("#ffeda0", "#fd8d3c", "#807dba"),
           colour = "black")+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax = Weight + se
  ),
  width = .5)

x %>% 
  ggplot(aes(
    x = Date,
    y = Weight
  ))+
  geom_line(aes(
    group = 1
  ))+
  geom_point(size = 4)


x %>% 
  ggplot(aes(
    x = Date,
    y = Weight
  ))+
  geom_line(aes(
    group = 1
  ))+
  geom_point(size = 4)+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax  =Weight + se
  ),
  width = .2)

#Bad: dodge width not specified
cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y =  Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           position = "dodge")+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax = Weight + se
  ), position = "dodge",
  width = 0.3)

## Good: dodge width set to same as bar width (0.9)

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y =  Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           position = "dodge")+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax = Weight + se
  ),
  position = position_dodge(0.9),
  width = 0.3)+
  theme(legend.position = "none")

x <- position_dodge(.3)

cabbage_exp %>% 
  ggplot(aes(
    x = Date, 
    y = Weight,
    colour = Cultivar,
    group = Cultivar
  ))+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax = Weight + se
  ),
  width = .2,
  size = .25,
  colour = "black", 
  position = x)

cabbage_exp %>% 
  ggplot(aes(
    x = Date, 
    y = Weight,
    colour = Cultivar,
    group = Cultivar
  ))+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax = Weight + se
  ),
  width = .2,
  size = .25,
  colour = "black", 
  position = x)+
  geom_line(position = x)

cabbage_exp %>% 
  ggplot(aes(
    x = Date, 
    y = Weight,
    colour = Cultivar,
    group = Cultivar
  ))+
  geom_errorbar(aes(
    ymin = Weight - se,
    ymax = Weight + se
  ),
  width = .2,
  size = .25,
  colour = "black", 
  position = x)+
  geom_line(position = x)+
  geom_point(position = x,
             size = 2.5)

###################################################
#7.8. Adding Annotations to Individual Facets
#7.8. Adición de anotaciones a facetas individuales
###################################################

view(mpg)
str(mpg)

# The base plot
x <- mpg %>% 
  ggplot(aes(
    x = displ,
    y = hwy
  ))+
  geom_point()+
  facet_grid(. ~ drv)

# A data frame with labels for each face
f.label <- data.frame(drv = c("4", "f", "r"), 
                      label = c("4wd", "Front", "Rear"))

x + geom_text(x = 6,
              y = 40,
              aes(
                label = label
              ), 
              data = f.label)
# If you use annotate(), the label will appear in all facets
x + annotate("text",
             x = 6,
             y = 42,
             label = "label text")


# Esta función devuelve un marco de datos con cadenas que representan la regresión
# ecuación y el valor de r^2
# Estas cadenas serán tratadas como expresiones matemáticas R

lm.labels <- function(dat){
  modelo <- lm(hwy ~ displ, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(modelo)[1], 2), 
                     round(coef(modelo)[2], 2))
  r <- cor(dat$displ, dat$hwy)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

# For the ddply() function
labels <- ddply(mpg, "drv", lm.labels)
labels



# Find r^2 values for each group
labels <- ddply(mpg, "drv", summarise, r2 = cor(displ, hwy)^2)
labels$r2 <- sprintf("italic(R^2) == %.2f", labels$r2)
# Plot with formula and R^2 values

x <- mpg %>% 
  ggplot(aes(
    x = displ,
    y = hwy
  ))+
  geom_point()+
  facet_grid(. ~ drv)

x

x + geom_smooth(method = lm, se = FALSE)+
  geom_text(x = 3,
            y = 40, 
            aes(
              label = formula
            ), 
            data = labels, 
            parse = TRUE, 
            hjust = 0)

x + geom_smooth(method = lm, se = FALSE)+
  geom_text(x = 3,
            y = 40, 
            aes(
              label = formula
            ), 
            data = labels, 
            parse = TRUE, 
            hjust = 0)+
  geom_text(x = 3, 
            y = 35,
            aes(
              label = r2
            ), 
            data = labels,
            parse = TRUE, 
            hjust = 0)
