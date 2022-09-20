library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)

#Controlling the Overall Appearance of Graphs
#Controlar la apariencia general de gráficos


# Controlar la apariencia general de los gráficos creados por ggplot2. La gramática
# de gráficos que subyace en ggplot2 se ocupa de cómo se procesan y muestran los datos.
# se ocupa por cosas como fuentes, colores de fondo, y así cuando se trata de 
# presentar sus datos, es muy probable que se desee para afinar la apariencia de
# estas cosas. El sistema de temas de ggplot2 proporciona control sobre la aparición
# de elementos que no son datos.

#########################################
#9.1. Setting the Title of a Graph
#9.1. Establecer el título de un gráfico
#########################################

view(heightweight)
str(heightweight)

#plot base
x <- heightweight %>% 
  ggplot(aes(
    x = ageYear,
    y = heightIn
  ))+
  geom_point()

x

x + ggtitle("Edad y talla de los Escolares")

x + ggtitle("Edad y talla\nde los Escolares")

#ggtitle() is equivalent to using labs(title = "Title text")

# Si desea mover el título dentro del área de trazado, puede usar uno de dos 
# métodos, los cuales son un poco complicados. El primer método es usar ggtitle()
# con un valor vjust negativo. El inconveniente de este método es que aún reserva
# un espacio en blanco sobre la región de trazado para el título. 

#move the title inside
x + ggtitle("Edad y Talla de los Escolares")+
  theme(plot.title = element_text(vjust = -2.5))

#El segundo método consiste en utilizar una anotación de texto, estableciendo 
# su posición x en la mitad del rango x y su posición y en Inf, lo que la coloca
# en la parte superior de la región de trazado. Esto también requiere un valor 
# vjust positivo para llevar el texto completamente dentro de la región de trazado:

x + annotate("text",
             x = mean(range(heightweight$ageYear)),
             y= Inf,
             label = "Edad y Talla de los Escolares",
             vjust = 1.5,
             size = 6)

########################################
#9.2. Changing the Appearance of Text
#9.2. Cambiar la aparariencia del texto
########################################

x + theme(axis.title = element_text(size = 16,
                                    lineheight = .9,
                                    family = "Times",
                                    face = "bold.italic",
                                    colour = "red"))

x + ggtitle("Edad y Talla\nde los Estudiantes")+
  theme(axis.title = element_text(size = 16,
                                    lineheight = .9,
                                    family = "Times",
                                    face = "bold.italic",
                                    colour = "red"))

x + ggtitle("Edad y Talla\nde los Estudiantes")+
  theme(plot.title = element_text(size = rel(1.5),
                                  lineheight = .9,
                                  family = "Times",
                                  face = "bold.italic",
                                  colour = "red"))


x + annotate("text",
             x = 15,
             y = 53,
             label = "Some text",
             size = 7,
             family = "Times",
             fontface = "bold.italic",
             colour = "red")

# rel(1.5) significa que la fuente será 1.5 veces el tamaño de fuente base del tema.
# Para los elementos del tema, el tamaño de fuente está en puntos.


x + geom_text(aes(
  label = weightLb
),
size = 3,
family = "Times",
colour = "red")

# Theme elements | Text geoms | Description
# family         | family     | Helvetica, Times, Courier
# face           | fontface   | plain, bold, italic, bold.italic
# colour         | colour     | Color (name or "#RRGGBB")
# size           | size       | Font size (in points for theme elements; in mm for geoms)
# hjust          | hjust      | Horizontal alignment: 0=left, 0.5=center, 1=right
# vjust          | vjust      | Vertical alignment: 0=bottom, 0.5=middle, 1=top
# angle          | angle      | Angle in degrees
# lineheight     |lineheight  | Line spacing multiplier
# 
# Elementos temáticos | Geomas de texto | Descripción
# familia             | familia         | Helvética, veces, mensajero
# fuente              | fuente          | normal, negrita, cursiva, negrita.cursiva
# color               | color           | Color (nombre o "#RRGGBB")
# tamaño              | tamaño          | Tamaño de fuente (en puntos para elementos temáticos; en mm para geoms)
# hjust               | hjust           | Alineación horizontal: 0=izquierda, 0,5=centro, 1=derecha
# vjust               | vjust           | Alineación vertical: 0=inferior, 0,5=medio, 1=superior
# ángulo              | ángulo          | Ángulo en grados
# altura de línea     | altura de línea | Multiplicador de interlineado


# Element name  | Description
# axis.title    | Appearance of axis labels on both axes
# axis.title.x  | Appearance of x-axis label
# axis.title.y  | Appearance of y-axis label
# axis.ticks    | Appearance of tick labels on both axes
# axis.ticks.x  | Appearance of x tick labels
# axis.ticks.y  | Appearance of y tick labels
# legend.title  | Appearance of legend title
# legend.text   | Appearance of legend items
# plot.title    | Appearance of overall plot title
# strip.text    | Appearance of facet labels in both directions
# strip.text.x  | Appearance of horizontal facet labels
# strip.text.y  | Appearance of vertical facet labels


# Nombre del elemento | Descripción
# eje.título          | Aspecto de las etiquetas de los ejes en ambos ejes
# eje.título.x        | Apariencia de la etiqueta del eje x
# eje.título.y        | Apariencia de la etiqueta del eje y
# eje.garrapatas      | Aparición de etiquetas de ticks en ambos ejes
# eje.garrapatas.x    | Apariencia de etiquetas x tick
# eje.ticks.y         | Aspecto de las etiquetas de marca y
# leyenda.título      | Aparición del título de la leyenda
# leyenda.texto       | Aparición de elementos de leyenda
# trama.titulo        | Apariencia del título general de la trama
# tira.texto          | Aspecto de las etiquetas de facetas en ambas direcciones
# tira.texto.x        | Apariencia de etiquetas de facetas horizontales
# tira.texto.y        | Apariencia de etiquetas de facetas verticales


###################
#9.3. Using Themes
#9.3. Uso de temas 
##################


#Grey theme (the default)
x + theme_grey()

#Black an white theme
x + theme_bw()


#Algunas propiedades de los elementos del tema que se usan comúnmente en ggplot2 
# son aquellas cosas que son controladas por theme(). La mayoría de estas cosas, 
# como el título, la leyenda y los ejes, están fuera del área de trazado, pero 
# algunas de ellas están dentro del área de trazado, como las líneas de cuadrícula
# y el color de fondo.

x + theme_grey(base_size = 16,
               base_family = "Times")

# Set default theme for current session
theme_set(theme_bw())

# This will use theme_bw()
x

theme_set(theme_minimal())

x

# Reset the default theme back to theme_grey()
theme_set(theme_grey())

x

######################################################
#9.4. Changing the Appearance of Theme Elements
#9.4. Cambiar la apariencia de los elementos del tema
######################################################


# Options for the plotting area
x + theme(
  panel.grid.major = element_line(colour = "red")
)

x + theme(
  panel.grid.major = element_line(colour = "red"),
  panel.grid.minor = element_line(colour = "blue",
                                  linetype = "dotted",
                                  size = 0.2)
)

x + theme(
  panel.grid.major = element_line(colour = "red"),
  panel.grid.minor = element_line(colour = "blue",
                                  linetype = "dotted",
                                  size = 0.2),
  panel.background = element_rect(fill = "#fb6a4a")
)


x + theme(
  panel.grid.major = element_line(colour = "red"),
  panel.grid.minor = element_line(colour = "blue",
                                  linetype = "dotted",
                                  size = 0.2),
  panel.background = element_rect(fill = "#fb6a4a"),
  panel.border = element_rect(colour = "blue", 
                              fill = NA,
                              size = 2)
)


# Options for text items
x + ggtitle("Aqui Título del grafico")

x + ggtitle("Aqui Título del grafico")+
  theme(
    axis.title.x = element_text(colour = "red", size = 14),
    axis.text.x = element_text(colour = "blue")
  )


x + ggtitle("Aqui Título del grafico")+
  theme(
    axis.title.x = element_text(colour = "red", size = 14),
    axis.text.x = element_text(colour = "blue"),
    axis.title.y = element_text(colour = "red", size = 14, angle = 90),
    axis.text.y = element_text(colour = "blue")
  )


x + ggtitle("Aqui Título del grafico")+
  theme(
    axis.title.x = element_text(colour = "red", size = 14),
    axis.text.x = element_text(colour = "blue"),
    axis.title.y = element_text(colour = "red", size = 14, angle = 90),
    axis.text.y = element_text(colour = "blue"),
    plot.title = element_text(colour = "red", size = 20, face = "bold")
  )


#options for the legend

x +
  theme(
    legend.background = element_rect(fill = "grey85",
                                     colour = "red", 
                                     size = 1)
  )

x +
  theme(
    legend.background = element_rect(fill = "grey85",
                                     colour = "red", 
                                     size = 1),
    legend.title = element_text(colour = "blue",
                                face = "bold", 
                                size = 14)
  )


x +
  theme(
    legend.background = element_rect(fill = "grey85",
                                     colour = "red", 
                                     size = 1),
    legend.title = element_text(colour = "blue",
                                face = "bold", 
                                size = 14),
    legend.text = element_text(colour = "red")
  )


x +
  theme(
    legend.background = element_rect(fill = "grey85",
                                     colour = "red", 
                                     size = 1),
    legend.title = element_text(colour = "blue",
                                face = "bold", 
                                size = 14),
    legend.text = element_text(colour = "red"),
    legend.key = element_rect(colour = "blue",
                              size = 0.25)
  )


# Options for facets
x + facet_grid(sex ~ .)

x + facet_grid(sex ~ .)+
  theme(
    strip.background = element_rect(fill = "pink")
  )


x + facet_grid(sex ~ .)+
  theme(
    strip.background = element_rect(fill = "pink"),
    strip.text.y = element_text(size = 14,
                                angle = -90,
                                face = "bold")
  )

x + facet_grid(sex ~ .)+
  theme(
    strip.background = element_rect(fill = "pink"),
    strip.text.x = element_text(size = 14,
                                angle = -90,
                                face = "bold")
  )

# theme() has no effect if before adding a complete theme
x + theme(
  axis.title.x = element_text(colour = "red")
)+
  theme_bw()

# theme() works if after a compete theme
x + theme_bw() + 
  theme(axis.title.x = element_text(color = "red", 
                                    size = 12))

# Name              | Description                            |Element type
# text              | All text elements                      | element_text()
# rect              | All rectangular elements               | element_rect()
# line              | All line elements                      | element_line()
# axis.line         | Lines along axes                       | element_line()
# axis.title        | Appearance of both axis labels         | element_text()
# axis.title.x      | X-axis label appearance                | element_text()
# axis.title.y      | Y-axis label appearance                | element_text()
# axis.text         | Appearance of tick labels on both axes | element_text()
# axis.text.x       | X-axis tick label appearance           | element_text()
# axis.text.y       | Y-axis tick label appearance           | element_text()
# legend.background | Background of legend                   | element_rect()
# legend.text       | Legend item appearance                 | element_text()
# legend.title      | Legend title appearance                | element_text()
# legend.position   | Position of the legend                 | "left", "right", "bottom", "top", or two-element numeric vector
#                                                              if you wish to place it inside the plot area (for more on legend
#                                                             placement, see Recipe 10.2)
# panel.background  | Background of plotting area            | element_rect()
# panel.border      | Border around plotting area            | element_rect(linetype="dashed")
# panel.grid.major  | Major grid lines                       | element_line()
# panel.grid.major.x| Major grid lines, vertical             | element_line()
# panel.grid.major.y| Major grid lines, horizontal           | element_line()
# panel.grid.minor  | Minor grid lines                       | element_line()
# panel.grid.minor.x| Minor grid lines, vertical             | element_line()
# panel.grid.minor.y| Minor grid lines, horizontal           | element_line()
# plot.background   | Background of the entire plot          | element_rect(fill = "white", col our = NA)
# plot.title        | Title text appearance                  | element_text()
# strip.background  | Background of facet labels             | element_rect()
# 
# 
# strip.text        | Text appearance for vertical and horizontal facet labels | element_text()
# strip.text.x      | Text appearance for horizontal facet labels              | element_text()
# strip.text.y      | Text appearance for vertical facet labels                | element_text()

################################
#9.5. Creating Your Own Themes
#9.5. Creando tus propios temas 
################################

# Start with theme_bw() and modify a few things

my.theme <- theme_bw()+
  theme(text = element_text(colour = "red"),
        axis.title = element_text(size  =rel(1.25)))

x + my.theme

####################################
#9.6. Hiding Grid Lines
#9.6. Ocultar lineas de cuadriculas
####################################

#hide all lines
x + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

# Hide the vertical grid lines (which intersect with the x-axis)
x + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)
# Hide the horizontal grid lines (which intersect with the y-axis)
x + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)
