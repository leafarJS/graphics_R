library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)
#Los gráficos de barras son quizás el tipo de 
#visualización de datos más utilizado. Ellos son
#normalmente usados para mostrar valores numéricos 
#(en el eje y), para diferentes categorías (en el
#eje x). Por ejemplo, un gráfico de barras sería 
#bueno para mostrar los precios de cuatro diferentes
#clases de artículos Un gráfico de barras 
#generalmente no sería tan bueno para mostrar los 
#precios a lo largo del tiempo, donde el tiempo es
#una variable continua, aunque se puede hacer, 
#como veremos en este capítulo.
#Hay una distinción importante que debe tener en 
#cuenta al hacer gráficos de barras:
#a veces, las alturas de las barras representan 
#recuentos de casos en el conjunto de datos y, 
#a veces, representan valores en el conjunto de 
#datos. Tenga presente esta distinción: puede ser
#una fuente de confusión ya que tienen relaciones 
#muy diferentes con los datos, pero el mismo término 
#es usado para los dos.

######################################
#3.1. Making a Basic Bar Graph
#3.1 Hacer un grafico de barras basico
######################################

view(pg_mean)
str(pg_mean)

pg_mean %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_bar(stat = "identity")

pg_mean %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_bar(stat = "identity", fill="red", colour = "black")
  
#Cuando x es una variable continua (o numérica),
#las barras se comportan de forma un poco diferente. 
#En cambio de tener una barra en cada valor x real, 
#hay una barra en cada valor x posible entre
#el mínimo y el máximo,Puedes convertir  una variable
#continua a una variable discreta usando factor():

view(BOD)
str(BOD)

# RESULTADO VARIABLE CONTINUA
BOD %>% 
  ggplot(aes(
    x = Time,
    y = demand
  ))+
  geom_bar(stat = "identity")

# RESULTADO CON VARIABLE DISCRETA
BOD %>% 
  ggplot(aes(
    x = as.factor(Time),
    y = demand
  ))+
  geom_bar(stat = "identity")

#############################
#3.2 Grouping Bars Together
#3.2 Agrupar barras juntas
#############################

view(cabbage_exp)
str(cabbage_exp)

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight
  ))+
  geom_bar(stat = "identity")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar
  ))+
  geom_bar(stat = "identity")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar
  ))+
  geom_bar(position = "dodge",
           stat = "identity")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar #variable discreta forever
  ))+
  geom_bar(position = "dodge", 
           colour = "black", 
           stat = "identity")+
  scale_fill_brewer(palette = "Pastel1")

# CUANDO FALTA UN DATO EN EL MARCO DE DATOS
x <- cabbage_exp[1:5,]
x
x %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar
  ))+
  geom_bar(stat = "identity", 
           position = "dodge", 
           colour = "black")+
  scale_fill_brewer(palette = "Pastel1")

###########################################
#3.3. Making a Bar Graph of Counts
#3.3. Hacer un gráfico de barras de conteo
###########################################

view(diamonds)
str(diamonds)
diamonds %>% 
  ggplot(aes(
    x = cut
  ))+
  geom_bar(fill ="red", 
           colour = "black")

diamonds %>% 
  ggplot(aes(
    x = carat
  ))+
  geom_histogram()

##########################################
#3.4. Using Colors in a Bar Graph
#3.4. usar colores en un gráfico de barras
##########################################

view(uspopchange)
str(uspopchange)
upc <- subset(uspopchange, rank(Change)>40)


upc %>% 
  ggplot(aes(
    x = Abb,
    y = Change
  ))+
  geom_bar(stat = "identity")

upc %>% 
  ggplot(aes(
    x = Abb,
    y = Change,
    fill = Region
  ))+
  geom_bar(stat = "identity")

upc %>% 
  ggplot(aes(
    x = Abb,
    y = Change,
    fill = Region
  ))+
  geom_bar(stat = "identity", 
           colour = "black")+
  scale_fill_manual(values = c("#feb24c", "#bd0026"))+
  xlab("Estados")+
  ylab("% Cambio Poblacional")

#####################################################################
#3.5. Coloring Negative and Positive Bars Differently
#3.5. 3.5. Colorear barras negativas y positivas de manera diferente 
#####################################################################

view(climate)
str(climate)
csub <- subset(climate, Source== "Berkeley" & Year >= 1900)

#crear una nueva columna booleana
csub$pos <- csub$Anomaly10y >= 0
head(csub, 3)
tail(csub, 3)

csub %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y,
    fill = pos
  ))+
  geom_bar(stat = "identity",
           position = "identity")

csub %>% 
  ggplot(aes(
    x = Year,
    y = Anomaly10y,
    fill = pos
  ))+
  geom_bar(stat = "identity", 
           position = "identity",
           colour = "black", 
           size = 0.25)+
  scale_fill_manual(values = c("#9e9ac8", "#6a51a3"),
                    guide = FALSE)

############################################
#3.6. Adjusting Bar Width and Spacing
#3.6. Ajuste ancho y espaciado de las barras
############################################

str(pg_mean)

#grafico estandar
pg_mean %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_bar(stat = "identity")

#barras mas estrechas
pg_mean %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_bar(stat = "identity",
           width = 0.5)

#barras maximo de ancho
pg_mean %>% 
  ggplot(aes(
    x = group,
    y = weight
  ))+
  geom_bar(stat = "identity", 
           width = 1)

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight
  ))+
  geom_bar(stat = "identity")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           width = 0.5,
           position = "dodge")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           width = 0.5,
           position = position_dodge(0.7))

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           width = 0.9,
           position = position_dodge())

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           position = position_dodge(0.9))

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           width = 0.9,
           position = position_dodge(0.9))

#########################################
#3.7. Making a Stacked Bar Graph
#3.7. Hacer un gráfico de barras apilado
#########################################


str(cabbage_exp)
head(cabbage_exp, 3)

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar
  ))+
  geom_bar(stat = "identity")

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar
  ))+
  geom_bar(stat = "identity")+
  guides(fill=guide_legend(reverse=TRUE))

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar,
    order = desc(Cultivar)
  ))+
  geom_bar(stat = "identity",
           colour = "#004529",
           width = 0.7)+
  scale_fill_manual(values = c("#d9f0a3", "#238443"))

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y = Weight, 
    fill = Cultivar
  ))+
  geom_bar(stat = "identity", colour = "black")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_fill_brewer(palette = "Pastel2")

#############################################################
#3.8. Making a Proportional Stacked Bar Graph
#3.8. Hacer un gráfico debarras apiladas proporcionales 100%
#############################################################

x = ddply(cabbage_exp,
          "Date",
          transform,
          percent_weight = Weight / sum(Weight) * 100)
str(x)
view(x)

x %>% 
  ggplot(aes(
    x = Date,
    y = percent_weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity")

x %>% 
  ggplot(aes(
    x = Date,
    y = percent_weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           colour = "black")+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_fill_brewer(palette = "Pastel3")

##############################################
#3.9. Adding Labels to a Bar Graph
#3.9. Agregar etiquetas a un grafico de barras
##############################################

cabbage_exp %>% 
  ggplot(aes(
    x = interaction(Date, Cultivar),
    y =  Weight
  ))+
  geom_bar(stat = "identity")+
  geom_text(aes(
    label = Weight
  ),
  vjust = 1.5, 
  colour = "white")

cabbage_exp %>% 
  ggplot(aes(
    x = interaction(Date, Cultivar),
    y =  Weight
  ))+
  geom_bar(stat = "identity")+
  geom_text(aes(
    label = Weight
  ),
  vjust = -0.2)

#ajustar los limites para ser un poca mas alta
cabbage_exp %>% 
  ggplot(aes(
    x = interaction(Date, Cultivar),
    y =  Weight
  ))+
  geom_bar(stat = "identity")+
  geom_text(aes(
    label = Weight
  ),
  vjust = -0.2)+
  ylim(0, max(cabbage_exp$Weight) * 1.05)

#Asigne las posiciones y ligeramente por 
#encima de la parte superior de la barra;
#el rango y del gráfico se ajustará automáticamente

cabbage_exp %>% 
  ggplot(aes(
    x = interaction(Date, Cultivar),
    y =  Weight
  ))+
  geom_bar(stat = "identity")+
  geom_text(aes(
    y = Weight + 0.1, 
    label = Weight
  ))

cabbage_exp %>% 
  ggplot(aes(
    x = Date,
    y =  Weight, 
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           position = "dodge")+
  geom_text(aes(
    label = Weight
  ),
  vjust = 1.5, 
  colour = "white",
  position = position_dodge(.9), size = 3)

ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y = cumsum(Weight))

ce %>% 
  ggplot(aes(
    x  = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity")+
  geom_text(aes(
    y = label_y,
    label = Weight
  ),
  vjust = 1.5,
  colour = "white")


ce <- ddply(ce, "Date", transform, label_y = cumsum(Weight)-0.5 * Weight)

ce %>% 
  ggplot(aes(
    x  = Date,
    y = Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity")+
  geom_text(aes(
    y = label_y,
    label = Weight
  ),
  vjust = 1.5,
  colour = "white")

ce %>% 
  ggplot(aes(
    x = Date,
    y =  Weight,
    fill = Cultivar
  ))+
  geom_bar(stat = "identity",
           colour = "yellow")+
  geom_text(aes(
    y = label_y,
    label = paste(format(Weight, nsmall = 2), "kg")
  ), size = 4)+
  guides(fill =  guide_legend(reverse =TRUE))+
  scale_fill_brewer(palette = "Pastel4")


view(tophitters2001)
str(tophitters2001)

tophic <- tophitters2001[1:15,]

tophic %>% 
  ggplot(aes(
    x = avg,
    y = name
  ))+
  geom_point()

tophic[, c("name", "lg", "avg")]

tophic %>% 
  ggplot(aes(
    x = avg,
    y = reorder(name, avg)
  ))+
  geom_point(size = 3)+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60",
                                          linetype = "dashed"))

tophic %>% 
  ggplot(aes(
    x = reorder(name, avg),
    y = avg
  ))+
  geom_point(size = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60",
                                          linetype = "dashed"))


#Obtenga los nombres, ordenados primero por lg, luego por avg
nameorder <- tophic$name[order(tophic$lg, tophic$avg)]
view(nameorder)

#Convierta el nombre en un factor, con niveles en el orden de nameorder
tophic$name <- factor(tophic$name, levels = nameorder)
view(tophic)

tophic %>% 
  ggplot(aes(
    x = avg,
    y =  name
  ))+
  geom_segment(aes(yend = name),
               xend = 0,
               colour = "gray50")+
  geom_point(size = 3,
             aes(colour = lg))+
  scale_color_brewer(palette = "Set1",
                     limits = c("NL", "AL"))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))


tophic %>% 
  ggplot(aes(
    x = avg,
    y = name
  ))+
  geom_segment(aes(
    yend = name
  ),
  xend = 0,
  colour = "grey50")+
  geom_point(size = 3, 
             aes(
               colour = lg
             ))+
  scale_color_brewer(palette = "Set3",
                     limits = c("NL", "AL"), 
                     guide =FALSE)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales="free_y", space="free_y")


















