#install.packages("ggplot2")
#install.packages("gcookbook")
#install.packages("xlsx")
library(tidyverse)
library(ggplot2)
library(gcookbook)

#######################################
#2.1 Crear un diagrama de dispersión
#2.1 Creating a Scatter Plot
######################################

View(mtcars)
head(mtcars)
tail(mtcars)
str(mtcars)

attach(mtcars)

plot(wt, mpg)
#ggplot2
qplot(wt, mpg)
#ggplot
mtcars %>% 
ggplot(aes(
  x = wt,
  y = mpg
  ))+
    geom_point()

###################################  
#2.2 crer un grafico de lineas
#2.2 Creating a Line Graph
##################################

view(pressure)
head(pressure)
tail(pressure)
str(pressure)
attach(pressure)

plot(temperature, pressure, type = "l")
points(temperature, pressure)
lines(temperature, pressure/2, col = "red")
points(temperature, pressure/2, col ="red")

#ggplot2
qplot(temperature, pressure, geom = c("line", "point"))

#ggplot
detach(pressure)
ggplot(data = pressure, 
       aes(
         x=temperature,
         y=pressure))+
  geom_line() + 
  geom_point()

###################################
#2.3 Crear un grafico de barras
#2.3 Creating a Bar Graph
##################################

view(BOD)
head(BOD)
tail(BOD)
str(BOD)

barplot(BOD$demand, names.arg = BOD$Time)

class(cyl)
table(cyl)
barplot(table(cyl))

#ggplot2
#____________________________________________#
# NO FUNCIONA POR QUE STAT ESTA DESCONTINUADO
#___________________________________________#

qplot(BOD$Time, BOD$demand, geom="bar", stat="identity")
qplot(Time, demand, data=BOD, geom="bar", stat="identity")
qplot(factor(BOD$Time), BOD$demand, geom="bar", stat="identity")


view(cyl)
#variable continua
qplot(cyl)

#variable discreta
qplot(factor(cyl))
qplot(factor(cyl), data = mtcars)

#ggplot
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(data = mtcars,
       aes(
         x = factor(cyl)
       ))+
  geom_bar()

ggplot(data = mtcars,
       aes(
         x = cyl
       ))+
  geom_bar()

###############################
#2.4 creating a Histogram
#2.4 graficar un histograma
##############################

hist(mpg)

#Especifique el número aproximado de contenedores con descansos
hist(mpg, breaks = 10)

#ggplot2
qplot(mpg)
qplot(mpg, data = mtcars, binwidth = 4)

#ggplot
ggplot(data = mtcars,
       aes(
         x = mpg
       ))+
  geom_histogram(binwidth = 4)

#####################################
#2.5 creating a box plot
#2.5 graficar un diagrama de caja
#####################################

view(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
tail(ToothGrowth)

attach(ToothGrowth)

plot(supp, len)
boxplot(len ~ supp, data = ToothGrowth)

#ggplot2
qplot(supp, len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
#usando 3 vectores separados
qplot(interaction(supp, dose), len, geom = "boxplot")

#ggplot
ToothGrowth %>% 
  ggplot(aes(
    x = supp,
    y = len
  ))+
  geom_boxplot()

ToothGrowth %>% 
  ggplot(aes(
    x = interaction(supp, dose),
    y = len
  ))+
  geom_boxplot()

#####################################
#2.6 Plotting a function curve
#2.6 Trazar una función curva
#####################################

curve(x^3 - 5*x, from = -4, to = 4)

myfun <- function(x){
  1/(1 + exp(-x + 10))
}
curve(myfun(x), from = 0, to = 20)
curve(1 - myfun(x), add = TRUE, col = "red")

#ggplot2
#____________________________________________#
# NO FUNCIONA POR QUE STAT ESTA DESCONTINUADO
#___________________________________________#
qplot(c(0,20), fun=myfun, stat="function", geom="line")


#ggplot
ggplot(data.frame(x = c(0,20)),
       aes(
         x = x
       ))+
  stat_function(fun = myfun, geom = "line")


