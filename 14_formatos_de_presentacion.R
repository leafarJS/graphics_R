library(tidyverse)
library(ggplot2)
library(gcookbook)
library(plyr)


# Salida para presentación
# En términos generales, las visualizaciones de datos tienen dos propósitos: 
# descubrimiento y comunicación. En la fase de descubrimiento, creará gráficos
# exploratorios y, cuando lo haga, es importante poder probar diferentes cosas 
# rápidamente. En la fase de comunicación, presentará sus gráficos a los demás. 
# Cuando hagas eso, necesitarás ajustar la aplicación, apariencia de los gráficos
# y, por lo general, deberá colocarlos en otro lugar que no sea la pantalla de su
# computadora. Aqui se trata sobre la última parte: guardar sus gráficos para que
# puedan presentarse en documentos


#########################################
#14.1. Outputting to PDF Vector Files
#14.1. Salida a archivos vectoriales PDF
#########################################


# Hay dos formas de generar archivos PDF. Un método es abrir el dispositivo de 
# gráficos PDF con pdf(), hacer los gráficos y luego cerrar el dispositivo con 
# dev.off(). Este método funciona para la mayoría de los gráficos en R, incluidos
# los gráficos básicos y los gráficos basados en cuadrículas como los creados por
# ggplot2 y lattice.


#width and height are in inches
pdf("plot_1.pdf", width = 4, height = 4)

#make plots
plot(mtcars$wt, mtcars$mpg)
print(mtcars %>% 
        ggplot(aes(
          x = wt,
          y = mpg
        ))+
        geom_point())
dev.off()

# Si crea más de un diagrama, cada uno irá en una página separada en la salida 
# del PDF. Tenga en cuenta que llamamos a print() en el objeto ggplot para 
# asegurarnos de que se generará incluso cuando este código esté en un script.
# El ancho y el alto están en pulgadas, por lo que para especificar las dimensiones
# en centímetros, debes hacer la conversión manualmente:

# 8 x 8 cm
pdf("plot_2.pdf", width = 8/2.54, height = 8/2.54)
mtcars %>% 
  ggplot(aes(
    x = wt, 
    y = mpg
  ))+
  geom_point()
  ggsave("plot_2.pdf", width = 8, height = 8, units = "cm")
  
# Los archivos PDF suelen ser la mejor opción cuando su objetivo es generar   
# documentos impresos. Funcionan fácilmente con LaTeX y se pueden usar en 
# presentaciones con Keynote de Apple, pero los programas de Microsoft pueden 
# tener problemas para importarlos. Los archivos PDF también suelen ser más pequeños que los archivos
# de mapa de bits, como los archivos de gráficos de red portátiles (PNG), porque 
# contienen un conjunto de instrucciones, como "Dibujar una línea de aquí para 
# allá”, en lugar de información sobre el color de cada píxel. Sin embargo, hay
# casos en los que los archivos de mapa de bits son más pequeños. Por ejemplo, 
# si tiene un diagrama de dispersión que está muy sobretrazado, un archivo PDF 
# puede terminar siendo mucho más grande que un PNG; aunque la mayoría de los 
# puntos estén ocultos, el archivo PDF seguirá conteniendo instrucciones para 
# dibujar cada uno de ellos, cada punto, mientras que un archivo de mapa de bits
# no contendrá la información redundante. 
  

##########################################    
#14.2. Outputting to SVG Vector Files
#14.2. Salida a archivos vectoriales SVG
##########################################
  
win.metafile("plot_3.wmf", 
               width = 4,
               height = 4)
  mtcars %>% 
    ggplot(aes(
      x = wt, 
      y = mpg
    ))+
    geom_point()
  dev.off()
  
#with ggsave()
ggsave("plot_3.wmf",
         width = 8,
         height = 8,
         units = "cm")
  
# Los archivos WMF se pueden crear y usar de la misma manera que los archivos PDF,
# pero solo se pueden crear en Windows:

  
# Los programas de Windows, como Microsoft Word y PowerPoint, tienen poca 
# compatibilidad con la importación de archivos PDF, pero admiten de forma nativa
# WMF. Un inconveniente es que los archivos WMF.
# no admiten transparencia (alfa).    
  
#################################################  
#14.4. Editing a Vector Output File
#14.4. Edición de un archivo de salida vectorial
#################################################

pdf("plot_1.pdf", 
      width = 4, 
      height = 4,
      useDingbats = FALSE)    
  
#OR
ggsave("plot_1.pdf",
         width = 4,
         height = 4,
         useDingbats = FALSE)
  

#Para evitar este problema, configure useDingbats=FALSE. Esto hará que los 
#círculos se dibujen como círculos en lugar de como caracteres de fuente.


####################################################
#14.5. Outputting to Bitmap (PNG/TIFF) Files
#14.5. Salida a archivos de mapa de bits (PNG/TIFF)
####################################################

#width and height are in pixels
png("plot_4.png",
    width = 400,
    height = 400)

#make plot
plot(mtcars$wt,
     mtcars$mpg)
dev.off()


#width and height are in pixels
png("plot_5-%d.png",
    width = 400,
    height = 400)

#make plot
plot(mtcars$mpg,
     mtcars$wt)
print(mtcars %>% 
        ggplot(aes(
          x = wt,
          y = mpg
        )))+
  geom_point()
dev.off()


# Tenga en cuenta que llamamos a print() en el objeto ggplot para asegurarnos de
# que se generará incluso cuando este código esté en un script.
# El ancho y la altura están en píxeles, y el valor predeterminado es una salida 
# de 72 píxeles por pulgada (ppp). Esta resolución es adecuada para mostrarse en
# una pantalla, pero se verá pixelada y irregular en la impresión.

ppi <- 300
# Calculate the height and width (in pixels) for a 4x4-inch image at 300 ppi
png("plot_6.png",
    width = 4 * ppi,
    height = 4 * ppi,
    res = ppi)
plot(mtcars$wt,
     mtcars$mpg)
dev.off()

# Si está creando gráficos a partir de una secuencia de comandos y arroja un error
# al crear uno, es posible que R no llegue a la llamada a dev.off(), y podría quedar
# en un estado en el que el dispositivo PNG todavía está abierto. Cuando esto sucede,
# el archivo PNG no se abrirá correctamente en un programa de visualización hasta 
# que llame manualmente a dev.off(). Si está creando un gráfico con ggplot2, 
# usar ggsave() puede ser un poco más simple. Simplemente guarda el último gráfico 
# creado con ggplot(). Usted especifica el ancho y la altura en pulgadas, no en 
# píxeles, y le dice cuántos píxeles por pulgada usar:

mtcars %>% 
ggplot(aes(
  x = wt,
  y = mpg
  ))+
  geom_point()

# Default dimensions are in inches, but you can specify the unit
ggsave("plot_7.png", 
       width = 8,
       height = 8,
       unit = "cm",
       dpi = 300)

#Con ggsave(), no necesita imprimir el objeto ggplot, y si hay un error al crear
#o guardar el gráfico, no es necesario cerrar manualmente el dispositivo gráfico.



# R admite otros formatos de mapa de bits, como BMP, TIFF y JPEG, pero en realidad
# no hay muchas razones para usarlos en lugar de PNG. La apariencia exacta de los
# mapas de bits resultantes varía de una plataforma a otra. A diferencia del 
# dispositivo de salida de PDF de R, que se procesa de manera consistente en 
# todas las plataformas, los dispositivos de salida de mapa de bits pueden 
# representar el mismo gráfico de manera diferente en Windows, Linux y Mac OS X.
# Incluso puede haber variaciones dentro de cada uno de estos sistemas operativos.
# Las diferentes plataformas renderizarán las fuentes de manera diferente, algunas
# plataformas tendrán líneas antialias (suaves) mientras que otras no, y algunas 
# plataformas admiten alfa (transparencia) mientras que otras no. Si su plataforma
# no es compatible con características como antialiasing y alfa, puede usar el 
# dispositivo CairoPNG(), del paquete Cairo:


#install.packages("Cairo")
library(Cairo)

CairoPNG("plot_8.png")
plot(mtcars$mpg, mtcars$wt)
dev.off()

######################################
#14.6. Using Fonts in PDF Files
#14.6. Uso de fuentes en archivos PDF
######################################

#install.packages("extrafont")
library(extrafont)

# Busque y guarde información sobre las fuentes instaladas en su sistema
font_import()

fonts()

loadfonts()

# En Windows, es posible que deba indicarle dónde está instalado Ghostscript
# (ajuste la ruta para que coincida con su instalación de Ghostscript)
Sys.setenv(R_GSCMD = "C:\Program Files\gs\gs9.55.0\bin/gswin32c.exe")




#library(ggplot2)

  ggplot(mtcars, aes(
    x = wt,
    y = mpg
  ))+
  geom_point()+
  ggtitle("El texto del titulo va aqui")+
  theme(text = element_text(size = 16,
                            family = "Impact"))
ggsave("plot_9.pdf",
       width = 4,
       height = 4)

embed_fonts("plot_9.pdf")


######################################################################
#14.7. Using Fonts in Windows Bitmap or Screen Output
#14.7. Uso de fuentes en mapa de bits de Windows o salida de pantalla
######################################################################


install.packages("extrafont")
library(extrafont)

# Find and save information about fonts installed on your system
font_import()

# List the fonts
fonts()

library(extrafont)
# Register the fonts for Windows
loadfonts("win")


library(ggplot2)
ggplot(mtcars, 
       aes(
         x = wt,
         y = mpg))+
  geom_point()+
  ggtitle("Title text goes here") +
  theme(text = element_text(size = 16,
                            family="Georgia",
                            face="italic"))
ggsave("plot_10.png",
       width = 4,
       height = 4,
       dpi = 300)
