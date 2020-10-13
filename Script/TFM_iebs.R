# https://rdrr.io/cran/warbleR/f/vignettes/Intro_to_warbleR.Rmd

rm(list = ls()) # Limpiar la memoria

#NOTE: functions are being renamed (run 'print(new_function_names)' to see new names). Both old and new names are available in this version 
#Please see citation('warbleR') for use in publication
#install.packages("seewave")

# INSTALAR LIBRERIAS
#---------------------------------------------------------------------------------------
install.packages("rmarkdown")

library(cluster) # daisy
library(tuneR)
library(knitr)
library(NatureSounds)
library(seewave)
library(warbleR) 
library(igraph) 

# CARGAR LOS DATOS
#-------------------------------------------------------------------------------------

# Data frame con la muestra de referencia que utilizaremos como plantilla:

plantilla <- data.frame(sound.files = "X_pitch-02.wav",
                        selec = 2, # Seleccionaremos la segunda muestra
                        channel = 1, 
                        start = 0,
                        end = 0.2,
                        stringsAsFactors = FALSE)

# creamos un objeto wav mediante el dataframe plantilla 
wave <- read_wave(plantilla, from = plantilla$start[1], to = plantilla$end[1])


low_freq = 0                    # limites del filtro pasabanda (sin  limite) 
high_freq= 20

# PLOT
# muestra del analisis espectral de la muestra de referencia
spectro(wave,
        wl = 512,
        # zp = TRUE,
        # ovlp = 30,
        # scale = TRUE,
        # osc = TRUE,
        # #flim = c(low_freq, high_freq ),
        # noisereduction = TRUE,
        # fastdisp = TRUE,
        # dB = 'max0',
        # flog = TRUE,
        # grid = TRUE,
        # collevels=seq(-115,0,10),
        # fftw = TRUE,
)

# Creación de un nuevo dataframe con todos los archivos de sonido en nuestro espacio de trabajo 
# con los que vamos a trabajar.

sel_tab_1 <- selection_table(whole.recs = TRUE)

# añadimos la plantilla inicial al dataframe anterior 
# Nota:(Ahora tenemos el archivo de referencia repetido)

sel_tab <- rbind(plantilla, as.data.frame(sel_tab_1))

# Establecemos la comparación de las muestras entre si:
comp_matrix <- cbind(paste(sel_tab$sound.files[1],
                           sel_tab$selec[1], sep = "-"),
                     sel_tab$sound.files)





# xcorr estima la similitud de dos ondas sonoras por medio de la correlación cruzada tiempo-frecuencia
# https://www.youtube.com/watch?v=sfsIhX2lQxw

#  ("lista") que contiene:
# 1) la matriz de correlación 
# 2) un marco de datos con valores de correlación en cada paso deslizante para cada comparación. 

# La lista, que también es de la clase 'xcorr.output', puede utilizarse para encontrar picos de detección con find_peaks o 
# para explorar gráficamente las detecciones utilizando lspe

# https://www.youtube.com/watch?v=sfsIhX2lQxw

xc_output <- xcorr(X = sel_tab,
           output = 'list',
           compare.matrix = comp_matrix,
           bp = c(low_freq, high_freq),
           na.rm = TRUE)

# la salida es una lista

matriz_principal <- data.frame(xc_output$scores$sound.files, 
                               xc_output$scores$score) # conversion de matriz a df # sel_tab$sound.files

# Esta es la matríz de correlación cruzada tiempo-frecuencia
xcor <- xcorr(X = sel_tab_1,
              bp = c(low_freq, 
                     high_freq), 
              wl = 512,
              ovlp = 99,
              path = NULL,
              type = "mfcc", 
              na.rm = TRUE)



# EXPLORAMOS DATOS
#---------------------------------------------------------------------------------------
xcor


# DEFINIMOS EL MODELO
#--------------------------------------------------------------------------------       -------

# Ejecutar una matriz simétrica completa que contiene las disimilitudes 

Dissimilarity <- daisy(xcor)


# Ejecutar el escalado multidimensional (CMS)
modelo <- cmdscale(Dissimilarity)

# Visualización del modelo resultante
modelo

# Calcular las distancias entre elementos
distancia <- dist(xcor, method = "euclidean")

# EVALUACION EL MODELO
#-------------------------------------------------------------------------------------
# 1) Visualizacion en diagrama de Boxplot entre las muestras
# 2) Visualizacion en diagrama de dispersion
# 3) Visualizacion en grafo


# 1) Visualizacion en diagrama de Boxplot entre las muestras
#-------------------------------------------------------------------------------------
graphics.off()
old.par <- par(mfrow=c(1, 2))
plot(matriz_principal, type = "d", xlab = "Coord.1", ylab = "Coord.2")
#text(matriz_principal[,1], matriz_principal[,2], labels = rownames(matriz_principal)) 

# 2) Visualizacion en diagrama de dispersion
#-------------------------------------------------------------------------------------


plot(xcor, type = "p", xlab = "Coord.1", ylab = "Coord.2")
#text(xcor[,1], xcor[,2], labels = rownames(xcor)) 
par(old.par)

# Las dos primeras coordenadas principales (valores propios) son:
valores <- cmdscale(distancia, eig = T)

#Valores propios calculados
valores$eig

# Determinar el ajuste
valores$GOF

# Definir un grafo de tamaño 5 para poder incluir todos los elementos
grafala <- graph.tree(ncol(xcor),mode = c("out"))



# Dar matriz_principal a los vertices del grafo con el matriz_principal de los archivos de audio
#colnames(xcor)
V(grafala)$label <- colnames(xcor)


layout <- layout.mds(grafala, dist = as.matrix(distancia))

graphics.off()

#plot(grafala, vertex.size = .1)
plot(grafala, type ="b", layout = layout, vertex.size = 0.5)





citation("pkgname")



