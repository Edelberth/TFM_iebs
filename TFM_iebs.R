# https://rdrr.io/cran/warbleR/f/vignettes/Intro_to_warbleR.Rmd

rm(list = ls()) # Limpiar la memoria

#NOTE: functions are being renamed (run 'print(new_function_names)' to see new names). Both old and new names are available in this version 
#Please see citation('warbleR') for use in publication
#install.packages("seewave")
#install.packages("NatureSounds")

#load packages
library(tuneR)
library(knitr)
library(NatureSounds)
library(seewave)
library(warbleR)

library(igraph)  # Cargar el conjunto de datos a utilizar

#library(audio)


setwd('/home/ion/TFM/X/')/home/ion/TFM/X



#warbleR_options(wav.path = '/home/ion/TFM/X/') 

# construimos un data frame a modo de plantilla con la muestra de referencia:

plantilla <- data.frame(sound.files = "X_pitch-17.wav",
                        selec = 2, # Seleccionaremos la segunda muestra
                        channel = 1, 
                        start = 0,
                        end = .5,
                        stringsAsFactors = FALSE)

# cargamos la muestra mediante el dataframe plantilla 
wave <- read_wave(plantilla)


low_freq = 0                    # defino los limites del filtro pasabanda 
high_freq= 20

# plot
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



# Creación de un nuevo dataframe con los archivos de sonido en nuestro espacio de trabajo con los que vamos a trabajar
sel_tab_1 <- selection_table(whole.recs = TRUE)

# object of class 'selection_table' 
# contains a selection table data frame with 17 rows and 5 columns: 
#         sound.files selec channel start       end
# 1 X_pitch-01.wav     1       1     0 0.2952381
# 2 X_pitch-02.wav     1       1     0 0.2952381
# 3 X_pitch-03.wav     1       1     0 0.2952381
# 4 X_pitch-04.wav     1       1     0 0.2952381
# 5 X_pitch-05.wav     1       1     0 0.2952381
# 6 X_pitch-06.wav     1       1     0 0.2952381
# ... and 11 more rows 

# añadir la plantilla inicial al dataframe anterior (Ahora tenemos ek archivo de referencia repetido)
sel_tab <- rbind(plantilla, as.data.frame(sel_tab_1))

# sound.files selec channel start       end
# 1  X_pitch-17.wav     2       1     0 0.5000000
# 2  X_pitch-01.wav     1       1     0 0.2952381
# 3  X_pitch-02.wav     1       1     0 0.2952381
# 4  X_pitch-03.wav     1       1     0 0.2952381
# 5  X_pitch-04.wav     1       1     0 0.2952381
# 6  X_pitch-05.wav     1       1     0 0.2952381
# 7  X_pitch-06.wav     1       1     0 0.2952381
# 8  X_pitch-07.wav     1       1     0 0.2952381
# 9  X_pitch-08.wav     1       1     0 0.2952381
# 10 X_pitch-09.wav     1       1     0 0.2952381
# 11 X_pitch-10.wav     1       1     0 0.2952381
# 12 X_pitch-11.wav     1       1     0 0.2952381
# 13 X_pitch-12.wav     1       1     0 0.2952381
# 14 X_pitch-13.wav     1       1     0 0.2952381
# 15 X_pitch-14.wav     1       1     0 0.2952381
# 16 X_pitch-15.wav     1       1     0 0.2952381
# 17 X_pitch-16.wav     1       1     0 0.2952381
# 18 X_pitch-17.wav     1       1     0 0.2952381

# Establecemos como vamos a comparar las muestras entre si
comp_matrix <- cbind(paste(sel_tab$sound.files[1],
                           sel_tab$selec[1], sep = "-"),
                     sel_tab$sound.files)

# [,1]               [,2]            
# [1,] "X_pitch-17.wav-2" "X_pitch-17.wav"
# [2,] "X_pitch-17.wav-2" "X_pitch-01.wav"
# [3,] "X_pitch-17.wav-2" "X_pitch-02.wav"
# [4,] "X_pitch-17.wav-2" "X_pitch-03.wav"
# [5,] "X_pitch-17.wav-2" "X_pitch-04.wav"
# [6,] "X_pitch-17.wav-2" "X_pitch-05.wav"
# [7,] "X_pitch-17.wav-2" "X_pitch-06.wav"
# [8,] "X_pitch-17.wav-2" "X_pitch-07.wav"
# [9,] "X_pitch-17.wav-2" "X_pitch-08.wav"
# [10,] "X_pitch-17.wav-2" "X_pitch-09.wav"
# [11,] "X_pitch-17.wav-2" "X_pitch-10.wav"
# [12,] "X_pitch-17.wav-2" "X_pitch-11.wav"
# [13,] "X_pitch-17.wav-2" "X_pitch-12.wav"
# [14,] "X_pitch-17.wav-2" "X_pitch-13.wav"
# [15,] "X_pitch-17.wav-2" "X_pitch-14.wav"
# [16,] "X_pitch-17.wav-2" "X_pitch-15.wav"
# [17,] "X_pitch-17.wav-2" "X_pitch-16.wav"
# [18,] "X_pitch-17.wav-2" "X_pitch-17.wav"


# xcorr estima la similitud de dos ondas sonoras por medio de la correlación cruzada tiempo-frecuencia

#  ("lista") que contiene:
# 1) la matriz de correlación 
# 2) un marco de datos con valores de correlación en cada paso deslizante para cada comparación. 

# La lista, que también es de la clase 'xcorr.output', puede utilizarse para encontrar picos de detección con find_peaks o 
# para explorar gráficamente las detecciones utilizando lspe

xc_output <- xcorr(X = sel_tab,
                   output = 'list', 
                   compare.matrix = comp_matrix, 
                   bp = c(low_freq, high_freq), 
                   na.rm = TRUE)
# 
# $max.xcorr.matrix
# [1] X1    X2    score
# <0 rows> (or 0-length row.names)
# 
# $scores
# dyad      sound.files                  template     time     score
# 1  X_pitch-17.wav-2/X_pitch-17.wav-whole.file X_pitch-17.wav-2 X_pitch-17.wav-whole.file 0.147619 1.0000000
# 2  X_pitch-17.wav-2/X_pitch-01.wav-whole.file X_pitch-17.wav-2 X_pitch-01.wav-whole.file 0.147619 0.7159771
# 3  X_pitch-17.wav-2/X_pitch-02.wav-whole.file X_pitch-17.wav-2 X_pitch-02.wav-whole.file 0.147619 0.7214788
# 4  X_pitch-17.wav-2/X_pitch-03.wav-whole.file X_pitch-17.wav-2 X_pitch-03.wav-whole.file 0.147619 0.7255659
# 5  X_pitch-17.wav-2/X_pitch-04.wav-whole.file X_pitch-17.wav-2 X_pitch-04.wav-whole.file 0.147619 0.7307239
# 6  X_pitch-17.wav-2/X_pitch-05.wav-whole.file X_pitch-17.wav-2 X_pitch-05.wav-whole.file 0.147619 0.7455057


# $selection.table
# sound.files      selec channel start       end              selection.id
# 1  X_pitch-17.wav          2       1     0 0.5000000          X_pitch-17.wav-2
# 2  X_pitch-17.wav whole.file       1     0 0.2952381 X_pitch-17.wav-whole.file
# 3  X_pitch-01.wav whole.file       1     0 0.2952381 X_pitch-01.wav-whole.file
# 4  X_pitch-02.wav whole.file       1     0 0.2952381 X_pitch-02.wav-whole.file
# 5  X_pitch-03.wav whole.file       1     0 0.2952381 X_pitch-03.wav-whole.file
# 6  X_pitch-04.wav whole.file       1     0 0.2952381 X_pitch-04.wav-whole.file
# 
# $hop.size.ms
# [1] 86.13281
# 
# $errors
# [1] dyad        sound.files template    time       
# <0 rows> (or 0-length row.names)
# 
# attr(,"class")
# [1] "list"         "xcorr.output"

# accedemos a los parametros dentro de la lista xc_output: nombre y score 
# y creamos la matriz_principal

matriz_principal <- data.frame(xc_output$scores$sound.files, 
                               xc_output$scores$score) # conversion de matriz a df # sel_tab$sound.files

xcor <- xcorr(X = sel_tab_1,
              bp = c(low_freq, high_freq), 
              wl = 512,
              ovlp = 90,
              path = NULL,
              type = "mfcc", 
              na.rm = TRUE)

# comp_matrix <- as.data.frame(comp_matrix)
# 
#                                     # V1          V2
#                                     # 1 test-01.wav-2 test-01.wav
#                                     # 2 test-01.wav-2 test-01.wav
#                                     # 3 test-01.wav-2 test-02.wav
#                                     # 4 test-01.wav-2 test-03.wav
#                                     # 5 test-01.wav-2 test-04.wav

# xcor <- xcorr(X = comp_matrix, 
#               wl = 512,
#               bp = c(0,15), 
#               ovlp = 90,
#               )

#ip = as.data.frame(installed.packages()[,c(1,3:4)])
#ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
#ip

# EXPLORAR DATOS
#---------------------------------------------------------------------------------------
xcor
dim(xcor)

# DEFINIMOS EL MODELO
#---------------------------------------------------------------------------------------

#Ejecutar el escalado multidimensional
#modelo <- cmdscale(xcor)

# Visualización del modelo resultante
#modelo

# Calcular las distancias entre elementos
distancia <- dist(xcor, method = "euclidean")
class(distancia)
dim(distancia)

#Visualizar los objetos como puntos de un mapa de dimension dos
graphics.off()
plot(xcor, type = "b", xlab = "Coord.1", ylab = "Coord.2")
text(xcor[,1], xcor[,2], labels = rownames(xcor)) 

# Las dos primeras coordenadas principales (valores propios) son:
valores <- cmdscale(distancia, eig = T)

#Valores propios calculados
valores$eig

# Determinar el ajuste
valores$GOF

# Definir un grafo de tamaño 5 para poder incluir todos los elementos
grafo <- graph.full(ncol(xcor))

grafala <- graph.tree(ncol(xcor),mode = c("out"))


# Dar matriz_principal a los vertices del grafo con el matriz_principal de los archivos de audio
#colnames(xcor)
V(grafo)$label <- colnames(xcor)

V(grafala)$label <- colnames(xcor)



layout <- layout.mds(grafo, dist = as.matrix(distancia))
layout <- layout.mds(grafala, dist = as.matrix(distancia))


graphics.off()
plot(grafo,  layout = layout, vertex.size = 0.5)

plot(grafala, vertex.size = .1)
plot(grafala,  layout = layout, vertex.size = 0.5)

vertex_attr_names(grafala)











