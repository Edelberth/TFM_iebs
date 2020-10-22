
library(cluster) # dadisy
library(tuneR)
library(knitr)
library(NatureSounds)
library(seewave)
library(warbleR) 
library(igraph) # hacer mencion al final no lo he hecho




#load data
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4","lbh_selec_table"))

#save sound files
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) 
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# run cross correlation on spectrograms (SPCC)
xcor <- xcorr(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir())

# run cross correlation on Mel cepstral coefficients (mfccs)
xcor <- xcorr(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir(), type = "mfcc")

# using the 'compare.matrix' argument to specify pairwise comparisons
# create matrix with ID of signals to compare
cmp.mt <- cbind(
  paste(lbh_selec_table$sound.files[1:10], lbh_selec_table$selec[1:10], sep = "-"), 
  paste(lbh_selec_table$sound.files[2:11], lbh_selec_table$selec[2:11], sep = "-"))

# run cross-correlation on the selected pairwise comparisongs
xcor <- xcorr(X = lbh_selec_table, compare.matrix = cmp.mt, 
              wl = 300, ovlp = 90, path = tempdir())

#------------------------------------------------------------------------------------------------------------------------------
todas_las_muestras <- selection_table(whole.recs = TRUE)


# Leemos el nombre de las muestras que hay en el directorio de trabajo
sample_WorkSpace <- selection_table(whole.recs = TRUE, extended = TRUE)


# measures acoustic parameters on acoustic signals for which the start and end times are provided
sample_acoust_param <- na.omit(specan(sample_WorkSpace, wl = 512, fsmooth = 0.1, threshold = 10, wn = "hanning",
             flim = c(0, 22), bp = c(0,20), fast.spec = FALSE, ovlp = 50, pal = reverse.gray.colors.2,
             widths = c(2, 1), main = NULL, plot = TRUE, all.detec = FALSE)) # specan measures acoustic parameters on acoustic signals for which the start and end times are provided.

sample_acoust_param_back <- sample_acoust_param

# readWave

tdir <- getwd()                                       # nombre espacio de trabajo
wav_names <- list.files(tdir, pattern = "\\.wav$")    # nombres de los archivos en el espacio de trabajo
cantidad_wav_names <- length(wav_names)



vect <- list("X","Y",c())

rWn <- c()
HIT = data.frame("wav_names"= NULL,"bottom.freq" = NULL, "top.freq" = NULL )

for (i in 1:cantidad_wav_names) {
  rWn <- readWave(print(wav_names[i]))
  vect <- append(vect, frange.detec(rWn)) #vect <- append(vect, c(frange.detec(rWn)))
}

for (i in 1:cantidad_wav_names) {
  
}




HIT[1,i] = data.frame("wav_names"= wav_names[i],"bottom.freq" = vect$bottom.freq[i],"top.freq" = vect$top.freq[i])


for (i in 1:cantidad_wav_names) {
  #vector_Nombres <- append(wav_names, print(i))   ... "X_pitch-16.wav" "X_pitch-17.wav" "17" 

  vect <- append(vect, c(frange.detec(RNW)))
}

for (i in 1:cantidad_wav_names) {
  vect <- vect
}


# Create a list containing strings, numbers, vectors and a logical
# values.
list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)

# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

# Show the list.
print(list_data)



























HIT[i,3] <- c(frange.detec(WN), plot = FALSE)


HIT[17,2]





for (i in 1:cantidad_wav_names) {
  #vector_Nombres <- append(wav_names, print(i))   ... "X_pitch-16.wav" "X_pitch-17.wav" "17" 
  vector_top_button <- append(vector_top_button, c(frange.detec(readWave(print(wav_names[i])))))
}

for (i in 1:cantidad_wav_names) {
  #vector_Nombres <- append(wav_names, print(i))   ... "X_pitch-16.wav" "X_pitch-17.wav" "17" 
  vector_top_button <- append(vector_top_button, frange.detec(readWave(print(wav_names[i])), plot = FALSE) )
}

vector_top_button <- append(vector_top_button, frange.detec(readWave(print(wav_names[17])), plot = FALSE) )




HIT <- data.frame(wav_names, vector_top_button)



HIT <- rbind(HIT, as.data.frame(vector_top_button$bottom.freq), as.data.frame(vector_top_button$top.freq))




frange.detec(readWave(print(wav_names[17])))







warnings()





















HIT <- data.frame(c(wav_names), c(frange.detec(readWave(print(wav_names[3])))), all.detec = TRUE)




newObjeto <- readWave(print(wav_names[1]))

HIT <- frange.detec((newObjeto))


as.data



describe()

list2env()



data(tico)
frange.detec(wave = tico, wl = 512, fsmooth = 0.01, threshold = 1, bp = c(2, 8),
             widths = c(4, 2))

HIT <- readWave(print(wav_names[2]))

# intentar acceder a la list como en el data frame pero con varios de estos X$xx$xxx <- asigno.


#old.par <- par(mfrow=c(2, 2))  # 

for (i in 1:cantidad_wav_names) {
  HIT[1] <- list ( wav_names[i] <- c(frange.detec(readWave(print(wav_names[1])))))
}

HIT <- list()
HIT[1] <- list ( wav_names = c(frange.detec(readWave(print(wav_names[1])))))


#sample_acoust_param_back$
vect_doble <- data.frame()
for (i in 1:cantidad_wav_names) {
  vect_doble[i] <- c(wav_names)
  #vect_doble$bottom.freq[i] <- c(frange.detec(readWave(print(wav_names[i]))))
  #vect_doble$top.freq[i]    <- vv[c(frange.detec(readWave(print(wav_names[i]))))]
}











HIT <- readWave("Hit.wav")

HIT <- frange.detec(HIT)





vect_doble$bottom.freq
vect_doble$top.freq

vect_doble <- as.data.frame()

tdir <- getwd()   
wav_names <- list.files(tdir, pattern = "\\.wav$")
cantidad_wav_names <- length(wav_names)

for (i in 1:cantidad_wav_names) {
  
  vect_doble$ <- c(frange.detec(readWave(print(wav_names[i]))))
  vect_doble$tope <- c(frange.detec(readWave(print(wav_names[i]))))
}

vect_doble <- as.data.frame(wav_names)













vect_doble[2] <- frange.detec(readWave(print(wav_names[2])))
HIT$top.freq <- frange.detec(readWave(print(wav_names[2])))



HIT[2] <- frange.detec(readWave(print(wav_names[2])))
HIT <- frange.detec(readWave(print(wav_names[3])))
HIT <- º(readWave(print(wav_names[4])))
text(xcor[,1],xcor[,2], labels = rownames(xcor), pos = 4, cex = 0.8 )


sample_acoust_param_back$
  
  
  
 



#paste("A", "wof", sep = ".")



sel_tab_1 <- selection_table(whole.recs = TRUE)

HIT <- readWave(as.character(c(sel_tab_1)))
 


for (i in c(wav_names)){
  
  waves_obj <- as.data.frame(sound.files = c(wav_names) )
  
  #sample_WorkSpace[1] <- readWave(sample_WorkSpace$)
  #sample_range_f <- as.data.frame(frange.detec(sample_WorkSpace$sound.files[i]))
}





newWobj <- readWave(tfile)
newWobj
file.remove(tfile)

plantilla <- data.frame(sound.files = "X_pitch-02.wav",
                        selec = 2, 
                        channel = 1, 
                        start = 0,
                        end = 2,
                        stringsAsFactors = FALSE)
plantilla






# detects the frequency range of acoustic signals on wave objects.
sample_range_f <- as.data.frame(frange.detec(sample_WorkSpace))



long_WorkSpace = nrow(sample_WorkSpace)




readWave(sample_WorkSpace$sound.files[i])







# convertimos el nombre de las columnas que hay en el dataframe al vector
wav_names <- list.files(tdir, pattern = "\\.wav$")
c(vec_names)
























frange(sample_WorkSpace, wl = 512, )
manualoc(wl = 1000, path = getwd())






