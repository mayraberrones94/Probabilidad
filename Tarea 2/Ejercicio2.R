#Lib
require(gutenbergr)
require(tidytext)
require(tm)
require(dplyr)
#require(wordcloud)
#require(wordcloud2)

austen = gutenberg_download(c(1342))
sense = gutenberg_download(c(161))

#Pride and prejudice
letras = austen %>% unnest_tokens(chars, text, "characters")
palabras = austen %>% unnest_tokens(words, text, "words")

#Sense and sensibility
letras_sense = sense %>% unnest_tokens(chars, text, "characters")
palabras_sense = sense %>% unnest_tokens(words, text, "words")

#######################
#Primeros Intentos de graficar
#######################

prueba1 =  as.data.frame(table(palabras))
p1 = prueba1[prueba1$Freq > 20,]
png('barplot1.png', width = 4000, height = 2500, res = 300)
barplot(p1$Freq, names.arg = p1$Var1)
dev.off() 

png('barplot2.png', width = 4000, height = 2500, res = 300)
barplot(sort(table(letras$chars), descending = TRUE))
dev.off()

png('barplot3.png', width = 4000, height = 2500, res = 300)
barplot(sort(table(palabras$words), decreasing  = TRUE))
dev.off()

png('barplot4.png', width = 4000, height = 2500, res = 300)
barplot(sort(table(palabras$words), decreasing  = TRUE), log = 'y')
dev.off() 

fpal_1 = as.data.frame(table(palabras$words))
flmu_1 = fpal_1[fpal_1$Freq > 20,]
mm_1 = flmu_1[order(flmu_1$Freq),]
png('barplot6.png', width = 5000, height = 2500, res = 300)
barplot(mm_1$Freq, names.arg = mm_1$Var1, log = "y", main = "d)")
dev.off() 

###################
#Grafica final P&p
#################
png('barplot5.png', width = 5000, height = 2500, res = 300)
par(mfrow=c(2,2))
barplot(table(palabras), main =  "a)")
barplot(mm$Freq, names.arg = mm$Var1, main = "b)")
#barplot(sort(table(letras$chars), descending = TRUE), main = "b)")
barplot(sort(table(palabras$words), decreasing  = FALSE), main = "c)")
barplot(mm_1$Freq, names.arg = mm_1$Var1, log = "y", main = "d)")
#barplot(sort(table(palabras$words), decreasing  = TRUE), log = 'y', main = "d)")
dev.off() 


###################
#Grafica final S&S
#################

fpals = as.data.frame(table(letras_sense$chars))
flmus = fpals[fpals$Freq > 20,]
mms = flmus[order(flmus$Freq),]
png('barplot6.png', width = 5000, height = 2500, res = 300)
barplot(mms$Freq, names.arg = mms$Var1)
dev.off() 

fpals_2 = as.data.frame(table(palabras_sense$words))
flmus_2 = fpals_2[fpals_2$Freq > 20,]
mms_2 = flmus_2[order(flmus_2$Freq),]
png('barplot6.png', width = 5000, height = 2500, res = 300)
barplot(mms_2$Freq, names.arg = mms_2$Var1, log = "y")
dev.off() 

png('barplot5_sense.png', width = 5000, height = 2500, res = 300)
par(mfrow=c(2,2))
barplot(table(palabras_sense), main =  "a)")
barplot(mms$Freq, names.arg = mms$Var1, main =  "b)")
#barplot(sort(table(letras_sense$chars), descending = TRUE), main = "b)")
barplot(sort(table(palabras_sense$words), decreasing  = FALSE), main = "c)")
barplot(mms_2$Freq, names.arg = mms_2$Var1, log = "y", main = "d)")
#barplot(sort(table(palabras_sense$words), decreasing  = TRUE), log = 'y', main = "d)")
dev.off() 


fpal = as.data.frame(table(letras$chars))
flmu = fpal[fpal$Freq > 20,]
mm = flmu[order(flmu$Freq),]
png('barplot6.png', width = 5000, height = 2500, res = 300)
barplot(mm$Freq, names.arg = mm$Var1)
dev.off() 

fpal_sense = as.data.frame(table(palabras_sense$words))


png('barplot51.png', width = 3000, height = 2500, res = 300)
par(mfrow=c(2,1))
barplot(table(palabras), main =  "a)")
barplot(table(palabras_sense), main =  "b)")
dev.off() 

png('barplot52.png', width = 3000, height = 2500, res = 300)
par(mfrow=c(2,1))
barplot(mm$Freq, names.arg = mm$Var1, main = "a)")
barplot(mms$Freq, names.arg = mms$Var1, main =  "b)")
dev.off() 

png('barplot53.png', width = 3300, height = 2500, res = 300)
par(mfrow=c(2,1))
barplot(mm_1$Freq, names.arg = mm_1$Var1, log = "y", main = "d)")
barplot(mms_2$Freq, names.arg = mms_2$Var1, log = "y", main = "d)")
dev.off() 


#Intento de sacar la informacion como csv 1
##############################
#No funciono, pero dejalo por si lo ocupas
###############################
write.csv(fpal,"/Users/mayraberrones/Documents/GitHub/Probabilidad/Proba1/austen_dat.csv", row.names = TRUE)
frec_1 = c(fpal$Freq)
frec_2 = c(fpal$Var1)
solo_palabras = c(palabras$words)
prueba = table(solo_palabras)
wo_duplicate = solo_palabras[!duplicated(solo_palabras) ] #This was funny xD

#############################
#Pride and Prejudice
###########################


nombres = c("Darcy", "Elizabeth", "Lydia", "Wickham", "Jane", "Bingley", "Lucas", "Collins", "Catherine", "Bennet", "Gardiner")
n_darcy = (fpal %>% filter(Var1 == "darcy"))$Freq
n_lizzy = (fpal %>% filter(Var1 == "elizabeth"))$Freq
n_jane = (fpal %>% filter(Var1 == "jane"))$Freq
n_wickham = (fpal %>% filter(Var1 == "wickham"))$Freq
n_bingley = (fpal %>% filter(Var1 == "bingley"))$Freq
n_lydia = (fpal %>% filter(Var1 == "lydia"))$Freq
n_lucas = (fpal %>% filter(Var1 == "lucas"))$Freq
n_collins = (fpal %>% filter(Var1 == "collins"))$Freq
n_catherine = (fpal %>% filter(Var1 == "catherine"))$Freq
n_gardiner = (fpal %>% filter(Var1 == "gardiner"))$Freq
n_bennet = (fpal %>% filter(Var1 == "bennet"))$Freq

colors_data = c("coral1", "cyan3", "deepskyblue2", "darkgoldenrod2", "darkgreen", "deeppink3")

todos = c(n_darcy, n_lizzy, n_lydia, n_wickham,n_jane, n_bingley, n_lucas, n_collins, n_catherine, n_bennet, n_gardiner)
png('barplot_pp.png', width = 5000, height = 2500, res = 300)
barplot(todos, names.arg = nombres, col = colors_data)
dev.off() 

###########
#Sense and Sensibbility
############

nombres_sense = c("Elinor", "Edward", "Marianne", "Brandon", "Willoughby", "Steele", "Jennings", "Dashwood")
n_elinor = (fpal_sense %>% filter(Var1 == "elinor"))$Freq
n_ferrars = (fpal_sense %>% filter(Var1 == "edward"))$Freq
n_marian = (fpal_sense %>% filter(Var1 == "marianne"))$Freq
n_brandon = (fpal_sense %>% filter(Var1 == "brandon"))$Freq
n_will = (fpal_sense %>% filter(Var1 == "willoughby"))$Freq
n_stele = (fpal_sense %>% filter(Var1 == "steele"))$Freq
n_jenn = (fpal_sense %>% filter(Var1 == "jennings"))$Freq
n_dash = (fpal_sense %>% filter(Var1 == "dashwood"))$Freq

colors_data2 = c("coral1", "cyan3", "deepskyblue2", "darkgoldenrod2", "darkgreen", "deeppink3")

todos_sense = c(n_elinor, n_ferrars, n_marian, n_brandon,n_will, n_stele, n_jenn, n_dash)
png('barplot_ss2.png', width = 5000, height = 2500, res = 300)
barplot(todos_sense, names.arg = nombres_sense, col = colors_data2)
dev.off() 





