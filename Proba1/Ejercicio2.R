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
png('barplot1.png', width = 4000, height = 2500, res = 300)
barplot(table(palabras))
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

###################
#Grafica final P&p
#################
png('barplot5.png', width = 5000, height = 2500, res = 300)
par(mfrow=c(2,2))
barplot(table(palabras), main =  "a)")
barplot(sort(table(letras$chars), descending = TRUE), main = "b)")
barplot(sort(table(palabras$words), decreasing  = TRUE), main = "c)")
barplot(sort(table(palabras$words), decreasing  = TRUE), log = 'y', main = "d)")
dev.off() 

###################
#Grafica final S&S
#################
png('barplot5_sense.png', width = 5000, height = 2500, res = 300)
par(mfrow=c(2,2))
barplot(table(palabras_sense), main =  "a)")
barplot(sort(table(letras_sense$chars), descending = TRUE), main = "b)")
barplot(sort(table(palabras_sense$words), decreasing  = TRUE), main = "c)")
barplot(sort(table(palabras_sense$words), decreasing  = TRUE), log = 'y', main = "d)")
dev.off() 


fpal = as.data.frame(table(palabras$words))
flmu = fpal[fpal$Freq > 500,]
png('barplot6.png', width = 5000, height = 2500, res = 300)
barplot(flmu$Freq, names.arg = flmu$Var1)
dev.off() 

fpal_sense = as.data.frame(table(palabras_sense$words))


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

