require(gutenbergr)
require(tidytext)
require(tm)
require(dplyr)

##################
#Descargar el archivo de austen y bronte
################

austen = gutenberg_download(c(1342))
bronte = gutenberg_download(c(768))


letra_austen= austen %>% unnest_tokens(chars, text, "characters")
palabra_austen = austen %>% unnest_tokens(word, text, "words")

letra_bronte = bronte %>% unnest_tokens(chars, text, "characters")
palabra_bronte = bronte %>% unnest_tokens(word, text, "words")

p_letras =  as.data.frame(table(letra_austen))
p4 = p_letras[p_letras$Freq > 20,]

#########
#Prueba de sentimiento
##########

get_sentiments("nrc")

nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
palabra_austen %>% inner_join(nrc_joy) %>% count(word, sort = TRUE)
palabra_bronte %>% inner_join(nrc_joy) %>% count(word, sort = TRUE)

get_sentiments("bing")
word_counts <- palabra_austen  %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup()

#######
#Experimentos tidy

tidy_bronte <- bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)
no = tidy_bronte %>% count(word, sort = TRUE)

tidy_austen <- austen %>% unnest_tokens(word, text) %>% anti_join(stop_words)
number = tidy_austen %>% count(word, sort = TRUE)
p3 = number[number$n >50,]
p32 = no[no$n > 50,]


 ###########
#Experimentos con Histogramas
############
png('histo_austen.png', width = 3000, height = 3100, res = 300)
h = hist(p3$n,
  main="",
  ylab = "Number of words",
  xlab="Frequency of words",
xlim=c(0,600)
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
dev.off()

png('histo_bronte.png', width = 3000, height = 3100, res = 300)
h2 = hist(p32$n,
         main="",
         ylab = "Number of words",
         xlab="Frequency of words",
         xlim=c(0,500), 
         ylim = c(0, 50)
)
text(h2$mids,h2$counts,labels=h2$counts, adj=c(0.5, -0.5))
dev.off()
#############################
#############################


##
#Graficas para frecuencia de palabras (todas)
library(ggplot2)

png('Ej3.png', width = 2000, height = 4000, res = 300)
palabra_austen %>% count(word, sort = TRUE) %>% filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() + 
  xlab(NULL) +
  coord_flip()
dev.off()

png('tidy_austen.png', width = 2000, height = 4000, res = 300)
tidy_austen %>% count(word, sort = TRUE) %>% filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() + 
  xlab(NULL) +
  coord_flip()
dev.off()


png('Ej32.png', width = 2000, height = 4000, res = 300)
palabra_bronte %>% count(word, sort = TRUE) %>% filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() + 
  xlab(NULL) +
  coord_flip()
dev.off()

#########################

####
#Graficas para palabras buenas y malas
####

bing_word_counts <- tidy_austen %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

png('Sentiment_austen.png', width = 3000, height = 2000, res = 300)
bing_word_counts %>% group_by(sentiment) %>% top_n(20) %>%
  ungroup() %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
dev.off()

bing_word <- tidy_bronte %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()



png('Sentiment_bronte.png', width = 3000, height = 2000, res = 300)
bing_word %>% group_by(sentiment) %>% top_n(20) %>%
  ungroup() %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
dev.off()

#####################
#Saber cuantas palabras positivas negativas
#Convertirlas a 1 y 0
##############

pos_bronte = bing_word[bing_word$sentiment == "positive",]
ejeb2 = sum(pos_bronte$n)
pos_austen = bing_word_counts[bing_word_counts$sentiment == "positive",]
eje2 = sum(pos_austen$n)

neg_bronte = bing_word[bing_word$sentiment == "negative",]
ejeb = sum(neg_bronte$n)
neg_austen = bing_word_counts[bing_word_counts$sentiment == "negative",]
eje = sum(neg_austen$n)

col2 = ifelse(bing_word$sentiment == "positive", 1, 0)
bing_word$sentiment
cbind(bing_word$sentiment)
olso = cbind(bing_word, col2)

####
#Estas no son las completas. No toman frecuencia
#########

#png('posneg_bronte.png', width = 1300, height = 2000, res = 300)
h3 = hist(1*(olso$col2 < .5), breaks = seq(0,1,0.5),
     main="", ylab = "Frequency", xlab=" ",
     xlim=c(0,1.0),  ylim = c(0, 1400))
text(h3$mids,h3$counts,labels=h3$counts, adj=c(0.5, -0.5))
#dev.off()

col3 = ifelse(bing_word_counts$sentiment == "positive", 1, 0)
bing_word_counts$sentiment
cbind(bing_word_counts$sentiment)
olso2 = cbind(bing_word_counts, col3)

#png('posneg_austen.png', width = 1300, height = 2000, res = 300)
h4 = hist(1*(olso2$col3 < .5), breaks = seq(0,1,0.5),
          main="", ylab = "Frequency", xlab=" ",
          xlim=c(0,1.0), ylim = c(0, 1000))
text(h4$mids,h4$counts,labels=h4$counts, adj=c(0.5, -0.5))
#dev.off()

######################
#Conteo total de positivo y negativo
################
i = 0
x1 = c()
for (i in i:ejeb) {
  x1 = append(x1, 0)
}
j = 0
for (j in j:ejeb2) {
  x1 = append(x1, 1)
}

#conteo completo de negativos y positivos
png('posneg_austen.png', width = 1300, height = 2000, res = 300)
h5 = hist(1*(x > .5), breaks = seq(0,1,0.5),
          main="",
          ylab = "Frequency",
          xlab=" ",
          xlim=c(0,1.0), 
          ylim = c(0, 4000))
text(h5$mids,h5$counts,labels=h5$counts, adj=c(0.5, -0.5))
dev.off()
png('posneg_bronte.png', width = 1300, height = 2000, res = 300)
h6 = hist(1*(x1 > .5), breaks = seq(0,1,0.5),
          main="",
          ylab = "Frequency",
          xlab=" ",
          xlim=c(0,1.0), 
          ylim = c(0, 6000))
text(h6$mids,h6$counts,labels=h6$counts, adj=c(0.5, -0.5))
dev.off()

table(x1)

############################
#Intento 1 del experimento
#######################

#rgeom
png('rgeom_austen1.png', width = 1300, height = 2000, res = 300)
hist(rgeom(10000, 0.51), main="",
     ylab = "Frequency", xlab=" ",
     ylim = c(0, 8000),xlim = c(0, 12)) #Jane Austen
dev.off()

png('rgeom_bronte.png', width = 1300, height = 2000, res = 300)
hist(rgeom(10000, 0.34), main="",
     ylab = "Frequency",
     xlab=" ",
     ylim = c(0, 7000), xlim = c(0, 27)) #Bronte
dev.off()

sum(sample(x, 50, replace = F))
sum(sample(x1, 50, replace = F))

resultados = numeric()
while(length(resultados)< 10000){
temp = 0; while (TRUE) {
  temp = temp + 1;
  if(sample(x,1) < 0.51){break}
};resultados = c(resultados, temp)
}
#png('co_rgeom_bronte.png', width = 1300, height = 2000, res = 300)
hist(resultados, main="",
     ylab = "Frequency", xlab=" ",
     ylim = c(0, 8000), xlim = c(0, 15))
dev.off()
##############################
#rnbionom
#######################
p = 0.34
n = 10000
k = 10
resultados = numeric()
while(length(resultados)< n){
  temp = 0; 
  exito = 0;
  while (TRUE) {
    temp = temp + 1;
    if(sample(x1,1) < p){
      exito = exito + 1;
      if (exito == k){break}
      }
  };resultados = c(resultados, temp)
}
png('co_rbinom_bronte.png', width = 1300, height = 2000, res = 300)
hist(resultados, main="",
     ylab = "Frequency", xlab=" ",
     ylim = c(0, 2000))
dev.off()

png('rbinom_bronte.png', width = 1300, height = 2000, res = 300)
hist(rnbinom(10000, k, p), main="",
     ylab = "Frequency", xlab=" ",
     ylim = c(0, 3000))
dev.off()

#######################
#rhyper
####################

table(50 - rhyper(10000,  2729, 5247, 50)) #bronte
table(50 - rhyper(10000, 3911, 3612, 50)) #bronte

total_bronte = 7523
total_malas = 3612
total_buentas = 3911
#total_bronte = 7976
#total_malas = 5247
#total_buentas = 2729
seleccionar = 50
limite = 24
#limite = 33
replicas = 1000

cont = 0
exper = c(rep(FALSE, total_malas), rep(TRUE, total_buentas))
for (replica in 1:replicas) {
  envio = sample(exper, seleccionar, replace = FALSE)
  cont = cont + (seleccionar - sum(envio) > limite)
}
print(cont/replicas)
 ###################
#Extras

library(wordcloud)
library(reshape2)

png('word_bronte.png', width = 1300, height = 1500, res = 300)
tidy_bronte %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("pink", "blue"),
                   max.words = 200)
dev.off()

png('word_austen.png', width = 1300, height = 1500, res = 300)
tidy_austen %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("pink", "blue"),
                   max.words = 200)
dev.off()


