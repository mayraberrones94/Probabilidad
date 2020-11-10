############################################################
#Ejercicio 1, page 247
############################################################

cards = sample(2:10, 10000, replace = TRUE)

all_odds = c()
for (j in 1:100) {
even = 0
odd = 0
for (i in 1:10000) {
  cards = sample(2:10, 1, replace = TRUE)
  if ((cards%% 2) == 0) {
    even = even + 1
  }else{
    odd = odd +1
  }
}
all_odds = c(all_odds, (((odd-even)*1)/10000))
}
install.packages("viridis")  # Install
library("viridis") 

png("Ej10_hist1.png", width = 1000, height = 1200, res = 300)
hist(all_odds, xlab = "", main = " ", ylim = c(0, 25), xlim = c(-0.149, -0.080), col = viridis(10))
dev.off()

library(ggplot2)

data <- data.frame(
  Group=c("Even", "Odd"),
  value=c(even, odd)
)

# Basic piechart
png("Ej10_even.png", width = 1000, height = 1200, res = 300)
ggplot(data, aes(x="", y=value, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void()
dev.off()

both = c(even, odd)
pie(both, col =c("firebrick", "dodgerblue"))
both


((odd-even)*1)/10000
-1/9

############################################################
#Ejercicio 6, page 247
############################################################

suma_total = c()
suma_total1 = c()
suma_total2 = c()

for (j in 1:100) {
  x = c()
  y = c()
  xy =c()
for (i in 1:10000) {
  d1 = sample(1:6, 1, replace = TRUE)
  d2 = sample(1:6, 1, replace = TRUE)
  
  x = c(x, (d1 + d2))
  y = c(y, (d1 - d2))
  xy = c(xy, ((d1 + d2)*(d1 - d2)))
}

tabx <- as.data.frame(table(x))
taby <- as.data.frame(table(y))
tabxy <- as.data.frame(table(xy))

tabx$col3 = (as.numeric(as.character(tabx$x))* ((as.numeric(tabx$Freq) * 1)/10000))
suma_total = c(suma_total, (sum(tabx$col3)))

taby$col3 = (as.numeric(as.character(taby$y))* ((as.numeric(taby$Freq) * 1)/10000))
suma_total1 = c(suma_total1, (sum(taby$col3)))

tabxy$col3 = (as.numeric(as.character(tabxy$xy))* ((as.numeric(tabxy$Freq) * 1)/10000))
suma_total2 = c(suma_total2, (sum(tabxy$col3)))
}

data <- data.frame(
  #Group= tabx$x,
  #Group= taby$y,
  Group= tabxy$xy,
  #value= tabx$Freq
  #value= taby$Freq
  value= tabxy$Freq
)
png("Ej10_pie3.png", width = 1000, height = 1200, res = 300)
# Basic piechart
ggplot(data, aes(x="", y=value, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void()
dev.off()

tabx$col3 = (as.numeric(as.character(tabx$x))* ((as.numeric(tabx$Freq) * 1)/10000))
taby$col3 = (as.numeric(as.character(taby$y))* ((as.numeric(taby$Freq) * 1)/10000))
tabxy$col3 = (as.numeric(as.character(tabxy$xy))* ((as.numeric(tabxy$Freq) * 1)/10000))


library("RColorBrewer")
display.brewer.all()

png("Ej10_hist2.png", width = 1000, height = 1400, res = 300)
hist(suma_total, xlab = "", main = " ", ylim = c(0, 35), xlim = c(6.9, 7.1), col=brewer.pal(n = 8, name = "Paired"))
dev.off()

png("Ej10_hist3.png", width = 1000, height = 1400, res = 300)
hist(suma_total1, xlab = "", main = " ", ylim = c(0, 35), xlim = c(-0.06, 0.080), col=brewer.pal(n = 8, name = "Set1"))
dev.off()

png("Ej10_hist4.png", width = 1000, height = 1400, res = 300)
hist(suma_total2, xlab = "", main = " ", ylim = c(0, 25), xlim = c(-0.4, 0.6), col=brewer.pal(n = 8, name = "Set2"))
dev.off()

############################################################
#Ejercicio 15, page 249
############################################################

library("arrangements")
permutations(x = c("Silver", "Gold"),freq = c(3, 2), k = 5)

draw = permutations(x = c(-1, 1),freq = c(3, 2), k = 5)
fila = c()

for (i in 1:10){
  cont = 0
  gold = 0
  for(j in 1:5) {
    cont = cont + (as.numeric(as.character(draw[c(i), c(j)])))
    if (cont >= 1) {
      break
    }
    if ((as.numeric(as.character(draw[c(i), c(j)]))) == 1) {
      gold = gold + 1
      if (gold == 2) {
        break
      }
    }
  }
  fila = c(fila, cont)
}

exp_draw = sample(fila, 10000, replace = T)

tabexp <- as.data.frame(table(exp_draw))
data <- data.frame(
  Group= tabexp$exp_draw,
  value= tabexp$Freq
)
png("Ej10_pie4.png", width = 1000, height = 1200, res = 300)
# Basic piechart
ggplot(data, aes(x="", y=value, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void()
dev.off()

png("Ej10_barplot1.png", width = 1000, height = 1200, res = 300)
barplot(lista, names.arg = c("Negative", "Positive"), main = " ", col=brewer.pal(n = 8, name = "Set2"))
dev.off()

 data = as.data.frame(table(lista))
lista = c(2941, (2052+5007))

############################################################
#Ejercicio 18, page 249
############################################################
repe = c()
for (k in 1:100) {
open =c()
for (j in 1:10000) {
keys = sample(1:6, 1, replace = TRUE)
trys = 0

for (i in 1:6) {
  if (keys == (as.numeric(as.character(i)))) {
    break
  }else{
    trys = trys + 1
  }
}
open = c(open, trys)
}
repe = c(repe, mean(open))
}

png("Ej10_hist5-1.png", width = 1000, height = 1400, res = 300)
hist(repe, xlab = "", main = " ", ylim = c(0, 25), xlim = c(2.44, 2.54), col=brewer.pal(n = 8, name = "Set1"))
dev.off()

png("Ej10_boxplot1.png", width = 1000, height = 1400, res = 300)
boxplot(open, col=brewer.pal(n = 8, name = "Set3"))
dev.off()
mean(open)


library("plotly")
png("Ej10_boxplot1.png", width = 1000, height = 1400, res = 300)
fig <- plot_ly(y = open, type = "box", name = "Keys to try")

fig

################################################
n_cups <- 8
cups <- sample(rep(c("milk", "tea"), each = n_cups / 2))
cups

guesses <- sample(rep(c("milk", "tea"), each = n_cups / 2))
guesses

cup_results <- cups == guesses
cup_results

n_right <- sum(cup_results)
n_right

sim_null_tea <- function(n_cups){
  cups <- sample(rep(c("milk", "tea"), each = n_cups / 2))
  guesses <- sample(rep(c("milk", "tea"), each = n_cups / 2))
  cup_results <- cups == guesses
  n_right <- sum(cup_results)
  return(n_right)
}
sim_null_tea(n_cups = 8)

tea_sims <- replicate(5, sim_null_tea(n_cups = 8))
tea_sims

tea_sims <- replicate(1000, sim_null_tea(n_cups = 8))
mean(tea_sims)

quantile(tea_sims, probs = c(0.025, 0.975))

mean(tea_sims == 8)
#https://geanders.github.io/RProgrammingForResearch/exploring-data-3.html
n_cups <- seq(from = 2, to = 14, by = 2)
perc_all_right <- sapply(n_cups, FUN = function(n_cups){
  cups_right <- replicate(1000, sim_null_tea(n_cups))
  out <- mean(cups_right == n_cups)
  return(out)
})
perc_all_right

tea_sims <- data.frame(n_cups, perc_all_right)
ggplot(tea_sims, aes(x = n_cups, y = perc_all_right)) + 
  geom_point() + xlab("# of cups tested") + 
  ylab("Probability of getting\nall cups right if guessing")

dhyper(x = 3, m = 4, n = 4, k = 4)

analytical_results <- data.frame(n_cups = seq(2, 15, 2)) %>%
  mutate(perc_all_right = dhyper(x = n_cups / 2,
                                 m = n_cups / 2,
                                 n = n_cups / 2,
                                 k = n_cups / 2))

ggplot(analytical_results, aes(x = n_cups, y = perc_all_right)) + 
  geom_line(color = "darkgray") +
  geom_point(data = tea_sims) + xlab("# of cups tested") + 
  ylab("Probability of getting\nall cups right if guessing")






