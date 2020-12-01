two.dice <- function(){
  dice <- sample(1:6, size = 2, replace = TRUE)
  return(sum(dice))
}

two.dice()
replicate(n = 20, expr = two.dice())

sims <- replicate(100, two.dice())
table(sims)
df = as.data.frame(table(sims)/length(sims))
barplot(table(sims)/length(sims), xlab = 'Sum', ylab = 'Relative Frequency', main = '100 Rolls of 2 Fair Dice')

# Libraries
library(ggplot2)

# Create data
data <- data.frame(x=df$sims,y=df$Freq)

# Plot
png("Ej13_dice1.png", width = 1000, height = 1300, res = 300)
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("Sum") +
  ylab("Relative Frequency")
dev.off()

##############################################################

more.sims <- replicate(1000, two.dice())
df1 = as.data.frame(table(more.sims)/length(more.sims))
plot(table(more.sims)/length(more.sims), 
     xlab = 'Sum', ylab = 'Relative Frequency', main = '1000 Rolls of 2 Fair Dice')

# Create data
data1 <- data.frame(x=df1$more.sims,y=df1$Freq)

# Plot
png("Ej13_dice2.png", width = 1000, height = 1300, res = 300)
ggplot(data1, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="blue", size=4) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Sum") +
  ylab("Relative Frequency")
dev.off()
#############################################################

even.more.sims <- replicate(100000, two.dice())
df2 = as.data.frame(table(even.more.sims)/length(even.more.sims))
plot(table(even.more.sims)/length(even.more.sims), 
     xlab = 'Sum', ylab = 'Relative Frequency', main = '100000 Rolls of 2 Fair Dice')

# Create data
data2 <- data.frame(x=df2$even.more.sims,y=df2$Freq)

# Plot
png("Ej13_dice3.png", width = 1000, height = 1300, res = 300)
ggplot(data2, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="red", size=4) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Sum") +
  ylab("Relative Frequency")
dev.off()
