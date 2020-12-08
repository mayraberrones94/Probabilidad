png("Ej14_dice.png", width = 1000, height = 1300, res = 300)
d_sample <- sample(1:6,10000, replace= TRUE)
hist(d_sample, col = "light blue",main="")
abline(v=3.5, col = "red",lty=1)
dev.off()

png("Ej14_dice1.png", width = 1000, height = 1300, res = 300)
x10 <- c()
k =10000
for ( i in 1:k) {
  x10[i] = mean(sample(1:6,10, replace = TRUE))}
hist(x10, col ="pink", main="Sample size =10",xlab ="Outcome of die roll")
abline(v = mean(x10), col = "Red")
abline(v = 3.5, col = "blue")
dev.off()


x30 <- c()
x100 <- c()
x1000 <- c()
k =10000
for ( i in 1:k){
  x30[i] = mean(sample(1:6,30, replace = TRUE))
  x100[i] = mean(sample(1:6,100, replace = TRUE))
  x1000[i] = mean(sample(1:6,1000, replace = TRUE))
}

png("Ej14_dice1.png", width = 1000, height = 1300, res = 300)
hist(x30, col ="light blue",main="",xlab ="die roll")
abline(v = mean(x30), col = "blue")
dev.off()

png("Ej14_dice2.png", width = 1000, height = 1300, res = 300)
hist(x100, col ="pink", main="",xlab ="die roll")
abline(v = mean(x100), col = "red")
dev.off()

png("Ej14_dice3.png", width = 1000, height = 1300, res = 300)
hist(x1000, col ="orange",main="",xlab ="die roll")
abline(v = mean(x1000), col = "red")
dev.off()