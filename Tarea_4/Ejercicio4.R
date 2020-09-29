#############
#Ejemplo de Elisa
##########

me = 1
la = 10
mu = exp(-la)
cu = numeric()
ce = numeric()
repl = 100000
br = 10
for (replica in 1:repl) {
  du = numeric()
  de = numeric()
  while(prod(du) > mu){
    du = c(du, runif(1))
  }
  while(sum(de) < me){
    de = c(de, rexp(1, la))
  }
  cu = c(cu, length(du))
  ce = c(ce, length(de))
}
png("e4-10_100000.png",  width = 1000, height = 1500, res = 300)
p1 = hist(rpois(repl, la), plot = FALSE)
p2 = hist(ce,  plot = FALSE)
p3 = hist(cu,   plot = FALSE)
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(144,238,144, max = 255, alpha = 80, names = "lt.green")
plot(p1, col=c1,  ylim=c(0,25000), xlab ="", main = "" )
plot(p2, col= c2, add = TRUE)
plot(p3, col= c3, add = TRUE)
dev.off()

#############################

png("E4_10.png",  width = 4000, height = 2000, res = 300)
par(mfrow = c(1,3))
hist(cu, xlab ="runif", main = "")
hist(ce, xlab ="rexp", main = "")
hist(rpois(repl, la), xlab ="rpois", main = "")
dev.off()

cu = numeric()
ce = numeric()
for (replica in 1:10000) {
  du = numeric()
  de = numeric()
  while(prod(du) > exp(-4)){
    du = c(du, runif(1))
  }
  while(sum(de) < 1){
    de = c(de, rexp(1, 4))
  }
  cu = c(cu, length(du))
  ce = c(ce, length(de))
}

png("E4_distpois.png", width = 1000, height = 2000, res = 300)
hist(rpois(10000, 4), xlab ="", main = "")
dev.off()
png("E4_distexp.png", width = 1000, height = 2000, res = 300)
hist(rexp(1, 4), xlab ="", main = "")
dev.off()
png("E4_distunif.png", width = 1000, height = 2000, res = 300)
hist(runif(10000),  xlab ="", main = "")
dev.off()

