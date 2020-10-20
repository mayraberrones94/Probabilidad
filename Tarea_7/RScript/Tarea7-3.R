
##############################
#Probar diferentes ys
##########################
x = runif(100)
y = a * sqrt(x) + b#Plinomial
#y = a*x^2 + b*x + c#Quadratica
#y = a * (exp(p * x))  #Exp
#y = a + b * log(x) #Logarithmic
#y = (a * x) / (b + x) #Michaelis

#x = 1:100
p = rnorm(1) # generate a standard normal random variable
#y = jitter(x^p, factor = length(x)/2)# raise x to the p^th power, and add some randomness

#png("Ej7_jitter.png", width = 1000, height = 1500, res = 300)
plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3) 
#dev.off()


require(mosaic)


Rand = data.frame(x, y)# Combine x and y into a data frame

png("Ej7_p5-1.png", width = 1000, height = 1200, res = 300)
plotPoints(y ~ x, data = Rand)
dev.off()

fm = lm(y ~ x, data = Rand)
summary(fm)
png("Ej7_p5-2.png", width = 1000, height = 1200, res = 300)
plot(fm, which = 1)
dev.off()
png("Ej7_p5-3.png", width = 1000, height = 1200, res = 300)
plot(fm, which = 2)
dev.off()

# e.g. for a log transformation
Rand$y.new = with(data = Rand, log(y))

png("Ej7_p5-4.png", width = 1000, height = 1200, res = 300)
plotPoints(y.new ~ x, data = Rand)
dev.off()

p = rnorm(1)
Rand$y = jitter(x^p, factor = length(x)/2)


tukeyLadder = function(x, q = NULL) {
  if (is.null(q)) {
    return(x)
  }
  if (q == 0) {
    x.new = log(x)
  } else {
    if (q < 0) {
      x.new = -x^q
    } else {
      x.new = x^q
    }
  }
  return(x.new)
}

tukeyPlot = function(x, y, q.y, q.x = 1, ...) {
  y.new = tukeyLadder(y, q.y)
  x.new = tukeyLadder(x, q.x)
  y.center = mean(y.new, na.rm = TRUE)
  x.center = mean(x.new, na.rm = TRUE)
  x.bottom = 0.1 * (max(y.new) - min(y.new)) + min(y.new)
  y.left = 0.1 * (max(x.new) - min(x.new)) + min(x.new)
  xyplot(y.new ~ x.new, panel = function(x, y, ...) {
    panel.xyplot(x, y, pch = 19, alpha = 0.2, cex = 2)
    panel.text(y.left, y.center, paste("q.y =", q.y), col = "red", cex = 2)
    panel.text(x.center, x.bottom, paste("q.x =", q.x), col = "red", cex = 2)
  })
}

require(manipulate)
manipulate(with(Rand, tukeyPlot(x, y, q.y)), q.y = slider(-3, 3, step = 0.50, 
                                                          initial = 1))

transformTukey(
  x,
  start = -3,
  end = 3,
  int = 0.50,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = TRUE
)
y.trans = transformTukey(y)

library(MASS)

plot(lm(y~x,data=Rand))
png("Ej7_boxcox5.png", width = 1500, height = 1200, res = 300)
boxcox(lm(y~x,data=Rand),lambda=seq(-3,3,by=.25))
dev.off()
