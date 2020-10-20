a = 10
b = 8
c = 7
k = runif(1)
qqnorm(y, main = " ")
qqline(y)
x <- runif(300,  min=-100, max=100) 
#y = a * x + b #Plinomial
y = a * x^2 + b*x + c

xm = mean(x)
ym = mean(y)

xx = x - xm
yy = y - ym

xy = x *y
xc = x **2
yc = y**2

sxy = sum(xy)
sxc = sum(xc)
syc = sum(yc)

raux = sxy / sqrt(sxc * syc)
n = length(x)

XY <- x * y
Xc <- x**2
Yc <- y**2
rdirar <- sum(XY) - (sum(x)*sum(y)) / n
rdirabX <- sum(Xc) - (sum(x)**2) / n
rdirabY <- sum(Yc) - (sum(y)**2) / n
rdirab <- sqrt(rdirabX * rdirabY)
rdir <- rdirar / rdirab

plot(x ~ y, type='p', col ='red', pch=16)
#y1 = a * sqrt(x) + b#Plinomial
y2 = a*x^2 + b*x + c#Quadratica
y3 = a * (exp(k * x))  #Exp
y4 = a + b * log(x) #Logarithmic

y5 = (a*x) / (b + x) #Michaelis
y6 = sin(pi/x) 
y2 = runif(1, min=1, max=25) + 5 *x + runif(1, min=1, max=25) *x^2
y <- 0.1*x^3 - 0.5 * x^2 - x + 10 + rnorm(length(x),0,8) 

m0 = aov(y ~ x)
m1 = lm (y ~ x)

m3 = lm(((y ^ lambda-1) / lambda) ~ x)
anova(m1)

transformTukey(
  x,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)


### Log-normal distribution example
Conc = rlnorm(100)
y2.trans = transformTukey(y2)


cor.test(x, y, method = "pearson")


# plot of x and y :
plot(x,y2,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3) 

# Can we find a polynome that fit this function ?
model <- lm(y ~ x + I(x^2) + I(x^3))

# I can get the features of this model :
summary(model)
model$coefficients
summary(model)$adj.r.squared

# For each value of x, I can get the value of y estimated by the model, and add it to the current plot !
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 )  

# I add the features of the model to the plot
coeff <- round(model$coefficients , 2)
text(3, -70 , paste("Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))

