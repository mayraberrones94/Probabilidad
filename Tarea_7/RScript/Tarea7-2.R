# DEFINICIONES
expo = function(lambda) x^lambda # Familia exponencial
expoT = function(lambda) x^lambda-1 # Familia exponencial trasladada
BoxCox = function(lambda) if (lambda!=0) (x^lambda-1)/lambda else log(x) # Familia de Box-Cox
geoMean = function(arg) exp(mean(log(arg))) # Media geométrica
geoBoxCox = function(lambda)# Familia de Box-Cox transformada
  if (lambda!=0) (x^lambda-1)/
  (lambda*(geoMean(x)^(lambda-1))) else geoMean(x)*log(x)

# GRÁFICOS
x = seq(from=0.5, to=1.5, by=0.1) # Los valores o datos que se desea transformar
e = seq(from=-2, to=2, by=1) # Los exponentes o valores de lambda
## Elegir una de estas cuatro familias
# Familia exponencial
f = expo
# Familia exponencial trasladada
f = expoT
# Familia de Box-Cox
f = BoxCox
# Familia de Box-Cox con la media geométrica
f = geoBoxCox
## Elegir si representar una función al lado de otra o todas en un gráfico
# Varias figuras (existe un máximo por línea)
x11()
par(mfcol=c(1, length(e)))
for (i1 in 1:length(e))
  plot(f(e[i1]), type='l', main='', xlab='', ylab='')
# En una misma figura
x11()
k = c(-2,-1,0,1,2)
plot(f(e[1]), type='l', main='', xlab='', ylab='')
for (i1 in 1:length(e))
  lines(f(e[i1]))
# Gráfico para comparar las cuatro familias de funciones
x11()
# Para seleccionar unos ejes verticales comunes a las cuatro figuras:
yLim = c(min(c(expo(e[1]), expoT(e[1]), BoxCox(e[1]), geoBoxCox(e[1]))),
         max(c(expo(e[1]), expoT(e[1]), BoxCox(e[1]), geoBoxCox(e[1]))))
for (i1 in 2:length(e)) {
  yLimTemp = c(min(c(expo(e[i1]), expoT(e[i1]), BoxCox(e[i1]), geoBoxCox(e[i1]))),
               max(c(expo(e[i1]), expoT(e[i1]), BoxCox(e[i1]), geoBoxCox(e[i1]))))
  if (yLimTemp[1] < yLim[1])
    yLim[1] = yLimTemp[1]
  if (yLimTemp[2] > yLim[2])
    yLim[2] = yLimTemp[2]
}
# Ahora hacemos las figuras
par(mfcol=c(1, 4))
plot(expo(e[1]), type='l', main='', xlab='', ylab='', ylim=yLim)
for (i1 in 1:length(e))
  lines(expo(e[i1]))

plot(expoT(e[1]), type='l', main='', xlab='', ylab='', ylim=yLim)
for (i1 in 1:length(e))
  lines(expoT(e[i1]))
plot(BoxCox(e[1]), type='l', main='', xlab='', ylab='', ylim=yLim)
for (i1 in 1:length(e))
  lines(BoxCox(e[i1]))
plot(geoBoxCox(e[1]), type='l', main='', xlab='', ylab='', ylim=yLim)
for (i1 in 1:length(e))
  lines(geoBoxCox(e[i1]))


# EJERCICIOS
## Elegir el problema de una variable que se quiere intentar resolver
# Para reducir la asimetría
funTransfor = BoxCox # Transformación: Box-Cox
funEvalua = function(arg) mean(((arg-mean(arg))/sd(arg))^3) # Objetivo: asimetría
funOptimi = function(arg) which.min(abs(arg)) # Criterio: minimizar la distancia al 0
x = rchisq(50, 3) # Datos de una distribución con simetría positiva
# Para aumentar la normalidad
funTransfor = BoxCox # Transformación: Box-Cox
funEvalua = function(arg) shapiro.test(arg)$p.value # Objetivo: normalidad
funOptimi = function(arg) which.max(arg) # Criterio: maximizar el valor p
x = rchisq(50, 3) # Datos de una distribución no normal
# Bucle que prueba valores de lambda y elige el que optimiza el objetivo
e = seq(-2, 2, 0.1) # Vector con los valores del parámetro lambda
vectorE = matrix(NA, length(e)) # Vector de evaluaciones
for (i1 in 1:length(e))
{
  datosTrans = funTransfor(e[i1])
  vectorE[i1] = funEvalua(datosTrans)
}

posLambdaEst = funOptimi(vectorE) # Posición del lambda
e[posLambdaEst] # Valor de lambda estimado
# Mostrar los resultados para la transformación elegida
x11()
par(mfcol=c(1, 3))
hist(x, freq=FALSE, labels=FALSE, main='', xlab='', ylab='')
plot(e, abs(vectorE), main='', xlab='', ylab='', ylim=yLim)
hist(funTransfor(e[poslambdaEst]), freq=FALSE, labels=FALSE, main='', xlab='', ylab='')


