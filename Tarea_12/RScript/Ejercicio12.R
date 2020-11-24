##########
#Problema 1

#d)
z = c()
p = 0
suma = 0
for (i in 0:29) {
  p = (1/(2^(i+1))) * i
  suma = suma + p
  z = c(z, p)
}
suma

#e)
p = 0
suma = 0
for (i in 0:45) {
  p = ((1/3)*((2/3)^(i)))*i
  suma = suma + p
}
suma

#f)
p = 0
suma = 0
for (i in 0:1023) {
  p = (exp(-2)*((2^(i))/(factorial(i))))* i
  suma = suma + p
}
suma



