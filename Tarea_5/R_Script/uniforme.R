uniforme = function(n, semilla) {
    a = 11551
    c = 27077
    m = 39709
    datos = numeric()
    x = semilla
    while (length(datos) < n) {
        x = (a * x + c) %% m
        datos = c(datos, x)
    }
    return(datos / (m - 1))
}
png("Ej5_runif.png", width = 1000, height = 1500, res = 300)
hist(runif(2000), xlab = "", main = " ")
dev.off()
png("Ej5_uniforme.png", width = 1000, height = 1500, res = 300)
hist(uniforme(2000, 27), xlab = "", main = " ")
dev.off()
png("Ej5_lcg.png", width = 1000, height = 1500, res = 300)
hist(lcg.rand(2000), xlab = "", main = " ")
dev.off()

uniform.test(hist(runif(2000)))
uniform.test(hist(uniforme(2000, 27)))
uniform.test(hist(lcg.rand(2000)))

x1 = uniforme(1000, 27)
y1 = uniforme(1000, 27)
z1 = uniforme(1000, 27)
png("Ej5_scuniforme.png", width = 1000, height = 1500, res = 300)
scatter3D(x1, y1, z1, colvar = NULL, pch = 20, cex = 0.5, theta = 20, main = "" )
dev.off()

summary(uniforme(5000, 27))

lcg.rand = function(n){
    rng = vector(length = n)
    m = 2 ** 32
    a = 1103515245
    c = 12345
    
    d = as.numeric(Sys.time()) * 1000
    #d = 27
    for (i in 1:n){
        d = (a * d + c) %% m
        rng[i] = d / m
    }
    return(rng)
}
#library(plot3D)
#library(animation)

lcg.rand()

x = lcg.rand(1000)
y = lcg.rand(1000)
z = lcg.rand(1000)
png("Ej5_sclcg.png", width = 1000, height = 1500, res = 300)
scatter3D(x, y, z, colvar = NULL, pch = 20, cex = 0.5, theta = 20, main = "" )
dev.off()

x2 = runif(1000)
y2 = runif(1000)
z2 = runif(1000)
png("Ej5_scrunif.png", width = 1000, height = 1500, res = 300)
scatter3D(x2, y2, z2, colvar = NULL, pch = 20, cex = 0.5, theta = 20, main = "" )
dev.off()