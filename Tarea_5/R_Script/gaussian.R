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

#Version 1
gaussian = function(mu, sigma) {
    u1 = runif(1);
    u2 = uniforme(2, u1 * 1000)
    #u = lcg.rand(2)
    z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
    z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
    datos = c(z0);
    return (sigma * datos + mu);
}
cat(gaussian(0, 1), '\n')

#Version2
gaussian = function(mu, sigma) {
    #u1 = lcg.rand(1)
    u1 = runif(1);
    u2 = uniforme(1, u1 * 1000)
    #u = lcg.rand(2)
    z0 = sqrt(-2 * log(u1)) * cos(2 * pi * u2);
    z1 = sqrt(-2 * log(u1)) * sin(2 * pi * u2);
    datos = c(z0);
    return (sigma * datos + mu);
}
cat(gaussian(0, 1), '\n')

ngaus = 5000
mu = 10
desv = 5
#orig = rnorm(ngaus, mu, desv)
#hist(orig)
nuevo = numeric()
while(length(nuevo) < ngaus){
    nuevo = c(nuevo, gaussian(mu, desv))
}
length(nuevo)
png("Ej5_run_uni.png", width = 1000, height = 1500, res = 300)
hist(nuevo, xlab = "", main = "")
dev.off()

