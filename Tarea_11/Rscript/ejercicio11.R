############################################################
library(magick)

kern <- matrix(0, ncol = 3, nrow = 3)
kern[1, c(1, 2, 3] <- 0.25
kern[2, c(1, 2, 3)] <- 0.25
kern[3, c(1, 2, 3)] <- 0.25
kern

img <- image_read('/Users/miguelberrones/Dropbox/RStudio_imac/Ejercicio_Elisa/puppy.jpg')
img_blurred <- image_convolve(img, kern)
image_append(c(img, img_blurred))
png("Ej11_blur1.png", width = 300, height = 300, res = 200)
img_blurred
dev.off()
img
img %>% image_convolve('Gaussian:0x5', scaling = '60,40%')

img %>% image_convolve('Sobel') %>% image_negate()

img %>% image_convolve('Sobel', scaling = '50%', bias = '50%')

img %>% image_convolve('DoG:0,0,2') %>% image_negate()

img %>% image_convolve('DoG:0,0,2', scaling = '100, 100%')
############################################################


library(tidyverse) # metapackage of all tidyverse packages
library(keras)
library(rsample)
library(reticulate)

list.files(path = "../input")

library(readr)
fash_mnist <- read_csv("fashion-mnist_train.csv")

dim(fash_mnist)

vizTrain <- function(input) {
  
  dimmax <- sqrt(ncol(fash_mnist[, -1]))
  
  dimn <- ceiling(sqrt(nrow(input)))
  par(mfrow = c(dimn, dimn), mar = c(0.1, 0.1, 0.1, 
                                     0.1))
  
  for (i in 1:nrow(input)) {
    m1 <- matrix(input[i, 2:ncol(input)], nrow = dimmax, 
                 byrow = T)
    m1 <- apply(m1, 2, as.numeric)
    m1 <- t(apply(m1, 2, rev))
    
    image(1:dimmax, 1:dimmax, m1, col = grey.colors(255), 
          xaxt = "n", yaxt = "n")
    text(2, 20, col = "white", cex = 1.2, fash_mnist[i, 
                                                     1])
  }
  
}

vizTrain(fash_mnist[1:25, ])

set.seed(859)
idx <- initial_split(data = fash_mnist, prop = 0.8, strata = "label")
fmnist_train <- training(idx)
fmnist_test <- testing(idx)

fmnist_train <- data.matrix(fmnist_train)
fmnist_test <- data.matrix(fmnist_test)

train_x <- fmnist_train[,-1]
train_y <- fmnist_train[,1]

test_x <- fmnist_test[,-1]
test_y <- fmnist_test[,1]

train_x_keras <- array_reshape(train_x, dim = dim(train_x))
test_x_keras <- array_reshape(test_x, dim = dim(test_x))

train_x_keras <- train_x_keras/255
test_x_keras <- test_x_keras/255

train_y_keras <- keras::to_categorical(train_y)

model <- keras_model_sequential()%>%
  layer_dense(unit = 512, 
              activation = "relu", 
              input_shape = 784,        # Input_shape hanya digunakan untuk layer pertama saja
              name = "hidden1",
              bias_initializer = init,
              kernel_initializer = init)%>%
  layer_dense(unit = 90, 
              activation = "relu", 
              name = "hidden2",
              bias_initializer = init,
              kernel_initializer = init)%>% 
  layer_dense(unit = 100, 
              activation = "relu", 
              name = "hidden3",
              bias_initializer = init,
              kernel_initializer = init)%>% 
  layer_dense(unit = 10, 
              activation = "softmax", 
              name = "output", 
              bias_initializer = init,
              kernel_initializer = init)

summary(model)

model%>%    # parameter yang bisa diatur adalah loss, optimizer dan metrics
  compile(loss = "categorical_crossentropy",
          optimizer = optimizer_adam(lr = 0.002),
          metrics = c("accuracy"))

history <- model%>%
  fit(train_x_keras,
      train_y_keras,
      epoch = 10,
      batch_size = 100,
      validation_split = 0.2)

plot(history)

pred <- predict_classes(object = model, x = test_x_keras)

caret::confusionMatrix(as.factor(pred), as.factor(test_y))

#########################################################




########################################################

obsfreq <- matrix(c(1052,277, 
                    1188, 37, 
                    990,270,
                    1083, 115,
                    910, 172,
                    1195, 81,
                    809, 325,
                    1113, 72, 
                    1115, 16,
                    1141, 30),nrow=2,ncol=10)

obsfreq

chisq.test(obsfreq)
png("Ej11_barplot.png", width = 1200, height = 1200, res = 300)
barplot(obsfreq, names.arg = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
dev.off()

#covariance

poc = c()
rest = c()

aux = c(50, 100, 200, 400, 800, 1600, 3200, 6400, 12800, 25600)
for (j in aux) {
for (i in 1:10) {
  num1 = sample(1:100000, j, replace=TRUE)
  num2 = sample(1:100000, j, replace=TRUE)
  
  pr = var(num1 + num2)
  prueba = var(num1) + var(num2) + (2*cov(num1, num2))
  if (pr == prueba) {
    poc = c(poc, j)
  }else{
    rest = c(rest, j)
  }
}
}
data = as.data.frame(table(poc))

data2 = as.data.frame(table(rest))

frec2 = matrix(c(72, 28,
                 75, 25,
                 64, 36,
                 58, 42,
                 63, 37,
                 58, 42,
                 63, 37,
                 63, 37,
                 72, 28,
                 72, 28), nrow = 2, ncol = 10)

png("Ej11_barplot3.png", width = 2500, height = 1500, res = 300)
barplot(frec2, names.arg = c(50, 100, 200, 400, 800, 1600, 3200, 6400, 12800, 25600))
dev.off()

frec3 = matrix(c(10, 0,
                 8, 2,
                 7, 3,
                 5, 5,
                 8, 2,
                 8, 2,
                 8, 2,
                 4, 6, 
                 9, 1,
                 10, 0), nrow = 2, ncol = 10)

png("Ej11_barplot4.png", width = 2500, height = 1500, res = 300)
barplot(frec3, names.arg = c(50, 100, 200, 400, 800, 1600, 3200, 6400, 12800, 25600))
dev.off()


frequencias <- matrix(c(681, 319, 
                    673, 327,
                     665, 335,
                     662, 338,
                     646, 354,
                    693, 307,
                   675, 325,
                     652, 348,
                    681, 319,
                    634, 366),nrow=2,ncol=10)

png("Ej11_barplot2.png", width = 2500, height = 1500, res = 300)
barplot(frequencias, names.arg = c(50, 100, 200, 400, 800, 1600, 3200, 6400, 12800, 25600))
dev.off()

