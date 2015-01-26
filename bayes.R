#### bayes filter #####
desvi<-2
puntos.x<-rnorm(100,0,desvi)
puntos.y<-rnorm(100,0,desvi)
punto.real<-c(3,5)
puntos.x<-rnorm(100,0,desvi)+punto.real[1]
puntos.y<-rnorm(100,0,desvi)+punto.real[2]
puntos<-cbind(puntos.x,puntos.y)
# crear el espacio de soluciones
space.x<-seq(2,4,0.05)
space.y<-seq(2,6,0.05)