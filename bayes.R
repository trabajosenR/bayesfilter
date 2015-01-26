#### bayes filter #####
setwd( "/Users/davidalejandro/Documents/Google Drive/R2015//bayesfilter")
desvi<-2
puntos.x<-rnorm(100,0,desvi)
puntos.y<-rnorm(100,0,desvi)
punto.real<-c(3,5)
puntos.x<-rnorm(100,0,desvi)+punto.real[1]
puntos.y<-rnorm(100,0,desvi)+punto.real[2]
puntos<-cbind(puntos.x,puntos.y)
# crear el espacio de soluciones
space.x<-seq(2,4,0.05)
space.y<-seq(4,6,0.05)
space<-matrix(1,length(space.y),length(space.x))
rownames(space)<-space.y
colnames(space)<-space.x
space
Po<-space/sum(space)
Pr<-space/sum(space)
source("funcionmax.R")
solu<-funcionmax(Po)
