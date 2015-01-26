#### bayes filter #####
setwd( "/Users/davidalejandro/Documents/Google Drive/R2015//bayesfilter") # se debe configurar para el caso particular
desvi<-2
sigma.puntos<- matrix(c(4,0,0,4), ncol=2)
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
Pr<-space/sum(space) #funcion priori
Po<-space/sum(space) #funcion posteri

source("funcionmax.R")
library("mvtnorm") # libreria multivariada
solu<-funcionmax(Po)
for(gen in 1:length(puntos.x))
{
  Pr<-Po
  temp<-matrix(0,length(space.y),length(space.x))
  for(i in 1:length(space.y))
  {
    for(j in 1:length(space.x))
    {
      temp[i,j]<-dmvnorm(c(puntos.x[gen],puntos.y[gen]),mean=c(space.y[i],space.x[j]),sigma=sigma.puntos)
    }
  }
  temp<-temp/sum(temp)
  Po<-Pr*temp
  solu<-rbind(solu,funcionmax(Po))
}


