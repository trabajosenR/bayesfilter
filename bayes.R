#### bayes filter #####
desvi<-2
sigma.puntos<- matrix(c(36,0,0,36), ncol=2)
total.datos<-5000
punto.real<-c(3,5)
puntos.x<-rnorm(total.datos,0,desvi)+punto.real[1]
puntos.y<-rnorm(total.datos,0,desvi)+punto.real[2]
puntos<-cbind(puntos.x,puntos.y)
# crear el espacio de soluciones
space.x<-seq(2,4,0.05)
space.y<-seq(4,6,0.05)
space<-matrix(1,length(space.y),length(space.x))
rownames(space)<-space.y
colnames(space)<-space.x
Pr<-space/sum(space) #funcion priori
Po<-space/sum(space) #funcion posteri
## grafica de los puntos
plot(puntos.x,puntos.y)
points(3,5,pch = 19,col = "red")
## inicial filtro
source("funcionmax.R")
library("mvtnorm") # libreria multivariada
solu<-c()
for(gen in 1:length(puntos.x))
{
  Pr<-Po
  temp<-matrix(0,length(space.y),length(space.x))
  for(i in 1:length(space.y))
  {
    for(j in 1:length(space.x))
    {
      temp.p<-dmvnorm(c(puntos.y[gen],puntos.x[gen]),mean=c(space.y[i],space.x[j]),sigma=sigma.puntos)
      temp[i,j]<-temp.p*Pr[i,j]
    }
  }
  temp<-temp/sum(temp)
  Po<-temp
  solu<-rbind(solu,c(space.x[funcionmax(Po)[2]],space.y[funcionmax(Po)[1]]))
  
  ## graficos
  val.x<-rep(punto.real[1],length(puntos.x))
  val.y<-rep(punto.real[2],gen)
  gen.total<-1:length(puntos.x)
  par(mfrow=c(2,1))
  plot(1:gen,solu[,1],type="o",col="green",main="convergencia X",xlab="iteraciones",ylab="valor en X")
  lines(gen.total,val.x,col="blue")
  plot(1:gen,solu[,2],type="o",col="green",main="convergencia Y",xlab="iteraciones",ylab="valor en Y")
  lines(1:gen,val.y,col="blue")
}

## conclusiones
# * la primera conclusion que se tiene de este codigo fue que este tipo de enfoque depende en gran medida de una buena 
#   estimacion de la desviacion estandar.
# * El tamaño de la desvición estandar afecta el desempeño del algoritmo, entre más grande sea la desviación estandar más
#   tiempo le tomará al algoritmo converger en los valores reales. 
# * otro problema encontrado en este enfoque es que si se tiene una muy mala estimación de la desviación estandar se puede llegar a indeterminar
#   las matrices de probabilidades.
# * si se tiene mucha desviacion en los puntos se necesita mucha iteraciones para encontrar la el lugar donde tiene la funciones pero se puede llegar. 



