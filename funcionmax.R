funcionmax<-function(Po)
{
  salida<-c(1,1)
  for(i in 1:nrow(Po))
  {
    for(j in 1:ncol(Po))
    {
      if(Po[i,j]==max(Po))
      {
        salida<-c(i,j)
      }
    }
  }
  return(salida) 
}
