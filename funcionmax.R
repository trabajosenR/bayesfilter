funcionmax<-function(Po)
{
  salida<-c(1,1)
  for(i in nrow(Po))
  {
    for(j in ncol(Po))
    {
      if(Po[i,j]==max(Po))
      {
        salida<-c(i,j)
      }
    }
  }
  return(salida)
  
}
