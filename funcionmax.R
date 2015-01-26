funionmax<-function(Po)
for(i in nrow(Po))
{
  for(j in ncol(Po))
  {
    if(Po[i,j]==max(Po))
    {
      salida<-c(i,j)
      break
    }
  }
}
return(salida)