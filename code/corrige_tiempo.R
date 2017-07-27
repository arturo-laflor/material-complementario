corrige_tiempo=function(columnaDataSet){
  
   valorqs<-as.character(columnaDataSet)
   var<-c(rep(0,length(valorqs)))
   for (i in 1:length(valorqs)) {
     var[i]<-valida_tiempo(valorqs[i])
   }
  
  return(var)

}