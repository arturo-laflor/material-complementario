convierte_binaria<-function(sqdt){
  
  cnv<-function(celda){
    if(celda==0 | celda==1 | identical(as.character(celda),'Buena')) {celda=0} else{celda=1}
  }
  
  tempdt<-as.data.frame(apply(sqdt, c(1,2),cnv))
}

