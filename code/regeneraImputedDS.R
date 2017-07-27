#dsOriginal: es el dataset con todas la columnas, las completas y las que tienen valores faltantes
#dsDataImputated: es el dataset que tiene las columnas con los datos imputados
#colNA: es un vector con los indices de las columnas que tenian datos faltantes
regeneraImputedDS<-function(dsOriginal,dsDataImputated,colNA){
  colNaordenada<-colNA[order(colNA)]
  qs_complete<-c()
  qs_complete<-dsOriginal[,1:(colNaordenada[1]-1)]
  qs_complete<-cbind.data.frame(qs_complete,completeSH[,1])
  contColNA<-2
  while(contColNA<=length(colNaordenada)){
    if((colNaordenada[contColNA]-colNaordenada[contColNA-1])>1){
      qs_complete<-cbind.data.frame(qs_complete,dsOriginal[,(colNaordenada[contColNA-1]+1):(colNaordenada[contColNA]-1)])
    }
    qs_complete<-cbind.data.frame(qs_complete,dsDataImputated[,contColNA])
    contColNA<-contColNA+1
  }
  qs_complete<-cbind.data.frame(qs_complete,dsOriginal[,(colNaordenada[contColNA-1]+1):dim(dsOriginal)[2]])
  colnames(qs_complete)<-colnames(dsOriginal)
  return(qs_complete)
}
