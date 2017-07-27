calcula_por_grupos<-function(ds,funcion,nombres,columna_grupo,columna_dato){
  z<-aggregate.data.frame(ds[columna_dato],list(ds[,columna_grupo]),funcion)
  names(z)<-(nombres)
  return(z)
}

