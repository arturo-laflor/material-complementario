elige_no_anonimos<-function(es_datos){
  es_datos<-es_datos[grep("@",es_datos$EMAIL), ]
}