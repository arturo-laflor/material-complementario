graba_recupera_archivo=function(dtQS,numero_clases){
  #graba el archivo y vuelve a cargar los datos
  nombre_archivo<-paste("./preproceso_de_datos/","csvSQ_",numero_clases,".csv",sep = "")
  write.csv(dtQS,file = nombre_archivo)
  dQS<-read.csv(file=nombre_archivo,header = TRUE,sep = ",")
  dQS<-dQS[-1] #quita el campo id que asigna automáticamente el comando writecvs
  #graba el archivo y vuelve a cargar los datos
}