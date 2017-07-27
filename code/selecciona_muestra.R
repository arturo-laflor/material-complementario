selecciona_muestra<-function(dSQ,muestra,num_clases){
  
  #dSQ<-read.csv(file=paste("./preproceso_de_datos/csvSQ_",num_clases,"_clases.csv",sep = ""),header = T,sep = ",")
  
  if(identical(muestra,"Sanos")){
    #elimina los registros que no cumplen con los siguientes criterios
    #Tiene algún padecimient crónico DD7 != 0
    #Está pasando por algún tipo de crisis DD8!=0
    #Personas que duermen menos de 6 horas SQ4 < 6.0
    #Personas que tiene problemas por respirar   SQ5d > 0
    #Personas que tiene probelmas por roncar     SQ5e > 0
    #Personas que no duermen por tener dolor     SQ5i > 0
    #Personas que no duermen por otros motivos   SQ5j > 0
    #Personas que toman medicamentos para dormir SQ6 > 0
    dSQ<-elige_sanos(dSQ)
  }else if(identical(muestra,"No sanos")){
    #Tiene algún padecimient crónico DD7 != 0 ||
    #Está pasando por algún tipo de crisis DD8!=0 ||
    #Personas que tiene problemas por respirar   SQ5d > 1 ||
    #Personas que tiene probelmas por roncar     SQ5e > 1 ||
    #Personas que no duermen por tener dolor     SQ5i > 0 ||
    #Personas que no duermen por otros motivos   SQ5j > 1 ||
    #Personas que toman medicamentos para dormir SQ6 > 0  ||
    dSQ<-elige_no_sanos(dSQ)
  }else if(identical(muestra,"No anónimos")){
    dSQ<-elige_no_anonimos(dSQ)
  }else if(identical(muestra,"Anónimos")){
    dSQ<-elige_anonimos(dSQ)
  }
  
  return(dSQ)
  
}