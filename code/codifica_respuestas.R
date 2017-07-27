codifica_respuestas=function(dataSet){
  
  #dataSet<-responses #para prueba
  
  dataSet[dataSet=='Nunca']<-0
  dataSet[dataSet=='Casi nunca']<-1
  dataSet[dataSet=='Algunas veces']<-2
  dataSet[dataSet=='Frecuentemente']<-3
  dataSet[dataSet=='Siempre']<-4
  
  dataSet[dataSet=='Nunca']<-0
  dataSet[dataSet=='Menos de una vez por semana']<-1
  dataSet[dataSet=='Una o dos veces por semana']<-2
  dataSet[dataSet=='Tres o más veces por semana']<-3
  
  dataSet[dataSet=='Nada difícil']<-0
  dataSet[dataSet=='Más o menos difícil']<-1
  dataSet[dataSet=='Poco difícil']<-2
  dataSet[dataSet=='Muy difícil']<-3
  
  dataSet[dataSet=='Muy buena']<-0
  dataSet[dataSet=='Más o menos buena']<-1
  dataSet[dataSet=='Más o menos mala']<-2
  dataSet[dataSet=='Muy mala']<-3
  
  #dataSet[dataSet=='Femenino']<-1
  #dataSet[dataSet=='Masculino']<-2
  
  # dataSet[dataSet=='Docente']<-1
  # dataSet[dataSet=='Empleado']<-2
  # dataSet[dataSet=='Profesional independiente']<-3
  # dataSet[dataSet=='Estudiante']<-4
  # dataSet[dataSet=='Otro']<-5
  
  # dataSet[dataSet=='Mental']<-1
  # dataSet[dataSet=='Físico']<-2
  # dataSet[dataSet=='Más mental que físico']<-3
  # dataSet[dataSet=='Más físico que mental']<-4
  
  # dataSet[dataSet=='ASD']<-1
  # dataSet[dataSet=='Católica']<-2
  # dataSet[dataSet=='Testigo de Jehová']<-3
  # dataSet[dataSet=='Evangélica']<-4
  # dataSet[dataSet=='Otra']<-5
  
  # dataSet[dataSet=='Casada(o)']<-1
  # dataSet[dataSet=='Soltera(o)']<-2
  # dataSet[dataSet=='Divorciada(o)']<-3
  # dataSet[dataSet=='Unión Libre']<-4
  
  ifelse(is.na(dataSet$DD6),dataSet$DD6<-NA,dataSet[dataSet$DD6=='Unión libre',7]<-"Unión Libre")
  #dataSet[dataSet$DD6=='Unión libre',7]<-"Unión Libre"
  
  # dataSet[dataSet=='Otro']<-5
  
  # dataSet[dataSet=='Ninguno']<-1
  # dataSet[dataSet=='Hipertensión']<-2
  # dataSet[dataSet=='Diabetes']<-3
  # dataSet[dataSet=='Depresión']<-4
  # dataSet[dataSet=='Otro']<-5
  
  # dataSet[dataSet=='No']<-1
  # dataSet[dataSet=='Financiera']<-2
  # dataSet[dataSet=='Divorcio']<-3
  # dataSet[dataSet=='Pérdida de un familiar cercano']<-4
  # dataSet[dataSet=='Otra']<-5
  
  
  return(dataSet)
}