elige_sanos<-function(es_datos){
  #Tiene algún padecimient crónico DD7 != 0
  #levels de DD7 "Depresión"    "Diabetes"     "Hipertensión" "Ninguno"      "Otro" 
  es_datos<-es_datos[as.factor(es_datos$DD7)=='Ninguno',]
  #Está pasando por algún tipo de crisis DD8!=0
  es_datos<-es_datos[as.factor(es_datos$DD8)=='No',]
  #Personas que duermen menos de 6 horas SQ4 < 6.0
  es_datos<-es_datos[es_datos$SQ4>5.9,]
  #Personas que tiene problemas por respirar   SQ5d > 0
  es_datos<-es_datos[es_datos$SQ5d<2,]
  
  #Personas que tiene probelmas por roncar     SQ5e > 0
  es_datos<-es_datos[es_datos$SQ5e<2,]
  #Personas que no duermen por tener dolor     SQ5i > 0
  es_datos<-es_datos[es_datos$SQ5i<1,]
  #Personas que no duermen por otros motivos   SQ5j > 0
  es_datos<-es_datos[es_datos$SQ5j<2,]
  #Personas que toman medicamentos para dormir SQ6 > 0
  es_datos<-es_datos[es_datos$SQ6<1,]
}