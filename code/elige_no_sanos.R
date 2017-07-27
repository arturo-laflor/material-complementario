elige_no_sanos<-function(es_datos){
   
  # pad_cronico<-es_datos[(as.factor(es_datos$DD7)!='Ninguno'),]
   
  # crisis<-es_datos[(as.factor(es_datos$DD8)!='No'),]
   
  # problemas_respirar<-es_datos[(es_datos$SQ5d>1),]
   
  # problemas_roncar<-es_datos[(es_datos$SQ5e>1),]
   
  # problemas_dolor<-es_datos[(es_datos$SQ5i>0),]
   
  # problemas_otros_motivos<-es_datos[(es_datos$SQ5j>1),]
   
  # toma_medicamentos<-es_datos[(es_datos$SQ6>0),]
  
  no_sanos<-es_datos[(as.factor(es_datos$DD7)!='Ninguno') | 
                       (as.factor(es_datos$DD8)!='No') | #Está pasando por algún tipo de crisis DD8!=0
                       (es_datos$SQ5d>1) | #Personas que tiene problemas por respirar   SQ5d > 1 
                       (es_datos$SQ5e>1) | #Personas que tiene probelmas por roncar     SQ5e > 1
                       (es_datos$SQ5i>0) | #Personas que no duermen por tener dolor     SQ5i > 0
                       (es_datos$SQ5j>1) | #Personas que no duermen por otros motivos   SQ5j > 1
                       (es_datos$SQ4<6) | #Personas que duermen menos a 6 horas   SQ4 < 6
                       (es_datos$SQ6>0),] #Personas que toman medicamentos para dormir SQ6 > 0
  
  }