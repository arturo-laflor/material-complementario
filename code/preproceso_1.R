#quita datos faltantes
#codifica valores capturados
#asigna nombres de columna a los datos
#quita timespam y correo electronico
preproceso_1<-function(tablaDatos){
  
  
  source(file="C:/Master/apuntes-articulo-feature-selection/code/asigna_nombres.R",encoding = "UTF8") 
  source(file="C:/Master/apuntes-articulo-feature-selection/code/cambia_tipo_fc.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/codifica_respuestas.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/cambia_tipo_ci.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/valida_tiempo.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/valida_edad.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/valida_minutos.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/calc_tiempo_sueno.R",encoding = "UTF8")
  
  
  # #elimina el id timestamp
  # #asigna NA a los campos vacíos excepto en el primer campo que es el email.
  # #este campo es opcional por lo tanto en muchos registros no estará.
  tablaDatos<-tablaDatos[-1]
  tablaDatos[,1]<-as.character(tablaDatos[,1])
  tablaDatos[tablaDatos[,1]=='',1]<-'anonimo'
  tablaDatos[tablaDatos=='']<-NA
  
  
  #tablaDatos<-dtQS #para pruebas
  
  # DD1 Edad
  # DD2 Sexo
  # DD3 Ocupación
  # DD4 Tipo de trabajo
  # DD5 Religión
  # DD6 Estado civil
  # DD7 Padecimiento cronico
  # DD8 Crisis
  # SQ1 Hora de acostarse
  # SQ2 Latencia (tiempo en dormirse minutos)
  # SQ3 hora de levantarse
  # SQ4 tiempo durmiendo (horas y fracción)
  # SQ5a problemas por latencia (30 o más minutos) 
  # SQ5b problemas por despertar en la noche o antes de la hora habitual
  # SQ5c problemas por ir al baño
  # SQ5d problemas  para respirar
  # SQ5e problemas por toser o roncar fuertemente
  # SQ5f problemas por demasiado frío
  # SQ5g problemas por demasiado calor
  # SQ5h problemas por pesadillas
  # SQ5i problemas por dolor
  # SQ5j problemas por otros motivos
  # SQ6  Medicamentos para dormir
  # SQ7  somnolencia diurna
  # SQ8 Entusiasmo y energía diurna
  # SQ9 Percepción general de la calidad de sueño
  # SH1.	Siesta
  # SH2.	horario para dormir
  # SH3.	horario para levantarme
  # SH4.	Hago ejercicio por las noches
  # SH5.	Hago ejercicio por las mañanas
  # SH6.	Hago ejercicio por las tardes
  # SH7.	Me quedo en la cama más de lo que debería dos o tres veces a la semana
  # SH8.	Consumo tabaco
  # SH9.	Consumo alcohol
  # SH10.	Consumo cafeína
  # SH11.	jugar video-juegos, navegar en la Internet, hacer limpieza
  # SH12.	Me voy a dormir sintiéndome estresado, enojado, molesto o nervioso
  # SH13.	Uso la cama para cosas que no tienen que ver con dormir
  # SH14.	Duermo en una cama que no es confortable
  # SH15.	iluminación
  # SH16.	ruido
  # SH17.	mucho frío
  # SH18.	mucho calor
  # SH19.	Realizo actividades importantes antes de la hora de dormir (pago de gastos o facturas, itinerarios u horarios, estudio)."
  # SH20.	Me pongo a pensar, planificar o me ocupo de preocupaciones cuando estoy en la cama."
  # SH21.	Ceno pesada 4
  
  #tablaDatos<-datos  #código que se utiliza para hacer pruebas con la función
  
  #asigna nombres a las columnas
  #con el fin de poder manipularlos y codificarlos
  #la funcion también regresa el dataset lleno y con los nuevos nombres de columnas 
  responses<-asigna_nombres(tablaDatos)
  
  #str(responses)
  
  #convierte los datos de factores a caracteres 
  #con el fin de poder manipularlos y codificarlos
  responses<-cambia_tipo_fc(responses)
  
  #codifica las respuestas en escala likert y cambia a integer los valores de respuesta
  responses<-codifica_respuestas(responses)
  
  
  #corrige edad
  responses$DD1<-sapply(responses$DD1,valida_edad) 
  
  #corrige los datos en la columna que tiene que ver con horas y tiempo para que 
  #queden en el formato adecuado
  
  responses$SQ1<-sapply(responses$SQ1,valida_tiempo)
  responses$SQ2<-sapply(responses$SQ2,valida_minutos)
  responses$SQ3<-sapply(responses$SQ3,valida_tiempo)
  responses$SQ4<-sapply(responses$SQ4,valida_tiempo)
  
  #responses<-responses[complete.cases(responses),]
 
  responses$SQ4<-calc_tiempo_sueno(data.frame(responses$SQ1,responses$SQ2,responses$SQ3,responses$SQ4))
  
  responses<-cambia_tipo_ci(responses)
  
  #convierte a double las respuestas de tiempo
  # ifelse(is.na(responses$SQ1),responses$SQ1<-NA,responses$SQ1<-as.double(responses$SQ1))
  # ifelse(is.na(responses$SQ2),responses$SQ2<-NA,responses$SQ2<-as.double(responses$SQ2))
  # ifelse(is.na(responses$SQ3),responses$SQ3<-NA,responses$SQ3<-as.double(responses$SQ3))
  # ifelse(is.na(responses$SQ4),responses$SQ4<-NA,responses$SQ4<-as.double(responses$SQ4))
  # 
  responses$SQ1<-as.double(responses$SQ1)
  responses$SQ2<-as.double(responses$SQ2)
  responses$SQ3<-as.double(responses$SQ3)
  responses$SQ4<-as.double(responses$SQ4)
  #convierte a double las respuestas de tiempo
  
  
 
  return(responses)
  
}