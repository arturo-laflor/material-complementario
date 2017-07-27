preproceso=function(datos_crudos,numero_clases){
  
  # library(ggplot2)
  # library(bitops)
  # library(RCurl)
  
  
  #Funciones locales
  source(file="C:/Master/apuntes-articulo-feature-selection/code/preproceso_1.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/baremo_PSQI.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/baremo_SHI.R",encoding = "UTF8")
  source(file="C:/Master/apuntes-articulo-feature-selection/code/regeneraImputedDS.R",encoding = "UTF8")
  
  
  # user<-Sys.info()[7]
  # user<-user[[1]]
  # ruta<-paste("C:/Users/",user,"/Google Drive/Doctorado/Tesis/Data/",sep = "")
  # datos_crudos<-read.csv(file=paste(ruta,"responses.csv",sep = ""),header = T,sep = ",")
  
  QS_data<-preproceso_1(datos_crudos)
  
  #```{r recordwithNA-continuous, echo=FALSE,results=FALSE}
  #identifica el índice del registro donde existe un valor faltante
  recNA<-c(which(is.na(QS_data$DD1)),which(is.na(QS_data$SQ1)),which(is.na(QS_data$SQ2)), which(is.na(QS_data$SQ3)),which(is.na(QS_data$SQ4)))
  #elimina los índices repetidos
  recNA<-unique(recNA)
  #elimina los registros donde hay faltantes
  QS_data<-QS_data[-recNA,]
  #reestructura los índices del dataset
  rownames(QS_data)<-1:nrow(QS_data)
  
  
  #esta línea de código identifica valores faltantes en las columnas de la 3 a la 9, como la
  #funcion which regresa los indices de la celda donde se encuetra el faltante tomando cada celda con un valor consecutivo, 
  #del resultado debe extraerse el residuo de la división entre el número de celda y la cantidad de registros en el dataset. 
  #"Esta es la forma de llegar al registro correcto donde está
  #el valor faltante"
  
  recNA<-c(which(is.na(QS_data[,3:9])))%%dim(QS_data)[1]
  
  #elimina los índices repetidos
  recNA<-unique(recNA)
  #elimina los registros donde hay faltantes
  QS_data<-QS_data[-recNA,]
  #reestructura los índices del dataset
  rownames(QS_data)<-1:nrow(QS_data)
  
  #duplicated(recNA) #función que encuentra los duplicados en un vector
  #```
  
  #```{r scoring-outliers, echo=FALSE,results=FALSE}
  library(outliers)
  library(magrittr)
  ic<-0.99
  
  
  #scores regresa un arreglo con valores TRUE/FALSE dependiendo de si el dato se encuentra en el intervalo que se está especificando
  #en este caso ic=0.99 (casi tres desviaciones estandar de la media). el punto en la función which es el arreglo que pasa como parámetro
  #y únicamente regresa el arreglo de indices donde existe un TRUE, es decir donde hay un valor que excede 0.99.
  outSQ4<-scores(QS_data$SQ4,type = "z",prob = ic)%>%which(.)
  outSQ4<-unique(outSQ4)
  #elimina los registros donde hay valores atípicos
  QS_data<-QS_data[-outSQ4,]
  #reestructura los índices del dataset
  rownames(QS_data)<-1:nrow(QS_data)
  
  outSQ2<-scores(QS_data$SQ2,type = "z",prob = ic)%>%which(.)
  outSQ2<-unique(outSQ2)
  #elimina los registros donde hay valores atípicos
  QS_data<-QS_data[-outSQ2,]
  #reestructura los índices del dataset
  rownames(QS_data)<-1:nrow(QS_data)
  
  
  
  library("mice")
  
  #busca las columnas que tienen valores faltantes
  colNA<-(c(which(is.na(QS_data)))%/%dim(QS_data)[1])+1
  
  #elimina los índices repetidos
  colNA<-unique(colNA)
  #construye el dataset con las columnas que tienen datos faltantes para hacer la imputacion
  qs_dataforimp<-QS_data[,colNA]
  
  #Genera 5 datasets de datos para imputar en el conjunto de datos que tiene los valores faltantes
  imputedSH<-mice(data = qs_dataforimp, m = 5, method = "pmm", maxit = 50, seed = 500)
  
  #imputa los datos con el dataset #dos en este caso elegido arbitrariamente  
  completeSH<-complete(imputedSH,2)
  
  
  QS_data_imputed<-regeneraImputedDS(QS_data,completeSH,colNA)
  
  #agrega las siguientes columnas
  #SQDUR  duration of sleep
  #SQDIS  sleep disturbance
  #SQLAT  sleep latency
  #SQDD   day disfunction due to sleepiness
  #SQEF   sleep efficiency
  #SQOQ   overall sleep quality
  #SQMS   need meds to sleep
  #SQTT   Total (sum of the seven components)
  #SQCL   Classification of sleep quality
  QSSHICOMPLETE<-baremo_PSQI(QSREADY,paste(numero_clases,"_clases",sep = ""))
  
  
  #PREPROCESO DE LOS FACTORES DE LA HIGIENE DEL SUEÑO
  
  # SHSTR  stress factors
  # SH11.	jugar video-juegos, navegar en la Internet, hacer limpieza
  # SH12.	Me voy a dormir sintiéndome estresado, enojado, molesto o nervioso
  # SH13.	Uso la cama para cosas que no tienen que ver con dormir
  # SH19.	Realizo actividades importantes antes de la hora de dormir (pago de gastos o facturas, 
  # itinerarios u horarios, estudio)."
  # SH20.	Me pongo a pensar, planificar o me ocupo de preocupaciones cuando estoy en la cama."
  
  
  #SHDIS  disruptors factors   (sum of the corresponding factors)
  # SH14.	Duermo en una cama que no es confortable
  # SH15.	iluminación
  # SH16.	ruido
  # SH17.	mucho frío
  # SH18.	mucho calor
  
  #SHCH   circadian andn homeostatics factors  (sum of the corresponding factors)
  # SH1.	Siesta
  # SH4.	Hago ejercicio por las noches
  # SH5.	Hago ejercicio por las mañanas
  # SH6.	Hago ejercicio por las tardes
  # SH21.	Ceno pesada 
  # SH2.	horario para dormir
  # SH3.	horario para levantarme
  # SH7.	Me quedo en la cama más de lo que debería dos o tres veces a la semana
  
  #SHDG   drugs factors         (sum of the corresponding factors)
  # SH8.	Consumo tabaco
  # SH9.	Consumo alcohol
  # SH10.	Consumo cafeína
  
  #SHTT   Total                 (sum of all factors)
  #SHCL   Classification of sleep hygiene
  QSSHICOMPLETE<-baremo_SHI(QSSHICOMPLETE)
  
  write.csv(file="C:/Users/artur/Google Drive/Doctorado/Tesis/Data/responses_clean_and_imputed.csv",QS_data_imputed)
  
  QSSHICOMPLETE

}