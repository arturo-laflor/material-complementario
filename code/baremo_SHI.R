baremo_SHI=function(dtQS){
  #SHSTR  stress factors
  # SH11.	jugar video-juegos, navegar en la Internet, hacer limpieza
  # SH12.	Me voy a dormir sintiéndome estresado, enojado, molesto o nervioso
  # SH13.	Uso la cama para cosas que no tienen que ver con dormir
  # SH19.	Realizo actividades importantes antes de la hora de dormir (pago de gastos o facturas, itinerarios u horarios, estudio)."
  # SH20.	Me pongo a pensar, planificar o me ocupo de preocupaciones cuando estoy en la cama."
  
  tmpSHSTR<-cbind.data.frame(dtQS$SH11,dtQS$SH12,dtQS$SH13,dtQS$SH19,dtQS$SH20)
  SHSTR<-apply(tmpSHSTR, 1, sum)
  dtQS<-cbind.data.frame(dtQS,SHSTR)
  
  #SHDIS  disruptors factors   (sum of the corresponding factors)
  # SH14.	Duermo en una cama que no es confortable
  # SH15.	iluminación
  # SH16.	ruido
  # SH17.	mucho frío
  # SH18.	mucho calor
  
  tmpSHDIS<-cbind.data.frame(dtQS$SH14,dtQS$SH15,dtQS$SH16,dtQS$SH17,dtQS$SH18)
  SHDIS<-apply(tmpSHDIS, 1, sum)
  dtQS<-cbind.data.frame(dtQS,SHDIS)
  
  
  #SHCH   circadian and homeostatics factors  (sum of the corresponding factors)
  # SH1.	Siesta
  # SH2.	horario para dormir
  # SH3.	horario para levantarme
  # SH4.	Hago ejercicio por las noches
  # SH7.	Me quedo en la cama más de lo que debería dos o tres veces a la semana
  # SH21.	Ceno pesada 
  
  #preguntar como se puede hacer con estos factores que son positivos para el bien dormir
  #contrario a todos los demás
  # SH5.	Hago ejercicio por las mañanas
  # SH6.	Hago ejercicio por las tardes
  
  
  tmpSHCH<-cbind.data.frame(dtQS$SH1,dtQS$SH2,dtQS$SH3,dtQS$SH4,dtQS$SH7,dtQS$SH21)
  SHCH<-apply(tmpSHCH, 1, sum)
  dtQS<-cbind.data.frame(dtQS,SHCH)
  
  
  #SHDG   drugs factors         (sum of the corresponding factors)
  # SH8.	Consumo tabaco
  # SH9.	Consumo alcohol
  # SH10.	Consumo cafeína
  tmpSHDG<-cbind.data.frame(dtQS$SH8,dtQS$SH9,dtQS$SH10)
  SHDG<-apply(tmpSHDG, 1, sum)
  dtQS<-cbind.data.frame(dtQS,SHDG)
  
  
  #SHTT   Total                 (sum of all factors)
  tmpSHTT<-cbind.data.frame(tmpSHDG,tmpSHCH,tmpSHDIS,tmpSHSTR)
  SHTT<-apply(tmpSHTT, 1, sum)
  dtQS<-cbind.data.frame(dtQS,SHTT)
  
  #SHCL   Classification of sleep hygiene
  return(dtQS)
  
}

