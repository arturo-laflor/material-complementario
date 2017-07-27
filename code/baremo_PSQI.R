#CALCULA LAS SIGUIENTES COLUMNAS


#SQDUR  duration of sleep
#SQDIS  sleep disturbance
#SQLAT  sleep latency
#SQDD   day disfunction due to sleepiness
#SQEF   sleep efficiency
#SQOQ   overall sleep quality
#SQMS   need meds to sleep
#SQTT   Total (sum of the seven components)
#SQCL   Classification of sleep quality

#dtQS contiene todas las columnas del cuestionario PSQI

baremo_PSQI=function(dtQS,clases){
  
  #se utiliza para las pruebas dentro de la funcion
    # dtQS<-d_crudos 
    # clases=2
    # dtQS<-preproceso_1(dtQS,2)
  #se utiliza para las pruebas dentro de la funcion
  
  
  #SQDUR  duration of sleep
  # PSQIDURAT 		DURATION OF SLEEP
  # IF Q4 > 7, THEN set value to 0
  # IF Q4 < 7 and > 6, THEN set value to 1
  # IF Q4 < 6 and > 5, THEN set value to 2
  # IF Q4 < 5, THEN set value to 3
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  #
  
  f_SQDUR=function(SQ4){
    if(!is.na(SQ4)){
      var<-0
      if(SQ4>=6 && SQ4<7){
        var<-1
      }else if(SQ4>=5 && SQ4<6){
        var<-2
      }else if(SQ4<5){
        var<-3
      }
    }else {var<-NA}
    return(var)
  }

  SQDUR<-sapply(dtQS$SQ4,f_SQDUR)
  dtQS<-cbind.data.frame(dtQS,SQDUR)
  #SQDUR  duration of sleep
  
  #SQDIS  sleep disturbance
  # PSQIDISTB	SLEEP DISTURBANCE
  # IF Q5b + Q5c + Q5d + Q5e + Q5f + Q5g + Q5h + Q5i + Q5j (IF Q5JCOM is null or Q5j 
  #is null, set the value of Q5j to 0) = 0, THEN set value to 0
  # 
  # IF Q5b + Q5c + Q5d + Q5e + Q5f + Q5g + Q5h + Q5i + Q5j (IF Q5JCOM is null or Q5j 
  #is null, set the value of Q5j to 0) > 1 and < 9, THEN set value to 1
  # 
  # IF Q5b + Q5c + Q5d + Q5e + Q5f + Q5g + Q5h + Q5i + Q5j 
  #(IF Q5JCOM is null or Q5j is null, set the value of Q5j to 0) > 9 and < 18, THEN set value to 2
  # 
  # IF Q5b + Q5c + Q5d + Q5e + Q5f + Q5g + Q5h + Q5i + Q5j 
  #(IF Q5JCOM is null or Q5j is null, set the value of Q5j to 0) > 18, THEN set value to 3
  # 
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  #
  f_SQDIS=function(SQ5b_j){
    
    if(!is.na(SQ5b_j)){
      var<-0
      if(SQ5b_j>1 && SQ5b_j<=9){
        var<-1
      }else if(SQ5b_j>9 && SQ5b_j<=18){
        var<-2
      }else if(SQ5b_j>18){
        var<-3
      }
    }else{
      var<-NA
    }
    
    return(var)
  }
  
  
  #SQ5bj<-apply(dtQS[,14:22],1, sum)
  
  tmpSQ5b_j<-cbind.data.frame(dtQS$SQ5b,dtQS$SQ5c,dtQS$SQ5d,dtQS$SQ5e,dtQS$SQ5f,dtQS$SQ5g,dtQS$SQ5h,dtQS$SQ5i,dtQS$SQ5j) 
  SQ5bj<-apply(tmpSQ5b_j,1,sum)
  SQDIS<-sapply(SQ5bj,f_SQDIS)
  dtQS<-cbind.data.frame(dtQS,SQDIS)
  #SQDIS  sleep disturbance
 
  # PSQILATEN 		SLEEP LATENCY
  # First, recode Q2 into Q2new thusly:
  #   IF Q2 > 0 and < 15, THEN set value of Q2new to 0
  # IF Q2 > 15 and < 30, THEN set value of Q2new to 1
  # IF Q2 > 30 and < 60, THEN set value of Q2new to 2
  # IF Q2 > 60, THEN set value of Q2new to 3
  # Next
  # IF Q5a + Q2new = 0, THEN set value to 0
  # IF Q5a + Q2new > 1 and < 2, THEN set value to 1
  # IF Q5a + Q2new > 3 and < 4, THEN set value to 2
  # IF Q5a + Q2new > 5 and < 6, THEN set value to 3
  # 
  f_SQLAT=function(SQ25a){
      
      SQ2<-SQ25a[1] 
      SQ5a<-SQ25a[2]
      if(!is.na(SQ2) && !is.na(SQ5a) ){
        var<-0
        if(SQ2>=0 && SQ2<15){
          var<-0
        }else if(SQ2>=15 && SQ2<29){
          var<-1
        }else if(SQ2>=30 && SQ2<60){
          var<-2  
        }else if(SQ2>=60){
          var<-3
        }
        if(var+SQ5a == 0){return(0)}
        else if(var+SQ5a >= 1 && var+SQ5a <= 2){return(1)}
        else if(var+SQ5a >= 3 && var+SQ5a <= 4){return(2)}
        else{return(3)}
    }else{
      return(NA)
    }
    
    
  }
  
  temppsq25<-cbind.data.frame(dtQS$SQ2,dtQS$SQ5a)
  SQLAT<-apply(temppsq25, 1, f_SQLAT)
  dtQS<-cbind.data.frame(dtQS,SQLAT)
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  #
  
  # PSQIDAYDYS 		DAY DYSFUNCTION DUE TO SLEEPINESS
  # IF Q7 + Q8 = 0, THEN set value to 0
  # IF Q7 + Q8 > 1 and < 2, THEN set value to 1
  # IF Q7 + Q8 > 3 and < 4, THEN set value to 2
  # IF Q7 + Q8 > 5 and < 6, THEN set value to 3
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  #
  f_SQDD=function(SQ78){
    if(!is.na(SQ78)){
      var<-0
      if(SQ78>0 && SQ78<=2){
        var<-1
      }else if(SQ78>2 && SQ78<=4){
        var<-2
      }else if(SQ78>4 && SQ78<=6){
        var<-3  
      }
    }else{
      var<-NA
    }
    
    return(var)
  }
  #temppsq78<-apply(dtQS[,24:25],1,sum)
  temp78<-cbind.data.frame(dtQS$SQ7,dtQS$SQ8)
  temppsq78<-apply(temp78,1,sum)
  SQDD<-sapply(temppsq78,f_SQDD)
  dtQS<-cbind.data.frame(dtQS,SQDD)
  
  # PSQIHSE 		SLEEP EFFICIENCY
  # Diffsec = Difference in seconds between day and time of day Q1 and day Q3
  # Diffhour = Absolute value of diffsec / 3600
  # newtib =IF diffhour > 24, then newtib = diffhour – 24
  # IF diffhour < 24, THEN newtib = diffhour
  # (NOTE, THE ABOVE JUST CALCULATES THE HOURS BETWEEN GNT (Q1) AND GMT (Q3))
  # tmphse = (Q4 / newtib) * 100
  # 
  # IF tmphse > 85, THEN set value to 0
  # IF tmphse < 85 and > 75, THEN set value to 1
  # IF tmphse < 75 and > 65, THEN set value to 2
  # IF tmphse < 65, THEN set value to 3
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  #
 
  f_SQSE=function(SQ134){
    
    SQ1<-as.numeric(SQ134[1])
    SQ3<-as.numeric(SQ134[2])
    SQ4<-as.numeric(SQ134[3])
    
    # SQ1<-1
    # SQ3<-2
    # SQ4<-NA
    # if(!is.na(SQ1) && !is.na(SQ3) && !is.na(SQ4)){
    #   print("entro")
    # }
    # var<-2133
    # if(SQ3==SQ1){
    #   print(paste(SQ1,SQ3,SQ4,sep = ",") )  
    # }
    if(!is.na(SQ1) && !is.na(SQ3) && !is.na(SQ4) && (SQ3!=SQ1)){
      if(SQ1>SQ3){SQ1<-SQ1-12.00}
      SE<-abs(SQ3-SQ1)
      SE= as.double(SQ4/SE*100)
      var<-0
      if(SE>=75 && SE<85){
        var<-1
      }else if(SE>=65 && SE<75){
        var<-2
      }else if(SE<65){
        var<-3  
      }
    }else{
      var<-NA
    }
    return(var)
  }
  temppsq134<-cbind.data.frame(dtQS$SQ1,dtQS$SQ3,dtQS$SQ4)
  SQSE<-apply(temppsq134,1,f_SQSE)
  dtQS<-cbind.data.frame(dtQS,SQSE)
  
  # PSQISLPQUAL	 	OVERALL SLEEP QUALITY
  # Q9
  SQSQ<-dtQS$SQ9
  dtQS<-cbind.data.frame(dtQS,SQSQ)
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  # 
  
  
  # PSQIMEDS 		NEED MEDS TO SLEEP
  # Q6
  SQMS<-dtQS$SQ6
  dtQS<-cbind.data.frame(dtQS,SQMS)
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  #
  
  # PSQI 			TOTAL
  # DURAT + DISTB + LATEN + DAYDYS + HSE + SLPQUAL + MEDS
  # Minimum Score = 0 (better); Maximum Score = 21 (worse)
  
  
  #SQTT<-apply(dtQS[,48:54],1,sum)
  tmpSQTT<-cbind.data.frame(dtQS$SQDUR,dtQS$SQDIS,dtQS$SQLAT,dtQS$SQDD,dtQS$SQSE,dtQS$SQSQ,dtQS$SQMS)
  SQTT<-apply(tmpSQTT,1,sum)
  dtQS<-cbind.data.frame(dtQS,SQTT)
  
  #dtQS[,48:56] #se utiliza para pruebas dentro de la funcion
  
  
  #cuatro clasificaciones
  # Interpretation:  	TOTAL < 5 associated with good sleep quality
  # TOTAL > 5 associated with poor sleep quality
  f_SQCL_4=function(SQTOTAL){
    
    if(!is.na(SQTOTAL)){
      var<-'Excelente'
      if(SQTOTAL>=2 && SQTOTAL<=5){
        var<-'Buena'
      }else if(SQTOTAL>5 && SQTOTAL<=8){
        var<-'Regular'
      }else if(SQTOTAL>8){
        var<-'Pobre'
      }
    }else{
      var<-NA
    }
    
    
    return(var)
  }
  #Tres clasificaciones
  # Interpretation:  	TOTAL < 5 associated with good sleep quality
  # TOTAL > 5 associated with poor sleep quality
  f_SQCL_3=function(SQTOTAL){
    
    
    if(!is.na(SQTOTAL)){
      var<-'Excelente'
      if(SQTOTAL>=2 && SQTOTAL<=5){
        var<-'Buena'
      }else if(SQTOTAL>5){
        var<-'Pobre'
      }
    }else{
      var<-NA
    }
    
    
    
    return(var)
  }
  # Dos clasificaciones
  # Interpretation:  	TOTAL < 5 associated with good sleep quality
  # TOTAL > 5 associated with poor sleep quality
  f_SQCL_2=function(SQTOTAL){
    
    if(!is.na(SQTOTAL)){
      var<-'Excelente'
      if(SQTOTAL>=0 && SQTOTAL<=5){
        var<-'Buena'
      }else if(SQTOTAL>5 && SQTOTAL<=40){
        var<-'Pobre'
      }else if(SQTOTAL>8){
        var<-'Pobre'  
      }
    }else{
      var<-NA
    }
    
    return(var)
  }
  #SQCL<-sapply(SQTT, f_SQCL)
  
  SQCL<-switch (clases,"2_clases"=sapply(SQTT, f_SQCL_2),"3_clases"=sapply(SQTT, f_SQCL_3),"4_clases"=sapply(SQTT, f_SQCL_4))
  
  dtQS<-cbind.data.frame(dtQS,SQCL)
  
}