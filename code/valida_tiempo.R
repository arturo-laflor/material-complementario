#Esta función recibe una cadena de tiempo válida que puede ser un número entero o una 
#cadena con un patron como 10:22, 11:00, 15:00, 13:40 etc.
#la funcion extrae ese patron y regresa el valor de tiempo en 
#una cadena que representa un número real con dos dígitos
#si no encuentra el patron regresa NA

#NOTA: En una expresión regular si se tiene en el patrón un '.' 
#y en el estring vienen ':', lo toma como váido, "investigar porqué"

valida_tiempo=function(strQs1){
  #valorqs<-as.character(responses$QS1)
  if(!is.na(strQs1)){
    qs<-regexpr('[0-9]{1,2}[:|/|.][0-9]{1,2}',strQs1)
    if(qs[1]!=-1){
      qss<-substr(strQs1,qs[1],qs[1]+attr(qs,'match.length')-1)
      pos<-regexpr('[:|/|.]',qss)
      s1<-as.numeric(substr(qss,1,pos[1]-1))
      if(s1>12){s1<-as.numeric(s1)-12}
      s2<-as.numeric(substr(qss,pos[1]+1,attr(qs,'match.length')))
      s2=s2/60
      if(s1>12 || s2>=1){
        s3=NA
      }else{
        s3=as.character(round(s1+s2,digits = 2))
      }
      
    }else{
      qs<-regexpr('[0-9]{1,2}-[0-9]{1,2}',strQs1)
      if(qs[1]!=-1){
        qss<-substr(strQs1,qs[1],qs[1]+attr(qs,'match.length')-1)
        pos<-regexpr('-',qss)
        s3<-as.numeric(substr(qss,1,pos[1]-1))
      }else{
        qs<-regexpr('[0-9]{1,2}',strQs1)
        if(qs[1]!=-1){
          s3<-substr(strQs1,qs[1],qs[1]+attr(qs,'match.length')-1)
          if(as.numeric(s3)>24){
            s3<-NA
          }else if(as.numeric(s3)>12){
            s3<-as.character(as.numeric(s3)-12)
          }
        }else {
          s3<-NA
        }
      } 
    }
  }else{
    s3<-NA
  }
  
  tryCatch(
    {
      if(!is.na(s3)){
        if(as.numeric(s3)<0){
          s3<-NA
        }else if(as.numeric(s3)<1){
          s3=as.numeric(s3)+12
        }else{
          s3<-as.numeric(s3)
        }
      }
      
    },
    error=function(e) {
      message(paste(e,", s3=",s3,sep = "") )
    },
    finally = {
      
    }
  )
  
  
  return(s3)
}