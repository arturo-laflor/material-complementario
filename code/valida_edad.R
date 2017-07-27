#Esta función recibe una cadena de que representa una edad  
#la cadena debe contener un número entero de uno o dos dígitos
#la funcion extrae ese patron y regresa la edad en años como
#un string que representa un numero entero

valida_edad=function(strEdad){
  #valorqs<-as.character(responses$QS1)
  if(!is.na(strEdad)){
    qs<-regexpr('[0-9]{1,2}',strEdad)
    if(qs[1]!=-1){
      s3<-substr(strEdad,qs[1],qs[1]+attr(qs,'match.length')-1)
    }else {
      s3<-NA
    }
  }else{
    s3<-NA
  }
  
  return(s3)
}