valida_minutos=function(strMinutos){
  #valorqs<-as.character(responses$QS1)
  if(!is.na(strMinutos)){
    qs<-regexpr('[0-9]{1,2}',strMinutos)
    if(qs[1]!=-1){
      s3<-substr(strMinutos,qs[1],qs[1]+attr(qs,'match.length')-1)
      if(as.numeric(s3)>60){s3<-'60'}
    }else {
      s3<-NA
    }
  }else{
    s3<-NA
  }
  
  return(s3)
}