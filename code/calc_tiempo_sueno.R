
#debe entrar un dataset con los cuatro parámetros que miden el sueño en el PSQI (SQ1,SQ2,SQ3,SQ4) en ese orden
calc_tiempo_sueno=function(dtsleep){
  
  calcula_sueno<-function(SQ134){
    HD<-as.numeric(SQ134[1])
    HL<-as.numeric(SQ134[3])
    minutos<-as.numeric(SQ134[2])
    if(!is.na(HD) && !is.na(HL) && !is.na(minutos)){
      if(HD>=HL){HD<-HD-12.00}
      SE<-abs(HL-HD)
      SE<-SE-round(minutos/60,digits = 2)
    }else{return(NA)}
    
  }
  
  
  SQSE<-apply(dtsleep,1,calcula_sueno)
  
}

# user<-Sys.info()[7]
# user<-user[[1]]
# 
# source(file=paste("C:/Users/",user,"/OneDrive/R util-codes/shqs_general_functions/regresa_datos_completos.R",sep = "")
#          ,encoding = "UTF8")
# library(magrittr)
# shqs<-regresa_datos_completos()%$%data.frame(SQ1,SQ2,SQ3,SQ4)




