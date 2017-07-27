library(ggplot2)
library(bitops)
library(RCurl)


#Funciones locales
source(file="C:/Master/apuntes-articulo-feature-selection/code/preproceso_1.R",encoding = "UTF8")
source(file="C:/Master/apuntes-articulo-feature-selection/code/baremo_PSQI.R",encoding = "UTF8")
source(file="C:/Master/apuntes-articulo-feature-selection/code/baremo_SHI.R",encoding = "UTF8")


#funciones online
script <- getURL("https://raw.githubusercontent.com/arturo-laflor/util-R-codes/master/QOfCategoricalF.R", ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)

script <- getURL("https://raw.githubusercontent.com/arturo-laflor/util-R-codes/master/QOfContinuousF.R", ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)

script <- getURL("https://raw.githubusercontent.com/arturo-laflor/util-R-codes/master/multiplot.R", ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)




#Lee el dataset como quedaba con el preproceso que ten'ia errores
user<-Sys.info()[7]
user<-user[[1]]
ruta<-paste("C:/Users/",user,"/Google Drive/Doctorado/Tesis/Data/",sep = "") 
d_resp_Bad_script<-read.csv(file=paste(ruta,"responses_after_preproc_1.1.csv",sep = ""),header = T,sep = ",")

d_resp_Bad_script<-d_resp_Bad_script[-1]
#Lee el dataset como quedaba con el preproceso que ten'ia errores

#```{r report-of-quality-contfeat-bad-script}
dfCONTFEAT<-cbind.data.frame(DD1=d_resp_Bad_script[,2],d_resp_Bad_script[,10:13])
DRCONT<-QOfContinuousF(dfCONTFEAT)

#```

#```{r histograms-bocplots, echo=FALSE,message=FALSE,warning=FALSE,results=FALSE}
# library(ggplot2)
# 
# histDD1<-ggplot(data=d_resp_Bad_script,aes(DD1))+geom_histogram(bins = 30)+labs(y="Count")
# histSQ1<-ggplot(data=d_resp_Bad_script,aes(SQ1))+geom_histogram(bins = 30)+labs(y="Count")
# histSQ2<-ggplot(data=d_resp_Bad_script,aes(SQ2))+geom_histogram(bins = 30)+labs(y="Count")
# histSQ3<-ggplot(data=d_resp_Bad_script,aes(SQ3))+geom_histogram(bins = 30)+labs(y="Count")
# histSQ4<-ggplot(data=d_resp_Bad_script,aes(SQ4))+geom_histogram(bins = 30)+labs(y="Count")
# 
# boxplotDD1<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),DD1))+geom_boxplot()+labs(x="Gender")
# boxplotSQ1<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ1))+geom_boxplot()+labs(x="Gender")
# boxplotSQ2<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ2))+geom_boxplot()+labs(x="Gender")
# boxplotSQ3<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ3))+geom_boxplot()+labs(x="Gender")
# boxplotSQ4<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ4))+geom_boxplot()+labs(x="Gender")
# 
# 
# multiplot(histDD1,histSQ1,histSQ2,histSQ3,histSQ4,boxplotDD1,boxplotSQ1,boxplotSQ2,boxplotSQ3,boxplotSQ4,cols=2)
#```

# ```{r report-of-quality-catdemofeat-bad-script}
# DRCATDEMO<-QOfCategoricalF(cbind.data.frame(d_resp_Bad_script[,3:9]))
# 
# ```

# ```{r report-of-quality-catshfeat}
# DRCATSH<-QOfCategoricalF(cbind.data.frame(d_resp_Bad_script[,28:48]))
# 
# write.csv(DRCATSH,file = paste(ruta,"quality-reposrt-raw-SH.csv",sep = ""))
# ```

#```{r preproceso-sin-errores}
user<-Sys.info()[7]
user<-user[[1]]
ruta<-paste("C:/Users/",user,"/Google Drive/Doctorado/Tesis/Data/",sep = "") 
d_crudos<-read.csv(file=paste(ruta,"responses.csv",sep = ""),header = T,sep = ",")
QS_data<-preproceso_1(d_crudos,2)
#```


# ```{r data-quality-report-contfeat}
# dfCONTFEAT<-cbind.data.frame(DD1=QS_data[,2],QS_data[,10:13])
# DRCONT<-QOfContinuousF(dfCONTFEAT)
# 
# ```


# ```{r data-quality-report-catdemofeat}
# DRCATDEMO<-QOfCategoricalF(cbind.data.frame(QS_data[,3:9]))
# #DRCATDEMO
# ```



#```{r recordwithNA-continuous, echo=FALSE,results=FALSE}
#identifica el índice del registro donde existe un valor faltante
recNA<-c(which(is.na(QS_data$DD1)),which(is.na(QS_data$SQ1)),which(is.na(QS_data$SQ2)), which(is.na(QS_data$SQ3)),which(is.na(QS_data$SQ4)))
#elimina los índices repetidos
recNA<-unique(recNA)
#elimina los registros donde hay faltantes
QS_data<-QS_data[-recNA,]
#reestructura los índices del dataset
rownames(QS_data)<-1:nrow(QS_data)

#duplicated(recNA) #función que encuentra los duplicados en un vector
#```

#```{r recordwithNA-categorical-demographics, echo=FALSE,results=FALSE}
#identifica el índice del registro donde existe un valor faltante

#recNA<-c(which(is.na(QS_data$DD2)),which(is.na(QS_data$DD3)),which(is.na(QS_data$DD4)), which(is.na(QS_data$DD5)),which(is.na(QS_data$DD6)))

#esta línea de código identifica valores faltantes en las columnas de la 3 a la 9, como la
#funcion which regresa los indices de la celda donde se encuetra el faltante tomando cada celda con un valor consecutivo, del resultado debe extraerse el residuo de la división entre el número de celda y la cantidad de registros en el dataset. "Esta es la forma de llegar al registro correcto donde está
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

#```


####################DATA IMPUTATION
# ```{r pattern-of-missing-values,echo=FALSE,results=FALSE}
# #library("mice")
# #library("VIM")
# 
# #md.pattern(QS_data[,14:48])
# 
# ```

# ```{r plot-of-missing-values,echo=FALSE}
# # dsplotmissingvalues<-data.frame(QS_data[,14:20],QS_data[,30:44]) 
# # 
# # plotofmv<-aggr(dsplotmissingvalues,col=c('darkgrey','black'),
# #                  numbers=TRUE,labels=names(dsplotmissingvalues),
# #                  cex.axis=.7,gap=3,ylab=c("Missing data","Pattern"))
# 
# ```

# ```{r data-imputation-process,echo=FALSE,results=FALSE}
# # qs_dataforimp<-cbind.data.frame(SQ5a=QS_data$SQ5a,SQ5c=QS_data$SQ5c, SH4=QS_data$SH4,SH6=QS_data$SH6,SH7=QS_data$SH7,SH11=QS_data$SH11,SH15=QS_data$SH15,SH17=QS_data$SH17) 
# # imputedSH<-mice(data = qs_dataforimp, m = 5, method = "pmm", maxit = 50, seed = 500)
# # summary(imputedSH)
# # 
# # imputeddata<-rbind.data.frame(imputedSH$imp$SQ5a,imputedSH$imp$SQ5c, imputedSH$imp$SH4,imputedSH$imp$SH6,imputedSH$imp$SH7,imputedSH$imp$SH11,imputedSH$imp$SH15,imputedSH$imp$SH17)
# ```

# ```{r completing-missing-values,echo=FALSE,results=FALSE}
# # completeSH<-complete(imputedSH,4)
# # md.pattern(completeSH)
# ```

# ```{r regenerating-dataset,echo=FALSE,results=FALSE}
# # QS_data_imputed<-cbind.data.frame(QS_data[,1:13],SQ5a=completeSH$SQ5a,SQ5b=QS_data$SQ5b,SQ5c=completeSH$SQ5c, QS_data[,17:30],SH4=completeSH$SH4,SH5=QS_data$SH5,completeSH[,4:5],QS_data[,35:37],SH11=completeSH$SH11,QS_data[,39:41],SH15=completeSH$SH15,SH16=QS_data$SH16,SH17=completeSH$SH17,QS_data[,45:48])
# # write.csv(file="C:/Users/artur/Google Drive/Doctorado/Tesis/Data/responses_clean_and_imputed.csv",QS_data_imputed)
# 
# ```


# ```{r}
# QSREADY<-read.csv(file=paste(ruta,"responses_clean_and_imputed.csv",sep = ""),header = T,sep = ",")
# QSREADY<-QSREADY[-1]
# 
# QofDSH<-QOfCategoricalF(QSREADY[,28:48])
# 
# write.csv(QofDSH,file = paste(ruta,"qofdsh.csv",sep = ""))
# ```


# ```{r}
# QSSHICOMPLETE<-baremo_PSQI(QSREADY,"2_clases")
# QSSHICOMPLETE<-baremo_SHI(QSSHICOMPLETE)
# 
# write.csv(QSSHICOMPLETE,file = paste(ruta,"QSSHICOMPLETE.csv",sep = ""))
# 
# ```
