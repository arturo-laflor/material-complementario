
library(ggplot2)
library(bitops)
library(RCurl)

#funciones online
script <- getURL("https://raw.githubusercontent.com/arturo-laflor/util-R-codes/master/QOfCategoricalF.R", ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)

script <- getURL("https://raw.githubusercontent.com/arturo-laflor/util-R-codes/master/QOfContinuousF.R", ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)

script <- getURL("https://raw.githubusercontent.com/arturo-laflor/util-R-codes/master/multiplot.R", ssl.verifypeer = FALSE)
eval(parse(text = script),envir=.GlobalEnv)


#Es necesario poner encoding=utf8 para que los acentossean tratados como acentos

#Taking user info to establish the appropriate path to public functions
user<-Sys.info()[7]
user<-user[[1]]

#loading private functions

source(file="./code/preproceso.R",encoding = "UTF8")
source(file="./code/selecciona_muestra.R",encoding = "UTF8")
source(file="./code/elimina_faltantes.R",encoding = "UTF8")
source(file="./code/convierte_binaria.R",encoding = "UTF8")
source(file="./code/preproceso_1.R",encoding = "UTF8")
source(file="./code/baremo_PSQI.R",encoding = "UTF8")
source(file="./code/baremo_SHI.R",encoding = "UTF8")
source(file="./code/graba_recupera_archivo.R",encoding = "UTF8")
source(file="./code/preproceso.R",encoding = "UTF8")
source(file="./code/elige_sanos.R",encoding = "UTF8")
source(file="./code/elige_no_sanos.R",encoding = "UTF8")
source(file="./code/elige_no_anonimos.R",encoding = "UTF8")
source(file="./code/elige_anonimos.R",encoding = "UTF8")
source(file="./code/calcula_por_grupos.R",encoding = "UTF8")
source(file="./code/asigna_nombres.R",encoding = "UTF8")



## Missing values

ruta<-paste("C:/Users/",user,"/Google Drive/Doctorado/Tesis/Data/",sep = "") 
d_crudos<-read.csv(file=paste(ruta,"responses.csv",sep = ""),header = T,sep = ",")

dt<-d_crudos[-1]
dt[,1]<-as.character(dt[,1])
dt[dt[,1]=='',1]<-'anonimo'

#asigna NA a los campos vacios excepto en el primer campo que es el email.
#este campo es opcional por lo tanto en muchos registros no estara.
dt[dt=='']<-NA

dt<-asigna_nombres(dt)
dt<-dt[-1]

write.csv(dt,file = paste(ruta,"respNA.csv",sep = ""))

#se utiliza el paquete amelia para generar el mapa de datos faltantes
#AmeliaView()

#Selecciona datos faltantes y los guarda en un archivo
dtna<-dt[!complete.cases(dt),]
dtna<-dtna[-1]
write.csv(dt,file = paste(ruta,"respNA.csv",sep = ""))


dtna<-dtna[-c(16,17,21),]

countofna<-data.frame(NAs=apply(dtna,2,FUN = function(x) length(x[is.na(x)])))

xmaycero<-apply(countofna,2,FUN = function(x) x[x>0])

xmaycero<-data.frame(P_Variable=row.names(xmaycero),xmaycero)

#gb<-ggplot(data=xmaycero,aes(P_Variable,NAs))+geom_col(width = 0.5,aes(fill=NAs))
gb<-ggplot(data=xmaycero,aes(P_Variable,NAs))+geom_col(width = 0.5)


## Data quality

d_preproc<-preproceso(d_crudos,"2_clases")

#write.csv(d_preproc,file="datapreproc.csv",col.names = T)


library(DT)
SQDUR<-cbind.data.frame(d_preproc[,10:13])
QualityTableSQDUR<-QOfContinuousF(SQDUR)
#datatable(QualityTableSQDUR)
#QualityTableSQDUR

SHI10<-cbind.data.frame(d_preproc[,28:37])
QualityTableSHI10<-QOfCategoricalF(SHI10)
#QualityTableSHI10

str(d_preproc$DD2)

d_preproc$DD2<-as.character(d_preproc$DD2)

d_preproc[d_preproc$DD2=='Femenino',3]<-'Female' 
d_preproc[d_preproc$DD2=='Masculino',3]<-'Male'




histSQ1<-ggplot(data=d_preproc,aes(SQ1))+geom_histogram(bins = 30)+labs(y="Count")
histSQ2<-ggplot(data=d_preproc,aes(SQ2))+geom_histogram(bins = 30)+labs(y="Count")
histSQ3<-ggplot(data=d_preproc,aes(SQ3))+geom_histogram(bins = 30)+labs(y="Count")
histSQ4<-ggplot(data=d_preproc,aes(SQ4))+geom_histogram(bins = 30)+labs(y="Count")

boxplotSQ1<-ggplot(data=d_preproc, aes(as.factor(DD2),SQ1))+geom_boxplot()+labs(x="Gender")
boxplotSQ2<-ggplot(data=d_preproc, aes(as.factor(DD2),SQ2))+geom_boxplot()+labs(x="Gender")
boxplotSQ3<-ggplot(data=d_preproc, aes(as.factor(DD2),SQ3))+geom_boxplot()+labs(x="Gender")
boxplotSQ4<-ggplot(data=d_preproc, aes(as.factor(DD2),SQ4))+geom_boxplot()+labs(x="Gender")


multiplot(histSQ1,histSQ2,histSQ3,histSQ4,boxplotSQ1,boxplotSQ2,boxplotSQ3,boxplotSQ4,cols=2)

