dSQ<-read.csv(file="C:/Master/Tesis/Quinto avance (Local)/preproceso_de_datos/csvSQ.csv",header = TRUE,sep = ",")

corre<-cor(cbind.data.frame(dSQ$SHSTR,dSQ$SHDIS,dSQ$SHCH,dSQ$SHDG,dSQ$SHTT),dSQ$SQTT,method = "spearman")
corre<-cor(cbind.data.frame(dSQ$SHSTR,dSQ$SHDIS,dSQ$SHCH,dSQ$SHDG,dSQ$SHTT),dSQ$SQTT,method = "pearson")
corre


library("ggplot2")

qplot(dSQ$DD1,bins=30)
g<-ggplot(dSQ,aes(factor(DD2),fill=SQCL))+geom_bar()+
  labs(x = "Sexo",y = "Calidad del Sueño",fill="SQ")
g+geom_text(aes(x=DD2,label=dSQ$DD2))




library("psych")
dsalfa<-cbind.data.frame(dSQ[,28:48],dSQ[,49:54])
dsalfa<-cbind.data.frame(dSQ[,28:31],dSQ[,34:48],dSQ[,49:54])

dsalfa<-cbind.data.frame(dSQ[,58:61],dSQ[,49:54])

alpha(dsalfa,check.keys = TRUE)




dsSanos<-dSQ[(dSQ$DD7==1 & dSQ$DD8==1),]
corre<-cor(cbind.data.frame(dSQ$SQTT,dSQ$SHSTR),method = "spearman")


corre

nombres<-names(cbind.data.frame(dSQ[,2:56],dSQ[,58:62]))

indices_correla<-cbind.data.frame(corre,nombres)
indices_correla[order(indices_correla,decreasing = TRUE),]




SQ4
DD5
SQTT

grafica<-ggplot(data=dSQ,aes(x=SQ4,y=SQTT))+geom_point(aes(colour=SQCL))
grafica+labs(x = "Duración",y = "Escala PSQI",fill="Clasificación")

grafica<-ggplot(data=dSQ,aes(x=SQ4,y=SQTT))+geom_point(aes(colour=DD5))
grafica+labs(x = "Duración",y = "Escala PSQI",fill="Clasificación")

grafica<-ggplot(data=dSQ,aes(x=SQ4,y=SQCL))+geom_point(aes(colour=DD5))
grafica+labs(x = "Duración",y = "Escala PSQI",fill="Clasificación")



grafica2<-ggplot(data=dSQ,aes(x=SHSTR,y=SQTT,shape=SQCL))+geom_point(aes(colour=SQCL))
grafica2+ggtitle("Factores de estrés y activación cerebral\n vs calidad del sueño")
grafica2+labs(x = "Estrés",y = "Escala PSQI",fill="Clasificación")

grafica3<-ggplot(data=dSQ,aes(x=SHDIS,y=SQTT))+geom_point(aes(colour=SQCL))
grafica3+labs(x = "DISRUPTORS",y = "Escala PSQI",fill="Clasificación")

grafica4<-ggplot(data=dSQ,aes(x=SHCH,y=SQTT))+geom_point(aes(colour=SQCL))
grafica4+labs(x = "CIRCADIANS",y = "Escala PSQI",fill="Clasificación")

grafica5<-ggplot(data=dSQ,aes(x=SHDG,y=SQTT))+geom_point(aes(colour=SQCL))
grafica5+labs(x = "DRUGS",y = "Escala PSQI",fill="Clasificación")

grafica6<-ggplot(data=dSQ,aes(x=SH12,y=SQTT))+geom_point(aes(colour=SQCL))
grafica6+labs(x = "Estrés",y = "Escala PSQI",fill="Clasificación")

caja_relTT<-ggplot(dSQ,aes(factor(DD5),SQTT))+ geom_boxplot(notch = TRUE)
caja_relTT<-ggplot(dSQ,aes(factor(DD5),SQ4))+ geom_boxplot(aes(colour=factor(dSQ$DD5)))
caja_relSTR<-ggplot(dSQ,aes(factor(DD5),SHSTR))+ geom_boxplot(aes(colour=factor(dSQ$DD5)))

histSQ<-ggplot(dSQ,aes(SQCL))+geom_bar(stat = "count",aes(fill=SQCL))

hist(dSQ$DD1)
grafica1<-ggplot(data = dSQ,aes(x=DD1))+geom_histogram(bins = 30)

edad<-(dSQ$DD1)^(1/25)
qplot(edad,bins=30)

shapiro.test(edad)


grafica2<-ggplot(data=dSQ,aes(x=SHSTR,y=SQTT,shape=DD5))+geom_point(aes(colour=DD5))
grafica2+ggtitle("Factores de estrés y activación cerebral\n vs calidad del sueño")
grafica2+labs(x = "Estrés",y = "Escala PSQI",fill="Clasificación")

grafica2<-ggplot(data=dSQ,aes(x=SQ2,y=SQ4,shape=DD5))+geom_point(aes(colour=DD5))
grafica2+labs(x = "Latencia",y = "Cantidad de horas de sueno",fill="Clasificación")

datostmp<-datosAplicada[datosAplicada$CR.AplicadaD == 2,]


ASD<-dSQ[dSQ$DD5=="ASD",13]
NOASD<-dSQ[dSQ$DD5!="ASD",13]

shapiro.test(ASD)
shapiro.test(NOASD)


var.test(ASD,NOASD)

t.test(ASD,NOASD,alternative = "two.sided",var.equal = TRUE)

boxplot(ASD,NOASD)



slm4<-dSQ[dSQ$SQ4>4.5 & dSQ$SQ4<9.0,]
caja_relTT<-ggplot(slm4,aes(factor(DD5),SQ4))+ geom_boxplot(aes(colour=factor(slm4$DD5)))+labs(x = "Religión",y = "Media de horas de sueño",fill="Clasificación")
caja_relTT+labs(x = "Religión",y = "Media de horas de sueño",fill="Clasificación")
caja_relTT

ASDSTRES<-dSQ[dSQ$DD5=="ASD",58]
NOASDSTRES<-dSQ[dSQ$DD5!="ASD",58]

shapiro.test(ASDSTRES)
shapiro.test(NOASDSTRES)


var.test(ASDSTRES,NOASDSTRES)

t.test(ASDSTRES,NOASDSTRES,alternative = "two.sided",var.equal = TRUE)

caja_relTT<-ggplot(slm4,aes(factor(DD5),SHSTR))+ geom_boxplot(aes(colour=factor(slm4$DD5)))




ASDHD<-dSQ[dSQ$DD5=="ASD",10]
NOASDHD<-dSQ[dSQ$DD5!="ASD",10]

shapiro.test(ASDHD)
shapiro.test(NOASDHD)


var.test(ASDHD,NOASDHD)

t.test(ASDHD,NOASDHD,alternative = "two.sided",var.equal = F)

caja_relTT<-ggplot(slm4,aes(factor(DD5),SQ1))+ geom_boxplot(aes(colour=factor(slm4$DD5)))

grafica2<-ggplot(data=dSQ,aes(x=SHSTR,y=SQTT,shape=SQCL))+geom_point(aes(colour=SQCL))


q<-ggplot(dSQ,aes(x=SQ4))+geom_histogram(bins=30)


