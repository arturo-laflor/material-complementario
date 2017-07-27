q<-ggplot(dSQ,aes(x=SQ4))+geom_histogram(bins=30)
q+labs(x="hours of sleep")

caja_relTT<-ggplot(dSQ,aes(factor(DD5),SQ4))+ geom_boxplot(aes(colour=factor(dSQ$DD5)))

caja_relTT+labs(x="Religion", y = "Hours of sleep",color="Clasificación")


data_religion<-data.frame(DD5=dSQ$DD5,SQ4=dSQ$SQ4)

data_religion$DD5<-as.character(data_religion$DD5)

data_religion[data_religion$DD5=='Católica',1]<-'Catholic'
data_religion[data_religion$DD5=='ASD',1]<-'SDA'
data_religion[data_religion$DD5=='Otra',1]<-'Other'
data_religion[data_religion$DD5=='Evangélica',1]<-'Evangelic'

caja_relTT<-ggplot(data_religion,aes(factor(DD5),SQ4))+ geom_boxplot(aes(colour=factor(data_religion$DD5)))

caja_relTT+labs(x="Religion", y = "Hours of sleep",color="Clasification")