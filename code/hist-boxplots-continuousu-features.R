library(ggplot2)

histDD1<-ggplot(data=d_resp_Bad_script,aes(DD1))+geom_histogram(bins = 30)+labs(y="Count")
histSQ1<-ggplot(data=d_resp_Bad_script,aes(SQ1))+geom_histogram(bins = 30)+labs(y="Count")
histSQ2<-ggplot(data=d_resp_Bad_script,aes(SQ2))+geom_histogram(bins = 30)+labs(y="Count")
histSQ3<-ggplot(data=d_resp_Bad_script,aes(SQ3))+geom_histogram(bins = 30)+labs(y="Count")
histSQ4<-ggplot(data=d_resp_Bad_script,aes(SQ4))+geom_histogram(bins = 30)+labs(y="Count")

boxplotDD1<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),DD1))+geom_boxplot()+labs(x="Gender")
boxplotSQ1<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ1))+geom_boxplot()+labs(x="Gender")
boxplotSQ2<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ2))+geom_boxplot()+labs(x="Gender")
boxplotSQ3<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ3))+geom_boxplot()+labs(x="Gender")
boxplotSQ4<-ggplot(data=d_resp_Bad_script, aes(as.factor(DD2),SQ4))+geom_boxplot()+labs(x="Gender")


multiplot(histSQ1,boxplotSQ1,histSQ2,boxplotSQ2,histSQ3,boxplotSQ3,histSQ4,boxplotSQ4,histDD1,boxplotDD1,cols=5)


library(ggplot2)

histDD1<-ggplot(data=QS_data,aes(DD1))+geom_histogram(bins = 30)+labs(y="Count")
histSQ1<-ggplot(data=QS_data,aes(SQ1))+geom_histogram(bins = 30)+labs(y="Count")
histSQ2<-ggplot(data=QS_data,aes(SQ2))+geom_histogram(bins = 30)+labs(y="Count")
histSQ3<-ggplot(data=QS_data,aes(SQ3))+geom_histogram(bins = 30)+labs(y="Count")
histSQ4<-ggplot(data=QS_data,aes(SQ4))+geom_histogram(bins = 30)+labs(y="Count")

boxplotDD1<-ggplot(data=QS_data, aes(as.factor(DD2),DD1))+geom_boxplot()+labs(x="Gender")
boxplotSQ1<-ggplot(data=QS_data, aes(as.factor(DD2),SQ1))+geom_boxplot()+labs(x="Gender")
boxplotSQ2<-ggplot(data=QS_data, aes(as.factor(DD2),SQ2))+geom_boxplot()+labs(x="Gender")
boxplotSQ3<-ggplot(data=QS_data, aes(as.factor(DD2),SQ3))+geom_boxplot()+labs(x="Gender")
boxplotSQ4<-ggplot(data=QS_data, aes(as.factor(DD2),SQ4))+geom_boxplot()+labs(x="Gender")


multiplot(histSQ1,boxplotSQ1,histSQ2,boxplotSQ2,histSQ3,boxplotSQ3,histSQ4,boxplotSQ4,histDD1,boxplotDD1,cols=5)

library(VIM)

dsplotmissingvalues<-data.frame(QS_data[,14:20],QS_data[,30:44]) 

plotofmv<-aggr(dsplotmissingvalues,col=c('darkgrey','black'),
               numbers=TRUE,labels=names(dsplotmissingvalues),
               cex.axis=.7,gap=3,ylab=c("Missing data","Pattern"))




