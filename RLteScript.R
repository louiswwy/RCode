<<<<<<< HEAD
########ggplot包#####################
=======
###########run only once
# install.packages("ggplot2")
# install.packages("knitr")
# install.packages("data.table")
# install.packages("gpairs")
# install.packages("plotrix")
# install.packages("pvclust")
# install.packages("cluster")
# install.packages("FactoMineR")
# install.packages("fpc")

########ggplot#####################
>>>>>>> 5e50988808ba8f388a14a72f9af990c4be577c28
library("ggplot2")
########SVM包##################
#library("kernlab")
###########贝叶斯包#########
#library("bnlearn")
########Stringr包#############
library("stringr")
#########knitr包##############
library("knitr")
#########data.table包##########
library("data.table")

library("gpairs")

library("plotrix")
#######常用聚类算法#########
library("pvclust")
library("cluster")
########PCA包###################
#做PCA可以使用FactoMineR包中的PCA()。和自带的prcomp(),princomp()
library("FactoMineR")
########fpc包#################
#fpc包中的pamk()
library("fpc")

##############HTTP#######################
data.DisHttp<-read.table("201406191100-ltehttpwap-sig13-11675500972.DAT"
                         ,header=TRUE,sep="|",fill=TRUE,colClasses="character",quote="",comment.char="")

######删除格式错误数据########
Delerror<-(data.DisHttp$EndTime!='')
data.Http<-data.DisHttp[Delerror,]
rm(Delerror)

data.HTTP<-data.Http
min(as.numeric(data.HTTP$BeginTime))
max(as.numeric(data.HTTP$BeginTime))
min(as.numeric(data.HTTP$EndTime))
max(as.numeric(data.HTTP$EndTime))
#######流程时间#########
ProcedureTime<-(as.numeric(data.HTTP$StopT) - as.numeric(data.HTTP$StartT))
data.HTTP1<-data.frame(data.HTTP,ProcedureTime)
rm(ProcedureTime)

#max(as.numeric(data.HTTP1$UpTime))

#########删除流程持续时间为0,上行在线时长/下行在线时长为0的项##########
HttpTime<-((data.HTTP1$ProcedureTime!=0)&(data.HTTP1$UpTime!='0')&(data.HTTP1$DownTime!='0'))
data.HTTP2<-data.HTTP1[HttpTime,]
rm(HttpTime)

#######上行平均带宽#####
upAvBand<-as.numeric(data.HTTP2$UpTraffic)/as.numeric(data.HTTP2$UpTime)
data.HTTP3<-data.frame(data.HTTP2,upAvBand)
rm(upAvBand)

#######下行平均带宽#####
downAvBand<-as.numeric(data.HTTP3$DownTraffic)/as.numeric(data.HTTP3$DownTime)
data.HTTP3<-data.frame(data.HTTP3,downAvBand)
rm(downAvBand)

#########上行误码率###############
UperrorRate<-(as.numeric(data.HTTP3$UpRePac)/(as.numeric(data.HTTP3$UpPac)+as.numeric(data.HTTP3$UpRePac)))
data.HTTP4<-data.frame(data.HTTP3,UperrorRate)
rm(UperrorRate)

#########下行误码率###############
DownerrorRate<-(as.numeric(data.HTTP3$DownRePac)/(as.numeric(data.HTTP3$DownPac)+as.numeric(data.HTTP3$DownRePac)))
data.HTTP4<-data.frame(data.HTTP4,DownerrorRate)
rm(DownerrorRate)

########FirstRespondTime、LastPacketTime、LastAckTime#####
FirstRespondTime<-(as.numeric(data.HTTP4$FirstRespondTime))
data.HTTP5<-data.frame(data.HTTP4,FirstRespondTime)

LastPacketTime<-(as.numeric(data.HTTP4$LastPacketTime))
data.HTTP5<-data.frame(data.HTTP5,LastPacketTime)

LastAckTime<-(as.numeric(data.HTTP4$LastAckTime))
data.HTTP5<-data.frame(data.HTTP5,LastAckTime)
rm(FirstRespondTime,LastPacketTime,LastAckTime)

names(data.HTTP5)[names(data.HTTP5)=="FirstRespondTime.1"]="FirstRespondTime"
names(data.HTTP5)[names(data.HTTP5)=="LastPacketTime"]="LastPacketTime"
names(data.HTTP5)[names(data.HTTP5)=="LastAckTime"]="LastAckTime"


################聚类准备##################
Data.AnA<-data.HTTP5[data.HTTP4$DownerrorRate!='NaN'
                     ,c('upAvBand',  'downAvBand',  'UperrorRate','DownerrorRate','FirstRespondTime','LastPacketTime','LastAckTime')]

# Data.AnA$upAvBand   <-Data.AnA$upAvBand   /max(Data.AnA$upAvBand) 
# Data.AnA$downAvBand <-Data.AnA$downAvBand /max(Data.AnA$downAvBand) 

Data.AnA$UperrorRate   <-1-Data.AnA$UperrorRate
Data.AnA$DownerrorRate <-1-Data.AnA$DownerrorRate

Data.AnA$FirstRespondTime   <-as.numeric(Data.AnA$FirstRespondTime)
Data.AnA$LastPacketTime     <-as.numeric(Data.AnA$LastPacketTime)
Data.AnA$LastAckTime        <-as.numeric(Data.AnA$LastAckTime)

# Data.AnA$FirstRespondTime   <-Data.AnA$FirstRespondTime/(max(Data.AnA$FirstRespondTime) )
# Data.AnA$LastPacketTime     <-Data.AnA$LastPacketTime  /(max(Data.AnA$LastPacketTime) )
# Data.AnA$LastAckTime        <-Data.AnA$LastAckTime     /(max(Data.AnA$LastAckTime) )
######改名#####
names(Data.AnA)[names(Data.AnA)=="UperrorRate"]="UpCorrecteRate";
names(Data.AnA)[names(Data.AnA)=="DownerrorRate"]="DownCorrecteRate";

############PCA#############
Data.AnAP3<-data.frame(scale(Data.AnA))

Scoreplot<-PCA(Data.AnAP3,scale.unit=TRUE,graph=FALSE)
scores<-data.frame(Scoreplot$ind$coord)
ggplot(scores,aes(Dim.1,Dim.2)) + geom_text(label=rownames(scores),colour="red") + geom_hline(yintercept=0) + geom_vline(xintercept=0) + labs(title="Score plot")


pca<-prcomp(Data.AnAP3,scale=TRUE)#,tol=.0,,scale=TRUE
summary(pca)
plot(pca)
plot(pca, type='l')
barplot(pca$sdev/pca$sdev[1])
#biplot(pca,main="双标图")

#相关矩阵
dcor<-cor(Data.AnAP3)
dcor
#求相关矩阵的特征向量，特征值
deig<-eigen(dcor)
deig
#输出特征值
deig$values
sumeigv<-sum(deig$values)
sumeigv
sum(deig$value[1:6])/sumeigv
#前6个主成份的累计贡献率达到90.7%的

#输出前六个主成分的荷载系数（特征向量）
pca$loadings[,1:6]

pca$sdev
#pca$x
head(pca$x)

pca2<-prcomp(Data.AnAP3,scale=TRUE,tol=0.66)
plot(pca2)

newDat<-predict(pca2)
# newDat2<-predict(pca,Data.AnAP2)
# newdat<-pca$x[,1:2]

pairs(pca2$x,main="Principal Component Analysis")

pca2$rotation

pload<-abs(pca2$rotation)
sweep(pload,2,colSums(pload),"/")#the proportional contribution to the each principal component

###########K值选择#############
#通过计算轮廓系数（silhouette coefficient）方法结合了凝聚度和分离度，可以以此来判断聚类的优良性。其值在-1到+1之间取值，值越大表示聚类效果越好。
#########计算不同K值的SSE#####
CalculeSSE<-function(data){
  # K值的开始与结果边界
  begin = 1
  length = 15
  #重复次数
  count = 50
  end = begin + length - 1
  # 结果容器
  resultSSE <<- c()
  resultSSE[begin:end] <<- 0
  # 遍历计算kmeans的SSE
  for(i in begin:end) {  
    # 计算SSE  
    tmp = c()
    tmp[1:count] = 0  
    for(j in 1:count) {    
      kcluster = kmeans(data, i)   
      tmp[j] = kcluster$tot.withinss  
    }  
    resultSSE[i] <<- mean(tmp)
  }
}

system.time(CalculeSSE(newDat))
# 绘制结果
plot(resultSSE, type="o", xlab="Number of Cluster", ylab="Sum of Squer Error");
rm(resultSSE)

###########计算Silhouette Coefficient#######


CalculeSC<-function(data){
  begin = 2
  length = 15
  count = 50
  end = begin + length - 1
  # 结果容器
  resultSC <<- c()
  resultSC[begin:end]<<- -1
  # 遍历计算kmeans的SSE
  for(i in begin:end) {  
    # Silhouette coefficient  
    tmp = c()  
    tmp[1:count] = 0  
    for(j in 1:count) {    
      kcluster = clara(newDat, i)    
      tmp[j] = kcluster$silinfo$avg.width   #silinfo : a list with all silhouette information,   
    }  
    resultSC[i]  <<- mean(tmp)
  }
}

system.time(CalculeSC(newDat))
# 绘制结果
plot(resultSC, type="o", xlab="Number of Cluster", ylab="Silhouette Coefficient")

# K=5时值最大，所以聚类效果最佳。

rm(resultSC)

###########聚类############
#bcl<-bootFlexclust(newDat, k=2:15, nboot=50, FUN=cclust, multicore=FALSE)
#######K-means############3
pkm<-kmeans(newDat,5,nstart=25,iter.max=10,algorithm="Hartigan-Wong")
plot(x=newDat[,2],y=newDat[,3],col=pkm$cluster,xlim=c(-5,10),ylim=c(-2,10),main="聚5类图",xlab="",ylab="") #,xlim=c(-5,0.5),ylim=c(-5,5)
plot(newDat,col=pkm$cluster,xlab="",ylab="",xlim=c(-5,2),ylim=c(-3,6))

###############CLARA (Clustering for Large Applications) algorithm###################
# It works by clustering a sample from the dataset and then assigns all objects in the dataset to these clusters.
#需要使用cluster包
kmC<-clara(newDat,5)
kmC$clusinfo

# Delerror<-(data.DisHttp$EndTime!='')
# data.Http<-data.DisHttp[Delerror,]

analyseData<-function(data,Clusting,PcaData,num){
  #for(i in 1:num){
    cluste<-Clusting$clustering==num
    data.cluste<-PcaData[cluste,]    
    a<-data.frame(row.names(data.cluste))
    matchs<-row.names(data) %in% a[,1]
    data.matchs<-data[matchs,]
    return(data.matchs)
  #}   
}

Clara1<-analyseData(Data.AnA,kmC,newDat,1)
Clara2<-analyseData(Data.AnA,kmC,newDat,2)
Clara3<-analyseData(Data.AnA,kmC,newDat,3)
Clara4<-analyseData(Data.AnA,kmC,newDat,4)
Clara5<-analyseData(Data.AnA,kmC,newDat,5)

nrow(Clara1)
nrow(Clara2)
nrow(Clara3)
nrow(Clara4)
nrow(Clara5)

StastiqueClara<-function(data,Clusting,PcaData,num){
  data_mean<-c()
  MEAN<-c() 
  for(i in 1:num){
    cluste<-Clusting$clustering==i
    data_cluste<-PcaData[cluste,]    
    a<-data.frame(row.names(data_cluste))
    matchs<-row.names(data) %in% a[,1]
    data_matchs<-data[matchs,]    
    #cat(data_matchs[1,1])
    #
    for(b in 1:ncol(data_matchs)){
      ColMean<-mean(data_matchs[,b])
      data_mean<-cbind(data_mean,ColMean)
    } 
#     writeLines("")
#     cat("MEAN",MEAN)
#     writeLines("")
#     cat("data_mean",data_mean)
    MEAN<-rbind(MEAN,data_mean)
    data_mean<-c()
  }  
  return(MEAN)
}

ClaraMean<-StastiqueClara(Data.AnA,kmC,newDat,5)
names(ClaraMean)<-c('upAvBand','downAvBand','UpCorrecteRate','DownCorrecteRate' ,'FirstRespondTime','LastPacketTime','LastAckTime')
plot(ClaraMean)#,col
#matrix


print(kmC)
# ###########统计信息##########
# summary(Data.AnA$UpCorrecteRate)
# summary(Data.AnA$DownCorrecteRate)
# 
# summary(Data.AnA$upAvBand)
# summary(Data.AnA$downAvBand)
# 
# summary(Data.AnA$FirstRespondTime)
# summary(Data.AnA$LastPacketTime)
# summary(Data.AnA$LastAckTime)
# 
# boxplot(Data.AnA$UpCorrecteRate)
# boxplot(Data.AnA$DownCorrecteRate)
# 
# boxplot(Data.AnA$upAvBand)
# boxplot(Data.AnA$downAvBand)
# 
# boxplot(Data.AnA$FirstRespondTime)
# boxplot(Data.AnA$LastPacketTime)
# boxplot(Data.AnA$LastAckTime)
# 
# mean(Data.AnA$UpCorrecteRate)
# mean(Data.AnA$DownCorrecteRate)
# 
# mean(Data.AnA$upAvBand)
# mean(Data.AnA$downAvBand)
# 
# mean(Data.AnA$FirstRespondTime)
# mean(Data.AnA$LastPacketTime)
# mean(Data.AnA$LastAckTime)
# 
# 
# # sd(Data.AnA$UpCorrecteRate)
# # sd(Data.AnA$DownCorrecteRate)
# # 
# # sd(Data.AnA$upAvBand)
# # sd(Data.AnA$downAvBand)
# # 
# # sd(Data.AnA$FirstRespondTime)
# # sd(Data.AnA$LastPacketTime)
# # sd(Data.AnA$LastAckTime)
# 

# #####直方图#############
# 
# #HistP<-ggplot(data=Data.AnA)
# #binsize<-diff(range(Data.AnA$UpCorrecteRate))/15
# #HistP<-HistP+geom_histogram(aes(x=UpCorrecteRate),binwidth =binsize, fill = "light green", colour = "red")
# #HistP
# 
# hist(Data.AnA$UpCorrecteRate)
# hist(Data.AnA$DownCorrecteRate)
# 
# hist(Data.AnA$upAvBand)
# hist(Data.AnA$downAvBand)
# 
# hist(Data.AnA$FirstRespondTime)
# hist(Data.AnA$LastPacketTime)
# hist(Data.AnA$LastAckTime)

# # ######核密度函数##################
# # density(Data.AnA$UpCorrecteRate)
# # density(Data.AnA$DownCorrecteRate)
# # 
# # density(Data.AnA$upAvBand)
# # density(Data.AnA$downAvBand)
# # 
# # density(Data.AnA$FirstRespondTime)
# # density(Data.AnA$LastPacketTime)
# # density(Data.AnA$LastAckTime)
# 

# #########QQ散点图###########
# qqnorm(Data.AnA$UpCorrecteRate,main='UpCorrecteRate')
# qqnorm(Data.AnA$DownCorrecteRate,main='DownCorrecteRate')
# 
# qqnorm(Data.AnA$upAvBand,main='upAvBand')
# qqnorm(Data.AnA$downAvBand,main='downAvBand')
# 
# qqnorm(Data.AnA$FirstRespondTime,main='FirstRespondTime')
# qqnorm(Data.AnA$LastPacketTime,main='LastPacketTime')
# qqnorm(Data.AnA$LastAckTime,main='LastAckTime')
# 

# ###############---###################
# Data.AnA2<-Data.AnA
# 
# ####可以使用的距离算法有"Hartigan-Wong", "Lloyd", "Forgy","MacQueen"四种
# ####   ,nstart=5, iter.max = 10
# PCAkm<-kmeans(newDat,5,nstart=25, iter.max = 10, algorithm = "Hartigan-Wong")
# plot(newDat,col=PCAkm$cluster)
# plot(PCAkm$centers)
# 
# 
# km2<-kmeans(Data.AnA2,5,nstart=25, iter.max = 10, algorithm = "Hartigan-Wong")
# plot(Data.AnA2,col=km2$cluster)
# ClusterDef2 <-function(Data,km){  
#   Data$cluster=factor(km$cluster)
#   Data.AnAP2<<-Data
#   
#   data=as.data.frame(km$centers)
#   Lev2<-rank(data[,2])
#   data<-cbind(data,Lev2)
#   centers2<<-data
#   for(i in 1:nrow(data)){  
#     cat(i)
#     cat(sprintf(": 类%s :", data[i,8])) #%s %f输出变量
#     switch(as.character(data[i,8]),
#            "1" =  {
#              cat("level 1.")
#              KM1<-(Data.AnAP2$cluster==i)  #函数内部强赋值
#              data.KM1<<-Data.AnAP2[KM1,]
#              writeLines("")
#              cat("总计：",nrow(data.KM1),"条")
#            },
#            
#            "2" =  {
#              cat("level 2.")
#              KM2<-(Data.AnAP2$cluster==i)
#              data.KM2<<-Data.AnAP2[KM2,]
#              writeLines("")
#              cat("总计：",nrow(data.KM2),"条")
#            },
#            
#            "3" =  {
#              cat("level 3.")
#              KM3<-(Data.AnAP2$cluster==i)
#              data.KM3<<-Data.AnAP2[KM3,]
#              writeLines("")
#              cat("总计：",nrow(data.KM3),"条")
#            },
#            
#            "4" =  {
#              cat("level 4.")
#              KM4<-(Data.AnAP2$cluster==i)
#              data.KM4<<-Data.AnAP2[KM4,]
#              writeLines("")
#              cat("总计：",nrow(data.KM4),"条")
#            },
#            
#            "5" =  {
#              cat("level 5.")
#              KM5<-(Data.AnAP2$cluster==i)
#              data.KM5<<-Data.AnAP2[KM5,]
#              writeLines("")
#              cat("总计：",nrow(data.KM5),"条")
#            }  
#     )
#     writeLines("")  #换行
#   }
# }
# 
# ClusterDef2(Data.AnA2,km2)
# plot(Data.AnA2,col = km2$cluster)
# # Lv2KM<-kmeans(data.KM1,5,nstart=25)
# # plot(data.KM1,col = Lv2KM$cluster)
# # 
# # ClusterDef2(data.KM1,Lv2KM)

# ############tu####################
# 
# p<-ggplot(data=Data.AnAP2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color="",size=5)) 
# p
# 
# p<-ggplot(data=data.KM1, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-1")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-2")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM3, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-3")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM4, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-4")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p
# 
# p<-ggplot(data=data.KM5, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster))+ggtitle("KMeans-5")
# p<-p+geom_point()
# p<-p+ xlim(0,1)+ylim(0,1)
# p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
# p

# # #######聚类分类######
# # num<-c(1:10)
# # #cbind(centers,num)
# # 
# # Center<-km$centers
# # Lev<-rank(Center[,2])
# # Center<-cbind(Center,Lev)
# # 
# # 
# # 

# # #########饼图##############
# # Values<-c(nrow(data.KM1),nrow(data.KM2),nrow(data.KM3),nrow(data.KM4),nrow(data.KM5),nrow(data.KM6),nrow(data.KM7),nrow(data.KM8),nrow(data.KM9),nrow(data.KM10))
# # Labels<-c("lev1","lev2","lev3","lev4","lev5","lev6","lev7","lev8","lev9","lev10")
# # percent_str <- paste(round(Values/sum(Values) * 100,1), "%", sep="")
# # Values <- data.frame(Percentage <- round(Values/sum(Values) * 100,1), Type = Labels,percent=percent_str )
# # names(Values)<-c("Percentage","Type","percent")
# # 
# # pie <- ggplot(Values, aes(x = "" ,y = Percentage, fill = Labels)) +  geom_bar(stat="identity",width = 3) 
# # pie = pie + coord_polar("y")
# # pie = pie + xlab('') + ylab('') + labs(fill="Types")
# # pie
# # 
# # data.table<- (table(Data.AnAP$cluster))
# # pie(data.table,main="Pie Chart of Cluster")
# # 
# # pic<-ggplot(Values,aes(x=Type,y=Percentage,fill=percent))+geom_bar(stat="identity") 
# # #pic<-pic+ylim(0,nrow(Data.AnAP))
# # pic<-pic+ggtitle("cluster ") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=20))
# # #pic<-pic+geom_text(hjust=0,vjust=-1,alpha=0.8)
# # pic
# # 

# # ########图###########
# # ggplot(data=Data.AnAP, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
# #   geom_point() + 
# #   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color="#2166AC",size=10)) 
# # 
# # #gpairs(Data.AnAP, upper.pars = list(scatter = 'stats'),
# # #     scatter.pars = list(pch = 1:3,
# # #                           col = as.numeric(Data.AnAP$cluster)))
# 
# 
# #RR<-data.frame(row.names(data.HTTP5))
# # data.HTTP6<-cbind(data.HTTP5,data.frame(row.names(data.HTTP5)))
# # names(data.HTTP6)[names(data.HTTP6)=="row.names.data.HTTP5."]="RowName";
# 

# # ############类1-统计#############
# # R1<-data.frame(row.names(data.KM1))
# # names(R1)[names(R1)=="row.names.data.KM1."]="RowName";
# # Cluster1<- (data.HTTP6[,'RowName']) %in% R1$RowName
# # data.Cluster1<-data.HTTP6[Cluster1,c('eNBip','MMEIP')]
# # 
# # 
# # data.Cluster1$eNBip<-as.numeric(data.Cluster1$eNBip)
# # data.Cluster1$MMEIP<-as.numeric(data.Cluster1$MMEIP)
# # 
# # table1<-data.frame(table(data.Cluster1$eNBip))
# # 
# # table1<-table1[order(table1[,2],decreasing=TRUE),]

# # ###############---##################
# # data.Cluster5$eNBip<-as.numeric(data.Cluster5$eNBip)
# # data.Cluster5$MMEIP<-as.numeric(data.Cluster5$MMEIP)
# # 
# # table5<-data.frame(table(data.Cluster5$eNBip))
# # 
# # table5<-table5[order(table5[,2],decreasing=TRUE),]
# # 
