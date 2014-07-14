########ggplot#####################
library("ggplot2", lib.loc="E:/Program Files/R/R-3.1.0/library")
########SVM##################
#library("kernlab", lib.loc="E:/Program Files/R/R-3.1.0/library")
###########贝叶斯#########
#library("bnlearn", lib.loc="E:/Program Files/R/R-3.1.0/library")
########Stringr#############
library("stringr", lib.loc="E:/Program Files/R/R-3.1.0/library")
#########knitr##############
library("knitr", lib.loc="E:/Program Files/R/R-3.1.0/library")
#########data.table##########
library("data.table", lib.loc="E:/Program Files/R/R-3.1.0/library")

library("gpairs", lib.loc="E:/Program Files/R/R-3.1.0/library")

library("plotrix", lib.loc="E:/Program Files/R/R-3.1.0/library")

library("FactoMineR")

library("rgl")
##############HTTP#######################
data.DisHttp<-read.table("201406191100-ltehttpwap-sig13-11675500972.DAT"
                         ,header=TRUE,sep="|",fill=TRUE,colClasses="character",quote="",comment.char="")

######删除格式错误数据########
Delerror<-(data.DisHttp$EndTime!='')
data.Http<-data.DisHttp[Delerror,]

data.HTTP<-data.Http

#######流程时间#########
ProcedureTime<-(as.numeric(data.HTTP$StopT) - as.numeric(data.HTTP$StartT))
data.HTTP1<-data.frame(data.HTTP,ProcedureTime)

#max(as.numeric(data.HTTP1$UpTime))

#########删除流程持续时间为0,上行在线时长/下行在线时长为0的项##########
HttpTime<-((data.HTTP1$ProcedureTime!=0)&(data.HTTP1$UpTime!='0')&(data.HTTP1$DownTime!='0'))
data.HTTP2<-data.HTTP1[HttpTime,]

#######上行平均带宽#####
upAvBand<-as.numeric(data.HTTP2$UpTraffic)/as.numeric(data.HTTP2$UpTime)
data.HTTP3<-data.frame(data.HTTP2,upAvBand)

#######下行平均带宽#####
downAvBand<-as.numeric(data.HTTP3$DownTraffic)/as.numeric(data.HTTP3$DownTime)
data.HTTP3<-data.frame(data.HTTP3,downAvBand)

#########上行误码率###############
UperrorRate<-(as.numeric(data.HTTP3$UpRePac)/(as.numeric(data.HTTP3$UpPac)+as.numeric(data.HTTP3$UpRePac)))
data.HTTP4<-data.frame(data.HTTP3,UperrorRate)

#########下行误码率###############
DownerrorRate<-(as.numeric(data.HTTP3$DownRePac)/(as.numeric(data.HTTP3$DownPac)+as.numeric(data.HTTP3$DownRePac)))
data.HTTP4<-data.frame(data.HTTP4,DownerrorRate)

########FirstRespondTime、LastPacketTime、LastAckTime#####
FirstRespondTime<-(as.numeric(data.HTTP4$FirstRespondTime))
data.HTTP5<-data.frame(data.HTTP4,FirstRespondTime)

LastPacketTime<-(as.numeric(data.HTTP4$LastPacketTime))
data.HTTP5<-data.frame(data.HTTP5,LastPacketTime)

LastAckTime<-(as.numeric(data.HTTP4$LastAckTime))
data.HTTP5<-data.frame(data.HTTP5,LastAckTime)

names(data.HTTP5)[names(data.HTTP5)=="FirstRespondTime.1"]="FirstRespondTime"
names(data.HTTP5)[names(data.HTTP5)=="LastPacketTime"]="LastPacketTime"
names(data.HTTP5)[names(data.HTTP5)=="LastAckTime"]="LastAckTime"


################聚类准备##################
Data.AnA<-data.HTTP5[data.HTTP4$DownerrorRate!='NaN'
                     ,c('upAvBand',  'downAvBand',	'UperrorRate','DownerrorRate','FirstRespondTime','LastPacketTime','LastAckTime')]

# Data.AnA$upAvBand   <-Data.AnA$upAvBand   /max(Data.AnA$upAvBand) 
# Data.AnA$downAvBand <-Data.AnA$downAvBand /max(Data.AnA$downAvBand) 
Data.AnA$upAvBand   <-Data.AnA$upAvBand
Data.AnA$downAvBand <-Data.AnA$downAvBand

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

# ppccaa<-PCA(Data.AnAP2,scale.unit=TRUE,graph=FALSE)
# scores<-data.frame(ppccaa$ind$coord)
# ggplot(scores,aes(Dim.1,Dim.2)) + geom_text(label=rownames(scores),colour="red") + geom_hline(yintercept=0) + geom_vline(xintercept=0) + labs(title="Score plot")


# pca<-prcomp(Data.AnAP2,scale=FALSE)
pca<-prcomp(Data.AnAP3,scale=TRUE)#,tol=.0,,scale=TRUE
summary(pca)
plot(pca)
plot(pca, type='l')
#plot3d(pca$PC1,pca$PC2,pca$PC3)
#biplot(pca)
barplot(pca$sdev/pca$sdev[1])
pca$sdev
pca$x
head(pca$x)

newDat<-predict(pca,Data.AnAP2)
# newDat2<-predict(pca,Data.AnAP2)
# newdat<-pca$x[,1:2]

# plot.ts(pca$x)

plot(pca$x)

pairs(pca$x,main="Principal Component Analysis")
pca$rotation
pload<-abs(pca$rotation)
sweep(pload,2,colSums(pload),"/")#the proportional contribution to the each principal component


###########统计信息##########
summary(Data.AnA$UpCorrecteRate)
summary(Data.AnA$DownCorrecteRate)

summary(Data.AnA$upAvBand)
summary(Data.AnA$downAvBand)

summary(Data.AnA$FirstRespondTime)
summary(Data.AnA$LastPacketTime)
summary(Data.AnA$LastAckTime)

boxplot(Data.AnA$UpCorrecteRate)
boxplot(Data.AnA$DownCorrecteRate)

boxplot(Data.AnA$upAvBand)
boxplot(Data.AnA$downAvBand)

boxplot(Data.AnA$FirstRespondTime)
boxplot(Data.AnA$LastPacketTime)
boxplot(Data.AnA$LastAckTime)

mean(Data.AnA$UpCorrecteRate)
mean(Data.AnA$DownCorrecteRate)

mean(Data.AnA$upAvBand)
mean(Data.AnA$downAvBand)

mean(Data.AnA$FirstRespondTime)
mean(Data.AnA$LastPacketTime)
mean(Data.AnA$LastAckTime)


# sd(Data.AnA$UpCorrecteRate)
# sd(Data.AnA$DownCorrecteRate)
# 
# sd(Data.AnA$upAvBand)
# sd(Data.AnA$downAvBand)
# 
# sd(Data.AnA$FirstRespondTime)
# sd(Data.AnA$LastPacketTime)
# sd(Data.AnA$LastAckTime)

#####直方图#############

#HistP<-ggplot(data=Data.AnA)
#binsize<-diff(range(Data.AnA$UpCorrecteRate))/15
#HistP<-HistP+geom_histogram(aes(x=UpCorrecteRate),binwidth =binsize, fill = "light green", colour = "red")
#HistP

hist(Data.AnA$UpCorrecteRate)
hist(Data.AnA$DownCorrecteRate)

hist(Data.AnA$upAvBand)
hist(Data.AnA$downAvBand)

hist(Data.AnA$FirstRespondTime)
hist(Data.AnA$LastPacketTime)
hist(Data.AnA$LastAckTime)
# ######核密度函数##################
# density(Data.AnA$UpCorrecteRate)
# density(Data.AnA$DownCorrecteRate)
# 
# density(Data.AnA$upAvBand)
# density(Data.AnA$downAvBand)
# 
# density(Data.AnA$FirstRespondTime)
# density(Data.AnA$LastPacketTime)
# density(Data.AnA$LastAckTime)

#########QQ散点图###########
qqnorm(Data.AnA$UpCorrecteRate,main='UpCorrecteRate')
qqnorm(Data.AnA$DownCorrecteRate,main='DownCorrecteRate')

qqnorm(Data.AnA$upAvBand,main='upAvBand')
qqnorm(Data.AnA$downAvBand,main='downAvBand')

qqnorm(Data.AnA$FirstRespondTime,main='FirstRespondTime')
qqnorm(Data.AnA$LastPacketTime,main='LastPacketTime')
qqnorm(Data.AnA$LastAckTime,main='LastAckTime')

##################################
Data.AnA2<-Data.AnA

####可以使用的距离算法有"Hartigan-Wong", "Lloyd", "Forgy","MacQueen"四种
####   ,nstart=5, iter.max = 10
PCAkm<-kmeans(newDat,5,nstart=25, iter.max = 10, algorithm = "Hartigan-Wong")
plot(newDat,col=PCAkm$cluster)
plot(PCAkm$centers)


km2<-kmeans(Data.AnA2,5,nstart=25, iter.max = 10, algorithm = "Hartigan-Wong")
plot(Data.AnA2,col=km2$cluster)
ClusterDef2 <-function(Data,km){  
  Data$cluster=factor(km$cluster)
  Data.AnAP2<<-Data
  
  data=as.data.frame(km$centers)
  Lev2<-rank(data[,2])
  data<-cbind(data,Lev2)
  centers2<<-data
  for(i in 1:nrow(data)){  
    cat(i)
    cat(sprintf(": 类%s :", data[i,8])) #%s %f输出变量
    switch(as.character(data[i,8]),
           "1" =  {
             cat("level 1.")
             KM1<-(Data.AnAP2$cluster==i)  #函数内部强赋值
             data.KM1<<-Data.AnAP2[KM1,]
             writeLines("")
             cat("总计：",nrow(data.KM1),"条")
           },
           
           "2" =  {
             cat("level 2.")
             KM2<-(Data.AnAP2$cluster==i)
             data.KM2<<-Data.AnAP2[KM2,]
             writeLines("")
             cat("总计：",nrow(data.KM2),"条")
           },
           
           "3" =  {
             cat("level 3.")
             KM3<-(Data.AnAP2$cluster==i)
             data.KM3<<-Data.AnAP2[KM3,]
             writeLines("")
             cat("总计：",nrow(data.KM3),"条")
           },
           
           "4" =  {
             cat("level 4.")
             KM4<-(Data.AnAP2$cluster==i)
             data.KM4<<-Data.AnAP2[KM4,]
             writeLines("")
             cat("总计：",nrow(data.KM4),"条")
           },
           
           "5" =  {
             cat("level 5.")
             KM5<-(Data.AnAP2$cluster==i)
             data.KM5<<-Data.AnAP2[KM5,]
             writeLines("")
             cat("总计：",nrow(data.KM5),"条")
           }  
    )
    writeLines("")  #换行
  }
}

ClusterDef2(Data.AnA2,km2)
plot(Data.AnA2,col = km2$cluster)
# Lv2KM<-kmeans(data.KM1,5,nstart=25)
# plot(data.KM1,col = Lv2KM$cluster)
# 
# ClusterDef2(data.KM1,Lv2KM)
############tu####################

p<-ggplot(data=Data.AnAP2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))
p<-p+geom_point()
p<-p+ xlim(0,1)+ylim(0,1)
p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color="",size=5)) 
p

p<-ggplot(data=data.KM1, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-1")
p<-p+geom_point()
p<-p+ xlim(0,1)+ylim(0,1)
p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
p

p<-ggplot(data=data.KM2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-2")
p<-p+geom_point()
p<-p+ xlim(0,1)+ylim(0,1)
p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
p

p<-ggplot(data=data.KM3, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-3")
p<-p+geom_point()
p<-p+ xlim(0,1)+ylim(0,1)
p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
p

p<-ggplot(data=data.KM4, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster ))+ggtitle("KMeans-4")
p<-p+geom_point()
p<-p+ xlim(0,1)+ylim(0,1)
p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
p

p<-ggplot(data=data.KM5, aes(x=UpCorrecteRate, y=DownCorrecteRate,color=cluster))+ggtitle("KMeans-5")
p<-p+geom_point()
p<-p+ xlim(0,1)+ylim(0,1)
p<-p+geom_point(data=centers2, aes(x=UpCorrecteRate, y=DownCorrecteRate,color='black',size=5)) 
p


#KM4
# ############聚类############
# km<-kmeans(Data.AnA,2,nstart=25)
# #fitted(km)
# 
# plot(Data.AnA,col = km$cluster)
# plot(km$centers)
# Data.AnAP<-Data.AnA
# Data.AnAP$cluster=factor(km$cluster)
# centers=as.data.frame(km$centers)
# 
# #######聚类分类######
# num<-c(1:10)
# #cbind(centers,num)
# 
# Center<-km$centers
# Lev<-rank(Center[,2])
# Center<-cbind(Center,Lev)
# 
# 
# 
# ######function###############
# ClusterDef <-function(data){  
#  for(i in 1:nrow(data)){  
#    cat(i)
#    cat(sprintf(": 类%s :", data[i,8])) #%s %f输出变量
#    switch(as.character(data[i,8]),
#            "1" =  {
#              cat("level 1.")
#              KM1<<-(Data.AnAP$cluster==i)  #函数内部强赋值
#              data.KM1<<-Data.AnAP[KM1,]
#              writeLines("")
#              cat("总计：",nrow(data.KM1),"条")
#              },
#           
#            "2" =  {
#              cat("level 2.")
#              KM2<<-(Data.AnAP$cluster==i)
#              data.KM2<<-Data.AnAP[KM2,]
#              writeLines("")
#              cat("总计：",nrow(data.KM2),"条")
#              },
#           
#            "3" =  {
#              cat("level 3.")
#              KM3<<-(Data.AnAP$cluster==i)
#              data.KM3<<-Data.AnAP[KM3,]
#              writeLines("")
#              cat("总计：",nrow(data.KM3),"条")
#              },
#           
#            "4" =  {
#              cat("level 4.")
#              KM4<<-(Data.AnAP$cluster==i)
#              data.KM4<<-Data.AnAP[KM4,]
#              writeLines("")
#              cat("总计：",nrow(data.KM4),"条")
#              },
#           
#            "5" =  {
#              cat("level 5.")
#              KM5<<-(Data.AnAP$cluster==i)
#              data.KM5<<-Data.AnAP[KM5,]
#              writeLines("")
#              cat("总计：",nrow(data.KM5),"条")
#            }  ,
#           "6" =  {
#             cat("level 6.")
#             KM6<<-(Data.AnAP$cluster==i)
#             data.KM6<<-Data.AnAP[KM6,]
#             writeLines("")
#             cat("总计：",nrow(data.KM6),"条")
#           }  ,
#           "7" =  {
#             cat("level 7.")
#             KM7<<-(Data.AnAP$cluster==i)
#             data.KM7<<-Data.AnAP[KM7,]
#             writeLines("")
#             cat("总计：",nrow(data.KM7),"条")
#           }  ,
#           "8" =  {
#             cat("level 8.")
#             KM8<<-(Data.AnAP$cluster==i)
#             data.KM8<<-Data.AnAP[KM8,]
#             writeLines("")
#             cat("总计：",nrow(data.KM8),"条")
#           }  ,
#           
#           "9" =  {
#             cat("level 9.")
#             KM9<<-(Data.AnAP$cluster==i)
#             data.KM9<<-Data.AnAP[KM9,]
#             writeLines("")
#             cat("总计：",nrow(data.KM9),"条")
#           }  ,
#           "10" =  {
#             cat("level 10.")
#             KM10<<-(Data.AnAP$cluster==i)
#             data.KM10<<-Data.AnAP[KM10,]
#             writeLines("")
#             cat("总计：",nrow(data.KM10),"条")
#           }  
#           
#           
#           )
#    writeLines("")  #换行
#  }
# }
# 
# ClusterDef(Center)
# #########饼图##############
# Values<-c(nrow(data.KM1),nrow(data.KM2),nrow(data.KM3),nrow(data.KM4),nrow(data.KM5),nrow(data.KM6),nrow(data.KM7),nrow(data.KM8),nrow(data.KM9),nrow(data.KM10))
# Labels<-c("lev1","lev2","lev3","lev4","lev5","lev6","lev7","lev8","lev9","lev10")
# percent_str <- paste(round(Values/sum(Values) * 100,1), "%", sep="")
# Values <- data.frame(Percentage <- round(Values/sum(Values) * 100,1), Type = Labels,percent=percent_str )
# names(Values)<-c("Percentage","Type","percent")
# 
# pie <- ggplot(Values, aes(x = "" ,y = Percentage, fill = Labels)) +  geom_bar(stat="identity",width = 3) 
# pie = pie + coord_polar("y")
# pie = pie + xlab('') + ylab('') + labs(fill="Types")
# pie
# 
# data.table<- (table(Data.AnAP$cluster))
# pie(data.table,main="Pie Chart of Cluster")
# 
# pic<-ggplot(Values,aes(x=Type,y=Percentage,fill=percent))+geom_bar(stat="identity") 
# #pic<-pic+ylim(0,nrow(Data.AnAP))
# pic<-pic+ggtitle("cluster ") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=20))
# #pic<-pic+geom_text(hjust=0,vjust=-1,alpha=0.8)
# pic
# 
# ####图类1-最差######
# #plot(data.KM1,col = 'black')
# ggplot(data=data.KM1, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# 
# ####图类2-差######
# #plot(data.KM2,col = 'black')
# ggplot(data=data.KM2, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ####图类3-一般######
# #plot(data.KM3,col = 'black')
# ggplot(data=data.KM3, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ####图类4-好######
# #plot(data.KM4,col = 'black')
# p<-ggplot(data=data.KM4, aes(x=upAvBand, y=downAvBand,color=cluster ))
# #p<-p+ xlim(0,20000)#min(upAvBand)-10,max(upAvBand)
# #p<-p+ ylim(250000,max(downAvBand)+1000000)#min(downAvBand)
# p<-p+ geom_point()  
# p<-p+ geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color="red",size=5)) 
# p
# ####图类5-优秀######
# #plot(data.KM5,col = 'black')
# ggplot(data=data.KM5, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# plot(data.KM6,col = 'black')
# ggplot(data=data.KM6, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ggplot(data=data.KM7, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ggplot(data=data.KM8, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ggplot(data=data.KM9, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ggplot(data=data.KM10, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color='black',size=5)) 
# 
# ########图###########
# ggplot(data=Data.AnAP, aes(x=upAvBand, y=downAvBand,color=cluster )) + 
#   geom_point() + 
#   geom_point(data=centers, aes(x=upAvBand, y=downAvBand,color="#2166AC",size=10)) 
# 
# #gpairs(Data.AnAP, upper.pars = list(scatter = 'stats'),
# #     scatter.pars = list(pch = 1:3,
# #                           col = as.numeric(Data.AnAP$cluster)))


#RR<-data.frame(row.names(data.HTTP5))
# data.HTTP6<-cbind(data.HTTP5,data.frame(row.names(data.HTTP5)))
# names(data.HTTP6)[names(data.HTTP6)=="row.names.data.HTTP5."]="RowName";

# ############类1-统计#############
# R1<-data.frame(row.names(data.KM1))
# names(R1)[names(R1)=="row.names.data.KM1."]="RowName";
# Cluster1<- (data.HTTP6[,'RowName']) %in% R1$RowName
# data.Cluster1<-data.HTTP6[Cluster1,c('eNBip','MMEIP')]
# 
# 
# data.Cluster1$eNBip<-as.numeric(data.Cluster1$eNBip)
# data.Cluster1$MMEIP<-as.numeric(data.Cluster1$MMEIP)
# 
# table1<-data.frame(table(data.Cluster1$eNBip))
# 
# table1<-table1[order(table1[,2],decreasing=TRUE),]
# 
# 
# ############类2-统计#############
# R2<-data.frame(row.names(data.KM2))
# names(R2)[names(R2)=="row.names.data.KM2."]="RowName";
# Cluster2<- (data.HTTP6[,'RowName']) %in% R2$RowName
# data.Cluster2<-data.HTTP6[Cluster2,c('eNBip','MMEIP')]
# 
# 
# data.Cluster2$eNBip<-as.numeric(data.Cluster2$eNBip)
# data.Cluster2$MMEIP<-as.numeric(data.Cluster2$MMEIP)
# 
# table2<-data.frame(table(data.Cluster2$eNBip))
# 
# table2<-table2[order(table2[,2],decreasing=TRUE),]
# ############类3-统计#############
# R3<-data.frame(row.names(data.KM3))
# names(R3)[names(R3)=="row.names.data.KM3."]="RowName";
# Cluster3<- (data.HTTP6[,'RowName']) %in% R3$RowName
# data.Cluster3<-data.HTTP6[Cluster3,c('eNBip','MMEIP')]
# 
# 
# data.Cluster3$eNBip<-as.numeric(data.Cluster3$eNBip)
# data.Cluster3$MMEIP<-as.numeric(data.Cluster3$MMEIP)
# 
# table3<-data.frame(table(data.Cluster3$eNBip))
# 
# table3<-table3[order(table3[,2],decreasing=TRUE),]
# 
# 
# ############类4-统计#############
# R4<-data.frame(row.names(data.KM4))
# names(R4)[names(R4)=="row.names.data.KM4."]="RowName";
# Cluster4<- (data.HTTP6[,'RowName']) %in% R4$RowName
# data.Cluster4<-data.HTTP6[Cluster4,c('eNBip','MMEIP')]
# 
# 
# data.Cluster4$eNBip<-as.numeric(data.Cluster4$eNBip)
# data.Cluster4$MMEIP<-as.numeric(data.Cluster4$MMEIP)
# 
# table4<-data.frame(table(data.Cluster4$eNBip))
# 
# table4<-table4[order(table4[,2],decreasing=TRUE),]
# 
# ############类5-统计#############
# R5<-data.frame(row.names(data.KM5))
# names(R5)[names(R5)=="row.names.data.KM5."]="RowName";
# Cluster5<- (data.HTTP6[,'RowName']) %in% R5$RowName
# data.Cluster5<-data.HTTP6[Cluster5,c('eNBip','MMEIP')]
# 
# 
# data.Cluster5$eNBip<-as.numeric(data.Cluster5$eNBip)
# data.Cluster5$MMEIP<-as.numeric(data.Cluster5$MMEIP)
# 
# table5<-data.frame(table(data.Cluster5$eNBip))
# 
# table5<-table5[order(table5[,2],decreasing=TRUE),]
# 
# summary(table1)
# #############
# Percentage<-function(data){
#   
#   table1$Var1<-as.numeric(table1$Var1)
#   for(i in 1:20){
#     cat("",as.numeric(table1[1,1]))
#     cat("",as.numeric(table1[1,2]))
#     cat("",as.numeric(table1[1,3]))
#   }
# }
# 
# a<-c(2,1,1,1,2,2,2,1,1,1,1,1)
# table(a)