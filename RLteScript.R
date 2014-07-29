###########安装包############
# run only once
# install.packages("ggplot2")
# install.packages("knitr")
# install.packages("data.table")
# install.packages("gpairs")
# install.packages("plotrix")
# install.packages("pvclust")
# install.packages("cluster")
# install.packages("fpc")
# install.packages("scatterplot3d")
# install.packages("rgl")
# install.packages(arules)

######载入包##########
########ggplot
library("ggplot2")
########SVM包
#library("kernlab")
###########贝叶斯包
#library("bnlearn")
########Stringr包
library("stringr")
#########knitr包
library("knitr")
#########data.table包
library("data.table")
library("gpairs")
library("plotrix")
#######常用聚类算法
library("pvclust")
library("cluster")
# ########fpc包
# #fpc包中的pamk()
# library("fpc")
library("rgl")
#关联规则
library(arules)  #加载arules程序包

##############导入HTTP数据#######################
data_DisHttp<-read.table("201406191100-ltehttpwap-sig13-11675500972.DAT"
                         ,header=TRUE,sep="|",fill=TRUE,colClasses="character",quote="",comment.char="")

#######数据清理############
######删除格式错误数据
Delerror<-(data_DisHttp$EndTime!='')
data_HTTP<-data_DisHttp[Delerror,]
rm(Delerror)

#########删除流程持续时间为0,上行在线时长/下行在线时长为0的项
HttpTime<-((data_HTTP$UpTime!='0')&(data_HTTP$DownTime!='0'))  #(data_HTTP$ProcedureTime!=0)&
data_HTTP<-data_HTTP[HttpTime,]
rm(HttpTime)

########数据信息##########
#L4协议中‘0’表示TCP协议数据，‘1’表示UDP协议数据
L4<-(data_HTTP$L4=='1')
data_udp<-data_HTTP[L4,]
cat("共有：",nrow(data_HTTP),"条数据，其中udp数据包含：",nrow(data_udp),"条。")

#Timestamp Conversion

#数据记录时间
cat("业务最早开始于：",as.character(min(as.POSIXlt(as.numeric(substr(data_HTTP$StartT,1,10)),"UTC", origin="1970-01-01"))),"")
cat("业务最晚开始与：",as.character(max(as.POSIXlt(as.numeric(substr(data_HTTP$StartT,1,10)),"UTC", origin="1970-01-01"))),"")
cat("业务最早结束于：",as.character(min(as.POSIXlt(as.numeric(substr(data_HTTP$StopT,1,10)),"UTC", origin="1970-01-01"))),"")
cat("业务最晚结束于: ",as.character(max(as.POSIXlt(as.numeric(substr(data_HTTP$StopT,1,10)),"UTC", origin="1970-01-01"))),"")

StartTimes <-as.POSIXlt(as.numeric(substr(data_HTTP$StartT,1,10)),"UTC", origin="1970-01-01")
StartMilliseconds <-as.numeric(substr(data_HTTP$StartT,11,13))/1000
EndTimes   <-as.POSIXlt(as.numeric(substr(data_HTTP$StopT,1,10)),"UTC", origin="1970-01-01")
EndMilliseconds   <-as.numeric(substr(data_HTTP$StopT,11,13))/1000
RecodeTime <-data.frame(StartTimes,StartMilliseconds,EndTimes,EndMilliseconds)

attach(RecodeTime) #StartTimes  EndTimes  StartMilliseconds	EndMilliseconds
cat("Data recoded in:",max(EndTimes)-min(StartTimes)," minits")

#######流程时间#########
# ProcedureTime<-(as.numeric(data_HTTP$StopT) - as.numeric(data_HTTP$StartT))
# data_HTTP<-data.frame(data_HTTP,ProcedureTime)
# rm(ProcedureTime)

########数据提取##########
#######上行平均带宽
upAvBand<-as.numeric(data_HTTP$UpTraffic)/as.numeric(data_HTTP$UpTime)
data_HTTP<-data.frame(data_HTTP,upAvBand)
rm(upAvBand)

#######下行平均带宽
downAvBand<-as.numeric(data_HTTP$DownTraffic)/as.numeric(data_HTTP$DownTime)
data_HTTP<-data.frame(data_HTTP,downAvBand)
rm(downAvBand)

# #########上行误码率###############
# UperrorRate<-(as.numeric(data_HTTP$UpRePac)/(as.numeric(data_HTTP$UpPac)+as.numeric(data_HTTP$UpRePac)))
# data_HTTP<-data.frame(data_HTTP,UperrorRate)
# rm(UperrorRate)
# 
# #########下行误码率###############
# DownerrorRate<-(as.numeric(data_HTTP$DownRePac)/(as.numeric(data_HTTP$DownPac)+as.numeric(data_HTTP$DownRePac)))
# data_HTTP<-data.frame(data_HTTP4,DownerrorRate)
# rm(DownerrorRate)

########FirstRespondTime、LastPacketTime、LastAckTime
firstRespondTime<-(as.numeric(data_HTTP$FirstRespondTime))
data_HTTP<-data.frame(data_HTTP,firstRespondTime)

lastPacketTime<-(as.numeric(data_HTTP$LastPacketTime))
data_HTTP<-data.frame(data_HTTP,lastPacketTime)

lastAckTime<-(as.numeric(data_HTTP$LastAckTime))
data_HTTP<-data.frame(data_HTTP,lastAckTime)
rm(firstRespondTime,lastPacketTime,lastAckTime)

################聚类准备##################
Data_PreAnalyse<-data_HTTP[c('upAvBand',  'downAvBand','firstRespondTime','lastPacketTime','lastAckTime')]  #,  'UperrorRate','DownerrorRate'

Data_PreAnalyse$firstRespondTime   <-as.numeric(Data_PreAnalyse$firstRespondTime)
Data_PreAnalyse$lastPacketTime     <-as.numeric(Data_PreAnalyse$lastPacketTime)
Data_PreAnalyse$lastAckTime        <-as.numeric(Data_PreAnalyse$lastAckTime)

# ##draw in 3d
# attach(Data_PreAnalyse)
# plot3d(upAvBand,downAvBand,firstRespondTime,xlab="upAvBand",ylab="downAvBand",zlab="FirstRespondTime")
# plot3d(upAvBand,downAvBand,lastPacketTime,xlab="upAvBand",ylab="downAvBand",zlab="LastPacketTime")
# plot3d(upAvBand,downAvBand,lastAckTime,xlab="upAvBand",ylab="downAvBand",zlab="LastAckTime")
# plot3d(FirstRespondTime,LastPacketTime,LastAckTime,xlab="HttpFirstRespondTime(MS)",ylab="HttpLastPacketTime(MS)",zlab="HttpLastAckTime(MS)")

Data_Scaled<-data.frame(scale(Data_PreAnalyse))

# attach(Data_PreAnalyse)
# plot3d(upAvBand,downAvBand,FirstRespondTime,main="")
# plot3d(upAvBand,downAvBand,LastPacketTime,main="")
# plot3d(upAvBand,downAvBand,LastAckTime,main="")
# plot3d(FirstRespondTime,LastPacketTime,LastAckTime,main="")

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
  resultSSE <- c()
  resultSSE[begin:end] <- 0
  # 遍历计算kmeans的SSE
  for(i in begin:end) {  
    # 计算SSE  
    tmp = c()
    tmp[1:count] = 0  
    for(j in 1:count) {    
      kcluster = kmeans(data, i)   
      tmp[j] = kcluster$tot.withinss  
    }  
    resultSSE[i] <- mean(tmp)
  }
  return(resultSSE)
}

system.time(resultSSE<-CalculeSSE(Data_Scaled))
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
  resultSC <- c()
  resultSC[begin:end]<- -1
  # 遍历计算kmeans的SSE
  for(i in begin:end) {  
    # Silhouette coefficient  
    tmp = c()  
    tmp[1:count] = 0  
    for(j in 1:count) {    
      kcluster = clara(data, i)    
      tmp[j] = kcluster$silinfo$avg.width   #silinfo : a list with all silhouette information,   
    }  
    resultSC[i]  <- mean(tmp)
  }
  return(resultSC)
}

system.time(resultSC<-CalculeSC(Data_Scaled))
# 绘制结果
plot(resultSC, type="o", xlab="Number of Cluster", ylab="Silhouette Coefficient")

# K=8时值最大，所以聚类效果最佳。

rm(resultSC)

###########聚类############
#bcl<-bootFlexclust(newDat, k=2:15, nboot=50, FUN=cclust, multicore=FALSE)
#######K-means############3
# pkm<-kmeans(Data_Scaled,8,nstart=25,iter.max=10,algorithm="Hartigan-Wong")
# attach(Data_Scaled)
# # plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col=pkm$cluster)
# plot(x=Data_PreAnalyseScaled[,2],y=Data_PreAnalyseScaled[,3],col=pkm$cluster,xlim=c(-5,10),ylim=c(-2,10),main="聚5类图",xlab="",ylab="") #,xlim=c(-5,0.5),ylim=c(-5,5)
# plot(Data_PreAnalyseScaled,col=pkm$cluster)#,xlab="",ylab="",xlim=c(-5,2),ylim=c(-3,6))

###############CLARA (Clustering for Large Applications) algorithm###################
# It works by clustering a sample from the dataset and then assigns all objects in the dataset to these clusters.
#需要使用cluster包
kmC<-clara(Data_Scaled,8)
kmC$clusinfo
attach(Data_PreAnalyse)
plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col=kmC$clustering,xlim=c(0,200),ylim=c(0,200),zlim=c(0,200))
plot3d(firstRespondTime,lastPacketTime,lastAckTime,size=3,col=kmC$clustering)

clust<-data.frame(kmC$clustering)
data_cluster<-data.frame(data_HTTP,clust)

# cluste2<-kmC$clustering==2
# data_cluste2<-data_cluster[cluste2,] 
# attach(data_cluste2)
# plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col='blue')
# plot3d(firstRespondTime,lastPacketTime,lastAckTime,size=3,col=kmC$clustering)

itemCount<-function(item){
  ItemCount<-as.data.frame(table((item)))
  Item<-as.numeric(as.character(ItemCount[,1]))
  Freq<-as.numeric(as.character(ItemCount[,2]))
  ItemFreq<-data.frame(Item,Freq)
  return(ItemFreq)
}

eNbCount<-itemCount(data_cluster$eNBip)
SGWCount<-itemCount(data_cluster$SGWIP)

#merge data with same xdr id
XdrCount<-data.frame(table(data_HTTP$xDRID))
XdrCount <- data.frame(lapply(XdrCount, as.character), stringsAsFactors=FALSE)
longxdr<-DeletRow(XdrCount,2)

mergedata<-function(data){
  longData<-c()
  for(l in 1:nrow(data)){  
    long<-(data_HTTP$xDRID==data[l,1])
    temp<-data_HTTP[long,]
    #temp<-data_HTTP[c("StartT","StopT","UpTraffic","DownTraffic","UpTime","DownTime","FirstRespondTime","LastPacketTime","LastAckTime")]
    tapply()
    longData<-rbind(longData,temp)
  }
  return(longData)
}
LongData<-mergedata(longxdr)

sorteNB<-sort(eNbCount$Freq,decreasing=TRUE)
sorteNB[1:20]
sortSGW<-sort(SGWCount$Freq,decreasing=TRUE)
sortSGW[1:30]

cat("共连接",nrow(eNbCount),"个基站") 

DeletRow<-function(item,seul){
  newItem<-item[1,]
  for(i in 1: nrow(item)){
    Nfreq<-item$Freq[i]
    if(Nfreq >= seul){
      newItem<-rbind(newItem,item[i,])
    }
  }
  newItem<-newItem[-1,]
  return(newItem)
}

NewEnb<-DeletRow(eNbCount,40)
NewSgw<-DeletRow(SGWCount,500)

cat("常用的基站有：",nrow(NewEnb),"座")
cat("常用的SGW有：",nrow(NewSgw),"个")





# frequnentSgwData<-(data_cluster$SGWIP %in% NewSgw$Item)
# data_freSgw<-data_cluster[frequnentSgwData,]
# Delerror<-(data.DisHttp$EndTime!='')
# data_HTTP<-data.DisHttp[Delerror,]




# plotClasterData<-function(data,Clusting,PcaData,num){
#   for(i in 1:num){
#     cluste<-Clusting$clustering==num
#     data.cluste<-PcaData[cluste,]    
#   }
# }

# analyseData<-function(data,Clusting,PcaData,num){
#   #for(i in 1:num){
#   cluste<-Clusting$clustering==num
#   data_cluste<-PcaData[cluste,]    
#   a<-data.frame(row.names(data_cluste))
#   matchs<-row.names(data) %in% a[,1]
#   data.matchs<-data[matchs,]
#   return(data.matchs)
#   #}   
# }
# 
# Clara1<-analyseData(Data_PreAnalyseScaled,kmC,newDat,1)
# Clara2<-analyseData(Data_PreAnalyseScaled,kmC,newDat,2)
# Clara3<-analyseData(Data_PreAnalyseScaled,kmC,newDat,3)
# Clara4<-analyseData(Data_PreAnalyseScaled,kmC,newDat,4)
# Clara5<-analyseData(Data_PreAnalyseScaled,kmC,newDat,5)
# 
# nrow(Clara1)
# nrow(Clara2)
# nrow(Clara3)
# nrow(Clara4)
# nrow(Clara5)

# drawPieChar<-function(){
#   Values<-c(nrow(Clara1),nrow(Clara2),nrow(Clara3),nrow(Clara4),nrow(Clara5))
#   Labels<-c("group1","group2","group3","group4","group5")
#   percent_str <- paste(round(Values/sum(Values) * 100,1), "%", sep="")
#   Values <- data.frame(Percentage <- round(Values/sum(Values) * 100,1), Type = Labels,percent=percent_str )
#   names(Values)<-c("Percentage","Type","percent")
#   
#   pie <- ggplot(Values, aes(x = "" ,y = Percentage, fill = Labels)) +  geom_bar(stat="identity",width = 3) + labs(title = "各组中条数比较",x = "",y = "")
#   pie = pie + coord_polar("y")
#   pie = pie + xlab('') + ylab('') + labs(fill="Types")
#   pie
#   return(pie)
# }
# 
# drawPieChar()

# StastiqueClara<-function(data,Clusting,PcaData,num){
#   data_mean<-c()
#   MEAN<-c() 
#   for(i in 1:num){
#     cluste<-Clusting$clustering==i
#     data_cluste<-PcaData[cluste,]    
#     a<-data.frame(row.names(data_cluste))
#     matchs<-row.names(data) %in% a[,1]
#     data_matchs<-data[matchs,]    
#     #cat(data_matchs[1,1])
#     #
#     for(b in 1:ncol(data_matchs)){
#       ColMean<-mean(data_matchs[,b])
#       data_mean<-cbind(data_mean,ColMean)
#     } 
#     #     writeLines("")
#     #     cat("MEAN",MEAN)
#     #     writeLines("")
#     #     cat("data_mean",data_mean)
#     MEAN<-rbind(MEAN,data_mean)
#     data_mean<-c()
#   }  
#   return(MEAN)
# }
# 
# ClaraMean<-StastiqueClara(Data.AnA,kmC,newDat,5)
# names(ClaraMean)<-c('upAvBand','downAvBand','UpCorrecteRate','DownCorrecteRate' ,'FirstRespondTime','LastPacketTime','LastAckTime')
# plot(ClaraMean)#,col
# #matrix
# 
# 
# print(kmC)