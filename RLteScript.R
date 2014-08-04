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
# R-weka
# install.packages("RWeka")
# install.packages("RWekajars")
# install.packages("bit")
# install.packages("ff")

######载入包##########
#ggplot
library("ggplot2")
#SVM包
#library("kernlab")
#贝叶斯包
#library("bnlearn")
#Stringr包
library("stringr")
#knitr包
library("knitr")
#data.table包
library("data.table")
library("gpairs")
library("plotrix")
#常用聚类算法
library("pvclust")
library("cluster")
#3d绘图
library("rgl")
#关联规则
library(Matrix)
library(arules)
#字符串处理
library("stringr")
#ff
library("bit")
library("ff")
#Rweka
library("rJava")
library("RWeka")
library("RWekajars"
        
        #######Function##########
        
        itemCount<-function(item){
          ItemCount<- data.frame(table(item))
          ItemCount<- data.frame(lapply(ItemCount, as.character), stringsAsFactors=FALSE)
          return(ItemCount)
        }
        
        mergedata<-function(data){
          longData<-c()
          for(l in 1:nrow(data)){  
            long<-(data_HTTP$xDRID==data[l,1])
            temp<-data_HTTP[long,]
            longData<-rbind(longData,temp)
          }
          return(longData)
        }
        
        MoreFreqRow<-function(item,seul){
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
        
        LessFreqRow<-function(item,seul){
          newItem<-item[1,]
          for(i in 1: nrow(item)){
            Nfreq<-item$Freq[i]
            if(Nfreq <= seul){
              newItem<-rbind(newItem,item[i,])
            }
          }
          newItem<-newItem[-1,]
          return(newItem)
        }
        
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
              kcluster = clara(data, i,metric = "euclidean")    
              tmp[j] = kcluster$silinfo$avg.width   #silinfo : a list with all silhouette information,   
            }  
            resultSC[i]  <- mean(tmp)
          }
          return(resultSC)
        }
        
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
        
        piechat<-function(data,title){
          data[,1]<-as.numeric(data[,1])
          data[,2]<-as.numeric(data[,2])
          lbls <- c()
          num<-nrow(data)
          sum<-as.numeric(apply(data, 2, sum)[2])
          for(i in 1:num){    
            string<-round((data[i,2]/sum)*100)
            string<-str_c("(",i,")：",string,"%")
            lbls<-c(lbls,string)
          }  
          pie(data[,2],col=rainbow(8), labels = lbls, main=title)
        }  
        
        
        cat("----------我是分割线----------")
        
        #画图原是设置
        oldpar <-par()
        
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
        detach(RecodeTime)
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
        Data_PreAnalyse<-data_HTTP[c('upAvBand','downAvBand','firstRespondTime','lastPacketTime','lastAckTime')]  #,  'UperrorRate','DownerrorRate'
        
        Data_PreAnalyse$firstRespondTime   <-as.numeric(Data_PreAnalyse$firstRespondTime)
        Data_PreAnalyse$lastPacketTime     <-as.numeric(Data_PreAnalyse$lastPacketTime)
        Data_PreAnalyse$lastAckTime        <-as.numeric(Data_PreAnalyse$lastAckTime)
        
        # ##draw in 3d
        # attach(Data_PreAnalyse)
        # plot3d(upAvBand,downAvBand,firstRespondTime,xlab="upAvBand",ylab="downAvBand",zlab="FirstRespondTime")
        # plot3d(upAvBand,downAvBand,lastPacketTime,xlab="upAvBand",ylab="downAvBand",zlab="LastPacketTime")
        # plot3d(upAvBand,downAvBand,lastAckTime,xlab="upAvBand",ylab="downAvBand",zlab="LastAckTime")
        # plot3d(FirstRespondTime,LastPacketTime,LastAckTime,xlab="HttpFirstRespondTime(MS)",ylab="HttpLastPacketTime(MS)",zlab="HttpLastAckTime(MS)")
        # detach(Data_PreAnalyse)
        Data_Scaled<-data.frame(scale(Data_PreAnalyse))
        
        # attach(Data_PreAnalyse)
        # plot3d(upAvBand,downAvBand,FirstRespondTime,main="")
        # plot3d(upAvBand,downAvBand,LastPacketTime,main="")
        # plot3d(upAvBand,downAvBand,LastAckTime,main="")
        # plot3d(FirstRespondTime,LastPacketTime,LastAckTime,main="")
        # detach(Data_PreAnalyse)
        ###########K值选择#############
        #通过计算轮廓系数（silhouette coefficient）方法结合了凝聚度和分离度，可以以此来判断聚类的优良性。其值在-1到+1之间取值，值越大表示聚类效果越好。
        #########计算不同K值的SSE#####
        # system.time(resultSSE<-CalculeSSE(Data_Scaled))
        # 
        # ###########计算Silhouette Coefficient#######
        # system.time(resultSC<-CalculeSC(Data_Scaled))
        # 
        # # 绘制结果
        # plot(resultSSE, type="o", xlab="Number of Cluster", ylab="Sum of Squer Error")
        # 
        # plot(resultSC, type="o", xlab="Number of Cluster", ylab="Silhouette Coefficient")
        # # K=8时值最大，所以聚类效果最佳。
        # rm(resultSSE)
        # rm(resultSC)
        
        ###########聚类############
        #bcl<-bootFlexclust(newDat, k=2:15, nboot=50, FUN=cclust, multicore=FALSE)
        #######K-means############3
        # pkm<-kmeans(Data_Scaled,8,nstart=25,iter.max=10,algorithm="Hartigan-Wong")
        # attach(Data_Scaled)
        # # plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col=pkm$cluster)
        # plot(x=Data_PreAnalyseScaled[,2],y=Data_PreAnalyseScaled[,3],col=pkm$cluster,xlim=c(-5,10),ylim=c(-2,10),main="聚5类图",xlab="",ylab="") #,xlim=c(-5,0.5),ylim=c(-5,5)
        # plot(Data_PreAnalyseScaled,col=pkm$cluster)#,xlab="",ylab="",xlim=c(-5,2),ylim=c(-3,6))
        # detach(Data_Scaled)
        ###############CLARA (Clustering for Large Applications) algorithm###################
        # It works by clustering a sample from the dataset and then assigns all objects in the dataset to these clusters.
        #需要使用cluster包
        kmC<-clara(Data_Scaled,8)
        kmC$clusinfo
        
        # # attach(Data_PreAnalyse)
        # # plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col=kmC$clustering,xlim=c(0,200),ylim=c(0,200),zlim=c(0,200))
        # # plot3d(firstRespondTime,lastPacketTime,lastAckTime,size=3,col=kmC$clustering)
        # # detach(Data_PreAnalyse)
        clust<-data.frame(kmC$clustering)
        data_cluster<-data.frame(data_HTTP,clust)
        
        # # cluste2<-kmC$clustering==2
        # # data_cluste2<-data_cluster[cluste2,] 
        # # attach(data_cluste2)
        # # plot3d(upAvBand,downAvBand,firstRespondTime,size=3,col='blue')
        # # plot3d(firstRespondTime,lastPacketTime,lastAckTime,size=3,col=kmC$clustering)
        # # detach(data_cluste2)
        ###########long procedure############
        #merge data with same xdr id
        XdrCount<-itemCount(data_HTTP$xDRID)
        longxdr<-MoreFreqRow(XdrCount,2)
        LongData<-mergedata(longxdr)
        
        ###########analyse data###################
        eNbCount<-itemCount(data_cluster$eNBip)
        SGWCount<-itemCount(data_cluster$SGWIP)
        
        #属性数值化
        eNbCount$item<-as.numeric(eNbCount$item)
        eNbCount$Freq<-as.numeric(eNbCount$Freq)
        
        SGWCount$item<-as.numeric(SGWCount$item)
        SGWCount$Freq<-as.numeric(SGWCount$Freq)
        
        ####统计####
        #前20个最频繁项
        sorteNB<-sort(as.numeric(eNbCount$Freq),decreasing=TRUE)
        sorteNB[1:20]
        sortSGW<-sort(as.numeric(SGWCount$Freq),decreasing=TRUE)
        sortSGW[1:30]
        
        cat("共连接",nrow(eNbCount),"个基站") 
        cat("共连接",nrow(SGWCount),"个SGW") 
        
        ####连接更多UE的基站####
        MoreEnb<-MoreFreqRow(eNbCount,100)
        MoreSgw<-MoreFreqRow(SGWCount,500)
        
        cat("常用的基站有：",nrow(MoreEnb),"座")
        cat("常用的SGW有 ：",nrow(MoreSgw),"个")
        
        
        FrequnentEnb<-(data_cluster$eNBip %in% MoreEnb$item)
        data_freEnb<-data_cluster[FrequnentEnb,][c(77,78,79,80,81,82)]
        
        ####连接较少UE的基站####
        LessEnb<-LessFreqRow(eNbCount,100)
        LessSgw<-LessFreqRow(SGWCount,500)
        
        cat("较不常用的基站有：",nrow(LessEnb),"座")
        cat("较不常用的SGW 有 ：",nrow(LessSgw),"个")
        
        
        NotFrequnentEnb<-(data_cluster$SGWIP %in% LessSgw$item)
        data_LessfreEnb<-data_cluster[NotFrequnentEnb,][c(77,78,79,80,81,82)]
        rm(SGWCount,sorteNB,sortSGW,MoreEnb,MoreSgw,FrequnentEnb,LessEnb,LessSgw,NotFrequnentEnb)
        #统计
        summary(data_freEnb)
        summary(data_LessfreEnb)
        
        cat("从两部分数据的最大值和均值中可以看出基站连接UE的数量的多少与各项时延成正比，而与下行流量成反比。而与上行流量没有较大关系")
        
        freCluste<-itemCount(data_freEnb$kmC.clustering)
        lessCluste<-itemCount(data_LessfreEnb$kmC.clustering)
        
        # 
        par(mfrow=c(1,2))#一行同时显示两个团
        piechat(freCluste ,"pie char: many UE connected")
        piechat(lessCluste,"pie char: less UE connected")
        par(oldpar)#还原设置
        # 
        # #+RTS 
        # 
        data_PreAR<-data_cluster[c(77,78,79,80,81,82)]
        #根据类划分数据。
        Data_ARule<-split(data_PreAR[1:5],data_PreAR[,6])
        #统计每个类的信息
        summary(data.frame(Data_ARule[1]))
        summary(data.frame(Data_ARule[2]))
        summary(data.frame(Data_ARule[3]))
        summary(data.frame(Data_ARule[4]))
        summary(data.frame(Data_ARule[5]))
        summary(data.frame(Data_ARule[6]))
        summary(data.frame(Data_ARule[7]))
        summary(data.frame(Data_ARule[8]))
        
        # 
        # ###关联规则#####
        # 
        #寻找区间
        qujian<-function(data,nk){  
          TData<-c()
          for(i in 1:nk){
            CData<-c()
            clu<-(data$kmC.clustering==i)
            datacluste<-data[clu,][,-6]
            for(n in 1:ncol(datacluste)){
              minV<-min(datacluste[,n])
              maxV<-max(datacluste[,n])
              RData<-rbind(minV,maxV)   
              CData<-cbind(CData,RData)
            }
            TData<-rbind(TData,CData)
          }
          rowname<-c("1","1","2","2","3","3","4","4","5","5","6","6","7","7","8","8")
          TData<-cbind(TData,rowname)
          return(TData)
        }
        
        system.time(Max_Min<-qujian(data_PreAR,8))
        #优化版
        qujian2<-function(data,nc){  
          TData<-c()
          for(i in 1:nc){
            CData<-c()
            datacluste<-data.frame(data[i])
            for(n in 1:ncol(datacluste)){
              minV<-min(datacluste[,n])
              maxV<-max(datacluste[,n])
              RData<-rbind(minV,maxV)   
              CData<-cbind(CData,RData)
            }
            TData<-rbind(TData,CData)    
          }
          return(TData)
        }
        
        system.time(Max_Min<-qujian2(Data_ARule,8))
        
        findSeuil<-function(Data){  
          RowSeuil<-c()
          #ColSeuil<-c()
          for(x in 1:5){
            ColSeuil<-c()
            data<-sort(as.numeric(Data[,x]))
            for(y in 1:15){
              seuil<-abs((data[y]-data[y+1])/2)+data[y]
              ColSeuil<-rbind(ColSeuil,seuil)
            }
            RowSeuil<-cbind(RowSeuil,ColSeuil)
          }
          return(RowSeuil)
        }
        
        system.time(Seuil<-findSeuil(Max_Min))
        
        arrangeValue<-function(data){
          for(x in 1:ncol(data)){
            maxX<-0
            maxY<-0
            for(y in 1:nrow(data)){
              if(data[y,x]==0){
                maxX<-x
                maxY<-y
              }
            }
            temY<-maxY+1
            tmaxY<-nrow(data)-maxY
            for(z in 1:nrow(data)){      
              if((maxY+z)<=nrow(data)){
                data[z,x]<-data[(maxY+z),x]
              }
              if((maxY+z)>nrow(data)){
                data[z,x]<-0
              }
            }
          }
          return(data)
        }
        
        system.time(Seuil<-arrangeValue(Seuil))
        
        #讲数值转换为区间
        data_AR<-data_PreAR
        
        ToString<-function(data,seuil){  
          for(x in 1:5){
            data[,x]<-as.numeric(data[,x])
            for(y in 1:nrow(data)){  
              
              #       cat(data[y,x],"\n")
              if(data[y,x]<=seuil[1,x]) {data[y,x]<-'1'}
              else if((seuil[1,x]<data[y,x]  && data[y,x]<=seuil[2,x]  && seuil[2,x]!=0)||  (seuil[1,x] <=data[y,x]&&seuil[2,x]==0))  {data[y,x]<-'2'}
              else if((seuil[2,x]<data[y,x]  && data[y,x]<=seuil[3,x]  && seuil[3,x]!=0)||  (seuil[2,x] <=data[y,x]&&seuil[3,x]==0))  {data[y,x]<-'3'}
              else if((seuil[3,x]<data[y,x]  && data[y,x]<=seuil[4,x]  && seuil[4,x]!=0)||  (seuil[3,x] <=data[y,x]&&seuil[4,x]==0))  {data[y,x]<-'4'}
              else if((seuil[4,x]<data[y,x]  && data[y,x]<=seuil[5,x]  && seuil[5,x]!=0)||  (seuil[4,x] <=data[y,x]&&seuil[5,x]==0))  {data[y,x]<-'5'}
              else if((seuil[5,x]<data[y,x]  && data[y,x]<=seuil[6,x]  && seuil[6,x]!=0)||  (seuil[5,x] <=data[y,x]&&seuil[6,x]==0))  {data[y,x]<-'6'}
              else if((seuil[6,x]<data[y,x]  && data[y,x]<=seuil[7,x]  && seuil[7,x]!=0)||  (seuil[6,x] <=data[y,x]&&seuil[7,x]==0))  {data[y,x]<-'7'}
              else if((seuil[7,x]<data[y,x]  && data[y,x]<=seuil[8,x]  && seuil[8,x]!=0)||  (seuil[7,x] <=data[y,x]&&seuil[8,x]==0))  {data[y,x]<-'8'}
              else if((seuil[8,x]<data[y,x]  && data[y,x]<=seuil[9,x]  && seuil[9,x]!=0)||  (seuil[8,x] <=data[y,x]&&seuil[9,x]==0))  {data[y,x]<-'9'}
              else if((seuil[9,x]<data[y,x]  && data[y,x]<=seuil[10,x] && seuil[10,x]!=0)|| (seuil[9,x] <=data[y,x]&&seuil[10,x]==0)) {data[y,x]<-'10'}
              else if((seuil[10,x]<data[y,x] && data[y,x]<=seuil[11,x] && seuil[11,x]!=0)|| (seuil[10,x]<=data[y,x]&&seuil[11,x]==0)) {data[y,x]<-'11'}
              else if((seuil[11,x]<data[y,x] && data[y,x]<=seuil[12,x] && seuil[12,x]!=0)|| (seuil[11,x]<=data[y,x]&&seuil[12,x]==0)) {data[y,x]<-'12'}
              else if((seuil[12,x]<data[y,x] && data[y,x]<=seuil[13,x] && seuil[13,x]!=0)|| (seuil[12,x]<=data[y,x]&&seuil[13,x]==0)) {data[y,x]<-'13'}
              else if((seuil[13,x]<data[y,x] && data[y,x]<=seuil[14,x] && seuil[14,x]!=0)|| (seuil[13,x]<=data[y,x]&&seuil[14,x]==0)) {data[y,x]<-'14'}
              else if((seuil[14,x]<data[y,x] && data[y,x]<=seuil[15,x] && seuil[15,x]!=0)|| (seuil[14,x]<=data[y,x]&&seuil[15,x]==0)) {data[y,x]<-'15'}
              else if(seuil[15,x]<data[y,x] && seuil[15,x]!=0) {data[y,x]<-'16'}
              
            }  
          }
          return(data)
        }  
        
        system.time(Data_AR<-ToString(data_PreAR,Seuil))
        
        ToStringNew<-function(data,seuil){  
          ColData<-c()
          for(x in 1:5){
            Data<-as.numeric(data[,x])
            for(y in 1:nrow(seuil)){  
              rowData1<-Data<=Seuil[1,]
              RowData1<-Data[rowData1]   
              RowData1[]<-"1"
              rowData2<-((seuil[1,]<=Data  && Data<=seuil[2,]  && seuil[2,]!=0)||(seuil[1,]<=Data&&seuil[2,]==0))
              RowData2<-Data[rowData2] 
              RowData2<-"2"
              rowData3<-((seuil[2,]<=Data  && Data<=seuil[3,]  && seuil[3,]!=0)||(seuil[2,]<=Data&&seuil[3,]==0))
              RowData3<-Data[rowData3] 
              RowData3<-"3"
              rowData4<-((seuil[3,]<=Data  && Data<=seuil[4,]  && seuil[4,]!=0)||(seuil[3,]<=Data&&seuil[4,]==0))
              RowData4<-Data[rowData4] 
              RowData4<-"4"
              rowData5<-((seuil[4,]<=Data  && Data<=seuil[5,]  && seuil[5,]!=0)||(seuil[4,]<=Data&&seuil[5,]==0))
              RowData5<-Data[rowData5] 
              RowData5<-"5"
              rowData6<-((seuil[5,]<=Data  && Data<=seuil[6,]  && seuil[6,]!=0)||(seuil[5,]<=Data&&seuil[6,]==0))
              RowData6<-Data[rowData6] 
              RowData6<-"6"
              rowData7<-((seuil[6,]<=Data  && Data<=seuil[7,]  && seuil[7,]!=0)||(seuil[6,]<=Data&&seuil[7,]==0))
              RowData7<-Data[rowData7] 
              RowData7<-"7"
              rowData8<-((seuil[7,]<=Data  && Data<=seuil[8,]  && seuil[8,]!=0)||(seuil[7,]<=Data&&seuil[8,]==0))
              RowData8<-Data[rowData8] 
              RowData8<-"8"
              rowData9<-((seuil[8,]<=Data  && Data<=seuil[9,]  && seuil[9,]!=0)||(seuil[8,]<=Data&&seuil[9,]==0))
              RowData9<-Data[rowData9] 
              RowData9<-"9"
              rowData10<-((seuil[9,]<=Data  && Data<=seuil[10,]  && seuil[10,]!=0)||(seuil[9,]<=Data&&seuil[10,]==0))
              RowData10<-Data[rowData10] 
              RowData10<-"10"
              rowData11<-((seuil[10,]<=Data  && Data<=seuil[11,]  && seuil[11,]!=0)||(seuil[10,]<=Data&&seuil[11,]==0))
              RowData11<-Data[rowData11] 
              RowData11<-"11"
              rowData12<-((seuil[11,]<=Data  && Data<=seuil[12,]  && seuil[12,]!=0)||(seuil[11,]<=Data&&seuil[12,]==0))
              RowData12<-Data[rowData12] 
              RowData12<-"12"
              rowData13<-((seuil[12,]<=Data  && Data<=seuil[13,]  && seuil[13,]!=0)||(seuil[12,]<=Data&&seuil[13,]==0))
              RowData13<-Data[rowData13] 
              RowData13<-"13"
              rowData14<-((seuil[13,]<=Data  && Data<=seuil[14,]  && seuil[14,]!=0)||(seuil[13,]<=Data&&seuil[14,]==0))
              RowData14<-Data[rowData14] 
              RowData14<-"14"
              rowData15<-((seuil[14,]<=Data  && Data<=seuil[15,]  && seuil[15,]!=0)||(seuil[14,]<=Data&&seuil[15,]==0))
              RowData15<-Data[rowData15] 
              RowData15<-"15"
              rowData16<-((seuil[15,]<=Data  && seuil[15,]!=0))
              RowData16<-Data[rowData16] 
              RowData16<-"16"
              rowData<-rbind(RowData15,  RowData14,	RowData13,	RowData12,	RowData11
                             ,	RowData10,	RowData9,	RowData8,	RowData7,	RowData6
                             ,	RowData5,	RowData4,	RowData3,	RowData2,	RowData1)
              
              
            }
            #     RowData<-c()
            #     nr<-row.names(rowData)
            #     RowData<-cbind(rowData,row.names(rowData))
            #ColData<-merge(x=rowData,y=ColData,by="row.names(rowData)")
          }
          return(rowData)
        }  
        rm(Data_AR2)
        system.time(Data_AR2<-ToStringNew(data_PreAR,Seuil))
        ####使用关联规则####
        
        str(Data_AR)
        system.time(transaction_data <- as(lapply(Data_AR[1:5], "[[", 1), "transactions"))
        system.time(transaction_data <- as(split(Data_AR[1:5],as.character(c(1:16))), "transactions")) # original_data[,"id"], original_data[,"type"]
        system.time(frequentsets<-eclat(transaction_data,parameter=list(support=0.05,maxlen=10))  )
        
        
        
        
        # 
        # 
        # # plotClasterData<-function(data,Clusting,PcaData,num){
        # #   for(i in 1:num){nn
        # #     cluste<-Clusting$clustering==num
        # #     data.cluste<-PcaData[cluste,]    
        # #   }
        # # }
        # 
        # # analyseData<-function(data,Clusting,PcaData,num){
        # #   #for(i in 1:num){
        # #   cluste<-Clusting$clustering==num
        # #   data_cluste<-PcaData[cluste,]    
        # #   a<-data.frame(row.names(data_cluste))
        # #   matchs<-row.names(data) %in% a[,1]
        # #   data.matchs<-data[matchs,]
        # #   return(data.matchs)
        # #   #}   
        # # }
        # # 
        # # Clara1<-analyseData(Data_PreAnalyseScaled,kmC,newDat,1)
        # # Clara2<-analyseData(Data_PreAnalyseScaled,kmC,newDat,2)
        # # Clara3<-analyseData(Data_PreAnalyseScaled,kmC,newDat,3)
        # # Clara4<-analyseData(Data_PreAnalyseScaled,kmC,newDat,4)
        # # Clara5<-analyseData(Data_PreAnalyseScaled,kmC,newDat,5)
        # # 
        # # nrow(Clara1)
        # # nrow(Clara2)
        # # nrow(Clara3)
        # # nrow(Clara4)
        # # nrow(Clara5)
        # 
        # # drawPieChar<-function(){
        # #   Values<-c(nrow(Clara1),nrow(Clara2),nrow(Clara3),nrow(Clara4),nrow(Clara5))
        # #   Labels<-c("group1","group2","group3","group4","group5")
        # #   percent_str <- paste(round(Values/sum(Values) * 100,1), "%", sep="")
        # #   Values <- data.frame(Percentage <- round(Values/sum(Values) * 100,1), Type = Labels,percent=percent_str )
        # #   names(Values)<-c("Percentage","Type","percent")
        # #   
        # #   pie <- ggplot(Values, aes(x = "" ,y = Percentage, fill = Labels)) +  geom_bar(stat="identity",width = 3) + labs(title = "各组中条数比较",x = "",y = "")
        # #   pie = pie + coord_polar("y")
        # #   pie = pie + xlab('') + ylab('') + labs(fill="Types")
        # #   pie
        # #   return(pie)
        # # }
        # # 
        # # drawPieChar()
        # 
        # # StastiqueClara<-function(data,Clusting,PcaData,num){
        # #   data_mean<-c()
        # #   MEAN<-c() 
        # #   for(i in 1:num){
        # #     cluste<-Clusting$clustering==i
        # #     data_cluste<-PcaData[cluste,]    
        # #     a<-data.frame(row.names(data_cluste))
        # #     matchs<-row.names(data) %in% a[,1]
        # #     data_matchs<-data[matchs,]    
        # #     #cat(data_matchs[1,1])
        # #     #
        # #     for(b in 1:ncol(data_matchs)){
        # #       ColMean<-mean(data_matchs[,b])
        # #       data_mean<-cbind(data_mean,ColMean)
        # #     } 
        # #     #     writeLines("")
        # #     #     cat("MEAN",MEAN)
        # #     #     writeLines("")
        # #     #     cat("data_mean",data_mean)
        # #     MEAN<-rbind(MEAN,data_mean)
        # #     data_mean<-c()
        # #   }  
        # #   return(MEAN)
        # # }
        # # 
        # # ClaraMean<-StastiqueClara(Data.AnA,kmC,newDat,5)
        # # names(ClaraMean)<-c('upAvBand','downAvBand','UpCorrecteRate','DownCorrecteRate' ,'FirstRespondTime','LastPacketTime','LastAckTime')
        # # plot(ClaraMean)#,col
        # # #matrix
        # # 
        # # 
        # # print(kmC)
        
        #rweka
        # write.arff(Data_Scaled,"test.arff")
        
        
        