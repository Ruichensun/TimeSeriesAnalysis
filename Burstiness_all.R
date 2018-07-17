setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA")
library(ggplot2)
  fly_files_list<-list()  
  fly_files<-c()
  Burst_all<-c()
  fly_files_index<-c()
  fly_number = c(1:450)
  for (i in fly_number) {
    fly_files = dir(pattern = paste0("Fly",i,"_E1","_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
    if (length(fly_files) ==1){
      fly_files_list<-append(fly_files_list,fly_files)
    }
  }
  
  pdf(paste0("Burstiness of Fly.pdf"))
  
  for (i in 1:length(fly_files_list)){
  
    x = read.table(fly_files_list[[i]],header=T,sep=",",stringsAsFactors = F)
    
    Index<-basename(fly_files_list[[i]])
    Index<-strsplit(strsplit(Index,"_")[[1]][2],"y")[[1]][2]
    fly_files_index<-c(fly_files_index,Index)
    
    speed_pre<-x$fly.position[2:length(x$fly.position)]-x$fly.position[1:(length(x$fly.position)-1)]
    burstiness_pre<-abs(speed_pre)
    burstiness_pre<-replace(burstiness_pre,burstiness_pre>0,1)
    burstiness_pretraining<-data.frame(burstiness_pre)
    p1<-ggplot(burstiness_pretraining, aes(x=(1:length(burstiness_pre))/20, y=burstiness_pre))
    p1<-p1+geom_bar(stat="identity")+ggtitle(paste0("Fly ",fly_number, "Before Training"))+xlab("Time(sec)")+ylab("Activity")+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
    
    par=(mfrow=c(5,1))
    plot(p1, main = paste0("Fly ",Index," E1 Burstiness"))
    
    inactive_pre<-rle(burstiness_pretraining$burstiness_pre)$length[rle(burstiness_pretraining$burstiness_pre)$values==0]
    active_pre<-rle(burstiness_pretraining$burstiness_pre)$length[rle(burstiness_pretraining$burstiness_pre)$values==1]
    Burst = (sd(inactive_pre)-mean(inactive_pre))/(sd(inactive_pre)+mean(inactive_pre))
    
    if (is.na(Burst)==1){  
      Burst = 1
    }
    Burst_all<-c(Burst_all, Burst)
    
  }
  dev.off()
  graphics.off()
  
  final_fly_files_Burstiness<-data.frame(fly_files_index,Burst_all)
