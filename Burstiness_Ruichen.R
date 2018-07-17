setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA")
library(ggplot2)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


fly_burstiness<-function(fly_number){
  
  Burst_pre<-c()
  Burst_mid<-c()
  Burst_post<-c()
  Burst<-c()
  
  fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
  fly_files = fly_files[grep("E1_",fly_files)] # Choose only E1 files, excluding all training files
  x = read.table(fly_files[1],header=T,sep=",",stringsAsFactors = F)
  y = read.table(fly_files[2],header=T,sep=",",stringsAsFactors = F)
  z = read.table(fly_files[3],header=T,sep=",",stringsAsFactors = F)

  speed_pre<-x$fly.position[2:length(x$fly.position)]-x$fly.position[1:(length(x$fly.position)-1)]
  burstiness_pre<-abs(speed_pre)
  burstiness_pre<-replace(burstiness_pre,burstiness_pre>0,1)
  # burstiness_pre<-replace(burstiness_pre,burstiness_pre[which(x$fly.position>=757)]==0,1)
  # burstiness_pre<-replace(burstiness_pre,burstiness_pre[which(x$fly.position<=10)]==0,1)

  speed_mid<-y$fly.position[2:length(y$fly.position)]-y$fly.position[1:(length(y$fly.position)-1)]
  burstiness_mid<-abs(speed_mid)
  burstiness_mid<-replace(burstiness_mid,burstiness_mid>0,1)
  # burstiness_mid<-replace(burstiness_mid,burstiness_mid[which(y$fly.position>=757)]==0,1)
  # burstiness_mid<-replace(burstiness_mid,burstiness_mid[which(y$fly.position<=10)]==0,1)

  speed_post<-z$fly.position[2:length(z$fly.position)]-z$fly.position[1:(length(z$fly.position)-1)]
  burstiness_post<-abs(speed_post)
  burstiness_post<-replace(burstiness_post,burstiness_post>0,1)
  # burstiness_post<-replace(burstiness_post,burstiness_post[which(y$fly.position>=757)]==0,1)
  # burstiness_post<-replace(burstiness_post,burstiness_post[which(y$fly.position<=10)]==0,1)

  pdf(paste0("Burstiness of Fly ",fly_number,".pdf"))

  burstiness_pretraining<-data.frame(burstiness_pre)
  p1<-ggplot(burstiness_pretraining, aes(x=(1:length(burstiness_pre))/20, y=burstiness_pre))
  p1<-p1+geom_bar(stat="identity")+ggtitle(paste0("Fly ",fly_number, "Before Training"))+xlab("Time(sec)")+ylab("Activity")+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

  burstiness_midtraining<-data.frame(burstiness_mid)

  p2<-ggplot(burstiness_midtraining, aes(x=(1:length(burstiness_mid))/20, y=burstiness_mid))
  p2<-p2+geom_bar(stat="identity")+ggtitle(paste0("Fly ",fly_number, "After 1st Training"))+xlab("Time(sec)")+ylab("Activity")+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

  burstiness_posttraining<-data.frame(burstiness_post)
  p3<-ggplot(burstiness_posttraining, aes(x=(1:length(burstiness_post))/20,y=burstiness_post))
  p3<-p3+geom_bar(stat="identity")+ggtitle(paste0("Fly ",fly_number, "After 2nd Training"))+xlab("Time(sec)")+ylab("Activity")+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

  multiplot(p1,p2,p3,cols=1)

  dev.off()
  graphics.off()

  inactive_pre<-rle(burstiness_pretraining$burstiness_pre)$length[rle(burstiness_pretraining$burstiness_pre)$values==0]
  active_pre<-rle(burstiness_pretraining$burstiness_pre)$length[rle(burstiness_pretraining$burstiness_pre)$values==1]
  Burst_pre = (sd(inactive_pre)-mean(inactive_pre))/(sd(inactive_pre)+mean(inactive_pre))
  # Burst_pre_cum = c(Burst_pre_cum, Burst_pre)

  inactive_mid<-rle(burstiness_midtraining$burstiness_mid)$length[rle(burstiness_midtraining$burstiness_mid)$values==0]
  active_mid<-rle(burstiness_midtraining$burstiness_mid)$length[rle(burstiness_midtraining$burstiness_mid)$values==1]
  Burst_mid = (sd(inactive_mid)-mean(inactive_mid))/(sd(inactive_mid)+mean(inactive_mid))
  # Burst_mid_cum = c(Burst_mid_cum, Burst_mid)

  inactive_post<-rle(burstiness_posttraining$burstiness_post)$length[rle(burstiness_posttraining$burstiness_post)$values==0]
  active_post<-rle(burstiness_posttraining$burstiness_post)$length[rle(burstiness_posttraining$burstiness_post)$values==1]
  Burst_post = (sd(inactive_post)-mean(inactive_post))/(sd(inactive_post)+mean(inactive_post))
  # Burst_post_cum = c(Burst_post_cum, Burst_post)

  Burst<-c(Burst_pre,Burst_mid,Burst_post)
  # Burst_cum<-c(Burst_pre_cum,Burst_mid_cum,Burst_post)
  # Burst_trial<-c()
  # Burst_trial<-list(Burst_trial,Burst)


  for (i in 1:length(Burst)){
    if (is.na(Burst[i])==1){  
      Burst[i] = 1
    }
  }
  return(Burst)
}




# length_burstiness_pre<-1:length(burstiness_pre)
# length_burstiness_post<-1:length(burstiness_post)
# 
# burstiness_pre_2<-rep(burstiness_pre,each=2)
# burstiness_post_2<-rep(burstiness_post,each=2)
# 
# burstiness_pre_2<-burstiness_pre_2[-length(burstiness_pre_2)]
# burstiness_post_2<-burstiness_post_2[-length(burstiness_post_2)]
# 
# length_burstiness_pre_2<-rep(length_burstiness_pre,each=2)[-1]
# length_burstiness_post_2<-rep(length_burstiness_post,each=2)[-1]
# 
# length_burstiness_pre_3<-c(min(length_burstiness_pre_2),length_burstiness_pre_2,max(length_burstiness_pre_2))
# length_burstiness_post_3<-c(min(length_burstiness_post_2),length_burstiness_post_2,max(length_burstiness_post_2))
# 
# burstiness_pre_3<-c(0,burstiness_pre_2,0)
# burstiness_post_3<-c(0,burstiness_post_2,0)
# 
# par(mfrow=c(2,1))
# plot(burstiness_pre[1:1000],type='s')
# polygon(length_burstiness_pre_3[1:1000],burstiness_pre_3[1:1000],col='blue')
# plot(burstiness_post[1:1000],type='s')
# polygon(length_burstiness_post_3[1:1000],burstiness_post_3[1:1000],col='blue')