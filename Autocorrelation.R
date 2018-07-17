setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training")


fly_autocorrelation<-function(fly_number){
  
  acf_pre<-c()
  acf_mid<-c()
  acf_post<-c()
  
  fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
  fly_files = fly_files[grep("E1_",fly_files)] # Choose only E1 files, excluding all training files
  x = read.table(fly_files[1],header=T,sep=",",stringsAsFactors = F)
  y = read.table(fly_files[2],header=T,sep=",",stringsAsFactors = F)
  z = read.table(fly_files[3],header=T,sep=",",stringsAsFactors = F)
  
pdf("Return Map_Fly456.pdf")
par(mfrow=c(2,2))
plot(x$fly.position[1:(length(x$fly.position)-1)],x$fly.position[2:length(x$fly.position)])
title(main="Position_Fly 456_20Hz_tao = 1")

transient_speed = x$fly.position[2:length(x$fly.position)]-x$fly.position[1:length(x$fly.position)]
plot(transient_speed[1:(length(transient_speed)-1)],transient_speed[2:length(transient_speed)])
title(main="Tranisent Velocity_Fly 456_20Hz_tao=1")

plot(transient_speed[1:(length(transient_speed)-20)],transient_speed[21:length(transient_speed)])
title(main="Transient Velocity_Fly 456_20Hz_tao=20")

transient_speed_20 = transient_speed[seq(1,length(transient_speed),20)]
plot(transient_speed_20[1:(length(transient_speed_20)-1)],transient_speed_20[2:length(transient_speed_20)])
title(main="Transient Velocity_Fly 456_1Hz_tao=1")
dev.off()
graphics.off()


pdf("Histogram_velocity_Fly456.pdf")
par(mfrow=c(2,2))
hist(transient_speed, breaks=500, xlim=c(-50,50), main=NULL)
title(main="Transient velocity_Fly456_20Hz")
hist(transient_speed_20, breaks=500, xlim=c(-50,50),main=NULL)
title(main="Transient velocity_Fly456_1Hz")
hist(abs(transient_speed), breaks=500, xlim=c(0,50), main=NULL)
title(main="Transient velocity_Fly456_20Hz_abs")
hist(abs(transient_speed_20), breaks=500, xlim=c(0,50), main=NULL)
title(main="Transient velocity_Fly456_1Hz_abs")

dev.off()
graphics.off()
