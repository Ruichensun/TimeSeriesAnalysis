setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA")
library(zoo)
fly_number = 256

fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
fly_files = fly_files[grep("E1_",fly_files)] # Choose only E1 files, excluding all training files
x = read.table(fly_files[1],header=T,sep=",",stringsAsFactors = F)
y = read.table(fly_files[2],header=T,sep=",",stringsAsFactors = F)
z = read.table(fly_files[3],header=T,sep=",",stringsAsFactors = F)

par(mfrow=c(3,1))

speed<-abs(diff(x$fly.position))
ma<-rollmean(speed,k=160)
plot(ma,ylim=c(0,9), type='l')
speed<-abs(diff(y$fly.position))
ma<-rollmean(speed,k=160)
plot(ma,ylim=c(0,9), type='l')
speed<-abs(diff(z$fly.position))
ma<-rollmean(speed,k=160)
plot(ma,ylim=c(0,9), type='l')
