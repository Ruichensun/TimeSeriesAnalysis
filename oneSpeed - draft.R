library(signal)
library(Hmisc)
library(ggplot2)

framerate = 20
#Select CSV files
get_fly_position_smoothened <- function(input_file){
  x = read.table(input_file,header=T,sep=",",stringsAsFactors=F) 
  # x = unlist(x[seq(1,length(x),by=50)])
  ##Remove the initial XXX points and the last XXX points
  # data_start_fly_position = which((x[,1])!=0)[1]
  pos_fly_position = as.numeric(x[[1]])       
  return(pos_fly_position)
}

fly_files <- function(path_DATA){
  setwd(path_DATA) 
  final_fly_files = c()
  for (j in length(list.files)){
    file <- dir(pattern = "_.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
    final_fly_files<-append(final_fly_files,file)
  }
  return(final_fly_files)
}

fly_speed <- function(pos_fly_position_info_s){
  len = length(pos_fly_position_info_s)
  pos_fly_position = pos_fly_position_info_s[20:len]
  #pos_fly_position = pos_fly_position_info_s
  data_speed = 0
  for(i in 2:length(pos_fly_position)){
    data_speed[i-1] = abs(pos_fly_position[i]-pos_fly_position[i-1])
  }
  return(data_speed)
}

path_DATA = "//vmware-host/Shared Folders/Downloads/核心能力/科研能力/科研项目/fruitfly-06-16/sample/Trained flies"
setwd(path_DATA)
pdf_name = "Speed"

files <- fly_files(path_DATA)
data1 = get_fly_position_smoothened(files[1])
data2 = get_fly_position_smoothened(files[3])
data3 = get_fly_position_smoothened(files[5])


Step = 160
FlySpeed = fly_speed(data3)
FlySpeedAvgstd3 = fly_speed_avgstd(FlySpeed, Step)
FlySpeed = fly_speed(data1)
FlySpeedAvgstd1 = fly_speed_avgstd(FlySpeed, Step)
FlySpeed = fly_speed(data2)
FlySpeedAvgstd2 = fly_speed_avgstd(FlySpeed, Step)

CV = function(FlySpeedAvgstd){
	x = FlySpeedAvgstd[,2]/(FlySpeedAvgstd[,1]+0.0001)
	return(x)
}
x = 1:length(FlySpeedAvgstd3[,2])
plot(x,CV(FlySpeedAvgstd3), ylab = "Coefficient of Variation in Windows", xlab = paste("Windows", Step, sep = ""), col="red", type = "l")
x = 1:length(FlySpeedAvgstd1[,2])
lines(x,CV(FlySpeedAvgstd1),col="blue") 
x = 1:length(FlySpeedAvgstd2[,2])
lines(x,CV(FlySpeedAvgstd2),col="green") 


x = 1:length(FlySpeedAvgstd3[,2])
plot(x,FlySpeedAvgstd3[,2], ylab = "Standard Deviation in Windows", xlab = paste("Windows", Step, sep = ""), col="red", type = "l")
x = 1:length(FlySpeedAvgstd1[,2])
lines(x,FlySpeedAvgstd1[,2],col="blue") 
x = 1:length(FlySpeedAvgstd2[,2])
lines(x,FlySpeedAvgstd2[,2],col="green") 

x = 1:length(FlySpeedAvgstd3[,1])
plot(x,FlySpeedAvgstd3[,1], ylab = "Average Speed in Windows", xlab = paste("Windows", Step, sep = ""), col="red", type = "l")
x = 1:length(FlySpeedAvgstd1[,1])
lines(x,FlySpeedAvgstd1[,1],col="blue") 
x = 1:length(FlySpeedAvgstd2[,1])
lines(x,FlySpeedAvgstd2[,1],col="green") 