##Import the function for obtaining statistics of each experiment

#source("get_fly_speed_and_position.R")
#source("normalization_against_E1.R")
library(signal) # required by the file get_fly_speed_and_position.R
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
#plot smoothened traces
plot_fly_smoothened_test_traces <- function(path_DATA, pdf_name){
  files <- fly_files(path_DATA)
  pdf(pdf_name)
  for (i in 1:length(files)){  
      pos_fly_position_info_s = get_fly_position_smoothened(files[i])
      t = (1:length(pos_fly_position_info_s))/framerate
      output_title_0 = gsub("ProcessedData_","",basename(files[i])) 
      output_title_1 = gsub(".csv","",output_title_0)
      plot(t,pos_fly_position_info_s,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")        
      lines(t,pos_fly_position_info_s,col="blue")
    }
  
  dev.off()
  graphics.off()
}

#####Choose one part to run#########
######

setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA")
path_DATA = "C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA"
pdf_name = "ProcessedData_plot.pdf"
plot_fly_smoothened_test_traces(path_DATA, pdf_name)

