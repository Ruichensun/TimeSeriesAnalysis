setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA")

library('rEDM')
x = read.table("ProcessedData_Fly256_E1_M_2015Jul20.csv",header=T,sep=",",stringsAsFactors = F)
y = read.table("ProcessedData_Fly256_E1T1E1T1E1_M_2015Jul20.csv",header=T,sep=",",stringsAsFactors = F)

pdf("Embedding dimentions of Fly 256 before and after training.pdf")
lib<-c(1,3000)
pred<-c(3001,11000)

#plot embedding dimension vs prediction skill of pre-training behavior
simplex_output<-simplex(x$fly.position,lib, pred)
plot(simplex_output$E,simplex_output$rho,type = "l", xlab = "Embedding Dimension (E)", ylab= "Forecast Skill (rho)")

#plot embedding dimension vs prediction skill of post-training behavior
simplex_output<-simplex(y$fly.position,lib, pred)
plot(simplex_output$E,simplex_output$rho,type = "l", xlab = "Embedding Dimension (E)", ylab= "Forecast Skill (rho)")

dev.off()
graphics.off()