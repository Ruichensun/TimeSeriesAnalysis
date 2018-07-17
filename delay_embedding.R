setwd("C:/Users/Ruichen/Desktop/CSSS2016/Fly_Training/DATA")

x = read.table("ProcessedData_Fly248_E1R1E1R1E1_F_2015Jul20.csv",header=T,sep=",",stringsAsFactors = F)

pdf("Return Map_different_tao_Fly248_E1R1E1R1E1.pdf")

plot(x$fly.position[1:(length(x$fly.position)-1)],x$fly.position[2:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 1")

plot(x$fly.position[1:(length(x$fly.position)-10)],x$fly.position[11:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 10")

plot(x$fly.position[1:(length(x$fly.position)-20)],x$fly.position[21:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 20")

plot(x$fly.position[1:(length(x$fly.position)-30)],x$fly.position[31:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 30")

plot(x$fly.position[1:(length(x$fly.position)-40)],x$fly.position[41:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 40")

plot(x$fly.position[1:(length(x$fly.position)-50)],x$fly.position[51:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 50")

plot(x$fly.position[1:(length(x$fly.position)-60)],x$fly.position[61:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 60")

plot(x$fly.position[1:(length(x$fly.position)-70)],x$fly.position[71:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 70")

plot(x$fly.position[1:(length(x$fly.position)-80)],x$fly.position[81:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 80")

plot(x$fly.position[1:(length(x$fly.position)-90)],x$fly.position[91:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 90")

plot(x$fly.position[1:(length(x$fly.position)-100)],x$fly.position[101:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 100")

plot(x$fly.position[1:(length(x$fly.position)-110)],x$fly.position[111:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 110")

plot(x$fly.position[1:(length(x$fly.position)-120)],x$fly.position[121:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 120")

plot(x$fly.position[1:(length(x$fly.position)-130)],x$fly.position[131:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 130")

plot(x$fly.position[1:(length(x$fly.position)-140)],x$fly.position[141:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 140")

plot(x$fly.position[1:(length(x$fly.position)-150)],x$fly.position[151:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 150")

plot(x$fly.position[1:(length(x$fly.position)-160)],x$fly.position[161:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 160")

plot(x$fly.position[1:(length(x$fly.position)-170)],x$fly.position[171:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 170")

plot(x$fly.position[1:(length(x$fly.position)-180)],x$fly.position[181:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 180")

plot(x$fly.position[1:(length(x$fly.position)-190)],x$fly.position[191:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 190")

plot(x$fly.position[1:(length(x$fly.position)-200)],x$fly.position[201:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 200")

plot(x$fly.position[1:(length(x$fly.position)-300)],x$fly.position[301:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 300")

plot(x$fly.position[1:(length(x$fly.position)-500)],x$fly.position[501:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 500")

plot(x$fly.position[1:(length(x$fly.position)-1000)],x$fly.position[1001:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 1000")

plot(x$fly.position[1:(length(x$fly.position)-2000)],x$fly.position[2001:length(x$fly.position)])
title(main="Position_Fly 248_20Hz_tao = 2000")

dev.off()
graphics.off()
