setwd("/home/honestkids/Documents/SCUDEM/scudem_2025")
args<-commandArgs(trailingOnly = TRUE)

data<-read.csv(args[1], header = TRUE)
png("plot.png", width=500, height=500)
plot(data$time, data$value)
dev.off()
