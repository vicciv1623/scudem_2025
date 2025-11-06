setwd("/home/honestkids/Documents/SCUDEM/scudem_2025/results")

library(readr)
library(tidyverse)
library(rgl)
library(car)
library(magick)
library(plotly)
library(RColorBrewer)
library(plot3D)

#reading velocity
velocity<-read.csv("velocity.txt", header=FALSE)
velocity<-as_tibble(t(velocity))
colnames(velocity)<-"particle1"
velocity<-velocity[-nrow(velocity),]
velocity$time<-1:nrow(velocity)

plot(velocity$time, velocity$particle1)

#reading mass
mass<-read.csv("mass.txt", header=FALSE)
mass<-as_tibble(t(mass))
colnames(mass)<-"particle1"
mass<-mass[-nrow(mass),]
mass$time<-1:nrow(mass)
mass<-mass[-1,]

plot(mass$time, mass$particle1)

#reading position
position<-read.csv("../dummy_pos.txt", header=FALSE)
position<-as_tibble(t(position))
colnames(position)<-sapply(colnames(position), function(x) paste0("particle", substr(x, 2, nchar(x))))
position<-position[-nrow(position),]
position<-position[-1, ]
position$time<-1:nrow(position)

#using apply()
#https://plotly.com/r/animations/
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html

colorsc<-brewer.pal(9, "BuPu")
extract_coord<-function(row){
  image_name<-paste0("images/all_3d_",row$time,".png")
  row<-t(row)
  row<-row[-nrow(row),,drop=FALSE] %>% as.data.frame()
  print(row)
  row<-row %>%
    filter(nchar(V1) > 0) %>%
    mutate(x = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 1))),
           y = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 2))),
           z = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 3))))
  row$colors<-pal(length(row$x))

  png(image_name, width=800, height=800)
  scatter3D(row$x, row$y, row$z, type="s", col=colorRampPalette(colorsc)(100),
            xlim=c(-100,100), ylim=c(-100,100), zlim=c(0,100),
            bty="b2", alpha=1, pch=19, ticktype="detailed",
            colkey=FALSE)
  dev.off()
}

#apply(position, 1, extract_coord)
for(i in 1:nrow(position)){
  extract_coord(position[i,])
}
