setwd("/home/honestkids/Documents/SCUDEM/scudem_2025/results")

library(readr)
library(tidyverse)
library(rgl)
library(car)
library(magick)
library(plotly)

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

extract_coord<-function(df){
  df<-df %>%
    mutate(case)
    mutate(x = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 1))),
           y = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 2))),
           z = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 3))))
  return (df)
}


for(time in 1:nrow(position)){
  temp<-position[time,]
  temp<-as.data.frame(t(temp)) %>% remove_rownames()
  temp<-temp %>% filter(nchar(V1) > 0)
  temp<-temp[-nrow(temp), ,drop=FALSE]
  temp<-extract_coord(temp)

  temp$colors<-pal(length(temp$x))
  print(temp)

  # use plotly + ggplot to create 3d graph
  # plot3d(
  #   x=temp$x,
  #   y=temp$z,
  #   z=temp$y,
  #   col=temp$colors,
  #   type="s",
  #   radius=0.5
  # )
  # movie3d(spin3d(axis=c(0,0,1), rpm=5), duration=10,
  #         dir=getwd(), movie="demo.gif")
}

#using apply()
#https://plotly.com/r/animations/
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html

pal<-colorRampPalette(c("red", "blue"))

extract_coord<-function(row){
  row<-t(row)
  row<-row[-nrow(row),,drop=FALSE] %>% as_tibble()
  row<-row %>%
    mutate(x = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 1))),
           y = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 2))),
           z = as.numeric(unlist(lapply(str_split(V1, ":"), `[[`, 3))))
  row$colors<-pal(length(row$x))
  row$num<-1:nrow(row)
  return (row)
}

plot_ly(row, x=~x, y=~y, z=~z, marker = list(color=~time, colorscale="Viridis")) %>%
  add_markers()
