setwd("/home/honestkids/Documents/SCUDEM/scudem_2025/results")

library(readr)
library(tidyverse)
library(rgl)
library(car)

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
position<-read.csv("position.txt", header=FALSE)
position<-as_tibble(t(position))
colnames(position)<-"particle1"
position<-position[-nrow(position),]
position<-position[-1, ]
position$time<-1:nrow(position)
position<-position %>%
  mutate(x = unlist(lapply(str_split(particle1, ":"), `[[`, 1)),
         y = unlist(lapply(str_split(particle1, ":"), `[[`, 2)),
         z = unlist(lapply(str_split(particle1, ":"), `[[`, 3)))
position<-position %>%
  mutate(x = as.numeric(x),
         y = as.numeric(y),
         z = as.numeric(z))

pal<-colorRampPalette(c("red", "blue"))
position$colors<-pal(length(position$time))

scatter3d(
  x=position$y, y=position$z, z= position$x,
  point.col = position$colors,
  type='s',
  radius=0.1
)

plot3d(
  x=position$x,
  y=position$z,
  z=position$y,
  col=position$colors,
  type="s",
  radius=1
)
