setwd("/home/honestkids/Documents/SCUDEM/scudem_2025/results")

library(readr)
library(tidyverse)
library(rgl)
library(car)
library(magick)
#library(plotly)
library(RColorBrewer)
library(plot3D)
library(purrr)

#reading velocity
velocity<-read.csv("velocity.txt", header=FALSE)
velocity<-as_tibble(t(velocity))
colnames(velocity)<-sapply(colnames(velocity), function(x) paste0("particle", substr(x, 2, nchar(x))))
#velocity<-velocity[-nrow(velocity),]
velocity$time<-1:nrow(velocity)
velocity<-velocity[,-20]
velocity<-velocity[-1,]

velocity_long<-velocity %>%
  pivot_longer(cols=starts_with("particle"), names_to="particle", values_to="v")
png("velocity.png", width=800, height=800)
ggplot(velocity_long, aes(x=time, y=v, color=particle)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,max(velocity_long$v, na.rm=TRUE)+2)) +
  theme(legend.position="none",
        aspect.ratio=1,
        panel.background = element_rect(fill="aliceblue"),
        panel.grid.major.x = element_line(color="cadetblue",
                                          linetype = "dashed"),
        panel.grid.major.y = NULL,
        panel.grid.minor.y = NULL,
        axis.line.x.bottom = element_line(color="black"),
        axis.line.x.top = NULL,
        axis.line.y.left = element_line(color="black"),
        axis.line.y.right = NULL,
        axis.ticks.y.left = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        plot.title = element_text(size=20)
        ) +
  labs(title="Change in velocity",
       x="Time",
       y="Velocity")
dev.off()

#reading mass
mass<-read.csv("mass.txt", header=FALSE)
mass<-as_tibble(t(mass))
colnames(mass)<-sapply(colnames(mass), function(x) paste0("particle", substr(x, 2, nchar(x))))
mass<-mass[-nrow(mass),]
mass$time<-1:nrow(mass)
mass<-mass[-1,]
mass<-mass[,-c(19,20,28)]
mass<-mass %>% select(-"particle57")
mass<-mass %>% select(-"particle72")
mass<-mass %>% select(-"particle64")

mass_long<-mass %>%
  pivot_longer(cols=starts_with("particle"), names_to="particle", values_to="m")
png("mass.png", width=800, height=800)
ggplot(mass_long, aes(x=time, y=m, color=particle)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.position="none",
        aspect.ratio=1,
        panel.background = element_rect(fill="aliceblue"),
        panel.grid.major.x = element_line(color="cadetblue",
                                          linetype = "dashed"),
        panel.grid.major.y = NULL,
        panel.grid.minor.y = NULL,
        axis.line.x.bottom = element_line(color="black"),
        axis.line.x.top = NULL,
        axis.line.y.left = element_line(color="black"),
        axis.line.y.right = NULL,
        axis.ticks.y.left = element_blank(),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        plot.title = element_text(size=20)
  ) +
  labs(title="Change in mass",
       x="Time",
       y="Mass")
dev.off()

#reading position
position<-read.csv("position.txt", header=FALSE)
position<-as_tibble(t(position))
colnames(position)<-sapply(colnames(position), function(x) paste0("particle", substr(x, 2, nchar(x))))
position$time<-1:nrow(position)
position<-position[-nrow(position),]
position<-position[-1, ]

position_long<-position %>%
  pivot_longer(cols=starts_with("particle"), names_to="particle", values_to="p")
position_long<-position_long %>%
  mutate(x = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 1], "")),
         y = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 2], "")),
         z = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 3], "")))
maxX<-max(position_long$x, na.rm=TRUE)
maxY<-max(position_long$y, na.rm=TRUE)
minX<-min(position_long$x, na.rm=TRUE)
minY<-min(position_long$y, na.rm=TRUE)

#using apply()
#https://plotly.com/r/animations/
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html

colorsc<-brewer.pal(9, "BuPu")
extract_coord<-function(row){
  image_name<-paste0("images/all_3d_",row$time,".png")
  row<-t(row)
  row<-row[-nrow(row),,drop=FALSE] %>% as.data.frame()
  row<-row %>%
    filter(nchar(V1) > 0) %>%
    mutate(x = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 1], "")),
           y = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 2], "")),
           z = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 3], "")))
  row$z<-700-row$z

  png(image_name, width=800, height=800)
  scatter3D(row$x, row$y, row$z, type="s", col=colorRampPalette(colorsc)(100),
            xlim=c(minX,maxX), ylim=c(minY,maxY), zlim=c(0,750),
            bty="b2", alpha=1, pch=19, ticktype="detailed",
            colkey=FALSE)
  dev.off()
}

#apply(position, 1, extract_coord)
for(i in 1:nrow(position)){
  print(i)
  if(i %in% c(19,20,28,57,72,64)) next
  extract_coord(position[i,])
}

# running this will abort R, make gif in website
# imgs<-list.files("images", full.names=TRUE)
# img_list<-lapply(imgs, image_read)
# img_joined<-image_join(img_list)
# img_animated<-image_animate(img_joined, fps=10)
# image_write(image=img_animated,
#             path="all_3d.gif")

# plotting 2d velocity vs mass
combined<-left_join(mass_long, velocity_long, by=c("particle","time"))
png("Velocity_vs_Mass.png", height=800, width=800)
ggplot(combined, aes(x=m, y=v, color=particle)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,max(velocity_long$v, na.rm=TRUE) +1)) +
  theme(legend.position="none",
        aspect.ratio=1,
        panel.background = element_rect(fill="ivory"),
        panel.grid.major.x = element_line(color="mediumpurple",
                                          linetype = "dashed"),
        panel.grid.major.y = NULL,
        panel.grid.minor.y = NULL,
        axis.line.x.bottom = element_line(color="black"),
        axis.line.x.top = NULL,
        axis.line.y.left = element_line(color="black"),
        axis.line.y.right = NULL,
        axis.ticks.y.left = element_blank(),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        plot.title = element_text(size=20)
  ) +
  labs(x="Mass",
       y="Velocity",
       title="Velocity vs Mass")
dev.off()

#plotting 2d bubble plot final locations
combined<-left_join(combined, position_long, by=c("particle","time"))
final_logistics<-combined %>% filter(time==max(combined$time))
final_logistics<-final_logistics %>%
  mutate(x = case_when(nchar()),
         y = as.numeric(unlist(lapply(str_split(p, ":"), `[[`, 2))),
         z = as.numeric(unlist(lapply(str_split(p, ":"), `[[`, 3))))

png("final_logistics.png", width=800, height=800)
ggplot(final_logistics, aes(x=x, y=y, size=v, fill=m)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range=c(0.1, 10), name="Velocity (km/s)") +
  scale_fill_gradient(high="blue", low="purple", name="Mass (kg)") +
  theme(aspect.ratio = 1,
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color="gray"),
        panel.grid.minor = element_line(color="gray"),
        axis.ticks = element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        plot.title = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)
        ) +
  labs(x="x position",
       y="y position",
       title="Final Locations of Particles")
dev.off()

#plotting 2d velocity vs altitude & mass v altitude
combined<-combined %>%
  mutate(z = as.numeric(na_if(str_split_fixed(p, ":", 3)[, 3], "")))
png("Mss_vs_dist_trav.png", height=800, width=800)
ggplot(combined, aes(x=z, y=m, color=particle)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.position="none",
        aspect.ratio=1,
        panel.background = element_rect(fill="ivory"),
        panel.grid.major.x = element_line(color="mediumpurple",
                                          linetype = "dashed"),
        panel.grid.major.y = NULL,
        panel.grid.minor.y = NULL,
        axis.line.x.bottom = element_line(color="black"),
        axis.line.x.top = NULL,
        axis.line.y.left = element_line(color="black"),
        axis.line.y.right = NULL,
        axis.ticks.y.left = element_blank(),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        plot.title = element_text(size=20)
  ) +
  labs(x="Distance traveled vertically",
       y="Mass",
       title="Mass vs Distance traveled")
dev.off()

#reading other#reading otherparticle
other<-read.csv("other.txt", header=FALSE)
other<-as_tibble(t(other))
colnames(other)<-sapply(colnames(other), function(x) paste0("particle", substr(x, 2, nchar(x))))
