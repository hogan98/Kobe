library(grid)
library(png)
library(rjson)
library(RCurl)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(knitr)


kobe=read.csv('/Users/chenrui/desktop/sta3011/data/kobe.csv',header=T)
kobe=kobe[!is.na(kobe$shot_made_flag),]
kobe$time_remain=10*kobe$minutes_remaining+kobe$seconds_remaining
kobe$home=as.factor(as.numeric(grepl('vs',kobe$matchup)))
kobe$last=as.factor(as.numeric(kobe$seconds_remaining<=5))
train=kobe
train$shot_made_flag=as.factor(train$shot_made_flag)
library(caret)
library(alr3)



pplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes_q(x = feat)) +
    geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    ggtitle(paste("accuracy by", feat))
  
}

pplot(period)
pplot(minutes_remaining)
pplot(seconds_remaining)
pplot(playoffs)
pplot(last)
pplot(home)


#### check performance in different season ###
train %>%
  group_by(season) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=season, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by season", x="Season") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))

#### check performance in action type  ###
train %>%
  group_by(action_type) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=action_type, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by action_type", x="action_type") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))

#### check performance in combined shot type  ###
train %>%
  group_by(combined_shot_type) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=combined_shot_type, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by combined_shot_type", x="combined_shot_type") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))

#### check performance with different opponent ###
train %>%
  group_by(opponent) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=opponent, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by opponent", x="opponent") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))

#### check performance with time  ###
train %>%
  group_by(time_remain) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=time_remain, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by time_remain", x="time_remain") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))


###opponent and shot_type
train %>%
  group_by(opponent) %>%
  summarise(TwoPoint=mean(as.numeric(shot_made_flag[shot_type=="2PT Field Goal"])-1),
            ThreePoint=mean(as.numeric(shot_made_flag[shot_type=="3PT Field Goal"])-1)) %>%
  ggplot(aes(x=opponent, group=1)) +
  geom_line(aes(y=TwoPoint, colour="TwoPoint")) +
  geom_line(aes(y=ThreePoint, colour="ThreePoint")) +
  geom_point(aes(y=TwoPoint, colour="TwoPoint"), size=3) +
  geom_point(aes(y=ThreePoint, colour="ThreePoint"), size=3) +
  labs(title="Accuracy by opponent", 
       subtitle="2PT Field Goal and 3PT Field Goal",
       x="Opponent", y="Accuracy") +
  theme_bw() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1)) 

### check if home or away effects###
mean(as.numeric(train$shot_made_flag[train$home==1])-1)
mean(as.numeric(train$shot_made_flag[train$home==0])-1)


courtImg.URL <- "https://i.pinimg.com/originals/4c/8e/4f/4c8e4f1dbd6d6a7580285b7a28b6c07b.png"
court <- rasterGrob(readPNG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))


ggplot(train, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -300, 300, -100, 850) +
  geom_point(aes(colour = shot_zone_basic, shape = shot_made_flag)) +
  xlim(-250, 250) +
  ylim(-50, 800)
train %>%
  group_by(shot_zone_basic) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=shot_zone_basic, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by shot_zone_basic", x="shot_zone_basic") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))

ggplot(train, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -300, 300, -100, 850) +
  geom_point(aes(colour = shot_zone_area, shape = shot_made_flag)) +
  xlim(-250, 250) +
  ylim(-50, 800)

train %>%
  group_by(shot_zone_area) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=shot_zone_area, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by shot_zone_area", x="shot_zone_area") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))

ggplot(train, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -300, 300, -100, 850) +
  geom_point(aes(colour = shot_zone_range, shape = shot_made_flag)) +
  xlim(-250, 250) +
  ylim(-50, 800)

train %>%
  group_by(shot_zone_range) %>%
  summarise(Accuracy=mean(as.numeric(shot_made_flag)-1)) %>%
  ggplot(aes(x=shot_zone_range, y=Accuracy, group=1)) +
  geom_line(aes(colour=Accuracy)) +
  geom_point(aes(colour=Accuracy), size=3) +
  scale_colour_gradient(low="orangered", high="chartreuse3") +
  labs(title="Accuracy by shot_zone_range", x="shot_zone_range") +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=45, hjust=1))



###check correlation
num=c(3,4,8,9,16)
vec=(1:ncol(train))[-num]
corr=matrix(rep(0,(ncol(train)-length(num))^2),ncol=(ncol(train)-length(num)))
for(i in 1:length(vec)){
  for(j in 1:length(vec)){
    if(i!=j){
      te=chisq.test(table(train[,vec[i]],train[,vec[j]]))
      corr[i,j]=te$p.value
    }
  }
}



### check time remain and distance
pplot(time_remain)
ggplot(train, aes(x = shot_made_flag, y = time_remain)) +
  geom_boxplot() + 
  coord_flip()

pplot(shot_distance)
ggplot(train, aes(x = shot_made_flag, y = shot_distance)) +
  geom_boxplot() + 
  coord_flip()

ggplot(train, aes(x = shot_made_flag, y = loc_x)) +
  geom_boxplot() + 
  coord_flip()
ggplot(train, aes(x = shot_made_flag, y = loc_y)) +
  geom_boxplot() + 
  coord_flip()

ggplot(train, aes(x = shot_made_flag, y = minutes_remaining)) +
  geom_boxplot() + 
  coord_flip()

ggplot(train, aes(x = shot_made_flag, y = seconds_remaining)) +
  geom_boxplot() + 
  coord_flip()

