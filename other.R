library(ggplot2)
library(corrplot)
library(caret)
require(grid)
library(GGally)
library(Rmisc)


train=read.table('/Users/chenrui/desktop/sta3011/data/train.txt',header=T)
test=read.table('/Users/chenrui/desktop/sta3011/data/test.txt',header=T)
pre.data=read.table('/Users/chenrui/desktop/sta3011/data/predict.txt',header=T)

kobe$shot_made_flag <- as.factor(kobe$shot_made_flag)

# shot_distance
shot_dist_plot <- qplot(shot_made_flag, shot_distance, data=kobe, geom=c("boxplot"), 
                        fill=shot_made_flag, main="Score by Shot Distance", ylab="Shot Distance")

# Loc_x and Loc_y
loc_x_plot <- qplot(shot_made_flag, loc_x, data=kobe, geom=c("boxplot"), 
                    fill=shot_made_flag, main="loc_x", ylab="loc_x")

loc_y_plot <- qplot(shot_made_flag, loc_y, data=kobe, geom=c("boxplot"), 
                    fill=shot_made_flag, main="loc_y", ylab="loc_y")

# Lat and Lon
lat_plot <- qplot(shot_made_flag, lat, data=kobe, geom=c("boxplot"), 
                  fill=shot_made_flag, main="Latitude", ylab="Latitude")

lon_plot <- qplot(shot_made_flag, lon, data=kobe, geom=c("boxplot"), 
                  fill=shot_made_flag, main="Longitude", ylab="Longitude")

options(repr.plot.width=8, repr.plot.height=8)

multiplot(loc_x_plot, loc_y_plot, lat_plot, lon_plot, shot_dist_plot, cols=2)

