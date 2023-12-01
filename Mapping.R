# Number of samples plotted on a map by country of origin
# input data includes coordinated (latitude and longitude) and corresponding number of samples

library(ggplot2)
library(magrittr)
library(mapproj)
library(tidyverse)
library(maps)
library(ggrepel)
library(viridis)

# MAPPING ALL SAMPLES
# import data
World <- map_data("world")
country<-c(allmammals$Country)
lat<-c(allmammals$Lat)
long<-c(allmammals$Long)
num<-c(allmammals$Number)

# create dataframe
data<-data_frame(country, lat, long, num)

# plotting
ggplot() +
  geom_polygon(data = World, aes(x=long, y = lat, group = group), fill="grey", alpha=.8) +
  geom_point( data=data, aes(x=long, y=lat, size=num, color=num),  alpha=.75) +
  scale_size_continuous(name="Number of Samples", range=c(1, 10))  +
  scale_color_viridis(trans="log") +
  geom_label_repel(data = data, fill="#F8F8F8",  min.segment.length=0, 
                   segment.size=0.15, max.overlaps = Inf, label.r=unit(0.5, "mm"), 
                   label.padding = unit(.8,"mm"), 
                   label=paste0(data$country, ": ", str_trim( as.character(format(data$num, big.mark=",")))), 
                   aes(x=long, y=lat, size=0.05), fontface='bold', size=1) +
  theme_minimal() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust=0.5)) + 
  theme(plot.title = element_text(lineheight=1, face="bold"))

# save
ggsave("finalmap3.jpg", plot=last_plot())
ggsave("updatedfinalmap.jpg", plot=last_plot(), height=720, width=1280, units="px")



# MAPPING JUST RUMEN
# import data
World <- map_data("world")
rumenmap<-read.csv("~/Desktop/R/rumenmap.csv")
country<-c(rumenmap$Country)
lat<-c(rumenmap$Lat)
long<-c(rumenmap$Long)
num<-c(rumenmap$Number)

# create dataframe 
data<-data_frame(country, lat, long, num)

# plotting
ggplot() +
  geom_polygon(data = World, aes(x=long, y = lat, group = group), fill="grey", alpha=.8) +
  geom_point( data=data, aes(x=long, y=lat, size=num, color=num),  alpha=.75) +
  scale_size_continuous(name="Number of Samples", range=c(1, 10))  +
  scale_color_viridis(trans="log") +
  geom_label_repel(data = data, fill="#F8F8F8",  min.segment.length=0, segment.size=0.15, max.overlaps = Inf, label.r=unit(0.5, "mm"), label.padding = unit(.8,"mm"), 
                   label=paste0(data$country, ": ", str_trim( as.character(format(data$num, big.mark=",")))), aes(x=long, y=lat, size=0.05), fontface='bold', size=1) +
  theme_minimal() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust=0.5)) + 
  theme(plot.title = element_text(lineheight=1, face="bold"))

# save
ggsave("REAL_FINAL_RUMEN_MAP.jpg", plot=last_plot())
ggsave("finalrumenmap.jpg", plot=last_plot(), height=720, width=1280, units="px")


# MAPPING JUST FECES 
# import data
World <- map_data("world")
country<-c(fecesmap$Country)
lat<-c(fecesmap$Lat)
long<-c(fecesmap$Long)
num<-c(fecesmap$Number)

# create data frame
data<-data_frame(country, lat, long, num)

# plotting 
ggplot() +
  geom_polygon(data = World, aes(x=long, y = lat, group = group), fill="grey", alpha=.8) +
  geom_point( data=data, aes(x=long, y=lat, size=num, color=num),  alpha=.75) +
  scale_size_continuous(name="Number of Samples", range=c(1, 10))  +
  scale_color_viridis(trans="log") +
  geom_label_repel(data = data, fill="#F8F8F8",  min.segment.length=0, segment.size=0.15, max.overlaps = Inf, label.r=unit(0.5, "mm"), label.padding = unit(.8,"mm"), 
                   label=paste0(data$country, ": ", str_trim( as.character(format(data$num, big.mark=",")))), aes(x=long, y=lat, size=0.05), fontface='bold', size=1) +
  theme_minimal() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.title = element_text(hjust=0.5)) + 
  theme(plot.title = element_text(lineheight=1, face="bold"))

# save
ggsave("finalmap3.jpg", plot=last_plot())
ggsave("finalfecesonly.jpg", plot=last_plot(), height=720, width=1280, units="px")








