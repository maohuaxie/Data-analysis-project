#load the libraries
library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(bit64)
library(RColorBrewer)
library(choroplethr)
require(scales)
library(readr)
library(lubridate)
# load data
sprint<- fread('D:/Sprint/sprintproject/1013sprint.csv')
dim(sprint)
head(sprint)
summary(sprint)
# summary and understand each column variable, I did try to use for loop to automate the following steps
# however, it is not working, hopfully, you guys could make it work and happen.

# UE_type cloumn, mobile phone, tablet as well as Roulter.
UE_type<- sprint[,19]
ggplot(data=UE_type)+geom_bar(mapping=aes(x=ue_type,color=ue_type))
ggplot(data=UE_type)+geom_bar(mapping=aes(x=ue_type,fill=ue_type))
ggplot(data=UE_type, aes(x = ue_type,color=ue_type,fill=ue_type) )+  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

# operation system column variable, such as android, windows, iOS etc.
os<- sprint[,16]
ggplot(data=os)+geom_bar(mapping=aes(x=os,color=os))
ggplot(data=os)+geom_bar(mapping=aes(x=os,fill=os))
ggplot(data=os, aes(x = os,color=os,fill=os) )+  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

# functionality column variable, such as web browsing, weather, advertisement.
functionality<- sprint[,11]
ggplot(data=functionality)+geom_bar(mapping=aes(x=functionality,color=functionality))
ggplot(data=functionality)+geom_bar(mapping=aes(x=functionality,fill=functionality))
ggplot(data=functionality, aes(x = functionality,color=functionality,fill=functionality) )+  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

# vendor column variable
vendor<-count(sprint[,12],vendor)
vendor=filter(vendor,n>5000)
ggplot(data=vendor)+geom_bar(mapping=aes(x=vendor,y= n,fill=vendor),stat="identity")

# vendor and Tcp.Downlink.Throughput column variable
# By using tplyr pakacage or aggregate function to do this
TCP.Downlink.Throughput.Mbps.<- sprint[,c(12,27)]
aggregate(sprint[,c(12,27)], by=list(sprint$vendor), FUN=function(x) { mean(!is.na(x))})

data=TCP.Downlink.Throughput.Mbps. %>% 
  group_by(vendor) %>%
  summarise_all(funs(mean(!is.na(.))))%>% arrange(desc(TCP.Downlink.Throughput.Mbps.))
ggplot(data=data[1:20,])+geom_bar(mapping=aes(x=vendor,y= TCP.Downlink.Throughput.Mbps.,fill=vendor),stat="identity")

# Hour and Tcp.Downlink.Throughput column variable
Hour<- sprint[,c(7,27)]
Hours=Hour %>% 
  group_by(hour) %>%
  summarise_all(funs(mean(!is.na(.))))%>% arrange(desc(TCP.Downlink.Throughput.Mbps.))
ggplot(data=Hours[1:20,])+geom_bar(mapping=aes(x=hour,y= TCP.Downlink.Throughput.Mbps.,fill=hour),stat="identity")

# convert datetime to POSIXct
sprint$date<-as.POSIXct(sprint$date,
                                    format = "%Y-%m-%d %H:%M",
                                    tz = "America/New_York")
sprint%>% 
  mutate(hour_of_day = hour(as.POSIXct(sprint$date,
                                       format = "%Y-%m-%d %H:%M",
                                      tz = "America/New_York")))
# level2 and Tcp.Downlink.Throughput column variable

level2<-sprint[,c(2,27)]

level=level2 %>% 
  group_by(level_2_name) %>%
  summarise_all(funs(mean(!is.na(.))))%>% arrange(desc(TCP.Downlink.Throughput.Mbps.))
ggplot(data=level[1:20,])+geom_bar(mapping=aes(x=level_2_name,y= TCP.Downlink.Throughput.Mbps.,fill=level_2_name),stat="identity")

# functionality and Tcp.Downlink.Throughput column variable

fun2<- sprint[,c(11,27)]

fun=fun2 %>% 
  group_by(functionality) %>%
  summarise_all(funs(mean(!is.na(.))))%>% arrange(desc(TCP.Downlink.Throughput.Mbps.))
ggplot(data=fun[1:20,])+geom_bar(mapping=aes(x=functionality,y= TCP.Downlink.Throughput.Mbps.,fill=functionality),stat="identity")

