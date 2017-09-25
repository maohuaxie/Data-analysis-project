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
