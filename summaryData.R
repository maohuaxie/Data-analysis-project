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

# load data
sprint<- fread('D:/Sprint/sprintproject/1013sprint.csv')
dim(sprint)
head(sprint)

summary(sprint)

rm(list=ls())
df2<-read.csv("D:/Sprint/1013sprint.csv", row.names = 1, header = TRUE, as.is = TRUE)
table<-0
for(i in 1:length(df2[1,])){
  table[i]<-sum(is.na(df2[,i]))
}
table

table2<-(table/length(df2[,1]))*100
table3<-0
for(i in 1:length(df2[1,])){
  table3[i]<-mean(df2[,i])
}
table3


write.csv(cbind(table,table2), file='D:/Sprint/Summary.csv')
summary(df2)
write.csv(summary(df2),file='D:/Sprint/Summary2.csv')

library(readxl)
ctable<- read_excel("D:/Sprint/summaryR.xlsx")
View(ctable)
ctable<-ctable[c(-2)]
ctable<-t(ctable)
ctable2<-read.csv('D:/Sprint/Summary.csv')
ctable<-cbind(ctable,ctable2[,2:3])
ctable<-ctable[c(-2,-3,-5,-7)]
colnames(ctable)<-c("min","mean","max","number for NA","percentage of NA")
write.csv(ctable,file='D:/Sprint/SummaryR2.csv')
ctable3<-read.csv('D:/Sprint/SummaryR2.csv')
table4<-0
for(i in 1:length(df2[1,])){
  table4[i]<-length(df2[,i])
}
table4
table5<-0
for(i in 1:length(df2[1,])){
  table5[i]<-mode(df2[,i])
}
table5
ctable3<-cbind(ctable3,table4[2:48],table5[2:48])
colnames(ctable3)<-c("header","min","mean","max","number for NA","percentage of NA","numbers of record","type")
write.csv(ctable3,file='D:/Sprint/SummaryR3.csv')
