#2021-2-26 Microbiology data analysis#

#fungiotu.xls# 
#1.phylum level diversity index#
#R语言实战v2#
#Zhaopeng statistic class
install.packages("swirl")
library(swirl)
swirl()
dtf <- data.frame(diet1 = c(90,95,100),
                  diet2 = c(120,125,130),
                  diet3 = c(125,130,135))
var.test(dtf$diet1,dtf$diet2, ratio = 1, alternative = "two.sided", conf.level = 0.95)
var.test(dtf$diet1,dtf$diet3)
var.test(dtf$diet2,dtf$diet3)


#normal distribution 2021.4.14-4.16
.libPaths("D:/R-3.6.3/library")
library(lattice)
library(MASS)
library(grid)
library(ggplot2)
library(plyr)
library(gridExtra)
library(cowplot)
library(grid)
moundcover<-read.csv("e:/r_data_dit/2021/normaldis.csv")
moundcover$X.totalmoundcover.10m2.
histogram(moundcover$X.totalmoundcover.10m2.)
nortest1<-shapiro.test(moundcover$X.totalmoundcover.10m2.)
curve(dnorm(x,mean(moundcover$X.totalmoundcover.10m2.),sd(moundcover$X.totalmoundcover.10m2.)),xlim = c(0,6.5),col="yellow",lwd = 3)

#data from 2017 using ggplot2
p2017 <- ggplot(data = moundcover, mapping = aes(X.totalmoundcover.10m2.))+
  geom_histogram(aes(y=..density..), breaks = seq(0,6.9326,by = 0.5777),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(moundcover$X.totalmoundcover.10m2.), sd = sd(moundcover$X.totalmoundcover.10m2.)))

p2017

ggplot(moundcover,aes(moundcover$X.totalmoundcover.10m2.))+geom_histogram(bins=30,colour="black",fill="white")


#data from 2017-2019
#2017-2020soildisturbanceMasterdatahenan2021-4-15
#make histogram and normal distribution for "zokor" and "no zokor" #treatment in each year
mound20172019<-read.csv("e:/r_data_dit/2021/2017-2020soildisturbanceMasterdatahenan2021-4-15.csv")

#check the data
head(mound20172019)

p <- ggplot(data = mound20172019, mapping = aes(X.totalmoundcover.10m2.))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
stat_function(fun= dnorm, args = list(mean = mean(mound20172019$X.totalmoundcover.10m2.), sd = sd(mound20172019$X.totalmoundcover.10m2.)))
p


#how to split OR merge the dataset based on your needs
colnames(mound20172019)[10]<-"totalmoundcover"

attach(mound20172019)
mound2017_zokor<-mound20172019[year==2017,]
mound2018_zokor1<-mound20172019[year==2018 & pika.zokor==1,]
mound2018_zokor0<-mound20172019[year==2018 & pika.zokor==0,]
mound2019_zokor1<-mound20172019[year==2019 & pika.zokor==1,]
mound2019_zokor0<-mound20172019[year==2019 & pika.zokor==0,] 
mound2020_zokor1<-mound20172019[year==2020 & pika.zokor==1,]
mound2020_zokor0<-mound20172019[year==2020 & pika.zokor==0,]  
dim(mound2017_zokor)
dim(mound2018_zokor1)
dim(mound2018_zokor0)
dim(mound2019_zokor1)
dim(mound2019_zokor0) 
dim(mound2020_zokor1)
dim(mound2019_zokor0)
  
#first try:: to make histogram and normal distribution 
#fig_mound2017_zokor
fig_mound2017_zokor <- ggplot(data = mound2017_zokor, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2017_zokor$totalmoundcover), sd = sd(mound2017_zokor$totalmoundcover)))+
  labs(title = "2017 zokor mound cover(background data)",x = "mound cover/10m2",y = "frequency")
fig_mound2017_zokor

#fig_mound2018_zokor1
fig_mound2018_zokor1 <- ggplot(data = mound2018_zokor1, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2018_zokor1$totalmoundcover), sd = sd(mound2018_zokor1$totalmoundcover)))+
  labs(title = "2018 zokor mound cover(zokor1)",x = "mound cover/10m2",y = "frequency")
fig_mound2018_zokor1

#fig_mound2018_zokor0
fig_mound2018_zokor0 <- ggplot(data = mound2018_zokor0, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2018_zokor0$totalmoundcover), sd = sd(mound2018_zokor0$totalmoundcover)))+
  labs(title = "2018 zokor mound cover(zokor0)",x = "mound cover/10m2",y = "frequency")
fig_mound2018_zokor0


#fig_mound2019_zokor1
fig_mound2019_zokor1 <- ggplot(data = mound2019_zokor1, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2019_zokor1$totalmoundcover), sd = sd(mound2019_zokor1$totalmoundcover)))+
  labs(title = "2019 zokor mound cover(zokor1)",x = "mound cover/10m2",y = "frequency")
fig_mound2019_zokor1

#fig_mound2019_zokor0
fig_mound2019_zokor0 <- ggplot(data = mound2019_zokor0, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2019_zokor0$totalmoundcover), sd = sd(mound2019_zokor0$totalmoundcover)))+
  labs(title = "2019 zokor mound cover(zokor0)",x = "mound cover/10m2",y = "frequency")
fig_mound2019_zokor0

#fig_mound2020_zokor1
fig_mound2020_zokor1 <- ggplot(data = mound2020_zokor1, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2020_zokor1$totalmoundcover), sd = sd(mound2020_zokor1$totalmoundcover)))+
  labs(title = "2020 zokor mound cover(zokor1)",x = "mound cover/10m2",y = "frequency")
fig_mound2020_zokor1

#fig_mound2020_zokor0
fig_mound2020_zokor0 <- ggplot(data = mound2020_zokor0, mapping = aes(totalmoundcover))+
  geom_histogram(aes(y=..density..), breaks = seq(0,10,by = 0.5),color="black", fill = "white")+
  stat_function(fun= dnorm, args = list(mean = mean(mound2020_zokor0$totalmoundcover), sd = sd(mound2020_zokor0$totalmoundcover)))+
  labs(title = "2020 zokor mound cover(zokor0)",x = "mound cover/10m2",y = "frequency")
fig_mound2020_zokor0

#how to rescale the normal distribution, is it neccessary
#use gridExtra  
moundcover_comp<-grid.arrange(fig_mound2017_zokor,fig_mound2017_zokor,fig_mound2018_zokor1,fig_mound2018_zokor0,fig_mound2019_zokor1,fig_mound2019_zokor0,fig_mound2020_zokor1,fig_mound2020_zokor0,ncol=2)

ggsave(file="moundcover_comp1.pdf",plot = moundcover_comp,width = 24,height = 30,units = "cm")  

#use grid
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 4,ncol = 2)))
define_region <-function(row,col){
  viewport(layout.pos.row = row,layout.pos.col = col)
}

print(fig_mound2017_zokor,vp = define_region(row = 1,col=1))
print(fig_mound2018_zokor1,vp = define_region(row = 2,col=1))
print(fig_mound2018_zokor0,vp = define_region(row = 2,col=2))
print(fig_mound2019_zokor1,vp = define_region(row = 3,col=1))
print(fig_mound2019_zokor0,vp = define_region(row = 3,col=2))
print(fig_mound2020_zokor1,vp = define_region(row = 4,col=1))
print(fig_mound2020_zokor0,vp = define_region(row = 4,col=2))














































