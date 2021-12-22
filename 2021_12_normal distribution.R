#lib.path ???C:/Users/oh_my/Documents/R/win-library/4.1???
#work dictionary

install.packages("pwr")
install.packages("vegan")
library ("pwr")
library("carData")
library("car")
library("multcomp")
library("tidyverse")
library("ggplot2")
library("stats")
library("car")
library("vegan")
library('permute')
library("lattice")
#For regression models
#Using both Q-Q plot and probability bar plot to test the normality of data
#Method 1
states <-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit, labels = row.names(states), id.method="identify",simulate=TRUE,main="Q-Q Plot")
#R????????????v2
#R in action analysis and graphics with R by Robert Kabacoff
#????????????8-6 ?????????????????????????????????

#Method 2
residplot <- function(fit, nbreaks = 10) {
  z <- rstudent(fit)
  hist(z,breaks=nbreaks, freq = FALSE,
       xlab = "Studentized redidual",
       main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x,mean=mean(z),sd=sd(z)),
        add=TRUE, col="blue", lwd = 2)
  lines(density(z)$x, density(z)$y,
        col="red",lwd=2,lty=2)
  legend("topright",legend = c("Normal Curve", "Kernel Density Curve"),
  lty = 1:2, col = c("blue","red"),cex=.7)
}
residplot(fit)


#Normality test
library(car)
attach(cholesterol)
qqPlot(lm(response ~ trt, data=cholesterol), simulate=TRUE,main="Q-Q Plot",labels=FALSE)
#simulate
#if TRUE calculate confidence envelope by parametric bootstrap; for lm object only. The method is due to Atkinson (1985).
#Atkinson, A. C. (1985) Plots, Transformations, and Regression. Oxford.

#Homogeneity of variance
bartlett.test(response ~ trt, data = cholesterol)


#Outliers test
library(car)
outlierTest(fit)


#2021-12-13 normality tests
#P198 R????????????v2
install.packages("tidyverse")
install.packages("ggplot2")

soil2007 <- read.csv("e:/?????????/A-??????/??????-??????/???????????????/2007-soil physichemical property.csv")


soil2007_10cm<-soil2007[which(soil2007$depth==10),]
soil2007_20cm<-soil2007[which(soil2007$depth==20),]
soil2007_30cm<-soil2007[which(soil2007$depth==30),]
#str()

qqPlot(lm(soil2007$water_content ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$pH ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$Electrical_conductivity ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$available_phosphorus ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$N ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$P ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$organic_matter ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$Total_potassium ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$available_potassium ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

qqPlot(lm(soil2007$alkali.hydrolyzale_nitrogen ~ soil2007$depth), simulate=TRUE,main="Q-Q Plot",labels=FALSE)

#what is the meaning of numbered values? Outliers?
#how to find the outliers
#If I need to transform/or scale the data?
#how to do the bar plot to test normal distribution?
#how to extract subset data?
#https://zhuanlan.zhihu.com/p/23824657

#I make some change here.2021/12/22





