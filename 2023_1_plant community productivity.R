# install.packages("RODBC")# Use MS access
# install.packages("ade4") #Exploratory and Euclidean Methods in Environmental Sciences. Tools for multivariate data analysis.Monte-Carlo Test
# install.packages("vegan") #Community Ecology Package. Ordination methods, diversity analysis and other functions for community and vegetation ecologists.
# install.packages("reshape2") #Flexibly restructure and aggregate data using just two functions: melt and 'dcast' (or 'acast').
# install.packages("ggplot2")
# install.packages("lattice") #The lattice add-on package is an implementation of Trellis graphics for R
# install.packages("plotrix") #Various Plotting Functions ... Lots of plots, various labeling, axis and color scaling functions.
# install.packages("lubridate")#time or date related,"Ops.POSIXt", "Ops.Date"
# install.packages("purrr")#for working with functions and vectors
# install.packages("sqldf")# running SQL statements on R data frames, optimized for convenience. sqldf works with the SQLite, H2, PostgreSQL
# install.packages("car")#qqPlot; an acronym for Companion to Applied Regression
# install.packages("tidyverse")#The 'tidyverse' is a set of packages that work in harmony because they share common data representations and 'API' design.
# install.packages("stringr")#for strings
# install.packages("dplyr")#a grammar of data manipulation, providing a consistent set of verbs that solve the most common data manipulation challenges
# install.packages("dendextend") #extending 'dendrogram' objects in R, letting you visualize and compare trees of 'hierarchical clusterings'. Heatmaps.
# install.packages("vioplot") #Violin plot, a boxplot combined with density of data.
# install.packages("ggpubr")
# install.packages("Rmisc")#arrange graphs.
# [ctrl + shift + c] can make notes.
# .libPaths()
# "C:/Users/oh_my/AppData/Local/R/win-library/4.2"
# "C:/Program Files/R/R-4.2.2/library" 

library("RODBC")
library("ade4")
library("vegan")
library("reshape2")
library("ggplot2")
library("lattice")
library("plotrix")
library("lubridate")
library("purrr")
library("sqldf")
library("car")
library("tidyverse")
library("vegan")
library("MASS")
library("readxl")
library("tidyr")
library("vioplot")
library("psych")#corr.test
library("ggpubr")#for making plots
library("Rmisc")
SummerHenan2021_22 <- odbcConnectAccess2007("E:/Access/2021_summer_henan_20231115.accdb")
PatchCover2021 <- sqlFetch(SummerHenan2021_22,"2021_summer_henan_patchescover")
Zokor_2021 <- sqlFetch(SummerHenan2021_22,"2021_summer_henan_sitesinformation")
PatchCover2022 <- sqlFetch(SummerHenan2021_22,"2022_summer_henan_patchescover")

############################################################
#Prepare Bare land patch data 20231129                                       
############################################################
#TurnrNA in bare land data to A, because it is base land   
#is.na(PatchCover2021[,7:56])<-"A"                         
############################################################
#A:base land
#B:bare land
#C:Ligularia_sp land
reNaTA <- function(x,n,m,na.omit=FALSE)
{for( n in 1:nrow(x))
{for( m in 7:ncol(x))
{if (is.na(x[n,m])==TRUE)
{x[n,m]<- "A"}
  else
  {m<-m+1}}
  n<-n+1} 
  result<- as.data.frame(x)
  return(result)}

PatchCover2021 <- reNaTA(PatchCover2021,1,7)
View(PatchCover2021)
PatchCo2021 <- PatchCover2021[order(PatchCover2021[,1]),]

PatchCover2022 <- reNaTA(PatchCover2022[101:400,],1,7) 
#####################################################################
CountPatch <- function(x,numA,numB,numC,na.omit=FALSE)
{for(n in 1:nrow(x))
{for(m in 7:56)
{if(x[n,m]=="A")
{numA<-numA+1;
m<-m+1}
  else if(x[n,m]=="B")
  {numB<-numB+1;
  m<-m+1}
  else if(x[n,m]=="C")
  {numC<-numC+1
  m<-m+1}
  else
  {m<-m+1}}
  x$NumA[n]<-numA;
  x$NumB[n]<-numB;
  x$NumC[n]<-numC;
  numA<-0;
  numB<-0;
  numC<-0;
  n<n+1}
  return(x)}
# 2022 bare land patch: "7c","7t","10-2c","10-2t","11-3c","11-3t"


PatchCo2021Counted <- CountPatch(x= PatchCo2021,numA = 0,numB = 0,numC = 0) 
PatchCo2022Counted <- CountPatch(x = PatchCover2022, numA = 0, numB = 0, numC = 0)
# attach(PatchCo2021Counted)
# detach(PatchCo2021Counted)
NumASum <- aggregate(PatchCo2021Counted$NumA~fieldcode,data = PatchCo2021Counted,sum)
NumBSum <- aggregate(PatchCo2021Counted$NumB~fieldcode,data = PatchCo2021Counted,sum) 
NumCSum <- aggregate(PatchCo2021Counted$NumC~fieldcode,data = PatchCo2021Counted,sum) 

NumASum <- aggregate(PatchCo2022Counted$NumA~fieldcode, data = PatchCo2022Counted,sum)
NumBSum <- aggregate(PatchCo2022Counted$NumB~fieldcode, data = PatchCo2022Counted,sum)
NumCSum <- aggregate(PatchCo2022Counted$NumC~fieldcode, data = PatchCo2022Counted,sum)

x1 <- merge(NumASum,NumBSum,by="fieldcode")
x2 <- merge(x1,NumCSum,by="fieldcode")
colnames(x2)[2]<-"NumA"
colnames(x2)[3]<-"NumB"
colnames(x2)[4]<-"NumC"
x2$NumAPer<-x2$NumA/2500*100  
x2$NumBPer<-x2$NumB/2500*100
x2$NumCPer<-x2$NumC/2500*100
apply(x2[-c(25:28),5:7],2,range)  
apply(x2[-c(25:28),5:7],2,mean)  

PatchCo2021Per <- x2  
PatchCo2022Per <- x2
write.csv(PatchCo2022Per, file = "PatchCo2022Per_20230224.csv",row.names = TRUE)
write.xlsx(PatchCo2021Per,file = "PatchCo2021Per_20231129.xlsx",rownames = FALSE)
####################################
# The outline of my data analysis ##
####################################
# In published data, the relationship between zokor abundance and bare land area, extract the original data of some robust papers.
# Create the parameter: "t"-treatment and "c"-control;
# Choose a statistic method to compare between "t" and "c";
# A frequency plot of bare land area in "c" and "t", respectively;


View(PatchCo2021Per)

str(grepl("c",PatchCo2021Per$fieldcode))
PatchCo2021Per$treatment <-grepl("c",PatchCo2021Per$fieldcode)

PatchCo2021Per$treatment[PatchCo2021Per$treatment == TRUE] <-"c"
PatchCo2021Per$treatment[PatchCo2021Per$treatment == FALSE] <-"t"
View(PatchCo2021Per)
#Violin plot
PatchCo2021Per$field.no <- substring(PatchCo2021Per$fieldcode,1,nchar(PatchCo2021Per$fieldcode)-1)

ZokorNumber<-data.frame(Zokor_2021$fieldcode,Zokor_2021$Total_number_of_zokor_in_treatment_fields,Zokor_2021$Killed_number_of_zokor_in_treatment_fields,Zokor_2021$The_remained_number_of_zokor_in_treatment_fields,Zokor_2021$Total_number_of_zokor_in_control_fields)
ZokorNumber[c(13,14),]
ZokorNumber_2021<-ZokorNumber[c(-13,-14),]

x1<-data.frame(ZokorNumber_2021$Zokor_2021.fieldcode,ZokorNumber_2021$Zokor_2021.Total_number_of_zokor_in_treatment_fields,ZokorNumber_2021$Zokor_2021.Killed_number_of_zokor_in_treatment_fields,rep("t",17))

x2<-data.frame(ZokorNumber_2021$Zokor_2021.fieldcode,ZokorNumber_2021$Zokor_2021.Total_number_of_zokor_in_control_fields,rep(0,17),rep("c",17))

names(x1)[1:4]<-c("field.no","Number_of_zokor_in_2021","Number_of_killed_zokor_in_2021","treatment")
names(x2)[1:4]<-c("field.no","Number_of_zokor_in_2021","Number_of_killed_zokor_in_2021","treatment")
ZokorNumber_2021_1<-rbind(x1,x2)
x3<-paste(ZokorNumber_2021_1$field.no,ZokorNumber_2021_1$treatment,sep = "")

ZokorNumber_2021_all<-cbind(ZokorNumber_2021_1,x3)
names(ZokorNumber_2021_all)[5]<-"fieldcode"
zokorNumber_PatchCover <- merge(ZokorNumber_2021_all,PatchCo2021Per,by="fieldcode")

zokorNumber_PatchCover$Remaining_number_of_zokor_2021<-zokorNumber_PatchCover$Number_of_zokor_in_2021-zokorNumber_PatchCover$Number_of_killed_zokor_in_2021

# zokorNumber_PatchCover_export<-zokorNumber_PatchCover[,c(1,2,3,4,5,7,10)]
# View(zokorNumber_PatchCover_export)
# write.csv(zokorNumber_PatchCover_export,file = "zokorNumber_PatchCover_export.csv",row.names=TRUE)
# write.csv(PatchCo2021Per,file = "PatchCo2021Per.csv",row.names = TRUE)
zokorNumber_PatchCover$treatment.y[zokorNumber_PatchCover$treatment.x=="c"]<-0
zokorNumber_PatchCover$treatment.y[zokorNumber_PatchCover$treatment.x=="t"]<-1


x4<-1:17
x5<-Zokor_2021$fieldcode[c(-13,-14)]
field.no2code<-data.frame(x5,x4)
names(field.no2code)[1:2]<-c("field.no","code")
#m=2

field.no2code_func<-function(x,y,n,m){
  for (n in 1:nrow(x)){
    for(m in 1:nrow(y)){
      if(x[n,2]==y[m,1]){
        x$code[n]<-y[m,2]}
      else{m<-m+1}}
    n<-n+1}
  return(x)}

x6<-field.no2code_func(zokorNumber_PatchCover,field.no2code,1,1)
zokorNumber_PatchCover_reg<-x6
write.csv(x6,file = "field.no2code_20230213.csv",row.names = TRUE)

View(zokorNumber_PatchCover_reg)
attach(zokorNumber_PatchCover_reg)
fit1<-lm(Number_of_zokor_in_2021~NumBPer,data = zokorNumber_PatchCover_reg)
summary(fit1)


x7<-zokorNumber_PatchCover_reg[,c(3,4,6,7,8,9,10,11,14)]
# x7$treatment.y<-as.numeric(x7$treatment.y)
#fit2<-lm(Number_of_zokor_in_2021~NumCPer)
a1<-cov(x7)
a2<-cor(x7)
a3<-cor(x7,method = "spearman")
cor.test(x7$Number_of_zokor_in_2021,x7$NumB,alternative = "two.side",method="pearson")
a4<-cor.test(x7$Number_of_zokor_in_2021,x7$NumB,alternative = "less",method="pearson")
a5<-corr.test(x7,use="complete")# package:psych
View(a5)
View(x7)      
ggscatter(x7,x="Number_of_zokor_in_2021",y="NumB",
          add = "reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab = "Number of zokor in 2021",ylab = "Bare land area (m2)")

ggscatter(x7,x="Number_of_zokor_in_2021",y="NumBPer",
          add = "reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab = "Number of zokor in 2021",ylab = "Bare land area (%)")

write.csv(zokorNumber_PatchCover_reg,file = "zokorNumber_PatchCover_reg.csv",row.names = TRUE)
write.csv(x7,file = "Zokor number and bare land in 2021_20230213.csv",row.names = TRUE)


zokorNumber_PatchCover_reg$Vegetated_Per <- zokorNumber_PatchCover_reg$NumCPer + zokorNumber_PatchCover_reg$NumAPer
PatchCover2021

x8 <- c("Number_of_zokor_in_2021","Number_of_killed_zokor_in_2021","Remaining_number_of_zokor_2021","~","NumAPer","NumBPer","NumCPer","Vegetated_Per")

#x is estimated data.
p1 <- ggscatter(zokorNumber_PatchCover_reg,x = "Number_of_zokor_in_2021", y = "Vegetated_Per",
          add = "reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab = "Number of zokor in 2021",ylab = "Proportion of vegetated area (%)")

p2 <- ggscatter(zokorNumber_PatchCover_reg,x = "Number_of_zokor_in_2021", y = "NumBPer",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Number of zokor in 2021",ylab = "Proportion of bare land (%)")

p3 <- ggscatter(zokorNumber_PatchCover_reg,x = "Number_of_zokor_in_2021", y = "NumCPer",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Number of zokor in 2021",ylab = "Proportion of poisonous plants (%)")
#x is real data.
p4 <- ggscatter(zokorNumber_PatchCover_reg,x = "Number_of_killed_zokor_in_2021", y = "Vegetated_Per",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Number of killed zokor in 2021",ylab = "Proportion of vegetated area  (%)")

p5 <- ggscatter(zokorNumber_PatchCover_reg,x = "Number_of_killed_zokor_in_2021", y = "NumBPer",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Number of killed zokor in 2021",ylab = "Proportion of bare land (%)")

p6 <- ggscatter(zokorNumber_PatchCover_reg,x = "Number_of_killed_zokor_in_2021", y = "NumCPer",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Number of killed zokor in 2021",ylab = "Proportion of poisonous plants (%)")

#x is estimated data.
p7 <- ggscatter(zokorNumber_PatchCover_reg,x = "Remaining_number_of_zokor_2021", y = "Vegetated_Per",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Remaining number of zokor 2021",ylab = "Proportion of vegetated area  (%)")
p8 <- ggscatter(zokorNumber_PatchCover_reg,x = "Remaining_number_of_zokor_2021", y = "NumBPer",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Remaining number of zokor 2021",ylab = "Proportion of bare land (%)")

p9 <- ggscatter(zokorNumber_PatchCover_reg,x = "Remaining_number_of_zokor_2021", y = "NumCPer",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Remaining number of zokor 2021",ylab = "Proportion of poisonous plants (%)")

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,cols = 3)














