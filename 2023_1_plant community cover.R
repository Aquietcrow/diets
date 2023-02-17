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
# library("dendextend")

#2023 1 14
# 2022 dataset has not been prepared
# using MCMC or resampling method, not average the cover data of subplot in each block
#plant species composition comparison 2022 12 05

#2022 12 29
#1#USING PUBLISHED DATA
#The min-middle-max density of plateau zokors.
#The min-middle-max density of plateau zokor mounds.
#The spatial pattern of plateau zokor and plateau zokor mounds.
#2#USING NETLOGO TO MAKE A MODEL OF PLATEAU ZOKORS DISTURBANCE MODEL.
#3#DO A PCA OF PLANT COMMUNITY DATA OF 2021.
#THEN PLANT COMMUNITY DATA OF 2022.
#THEN COMPARE BETWEEN 2021 AND 2022.

#2022 12 16 
#This paper is closely related to my analysing method
#Find more good papers in this field, then do the basic analysis.
# View(spe_plantnames)
# unpalatable_species
# grass
# sedge
# legume
# forb  


#2022 12 9
#test independence of errors of each sampling site 
#Divide the 2022 data to base group and Ligularia_sp group
#Divide the species name list to different functional groups
#Bryce, R., van der Wal, R., Mitchell, R. et al. Metapopulation Dynamics of a Burrowing Herbivore Drive Spatio-temporal Dynamics of Riparian Plant Communities. Ecosystems 16, 1165–1177 (2013). https://doi.org/10.1007/s10021-013-9677-9
# Input data from MS Access
# ls()
# rm()

################################################################
#Import data from MS Access
###############################################################
SummerHenan2021_22<-odbcConnectAccess2007("E:/Access/2021_summer_henan.accdb")
SpeCov2021_22<-sqlFetch(SummerHenan2021_22,"2021_summer_henan_plotcover")
spe_plantnames <- sqlFetch(SummerHenan2021_22,"2021_summer_henan_speciesname")
spe_plantnames_rightorder<-spe_plantnames[order(spe_plantnames[,1]),]
# spe_plantnames_rightorder$Chinesename==colnames(SpeCov2021)[8:114]
colnames(SpeCov2021_22)[8:114]<-spe_plantnames_rightorder$Latinname

View(SpeCov2021_22)
date1<-as.Date('2022-01-01')
#"Ops.POSIXt", "Ops.Date"
#Failedfield<-c("42c","42t","43c","43t")
SpeCov2021<-subset(SpeCov2021_22,SpeCov2021_22$time < date1)
SpeCov2022<-subset(SpeCov2021_22,SpeCov2021_22$time > date1)

########################################################################
#Compare the average cover between each species in each field and #treatment in 2021 and 2022.
#######################################################################
FailedfieldNo1<-grep("42c",SpeCov2021$fieldcode,fixed = TRUE)
FailedfieldNo2<-grep("42t",SpeCov2021$fieldcode,fixed = TRUE)
FailedfieldNo3<-grep("43c",SpeCov2021$fieldcode,fixed = TRUE)
FailedfieldNo4<-grep("43t",SpeCov2021$fieldcode,fixed = TRUE)
FailedfieldNoall<-c(FailedfieldNo1,FailedfieldNo2,FailedfieldNo3,FailedfieldNo4)

SpeCov2021_dele_failedfields <- SpeCov2021[-FailedfieldNoall,]
SpeCov2021_Base <- subset(SpeCov2021_dele_failedfields,SpeCov2021_dele_failedfields$functionalgroup == "base")
SpeCov2021_Ligularia <- subset(SpeCov2021_dele_failedfields,SpeCov2021_dele_failedfields$functionalgroup == "Ligularia_sp")
SpeCov2021_Bare <- subset(SpeCov2021_dele_failedfields,SpeCov2021_dele_failedfields$functionalgroup == "bare")
# View(SpeCov2021_Base)
# View(SpeCov2021_Ligularia)
# View(SpeCov2021_Bare)
subplot_inform <- SpeCov2021_dele_failedfields[2:4]#Extract the subplot functional group information from 2021's data.
SpeCov2022_mergedsubplot<-merge(SpeCov2022,subplot_inform,by="subplot") 
write.csv(SpeCov2022_mergedsubplot,file = "SpeCov2022_mergedsubplot.csv",row.names = TRUE)
#dim() #to check data structure
#length() #to for check data length
# rm(list = ls(all=TRUE))

######################################################
#Cover analysis                                      
######################################################
#Function -- Replace the NA in plant cover investigation to 0.   
######################################################
reNaT0 <- function(x,na.omit=FALSE)
{for( n in 1:nrow(x))
{for( m in 7:ncol(x))
{if (is.na(x[n,m])==TRUE)
{x[n,m]<- 0}
  else
  {m<-m+1}}
  n<-n+1} 
  result<-x
  return(result)}

SpeCov2021_Base_0<-reNaT0(SpeCov2021_Base)
SpeCov2021_Ligularia_0<-reNaT0(SpeCov2021_Ligularia)


###################################################################
#create a new dataframe based on the number of plant species
x<-"a"
spe_col<-c(rep(x,99))
spe_num<-c(rep(1,99))
spe_occur<-data.frame(spe_col,spe_num)
##############################################
#Function -- plant species concurrences
##############################################
func_occur<-function(x,sum_occur=0){
  for(n in 8:ncol(x)){
    for(m in 1:nrow(x)){
      if(is.na(x[m,n])==TRUE){
        sum_occur<-sum_occur
        m<-m+1}
      else{
        sum_occur<-sum_occur+1
        m<-m+1}
    }
    spe_occur[n-7,1]<-colnames(SpeCov2021_Base)[n]
    spe_occur[n-7,2]<-sum_occur/nrow(x)
    sum_occur<-0
    n=n+1
  }
  return(spe_occur)
}
#####################################################
#2022 11 29
#recalculate the species occurrence
spe_fre_21<-func_occur(SpeCov2021_Base)
length(colnames(SpeCov2021_Base))
colnames(SpeCov2021_Base)[103]
spe_fre_21order<-spe_fre_21[order(spe_fre_21[,2],decreasing = T),]

spe_freLig_21<-func_occur(SpeCov2021_Ligularia)
spe_freLig_21order<-spe_freLig_21[order(spe_freLig_21[,2],decreasing = T),]
# trim=.2
# warnings()

#####################################################
#Average cover of base plot 2022 12 04
#####################################################
spe_cov_base_ave21<-apply(SpeCov2021_Base_0[8:103],2,mean,trim=0,options("scipen"=100, "digits"=4))#print format
#Q: trim'必需是长度必需为一的数值 A:trim=0
spe_cov_base_se21<-apply(SpeCov2021_Base_0[8:103],2,std.error,options("scipen"=100, "digits"=4))
# head(spe_cov_base_ave21)
# tail()
spe_name<-colnames(SpeCov2021_Base_0)[8:103]
spe_covave<-data.frame(spe_name,spe_cov_base_ave21,spe_cov_base_se21)
summary(spe_cov_base_ave21==spe_covave$spe_cov_base_ave21)#check if the vector is equal to the column.
summary(rownames(spe_covave)==spe_covave$spe_name)
spe_covave_21order<-spe_covave[order(spe_covave[,2],decreasing = T),]
grep("Trigonotis_peduncularis",spe_covave_21order$spe_name,fixed = TRUE)#result before no.27 is bigger than 1%.

#####################################################
#Average cover of Ligularia_virgaurea plot 2022 12 04
#####################################################
spe_cov_Lig_ave21<-apply(SpeCov2021_Ligularia_0[8:103],2,mean,trim=0,options("scipen"=100, "digits"=4))#print format
spe_cov_Lig_se21<-apply(SpeCov2021_Ligularia_0[8:103],2,std.error,options("scipen"=100, "digits"=4))
spe_cov_Lig_ave<-data.frame(spe_name,spe_cov_Lig_ave21,spe_cov_Lig_se21)
spe_cov_Lig_ave_21order<-spe_cov_Lig_ave[order(spe_cov_Lig_ave[,2],decreasing = T),]
grep("Kobresia_sp_unknown",spe_cov_Lig_ave_21order$spe_name,fixed = TRUE)
#result before no.26 is bigger than 1%

#export the data set.
write.csv(spe_covave_21order, file = "spe_covave_21order.csv",row.names = TRUE)
write.csv(spe_fre_21order,file = "spe_fre_21order.csv",row.names = TRUE)
write.csv(spe_freLig_21order,file = "spe_freLig_21order.csv",row.names = TRUE)
write.csv(spe_cov_Lig_ave_21order,file = "spe_cov_Lig_ave_21order.csv",row.names = TRUE)
############################################################
#Using base data from 2022 December
############################################################

spe_covave_21order<-read.csv("spe_covave_21order.csv",header=TRUE)
spe_fre_21order<-read.csv("spe_fre_21order.csv",header=TRUE)
# head(spe_covave_21order)
# head(spe_fre_21order)
# head(spe_CovFre)
colnames(spe_covave_21order)[2]<-"spe_name"
colnames(spe_covave_21order)[3]<-"spe_cov_ave" 
colnames(spe_covave_21order)[4]<-"spe_cov_sd"  

colnames(spe_fre_21order)[2]<-"spe_name"
colnames(spe_fre_21order)[3]<-"spe_num"
spe_CovFre <- merge(spe_covave_21order[2:4],spe_fre_21order[2:3],by="spe_name")
# df_2yx<- spe_CovFre[,c("spe_name","spe_cov_ave","spe_cov_sd","spe_num")]
# df_yx1<- spe_CovFre[1:20,c("spe_name","spe_cov_ave")]
df_yx2<- spe_covave_21order[1:20,c("spe_name","spe_cov_ave")]

df_yx3<- spe_fre_21order[1:20,c("spe_name","spe_num")]

############################################################
# Draw the bar plot using ggplot -- base subplot
###############################################################
ggplot(df_yx2,aes(x=reorder(spe_name,-spe_cov_ave),y=spe_cov_ave))+
  geom_col()+
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1,size=12,face = "bold"))+
  theme(axis.text.y = element_text(size=12, face="bold"))+
  xlab("Plant species")+
  ylab("Species average coverage")+
  theme(axis.title.x = element_text(vjust = 2,size=14,face="bold"))+
  theme(axis.title.y = element_text(vjust=2,size=14,face = "bold"))

ggplot(df_yx3,aes(x=reorder(spe_name,-spe_num),y=spe_num))+
  geom_col()+
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1,size=12,face = "bold"))+
  theme(axis.text.y = element_text(size=12, face="bold"))+
  xlab("Plant species")+
  ylab("Occurrence frequency")+
  theme(axis.title.x = element_text(vjust = 2,size=14,face="bold"))+
  theme(axis.title.y = element_text(vjust=2,size=14,face = "bold"))


# Diversity indices

pkgs<-c("ade4","adegraphics","adespatial","vegan", "vegetarian","ggplot2","FD","taxize")
# lapply(pkgs,require, character.only=T)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(pkgs)

# SpeCov2021_Base
# SpeCov2021_Ligularia_0
# SpeCov2021_Ligularia



#I need to check if need to deleted some subplot in "SpeCov2022_mergedsubplot" those were amended a new plot because of new zokor mounds.
# grouping the species name list, e.g. livestock edible species, and not edible species, zokor selected and not selected species.


# Spe_base_2021<-SpeCov2021_Base_0[,8:114]
# y1 <- cbind(SpeCov2021_Base_0,div_base_2021)
# View(SpeCov2021_Base_0)
SpeCov_2022 <- reNaT0(SpeCov2022_mergedsubplot)
SpeCov_2021 <- reNaT0(SpeCov2021_dele_failedfields)

z1 <- SpeCov_2021[,8:114]
z2 <- SpeCov_2022[,8:114]
#可以更改
########################################################
N0 <- rowSums(z1 > 0)
N0 <- specnumber(z1)
H <- diversity(z1) # shannon entropy (base e)
Hb2 <- diversity(z1, base = 2) #shannon entropy (base 2)
N1 <- exp(H) #Shannon diversity (base e); number of abundant species
N1b2 <- 2^Hb2 #shannon diversity (base 2)
N2 <- diversity(z1,"inv")#simpson diversity; (number of dominant species)
J <- H / log(N0) #Pielou evenness
E10 <- N1/N0 #Shannon evenness (Hill's ratio)
E20 <- N2/N0 #Simpson evenness (Hill's ratio)
div_2021 <- data.frame(N0,H,Hb2,N1,N1b2,N2,E10,E20,J)
########################################################
N0 <- rowSums(z2 > 0)
N0 <- specnumber(z2)
H <- diversity(z2) # shannon entropy (base e)
Hb2 <- diversity(z2, base = 2) #shannon entropy (base 2)
N1 <- exp(H) #Shannon diversity (base e); number of abundant species
N1b2 <- 2^Hb2 #shannon diversity (base 2)
N2 <- diversity(z2,"inv")#simpson diversity; (number of dominant species)
J <- H / log(N0) #Pielou evenness
E10 <- N1/N0 #Shannon evenness (Hill's ratio)
E20 <- N2/N0 #Simpson evenness (Hill's ratio)
div_2022 <- data.frame(N0,H,Hb2,N1,N1b2,N2,E10,E20,J)
########################################################


a1 <- c(2,3,4,7)
c1 <- cbind(SpeCov_2021[a1],div_2021)
a2 <- c(1,3,4,7,115,116)
c2 <- cbind(SpeCov_2022[a2],div_2022)

write.csv(c1,file = "Diversity indices in 2021.csv",row.names = TRUE)
