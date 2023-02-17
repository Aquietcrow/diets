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


#Divide the 2022 data to base group and Ligularia_sp group
#Divide the species name list to different functional groups
#Bryce, R., van der Wal, R., Mitchell, R. et al. Metapopulation Dynamics of a Burrowing Herbivore Drive Spatio-temporal Dynamics of Riparian Plant Communities. Ecosystems 16, 1165–1177 (2013). https://doi.org/10.1007/s10021-013-9677-9

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


#2022 12 29
#test independence of errors of each sampling site 
# Input data from MS Access
# ls()
# rm()

################################################################
#Import data from MS Access
###############################################################
SummerHenan2021_22<-odbcConnectAccess2007("E:/Access/2021_summer_henan.accdb")
PatchCover2021<-sqlFetch(SummerHenan2021_22,"2021_summer_henan_patchescover")
SpeCov2021_22<-sqlFetch(SummerHenan2021_22,"2021_summer_henan_plotcover")
Biomas2021_22<-sqlFetch(SummerHenan2021_22,"2021_summer_henan_biomass")
spe_plantnames <- sqlFetch(SummerHenan2021_22,"2021_summer_henan_speciesname")
background_zokorLivestock<-sqlFetch(SummerHenan2021_22,"2021_summer_henan_sitesinformation")
# write.csv(background_zokorLivestock, file = "background_zokorLivestock.csv",row.names = TRUE)
spe_plantnames_rightorder<-spe_plantnames[order(spe_plantnames[,1]),]
# spe_plantnames_rightorder$Chinesename==colnames(SpeCov2021)[8:114]
colnames(SpeCov2021_22)[8:114]<-spe_plantnames_rightorder$Latinname

View(SpeCov2021_22)
date1<-as.Date('2022-01-01')
#"Ops.POSIXt", "Ops.Date"
#Failedfield<-c("42c","42t","43c","43t")
SpeCov2021<-subset(SpeCov2021_22,SpeCov2021_22$time < date1)
SpeCov2022<-subset(SpeCov2021_22,SpeCov2021_22$time > date1)

########################################################################Compare the average cover between each species in each field and #treatment in 2021 and 2022.
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
subplot_inform <- SpeCov2021_dele_failedfields[2:4]
x1<-merge(SpeCov2022,subplot_inform,by="subplot") 
#dim() #to check data structure
#length() #to for check data length


############################################

# rm(list = ls(all=TRUE))

############################################################
#Prepare Bare land patch data                                        
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

PatchCover2021new<-reNaTA(PatchCover2021,1,7)
View(PatchCover2021new)
PatchCo2021Reorder<-PatchCover2021new[order(PatchCover2021new[,1]),]
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


PatchCo2021Counted<-CountPatch(x=PatchCo2021Reorder,numA = 0,numB = 0,numC = 0) 

# attach(PatchCo2021Counted)
# detach(PatchCo2021Counted)
NumASum <-aggregate(PatchCo2021Counted$NumA~fieldcode,data = PatchCo2021Counted,sum)
NumBSum <-aggregate(PatchCo2021Counted$NumB~fieldcode,data = PatchCo2021Counted,sum) 
NumCSum <-aggregate(PatchCo2021Counted$NumC~fieldcode,data = PatchCo2021Counted,sum) 

x1<-merge(NumASum,NumBSum,by="fieldcode")
x2<-merge(x1,NumCSum,by="fieldcode")
colnames(x2)[3]<-"NumB"
colnames(x2)[4]<-"NumC"
x2$NumAPer<-x2$NumA/2500  
x2$NumBPer<-x2$NumB/2500
x2$NumCPer<-x2$NumC/2500
apply(x2[,5:7],2,range)  
apply(x2[,5:7],2,mean)  

PatchCo2021Per<-x2  

######################################################
#Cover analysis                                      #
######################################################
#Replace the NA in plant cover investigation to 0.   #
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
#spe_henan_notbare <- rbind(spe_henan_base,spe_henan_Ligularia_sp,spe_henan_Oxytropis_sp)
apply(spe_henan_notbare,2,range)# The range of data in each column.
#table() function is to test the occurrence of different variable.
#fractions() #ref. packages: MASS
#sqrt() #^
#t()
#traceback()
#spe[row,col],spe[m=row,n=col]
#spe_new<-spe[,4:102]
#spe_t<-t(spe_new)

###################################################################
#create a new dataframe
spe_col<-c(rep(x,99))
spe_num<-c(rep(1,99))
spe_occur<-data.frame(spe_col,spe_num)
##############################################
#Iteration 2022 12 04
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
#recalculate the frequency and average cover data
spe_fre_21<-func_occur(SpeCov2021_Base)
length(colnames(SpeCov2021_Base))
colnames(SpeCov2021_Base)[103]
spe_fre_21order<-spe_fre_21[order(spe_fre_21[,2],decreasing = T),]

spe_freLig_21<-func_occur(SpeCov2021_Ligularia)
spe_freLig_21order<-spe_freLig_21[order(spe_freLig_21[,2],decreasing = T),]
#barplot(spe_order)
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
# Draw the bar plot using ggplot.
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

# barplot(df_yx2$spe_cov_ave,main="Average coverage",xlab="Plant species", ylab = "Cover", horiz = TRUE,names.arg = df_yx2$spe_name)
# barplot(df_yx3$spe_num, main="Occurrence frequency", xlab = "Plant species", ylab = "Frequency", horiz = FALSE)
############################################################
#Using Lig data from 2022 December
############################################################








#####################################################################################
#using MCMC or resampling method, not average the cover data of subplot in each block
#plant species composition comparison 2022 12 05
#####################################################################################





######################################################################
#Biomass analysis                                                    #
######################################################################
#I has failed the cooperation with herders in Field 42 and Field 42. #     
######################################################################
FailedfieldNo1<-grep("42c",bioma_nobare$fieldcode,fixed = TRUE)
FailedfieldNo2<-grep("42t",bioma_nobare$fieldcode,fixed = TRUE)
FailedfieldNo3<-grep("43c",bioma_nobare$fieldcode,fixed = TRUE)
FailedfieldNo4<-grep("43t",bioma_nobare$fieldcode,fixed = TRUE)
FailedfieldNoall<-c(FailedfieldNo1,FailedfieldNo2,FailedfieldNo3,FailedfieldNo4)
bioma_nobare2021<-bioma_nobare[-FailedfieldNoall,]
bioma_all2021<-merge(subplot_all,bioma_nobare2021,by="subplot")
a<-c(1,2,3,4,8,11:19)
bioma_all2021clean<-bioma_all2021[-193,a]

##################################################################
# Goal_Subset_Omit every row that satisfy bioma$functionalgroup = "bare"
#Biomass data analysis 2022 03, biomass data has not update yet[2022 12 04]
##################################################################
subset(bioma,bioma$functionalgroup != "bare")
#Ligularia_sp
bioma_subset1 <- subset(bioma,is.na(bioma$functionalgroup) == TRUE)# this chooses the NA value
bioma_subset2 <- subset(bioma,bioma$functionalgroup != "bare")# this chooses other description,such as,Ligularia_sp
bioma_nobare <- rbind(bioma_subset1,bioma_subset2)
summary(bioma_nobare)
#moldy samples were labeled as integer "1".
bioma_nobare$if_moldy
bioma_nobare$`grass(g)`[is.na(bioma_nobare$`grass(g)`)==TRUE] <- 0
bioma_nobare$`sedge(g)`[is.na(bioma_nobare$`sedge(g)`)==TRUE] <- 0
bioma_nobare$`legume(g)`[is.na(bioma_nobare$`legume(g)`)==TRUE] <- 0
bioma_nobare$`forb(g)`[is.na(bioma_nobare$`forb(g)`)==TRUE] <- 0
bioma_nobare$`poisonousplants(g)`[is.na(bioma_nobare$`poisonousplants(g)`)==TRUE] <- 0
bioma_nobare$`litter(g)`[is.na(bioma_nobare$`litter(g)`)==TRUE] <- 0

##############################################################
# Goal_Add "t" to each row [2022 03]
# if the string in bioma_nobare$fieldcode has "t"
#   TRUE, add one paremeter "t" 
#   FALSE, add one paremeter "c"
#grep() and grepl() are arguments can search certain string for target string.
#str(), summary()
#nchar(bioma_nobare$fieldcode[1])
##############################################################
str(grepl("c",bioma_nobare$fieldcode))
bioma_nobare$treatment <- grepl("c",bioma_nobare$fieldcode)
#control flow
bioma_nobare$treatment[bioma_nobare$treatment == TRUE] <-"c"
bioma_nobare$treatment[bioma_nobare$treatment == FALSE] <-"t"
bioma_nobare$treatment
bioma_nobare$totalbiomass(g) <- bioma_nobare$`grass(g)` + bioma_nobare$`sedge(g)`+ bioma_nobare$`legume(g)`+bioma_nobare$`forb(g)` + bioma_nobare$`poisonousplants(g)`

###########################################################
#turn the string(chr) to factor
##########################################################
bioma_nobare$fieldcode <- as.character(bioma_nobare$fieldcode)
bioma_nobare$field.no <- as.factor(bioma_nobare$field.no)
bioma_nobare$treatment <- as.factor(bioma_nobare$treatment)
#NOTE: Some sites do have NA. I should check before I compare these sites with the other sites.

####To be continue.2022 12 05
a<-melt(bioma_all2021clean,id=c("functionalgroup.x","treatment"))

##############################################################
#delete environmental variables 
##############################################################
#a<-ls()
#rm(list=a[which(a!='bioma_nobare')])

##############################################################
#Goal_Graphs
##############################################################

############################################################
# 2022 12 05 
# No.1 To test the normality of data
# No.2 If variances in different groups are different? we hope variances in different groups are not different[this test is sensitive to outliers]
############################################################
 

##############################################################
#p135 <R in Action 2nd> in Chinese
#analysis the biomass data.
##############################################################
mystats <- function(x, na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]#the data which not NA#
    m <- mean(x)
    n <-length(x)
    s <-sd(x)/sqrt(n)
  return(c(n=n,mean=m,se=s))
}

dstats <- function(x) sapply(x,mystats)

mylist <-c("grass(g)","sedge(g)","legume(g)","forb(g)","poisonousplants(g)","litter(g)","totalbiomass(g)")

by(bioma_nobare[mylist],bioma_nobare$fieldcode,dstats)
# with()
##############################################################
#Q: how to set the decimal to 0.01g 0.01g.
#To get the field number.
#bioma_nobare$field.no <- substring(bioma_nobare$fieldcode,1,nchar(bioma_nobare$fieldcode)-1)
##############################################################

##############################################################
#two-way anova 2022 12 5
# ok, now do a 2-way ANOVA, or GLM, with as factor field, treatment, and field*treatment, and as dependent total biomass, then thee same for grass, etc.
##############################################################

attach(bioma_all2021clean)
detach(bioma_all2021clean)

a<-aggregate(bioma_all2021clean[mylist],by=list(field.no,treatment,functionalgroup.x), FUN=mean)
b<-aggregate(bioma_all2021clean[mylist],by=list(field.no,treatment,functionalgroup.x), FUN=sd)
aggregate(bioma_all2021clean[mylist],by=list(field.no), FUN=mean)
aggregate(bioma_all2021clean[mylist],by=list(treatment), FUN=mean)  

fit1<-qqPlot(lm(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$treatment,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

fit_totalbio_treatment<-aov(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$treatment)
summary(fit_totalbio_treatment)
#There is no difference between different treatment.
fit_totalbio_site<-aov(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$`field.no`)
summary(fit_totalbio_site)
# Results
# Df Sum Sq Mean Sq F value
# bioma_all2021clean$field.no  16   3554  222.12   6.225
# Residuals                   314  11205   35.68        
# Pr(>F)    
# bioma_all2021clean$field.no 4.97e-12 ***
#   Residuals                               
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# #There is significant difference in different sites.

################################################
#How to find outliers from the normality test
################################################
outlierTest(fit1)
outlierTest(fit2)

fit2<-bartlett.test(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$`treatment`,data = bioma_all2021clean)
#Results
# Bartlett test of homogeneity of variances
# 
# data:  bioma_all2021clean$`totalbiomass(g)` by bioma_all2021clean$treatment
# Bartlett's K-squared = 18.955, df = 1, p-value =1.338e-05
# print(1.338e-05,options())

fit3<-qqPlot(lm(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`grass(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`sedge(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`legume(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`forb(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`poisonousplants(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`litter(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)
# aggregate(bioma_nobare[mylist],by=list(field.no,treatment), FUN=sd)

####################################################################
# Two-way anova 2022 12 05
####################################################################
# fit_bioma_nobare_litter <- aov(bioma_nobare$`litter`~treatment*field.no)
# summary(fit_bioma_nobare_litter)
# fit_bioma_nobare_tb <- aov(bioma_nobare$`totalbiomass(g)`~treatment*field.no)
# summary(fit_bioma_nobare_tb)
my_data <- bioma_all2021clean

##########################################
#based on tidyverse package///facet_wrap
#grass biomass
##########################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`grass(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on grass biomass")

result1<-aov(`grass(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`grass(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`grass(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`grass(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
#all fields have no difference in their control and treatment.
bioma_all2021clean$`sedge(g)`

#########################################################################
#sedge biomass
#########################################################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`sedge(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on sedge biomass")

result1<-aov(`sedge(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`sedge(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`sedge(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`sedge(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
#all fields have no difference in their control and treatment.
#########################################################################
#forb biomass
#########################################################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`forb(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on forb biomass")

result1<-aov(`forb(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`forb(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`forb(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`forb(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
#all fields have no difference in their control and treatment.


#########################################################################
#poisonousplants biomass
#########################################################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`litter(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on litters")

result1<-aov(`litter(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`litter(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`litter(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`litter(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
#all fields have no difference in their control and treatment.

#########################################################################
#poisonousplants biomass
#########################################################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`poisonousplants(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on poisonous plants biomass")

result1<-aov(`poisonousplants(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`poisonousplants(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`poisonousplants(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`poisonousplants(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
#all fields have no difference in their control and treatment.

#########################################################################
# legume biomass
# bioma_all2021clean
#########################################################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`legume(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on legume biomass")

result1<-aov(`legume(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`legume(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`legume(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`legume(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
#all fields have no difference in their control and treatment.

#########################################################################
#total biomass
#########################################################################
my_data %>% head()
my_data %>%
  ggplot(aes(x=`treatment`,y=`totalbiomass(g)`,fill=`treatment`))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(field.no))+
  labs(title = "Effects of treatment and site on total biomass")

result1<-aov(`totalbiomass(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "field.no") %>%
  broom::tidy()
# result is significantly different among different sites
result2<-aov(`totalbiomass(g)`~treatment+field.no,data = my_data) %>%
  TukeyHSD(which = "treatment") %>%
  broom::tidy()
# result is not significantly different between different treatments
result3<-aov(`totalbiomass(g)`~treatment*field.no,data = my_data) %>%
  broom::tidy()
#no interaction between the treatment and field.no, the result is caused by field.no difference.
resultx4<-my_data%>%
  group_by(field.no)%>%
  summarise(
    broom::tidy(aov(`totalbiomass(g)`~treatment,data = cur_data())),
    .groups = "keep"
  )%>%
  select(term,statistic,p.value)%>%
  filter(term != "Residuals")%>%
  arrange(p.value)
###########################################################################facet_wrap
#stack bar plot
#############################################################################
View(bioma_all2021clean)
a<-c(6:10,12,14)
bioma_all2021_forstack<-bioma_all2021clean[,a]
# colnames(bioma_all2021clean)[6:14]
View(bioma_all2021_forstack)

mylist2<-c("grass(g)","sedge(g)","legume(g)","forb(g)","poisonousplants(g)")
bioma_all2021_stack_2<-aggregate(bioma_all2021_forstack[mylist2],by=list(bioma_all2021_forstack$field.no,bioma_all2021_forstack$treatment), FUN=mean)
View(bioma_all2021_stack_2)
colnames(bioma_all2021_stack_2)[1]<-"field.no" 
colnames(bioma_all2021_stack_2)[2]<-"treatment"

bioma_all2021_stack<-melt(bioma_all2021_stack_2,id=c("treatment","field.no"))
View(bioma_all2021_stack)
colnames(bioma_all2021_stack) [3]<-"functional_group"
colnames(bioma_all2021_stack) [4]<-"biomass"
# [1] "treatment"        "field.no"         "functional_group"
# [4] "biomass(g)"
#gather() has reshape data function, similar to melt(). 
#df2 <-bioma_all2021_forstack%>%gather(treatment,field.no,ends_with("(g)"))

#facet_wrap stacked bar plot r ggplot2
ggplot(bioma_all2021_stack,aes(x=treatment,y=biomass,fill=functional_group))+
  geom_col()+
  labs(y="biomass(g)")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x=element_text( vjust = 1, hjust = 1,size=12,face = "bold"))+
  theme(axis.title.x = element_text(vjust = 2,size=14,face="bold"))+
  theme(axis.title.y = element_text(vjust=2,size=14,face = "bold"))+
  facet_grid(.~field.no)





















