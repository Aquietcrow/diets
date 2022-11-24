#install.packages("RODBC")
#install.packages("doBy")
#install.packages("plotrix")
#install.packages("lubridate")
install.packages("purrr")
#[ctrl + shift + c] can make notes.
#"RODBC" for Microsoft Access
#"ade4" for Monte-Carlo Test

library("RODBC")
library("ade4")
library("vegan")
library("reshape2")
library("ggplot2")
library("lattice")
library("plotrix")
library("lubridate")
library("purrr")
#Input data
SummerHenan2021<-odbcConnectAccess2007("E:/Access/2021_summer_henan.accdb")
PatchCover2021_22<-sqlFetch(SummerHenan2021,"2021_summer_henan_patchescover")
SpeCov2021_22<-sqlFetch(SummerHenan2021,"2021_summer_henan_plotcover")
Biomas2021<-sqlFetch(SummerHenan2021,"2021_summer_henan_biomass")
spe_plantnames <- sqlFetch(SummerHenan2021,"2021_summer_henan_speciesname")
spe_plantnames_rightorder<-spe_plantnames[order(spe_plantnames[,1]),]
# View(SpeCov2021)
# View(bioma)
# View(bioma_subset1)
# spe_plantnames_rightorder$Chinesename==colnames(SpeCov2021)[8:114]
colnames(SpeCov2021_22)[8:114]<-spe_plantnames_rightorder$Latinname
# str(spe_plant)
View(SpeCov2021_22)
date1<-as.Date('2022-01-01')
#"Ops.POSIXt", "Ops.Date"
#Failedfield<-c("42c","42t","43c","43t")
SpeCov2021<-subset(SpeCov2021_22,SpeCov2021_22$time < date1)
# FailedfieldNo1<-grep("42c",SpeCov2021$fieldcode,fixed = TRUE)
# FailedfieldNo2<-grep("42t",SpeCov2021$fieldcode,fixed = TRUE)
# FailedfieldNo3<-grep("43c",SpeCov2021$fieldcode,fixed = TRUE)
# FailedfieldNo4<-grep("43t",SpeCov2021$fieldcode,fixed = TRUE)
# FailedfieldNoall<-c(FailedfieldNo1,FailedfieldNo2,FailedfieldNo3,FailedfieldNo4)
SpeCov2021new<-SpeCov2021[-FailedfieldNoall,]
# View(SpeCov2021new_base)
SpeCov2021new_base <- subset(SpeCov2021new,SpeCov2021new$functionalgroup == "base")
SpeCov2021new_Ligularia <- subset(SpeCov2021new,SpeCov2021new$functionalgroup == "Ligularia_sp")

View(SpeCov2021new_Ligularia)

CountPatch <- function(x,n,m,FieCodeInput,PatchType,NumOfPatch = 0){
  for(n in 1:nrow(x)){
    for(m in 7:ncol(x)){
      if(x[n,m]==PatchType){
        NumOfPatch<-NumOfPatch+1
        m<- m+1
      } 
      else{
        NumOfPatch<-NumOfPatch
        m<-m+1
      }
    }
  }
  return()
  write.csv(fieldcode,PatchType,NumOfPatch)
}

func_occur<-function(x,m,n,sum_occur=0){
  for(n in 4:ncol(x)){
    for(m in 1:nrow(x)){
      if(x[m,n]!=0){
        sum_occur<-sum_occur+1
        m<-m+1}
      else{
        sum_occur<-sum_occur
        m<-m+1}
    }
    spe_occur[n-3,2]<-sum_occur/382*100
    spe_occur[n-3,1]<-colnames(spe)[n]
    sum_occur<-0
    n=n+1
  }
  return(spe_occur)
}
View(PatchCover2021_22)
# rm("Failedfield","FailedfieldNo")

############################################################
#Using data with Ligularia_sp from 2022 March
############################################################

# spe_covave_order<-read.csv("spe_covave_order.csv",header=TRUE)
# spe_fre_order<-read.csv("spe_fre_order.csv",header=TRUE)
# colnames(spe_fre_order)[2]<-"spe_name"
# spe_CovFre <- merge(spe_covave_order,spe_fre_order,by="spe_name")
# df_2yx<-spe_CovFre[,c("spe_name","spe_cov_ave","spe_cov_sd","spe_num")]
# df_yx1<- spe_CovFre[1:20,c("spe_name","spe_cov_ave")]
# df_yx2<- spe_covave_order[1:20,c("spe_name","spe_cov_ave")]
# df_yx3<- spe_fre_order[1:20,c("spe_name","spe_num")]


# Draw the graph
ggplot(df_yx2,aes(x=reorder(spe_name,-spe_cov_ave),y=spe_cov_ave))+geom_col()+theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1,size=12,face = "bold"))+theme(axis.text.y = element_text(size=12, face="bold"))+xlab("Plant species")+ylab("Species average coverage")+theme(axis.title.x = element_text(vjust = 2,size=14,face="bold"))+theme(axis.title.y = element_text(vjust=2,size=14,face = "bold"))

ggplot(df_yx3,aes(x=reorder(spe_name,-spe_num),y=spe_num))+geom_col()+theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1,size=12,face = "bold"))+theme(axis.text.y = element_text(size=12, face="bold"))+xlab("Plant species")+ylab("Occurrence frequency")+theme(axis.title.x = element_text(vjust = 2,size=14,face="bold"))+theme(axis.title.y = element_text(vjust=2,size=14,face = "bold"))

# barplot(df_yx2$spe_cov_ave,main="Average coverage",xlab="Plant species", ylab = "Cover", horiz = TRUE,names.arg = df_yx2$spe_name)
# barplot(df_yx3$spe_num, main="Occurrence frequency", xlab = "Plant species", ylab = "Frequency", horiz = FALSE)

##############################################
#%subset the poisonous plant plots% 2022 11 18
##############################################
str(PatchCover2021)
head(PatchCover2021)

##################################################################
# Goal_Subset_Omit every row that satisfy bioma$functionalgroup = "bare"
#Biomass data analysis
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
# Goal_Add "t" to each row [I can do this before Goal No.2]
# if the string in bioma_nobare$fieldcode has "t"
#   TRUE, add one paremeter "t" 
#   FALSE, add one paremeter "c"
##############################################################
 
##############################################################
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


##############################################################
#delete environmental variables 
##############################################################
#a<-ls()
#rm(list=a[which(a!='bioma_nobare')])
#ls()
#bioma_nobare <- subset(bioma_nobare, select = -mid)


##############################################################
#Goal_Graphs
##############################################################

#Frequency distribution histogram of total biomass
#Frequency distribution histogram of grass/sedges/legumes/forbs/poisonous plants/litter

#colnames(bioma_nobare) [15]<-"totalbiomass(g)"

#To test the normality of data
 hist(bioma_nobare$`grass(g)`)
# hist(bioma_nobare$`sedge(g)`)
# hist(bioma_nobare$`legume(g)`)
# hist(bioma_nobare$`forb(g)`)
# hist(bioma_nobare$`poisonousplants(g)`)
# hist(bioma_nobare$`litter(g)`)
# hist(bioma_nobare$`totalbiomass(g)`)

##########################################################
# Goal_Statistic analysis using Monte-Carlo randomization
# if total biomass are different between control groups(c) and treatment groups(t){homogeneous}

###########################################################
#Q: For biomass data, if I turn NA to 0, the average will be lower. Is it right? 
#A: Right, because 0 is also a data here.
###########################################################
 

###########################################################
#turn the string(chr) to factor
 ##########################################################
bioma_nobare$fieldcode <- as.character(bioma_nobare$fieldcode)
bioma_nobare$field.no <- as.factor(bioma_nobare$field.no)
bioma_nobare$treatment <- as.factor(bioma_nobare$treatment)
#NOTE: Some sites do have NA. I should check before I compare these sites with the other sites.

##############################################################
#p135 <R in Action 2nd> in Chinese
#What?
##############################################################
mystats <- function(x, na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <-length(x)
  s <-sd(x)/sqrt(n)
  return(c(n=n,mean=m,se=s))
}

dstats <- function(x) sapply(x,mystats)

mylist <-c("grass(g)","sedge(g)","legume(g)","forb(g)","poisonousplants(g)","litter(g)","totalbiomass(g)")

by(bioma_nobare[mylist],bioma_nobare$fieldcode,dstats)

##############################################################
#Q: how to set the decimal to 0.01g 0.01g.
#To get the field number.
#bioma_nobare$field.no <- substring(bioma_nobare$fieldcode,1,nchar(bioma_nobare$fieldcode)-1)


##############################################################
#two-way anova
# ok, now do a 2-way ANOVA, or GLM, with as factor field, treatment, and field*treatment, and as dependent total biomass, then thee same for grass, etc.
##############################################################
attach(bioma_nobare)
aggregate(bioma_nobare[mylist],by=list(field.no,treatment), FUN=mean)
aggregate(bioma_nobare[mylist],by=list(field.no,treatment), FUN=sd)

fit_bioma_nobare_grass <- aov(bioma_nobare$`grass(g)`~treatment*field.no)
summary(fit_bioma_nobare_grass)
fit_bioma_nobare_sedge <- aov(bioma_nobare$`sedge(g)`~treatment*field.no)
summary(fit_bioma_nobare_sedge)
fit_bioma_nobare_legume <- aov(bioma_nobare$`legume(g)`~treatment*field.no)
summary(fit_bioma_nobare_legume)
fit_bioma_nobare_forb <- aov(bioma_nobare$`forb(g)`~treatment*field.no)
summary(fit_bioma_nobare_forb)
fit_bioma_nobare_pp <- aov(bioma_nobare$`poisonousplants(g)`~treatment*field.no)
summary(fit_bioma_nobare_pp)
fit_bioma_nobare_litter <- aov(bioma_nobare$`litter`~treatment*field.no)
summary(fit_bioma_nobare_litter)
fit_bioma_nobare_tb <- aov(bioma_nobare$`totalbiomass(g)`~treatment*field.no)
summary(fit_bioma_nobare_tb)


#spe_henan_notbare <- subset(spe_henan,spe_henan$functionalgroup != "bare")
spe_henan_base <- subset(spe_henan,spe_henan$functionalgroup == "base")
spe_henan_Ligularia_sp <- subset(spe_henan,spe_henan$functionalgroup == "Ligularia_sp")
spe_henan_Oxytropis_sp <- subset(spe_henan,spe_henan$functionalgroup == "Oxytropis_sp")
spe_henan_bare <- subset(spe_henan,spe_henan$functionalgroup == "bare")

#str(spe_henan_notbare)#382*106
str(spe_henan_base)#252
str(spe_henan_Ligularia_sp)#119
str(spe_henan_Oxytropis_sp)#11

#spe_henan_notbare <- rbind(spe_henan_base,spe_henan_Ligularia_sp,spe_henan_Oxytropis_sp)
str(spe_henan_bare)#84*106
str(spe_henan)#467*106, one row is NA.
View(spe_henan_notbare)#382*106
# Be cautious about the $ID column.

#first rank the average value of coverage
#then make a boxplot
#I can use some common methods in microbiological analysis.
#Turn all data NA to 0
boxplot(spe_henan_Ligularia_sp[8:106])
boxplot(spe_henan_Oxytropis_sp[8:106])
boxplot(spe_henan_base[8:106])
boxplot(spe_henan_bare[8:106])

#summary(colnames(spe_henan_notbare)[8:106] == colnames(spe_henan_notbare_NA)[1:99])
#summary(rownames(spe_henan_notbare)[1:382] == rownames(spe_henan_notbare_NA)[1:382])

#spe_henan_notbare_NA has NA in data set.
#spe_henan_notbare is without NA in data set.
#spe_henan_notbare_data is pure data without other field information.
spe_henan_notbare_NA <- rbind(spe_henan_base,spe_henan_Ligularia_sp,spe_henan_Oxytropis_sp)

spe_henan_notbare_data <- spe_henan_notbare_NA[8:106]
spe_henan_notbare_data [is.na(spe_henan_notbare_data[1:99])==TRUE] <- 0

spe_henan_notbare[8:106] <-spe_henan_notbare_data[1:99]
apply(spe_henan_notbare,2,range)# The range of data in each column.

#[Ranked frequency of Occurrence of each species]
#Target_ the usage of table() function.
#how many NA data, how many not NA data.
#rank the frequency
#make a barplot

#calculate occurrence frequency of each species
#NA to 0, occur to 1


#[Ranked coverage of each species]
#[Two way anova of biomass with statistic information]

#fractions() #ref. packages: MASS
#sqrt()
#^

#转置t()
#整合数据 aggregate()
spe_sub<-colnames(spe_henan_notbare_NA)[c(2:4,8:106)]
spe<-subset(spe_henan_notbare,select = spe_sub)
str(spe)
#aggregate()

#traceback()
#spe[row,col],spe[m=row,n=col]
rm(spe_occur)
#spe_new<-spe[,4:102]
#spe_t<-t(spe_new)
spe_col<-c(rep(x,99))
spe_num<-c(rep(1,99))
spe_occur<-data.frame(spe_col,spe_num)

##############################################
#iteration
##############################################
func_occur<-function(x,m,n,sum_occur=0){
  for(n in 4:ncol(x)){
    for(m in 1:nrow(x)){
      if(x[m,n]!=0){
        sum_occur<-sum_occur+1
        m<-m+1}
      else{
        sum_occur<-sum_occur
        m<-m+1}
  }
    spe_occur[n-3,2]<-sum_occur/382*100
    spe_occur[n-3,1]<-colnames(spe)[n]
    sum_occur<-0
    n=n+1
  }
  return(spe_occur)
}
#####################################################
func_occur_NA<-function(x,m,n,sum_occur=0){
  for(n in 8:ncol(x)){
    for(m in 1:nrow(x)){
      if(is.na(x[m,n]) ==TRUE){
        sum_occur<-sum_occur
        m<-m+1}
      else{
        sum_occur<-sum_occur+1
        m<-m+1}
    }
    spe_occur[n-7,2]<-sum_occur/382*100
    spe_occur[n-7,1]<-colnames(spe)[n-4]
    sum_occur<-0
    n=n+1
  }
  return(spe_occur)
}

#rm(spe_col)
#rm(spe_num)
spe_fre<-func_occur(spe,1,4)
spe_fre_haveNA<-func_occur_NA(spe_henan_notbare_NA,1,8)
# 100-sum(spe$Oxytropis_ochrocephala==0)/382*100
spe_fre_order<-spe_fre[order(spe_fre[,2],decreasing = T),]
spe_fre_haveNA_order<-spe_fre_haveNA[order(spe_fre_haveNA[,2],decreasing = T),]

#barplot(spe_order)

#trim=.2

spe_cov_ave<-apply(spe[4:102],2,mean)#print format
# R in Action 3.4.6 数字标注 
# Next goal: ggplot2
spe_cov_se<-apply(spe[4:102],2,std.error)
#ls()
head(spe_cov_ave)

spe_name<-colnames(spe)[4:102]
#Succeed
spe_covave<-data.frame(spe_cov_ave,spe_name,spe_cov_se)
summary(spe_cov_ave==spe_covave$spe_cov_ave)
summary(names(spe_cov_ave)==spe_covave$spe_name)
#options(scipen = 200)
#View(spe_covave)
#warnings()
spe_covave_order<-spe_covave[order(spe_covave[,1],decreasing = T),]



write.csv(spe_covave_order, file = "spe_covave_order.csv",row.names = TRUE)

write.csv(spe_fre_order,file = "spe_fre_order.csv",row.names = TRUE)


#ggplot2 practice
# ggplot(mpg, aes(hwy, cty)) +  geom_point(aes(color = cyl))+ geom_smooth(method="lm") +coord_cartesian()+scale_color_gradient()+theme_bw()


















