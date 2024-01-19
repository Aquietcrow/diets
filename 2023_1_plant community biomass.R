# install.packages("RODBC") #Use MS access
# install.packages("ade4") #Exploratory and Euclidean Methods in Environmental Sciences. Tools for multivariate data analysis.Monte-Carlo Test
# install.packages("vegan") #Community Ecology Package. Ordination methods, diversity analysis and other functions for community and vegetation ecologists.
# install.packages("reshape2") #Flexibly restructure and aggregate data using just two functions: melt and 'dcast' (or 'acast').
# install.packages("ggplot2")
# install.packages("lattice") #The lattice add-on package is an implementation of Trellis graphics for R
# install.packages("plotrix") #Various Plotting Functions ... Lots of plots, various labeling, axis and color scaling functions.
# install.packages("lubridate") #time or date related,"Ops.POSIXt", "Ops.Date"
# install.packages("purrr") #for working with functions and vectors
# install.packages("sqldf") #running SQL statements on R data frames, optimized for convenience. sqldf works with the SQLite, H2, PostgreSQL
# install.packages("car") #qqPlot; an acronym for Companion to Applied Regression
# install.packages("tidyverse") #The 'tidyverse' is a set of packages that work in harmony because they share common data representations and 'API' design.
# install.packages("stringr") #for strings
# install.packages("dplyr") #a grammar of data manipulation, providing a consistent set of verbs that solve the most common data manipulation challenges
# install.packages("dendextend") #extending 'dendrogram' objects in R, letting you visualize and compare trees of 'hierarchical clusterings'. Heatmaps.
# install.packages("moments") #to analyze skewness,in order to transform data to normal distribution.
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
library("dplyr")
library("moments")

SummerHenan2021_22 <- odbcConnectAccess2007("E:/Access/2021_summer_henan.accdb")
Biomas2021_22 <- sqlFetch(SummerHenan2021_22,"2021_summer_henan_biomass")

######################################################################
#Biomass analysis                                                    #
######################################################################
#I have failed the cooperation with herders in Field 42 and Field 43.#     
######################################################################

FailedfieldNo1 <- grep("42c",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNo2 <- grep("42t",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNo3 <- grep("43c",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNo4 <- grep("43t",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNoall <- c(FailedfieldNo1,FailedfieldNo2,FailedfieldNo3,FailedfieldNo4)

Biomas2021_22 <- Biomas2021_22[-FailedfieldNoall,]
##################################################################
# subplot_inform <- SpeCov2021_dele_failedfields[2:4] #Extract the subplot functional group information from 2021's data.
subplot_inform <- read.csv("subplot_inform_20230215.csv",header = TRUE)
str(grepl("c",subplot_inform$fieldcode))
subplot_inform$treatment <- grepl("c",subplot_inform$fieldcode)
#control flow
subplot_inform$treatment[subplot_inform$treatment == TRUE] <- "c"
subplot_inform$treatment[subplot_inform$treatment == FALSE] <- "t"
subplot_inform$field.no <- substring(subplot_inform$fieldcode,1,nchar(subplot_inform$fieldcode) - 1)
subplot_inform$treatment_code[subplot_inform$treatment == "c"] <- 0
subplot_inform$treatment_code[subplot_inform$treatment == "t"] <- 1

field.no2code_func<-function(x,y,h,n,m){
  for (n in 1:nrow(x)){
    for(m in 1:nrow(y)){
      if(x[n,h]==y[m,1]){
        x$code[n]<-y[m,2]}
      else{m<-m+1}}
    n<-n+1}
  return(x)}

subplot_inform_20230221 <- field.no2code_func(subplot_inform,field.no2code,5,1,1)

write.csv(subplot_inform_20230221,file = "subplot_inform_20230221.csv",row.names = TRUE)


Biomas2021_22_mergedsubplot <- merge(Biomas2021_22,subplot_inform_20230221,by = "subplot") 

names(Biomas2021_22_mergedsubplot)[16] <- "functionalgroup"
names(Biomas2021_22_mergedsubplot)[15] <- "fieldcode"

Biomas2021_22_full <- Biomas2021_22_mergedsubplot[,c(-2,-3,-4,-6)]
names(Biomas2021_22_full)[4:9]
####################################
# The outline of my data analysis ##
####################################

# replace NA data with 0.(done)
# subset different types of plot, e.g. "bare","base","Ligularia_sp","Oxytropis_sp"
# get the Q-Q plot of all data, to test the normality of data.
# mean and sd.
# aov one-way anova, analysis the variance in diffeent treatment or different experimental sites.
# two-way anova,analysis the variance in different treatment, different experimental sites, and their interactions.
# Moldy samples were labeled as integer "1".

reNaT0 <- function(x,na.omit=FALSE)
{for( n in 1:nrow(x))
{for( m in 4:9)
{if (is.na(x[n,m])==TRUE)
{x[n,m]<- 0}
  else
  {m<-m+1}}
  n<-n+1} 
  result<-x
  return(result)}

Biomas2021_22_full_0 <- reNaT0(Biomas2021_22_full)
head(Biomas2021_22_full_0[,4:9])

Biomas2021_22_reg <- subset(Biomas2021_22_full_0,Biomas2021_22_full_0$functionalgroup != "bare")# this chooses other description,such as,Ligularia_sp
##############################################################
#grep() and grepl() are arguments can search certain string for target string.
#str(), summary()
#nchar(bioma_nobare$fieldcode[1])
##################
#deal with missing data 20230220
##################
#1c-4q-1;1c-4q-2 sedge [2021]
write.csv(e1,file = "e1_20230221.csv",row.names = TRUE)
e1 <- read.csv("E:/r_data_dit/e1_20230221.csv",header = TRUE)
f1 <- e1[101:106,6]
# f1 <- c(0.00, 0.13, 0.25, 0.19, 0.00, 0.42)
g1 <- round(mean(f1), digits = 2)
e1[107:108,6] <- c(g1,g1)#0.16g

#1t-3q-2 sedge [2021]
f2 <- c(e1[109:113,6],e1[115:116,6])
# f2<- c(0.20, 0.09, 0.49, 0.49, 0.53, 0.00, 0.28)
g2 <- round(mean(f2), digits = 2)
e1[114,6] <- g2#0.3

#10-2c-4q-1 sedge [2021]
f3 <- c(e1[1:6,6],e1[8,6])
# f3 <-c(1.60, 4.01, 3.78, 2.54, 3.16, 1.31, 0.46)
g3 <- round(mean(f3),digits = 2)
e1[7,6] <- g3#2.41g

#11-4t-1q-3 sedge [2021]
f4 <- c(e1[42,6],e1[44:49,6])
# f4 <- c(0.54, 3.67, 0.55, 0.59, 0.24, 0.46, 0.27)
g4 <- round(mean(f4),digits = 2)
e1[43,6] <- g4#0.9g

#12c-4q-4 sedge [2021]
f5 <- e1[50:56,6]
# f5 <-c(3.31, 1.28, 3.35, 0.22, 0.00, 0.54, 0.00)
g5 <- round(mean(f5),digits = 2)
e1[57,6] <- g5#1.24g

#41t-2q-2 grass [2021]
f6 <- c(e1[162:163,5],e1[165:166,5])
#f6 <- c(5.58, 9.46, 3.93, 4.44)
g6 <- round(mean(f6),digits = 2)
e1[164,5] <- g6 #5.85g

#41t-4q-2 sedge [2021]
f7 <- e1[162:165,6]
#f7<-c(1.98, 1.26, 0.27, 1.39)
g7 <- round(mean(f7),digits = 2)
e1[166,6] <- g7#1.23g

#12c-3q-2 sedge [2022]
f8 <- c(e1[50:53,20],e1[55:57,20])
#f8<-c(2.92, 7.24, 6.02, 4.63, 3.34, 0.08, 0.26)
g8 <- round(mean(f8), digits = 2)
e1[54,20] <- g8 #(3.5g)
h1 <- c(g1,g2,g3,g4,g5,g6,g7,g8)

# x1 <- grep("1c-4q-1",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[344,5] <- 0.16
# x2 <- grep("1c-4q-2",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[346,5] <- 0.16
# x3 <- grep("1t-3q-2",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[357,5] <- 0.3
# x4 <- grep("10-2c-4q-1",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[20,5] <- 2.41
# x5 <- grep("11-4t-1q-3",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[125,5] <- 0.9
# x6 <- grep("12c-4q-4",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[168,5] <- 1.24
# x7 <- grep("41t-2q-2",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[490,4] <- 5.85
# x8 <- grep("41t-4q-2",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[498,5] <- 1.23
# x9 <- grep("12c-3q-2",Biomas2021_22_reg$subplot,fixed = TRUE)
Biomas2021_22_reg[159,5] <- 3.5


#one way
# mylist <- names(Biomas2021_22_reg)[-c(4:9)]
# Biomas2021_22_x <- melt(Biomas2021_22_reg,id = mylist)
# names(Biomas2021_22_x)[10:11] <- c("functionalgroup_subplot","Biomass")
# 
write.csv(Biomas2021_22_reg,file = "Biomas2021_22_reg_20230221.csv",row.names = TRUE)
z1 <- aggregate(Biomas2021_22_reg,by = list(Biomas2021_22_reg$time,Biomas2021_22_reg$code,Biomas2021_22_reg$treatment_code,Biomas2021_22_reg$functionalgroup),FUN = mean)
names(z1)[1:4] <- c("time","code","treatment_code","functionalgroup")
Biomas2021_22_avera <- z1[,c(1:4,8:13)]
Biomas2021_22_avera$` total_biomass(g)` <- Biomas2021_22_avera$`grass(g)` + Biomas2021_22_avera$`sedge(g)` + Biomas2021_22_avera$`legume(g)` + Biomas2021_22_avera$`forb(g)`+Biomas2021_22_avera$`poisonousplants(g)`
c1 <- subset(c, F$functionalgroup == "base")
c2 <- subset(Biomas2021_22_avera, Biomas2021_22_avera$functionalgroup == "Ligularia_sp")
c3 <- subset(Biomas2021_22_avera, Biomas2021_22_avera$functionalgroup == "Oxytropis_sp")
c1_2021 <- subset(c1,c1$time == 2021)
c1_2022 <- subset(c1,c1$time == 2022)

d1 <- merge(c1_2021,c1_2022,by = c("code","treatment_code"))

##################################################
c2_2021 <- subset(c2,c2$time == 2021)
c2_2022 <- subset(c2,c2$time == 2022)
d2 <- merge(c2_2021,c2_2022, by = c("code","treatment_code"))

c3_2021 <- subset(c3,c3$time == 2021)
c3_2022 <- subset(c3,c3$time == 2022)
d3 <- merge(c3_2021,c3_2022, by = c("code","treatment_code"))

Biomas2021_22_avera_year <- rbind(d1,d2,d3)

Biomas2021_22_avera_year$change_of_grass_biomass_percentage <- (Biomas2021_22_avera_year$`grass(g).y`/Biomas2021_22_avera_year$` total_biomass(g).y` - Biomas2021_22_avera_year$`grass(g).x`/Biomas2021_22_avera_year$` total_biomass(g).x`)*100

Biomas2021_22_avera_year$change_of_sedge_biomass_percentage <- (Biomas2021_22_avera_year$`sedge(g).y`/Biomas2021_22_avera_year$` total_biomass(g).y` - Biomas2021_22_avera_year$`sedge(g).x`/Biomas2021_22_avera_year$` total_biomass(g).x`)*100

Biomas2021_22_avera_year$change_of_legume_biomass_percentage <- (Biomas2021_22_avera_year$`legume(g).y`/Biomas2021_22_avera_year$` total_biomass(g).y` - Biomas2021_22_avera_year$`legume(g).x`/ Biomas2021_22_avera_year$` total_biomass(g).x`)*100

Biomas2021_22_avera_year$change_of_forb_biomass_percentage <- (Biomas2021_22_avera_year$`forb(g).y`/Biomas2021_22_avera_year$` total_biomass(g).y` - Biomas2021_22_avera_year$`forb(g).x`/Biomas2021_22_avera_year$` total_biomass(g).x`)*100

Biomas2021_22_avera_year$change_of_poisonousplants_biomass_percentage <- (Biomas2021_22_avera_year$`poisonousplants(g).y`/Biomas2021_22_avera_year$` total_biomass(g).y` - Biomas2021_22_avera_year$`poisonousplants(g).x`/Biomas2021_22_avera_year$` total_biomass(g).x`)*100

# Biomas2021_22_avera_year$change_of_litter_biomass_percentage <- (Biomas2021_22_avera_year$`litter(g).y` - Biomas2021_22_avera_year$`litter(g).x`)/Biomas2021_22_avera_year$`litter(g).x`*100
# Biomas2021_22_avera_year$change_of_total_biomass_percentage <- (Biomas2021_22_avera_year$` total_biomass(g).y` - Biomas2021_22_avera_year$` total_biomass(g).x`)/Biomas2021_22_avera_year$` total_biomass(g).x`*100

#grass(g).y is 2022's data; grass(g).x is 2021's data.

# e1$total_biomass_2021 <- (e1$`grass(g).x` + e1$`sedge(g).x` + e1$`legume(g).x` + e1$`forb(g).x` + e1$`poisonousplants(g).x`)
# e1$total_biomass_2022 <- (e1$`grass(g).y` + e1$`sedge(g).y` + e1$`legume(g).y` + e1$`forb(g).y` + e1$`poisonousplants(g).y`)
# 
# The extreme value in the percentage change data.
Biomas2021_22_avera_year[c(9,3,1,12,11),21]#"change_of_poisonousplants_biomass_percentage"
Biomas2021_22_avera_year[c(50,19,48,10,47),24]#"change_of_legume_biomass_percentage"
Biomas2021_22_avera_year[c(66,40,9,10,12),26]#"change_of_litter_biomass_percentage"

names(Biomas2021_22_avera_year)[c(5:11,14:20)] <- c("grass(g).2021","sedge(g).2021","legume(g).2021","forb(g).2021","poisonousplants(g).2021","litter(g).2021","total_biomass(g).2021","grass(g).2022","sedge(g).2022","legume(g).2022","forb(g).2022","poisonousplants(g).2022","litter(g).2022","total_biomass(g).2022")

write.csv(e1,file = "Biomas2021_22 20230221.csv",row.names = TRUE)
write.csv(h1,file = "Calculated missing value.csv",row.names = TRUE)
write.csv(Biomas2021_22_avera_year,file = "Biomas2021_22_avera_year_20230221.csv",row.names = TRUE)

Biomas2021_22_avera_year$treatment_code <- as.factor(Biomas2021_22_avera_year$treatment_code)
Biomas2021_22_avera_year$code <- as.factor(Biomas2021_22_avera_year$code)
attach(Biomas2021_22_avera_year)


z1_normality <- apply(Biomas2021_22_avera_year[,c(5:11,14:25)],2,shapiro.test)
# ggqqplot(Biomas2021_22_avera_year$`legume(g).2021`)
apply(d1[5:11], 2, shapiro.test)
apply(d2[5:11], 2, shapiro.test)
apply(d3[5:11], 2, shapiro.test)

#https://www.researchgate.net/post/How_do_you_transform_a_non-normal_set_of_data_into_a_normal_distribution
#https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/
x2 <- c(5:11,14:20) #Raw data region.
x1 <- c(5,7,8,9,14,16,18,19) #non-normal distribution data.
skewness(Biomas2021_22_avera_year[,5],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,7],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,8],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,9],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,14],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,16],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,18],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,19],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,22],na.rm = TRUE)#Positively skewed distribution
skewness(Biomas2021_22_avera_year[,23],na.rm = TRUE)# -0.08345746
skewness(Biomas2021_22_avera_year[,25],na.rm = TRUE)# -1.560733
names(Biomas2021_22_avera_year)[19]
names(Biomas2021_22_avera_year)[x1]

Biomas2021_22_avera_year$`grass(g).2021.ln(x+1)` <- log10(Biomas2021_22_avera_year[,5] + 1)
Biomas2021_22_avera_year$`legume(g).2021.ln(x+1)` <- log10(Biomas2021_22_avera_year[,7] + 1)
# apply(Biomas2021_22_avera_year[27],2, shapiro.test) #ln( x + 1 ) transformed legume(g).x does not follow normal distribution.
Biomas2021_22_avera_year$`forb(g).2021.ln(x+1)` <- log10(Biomas2021_22_avera_year[,8] + 1)
Biomas2021_22_avera_year$`poisonousplants(g).2021.ln(x+1)` <- log10(Biomas2021_22_avera_year[,9] + 1)
# shapiro.test(Biomas2021_22_avera_year[,29])#ln( x + 1 ) transformed poisonousplants(g).x does not follow normal distribution.
Biomas2021_22_avera_year$`grass(g).2022.ln(x+1)` <- log10(Biomas2021_22_avera_year[,14] + 1)
Biomas2021_22_avera_year$`legume(g).2022.ln(x+1)` <- log10(Biomas2021_22_avera_year[,16] + 1) # ln( x + 1 ) transformed legume(g).y does not follow normal distribution.
Biomas2021_22_avera_year$`poisonousplants(g).2022.ln(x+1)` <- log10(Biomas2021_22_avera_year[,18] + 1) #ln( x + 1 ) transformed poisonousplants(g).y does not follow normal distribution.
Biomas2021_22_avera_year$`litter(g).2022.ln(x+1)` <- log10(Biomas2021_22_avera_year[,19] + 1)
apply(Biomas2021_22_avera_year[26:33],2, shapiro.test)
write.csv(Biomas2021_22_avera_year,file = "Biomas2021_22_avera_year_20230222.csv",row.names = TRUE)










































