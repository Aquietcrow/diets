# install.packages("RODBC")# Use MS access
# install.packages("ade4") #Exploratory and Euclidean Methods in Environmental Sciences. Tools for multivariate data analysis.Monte-Carlo Test
# install.packages("vegan") #Community Ecology Package. Ordination methods, diversity analysis and other functions for community and vegetation ecologists.
# install.packages("reshape2") #Flexibly restructure and aggregate data using just two functions: melt and 'dcast' (or 'acast').
# install.packages("ggplot2")
# install.packages("lattice") #The lattice add-on package is an implementation of Trellis graphics for R
# install.packages("plotrix") #Various Plotting Functions ... Lots of plots, various labeling, axis and color scaling functions.
install.packages("lubridate")#time or date related,"Ops.POSIXt", "Ops.Date"
# install.packages("purrr")#for working with functions and vectors
# install.packages("sqldf")# running SQL statements on R data frames, optimized for convenience. sqldf works with the SQLite, H2, PostgreSQL
# install.packages("car")#qqPlot; an acronym for Companion to Applied Regression
# install.packages("tidyverse")#The 'tidyverse' is a set of packages that work in harmony because they share common data representations and 'API' design.
install.packages("stringr")#for strings
install.packages("dplyr")#a grammar of data manipulation, providing a consistent set of verbs that solve the most common data manipulation challenges
# install.packages("dendextend") #extending 'dendrogram' objects in R, letting you visualize and compare trees of 'hierarchical clusterings'. Heatmaps.
# install.packages("vioplot") #Violin plot, a boxplot combined with density of data.
# install.packages("ggpubr")
# install.packages("Rmisc")#arrange graphs.
install.packages("openxlsx")
# [ctrl + shift + c] can make notes
.libPaths()
# "C:/Users/oh_my/AppData/Local/R/win-library/4.2"
# "C:/Program Files/R/R-4.2.2/library" 
tempdir()
# [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
dir.create(tempdir())
getwd()
setwd("E:/r_data_dit")
#load library
library("RODBC")
library("lubridate")
library("tidyr")
library("stringr")
library("openxlsx")
library("dplyr")
library("plotrix")
###############################################################
#Import data from MS Access
###############################################################
SummerHenan <- odbcConnectAccess2007("E:/Access/2021_summer_henan_20231115.accdb")
SpeCov <- sqlFetch(SummerHenan,"2021_summer_henan_plotcover")
SpeBio <- sqlFetch(SummerHenan,'2021_summer_henan_biomass')
spe_plantnames <- sqlFetch(SummerHenan,"2021_summer_henan_speciesname")
Zokor_2021 <- sqlFetch(SummerHenan,"2021_summer_henan_sitesinformation")

# garbage collection
#  closing unused RODBC handle
odbcCloseAll()
spe_plantnames_rightorder <- spe_plantnames[order(spe_plantnames[,1]),]
# replace the Chinese with Latin name
head(spe_plantnames_rightorder)
write.xlsx(spe_plantnames_rightorder,file = "spe_plantnames_rightorder20231204.xlsx", rownames = FALSE)
colnames(SpeCov)[8:119] <- spe_plantnames_rightorder$Latinname

date1 <- as.Date('2022-01-01')
date2 <- as.Date('2023-01-01')
#"Ops.POSIXt", "Ops.Date"
#Failedfield<-c("42c","42t","43c","43t")

# Next time try to use "apply" function
SpeCov$time_year[SpeCov$time < date1] <- 2021
SpeCov$time_year[SpeCov$time > date1 & SpeCov$time < date2] <- 2022
SpeCov$time_year[SpeCov$time > date2] <- 2023 

colnames(SpeCov)[8:119]
# SpeCov[,9:119] == 0

Replace_RarespeciesValue <- function(X,na.omit=TRUE)
  {for (n in 1:nrow(X))
    {for (m in 8:119)
      {if (is.na(X[n,m]) == TRUE)
        {m <- m + 1}
      else if (X[n,m] == 0) {
        X[n,m] <- 0.001;
        m <- m + 1
        }
      else
      {m <- m + 1}}
    n < n + 1}
  return(X)}

SpeCov1 <- Replace_RarespeciesValue(SpeCov)
# write.xlsx(SpeCov1, file = "henan_SpeCov20231121.xlsx",rowNames = FALSE)	

Replace_NA <- function(X,X_row,X_colstart,X_colend,na.omit = FALSE){
  for (n in X_row:nrow(X)) {
    for (m in X_colstart:X_colend) {
      if (is.na(X[n,m]) == TRUE){
        X[n,m] <- 0;
        m <- m + 1
      }
      else{
        m <- m + 1
      }
    }
  n <- n + 1}
  return(X)
}

SpeCov2 <- Replace_NA(SpeCov1,X_row = 1, X_colstart = 8, X_colend = 119)

# write.xlsx(SpeCov2, file = "henan_SpeCov20231121_2.xlsx",rowNames = FALSE)	

SpeCov <- SpeCov2

SpeCov$fieldcode <- str_replace(SpeCov$fieldcode, pattern = "t", replacement = " t")
SpeCov$fieldcode <- str_replace(SpeCov$fieldcode, pattern = "c", replacement = " c")

SpeCov$subplot_1 <- str_replace(SpeCov$subplot, pattern = "t", replacement = " t")
SpeCov$subplot_1 <- str_replace(SpeCov$subplot_1, pattern = "c", replacement = " c")

SpeCov[c("field","field_1")] <- str_split_fixed(SpeCov$subplot_1,pattern = " ", n = 2)
SpeCov[c("treatment","region_q","quadrat")] <- str_split_fixed(SpeCov$field_1, pattern = "-", n = 3)

SpeCov$field.no <- as.numeric(factor(SpeCov$field))

subplot_inform <- read.csv("E:/r_data_dit/subplot_inform_20230215.csv",header = TRUE)
SpeCov <- merge(SpeCov,subplot_inform,by = "subplot") 

#########################To be continued 2023 12 7 #############################

####################################################################
####################################################################
colnames(SpeBio)[7] <- "time_year"
colnames(SpeBio)[8:13]




SpeBio1 <- Replace_NA(SpeBio,X_row = 1, X_colstart = 8, X_colend = 13)
# write.xlsx(SpeBio1, file = "henan_SpeBio20231121.xlsx",rowNames = FALSE)
SpeBio <- SpeBio1

SpeBio$time_year <- as.numeric(SpeBio$time_year)
X1 <- merge(SpeCov,SpeBio, by = c("time_year", "subplot"), all.x = TRUE)

########################################################################
# Analysis on abundance 20231130
########################################################################

c1 <- grep("subplot",colnames(X1))[1]
c1
c2 <- grep("time_year",colnames(X1))
c2
c3 <- grep("treatment.x",colnames(X1))
c3
c4 <- grep("field.no.x",colnames(X1))
c4
c5 <- grep("functionalgroup.y",colnames(X1))
c5
c6_1 <- grep("Ligularia_virgaurea",colnames(X1))
c6_1
c6_2 <- grep("Anemone_rivularis var. flore-minore",colnames(X1))
c6_2
c7 <- grep("region_q",colnames(X1))
c7
c8 <- grep("quadrat",colnames(X1))
c8
c9 <- grep("grazing_regime",colnames(X1))
c9

Abundance_all <- X1[,c(c1,c2,c3,c4,c5,c7,c8,c9,c6_1:c6_2)]
colnames(Abundance_all)
#######################################################################################
# Deal with duplicated columns.
#######################################################################################
# write.xlsx(Abundance_all, file = "Abundance_all20231130.xlsx",rowNames = FALSE)

d1 <- grep("Brassicaceae",colnames(Abundance_all))
d1
Abundance_all$`Brassicaceae_sp` <- Abundance_all[,d1[1]] + Abundance_all[,d1[2]]

# Halenia_corniculata column has no value.
d2_2 <- grep("Halenia_corniculata", colnames(Abundance_all))

#
d3 <- grep("Artemisia",colnames(Abundance_all))
colnames(Abundance_all)[d3]
Abundance_all$Artemisia_sp <- Abundance_all[,d3[1]] + Abundance_all[,d3[2]] + Abundance_all[,d3[4]]


d4_1 <- grep("Gentiana_aristata",colnames(Abundance_all))
d4_2 <- grep("Gentiana_sp_unknown",colnames(Abundance_all))
Abundance_all$Gentiana_aristata_ <- Abundance_all[,d4_1] + Abundance_all[,d4_2] 


d5_1 <- grep("Ligusticum_thomsonii",colnames(Abundance_all))
d5_2 <- grep("Semenovia_malcolmii",colnames(Abundance_all) )
Abundance_all$`Semenovia_malcolmii_` <- Abundance_all[,d5_1] + Abundance_all[,d5_2]

# This two species are okay.
d6_1 <- grep("Sphallerocarpus_gracilis",colnames(Abundance_all))
d6_2 <- grep("Carum_carvi",colnames(Abundance_all))


d7_1 <- grep("Carex_sp_unknown",colnames(Abundance_all))
d7_2 <- grep("Carex_atrofusca", colnames(Abundance_all))
Abundance_all$`Carex_sp` <- Abundance_all[,d7_1] + Abundance_all[,d7_2]

d8_1 <- grep("Carduus_nutans",colnames(Abundance_all))
d8_2 <- grep("Cirsium_souliei",colnames(Abundance_all))

Abundance_all$`Cirsium_souliei_` <- Abundance_all[,d8_1] + Abundance_all[,d8_2]

Abundance_all <- Abundance_all[,-c(d1,d2_2,d3[1],d3[2],d3[4],d4_1,d4_2,d5_1,d5_2,d7_1,d7_2,d8_1,d8_2)]
write.xlsx(Abundance_all, file = "Abundance_all20231130_1.xlsx",rowNames = FALSE)


####################################################
#shooting the error data
#2023-7t-1q-2 Poa_araratica 0.04
#2023-11-4c-3q-1 Thlaspi_arvense 0.07
#Lagotis_brachystachya has no value
#Smelowskia_tibetica has no value

e1_1 <- grep("7t-1q-2",Abundance_all$subplot)
e1_1
e1_2 <- grep("2023",Abundance_all$time_year)
e1_2
e1_3 <- intersect(e1_1,e1_2)
e1_3
e1_4 <- grep("Poa_araratica",colnames(Abundance_all))
e1_4
Abundance_all[e1_3[2],e1_4] <- 0.04

e2_1 <- grep("11-4c-3q-1",Abundance_all$subplot)
e2_1
e2_2 <- grep("2023",Abundance_all$time_year)
e2_2
e2_3 <- intersect(e2_1,e2_2)
e2_3
e2_4 <- grep("Thlaspi_arvense",colnames(Abundance_all))
e2_4
Abundance_all[e2_3[2],e2_4] <- 0.07

f1_1 <- grep("Lagotis_brachystachya",colnames(Abundance_all))
f1_2 <- grep("Smelowskia_tibetica",colnames(Abundance_all))

g1_1 <- grep("bareland",colnames(Abundance_all))
g1_1
g1_2 <- grep("yak_dung",colnames(Abundance_all))
g1_2

Abundance_all1 <- Abundance_all[,-c(f1_1,f1_2,g1_1,g1_2)]

write.xlsx(Abundance_all1, file = "Abundance_all20231204.xlsx",rowNames = FALSE)

# save(X1,file = 'E:/r_data_dit/henan_SpeCov_SpeBio.Rdata')  
# load('E:/r_data_dit/henan_SpeCov_SpeBio.Rdata')
spe_plantnames_rightorder$Latinname
h2 <- colnames(Abundance_all1)

intersect(colnames(Abundance_all1),spe_plantnames_rightorder$Latinname)
intersect(spe_plantnames_rightorder$Latinname,colnames(Abundance_all1))
str(setdiff(spe_plantnames_rightorder$Latinname,colnames(Abundance_all1)))
setdiff(colnames(Abundance_all1),spe_plantnames_rightorder$Latinname)

amend_sp <- data.frame(Latinname = c("Brassicaceae_sp","Artemisia_sp","Gentiana_aristata_","Semenovia_malcolmii_","Carex_sp","Cirsium_souliei_"), functional_group = c("forb","Unpalatable_forb","Unpalatable_forb","forb","sedge","Unpalatable_forb"))
# it's 100 species in total.

h1 <- grep("Unpalatable_forb",spe_plantnames_rightorder$functional_group)
sp_unpalatable <- spe_plantnames_rightorder$Latinname[h1]
sp_unpalatable[12]
sp_unpalatable[18]
sp_unpalatable[19]
sp_unpalatable[30]
sp_unpalatable[32]

mygrep_col <- function(x,y,z){
  for (i in 1 : length(x)) {
    if (length(grep(x[i],colnames(y),fixed = TRUE)) == 0) {
      z[i] <- 0
      i <- i + 1
    }
    else {
    z[i] <- grep(x[i],colnames(y),fixed = TRUE)
    i <- i + 1}
  }
  return(z)
}

print(mygrep_col)

list <- c(1:length(sp_unpalatable))
mylist <- mygrep_col(x = sp_unpalatable,y = Abundance_all1,z = list)
mylist <- subset(mylist,mylist != 0)
#######################################To be continued 20231201##########################################
Abundance_all1$`unpalatable_abundance` <- rowSums(Abundance_all1[,c(mylist,104)])

n1 <- setdiff(c(9:108),c(mylist,104))
Abundance_all1$`palatable_abundance` <- rowSums(Abundance_all1[,n1])
Abundance_all1$`total_abundance` <- rowSums(Abundance_all1[,c(9:108)])
Abundance_all1$`palatable_abundance%` <- Abundance_all1$palatable_abundance/Abundance_all1$total_abundance*100
Abundance_all1$`unpalatable_abundance%` <- Abundance_all1$unpalatable_abundance/Abundance_all1$total_abundance*100

write.xlsx(Abundance_all1,file = "Abundance_all20231205(2).xlsx",rowNames = FALSE)

mylist_base <- grep("base",Abundance_all1$functionalgroup.y,fixed = TRUE)
grep("unpalatable_abundance",colnames(Abundance_all1),fixed = TRUE)
grep("unpalatable_abundance%",colnames(Abundance_all1),fixed = TRUE)
henan_abundance <- Abundance_all1[mylist_base,c(1:8,109:113)]

henan_abundance$time_year <- as.factor(henan_abundance$time_year)
henan_abundance$treatment <- as.factor(henan_abundance$treatment.x)
henan_abundance$field.no <- as.factor(henan_abundance$field.no.x)


henan_abundance$grazing_regime <- as.factor(henan_abundance$grazing_regime)

henan_abundance$grazing_regime2 <- str_replace(henan_abundance$grazing_regime2, pattern = "summer+winter", replacement = "summer")

###################################################
start1 <- grep("unpalatable_abundance",colnames(henan_abundance))
start1
end1 <- grep("unpalatable_abundance%", colnames(henan_abundance)) 
end1

henan_abundance_field_mean <- aggregate(henan_abundance[,9:13],by = list(henan_abundance$time_year,henan_abundance$treatment,henan_abundance$field.no),mean)
henan_abundance_field_sd <- aggregate(henan_abundance[,9:13],by = list(henan_abundance$time_year,henan_abundance$treatment,henan_abundance$field.no),sd)

# think about the "grazing_regime" 
henan_abundance_field_mean1 <- aggregate(henan_abundance[,9:13], by = list(henan_abundance$time_year,henan_abundance$treatment,henan_abundance$grazing_regime,henan_abundance$field.no),mean)

write.xlsx(henan_abundance_field_mean1, file = "henan_abundance_field_mean1_20231207.xlsx",rowNames = FALSE)


# normal distribution for "henan_abundance_field_mean"
apply(henan_abundance_field_mean[4:8],2,shapiro.test)
#the proportion of palatable abundance is not significantly different from normal distribution in shapiro-wilk normality test. p > 0.05

henan_abundance_mean <- aggregate(henan_abundance_field_mean[4:8],by = list(henan_abundance_field_mean$Group.1,henan_abundance_field_mean$Group.2),mean)

henan_abundance_sd <- aggregate(henan_abundance_field_mean[4:8],by = list(henan_abundance_field_mean$Group.1,henan_abundance_field_mean$Group.2),sd)

henan_abundance_se <- aggregate(henan_abundance_field_mean[4:8],by = list(henan_abundance_field_mean$Group.1,henan_abundance_field_mean$Group.2),std.error)

# normal distribution for "henan_abundance_field_mean1"
apply(henan_abundance_field_mean1[5:9],2,shapiro.test)

henan_abundance_mean1 <- aggregate(henan_abundance_field_mean1[5:9], by = list(henan_abundance_field_mean1$Group.1,henan_abundance_field_mean1$Group.2,henan_abundance_field_mean1$Group.3),mean)

henan_abundance_se1 <- aggregate(henan_abundance_field_mean1[5:9], by = list(henan_abundance_field_mean1$Group.1,henan_abundance_field_mean1$Group.2,henan_abundance_field_mean1$Group.3),std.error)



write.xlsx(henan_abundance_field_mean,file = "henan_abundance_field_mean20231205.xlsx",rowNames = FALSE)
write.xlsx(henan_abundance_field_sd,file = "henan_abundance_field_sd20231205.xlsx",rowNames = FALSE)
write.xlsx(henan_abundance_mean,file = "henan_abundance_mean20231205.xlsx",rowNames = FALSE)
write.xlsx(henan_abundance_sd, file = "henan_abundance_sd20231205.xlsx", rowNames = FALSE)
write.xlsx(henan_abundance_se, file = "henan_abundance_se20231206.xlsx", rowNames = FALSE)

write.xlsx(henan_abundance_mean1,file = "henan_abundance_mean1_20231207.xlsx",rowNames = FALSE)
write.xlsx(henan_abundance_se1,file = "henan_abundance_se1_20231207.xlsx",rowNames = FALSE)
#########################################To be continue################################

b1 <- aov(henan_abundance_field_mean$unpalatable_abundance ~ henan_abundance_field_mean$Group.1 * henan_abundance_field_mean$Group.2)
summary(b1)
TukeyHSD(b1)

b2 <- aov(henan_abundance_field_mean$palatable_abundance ~ henan_abundance_field_mean$Group.1 * henan_abundance_field_mean$Group.2)
summary(b2)
TukeyHSD(b2)


b3 <- aov(henan_abundance_field_mean$`palatable_abundance%` ~ henan_abundance_field_mean$Group.1 * henan_abundance_field_mean$Group.2)
summary(b3)
TukeyHSD(b3)

b4 <- aov(henan_abundance_field_mean$`unpalatable_abundance%`~ henan_abundance_field_mean$Group.1 * henan_abundance_field_mean$Group.2)
summary(b4)
TukeyHSD(b4)

#####################################To be continued 20231207###############################################
b5 <- aov(henan_abundance_field_mean1$`palatable_abundance%`~henan_abundance_field_mean1$Group.1*henan_abundance_field_mean1$Group.3*henan_abundance_field_mean1$Group.2)
# Yes, grazing regime has an effect on palatable biomass. 

summary(b5)
TukeyHSD(b5)
#######################################################################################
ListOf_S2022 <- grep("Supplementary plot_2022",X1$Comments,fixed = TRUE)
X1[ListOf_S2022,1:7]
ListOf_S2023 <- grep("Supplementary plot_2023",X1$Comments,fixed = TRUE)
X1[ListOf_S2023,1:7]



# missing data in F13 [2023/11/22]
# 13c-1q-2 grass[2023]: 4.45g
# #######################################################################################
# x13c1q2 <- c(5.02 , 4.21, 2.75 , 1.73 , 4.75 , 7.34 , 4.46 , 2.68 ,0.94, 2.66,12.44)
# min(x13c1q2)
# max(x13c1q2)
# length(x13c1q2)
# sum(x13c1q2)/length(x13c1q2)
# 
# # 13c-3q-1 forb[2023]: 10.84g
# x13c3q1 <- c(6.43,7.31,9.02,13.44,16.13,20.18,6.68,11.99,5.76,7.73,14.53)
# min(x13c3q1)
# max(x13c3q1)
# sum(x13c3q1)/length(x13c3q1)

# 13c-3q-1 poisonous: 0g

bare_all <- grep("bare",X1$functionalgroup.y)
missing_55t2q3 <- grep("55-1t-2q-3",X1$subplot)
biomass_all <- X1[c(-bare_all,-missing_55t2q3),]
##########################################################################################
# 13c-1q-2 grass[2023]: 4.45g
n1 <- grep("13c-1q-2",biomass_all$subplot)
n2 <- grep("2023",biomass_all$time_year)
n3 <- grep("grass",colnames(biomass_all))

n4 <- intersect(n1,n2)
biomass_all[n4,n3] <- c(4.45,4.45)
############################################################################################
# 13c-3q-1 forb[2023]: 10.84g
n1 <- grep("13c-3q-1",biomass_all$subplot)
biomass_all[n1,]
n2 <- grep("2023",biomass_all$time_year)
n2
n3 <- grep("forb",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
n4
biomass_all[n4,n3] <- c(10.84)
############################################################################################
#12c-3q-2 sedge [2022]: 3.5g

n1 <- grep("12c-3q-2",biomass_all$subplot)
biomass_all[n1,]
n2 <- grep("2022",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
n4
biomass_all[n4,n3] <-3.5
############################################################################################
#1c-4q-1;1c-4q-2 sedge [2021]: 0.16g, 0.16g
n11 <- grep("1c-4q-1",biomass_all$subplot)
n12 <- grep("1c-4q-2",biomass_all$subplot)
n1 <- union(n11,n12)
n1
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n4 <- intersect(c(172,173),n2)
n4
biomass_all[n4,n3] <- c(0.16,0.16)
############################################################################################
#1t-3q-2 sedge [2021]: 0.3g
n1 <- grep("1t-3q-2",biomass_all$subplot)
biomass_all[n1,1:5]
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n3
n4 <- intersect(c(179,534,911),n2)
n4
biomass_all[n4,n3] <- 0.3
############################################################################################
#10-2c-4q-1 sedge [2021]: 2.41g
n1 <- grep("10-2c-4q-1",biomass_all$subplot)
n1
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
biomass_all[n4,n3] <- 2.41


############################################################################################
#11-4t-1q-3 sedge [2021]: 0.9g
n1 <- grep("11-4t-1q-3",biomass_all$subplot)
n1
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
biomass_all[n4,n3] <- 0.9
############################################################################################
#12c-4q-4 sedge [2021]: 1.24g
n1 <- grep("12c-4q-4",biomass_all$subplot)
n1
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
biomass_all[n4,n3] <- 1.24

############################################################################################
#41t-2q-2 grass [2021]: 5.85g
n1 <- grep("41t-2q-2",biomass_all$subplot)
n1
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("grass",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
biomass_all[n4,n3] <- 5.85


############################################################################################
#41t-4q-2 sedge [2021]: 1.23g
n1 <- grep("41t-4q-2",biomass_all$subplot)
n1
n2 <- grep("2021",biomass_all$time_year)
n2
n3 <- grep("sedge",colnames(biomass_all))
n3
n4 <- intersect(n1,n2)
biomass_all[n4,n3] <- 1.23
############################################################################################
print("missing data finish")

biomass_all$totalforb <- biomass_all$`forb(g)` + biomass_all$`poisonousplants(g)`
biomass_all$totalbiomass <- biomass_all$`grass(g)` + biomass_all$`sedge(g)` + biomass_all$`legume(g)` + biomass_all$`forb(g)` + biomass_all$`poisonousplants(g)`
biomass_all$graminoid <- biomass_all$`grass(g)`+ biomass_all$`sedge(g)`
write.xlsx(biomass_all, file = "henan_SpeCov_SpeBio20231207.xlsx",rowNames = FALSE)

# rm(a13,a13_mean,biomass_all_mean,biomass_all_sd,bio_mean_without_13,bio_without_a13)

# PatchCo2021 <- read.xlsx("PatchCo2021Per_20231129.xlsx")
############################################################################################
############################################################################################
############################################################################################
# I want to regroup the result and analysis the anova.
biomass_all$functionalgroup.y

m1 <- grep("summer",biomass_all$grazing_regime)
biomass_all$grazing_regime1 <- biomass_all$grazing_regime
biomass_all$grazing_regime1[m1] <- "summer"




biomass_all$treatment <- as.factor(biomass_all$treatment.x)
biomass_all$time_year <- as.factor(biomass_all$time_year)
biomass_all$field.no <- as.factor(biomass_all$field)
biomass_all$grazing_regime1 <- as.factor(biomass_all$grazing_regime1)
grep("treatment",colnames(biomass_all))
grep("time_year",colnames(biomass_all))
grep("field.no",colnames(biomass_all))
grep("subplot",colnames(biomass_all))
grep("grazing_regime1",colnames(biomass_all))

############################################################################################

############################################################################################
grep("functionalgroup.x",colnames(biomass_all))
biomass <- biomass_all[,c(1,2,127,130,149,150,140:145,147:148,151)]
biomass2 <- biomass[!duplicated(biomass),]

dim(biomass)
dim(biomass2)
#semi_join() return all rows from x with a match in y.
#anti_join() return all rows from x without a match in y.
anti_join(biomass,biomass2)
dim(semi_join(biomass2,biomass))

dim(biomass2)
ratio1 <- 0.0625
biomass2$`grass_gm-2` <- biomass2$`grass(g)`/ratio1
biomass2$`sedge_gm-2` <- biomass2$`sedge(g)`/ratio1
biomass2$`legume_gm-2`<- biomass2$`legume(g)`/ratio1
biomass2$`forb_gm-2` <- biomass2$`forb(g)`/ratio1
biomass2$`poisonous_gm-2` <- biomass2$`poisonousplants(g)`/ratio1
biomass2$`litter_gm-2` <- biomass2$`litter(g)`/ratio1
biomass2$`totalforb_gm-2` <- biomass2$totalforb / ratio1
biomass2$`totalbiomass_gm-2` <- biomass2$totalbiomass /ratio1
biomass2$`graminoid_gm-2` <- biomass2$graminoid/ratio1
write.xlsx(biomass2,file = "biomass20231207.xlsx",rownames = FALSE)

# biomass2%>%select(which(!(colnames(biomass2)%in%colnames(biomass))))
colnames(biomass2)
mylist_base2 <- grep("base",biomass2$functionalgroup.y)

biomass2 <- biomass2[mylist_base2,]
dim(biomass2)
# a13 <- grep("13",biomass_all$subplot)
# bio_without_a13 <- biomass_all[-a13,c(1,5,133,131,140:145,147:148)]
# bio_without_a13$treatment <- as.factor(bio_without_a13$treatment.y)
# cor(bio_without_a13[,c(5:12)])

start1 <- grep("grass_gm-2",colnames(biomass2))
start1
start2 <- grep("graminoid_gm-2",colnames(biomass2))
start2
#####################################################################

biomass_field_mean <- aggregate(biomass2[start1:start2],by = list(biomass2$time_year,biomass2$treatment,biomass2$field.no),mean)
biomass_field_sd <- aggregate(biomass2[start1:start2], by = list(biomass2$time_year, biomass2$treatment,biomass2$field.no),sd)

biomass_field_mean$time_year <- as.factor(biomass_field_mean$Group.1)
biomass_field_mean$treatment <- as.factor(biomass_field_mean$Group.2)
biomass_field_mean$`field.no` <- as.factor(biomass_field_mean$Group.3)

boxplot(biomass_field_mean$`graminoid_gm-2`)
boxplot(biomass_field_mean$`totalforb_gm-2`)
boxplot(biomass_field_mean$`totalbiomass_gm-2`)

# biomass_field_mean$`log+1_totalforb_gm-2` <- (log(biomass_field_mean$`totalforb_gm-2`) + 1)
# biomass_field_mean$`log10_totalforb_gm-2` <- log10(biomass_field_mean$`totalforb_gm-2`) 
# boxplot(biomass_field_mean$`log_totalforb_gm-2`)
# boxplot(biomass_field_mean$`log+1_totalforb_gm-2`)

apply(biomass_field_mean[c(10:12)],2,shapiro.test)

#total forb is not following the normal distribution
biomass_field_sd$time_year <- as.factor(biomass_field_mean$Group.1)
biomass_field_sd$treatment <- as.factor(biomass_field_mean$Group.2)
biomass_field_sd$`field.no` <- as.factor(biomass_field_mean$Group.3)

write.xlsx(biomass_field_mean , file = "biomass_field_mean20231206.xlsx",rowNames = FALSE)
write.xlsx(biomass_field_sd, file = "biomass_field_sd20231206.xlsx",rowNames = FALSE)

start1 <- grep("grass_gm-2",colnames(biomass_field_mean))
start1

start2 <- grep("graminoid_gm-2",colnames(biomass_field_mean))
start2

biomass_mean <- aggregate(biomass_field_mean[start1:start2],by = list(biomass_field_mean$time_year,biomass_field_mean$treatment),mean)
biomass_sd <- aggregate(biomass_field_mean[start1:start2], by = list(biomass_field_mean$time_year, biomass_field_mean$treatment),sd)
biomass_se <- aggregate(biomass_field_mean[start1:start2], by = list(biomass_field_mean$time_year, biomass_field_mean$treatment),std.error)


write.xlsx(biomass_mean, file = "biomass_mean20231206(1).xlsx",rowNames = FALSE)
write.xlsx(biomass_sd, file = "biomass_sd20231130.xlsx",rowNames = FALSE)
write.xlsx(biomass_se, file = "biomass_se20231206(1).xlsx",rowNames = FALSE)

#####################################################################
# Think about grazing regime 20231207



biomass_field_mean1 <-  aggregate(biomass2[start1:start2],by = list(biomass2$time_year,biomass2$grazing_regime1,biomass2$treatment,biomass2$field.no),mean)


biomass_field_mean1$time_year <- as.factor(biomass_field_mean1$Group.1)
biomass_field_mean1$grazing_regime <- as.factor(biomass_field_mean1$Group.2)
biomass_field_mean1$`treatment` <- as.factor(biomass_field_mean1$Group.3)
biomass_field_mean1$`field.no` <- as.factor(biomass_field_mean1$Group.4)

boxplot(biomass_field_mean1$`graminoid_gm-2`)
boxplot(biomass_field_mean1$`totalforb_gm-2`)
boxplot(biomass_field_mean1$`totalbiomass_gm-2`)

apply(biomass_field_mean1[c(11:13)],2,shapiro.test)
head(biomass_field_mean1)

write.xlsx(biomass_field_mean1,file = "biomass_field_mean1_20231207.xlsx",rowNames = FALSE)



start1 <- grep("grass_gm-2",colnames(biomass_field_mean))
start1

start2 <- grep("graminoid_gm-2",colnames(biomass_field_mean))
start2

biomass_mean1 <- aggregate(biomass_field_mean1[start1:start2],by = list(biomass_field_mean1$time_year,biomass_field_mean1$grazing_regime,biomass_field_mean1$treatment),mean)
biomass_se1 <- aggregate(biomass_field_mean1[start1:start2], by = list(biomass_field_mean1$time_year,biomass_field_mean1$grazing_regime,biomass_field_mean1$treatment),std.error)

write.xlsx(biomass_mean1, file = "biomass_mean1_20231207.xlsx",rowNames = FALSE)
write.xlsx(biomass_se1, file = "biomass_se1_20231207.xlsx",rowNames = FALSE)



# a13_mean <- grep("13",biomass_all_mean$Group.3)
# bio_mean_without_13 <- biomass_all_mean[-a13_mean,c(4,3,143:148,150:152)]


# bio_mean_without_13$field.no <- as.factor(bio_mean_without_13$Group.3)
# write.xlsx(biomass_all_mean, file = "biomass_all_mean20231124_2.xlsx",rowNames = FALSE)
# write.xlsx(biomass_all_sd, file = "biomass_all_sd20231124_2.xlsx",rowNames = FALSE)
# rm(a13,f13_data,b13_data,f13_data_1,f13resultmean,f13resultsd )


# grass 1
# fit_grass <- lm(bio_without_a13$`grass(g)` ~ bio_without_a13$time_year, data = bio_without_a13)
# summary(fit_grass)
# plot(bio_without_a13$time_year,bio_without_a13$`grass(g)` ,xlab = "time_year",ylab = "grass")
# abline(fit_grass)
# 
# fit_grass_zokor <- lm(bio_without_a13$`grass(g)` ~ bio_without_a13$treatment, data = bio_without_a13)
# summary(fit_grass_zokor)
# plot(bio_without_a13$treatment,bio_without_a13$`grass(g)` ,xlab = "zokor",ylab = "grass")
# abline(fit_grass_zokor)
############################################################################################
###########################2 way anova#################################################################
############################################################################################
fit_pois <- lm(bio_without_a13$`poisonousplants(g)` ~ bio_without_a13$time_year, data = bio_without_a13)
summary(fit_pois)
plot(bio_without_a13$time_year,bio_without_a13$`poisonousplants(g)`, xlab = "year",ylab = "pois")
abline(fit_pois)

# grass 2

# fit_grass_mean_zokor <- lm(biomass_all_mean$`grass(g)` ~ biomass_all_mean$treatment, data = biomass_all_mean)
# summary(fit_grass_mean_zokor)
# plot(biomass_all_mean$treatment,biomass_all_mean$`grass(g)` ,xlab = "zokor",ylab = "grass_mean")
# abline(fit_grass_mean_zokor)

# grass 3
# fit_grass_mean_without_13 <- lm(bio_mean_without_13$`grass(g)` ~ bio_mean_without_13$treatment + bio_mean_without_13$time_year + bio_mean_without_13$field.no)
# anova(fit_grass_mean_without_13)
# AIC(fit_grass_mean_without_13)


# treatment + time_year + treatment:time_year
a6 <- aov(biomass_field_mean$`litter_gm-2` ~ biomass_field_mean$treatment * biomass_field_mean$time_year)
summary(a6)

a7 <- aov(biomass_field_mean$`totalforb_gm-2` ~ biomass_field_mean$treatment * biomass_field_mean$time_year)
summary(a7)

a8 <- aov(biomass_field_mean$`totalbiomass_gm-2` ~ biomass_field_mean$treatment * biomass_field_mean$time_year)
summary(a8)

a9 <- aov(biomass_field_mean$`graminoid_gm-2` ~ biomass_field_mean$treatment * biomass_field_mean$time_year)
summary(a9)

################################################################################
########################2 way anova#############################################

a1 <- aov(biomass_field_mean1$`graminoid_gm-2`~biomass_field_mean1$time_year*biomass_field_mean1$grazing_regime*biomass_field_mean1$treatment)
summary(a1)
TukeyHSD(a1)

a2 <- aov(biomass_field_mean1$`totalforb_gm-2`~biomass_field_mean1$time_year*biomass_field_mean1$grazing_regime*biomass_field_mean1$treatment)
summary(a2)
TukeyHSD(a2)

a3 <- aov(biomass_field_mean1$`totalbiomass_gm-2`~biomass_field_mean1$time_year*biomass_field_mean1$grazing_regime*biomass_field_mean1$treatment)
summary(a3)
TukeyHSD(a3)

################################################################################
# treatment + time_year + treatment:time_year

head(biomass_all)

colnames(Zokor_2021)

zokor1 <- read.xlsx("E:/r_data_dit/zokor20231206.xlsx")
zokor1$type <- as.factor(zokor1$type)

zokor1_mean <- aggregate(zokor1[,3:4], by = list(zokor1$type), mean)# try
zokor1_se <- aggregate(zokor1[,3:4], by = list(zokor1$type),std.error)

write.xlsx(zokor1_mean, file = "zokor1_mean20231206.xlsx",rownames = FALSE)
write.xlsx(zokor1_se, file = "zokor1_se20231206.xlsx",rownames = FALSE)
# install.packages(c("VIM","mice","minqa"))

# data(sleep,package = "VIM")
# md.pattern(sleep)
# 
# library("VIM")
# aggr(sleep,prop = FALSE,numbers = TRUE)
# aggr(sleep,prop = TRUE, numbers = TRUE)
# matrixplot(sleep)
# 
# marginplot(sleep[c("Gest","Dream")],pch=c(20), col = c("darkgray","red","blue"))
# 
# x <- as.data.frame(abs(is.na(sleep)))
# head(sleep, n = 5)
# y <- x[which(apply(x,2,sum) > 0)]
# cor(y)
# cor(sleep, y, use = "pairwise.complete.obs")
# 
# 
# # imp <- mice(data, m)
# # fit <- with(imp, analysis)
# # pooled <- pool(fit)
# # summary(pooled)
# library(mice)
# data(sleep, package = "VIM")
# imp <- mice(sleep, seed = 1234)
# 
# fit <- with(imp,lm(Dream~Span +Gest))
# pooled <- pool(fit)
# summary(pooled)







