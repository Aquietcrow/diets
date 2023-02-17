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

SummerHenan2021_22<-odbcConnectAccess2007("E:/Access/2021_summer_henan.accdb")
Biomas2021_22<-sqlFetch(SummerHenan2021_22,"2021_summer_henan_biomass")

######################################################################
#Biomass analysis                                                    #
######################################################################
#I have failed the cooperation with herders in Field 42 and Field 43.#     
######################################################################

FailedfieldNo1<-grep("42c",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNo2<-grep("42t",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNo3<-grep("43c",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNo4<-grep("43t",Biomas2021_22$fieldcode,fixed = TRUE)
FailedfieldNoall<-c(FailedfieldNo1,FailedfieldNo2,FailedfieldNo3,FailedfieldNo4)

Biomas2021_22 <- Biomas2021_22[-FailedfieldNoall,]
##################################################################
# subplot_inform <- SpeCov2021_dele_failedfields[2:4] #Extract the subplot functional group information from 2021's data.
str(grepl("c",subplot_inform$fieldcode))
subplot_inform$treatment <- grepl("c",subplot_inform$fieldcode)
#control flow
subplot_inform$treatment[subplot_inform$treatment == TRUE] <-"c"
subplot_inform$treatment[subplot_inform$treatment == FALSE] <-"t"
subplot_inform$field.no <- substring(subplot_inform$fieldcode,1,nchar(subplot_inform$fieldcode)-1)
#
field.no2code_func<-function(x,y,h,n,m){
  for (n in 1:nrow(x)){
    for(m in 1:nrow(y)){
      if(x[n,h]==y[m,1]){
        x$code[n]<-y[m,2]}
      else{m<-m+1}}
    n<-n+1}
  return(x)}

subplot_inform_20230215 <- field.no2code_func(subplot_inform,field.no2code,5,1,1)

write.csv(subplot_inform_20230215,file = "subplot_inform_20230215.csv",row.names = TRUE)


Biomas2021_22_mergedsubplot<-merge(Biomas2021_22,subplot_inform_20230215,by="subplot") 
# write.csv(Biomas2021_22_mergedsubplot,file = "Biomas2021_22_mergedsubplot.csv",row.names = TRUE)

# subset(data,data$AGE>25 & data$SAL>8000)
# 重命名
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
# aov one-way anova, analysis the variance in different treatment or different experimental sites.
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
#################################################################
#Continue to do absolute biomass analysis 2023 02 07
#################################################################

#Ligularia_sp

Biomas2021_22_reg<- subset(Biomas2021_22_full_0,Biomas2021_22_full_0$functionalgroup != "bare")# this chooses other description,such as,Ligularia_sp

##############################################################
#grep() and grepl() are arguments can search certain string for target string.
#str(), summary()
#nchar(bioma_nobare$fieldcode[1])

#one way
mylist <- names(Biomas2021_22_reg)[-c(4:9)]
Biomas2021_22_x <- melt(Biomas2021_22_reg,id = mylist)
names(Biomas2021_22_x)[10:11] <- c("functionalgroup_subplot","Biomass")

write.csv(Biomas2021_22_x,file = "Biomas2021_22 20230216.csv",row.names = TRUE)





# with()
##############################################################
#two-way anova 2022 12 5
# ok, now do a 2-way ANOVA, or GLM, with as factor field, treatment, and field*treatment, and as dependent total biomass, then thee same for grass, etc.
##############################################################
# attach()
# detach()

a <- aggregate(bioma_all2021clean[mylist],by=list(field.no,treatment,functionalgroup.x), FUN=mean)
b <- aggregate(bioma_all2021clean[mylist],by=list(field.no,treatment,functionalgroup.x), FUN=sd)
aggregate(bioma_all2021clean[mylist],by=list(field.no), FUN=mean)
aggregate(bioma_all2021clean[mylist],by=list(treatment), FUN=mean)  

fit1<-qqPlot(lm(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$treatment,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

fit_totalbio_treatment<-aov(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$treatment)
summary(fit_totalbio_treatment)
#There is no difference between different treatment.
fit_totalbio_site<-aov(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$`field.no`)
summary(fit_totalbio_site)


################################################
#How to find outliers from the normality test
################################################
outlierTest(fit1)
outlierTest(fit2)

fit2<-bartlett.test(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$`treatment`,data = bioma_all2021clean)

fit3<-qqPlot(lm(bioma_all2021clean$`totalbiomass(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

qqPlot(lm(bioma_all2021clean$`grass(g)`~bioma_all2021clean$`field.no`,data = bioma_all2021clean),simulate=TRUE,main="Q-Q Plot",labels=TRUE)

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


###########################################################################
#facet_wrap
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

