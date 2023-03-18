library(ggplot2)
library(ggpubr)
library(Rmisc)
Diversity_2021 <- merge(c1,subplot_inform_20230215,by = "subplot")
Diversity_2022 <- merge(c2,subplot_inform_20230215,by = "subplot")
# Dataset:
# Biomas2021_22_avera_year
# zokorNumber_PatchCover_reg
# Diversity_2021 needs to be averaged.
# Diversity_2022 needs to be averaged.
 
# apply(Biomas2021_22_avera_year[26:33],2, shapiro.test)
# apply(zokorNumber_PatchCover_reg[10],2,shapiro.test)
# skewness(zokorNumber_PatchCover_reg[,10],na.rm = TRUE) #0.7819914
# zokorNumber_PatchCover_reg$`bare_land_area.2021.ln(x+1)` <- log10(zokorNumber_PatchCover_reg$NumBPer)
# apply(zokorNumber_PatchCover_reg[17],2,shapiro.test)
names(zokorNumber_PatchCover_reg)[12] <- "treatment_code"
# Biomas_bareland <- merge(Biomas2021_22_avera_year,zokorNumber_PatchCover_reg,by = c("code","treatment_code"))

diver_2021 <- Diversity_2021[c(1,2,3,4,5,8,16,18)]
diver_2022 <- Diversity_2022[c(1,2,4,6,7,10,18,20)]
names(diver_2021)[c(2,3)] <- c("fieldcode","functionalgroup")
names(diver_2022)[c(2,4)] <- c("fieldcode","functionalgroup")

names(Biomas2021_22_avera_year)[4:11] <- c("dominantplants","grass2021","sedge2021","legume2021","forb2021","poisonousplants2021","litter2021","totalbiomass2021")
names(Biomas2021_22_avera_year)[14:20] <- c("grass2022","sedge2022","legume2022","forb2022","poisonousplants2022","litter2022","total_biomass2022")

Biomas_PatchCover <- merge(Biomas2021_22_avera_year,zokorNumber_PatchCover_reg, by = c("code","treatment_code"))

#2023 02 24
#combine grass and sedge biomass and call in graminoid,
#combined legume, poisonous, and forb, and call it forb,
#the original data is g/(25cm*25cm), also for all data, recalculate all the data in g/m2 vegetated areas
#Keep all notes.

Biomas_PatchCover$`graminoid1m2021` <- (Biomas_PatchCover$grass2021 + Biomas_PatchCover$sedge2021)/0.0625
Biomas_PatchCover$`forbs1m2021` <- (Biomas_PatchCover$forb2021 + Biomas_PatchCover$poisonousplants2021 + Biomas_PatchCover$legume2021)/0.0625

Biomas_PatchCover$`totalbiomass1m2021` <- (Biomas_PatchCover$totalbiomass2021/0.0625)

Biomas_PatchCover$`graminoid1m2022` <- (Biomas_PatchCover$grass2022 + Biomas_PatchCover$sedge2022)/0.0625
Biomas_PatchCover$`forbs1m2022` <- (Biomas_PatchCover$forb2022 + Biomas_PatchCover$poisonousplants2022 + Biomas_PatchCover$legume2022)/0.0625

Biomas_PatchCover$`totalbiomass1m2022` <- (Biomas_PatchCover$total_biomass2022/0.0625)

#Change of the ratio of biomass. First calculate the % ratio of graminoid, forbs,then subtraction

Biomas_PatchCover$`graminoid_biomass_percentage_change` <- (Biomas_PatchCover$graminoid1m2022/Biomas_PatchCover$totalbiomass1m2022 - Biomas_PatchCover$graminoid1m2021/Biomas_PatchCover$totalbiomass1m2021)*100
Biomas_PatchCover$`forb_biomass_percentage_change` <- (Biomas_PatchCover$forbs1m2022/Biomas_PatchCover$totalbiomass1m2022 - Biomas_PatchCover$forbs1m2021/Biomas_PatchCover$totalbiomass1m2021)*100
#Biomass change based on the 2021 biomass. First subtract between 2022 biomass - 2021 biomass, then divided by 2021 biomass.
bare_2022 <- read.csv("zokorNumber_PatchCover_reg20230306.csv",header = TRUE,sep = ",")
x1 <- merge(bare_2022,Biomas_PatchCover,by = c("fieldcode"))


Biomas_PatchCover21_22 <- x1
write.csv(Biomas_PatchCover21_22, file = "Biomas_PatchCover21_22.csv", row.names = TRUE)
write.csv(Biomas_PatchCover, file = "Biomas_PatchCover.csv", row.names = TRUE)

Biomas_PatchCover21_22$Vegetated_Per_2022 <- (1 - Biomas_PatchCover21_22$NumBPer_2022)

Biomas_PatchCover$`graminoid_biomass_percentage_change_2021` <- (Biomas_PatchCover$graminoid1m2022 - Biomas_PatchCover$graminoid1m2021)/Biomas_PatchCover$graminoid1m2021*100

Biomas_PatchCover$`forb_biomass_percentage_change_2021` <- (Biomas_PatchCover$forbs1m2022 - Biomas_PatchCover$forbs1m2021)/Biomas_PatchCover$forbs1m2021*100

Biomas_PatchCover$`total_biomass_percentage_change` <- (Biomas_PatchCover$totalbiomass1m2022 - Biomas_PatchCover$totalbiomass1m2021)/Biomas_PatchCover$totalbiomass1m2021*100


Biomas_PatchCover$graminoid1m2021_weighted <- Biomas_PatchCover$graminoid1m2021*(100 - Biomas_PatchCover$NumBPer)/100
Biomas_PatchCover$forbs1m2021_weighted <- Biomas_PatchCover$forbs1m2021*(100 - Biomas_PatchCover$NumBPer)/100
Biomas_PatchCover$totalbiomass1m2021_weighted <- Biomas_PatchCover$totalbiomass1m2021*(100 - Biomas_PatchCover$NumBPer)/100
# Biomas_PatchCover[1:10,c(49:51,60:62)]

head(Biomas_PatchCover)

write.csv(Biomas_PatchCover,file = "Biomas_PatchCover_20230224.csv",row.names = TRUE)
write.csv(diver.2021_PatchCover,file = "diver.2021_PatchCover_20230224.csv",row.names = TRUE)
write.csv(diver.2022_PatchCover,file = "diver.2022_PatchCover_20230224.csv",row.names = TRUE)

#x is estimated data.

p1 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "graminoid1m2021",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Graminoids biomass in 2021(g/m2)")
p2 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "forbs1m2021",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Forbs biomass in 2021(g/m2)")

p3 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "totalbiomass1m2021",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Total biomass in 2021(g/m2)")
#
p4 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "graminoid1m2022",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Graminoids biomass in 2022(g/m2)")

p5 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "forbs1m2022",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Forbs biomass in 2022(g/m2)")

p6 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "totalbiomass1m2022",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Total biomass in 2022(g/m2)")

p7 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "graminoid_biomass_percentage_change",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Change of graminoids ratio in total biomass (%)")

p8 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "forb_biomass_percentage_change",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Change of forbs ratio in total biomass (%)")

p9 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "graminoid_biomass_percentage_change_2021",
                add = "reg.line",conf.int = TRUE,
                cor.coef = TRUE,cor.method = "pearson",
                xlab = "Percentage of bare land area in 2021 (%)",ylab = "Change of graminoid biomass compared with 2021 (%)")

p10 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "forb_biomass_percentage_change_2021",
                 add = "reg.line",conf.int = TRUE,
                 cor.coef = TRUE,cor.method = "pearson",
                 xlab = "Percentage of bare land area in 2021 (%)",ylab = "Change of forbs biomass compared with 2021 (%)")

p11 <- ggscatter(Biomas_PatchCover,x = "NumBPer", y = "total_biomass_percentage_change",
                 add = "reg.line",conf.int = TRUE,
                 cor.coef = TRUE,cor.method = "pearson",
                 xlab = "Percentage of bare land area in 2021 (%)",ylab = "Change of total biomass compared with 2021 (%)")

multiplot(p1,p2,p3,cols = 1)
multiplot(p4,p5,p6,cols = 1)
multiplot(p7,p8,cols = 2)
multiplot(p9,p10,cols = 2)
p11

View(Biomas_PatchCover)
#TYPE I (SEQUENTIAL) Effects are adjusted for those that appear earlier in the formula. A is unadjusted. B
#is adjusted for the A. The A:B interaction is adjusted for A and B.

a1 <- aov(Biomas_PatchCover$graminoid1m2021 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a2 <- aov(Biomas_PatchCover$forbs1m2021 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a3 <- aov(Biomas_PatchCover$totalbiomass1m2021 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a4 <- aov(Biomas_PatchCover$graminoid1m2022 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a5 <- aov(Biomas_PatchCover$forbs1m2022 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a6 <- aov(Biomas_PatchCover$totalbiomass1m2022 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a7 <- aov(Biomas_PatchCover$graminoid_biomass_percentage_change ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a8 <- aov(Biomas_PatchCover$forb_biomass_percentage_change ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a9 <- aov(Biomas_PatchCover$graminoid_biomass_percentage_change_2021 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a10 <- aov(Biomas_PatchCover$forb_biomass_percentage_change_2021 ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
a11 <- aov(Biomas_PatchCover$total_biomass_percentage_change ~ Biomas_PatchCover$treatment_code + Biomas_PatchCover$NumBPer)
summary(a1)# coviriate bare land patch has 

b1 <- aov(diver.2021_PatchCover$N0 ~ diver.2021_PatchCover$treatment_code + diver.2021_PatchCover$NumBPer)
b2 <- aov(diver.2021_PatchCover$N1 ~ diver.2021_PatchCover$treatment_code + diver.2021_PatchCover$NumBPer)
b3 <- aov(diver.2022_PatchCover$N0 ~ diver.2022_PatchCover$treatment_code + diver.2022_PatchCover$NumBPer)
b4 <- aov(diver.2022_PatchCover$N1 ~ diver.2022_PatchCover$treatment_code + diver.2022_PatchCover$NumBPer)
summary(b1)

Biomas2021_22_avera$time_factor <- factor(Biomas2021_22_avera$time, labels = c("2021","2022"))
Biomas2021_22_avera$treatment_factor <- factor(Biomas2021_22_avera$treatment_code, labels = c("+zokor","-zokor"))

Biomas2021_22_avera$graminoid1m <- (Biomas2021_22_avera$`grass(g)` + Biomas2021_22_avera$`sedge(g)`)/0.0625
Biomas2021_22_avera$forbs1m <- (Biomas2021_22_avera$`legume(g)` + Biomas2021_22_avera$`forb(g)` + Biomas2021_22_avera$`poisonousplants(g)`)/0.0625
Biomas2021_22_avera$total_biomass1m <- (Biomas2021_22_avera$`grass(g)` + Biomas2021_22_avera$`sedge(g)` + Biomas2021_22_avera$`legume(g)` + Biomas2021_22_avera$`forb(g)` + Biomas2021_22_avera$`poisonousplants(g)`)/0.0625

#type 1
graminoid_aov <- aov(graminoid1m~treatment_factor*time_factor,Biomas2021_22_avera)
summary(graminoid_aov)
TukeyHSD()

forbs_aov <- aov(forbs1m ~ treatment_factor*time_factor, Biomas2021_22_avera)
summary(forbs_aov)
TukeyHSD(forbs_aov)

total_biomass_aov <- aov(total_biomass1m ~ treatment_factor*time_factor, Biomas2021_22_avera)
summary(total_biomass_aov)
TukeyHSD()

graminoid_weighted_aov <- aov(graminoid1m_weighted ~ treatment_factor*time_factor, Biomas2021_22_avera_PatchCover)
summary(graminoid_weighted_aov)
TukeyHSD()

forbs_weighted_aov <- aov(forbs1m_weighted ~ treatment_factor*time_factor, Biomas2021_22_avera_PatchCover)
summary(forbs_weighted_aov)
TukeyHSD()

totalbiomass_weighted_aov <- aov(total_biomass1m_weighted ~ treatment_factor*time_factor, Biomas2021_22_avera_PatchCover)
summary(totalbiomass_weighted_aov)
TukeyHSD()

summary(total_biomass_aov)
summary(forbs_weighted_aov)
summary(totalbiomass_weighted_aov)
#Biomas2021_22_avera_PatchCover
View(Biomas2021_22_avera_PatchCover)
# p_plot_1 <- ggplot(Biomas2021_22_avera_PatchCover, aes(x = time_factor, y = graminoid1m_weighted, color = treatment_factor)) +
#   geom_point()

###############################################################################################################
###################################Here##########################################################################
Biomas_PatchCover21_22_mean <- aggregate(Biomas_PatchCover21_22[,c(11,62:67,)])
Biomas_PatchCover21_22_sd <- 
names(Biomas2021_22_avera_PatchCover_mean)[7:12] <- c("graminoid1m_mean","forbs1m_mean","total_biomass1m_mean","graminoid1m_weighted_mean","forbs1m_weighted_mean","total_biomass1m_weighted_mean")
names(Biomas2021_22_avera_PatchCover_sd)[7:12] <- c("graminoid1m_sd","forbs1m_sd","total_biomass1m_sd","graminoid1m_weighted_sd","forbs1m_weighted_sd","total_biomass1m_weighted_sd")
Bio <- merge(Biomas2021_22_avera_PatchCover_mean, Biomas2021_22_avera_PatchCover_sd, by = c("Group.1","Group.2"))
# Bio$Year <- 
names(Bio)[1:2] <- c("Year","Treatment")

Bio$time_factor <- factor(Bio$Year, labels = c("2021","2022"))
Bio$treatment_factor <- factor(Bio$Treatment, labels = c("+zokor","-zokor"))

plot1 <- ggplot(Bio, aes(x = time_factor, y = Bio$graminoid1m_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Graminoid Biomass (g/m2)") +
  geom_errorbar(aes(x = time_factor, ymin= Bio$graminoid1m_mean - Bio$graminoid1m_sd, ymax=Bio$graminoid1m_mean + Bio$graminoid1m_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  theme(legend.position = "none") +
  labs(title = "A")

plot2 <- ggplot(Bio, aes(x = time_factor, y = Bio$forbs1m_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Forb Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio$forbs1m_mean - Bio$forbs1m_sd, ymax=Bio$forbs1m_mean + Bio$forbs1m_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  theme(legend.position = "none") +
  labs(title = "B")

plot3 <- ggplot(Bio, aes(x = time_factor, y = Bio$total_biomass1m_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Total Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio$total_biomass1m_mean - Bio$total_biomass1m_sd, ymax= Bio$total_biomass1m_mean + Bio$total_biomass1m_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  theme(legend.position = "none") +
  labs(title = "C")

plot4 <- ggplot(Bio, aes(x = time_factor, y = Bio$graminoid1m_weighted_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Graminoid Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio$graminoid1m_weighted_mean - Bio$graminoid1m_weighted_sd, ymax= Bio$graminoid1m_weighted_mean + Bio$graminoid1m_weighted_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  theme(legend.position = "none") +
  labs(title = "D")

plot5 <- ggplot(Bio, aes(x = time_factor, y = Bio$forbs1m_weighted_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Forb Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio$forbs1m_weighted_mean - Bio$forbs1m_weighted_sd, ymax= Bio$forbs1m_weighted_mean + Bio$forbs1m_weighted_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  theme(legend.position = "none") +
  labs(title = "E")

plot6 <- ggplot(Bio, aes(x = time_factor, y = Bio$total_biomass1m_weighted_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Total Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio$total_biomass1m_weighted_mean - Bio$total_biomass1m_weighted_sd, ymax= Bio$total_biomass1m_weighted_mean + Bio$total_biomass1m_weighted_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  theme(legend.position = "none")+
  labs(title = "F")
multiplot(plot1,plot4,plot2,plot5,plot3,plot6,cols = 3)
############################################################################################################
zokorNumber_PatchCover_reg$Number_of_killed_zokor_in_2021
zokorNumber_PatchCover_reg$Remaining_number_of_zokor_2021

zokorEstimation_mean <- aggregate(zokorNumber_PatchCover_reg[,c(3,4,12,14)], by=list( zokorNumber_PatchCover_reg$treatment_code), FUN = mean)

zokorEstimation_sd <- aggregate(zokorNumber_PatchCover_reg[,c(3,4,12,14)], by=list( zokorNumber_PatchCover_reg$treatment_code), FUN = std.error) #library(plotrix)

names(zokorEstimation_mean)[1] <- "treatment_code"
names(zokorEstimation_sd)[1] <- "treatment_code"
zokorEstimation_mean$treatment_factor <- factor(zokorEstimation_mean$treatment_code, labels = c("+zokor","-zokor"))
zokorEstimation_sd$treatment_factor <- factor(zokorEstimation_sd$treatment_code, labels = c("+zokor","-zokor"))

zokor_mean <- melt(zokorEstimation_mean[,c(1,5,3)],id = c("treatment_code"))
zokor_sd <- melt(zokorEstimation_sd[,c(1,5,3)], id = c("treatment_code"))

zokor_mean <- data.frame(treatment_code = c(0,1,0,1),
                         number_type = c("The number of killed zokor in 2021","The number of killed zokor in 2021","The remaining number of zokor in 2021","The remaining number of zokor in 2021"),
                         number = c(0.000000,3.529412,5.176471,1.411765),
                         year = c(2021,2021,2021,2021))

zokor_sd <- data.frame(treatment_code = c(0,1,0,1),
                       number_type = c("The number of killed zokor in 2021","The number of killed zokor in 2021","The remaining number of zokor in 2021","The remaining number of zokor in 2021"),
                       number_sd = c(0.0000000,0.4122896,0.7237326,0.3215178))
zokor_meansd <- merge(zokor_mean, zokor_sd,by = c("treatment_code","number_type") )

zokor_mean2022 <- data.frame(treatment_code = c(0,0,1,1),
                             number_type = c("The number of killed zokor in 2022","The remaining number of zokor in 2022","The number of killed zokor in 2022","The remaining number of zokor in 2022"),
                             number = c(0,0,0,0),
                             year = c(2022,2022,2022,2022),
                             number_sd = c(0,0,0,0))
zokor1 <- rbind(zokor_meansd,zokor_mean2022)

zokor1$number_type <- as.factor(zokor1$number_type)
zokor1$treatment_factor <- factor(zokor1$treatment_code, labels = c("+zokor","-zokor"))
zokor1$year <- as.factor(zokor1$year)
factor(zokor1$number_type)
factor(zokor1$treatment_factor)
factor(zokor1$year)
factor(zokor1$treatment_year)
zokor1$treatment_year <- c("+zokor 2021","+zokor 2021","-zokor 2021","-zokor 2021","+zokor 2022","+zokor 2022","-zokor 2022","-zokor 2022")
zokor1$treatment_year <- factor(zokor1$treatment_year, labels = c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"))
factor(zokor1$treatment_year)



area_mean <- aggregate(zokorNumber_PatchCover_reg[,c(10,12,16)], by = list( zokorNumber_PatchCover_reg$treatment_code), FUN = mean)
area_sd <- aggregate(zokorNumber_PatchCover_reg[,c(10,12,16)], by = list( zokorNumber_PatchCover_reg$treatment_code), FUN =  std.error)
area_mean$treatment_factor <- factor(area_mean$Group.1, labels = c("+zokor","-zokor"))
area_sd$treatment_factor <- factor(area_sd$Group.1, labels = c("+zokor","-zokor"))
area1 <- cbind(area_mean,area_sd)
write.csv(area1, file = "area1.csv", row.names = TRUE)



zokor2 <- read.csv("zokor1.csv",header = TRUE,sep = ",")
zokor2$treatment_year_1 <- factor( c("+zokor 2021","+zokor 2021","-zokor 2021","-zokor 2021","+zokor 2022","+zokor 2022","-zokor 2022","-zokor 2022"),levels = c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"))
factor(zokor2$treatment_year_1)
zokor2$number_type_1 <- factor(zokor2$number_type, levels = c("Killed zokor","Remaining zokor"))




########################################################################################################
p1 <- ggplot(zokor2,aes(x = zokor2$treatment_year_1,y = zokor2$number,fill = zokor2$number_type_1)) +
  geom_col(color = "black", width = .6, position = "stack") + 
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(name = "Average number of zokor in experimental plots (50m * 50m)", limits = c(0,7)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(x = "Treatments") +
  geom_errorbar(aes(ymin = zokor2$number_sd_min, ymax = zokor2$number_sd_max),stat = "identity",position = position_dodge(0),width = .1) +
  labs(title = "G")


area1 <- read.csv("area1.csv",header = TRUE,sep = ",")
area1$treatment_year_1 <- factor(c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"),levels = c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"))
factor(area1$treatment_year_1)


p2 <- ggplot(area1, aes(x=area1$treatment_year_1, y = area1$NumBPer)) +
  geom_col(color = "black", width = .6) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(name = "Percent of bare land (%)", limits = c(0,10)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(x = "Treatments") +
  geom_errorbar(aes(ymin = area1$NumBPer - area1$NumBPer_sd, ymax = area1$NumBPer + area1$NumBPer_sd),stat = "identity",position = position_dodge(0),width = .1) +
  labs(title = "H")

p3 <- ggplot(area1, aes(x=area1$treatment_year_1, y = area1$Vegetated_Per)) +
  geom_col(color = "black", width = .6) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(name = "Percent of vegetated land (%)", limits = c(0,100)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(x = "Treatments") +
  geom_errorbar(aes(ymin = area1$Vegetated_Per - area1$Vegetated_Per_sd, ymax = area1$Vegetated_Per + area1$Vegetated_Per_sd),stat = "identity",position = position_dodge(0),width = .1) +
  labs(title = "I")
p3

multiplot(p1,p2,p3,cols = 3)

write.csv(zokorNumber_PatchCover_reg,file = "zokorNumber_PatchCover_reg20230306.csv",row.names = TRUE)

z1 <- read.csv("PatchCo2022Per_20230224.csv",header = TRUE,sep = ",")
z1 <- z1[1:5,]
ggscatter(z1,x="NumBPer_Line_2022",y="NumBPer_uav_2022",
          add = "reg.line",conf.int = TRUE,
          cor.coef = TRUE,cor.method = "pearson",
          xlab = "NumBPer_Line_2022 %",ylab = "NumBPer_uav_2022")


multiplot(plot1,plot4,p1,plot2,plot5,p2,plot3,plot6,p3,cols = 3)













