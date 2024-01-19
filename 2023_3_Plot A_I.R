library(ggplot2)
library(ggpubr)
library(Rmisc)
library(plotrix)
setwd("E:/r_data_dit")
bare_2022 <- read.csv("zokorNumber_PatchCover_reg20230306.csv",header = TRUE,sep = ",")
bare_2022$Vegetated_Per_2022 <- (100 - bare_2022$NumBPer_2022)
bare_2022$NumBPer_2022 <- bare_2022$NumBPer_2022*100
bare_2022_mean <- aggregate(bare_2022[7:11], by = list(bare_2022$treatment_code),FUN = mean, na.rm = TRUE)
bare_2022_sd <- aggregate(bare_2022[7:11], by = list(bare_2022$treatment_code),FUN = std.error, na.rm = TRUE)

write.csv(bare_2022_mean, file = "bare_2022_mean.csv",row.names = TRUE)
write.csv(bare_2022_sd, file = "bare_2022_sd.csv",row.names = TRUE)

Biomas_PatchCover <- Biomas_PatchCover[,1:62]
Bio2 <- merge(Biomas_PatchCover,bare_2022,by = c("fieldcode"))

Bio2$Vegetated_Per_2022
write.csv(bare1,file = "x1.csv", row.names = TRUE)

Bio2$graminoid1m2022_weighted <- Bio2$graminoid1m2022*Bio2$Vegetated_Per_2022/100
Bio2$forbs1m2022_weighted <- Bio2$forbs1m2022*Bio2$Vegetated_Per_2022/100
Bio2$totalbiomass1m2022_weighted <- Bio2$totalbiomass1m2022*Bio2$Vegetated_Per_2022/100

Bio2_mean <- aggregate(Bio2[,c(3,49:54,60:62,76:78)], by = list(Bio2$treatment_code.x),FUN = mean, na.rm = TRUE)
Bio2_sd <- aggregate(Bio2[,c(3,49:54,60:65,76:78)], by = list(Bio2$treatment_code.x),FUN = std.error, na.rm = TRUE)

write.csv(Biomas_PatchCover,file = "Biomas_PatchCover_20230224.csv",row.names = TRUE)
write.csv(bare_2022, file = "x2.csv", row.names = TRUE)
write.csv(Bio, file = "Bio.csv",row.names = TRUE )
write.csv(Bio2, file = "Bio2.csv", row.names = TRUE)
write.csv(Bio2_mean,file = "Bio2_mean1.csv", row.names = TRUE)
write.csv(Bio2_sd, file = "Bio2_sd1.csv", row.names = TRUE)
Biomas2021_22_avera_PatchCover_mean <- aggregate(Biomas2021_22_avera_PatchCover[,c(1:4,14:16,32:34)], by = list(Biomas2021_22_avera_PatchCover$time,Biomas2021_22_avera_PatchCover$treatment_code), FUN = mean, na.rm = TRUE)

Biomas2021_22_avera_PatchCover_sd <- aggregate(Biomas2021_22_avera_PatchCover[,c(1:4,14:16,32:34)], by = list(Biomas2021_22_avera_PatchCover$time,Biomas2021_22_avera_PatchCover$treatment_code), FUN = std.error)

zokor2 <- read.csv("zokor1.csv",header = TRUE,sep = ",")
zokor2$treatment_year_1 <- factor( c("+zokor 2021","+zokor 2021","-zokor 2021","-zokor 2021","+zokor 2022","+zokor 2022","-zokor 2022","-zokor 2022"),levels = c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"))
factor(zokor2$treatment_year_1)
zokor2$number_type_1 <- factor(zokor2$number_type, levels = c("Killed zokor","Remaining zokor"))

Bio_1 <- read.csv("Bio.csv",header = TRUE, sep = ",")
Bio_11$time_factor <- factor(Bio_1$Year, labels = c("2021","2022"))
Bio_11$treatment_factor <- factor(Bio_1$Treatment, labels = c("+zokor","-zokor"))
Bio_1copy<-Bio_1
Bio_1 <- Bio_11
# Biomass
plot1 <- ggplot(Bio_11, aes(x = time_factor, y = Bio_11$graminoid1m_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Graminoid Biomass (g/m2)") +
  geom_errorbar(aes(x = time_factor, ymin= Bio_11$graminoid1m_mean - Bio_11$graminoid1m_sd, ymax=Bio_11$graminoid1m_mean + Bio_11$graminoid1m_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  geom_text(aes(y = Bio_11$graminoid1m_mean + 2.3*Bio_11$graminoid1m_sd, label = plot1_label), position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") )+
  theme(legend.title = element_blank())
plot1_label <- c("a","a","a","a")




plot2 <- ggplot(Bio_11, aes(x = time_factor, y = Bio_11$forbs1m_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Forb Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio_11$forbs1m_mean - Bio_11$forbs1m_sd, ymax=Bio_11$forbs1m_mean + Bio_11$forbs1m_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  geom_text(aes(y = Bio_11$forbs1m_mean + 2.3*Bio_11$forbs1m_sd, label = plot2_label), position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  theme(legend.title = element_blank())
plot2_label <- c("ab","a","bc","c")


plot3 <- ggplot(Bio_11, aes(x = time_factor, y = Bio_11$total_biomass1m_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Total Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio_11$total_biomass1m_mean - Bio_11$total_biomass1m_sd, ymax= Bio_11$total_biomass1m_mean + Bio_11$total_biomass1m_sd),stat = "identity" , position = position_dodge(0.8),width = .1) +
  geom_text(aes(y = Bio_11$total_biomass1m_mean + 2.3*Bio_11$total_biomass1m_sd, label = plot3_label), position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  theme(legend.title = element_blank())
plot3_label <- c("ab","a","b","b")

plot4 <- ggplot(Bio_11, aes(x = time_factor, y = Bio_11$graminoid1m_weighted_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Graminoid Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio_11$graminoid1m_weighted_mean - Bio_11$graminoid1m_weighted_sd, ymax= Bio_11$graminoid1m_weighted_mean + Bio_11$graminoid1m_weighted_sd),stat = "identity" , position = position_dodge(0.8),width = .1) +
  geom_text(aes(y = Bio_11$graminoid1m_weighted_mean + 2.3*Bio_11$graminoid1m_weighted_sd, label = plot4_label), position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  theme(legend.title = element_blank() )
plot4_label <- c("a","a","a","a")


plot5 <- ggplot(Bio_11, aes(x = time_factor, y = Bio_11$forbs1m_weighted_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Forb Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio_11$forbs1m_weighted_mean - Bio_11$forbs1m_weighted_sd, ymax= Bio_11$forbs1m_weighted_mean + Bio_11$forbs1m_weighted_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  geom_text(aes(y = Bio_11$forbs1m_weighted_mean + 2.3*Bio_11$forbs1m_weighted_sd, label = plot5_label), position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  theme(legend.title = element_blank())
plot5_label <- c("ab","a","b","b")

plot6 <- ggplot(Bio_11, aes(x = time_factor, y = Bio_11$total_biomass1m_weighted_mean, fill = treatment_factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Total Biomass (g/m2)")+
  geom_errorbar(aes(x = time_factor, ymin= Bio_11$total_biomass1m_weighted_mean - Bio_11$total_biomass1m_weighted_sd, ymax= Bio_11$total_biomass1m_weighted_mean + Bio_11$total_biomass1m_weighted_sd),stat = "identity" , position = position_dodge(0.8),width = .1)+
  geom_text(aes(y = Bio_11$total_biomass1m_weighted_mean + 2.3*Bio_11$total_biomass1m_weighted_sd, label = plot6_label), position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme(legend.position = "bottom")+
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  theme(legend.title = element_blank())
plot6_label <- c("ab","a","b","b")

write.csv(zokor2, file = "zokor2.csv", row.names = TRUE)
#########################################################################################################
zokor2<-read.csv("zokor2.csv",header = TRUE,sep = ",")

p1 <- ggplot(zokor2,aes(x = zokor2$treatment_year_1,y = zokor2$number,fill = zokor2$number_type_1)) +
  geom_col(color = "black", width = .6, position = "stack") + 
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(name = "Average number of zokor in experimental plots (50m * 50m)", limits = c(0,7)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  labs(x = "Treatments") +
  geom_errorbar(aes(ymin = zokor2$number_sd_min, ymax = zokor2$number_sd_max),stat = "identity",position = position_dodge(0),width = .1) 
p1

area1 <- read.csv("area1.csv",header = TRUE,sep = ",")
area1$treatment_year_1 <- factor(c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"),levels = c("+zokor 2021","-zokor 2021","+zokor 2022","-zokor 2022"))
factor(area1$treatment_year_1)


p2 <- ggplot(area1, aes(x=area1$treatment_year_1, y = area1$NumBPer)) +
  geom_point(color = "black", size = 3, stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(name = "Percent of bare land (%)", limits = c(1,5)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold") ) +
  labs(x = "Treatments") +
  geom_errorbar(aes(ymin = area1$NumBPer - area1$NumBPer_sd, ymax = area1$NumBPer + area1$NumBPer_sd),stat = "identity",position = position_dodge(0),width = .1) 
p2
p3 <- ggplot(area1, aes(x = area1$treatment_year_1, y = area1$Vegetated_Per)) +
  geom_point(color = "black", size = 3, stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(name = "Percent of vegetated land (%)", limits = c(94,98)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, face = "bold"), 
        title = element_text(size = 15, face = "bold")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(x = "Treatments") +
  geom_errorbar(aes(ymin = area1$Vegetated_Per - area1$Vegetated_Per_sd, ymax = area1$Vegetated_Per + area1$Vegetated_Per_sd),stat = "identity",position = position_dodge(0),width = .1)
p3

plot_combined<- multiplot(plot1,plot4,p1,plot2,plot5,p2,plot3,plot6,p3,cols = 3)

ggsave("plot_combined.tiff",plot_combined, width = 16, height = 16 )
#type 1
ggsave("plot1.tiff", plot1,width=5,height = 5)
ggsave("plot2.tiff", plot2,width=5,height = 5)
ggsave("plot3.tiff", plot3,width=5,height = 5)
ggsave("plot4.tiff", plot4,width=5,height = 5)
ggsave("plot5.tiff", plot5,width=5,height = 5)
ggsave("plot6.tiff", plot6,width=5,height = 5)
ggsave("p1.tiff", p1,width=7,height = 7)
ggsave("p2.tiff", p2,width=5,height = 5)
ggsave("p3.tiff", p3,width=5,height = 5)
write.csv(Bio2,file = "Bio2_20230308.csv", row.names = TRUE )
Bio2_1 <- Bio2[,c(2, 3, 5, 4,49:51,60:62)]
Bio2_2 <- Bio2[,c(2, 3, 5, 13,52:54,76:78)]
Bio_name <- c("code","treatment_code","dominantplants","time","graminoid1m","forbs1m","totalbiomass1m","graminoid1m_weighted","forbs1m_weighted","totalbiomass1m_weighted")
names(Bio2_1) <- Bio_name
names(Bio2_2) <- Bio_name
Bio3 <- rbind(Bio2_1,Bio2_2)


# Bio3 <- read.csv("Bio3.csv",header = TRUE,sep = ",")

Bio3$time_factor <- factor(Bio3$time, labels = c("2021","2022"))
Bio3$treatment_factor <- factor(Bio3$treatment_code, labels = c("+zokor","-zokor"))

graminoid_aov <- aov(graminoid1m~treatment_factor+time_factor+treatment_factor*time_factor,Bio3)
summary(graminoid_aov)
TukeyHSD(graminoid_aov)

forbs_aov <- aov(forbs1m ~ treatment_factor + time_factor + treatment_factor*time_factor, Bio3)
summary(forbs_aov)
TukeyHSD(forbs_aov)

total_biomass_aov <- aov(totalbiomass1m ~ treatment_factor*time_factor, Bio3)
summary(total_biomass_aov)
TukeyHSD(total_biomass_aov)

graminoid_weighted_aov <- aov(graminoid1m_weighted ~ treatment_factor*time_factor, Bio3)
summary(graminoid_weighted_aov)
TukeyHSD(graminoid_weighted_aov, which = "treatment_factor:time_factor")

forbs_weighted_aov <- aov(forbs1m_weighted ~ treatment_factor + time_factor + treatment_factor*time_factor, Bio3)
summary(forbs_weighted_aov)
TukeyHSD(forbs_weighted_aov,which = "treatment_factor:time_factor")

forbs_weighted_aov1 <- aov(forbs1m_weighted ~ time_factor*treatment_factor, Bio3)
summary(forbs_weighted_aov1)
x1 <- TukeyHSD(forbs_weighted_aov1,which = "time_factor:treatment_factor")





x2 <- aggregate(Bio3, by = list(Bio3$time_factor,Bio3$treatment_factor),FUN = mean, na.rm = TRUE)

totalbiomass_weighted_aov <- aov(totalbiomass1m_weighted ~ treatment_factor*time_factor, Bio3)
summary(totalbiomass_weighted_aov)
TukeyHSD(totalbiomass_weighted_aov)

install.packages("PROJ")
install.packages("sqlite3")


library(PROJ)
