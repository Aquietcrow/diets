View(Biomas2021_22_reg) #easy to choose from base subplot. 2021 & 2022
View(c1) #2021 use base subplot
View(c2) #2022 use base subplot

Diversity_2021 <- merge(c1,subplot_inform_20230215,by = "subplot")
Diversity_2022 <- merge(c2,subplot_inform_20230215,by = "subplot")
View(zokorNumber_PatchCover_reg)#ok 2021
