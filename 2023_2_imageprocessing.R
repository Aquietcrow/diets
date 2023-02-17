pkgs<-c("rgdal","caret","raster","foreign", "kernlab")
lapply(pkgs,require, character.only=T)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(pkgs)

bandnames <- list.files("D:/multispectral/7c", pattern="\\.tif$", full.names = T)
bandnames
ls_7c <-stack(bandnames)
ls_7c
plot(ls_7c)
plotRGB(ls_7c, r=3, g=2, b=1, stretch="hist")
plotRGB(ls_7c, r=10, g=3, b=1, stretch="hist")


bandnames_2 <- list.files("D:/multispectral/7c_noalpha_reflectance", pattern="\\.tif$", full.names = T)
bandnames_2
ls_7c_noalpha_refl <-stack(bandnames_2)
ls_7c_noalpha_refl
plot(ls_7c_noalpha_refl)

bandnames_3 <- list.files("D:/multispectral/7c_transparent_reflectance", pattern="\\.tif$", full.names = T)
bandnames_3
ls_transparent_refl <-stack(bandnames_3)
ls_transparent_refl
plot(ls_transparent_refl)
