#https://vt-hydroinformatics.github.io/rgeoraster.html
pkgs<-c("tidyverse","raster","sf","whitebox", "tmap")
lapply(pkgs,require, character.only=T)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(pkgs)

whitebox::install_whitebox()

library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(whitebox)
#If installing/using whitebox for the first time
#install_whitebox()


whitebox::wbt_init()

theme_set(theme_classic())
tmap_mode("view")

dem <- raster("H:/A/DEM_pr1.tif", crs = '+init=EPSG:4326')

wbt_breach_depressions_least_cost(
  dem = "H:/A/DEM_pr1.tif",
  output = "H:/A/dem_breached.tif",
  dist = 5,
  fill = TRUE)

wbt_fill_depressions_wang_and_liu(
  dem = "H:/A/dem_breached.tif",
  output = "H:/A/dem_filled_breached.tif"
)

filled_breached <- raster("H:/A/dem_filled_breached.tif")

## What did this do?
difference <- dem - filled_breached

difference[difference == 0] <- NA

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()+
  tm_shape(difference)+
  tm_raster(style = "cont",legend.show = TRUE)+
  tm_scale_bar()
## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.



wbt_d8_flow_accumulation(input = "H:/A/dem_filled_breached.tif",
                         output = "H:/A/D8FA.tif")

d8 <- raster("H:/A/D8FA.tif")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(log(d8))+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE, alpha = .5)+
  tm_scale_bar()
## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

wbt_d_inf_flow_accumulation("H:/A/dem_filled_breached.tif",
                            "H:/A/DinfFA.tif")

dinf <- raster("H:/A/DinfFA.tif")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(log(dinf))+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE, alpha = 0.5)+
  tm_scale_bar()

## Variable(s) "NA" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.

