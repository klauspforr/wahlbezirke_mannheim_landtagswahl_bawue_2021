rm(list=ls())
gc()

setwd("E:/R/wahlbezirke/")
#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")
if(!require("xlsx")) install.packages("xlsx") 

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(xlsx)

#available_features()

#head(available_tags("addr"))

# import street data from excel file
streetdata<-as_tibble(read.xlsx2(file="StrVerz_LTW21.xlsx",
                                 sheetName="str_ltw21",
                                 colIndex=c(1,2,5,7),
                                 as.data.frame=T,
                                 header=T))
streetdata<-streetdata

q2<-getbb("Mannheim") %>%
  opq() %>%
  add_osm_feature("name", "Franklinschule")
schule<-osmdata_sf(q2)
schule$osm_points

mad_map <- get_map(getbb("Mannheim"), maptype = "toner-background")
#final map
ggmap(mad_map)+
  geom_sf(data = schule$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")

q <- getbb("Mannheim") %>%
  opq() %>%
  add_osm_feature("addr:city", "Mannheim")

str(q)

mannheim <- osmdata_sf(q)
mannheim
length(mannheim$osm_points$geometry)
save(mannheim,file="E:/R/wahlbezirke/mannheim.RData")

nrow(mannheim$osm_points[which(cinema$osm_points$addr.city=="Mannheim"),])
#ding<-cinema$osm_points[which(cinema$osm_points$addr.city=="Mannheim"),]

ggplot(schule$osm_points)+
  geom_sf(colour = "#08519c",
          fill = "#08306b",
          alpha = .5,
          size = 1,
          shape = 21)+
  theme_void()

mad_map <- get_map(getbb("Mannheim"), maptype = "toner-background")
#final map
ggmap(mad_map)+
  geom_sf(data = cinema$osm_points[which(cinema$osm_points$addr.city=="Mannheim"),],
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")

https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
  https://wiki.openstreetmap.org/wiki/Key:addr
names(cinema$osm_points)