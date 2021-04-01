rm(list=ls())
gc()

setwd("E:/R/wahlbezirke/")
#install the osmdata, sf, tidyverse and ggmap package
if(!require("xlsx")) install.packages("xlsx") 
if(!require("tidyr")) install.packages("tidyr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("tmaptools")) install.packages("tmaptools")
#if(!require("readr")) install.packages("readr")
#if(!require("osmdata")) install.packages("osmdata")
#if(!require("tidyverse")) install.packages("tidyverse")
#if(!require("sf")) install.packages("sf")
#if(!require("ggmap")) install.packages("ggmap")


#load packages
library(xlsx)
library(tidyr)
library(dplyr)
library(tmaptools)
#library(readr)
#library(tidyverse)
#library(osmdata)
#library(sf)
#library(ggmap)


#available_features()

#head(available_tags("addr"))

# import street data from excel file
streetdata<-as_tibble(read.xlsx2(file="StrVerz_LTW21.xlsx",
                                 sheetName="str_ltw21",
                                 colIndex=c(1,2,5,7),
                                 as.data.frame=T,
                                 header=T))
# reshape data into long format
# start and end of street as separate rows
streetdata<-pivot_longer(streetdata,cols=2:3,
                         names_to="type",
                         values_to="number",
                         values_transform = list(number=as.character))
# drop type
streetdata<-streetdata %>% select(!"type")

# repair block addresses
streetdata<-as.data.frame(streetdata)
streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                         "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1]<-
  sub(" ","",streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                                      "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1])

# address house number to street names
streetdata[substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                         "K","L","M","N","O","P","Q","R","S","T","U") &
           substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9),1]<-
  paste0(streetdata[substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                  "K","L","M","N","O","P","Q","R","S","T","U") &
                    substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9),1],",",
         streetdata[substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                  "K","L","M","N","O","P","Q","R","S","T","U") &
                    substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9),3])

streetdata[!(substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O","P","Q","R","S","T","U") &
             substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9)),1]<-
  paste0(streetdata[!(substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                    "K","L","M","N","O","P","Q","R","S","T","U") &
                      substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9)),1]," ",
         streetdata[!(substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                    "K","L","M","N","O","P","Q","R","S","T","U") &
                      substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9)),3])

# drop house numbers
streetdata<-streetdata %>% select(!"number")

# repair streetname abbrevations
streetdata[grep("Stra ",streetdata[,1]),1]<-sub("Stra ","Straße ",streetdata[grep("Stra ",streetdata[,1]),1])
streetdata[grep("Str ",streetdata[,1]),1]<-sub("Str ","Straße ",streetdata[grep("Str ",streetdata[,1]),1])
streetdata[grep("St ",streetdata[,1]),1]<-sub("St ","Straße ",streetdata[grep("St ",streetdata[,1]),1])
streetdata[grep("S ",streetdata[,1]),1]<-sub("S ","Straße ",streetdata[grep("S ",streetdata[,1]),1])
streetdata[grep("Str. ",streetdata[,1]),1]<-sub("Str. ","Straße ",streetdata[grep("Str. ",streetdata[,1]),1])
streetdata[grep("str. ",streetdata[,1]),1]<-sub("str. ","straße ",streetdata[grep("str. ",streetdata[,1]),1])
streetdata[grep("W ",streetdata[,1]),1]<-sub("W ","Weg ",streetdata[grep("W ",streetdata[,1]),1])
streetdata[grep("W. ",streetdata[,1]),1]<-sub("W. ","Weg ",streetdata[grep("W. ",streetdata[,1]),1])
streetdata[grep("Rg. ",streetdata[,1]),1]<-sub("Rg. ","Ring ",streetdata[grep("Rg. ",streetdata[,1]),1])
streetdata[grep("Pl. ",streetdata[,1]),1]<-sub("Pl. ","Platz ",streetdata[grep("Pl. ",streetdata[,1]),1])
streetdata[grep("An d. Kammerschleuse",streetdata[,1]),1]<-sub("An d. Kammerschleuse","An der Kammerschleuse",streetdata[grep("An d. Kammerschleuse",streetdata[,1]),1])
streetdata[grep("Bürgerm.-Fuchs-Straße",streetdata[,1]),1]<-sub("Bürgerm.-Fuchs-Straße","Bürgermeister-Fuchs-Straße",streetdata[grep("Bürgerm.-Fuchs-Straße",streetdata[,1]),1])
streetdata[grep("Chr.-",streetdata[,1]),1]<-sub("Chr.-","Christian-",streetdata[grep("Chr.-",streetdata[,1]),1])
streetdata[grep("Der Hohe Weg Z.Rhein",streetdata[,1]),1]<-sub("Der Hohe Weg Z.Rhein","Der Hohe Weg Zum Rhein",streetdata[grep("Der Hohe Weg Z.Rhein",streetdata[,1]),1])
streetdata[grep("Dietr.Bonhoeffer",streetdata[,1]),1]<-sub("Dietr.Bonhoeffer","Dietrich-Bonhoeffer",streetdata[grep("Dietr.Bonhoeffer",streetdata[,1]),1])
streetdata[grep("Elis.-V-Thadden",streetdata[,1]),1]<-sub("Elis.-V-Thadden","Elisabeth-Von-Thadden",streetdata[grep("Elis.-V-Thadden",streetdata[,1]),1])
streetdata[grep("Esther-Charl.-Brandes",streetdata[,1]),1]<-sub("Esther-Charl.-Brandes","Esther-Charlotte-Brandes",streetdata[grep("Esther-Charl.-Brandes",streetdata[,1]),1])
streetdata[grep("Friedr.",streetdata[,1]),1]<-sub("Friedr.","Friedrich",streetdata[grep("Friedr.",streetdata[,1]),1])
streetdata[grep("-V-",streetdata[,1]),1]<-sub("-V-","-Von-",streetdata[grep("-V-",streetdata[,1]),1])
streetdata[grep("-V.-",streetdata[,1]),1]<-sub("-V.-","-Von-",streetdata[grep("-V.-",streetdata[,1]),1])
streetdata[grep("E.-Altmann-Gottheiner",streetdata[,1]),1]<-sub("E.-Altmann-Gottheiner","Elisabeth-Altmann-Gottheiner",streetdata[grep("E.-Altmann-Gottheiner",streetdata[,1]),1])
streetdata[grep("Fred-J.-Schoeps-Straße",streetdata[,1]),1]<-sub("Fred-J.-Schoeps-Straße","Fred-Joachim-Schoeps-Straße",streetdata[grep("Fred-J.-Schoeps-Straße",streetdata[,1]),1])
streetdata[grep("Gerh.-Hauptmann-Straße",streetdata[,1]),1]<-sub("Gerh.-Hauptmann-Straße","Gerhart-Hauptmann-Straße",streetdata[grep("Gerh.-Hauptmann-Straße",streetdata[,1]),1])
streetdata[grep("Herm.-Gropengießer",streetdata[,1]),1]<-sub("Herm.-Gropengießer","Hermann-Gropengießer",streetdata[grep("Herm.-Gropengießer",streetdata[,1]),1])
streetdata[grep("Herm.-Heimerich",streetdata[,1]),1]<-sub("Herm.-Heimerich","Hermann-Heimerich",streetdata[grep("Herm.-Heimerich",streetdata[,1]),1])
streetdata[grep("Herm.-Hesse",streetdata[,1]),1]<-sub("Herm.-Hesse","Hermann-Hesse",streetdata[grep("Herm.-Hesse",streetdata[,1]),1])
streetdata[grep("Herm.-Löns",streetdata[,1]),1]<-sub("Herm.-Löns","Hermann-Löns",streetdata[grep("Herm.-Löns",streetdata[,1]),1])
streetdata[grep("Neischwand.",streetdata[,1]),1]<-sub("Neischwand.","Neischwander",streetdata[grep("Neischwand.",streetdata[,1]),1])
streetdata[grep("Phil.-Brunnemer",streetdata[,1]),1]<-sub("Phil.-Brunnemer","Philipp-Brunnemer",streetdata[grep("Phil.-Brunnemer",streetdata[,1]),1])
streetdata[grep("Reichsk.-Müller",streetdata[,1]),1]<-sub("Reichsk.-Müller","Reichskanzler-Müller",streetdata[grep("Reichsk.-Müller",streetdata[,1]),1])
streetdata[grep("Tauberbischofsh.Straße",streetdata[,1]),1]<-sub("Tauberbischofsh.Straße","Tauberbischofsheimer Straße",streetdata[grep("Tauberbischofsh.Straße",streetdata[,1]),1])
streetdata[grep("Verb.-Kanal Li.Ufer",streetdata[,1]),1]<-sub("Verb.-Kanal Li.Ufer","Verbindungs-Kanal Linkes Ufer",streetdata[grep("Verb.-Kanal Li.Ufer",streetdata[,1]),1])
streetdata[grep("Verl.Jungbuschstraße",streetdata[,1]),1]<-sub("Verl.Jungbuschstraße","Verlängerte Jungbuschstraße",streetdata[grep("Verl.Jungbuschstraße",streetdata[,1]),1])
streetdata[grep("Wilh.-Furtwängl.Straße",streetdata[,1]),1]<-sub("Wilh.-Furtwängl.Straße","Wilhelm-Furtwänglänger-Straße",streetdata[grep("Wilh.-Furtwängl.Straße",streetdata[,1]),1])
streetdata[grep("Alter Rangierbahnhof 5",streetdata[,1]),1]<-sub("Alter Rangierbahnhof 5","49.474377122229455, 8.47689138464993",streetdata[grep("Alter Rangierbahnhof 5",streetdata[,1]),1])
streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1]<-sub("Christian-FriedrichSchwan","Christian-Friedrich-Schwan",streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1])
streetdata[grep("Friedrichch",streetdata[,1]),1]<-sub("Friedrichch","Friedrich",streetdata[grep("Friedrichch",streetdata[,1]),1])
streetdata[grep("Gewann auf den Ried 6",streetdata[,1]),1]<-sub("Gewann auf den Ried 6","49.50161936443979, 8.537328771035872",streetdata[grep("Gewann auf den Ried 6",streetdata[,1]),1])


# get geocodes for adresses from OSM
ding<-geocode_OSM(streetdata[,1])[,c(3,2)]

# for(i in 1:nrow(streetdata)){
#   coordinates = getbb(streetdata[i,1])
#   streetdata$long[i] = (coordinates[1,1] + coordinates[1,2])/2
#   streetdata$lat[i] = (coordinates[2,1] + coordinates[2,2])/2
# }
# 
# q2<-getbb("Mannheim") %>%
#   opq() %>%
#   add_osm_feature("name", "Franklinschule")
# ding<-osmdata_sf(q2)

#subset1<-streetdata %>% filter()
#streetdata<-unite(data=streetdata,Straße,number,col="adresse",remove=T)

# q2<-getbb("Mannheim") %>%
#   opq() %>%
#   add_osm_feature("name", "Franklinschule")
# schule<-osmdata_sf(q2)
# schule$osm_points
# 
# mad_map <- get_map(getbb("Mannheim"), maptype = "toner-background")
# #final map
# ggmap(mad_map)+
#   geom_sf(data = schule$osm_points,
#           inherit.aes = FALSE,
#           colour = "#238443",
#           fill = "#004529",
#           alpha = .5,
#           size = 4,
#           shape = 21)+
#   labs(x = "", y = "")
# 
# q <- getbb("Mannheim") %>%
#   opq() %>%
#   add_osm_feature("addr:city", "Mannheim")
# 
# str(q)
# 
# mannheim <- osmdata_sf(q)
# mannheim
# length(mannheim$osm_points$geometry)
# save(mannheim,file="E:/R/wahlbezirke/mannheim.RData")
# 
# nrow(mannheim$osm_points[which(cinema$osm_points$addr.city=="Mannheim"),])
# #ding<-cinema$osm_points[which(cinema$osm_points$addr.city=="Mannheim"),]
# 
# ggplot(schule$osm_points)+
#   geom_sf(colour = "#08519c",
#           fill = "#08306b",
#           alpha = .5,
#           size = 1,
#           shape = 21)+
#   theme_void()
# 
# mad_map <- get_map(getbb("Mannheim"), maptype = "toner-background")
# #final map
# ggmap(mad_map)+
#   geom_sf(data = cinema$osm_points[which(cinema$osm_points$addr.city=="Mannheim"),],
#           inherit.aes = FALSE,
#           colour = "#238443",
#           fill = "#004529",
#           alpha = .5,
#           size = 4,
#           shape = 21)+
#   labs(x = "", y = "")
# 
# https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
#   https://wiki.openstreetmap.org/wiki/Key:addr
# names(cinema$osm_points)