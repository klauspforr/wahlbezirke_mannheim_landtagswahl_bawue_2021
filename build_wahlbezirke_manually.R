rm(list=ls())
gc()

# load packages
library(xlsx)
library(tidyr)
library(dplyr)
library(tmaptools)
library(osmdata)
library(sp)
library(sf)
library(ggplot2)
library(ggmap)
library(concaveman)

# look up addresses individually for each Wahlbezirk by inspecting adresses in Excel sheet
# 01111
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('A1','A2','A3','A4','A5',
                                                  'B1','B2','B3','B4','B5','B6','B7',
                                                  'C1','C2','C3','C4','C6')) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
    opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'name', value =c('Amtsgericht Mannheim','Mensa am Schloss','Schloss Mannheim')) %>%
    osmdata_sf ()
)
wahlbezirke<-as_Spatial(st_geometry(concaveman(osm_query$osm_points,concavity = 2)),IDs="01111")

# 01112
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('C7','C8',
                                                  'D1','D2','D3','D4','D5','D6','D7',
                                                  'E1','E2','E3','E4','E5','E6','E7',
                                                  'F1','F2','F3','F4','F5','F6')) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,as_Spatial(st_geometry(concaveman(osm_query$osm_points,concavity = 2)),IDs="01112"))

# 01113
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('C7','C8',
                                                  'D1','D2','D3','D4','D5','D6','D7',
                                                  'E1','E2','E3','E4','E5','E6','E7',
                                                  'F1','F2','F3','F4','F5','F6')) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,as_Spatial(st_geometry(concaveman(osm_query$osm_points,concavity = 2)),IDs="01112"))


#wahlbezirke@polygons[[1]]@ID



#osm_query$overpass_call
ggmap(get_map(c(8.45,49.48,8.47,49.492),source="osm"))+
  geom_sf(aes(alpha=0),
          data=st_as_sf(wahlbezirke),inherit.aes=F,show.legend = F)
#str(getbb("Mannheim"))
#getbb("Mannheim")
#