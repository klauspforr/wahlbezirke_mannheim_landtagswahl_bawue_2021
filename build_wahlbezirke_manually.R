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
    osmdata_sf (),
    opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'name', value =c('Amtsgericht Mannheim','Mensa am Schloss','Schloss Mannheim')) %>%
    osmdata_sf ()
)
wahlbezirke<-SpatialPolygonsDataFrame(as_Spatial(st_geometry(
  concaveman(osm_query$osm_points,concavity = 2)),IDs="01111"),
  data=data.frame(ID="01111",row.names="01111"))

# 01112
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('C7','C8',
                                                  'D1','D2','D3','D4','D5','D6','D7',
                                                  'E1','E2','E3','E4','E5','E6','E7',
                                                  'F1','F2','F3','F4','F5','F6')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01112"),
                     data=data.frame(ID="01112",row.names="01112")))

# 01121
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('F7',
                                                  'G4','G5','G6','G7',
                                                  'H4','H5','H6','H7',
                                                  'I3','I4','I5','I6','I7',
                                                  'K3','K4','K5')) %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'name', value = c('K6','K7')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01121"),
                     data=data.frame(ID="01121",row.names="01121")))

# 01122
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('G2','G3',
                                                  'H1','H2','H3',
                                                  'S1','S2','S3','S4','S6',
                                                  'T4','T5','T6')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01122"),
                     data=data.frame(ID="01122",row.names="01122")))

# 01123
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('I1','I2',
                                                  'K1','K2',
                                                  'T1','T2','T3',
                                                  'U1','U2','U3','U4','U5','U6')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(st_cast(osm_query$osm_polygons,"POINT"),concavity = 2)),IDs="01123"),
                     data=data.frame(ID="01123",row.names="01123")))

#osm_query$overpass_call

ding<-st_as_sf(wahlbezirke)
ding_points <- sf::st_point_on_surface(ding)
ding_coords <- as.data.frame(sf::st_coordinates(ding_points))
ding_coords$ID <- ding$ID
ggmap(get_map(c(8.455,49.48,8.476,49.495),source="osm")) +
  geom_sf(aes(alpha=0),
          data=ding,inherit.aes=F,show.legend = F)+
  geom_text(data = ding_coords, aes(X, Y, label = ID), colour = "black")
# 
#   
#   geom_sf(data = ding, aes(fill=))
# 
# +
#   geom_text(data = ding_coords, aes(X, Y, label = ID), colour = "black")
#   
# 
# 
#   geom_sf(aes())+
#   geom_text(aes(, colour = "white")
# ggmap(get_map(c(8.455,49.48,8.476,49.495),source="osm"))+
#   geom_sf(aes(alpha=0),
#           data=st_as_sf(wahlbezirke),inherit.aes=F,show.legend = F)+
#   geom_text(data=wahlbezirke,aes(X,Y,label = ID),inherit.aes=F, colour = "black")

#str(getbb("Mannheim"))
#getbb("Mannheim")
#